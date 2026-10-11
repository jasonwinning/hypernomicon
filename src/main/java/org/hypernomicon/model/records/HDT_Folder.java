/*
 * Copyright 2015-2026 Jason Winning
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.hypernomicon.model.records;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.HyperDB.*;

import java.io.IOException;
import java.nio.file.*;
import java.util.List;

import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.FilePathRegistry;
import org.hypernomicon.util.file.deletion.FileDeletion;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult;

//---------------------------------------------------------------------------

public class HDT_Folder extends HDT_RecordBase implements HDT_RecordWithFolderPath
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final List<HDT_Folder> childFolders;
  public final List<HDT_Note> notes;

  private final HyperPath path;
  private boolean checkedForExistence;

//---------------------------------------------------------------------------

  public HDT_Folder(RecordState xmlState, DatasetAccessor<HDT_Folder> dataset)
  {
    super(xmlState, dataset);

    checkedForExistence = false;

    childFolders  = getSubjList(rtParentFolderOfFolder );
    notes         = getSubjList(rtFolderOfNote);

    path = new HyperPath(getObjPointer(rtParentFolderOfFolder), this);
  }

//---------------------------------------------------------------------------

  /** @see org.hypernomicon.model.AbstractHyperDB#isSpecialFolder(int, boolean) */
  public boolean isSpecial(boolean checkSubfolders) { return db.isSpecialFolder(getID(), checkSubfolders); }

  @Override public HyperPath getPath()              { return path; }
  @Override public String name()                    { return path.getNameStr(); }
  @Override public String getNameEngChar()          { return convertToEnglishChars(name()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Check if this folder's parent chain connects back to the root folder.
   * This can return false if parent folder records have been deleted.
   * @return true if the folder is connected to root, false otherwise
   */
  public boolean isConnectedToRoot()
  {
    HDT_Folder folder = this;

    do
    {
      if (folder.getID() == ROOT_FOLDER_ID)
        return true;

      folder = folder.parentFolder();
    }
    while (folder != null);

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Rename this folder on disk and update the folder record. The physical rename is
   * performed by {@link FilePath#renameDirectory}, which delegates to {@link FilePath#filesMove}
   * for watcher stop/restart and FTS index notification. After the physical rename,
   * {@code path.assign()} is called, which triggers {@code onSubtreeMoved()} to re-key
   * HyperPath associations in the {@link FilePathRegistry} under the new path.
   * @return true if the rename succeeded, false if validation failed or an error occurred
   */
  public boolean renameTo(String newName)
  {
    if (getID() == ROOT_FOLDER_ID)
      return falseWithErrorPopup("Unable to rename the folder: Root folder cannot be renamed.");

    if (this == db.getXmlFolder())
      return falseWithErrorPopup("Unable to rename the folder: XML folder cannot be renamed.");

    if (parentFolder() == null)
      return falseWithErrorPopup("Unable to rename the folder: Parent folder record is null.");

    FilePath srcFilePath = filePath();

    if (srcFilePath.exists() == false)
      return falseWithErrorPopup("Unable to rename the folder: It does not exist.");

    FilePath parentFilePath = parentFolder().filePath();

    if (parentFilePath.exists() == false)
      return falseWithErrorPopup("Unable to rename the folder: Parent folder does not exist.");

    FilePath destFilePath = parentFilePath.resolve(newName);

    if (destFilePath.exists())
      return falseWithErrorPopup("Unable to rename the folder: A file or folder already has that name.");

    try
    {
      srcFilePath.renameDirectory(destFilePath);
    }
    catch (FileSystemException e)
    {
      if ((e instanceof AccessDeniedException) || (e.getClass() == FileSystemException.class))
      {
        srcFilePath.anyOpenFilesInDir(); // shows popup identifying the locked path, if any
        return false;
      }

      return falseWithErrorPopup("Unable to rename the folder: " + getThrowableMessage(e));
    }
    catch (IOException e)
    {
      return falseWithErrorPopup("Unable to rename the folder: " + getThrowableMessage(e));
    }

    setNameInternal(newName, true);
    path.assign(parentFolder(), FilePath.of(newName));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Move this folder (and its entire subtree) to a new parent folder by re-parenting the
   * existing record rather than creating new records and deleting old ones. This preserves
   * record IDs and all associations (notes, work files, misc files).
   * <p>
   * The physical move is performed by {@link FilePath#filesMove}, which handles watcher
   * stop/restart and FTS index notification. After the physical move, {@code path.assign()}
   * is called, which triggers {@code onSubtreeMoved()} to re-key HyperPath associations
   * in the {@link FilePathRegistry} under the new path. Registry eviction is NOT performed
   * by {@code filesMove} for directory moves, because {@code onSubtreeMoved()} needs the
   * old entries to still be present in order to collect and re-key them.
   * @param newParent The destination parent folder
   * @return True if the folder was moved successfully
   * @throws IOException If the filesystem move fails
   */
  public boolean moveToNewParent(HDT_Folder newParent) throws IOException
  {
    if (getID() == ROOT_FOLDER_ID)
      return falseWithErrorPopup("Unable to move the folder: Root folder cannot be moved.");

    if (this == db.getXmlFolder())
      return falseWithErrorPopup("Unable to move the folder: XML folder cannot be moved.");

    if (parentFolder() == null)
      return falseWithErrorPopup("Unable to move the folder: Parent folder record is null.");

    if (newParent == null)
      return falseWithErrorPopup("Unable to move the folder: Destination parent is null.");

    if (newParent == parentFolder())
      return true;

    FilePath srcFilePath = filePath();

    if (srcFilePath.exists() == false)
      return falseWithErrorPopup("Unable to move the folder: It does not exist.");

    if (newParent.filePath().exists() == false)
      return falseWithErrorPopup("Unable to move the folder: Destination parent does not exist.");

    FilePath destFilePath = newParent.filePath().resolve(name());

    if (destFilePath.exists())
      return falseWithErrorPopup("Unable to move the folder: A file or folder already exists at \"" + destFilePath + "\".");

    FilePath.filesMove(srcFilePath, destFilePath);

    path.assign(newParent, FilePath.of(name()));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Delete the folder on the file system and delete the folder record and its child folder records
   * @return True if the folder was deleted successfully
   */
  public boolean delete()
  {
    FilePath filePath = filePath();

    if (db.isProtectedRecord(this, true))
      return falseWithErrorPopup("The folder \"" + filePath + "\" is in use by the database and cannot be deleted.");

    if (parentFolder() == null)
      return falseWithErrorPopup("Unable to delete the folder \"" + filePath + "\": Parent folder record is null.");

    if (filePath.exists() == false)
      return falseWithErrorPopup("Unable to delete the folder \"" + filePath + "\": It does not exist.");

    boolean restartWatcher = folderTreeWatcher.stop();

    DeletionResult result = FileDeletion.ofDirWithContents(filePath).interactive().execute();

    if (result != DeletionResult.SUCCESS)
    {
      if (restartWatcher)
        folderTreeWatcher.createNewWatcherAndStart();

      return false;
    }

    deleteFolderRecordTree(this);

    if (restartWatcher)
      folderTreeWatcher.createNewWatcherAndStart();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Deletes a folder's record and the records of all its descendant folders. Records only: the
   * directories were deleted beforehand by {@link #delete()}, or had already vanished from the disk
   * when the File Manager's prune found them. Those are this method's two callers.
   * <p>
   * The order matters. Folder records form a tree through the parent-folder pointer, and the
   * database does not cascade a deletion down it. Moreover, deleting a folder record takes a
   * shortcut in {@code AbstractHyperDB.deleteRecord} ({@code folderDeletionBypassEnabled}): instead
   * of the database-wide pointer cleanup that other record types get, the expired record is removed
   * from its HyperCore directly, on the strength of {@code doFolderDeletionCheck}'s assertion that
   * nothing points at it any more. The depth-first walk here is what makes that true for child
   * folders. The callers see to everything else that could point at a folder in the subtree:
   * {@link #delete()} detaches the folder's notes first, and both callers only reach a folder whose
   * contents are not in use (no work files, misc. files or pictures), the File Manager refusing such
   * a deletion and the prune skipping such a folder.
   * <p>
   * Each record's expiry severs its relation to the parent, which is why the child list is copied
   * before it is walked, and runs the delete handlers, through which the File Manager's tree model
   * and folder history drop the folder. A folder whose record is already gone (its ID is no longer
   * positive) is skipped. Call this on the FX thread only: the folder tree watcher's deletion case
   * deliberately leaves it to the prune, since calling it from the watcher thread would race with
   * the prune's walk.
   *
   * @param folder The root of the subtree to delete; its own record goes last
   */
  public static void deleteFolderRecordTree(HDT_Folder folder)
  {
    if (folder.childFolders.isEmpty() == false)
      List.copyOf(folder.childFolders).forEach(HDT_Folder::deleteFolderRecordTree);

    if (folder.getID() > 0)
      db.deleteRecord(folder);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Check if this folder and its subfolders exist, collecting any missing folders into the provided list.
   * @param missingFolders List to add missing folder paths to
   */
  public void checkExists(List<FilePath> missingFolders)
  {
    if (checkedForExistence) return;

    checkedForExistence = true;

    if (getID() != ROOT_FOLDER_ID)
      if (filePath().exists() == false)
        if (isSpecial(false) || path.isInUseByRecords())
          missingFolders.add(filePath());

    childFolders.forEach(child -> child.checkExists(missingFolders));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Note closestAncestorNote()
  {
    return notes.isEmpty() ? nullSwitch(parentFolder(), db.notes.getByID(1), HDT_Folder::closestAncestorNote) : notes.getFirst();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Folder createSubfolder(String folderName)
  {
    boolean restartWatcher = folderTreeWatcher.stop();

    FilePath childFilePath = filePath().resolve(folderName.strip());
    HDT_Folder childFolder = null;

    try
    {
      childFilePath.createDirectory();
      childFolder = HyperPath.getFolderFromFilePath(childFilePath, true);
    }
    catch (FileAlreadyExistsException e)
    {
      errorPopup("Unable to create the folder: A file with that name already exists.");
    }
    catch (IOException | InvalidPathException e)
    {
      errorPopup("Unable to create the folder: " + getThrowableMessage(e));
    }

    if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();

    FileManager.setNeedRefresh();

    return childFolder;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
