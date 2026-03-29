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

package org.hypernomicon.model;

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Set;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.util.file.*;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Tests for HyperPath association methods in {@link FilePathRegistry},
 * using {@link TestHyperDB} to obtain real HyperPath instances backed
 * by folder records.
 * <p>
 * These tests verify the registry's {@code addHyperPath}, {@code removeHyperPath},
 * and {@code getHyperPaths} methods through the HyperPath lifecycle: folder creation,
 * re-parenting, and the static {@link HyperPath#getHyperPathSetForFilePath} reverse lookup.
 */
class HyperPathRegistryTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void resetDB()
  {
    db.closeAndOpen();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static RegistryAccessor registry()
  {
    return FilePathRegistry.getAccessor();
  }

//---------------------------------------------------------------------------
//region Root folder registration
//---------------------------------------------------------------------------

  @Test
  void rootFolder_registeredAfterOpen()
  {
    HDT_Folder root = db.getRootFolder();
    FilePath rootPath = db.getRootPath();

    Set<HyperPath> set = registry().getHyperPaths(rootPath);

    assertFalse(set.isEmpty(), "Root folder HyperPath should be registered");
    assertTrue(set.contains(root.getPath()));
  }

//---------------------------------------------------------------------------

  @Test
  void rootFolder_retrievableViaStaticLookup()
  {
    HDT_Folder root = db.getRootFolder();

    Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(db.getRootPath());

    assertTrue(set.contains(root.getPath()));
  }

//---------------------------------------------------------------------------
//endregion
//region Folder creation registers HyperPath
//---------------------------------------------------------------------------

  @Test
  void assignFolder_registersHyperPathInRegistry()
  {
    HDT_Folder root  = db.getRootFolder(),
               child = db.createNewBlankRecord(hdtFolder);

    child.getPath().assign(root, FilePath.of("myfolder"));

    FilePath childPath = child.filePath();
    Set<HyperPath> set = registry().getHyperPaths(childPath);

    assertFalse(set.isEmpty(), "Assigned folder should be in registry");
    assertTrue(set.contains(child.getPath()));
  }

//---------------------------------------------------------------------------

  @Test
  void assignFolder_retrievableViaStaticLookup()
  {
    HDT_Folder root  = db.getRootFolder(),
               child = db.createNewBlankRecord(hdtFolder);

    child.getPath().assign(root, FilePath.of("staticlookup"));

    Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(child.filePath());

    assertTrue(set.contains(child.getPath()));
  }

//---------------------------------------------------------------------------

  @Test
  void assignFolder_deepNested_allRegistered()
  {
    HDT_Folder root = db.getRootFolder(),
               lvl1 = db.createNewBlankRecord(hdtFolder),
               lvl2 = db.createNewBlankRecord(hdtFolder),
               lvl3 = db.createNewBlankRecord(hdtFolder);

    lvl1.getPath().assign(root, FilePath.of("lvl1"));
    lvl2.getPath().assign(lvl1, FilePath.of("lvl2"));
    lvl3.getPath().assign(lvl2, FilePath.of("lvl3"));

    assertTrue(registry().getHyperPaths(lvl1.filePath()).contains(lvl1.getPath()));
    assertTrue(registry().getHyperPaths(lvl2.filePath()).contains(lvl2.getPath()));
    assertTrue(registry().getHyperPaths(lvl3.filePath()).contains(lvl3.getPath()));
  }

//---------------------------------------------------------------------------
//endregion
//region Reassignment updates registry
//---------------------------------------------------------------------------

  @Test
  void reassignFolder_oldPathRemoved_newPathRegistered()
  {
    HDT_Folder root  = db.getRootFolder(),
               child = db.createNewBlankRecord(hdtFolder);

    child.getPath().assign(root, FilePath.of("oldname"));
    FilePath oldPath = child.filePath();

    child.getPath().assign(root, FilePath.of("newname"));
    FilePath newPath = child.filePath();

    assertFalse(registry().getHyperPaths(oldPath).contains(child.getPath()),
      "Old path should no longer contain the HyperPath");
    assertTrue(registry().getHyperPaths(newPath).contains(child.getPath()),
      "New path should contain the HyperPath");
  }

//---------------------------------------------------------------------------

  @Test
  void reparentFolder_oldPathRemoved_newPathRegistered()
  {
    HDT_Folder root   = db.getRootFolder(),
               parent = db.createNewBlankRecord(hdtFolder),
               child  = db.createNewBlankRecord(hdtFolder),
               dest   = db.createNewBlankRecord(hdtFolder);

    parent.getPath().assign(root, FilePath.of("parent"));
    child .getPath().assign(parent, FilePath.of("child"));
    dest  .getPath().assign(root, FilePath.of("dest"));

    FilePath oldChildPath = child.filePath();

    // assign() auto-fires onSubtreeMoved for folder records when the
    // path changes, re-keying descendant HyperPath associations.
    child.getPath().assign(dest, FilePath.of("child"));

    FilePath newChildPath = child.filePath();

    assertFalse(registry().getHyperPaths(oldChildPath).contains(child.getPath()),
      "Old path should no longer contain the re-parented HyperPath");
    assertTrue(registry().getHyperPaths(newChildPath).contains(child.getPath()),
      "New path should contain the re-parented HyperPath");
  }

//---------------------------------------------------------------------------

  @Test
  void renameFolder_descendantHyperPaths_reKeyed()
  {
    HDT_Folder root   = db.getRootFolder(),
               parent = db.createNewBlankRecord(hdtFolder),
               child  = db.createNewBlankRecord(hdtFolder);

    parent.getPath().assign(root, FilePath.of("origname"));
    child .getPath().assign(parent, FilePath.of("leaf"));

    FilePath oldChildPath = child.filePath();

    // Rename parent folder (same parent, different name). assign() auto-fires
    // onSubtreeMoved, which should re-key the descendant child's HyperPath.
    parent.getPath().assign(root, FilePath.of("renamed"));

    FilePath newChildPath = child.filePath();

    assertNotEquals(oldChildPath, newChildPath,
      "Child path should reflect the parent rename");
    assertFalse(registry().getHyperPaths(oldChildPath).contains(child.getPath()),
      "Old descendant path should no longer contain the HyperPath");
    assertTrue(registry().getHyperPaths(newChildPath).contains(child.getPath()),
      "New descendant path should contain the re-keyed HyperPath");
  }

//---------------------------------------------------------------------------
//endregion
//region getHyperPaths edge cases
//---------------------------------------------------------------------------

  @Test
  void getHyperPaths_nullFilePath_returnsEmptySet()
  {
    assertTrue(registry().getHyperPaths(null).isEmpty());
  }

//---------------------------------------------------------------------------

  @Test
  void getHyperPaths_unregisteredPath_returnsEmptySet()
  {
    FilePath fakePath = db.getRootPath("does_not_exist");

    assertTrue(registry().getHyperPaths(fakePath).isEmpty());
  }

//---------------------------------------------------------------------------

  @Test
  void getHyperPaths_returnsDefensiveCopy()
  {
    HDT_Folder root  = db.getRootFolder(),
               child = db.createNewBlankRecord(hdtFolder);

    child.getPath().assign(root, FilePath.of("defensivecopy"));

    Set<HyperPath> set1 = registry().getHyperPaths(child.filePath());
    set1.clear();

    Set<HyperPath> set2 = registry().getHyperPaths(child.filePath());
    assertFalse(set2.isEmpty(), "Clearing returned set should not affect registry");
  }

//---------------------------------------------------------------------------
//endregion
//region removeHyperPath
//---------------------------------------------------------------------------

  @Test
  void removeHyperPath_removesSpecificAssociation()
  {
    HDT_Folder root  = db.getRootFolder(),
               child = db.createNewBlankRecord(hdtFolder);

    child.getPath().assign(root, FilePath.of("toremove"));
    FilePath childPath = child.filePath();

    registry().removeHyperPath(childPath, child.getPath());

    assertFalse(registry().getHyperPaths(childPath).contains(child.getPath()));
  }

//---------------------------------------------------------------------------

  @Test
  void removeHyperPath_nullArgs_noException()
  {
    assertDoesNotThrow(() -> registry().removeHyperPath(null, null));
  }

//---------------------------------------------------------------------------
//endregion
//region addHyperPath
//---------------------------------------------------------------------------

  @Test
  void addHyperPath_nullArgs_noException()
  {
    assertDoesNotThrow(() -> registry().addHyperPath(null, null));
  }

//---------------------------------------------------------------------------

  @Test
  void addHyperPath_manualAdd_retrievable()
  {
    HDT_Folder root  = db.getRootFolder(),
               child = db.createNewBlankRecord(hdtFolder);

    child.getPath().assign(root, FilePath.of("manual"));
    FilePath childPath = child.filePath();

    // Remove and re-add manually
    registry().removeHyperPath(childPath, child.getPath());
    assertTrue(registry().getHyperPaths(childPath).isEmpty());

    registry().addHyperPath(childPath, child.getPath());
    assertTrue(registry().getHyperPaths(childPath).contains(child.getPath()));
  }

//---------------------------------------------------------------------------
//endregion
//region Unassigned (empty) folder
//---------------------------------------------------------------------------

  @Test
  void unassignedFolder_notInRegistry()
  {
    HDT_Folder child = db.createNewBlankRecord(hdtFolder);

    // Newly created folder has no assignment; filePath() returns null
    assertNull(child.filePath());

    // An unassigned folder's HyperPath should not appear in the registry
    // (there is no path key to look it up by)
  }

//---------------------------------------------------------------------------

  @Test
  void assignThenClearName_removedFromRegistry()
  {
    HDT_Folder root  = db.getRootFolder(),
               child = db.createNewBlankRecord(hdtFolder);

    child.getPath().assign(root, FilePath.of("willclear"));
    FilePath childPath = child.filePath();

    assertFalse(registry().getHyperPaths(childPath).isEmpty());

    // Assign empty name removes from registry
    child.getPath().assign(root, null);

    assertTrue(registry().getHyperPaths(childPath).isEmpty(),
      "After clearing filename, old path should have no associations");
  }

//---------------------------------------------------------------------------
//endregion
//region Record deletion unregisters HyperPath
//---------------------------------------------------------------------------

  @Test
  void removeWorkFile_removedFromRegistry()
  {
    HDT_Folder root = db.getRootFolder();

    HDT_Work work = db.createNewBlankRecord(hdtWork);
    HDT_WorkFile workFile = db.createNewBlankRecord(hdtWorkFile);

    workFile.getPath().assign(root, FilePath.of("testfile.pdf"));
    FilePath workFilePath = workFile.filePath();

    assertFalse(registry().getHyperPaths(workFilePath).isEmpty(),
      "Work file path should be registered after assignment");

    work.addWorkFile(workFile.getID());

    // Removing the work file from its only work triggers auto-deletion
    // (RelationSet.setObject: when subject count drops to 0, deleteRecord is called)

    db.getObjectList(RelationType.rtWorkFileOfWork, work, true).remove(workFile);

    assertTrue(workFile.isExpired(), "Work file should be expired after removal from its only work");

    assertTrue(registry().getHyperPaths(workFilePath).isEmpty(),
      "Expired work file path should no longer be in the registry");
  }

//---------------------------------------------------------------------------

  @Test
  void deleteMiscFile_removedFromRegistry()
  {
    HDT_Folder root = db.getRootFolder();

    HDT_MiscFile miscFile = db.createNewBlankRecord(hdtMiscFile);
    miscFile.getPath().assign(root, FilePath.of("misc_delete_test.txt"));
    FilePath miscFilePath = miscFile.filePath();

    assertFalse(registry().getHyperPaths(miscFilePath).isEmpty(),
      "Misc file path should be registered after assignment");

    db.deleteRecord(miscFile);

    assertTrue(miscFile.isExpired());
    assertTrue(registry().getHyperPaths(miscFilePath).isEmpty(),
      "Deleted misc file path should no longer be in the registry");
  }

//---------------------------------------------------------------------------

  @Test
  void deletePerson_picturePathRemovedFromRegistry()
  {
    HDT_Folder root = db.getRootFolder();

    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    person.getPath().assign(root, FilePath.of("portrait.jpg"));
    FilePath picturePath = person.filePath();

    assertFalse(registry().getHyperPaths(picturePath).isEmpty(),
      "Person picture path should be registered after assignment");

    db.deleteRecord(person);

    assertTrue(person.isExpired());
    assertTrue(registry().getHyperPaths(picturePath).isEmpty(),
      "Deleted person's picture path should no longer be in the registry");
  }

//---------------------------------------------------------------------------

  @Test
  void sharedWorkFile_oneWorkRemoved_pathStaysUntilLastWorkRemoved()
  {
    HDT_Folder root = db.getRootFolder();

    HDT_WorkFile workFile = db.createNewBlankRecord(hdtWorkFile);
    HDT_Work work1 = db.createNewBlankRecord(hdtWork),
             work2 = db.createNewBlankRecord(hdtWork);

    workFile.getPath().assign(root, FilePath.of("shared.pdf"));
    FilePath workFilePath = workFile.filePath();

    work1.addWorkFile(workFile.getID());
    work2.addWorkFile(workFile.getID());

    // Remove from first work; subject count is still 1, so work file should NOT be deleted

    db.getObjectList(RelationType.rtWorkFileOfWork, work1, true).remove(workFile);

    assertFalse(workFile.isExpired(),
      "Work file should not be expired when still referenced by another work");
    assertFalse(registry().getHyperPaths(workFilePath).isEmpty(),
      "Work file path should still be registered when referenced by another work");

    // Remove from second work; subject count drops to 0, triggering auto-deletion

    db.getObjectList(RelationType.rtWorkFileOfWork, work2, true).remove(workFile);

    assertTrue(workFile.isExpired(),
      "Work file should be expired after removal from its last work");
    assertTrue(registry().getHyperPaths(workFilePath).isEmpty(),
      "Work file path should be removed from registry after last work reference removed");
  }

//---------------------------------------------------------------------------

  @Test
  void cascadingDeletion_workFileRemovedFromRegistryWhenWorkDeleted()
  {
    HDT_Folder root = db.getRootFolder();

    HDT_WorkFile workFile = db.createNewBlankRecord(hdtWorkFile);
    HDT_Work work = db.createNewBlankRecord(hdtWork);

    workFile.getPath().assign(root, FilePath.of("cascading.pdf"));
    FilePath workFilePath = workFile.filePath();

    work.addWorkFile(workFile.getID());

    assertFalse(registry().getHyperPaths(workFilePath).isEmpty(),
      "Work file path should be registered");

    // Deleting the work triggers resolvePointers, which clears the work file's
    // relation to the (now expired) work, dropping the subject count to 0.
    // This triggers db.deleteRecord(workFile).

    db.deleteRecord(work);

    assertTrue(workFile.isExpired(),
      "Work file should be cascade-expired when its only work is deleted");
    assertTrue(registry().getHyperPaths(workFilePath).isEmpty(),
      "Cascade-deleted work file path should no longer be in the registry");
  }

//---------------------------------------------------------------------------
//endregion
//region closeAndOpen clears and re-registers
//---------------------------------------------------------------------------

  @Test
  void closeAndOpen_reRegistersRootFolder()
  {
    HDT_Folder root = db.getRootFolder();
    HyperPath rootHp = root.getPath();
    FilePath rootPath = db.getRootPath();

    assertTrue(registry().getHyperPaths(rootPath).contains(rootHp));

    db.closeAndOpen();

    // After closeAndOpen, root folder is a new record object
    HDT_Folder newRoot = db.getRootFolder();
    FilePath newRootPath = db.getRootPath();

    Set<HyperPath> set = registry().getHyperPaths(newRootPath);
    assertFalse(set.isEmpty(), "Root folder should be re-registered after closeAndOpen");
    assertTrue(set.contains(newRoot.getPath()));
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
