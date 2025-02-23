/*
 * Copyright 2015-2025 Jason Winning
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

package org.hypernomicon.dialogs;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;

import java.io.IOException;
import java.util.EnumSet;

import org.apache.commons.io.FilenameUtils;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;

public class FileDlgCtrlr extends HyperDlg
{
  @FXML private Button btnUseRecord, btnBrowseOld;
  @FXML private CheckBox chkDontChangeFilename;
  @FXML private Label lblName;
  @FXML private RadioButton rbCopy, rbMove, rbNeither;
  @FXML private TextField tfCurrentPath, tfFileName, tfNewPath;
  @FXML private ComboBox<HyperTableCell> cbType;

  @FXML public Button btnCancel;
  @FXML public TextField tfRecordName;

  private final RecordType recordType;
  private final HDT_Work curWork;
  private final boolean canBrowseToExistingMiscFileRecord;

  public final HyperCB hcbType;

  private FilePath srcFilePath;
  private HDT_RecordWithPath curFileRecord;
  private boolean copyOnly;

  @SuppressWarnings("unchecked")
  public <HDT_T extends HDT_RecordWithPath> HDT_T getFileRecord() { return (HDT_T) curFileRecord; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FileDlgCtrlr(String title, HDT_MiscFile curFileRecord, String recordName, boolean canBrowseToExistingMiscFileRecord)
  {
    this(title, hdtMiscFile, curFileRecord, null, recordName, canBrowseToExistingMiscFileRecord);
  }

  public FileDlgCtrlr(String title, HDT_WorkFile curFileRecord, HDT_Work curWork)
  {
    this(title, hdtWorkFile, curFileRecord, curWork, "", false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FileDlgCtrlr(String title, RecordType recordType, HDT_RecordWithPath curFileRecord, HDT_Work curWork, String recordName, boolean canBrowseToExistingMiscFileRecord)
  {
    super("FileDlg", title, true);

    this.canBrowseToExistingMiscFileRecord = canBrowseToExistingMiscFileRecord;
    this.curWork = curWork;
    this.recordType = recordType;
    srcFilePath = null;
    copyOnly = false;

    hcbType = new HyperCB(cbType, ctDropDown, new StandardPopulator(hdtFileType));

    tfFileName.disableProperty().bind(chkDontChangeFilename.selectedProperty());

    setFileRecord(curFileRecord);

    if (recordType == hdtMiscFile)
      tfRecordName.setText(recordName);

    tfCurrentPath.setEditable(false);
    tfNewPath.setEditable(false);

    rbCopy.setDisable(true); // The only way copy is allowed is if the user selects a file by clicking browse first

    btnBrowseOld.setOnAction(event -> btnBrowseOldClick(false));

    onShown = () ->
    {
      if (FilePath.isEmpty(srcFilePath))
        btnBrowseOldClick(true);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setFileRecord(HDT_RecordWithPath curFileRecord)
  {
    this.curFileRecord = curFileRecord;

    if (curFileRecord != null)
    {
      srcFilePath = curFileRecord.filePath();
      if (FilePath.isEmpty(srcFilePath) == false)
      {
        tfCurrentPath.setText(srcFilePath.toString());

        if (db.unenteredPath().isSubpath(srcFilePath))
          tfNewPath.setText(db.miscFilesPath().toString());
        else
        {
          tfNewPath.setText(srcFilePath.getDirOnly().toString());
          setRB_CurrentLocationOnly();
        }
      }
      else
        setRB_CurrentLocationOnly();
    }

    rbNeither.setDisable(FilePath.isEmpty(srcFilePath));

    if (recordType == hdtWorkFile)
    {
      cbType.setDisable(true);

      lblName.setText("This will be the description");
      btnUseRecord.setText("Use description");

      if (curFileRecord != null)
        tfRecordName.setText(curFileRecord.name());
      else
      {
        FilePath newFilePath = null;

        if (curWork.workFiles.size() > 0)
          newFilePath = curWork.filePath();

        tfNewPath.setText(FilePath.isEmpty(newFilePath) ? db.topicalPath().toString() : newFilePath.getDirOnly().toString());
      }
    }
    else
    {
      if (tfNewPath.getText().isEmpty())
        tfNewPath.setText(FilePath.isEmpty(srcFilePath) ? db.miscFilesPath().toString() : srcFilePath.getDirOnly().toString());

      if (curFileRecord != null)
      {
        hcbType.selectIDofRecord(((HDT_MiscFile) curFileRecord).fileType);

        tfRecordName.setText(curFileRecord.name());
      }
    }

    if (FilePath.isEmpty(srcFilePath) == false)
      tfFileName.setText(srcFilePath.getNameOnly().toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setRB_CurrentLocationOnly()
  {
    rbCopy.setDisable(true);
    rbMove.setDisable(true);
    rbNeither.setDisable(false);

    rbNeither.setSelected(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setRB_OtherLocationsOK()
  {
    rbCopy.setDisable(false);
    rbMove.setDisable(copyOnly);
    rbNeither.setDisable(false);

    if (rbMove.isSelected() && copyOnly)
      rbCopy.setSelected(true);

    if ((copyOnly == false) && rbNeither.isSelected())
      rbMove.setSelected(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setRB_CopyOnly()
  {
    rbCopy.setDisable(false);
    rbMove.setDisable(true);
    rbNeither.setDisable(true);

    rbCopy.setSelected(true);
    copyOnly = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnLaunchClick()
  {
    launchFile(srcFilePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Set the source file to something different than the path of the misc. file
   * @param newSrc The filePath to use
   * @param initial Whether the dialog is just opening or not
   */
  public void setSrcFilePath(FilePath newSrc, boolean initial)
  {
    if (FilePath.isEmpty(newSrc)) return;

    if (db.isProtectedFile(newSrc, false))
    {
      errorPopup("That file cannot be assigned to a record.");
      return;
    }

    srcFilePath = newSrc;
    copyOnly = false;

    // See if the chosen file is currently assigned to a file record

    HDT_RecordWithPath existingRecord = HyperPath.getRecordFromFilePath(newSrc);

    if (existingRecord != null)
    {
      if (existingRecord == curFileRecord) // chosen file is the one already attached to this record
      {
        setRB_CurrentLocationOnly();
      }
      else
      {
        if ((existingRecord.getType() == hdtMiscFile) && initial && canBrowseToExistingMiscFileRecord)
        {
          setFileRecord(existingRecord);
          Platform.runLater(this::btnOkClick);
          return;
        }

        infoPopup(HyperPath.alreadyInUseMessage(newSrc, existingRecord));

        // disable moving, only enable copying
        setRB_CopyOnly();
      }
    }
    else  // chosen file is not already attached to a record
    {
      if (db.getRootPath().isSubpath(newSrc) && (db.unenteredPath().isSubpath(newSrc) == false))
      {
        rbNeither.setDisable(false);
      }
      else
      {
        if (rbNeither.isSelected())
          rbMove.setSelected(true);
        rbNeither.setDisable(db.unenteredPath().isSubpath(newSrc) == false);
      }

      rbMove.setDisable(false);
      rbCopy.setDisable(false);
    }

    tfCurrentPath.setText(srcFilePath.toString());
    tfFileName.setText(srcFilePath.getNameOnly().toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnBrowseOldClick(boolean initial)
  {
    FileChooser fileChooser = new FileChooser();

    if ((recordType == hdtWorkFile) && EnumSet.of(wtBook, wtChapter, wtThesis, wtNone, wtPaper).contains(curWork.getWorkTypeEnum()))
      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Adobe PDF file (*.pdf)", "*.pdf"));

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));
    fileChooser.setInitialDirectory(db.unenteredPath().toFile());

    setSrcFilePath(showOpenDialog(fileChooser), initial);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnUseRecordClick()
  {
    String ext = FilenameUtils.getExtension(tfFileName.getText());
    if (ext.length() > 0) ext = FilenameUtils.EXTENSION_SEPARATOR_STR + ext;
    tfFileName.setText(tfRecordName.getText() + ext);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnUseFileClick()
  {
    tfRecordName.setText(FilenameUtils.getBaseName(tfFileName.getText()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnExploreClick()
  {
    launchFile(new FilePath(tfNewPath.getText()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnBrowseNewClick()
  {
    DirectoryChooser dirChooser = new DirectoryChooser();

    FilePath chosenFilePath = new FilePath(tfNewPath.getText());

    dirChooser.setTitle("Select new location");

    dirChooser.setInitialDirectory(chosenFilePath.isDirectory() ? chosenFilePath.toFile() : db.getRootPath().toFile());

    chosenFilePath = showDirDialog(dirChooser);
    if (FilePath.isEmpty(chosenFilePath)) return;

    if (db.getRootPath().isSubpath(chosenFilePath) == false)
    {
      errorPopup("The file cannot be copied or moved outside the database folder structure.");
      return;
    }

    tfNewPath.setText(chosenFilePath.getDirOnly().toString());

    setRB_OtherLocationsOK();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (FilePath.isEmpty(srcFilePath))
      return falseWithErrorPopup("Choose a file.", btnBrowseOld);

    if ((chkDontChangeFilename.isSelected() == false) && tfFileName.getText().isEmpty())
      return falseWithErrorPopup("You must enter a destination file name.", tfFileName);

    if (tfNewPath.getText().isEmpty())
      return falseWithErrorPopup("You must enter a destination path.", tfNewPath);

    if (recordType == hdtMiscFile)
    {
      if (tfRecordName.getText().isBlank())
        return falseWithWarningPopup("The record name is blank.", tfRecordName);

      int fileTypeID = hcbType.selectedID();
      if ((fileTypeID < 1) && hcbType.getText().isEmpty())
        return falseWithErrorPopup("You must enter a file type.", cbType);
    }

    if (FilePath.isFilenameValid(tfFileName.getText()) == false)
      return falseWithErrorPopup("Invalid file name: \"" + tfFileName.getText() + '"', tfFileName);

    // check to see if destination file name currently points to a file in the database

    FilePath fileName = chkDontChangeFilename.isSelected() ? srcFilePath.getNameOnly() : new FilePath(tfFileName.getText()),
             destFilePath = rbNeither.isSelected() ? srcFilePath.getDirOnly().resolve(fileName) : new FilePath(tfNewPath.getText()).resolve(fileName);

    HDT_RecordWithPath existingRecord = HyperPath.getRecordFromFilePath(destFilePath);

    if ((existingRecord != null) && (existingRecord != curFileRecord))
      return falseWithErrorPopup(HyperPath.alreadyInUseMessage(destFilePath, existingRecord));

    boolean success = true;

    if (rbNeither.isSelected() == false)
    {
      try
      {
        if (rbCopy.isSelected())
          success = srcFilePath.copyTo(destFilePath, true);
        else if (srcFilePath.equals(destFilePath) == false)
        {
          success = srcFilePath.moveTo(destFilePath, true);
          if (success) db.unmapFilePath(srcFilePath);
        }
      }
      catch (IOException e)
      {
        return falseWithErrorPopup("Unable to " + (rbCopy.isSelected() ? "copy" : "move") + " the file. Reason: " + getThrowableMessage(e));
      }
    }
    else if (srcFilePath.equals(destFilePath) == false)
    {
      try
      {
        success = srcFilePath.renameTo(fileName.toString());

        if (success == false)
          errorPopup("Unable to rename the file.");
        else
          db.unmapFilePath(srcFilePath);
      }
      catch (IOException e)
      {
        return falseWithErrorPopup("Unable to rename the file: " + getThrowableMessage(e));
      }
    }

    if (success == false) return false;

    if ((recordType == hdtWorkFile) && (curFileRecord == null))
    {
      HDT_WorkFile workFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, destFilePath);
      if (workFile == null)
        return falseWithInternalErrorPopup(67830);

      workFile.setName(tfRecordName.getText());
      curWork.addWorkFile(workFile.getID());
    }
    else
    {
      if (recordType == hdtWorkFile)
        curFileRecord.setName(tfRecordName.getText());

      HDT_Folder folder = HyperPath.getFolderFromFilePath(destFilePath.getDirOnly(), true);
      if (folder == null)
        return falseWithInternalErrorPopup(22937);

      curFileRecord.getPath().assign(folder, destFilePath.getNameOnly());
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
