/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.view.dialogs;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import java.io.IOException;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;

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
  @FXML private Button btnUseRecord;
  @FXML private CheckBox chkDontChangeFilename;
  @FXML private Label lblName;
  @FXML private RadioButton rbCopy, rbMove, rbNeither;
  @FXML private TextField tfCurrentPath, tfFileName, tfNewPath;
  @FXML public Button btnCancel;
  @FXML public ComboBox<HyperTableCell> cbType;
  @FXML public TextField tfRecordName;

  private FilePath srcFilePath;
  private HDT_RecordWithPath curFileRecord;
  private HDT_RecordType recordType;
  private HDT_Work curWork;
  private HyperCB hcbType;
  private boolean copyOnly;

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

  public static FileDlgCtrlr create(String title, HDT_RecordType recordType, HDT_RecordWithPath curFileRecord, HDT_Work curWork, String recordName)
  {
    FileDlgCtrlr fdc = HyperDlg.create("FileDlg.fxml", title, true);
    fdc.init(recordType, curFileRecord, curWork, recordName);
    return fdc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_RecordType recordType, HDT_RecordWithPath curFileRecord, HDT_Work curWork, String recordName)
  {
    this.curFileRecord = curFileRecord;
    this.curWork = curWork;
    this.recordType = recordType;
    srcFilePath = null;
    copyOnly = false;

    hcbType = new HyperCB(cbType, ctDropDown, new StandardPopulator(hdtFileType), null);

    tfFileName.disableProperty().bind(chkDontChangeFilename.selectedProperty());

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

        if (FilePath.isEmpty(newFilePath) == false)
          tfNewPath.setText(newFilePath.getDirOnly().toString());
        else
          tfNewPath.setText(db.topicalPath().toString());
      }
    }
    else
    {
      if (tfNewPath.getText().length() == 0)
      {
        if (FilePath.isEmpty(srcFilePath) == false)
          tfNewPath.setText(srcFilePath.getDirOnly().toString());
        else
          tfNewPath.setText(db.miscFilesPath().toString());
      }

      tfRecordName.setText(recordName);
      if (curFileRecord != null)
        hcbType.addAndSelectEntry(HDT_MiscFile.class.cast(curFileRecord).fileType, HDT_Record::getCBText);
    }

    if (FilePath.isEmpty(srcFilePath) == false)
      tfFileName.setText(srcFilePath.getNameOnly().toString());

    tfCurrentPath.setEditable(false);
    tfNewPath.setEditable(false);

    rbCopy.setDisable(true); // The only way copy is allowed is if the user selects a file by clicking browse first

    onShown = () ->
    {
      if (recordType == hdtWorkFile)
      {
        if (curFileRecord == null)
          btnBrowseOldClick();
      }
      else
      {
        if (FilePath.isEmpty(srcFilePath))
          btnBrowseOldClick();
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnLaunchClick()
  {
    launchFile(srcFilePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setSrcFilePath(FilePath newSrc)
  {
    if (FilePath.isEmpty(newSrc)) return;

    if (db.isProtectedFile(newSrc))
    {
      messageDialog("That file cannot be assigned to a record.", mtError);
      return;
    }

    srcFilePath = newSrc;
    copyOnly = false;

    // See if the chosen file is currently assigned to a file record

    HDT_RecordWithPath existingRecord = getRecordFromFilePath(newSrc);

    if (existingRecord != null)
    {
      if (existingRecord == curFileRecord) // chosen file is the one already attached to this record
      {
        setRB_CurrentLocationOnly();
      }
      else
      {
        if (existingRecord.getType() == hdtMiscFile)
          messageDialog("That file is already in use as a miscellaneous file, record ID: " + existingRecord.getID(), mtInformation);
        else if (existingRecord.getType() == hdtWorkFile)
        {
          HDT_WorkFile workFile = HDT_WorkFile.class.cast(existingRecord);
          if (workFile.works.size() > 0)
            messageDialog("That file is already in use as a work file, work record ID: " + workFile.works.get(0).getID(), mtInformation);
          else
            messageDialog("That file is already in use as a work file, ID: " + workFile.getID(), mtInformation);
        }
        else if (existingRecord.getType() == hdtPerson)
          messageDialog("That file is already in use as a picture, person record ID: " + existingRecord.getID(), mtInformation);
        else
          messageDialog("That file is already in use, record ID: " + existingRecord.getID(), mtInformation);

        // disable moving, only enable copying
        setRB_CopyOnly();
      }
    }
    else  // chosen file is not already attached to a record
    {
      if (db.getRootPath().isSubpath(newSrc))
      {
        rbNeither.setDisable(false);
      }
      else
      {
        if (rbNeither.isSelected())
          rbMove.setSelected(true);
        rbNeither.setDisable(true);
      }

      rbMove.setDisable(false);
      rbCopy.setDisable(false);
    }

    tfCurrentPath.setText(srcFilePath.toString());
    tfFileName.setText(srcFilePath.getNameOnly().toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnBrowseOldClick()
  {
    FileChooser fileChooser = new FileChooser();

    if (recordType == hdtWorkFile)
    {
      WorkTypeEnum enumVal = curWork.getWorkTypeEnum();

      switch (enumVal)
      {
        case wtBook: case wtChapter: case wtNone: case wtPaper:

          fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Adobe PDF file (*.pdf)", "*.pdf"));
          break;

        default :
          break;
      }
    }

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));
    fileChooser.setInitialDirectory(db.unenteredPath().toFile());

    setSrcFilePath(new FilePath(fileChooser.showOpenDialog(getStage())));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_RecordWithPath getRecordFromFilePath(FilePath filePath)
  {
    HDT_RecordWithPath record, firstRecord = null;
    Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);

    if (collEmpty(set)) return null;

    for (HyperPath setPath : set)
    {
      record = setPath.getRecord();
      if (record != null)
      {
        if (record.getType() == hdtPerson) return record;
        if ((record.getType() != hdtFolder) && (record.getType() != recordType)) return record;
        if (firstRecord == null) firstRecord = record;
      }
    }

    return firstRecord;
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

    if (chosenFilePath.exists() && chosenFilePath.isDirectory())
      dirChooser.setInitialDirectory(chosenFilePath.toFile());
    else
      dirChooser.setInitialDirectory(db.getRootPath().toFile());

    chosenFilePath = new FilePath(dirChooser.showDialog(dialogStage));
    if (FilePath.isEmpty(chosenFilePath)) return;

    if (db.getRootPath().isSubpath(chosenFilePath) == false)
    {
      messageDialog("The file cannot be copied or moved outside the database folder structure.", mtError);
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
      return falseWithErrorMessage("You must enter a source file name.", tfCurrentPath);

    if ((chkDontChangeFilename.isSelected() == false) && (tfFileName.getText().length() == 0))
      return falseWithErrorMessage("You must enter a destination file name.", tfFileName);

    if (tfNewPath.getText().length() == 0)
      return falseWithErrorMessage("You must enter a destination path.", tfNewPath);

    if (recordType == hdtMiscFile)
    {
      int fileTypeID = hcbType.selectedID();
      if ((fileTypeID < 1) && (hcbType.getText().length() == 0))
        return falseWithErrorMessage("You must enter a file type.", cbType);
    }

    // check to see if destination file name currently points to a file in the database

    FilePath fileName;
    boolean success = true;
    FilePath destFilePath = null;

    if (chkDontChangeFilename.isSelected())
      fileName = srcFilePath.getNameOnly();
    else
      fileName = new FilePath(tfFileName.getText());

    if (rbNeither.isSelected())
      destFilePath = srcFilePath.getDirOnly().resolve(fileName);
    else
      destFilePath = new FilePath(tfNewPath.getText()).resolve(fileName);

    HDT_RecordWithPath existingRecord = getRecordFromFilePath(destFilePath);

    if (existingRecord != null)
    {
      if (existingRecord == curFileRecord) // chosen file is the one already attached to this record
      {
        // Nothing needs to be done here
      }
      else
      {
        if (existingRecord.getType() == hdtMiscFile)
          return falseWithErrorMessage("Destination file name is already in use as a miscellaneous file, record ID: " + existingRecord.getID());
        else if (existingRecord.getType() == hdtWorkFile)
        {
          HDT_WorkFile workFile = HDT_WorkFile.class.cast(existingRecord);
          if (workFile.works.size() > 0)
            return falseWithErrorMessage("Destination file name is already in use as a work file, work record ID: " + workFile.works.get(0).getID());
          else
            return falseWithErrorMessage("Destination file name is already in use as a work file, ID: " + workFile.getID());
        }
        else if (existingRecord.getType() == hdtPerson)
          return falseWithErrorMessage("Destination file name is already in use as a picture, person record ID: " + existingRecord.getID());
        else
          return falseWithErrorMessage("Destination file name is already in use, record ID: " + existingRecord.getID());
      }
    }

    if (rbNeither.isSelected() == false)
    {
      try
      {
        if (rbCopy.isSelected())
          success = srcFilePath.copyTo(destFilePath, true);
        else
        {
          if (srcFilePath.equals(destFilePath))
            success = true;
          else
          {
            success = srcFilePath.moveTo(destFilePath, true);
            if (success) db.unmapFilePath(srcFilePath);
          }
        }
      }
      catch (IOException e)
      {
        return falseWithErrorMessage("Unable to " + (rbCopy.isSelected() ? "copy" : "move") + " the file. Reason: " + e.getMessage());
      }
    }
    else
    {
      if (srcFilePath.equals(destFilePath))
        success = true;
      else
      {
        try
        {
          success = srcFilePath.renameTo(fileName.toString());

          if (success == false)
            messageDialog("Unable to rename the file.", mtError);
          else
            db.unmapFilePath(srcFilePath);
        }
        catch (IOException e)
        {
          return falseWithErrorMessage("Unable to rename the file: " + e.getMessage());
        }
      }
    }

    if (!success) return false;

    if ((recordType == hdtWorkFile) && (curFileRecord == null))
    {
      HDT_WorkFile workFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, destFilePath);
      if (workFile == null)
        return falseWithErrorMessage("Internal error #67830");

      workFile.setName(tfRecordName.getText());
      curWork.addWorkFile(workFile.getID(), true, true);
    }
    else
    {
      if (recordType == hdtWorkFile)
        HDT_WorkFile.class.cast(curFileRecord).setName(tfRecordName.getText());

      HDT_Folder folder = HyperPath.getFolderFromFilePath(destFilePath.getDirOnly(), true);
      if (folder == null)
        return falseWithErrorMessage("Internal error 22937");

      curFileRecord.getPath().assign(folder, destFilePath.getNameOnly());
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
