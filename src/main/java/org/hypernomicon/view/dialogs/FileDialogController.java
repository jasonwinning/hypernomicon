/*
 * Copyright 2015-2018 Jason Winning
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
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.filePath.FilePath;

import java.io.IOException;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;

public class FileDialogController extends HyperDialog
{
  @FXML private TextField tfCurrentPath;
  @FXML public TextField tfRecordName;
  @FXML private TextField tfFileName;
  @FXML private TextField tfNewPath;
  @FXML private RadioButton rbMove;
  @FXML private RadioButton rbCopy;
  @FXML private RadioButton rbNeither;
  @FXML private CheckBox chkDontChangeFilename;
  @FXML private Label lblName;
  @FXML private Button btnLaunch;
  @FXML private Button btnBrowseOld;
  @FXML private Button btnUseFile;
  @FXML private Button btnExplore;
  @FXML private Button btnBrowseNew;
  @FXML private Button btnUseRecord;
  @FXML private Button btnOk;
  @FXML public Button btnCancel;
  
  public FilePath srcFilePath;
  private HDT_RecordWithPath curFileRecord;
  private HDT_RecordType recordType;
  private HDT_Work curWork;
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

  public static FileDialogController create(String title, HDT_RecordType recordType, HDT_RecordWithPath curFileRecord, HDT_Work curWork, String recordName)
  {
    FileDialogController fdc = HyperDialog.create("FileDialog.fxml", title, true);
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
    this.srcFilePath = null;
    copyOnly = false;
    
    tfFileName.disableProperty().bind(chkDontChangeFilename.selectedProperty());
    
    if (curFileRecord != null)
    {
      srcFilePath = curFileRecord.getPath().getFilePath();
      if (FilePath.isEmpty(srcFilePath) == false)
      {
        tfCurrentPath.setText(srcFilePath.toString());
        tfNewPath.setText(srcFilePath.getDirOnly().toString());
      }
      
      setRB_CurrentLocationOnly();      
    }
    
    rbNeither.setDisable(FilePath.isEmpty(srcFilePath));
    
    if (recordType == hdtWorkFile)
    {
      lblName.setText("This will be the description");
      btnUseRecord.setText("Use description");
           
      if (curFileRecord != null)
        tfRecordName.setText(curFileRecord.name());
      else
      {             
        FilePath newFilePath = null;
        
        if (curWork.workFiles.size() > 0)
          newFilePath = curWork.getPath().getFilePath();
          
        if (FilePath.isEmpty(newFilePath) == false)
          tfNewPath.setText(newFilePath.getDirOnly().toString());
        else
          tfNewPath.setText(db.getPath(PREF_KEY_TOPICAL_PATH, null).toString());           
      }
    }
    else
    {
      if (FilePath.isEmpty(srcFilePath) == false)
        tfNewPath.setText(srcFilePath.getDirOnly().toString());
      
      if (tfNewPath.getText().length() == 0)
        tfNewPath.setText(db.getPath(PREF_KEY_MISC_FILES_PATH, null).toString());
      
      tfRecordName.setText(recordName);
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
        if (curFileRecord.getPath().isEmpty())
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

  @FXML private void btnBrowseOldClick()
  {
    FileChooser fileChooser = new FileChooser();
    
    if (recordType == hdtWorkFile)
    {
      WorkTypeEnum enumVal = curWork.getWorkTypeValue();
      
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
    fileChooser.setInitialDirectory(db.getPath(PREF_KEY_UNENTERED_PATH, null).toFile());

    FilePath chosenFilePath = new FilePath(fileChooser.showOpenDialog(getStage()));

    if (FilePath.isEmpty(chosenFilePath)) return;
    
    srcFilePath = chosenFilePath;
    copyOnly = false;
    
    // See if the chosen file is currently assigned to a file record
    
    HDT_RecordWithPath existingRecord = getRecordFromFilePath(chosenFilePath);
    
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
      if (db.getRootFilePath().isSubpath(chosenFilePath))
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

  private HDT_RecordWithPath getRecordFromFilePath(FilePath filePath)
  {
    HDT_RecordWithPath record, firstRecord = null;
    Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);
    
    if (set == null) return null;
    if (set.size() == 0) return null;
    
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
    String name = FilenameUtils.getBaseName(tfFileName.getText());
    tfRecordName.setText(name);
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
      dirChooser.setInitialDirectory(db.getRootFilePath().toFile());
        
    chosenFilePath = new FilePath(dirChooser.showDialog(dialogStage));
    if (FilePath.isEmpty(chosenFilePath)) return;

    if (db.getRootFilePath().isSubpath(chosenFilePath) == false)
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
    boolean success = true;
    FilePath destFilePath = null;
    
    if (FilePath.isEmpty(srcFilePath))
    {
      messageDialog("You must enter a source file name.", mtError);
      safeFocus(tfCurrentPath);
      return false;       
    }

    if ((chkDontChangeFilename.isSelected() == false) && (tfFileName.getText().length() == 0))
    {
      messageDialog("You must enter a destination file name.", mtError);
      safeFocus(tfFileName);
      return false;
    }          
    
    if (tfNewPath.getText().length() == 0)
    {
      messageDialog("You must enter a destination path.", mtError);
      safeFocus(tfNewPath);
      return false;
    }

    // check to see if destination file name currently points to a file in the database
    
    FilePath fileName;
    
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
          messageDialog("Destination file name is already in use as a miscellaneous file, record ID: " + existingRecord.getID(), mtError);
        else if (existingRecord.getType() == hdtWorkFile)
        {
          HDT_WorkFile workFile = HDT_WorkFile.class.cast(existingRecord);
          if (workFile.works.size() > 0)
            messageDialog("Destination file name is already in use as a work file, work record ID: " + workFile.works.get(0).getID(), mtError);
          else
            messageDialog("Destination file name is already in use as a work file, ID: " + workFile.getID(), mtError);
        }
        else if (existingRecord.getType() == hdtPerson)
          messageDialog("Destination file name is already in use as a picture, person record ID: " + existingRecord.getID(), mtError);
        else
          messageDialog("Destination file name is already in use, record ID: " + existingRecord.getID(), mtError);
        
        return false;
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
        messageDialog("Unable to " + (rbCopy.isSelected() ? "copy" : "move") + " the file. Reason: " + e.getMessage(), mtError);
        return false;      
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
          success = false;
          messageDialog("Unable to rename the file: " + e.getMessage(), mtError);
        }
        
      }
    }
    
    if (success)
    {      
      if ((recordType == hdtWorkFile) && (curFileRecord == null))
      {
        HDT_WorkFile workFile;
        
        workFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, destFilePath);
        if (workFile == null)
        {
          messageDialog("Internal error #67830", mtError);
          return false;
        }
        
        workFile.setName(tfRecordName.getText());
        curWork.addWorkFile(workFile.getID(), true, true);
      }
      else
      {
        if (recordType == hdtWorkFile)
        {
          HDT_WorkFile.class.cast(curFileRecord).setName(tfRecordName.getText());
        }
        
        HDT_Folder folder = HyperPath.getFolderFromFilePath(destFilePath.getDirOnly(), true);
        if (folder == null)
        {
          messageDialog("Internal error 22937", mtError);
          return false;
        }
        
        curFileRecord.getPath().assign(folder, destFilePath.getNameOnly());                
      }
    }
      
    return success;
  }

  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
