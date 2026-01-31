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

package org.hypernomicon.dialogs;

import java.io.IOException;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.FilePathSet;

import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;

//---------------------------------------------------------------------------

public class NewDatabaseDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private CheckBox chkInst, chkFields, chkRanks, chkStatus, chkRegions, chkCountries;
  @FXML private TextField tfPapers, tfBooks, tfUnentered, tfPictures, tfTopicFolders, tfMiscFiles, tfResults;

  private final String newPath;

//---------------------------------------------------------------------------

  public NewDatabaseDlgCtrlr(String newPath)
  {
    super("NewDatabaseDlg", "Customize How Database Will Be Created", true);

    tfPictures    .setText(DEFAULT_PICTURES_PATH);
    tfBooks       .setText(DEFAULT_BOOKS_PATH);
    tfPapers      .setText(DEFAULT_PAPERS_PATH);
    tfUnentered   .setText(DEFAULT_UNENTERED_PATH);
    tfMiscFiles   .setText(DEFAULT_MISC_FILES_PATH);
    tfResults     .setText(DEFAULT_RESULTS_PATH);
    tfTopicFolders.setText(DEFAULT_TOPICAL_PATH);

    this.newPath = newPath;

    chkCountries.selectedProperty().addListener((ob, ov, nv) ->
    {
      if (Boolean.TRUE.equals(nv) == false)
        chkRegions.setSelected(false);
    });

    chkRegions.selectedProperty().addListener((ob, ov, nv) ->
    {
      if (Boolean.TRUE.equals(nv))
        chkCountries.setSelected(true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean addNameToSet(FilePathSet set, String name)
  {
    name = name.strip();

    if (name.isEmpty())
      return falseWithErrorPopup("Folder name cannot be blank.");

    if (name.equalsIgnoreCase(DEFAULT_XML_PATH))
      return falseWithErrorPopup("The name XML is resevered for the XML folder.");

    if ((FilePath.isFilenameValid(name) == false) || (name.equals(new FilePath(name).getNameOnly().toString()) == false))
      return falseWithErrorPopup("Folder name is invalid: " + name);

    set.add(new FilePath(name));
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    FilePathSet set = new FilePathSet();

    if (! (addNameToSet(set, tfPictures    .getText()) &&
           addNameToSet(set, tfBooks       .getText()) &&
           addNameToSet(set, tfPapers      .getText()) &&
           addNameToSet(set, tfUnentered   .getText()) &&
           addNameToSet(set, tfMiscFiles   .getText()) &&
           addNameToSet(set, tfResults     .getText()) &&
           addNameToSet(set, tfTopicFolders.getText())))
      return false;

    if (set.size() < 7)
      return falseWithErrorPopup("Enter a unique name for each folder.");

    FilePath filePath = new FilePath(newPath);

    try
    {
      saveStringBuilderToFile(new StringBuilder(DEFAULT_XML_PATH + '/' + SETTINGS_FILE_NAME), filePath.resolve(HDB_DEFAULT_FILENAME), XML_FILES_CHARSET);

      filePath.resolve(DEFAULT_XML_PATH                ).createDirectory();
      filePath.resolve(tfPictures    .getText().strip()).createDirectory();
      filePath.resolve(tfBooks       .getText().strip()).createDirectory();
      filePath.resolve(tfPapers      .getText().strip()).createDirectory();
      filePath.resolve(tfUnentered   .getText().strip()).createDirectory();
      filePath.resolve(tfMiscFiles   .getText().strip()).createDirectory();
      filePath.resolve(tfResults     .getText().strip()).createDirectory();
      filePath.resolve(tfTopicFolders.getText().strip()).createDirectory();
    }
    catch (IOException e)
    {
      return falseWithErrorPopup("Unable to create new database: " + getThrowableMessage(e));
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EnumSet<RecordType> getChoices()
  {
    EnumSet<RecordType> choices = EnumSet.noneOf(RecordType.class);

    if (chkCountries.isSelected()) choices.add(hdtCountry);
    if (chkRegions  .isSelected()) choices.add(hdtRegion);

    if (chkFields.isSelected())
    {
      choices.add(hdtField);
      choices.add(hdtSubfield);
    }

    if (chkInst  .isSelected ())   choices.add(hdtInstitution);
    if (chkRanks .isSelected ())   choices.add(hdtRank);
    if (chkStatus.isSelected ())   choices.add(hdtPersonStatus);

    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Map<String, String> getFolders()
  {
    Map<String, String> folderMap = new HashMap<>();

    folderMap.put(FolderIDPrefKey.PICTURES  , tfPictures    .getText().strip());
    folderMap.put(FolderIDPrefKey.BOOKS     , tfBooks       .getText().strip());
    folderMap.put(FolderIDPrefKey.PAPERS    , tfPapers      .getText().strip());
    folderMap.put(FolderIDPrefKey.UNENTERED , tfUnentered   .getText().strip());
    folderMap.put(FolderIDPrefKey.MISC_FILES, tfMiscFiles   .getText().strip());
    folderMap.put(FolderIDPrefKey.RESULTS   , tfResults     .getText().strip());
    folderMap.put(FolderIDPrefKey.TOPICAL   , tfTopicFolders.getText().strip());

    return folderMap;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
