/*
 * Copyright 2015-2023 Jason Winning
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

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;

import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;

public class NewDatabaseDlgCtrlr extends HyperDlg
{
  @FXML private CheckBox chkInst, chkFields, chkRanks, chkStatus, chkRegions, chkCountries;
  @FXML private TextField tfPapers, tfBooks, tfUnentered, tfPictures, tfTopicFolders, tfMiscFiles, tfResults;

  private String newPath;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean addNameToSet(FilePathSet set, String name)
  {
    name = ultraTrim(name);

    if (name.isEmpty())
      return falseWithErrorMessage("Folder name cannot be blank.");

    if (name.equalsIgnoreCase(DEFAULT_XML_PATH))
      return falseWithErrorMessage("The name XML is resevered for the XML folder.");

    if ((FilePath.isFilenameValid(name) == false) || (name.equals(new FilePath(name).getNameOnly().toString()) == false))
      return falseWithErrorMessage("Folder name is invalid: " + name);

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
      return falseWithErrorMessage("Enter a unique name for each folder.");

    FilePath filePath = new FilePath(newPath);

    try
    {
      saveStringBuilderToFile(new StringBuilder(DEFAULT_XML_PATH + '/' + SETTINGS_FILE_NAME), filePath.resolve(HDB_DEFAULT_FILENAME));

      filePath.resolve(DEFAULT_XML_PATH                   ).createDirectory();
      filePath.resolve(ultraTrim(tfPictures    .getText())).createDirectory();
      filePath.resolve(ultraTrim(tfBooks       .getText())).createDirectory();
      filePath.resolve(ultraTrim(tfPapers      .getText())).createDirectory();
      filePath.resolve(ultraTrim(tfUnentered   .getText())).createDirectory();
      filePath.resolve(ultraTrim(tfMiscFiles   .getText())).createDirectory();
      filePath.resolve(ultraTrim(tfResults     .getText())).createDirectory();
      filePath.resolve(ultraTrim(tfTopicFolders.getText())).createDirectory();
    }
    catch(Exception e)
    {
      return falseWithErrorMessage("An error occurred while trying to create the directories: " + e.getMessage());
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

  public static NewDatabaseDlgCtrlr build(String newPath)
  {
    return ((NewDatabaseDlgCtrlr) create("NewDatabaseDlg", "Customize How Database Will Be Created", true)).init(newPath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private NewDatabaseDlgCtrlr init(String newPath)
  {
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

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Map<String, String> getFolders()
  {
    Map<String, String> folderMap = new HashMap<>();

    folderMap.put(PREF_KEY_PICTURES_FOLDER_ID  , ultraTrim(tfPictures    .getText()));
    folderMap.put(PREF_KEY_BOOKS_FOLDER_ID     , ultraTrim(tfBooks       .getText()));
    folderMap.put(PREF_KEY_PAPERS_FOLDER_ID    , ultraTrim(tfPapers      .getText()));
    folderMap.put(PREF_KEY_UNENTERED_FOLDER_ID , ultraTrim(tfUnentered   .getText()));
    folderMap.put(PREF_KEY_MISC_FILES_FOLDER_ID, ultraTrim(tfMiscFiles   .getText()));
    folderMap.put(PREF_KEY_RESULTS_FOLDER_ID   , ultraTrim(tfResults     .getText()));
    folderMap.put(PREF_KEY_TOPICAL_FOLDER_ID   , ultraTrim(tfTopicFolders.getText()));

    return folderMap;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
