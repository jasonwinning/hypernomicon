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

import java.util.EnumSet;
import java.util.HashMap;

import org.hypernomicon.model.records.HDT_RecordType;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;
import java.io.File;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

public class NewDatabaseDlgCtrlr extends HyperDlg
{
  @FXML private Button btnOK, btnCancel;
  @FXML private CheckBox cbInst, cbFields, cbRanks, cbStatus, cbStates, cbCountries;
  @FXML private TextField tfPapers, tfBooks, tfUnentered, tfPictures, tfTopicFolders, tfMiscFiles, tfResults;

  private String newPath;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    boolean success;

    try
    {
      success              = new File(newPath + File.separator + tfPictures    .getText()).mkdirs();
      if (success) success = new File(newPath + File.separator + tfBooks       .getText()).mkdirs();
      if (success) success = new File(newPath + File.separator + tfPapers      .getText()).mkdirs();
      if (success) success = new File(newPath + File.separator + tfUnentered   .getText()).mkdirs();
      if (success) success = new File(newPath + File.separator + tfMiscFiles   .getText()).mkdirs();
      if (success) success = new File(newPath + File.separator + tfResults     .getText()).mkdirs();
      if (success) success = new File(newPath + File.separator + tfTopicFolders.getText()).mkdirs();
    }
    catch(Exception e)
    {
      return falseWithErrorMessage("An error occurred while trying to create the directories: " + e.getMessage());
    }

    return success ? true : falseWithErrorMessage("An error occurred while trying to create the directories.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EnumSet<HDT_RecordType> getChoices()
  {
    EnumSet<HDT_RecordType> choices = EnumSet.noneOf(HDT_RecordType.class);

    if (cbCountries.isSelected()) choices.add(hdtCountry);

    if (cbFields.isSelected())
    {
      choices.add(hdtField);
      choices.add(hdtSubfield);
    }

    if (cbInst.isSelected())      choices.add(hdtInstitution);
    if (cbRanks.isSelected())     choices.add(hdtRank);
    if (cbStates.isSelected())    choices.add(hdtState);
    if (cbStatus.isSelected())    choices.add(hdtPersonStatus);

    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewDatabaseDlgCtrlr create(String title, String newPath)
  {
    NewDatabaseDlgCtrlr ndd = HyperDlg.create("NewDatabaseDlg.fxml", title, true);
    ndd.init(newPath);
    return ndd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(String newPath)
  {
    tfPictures    .setText(DEFAULT_PICTURES_PATH);
    tfBooks       .setText(DEFAULT_BOOKS_PATH);
    tfPapers      .setText(DEFAULT_PAPERS_PATH);
    tfUnentered   .setText(DEFAULT_UNENTERED_PATH);
    tfMiscFiles   .setText(DEFAULT_MISC_FILES_PATH);
    tfResults     .setText(DEFAULT_RESULTS_PATH);
    tfTopicFolders.setText(DEFAULT_TOPICAL_PATH);

    this.newPath = newPath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HashMap<String, String> getFolders()
  {
    HashMap<String, String> folders = new HashMap<>();

    folders.put(PREF_KEY_PICTURES_PATH  , tfPictures    .getText());
    folders.put(PREF_KEY_BOOKS_PATH     , tfBooks       .getText());
    folders.put(PREF_KEY_PAPERS_PATH    , tfPapers      .getText());
    folders.put(PREF_KEY_UNENTERED_PATH , tfUnentered   .getText());
    folders.put(PREF_KEY_MISC_FILES_PATH, tfMiscFiles   .getText());
    folders.put(PREF_KEY_RESULTS_PATH   , tfResults     .getText());
    folders.put(PREF_KEY_TOPICAL_PATH   , tfTopicFolders.getText());

    return folders;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
