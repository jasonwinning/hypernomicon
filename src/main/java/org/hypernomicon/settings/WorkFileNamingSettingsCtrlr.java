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

package org.hypernomicon.settings;

import static org.hypernomicon.App.app;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;

import java.util.Map.Entry;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.model.records.HDT_WorkFile.FileNameAuthor;
import org.hypernomicon.settings.SettingsDlgCtrlr.SettingsControl;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.layout.VBox;
import javafx.stage.Window;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class WorkFileNamingSettingsCtrlr implements SettingsControl
{
  @FXML private CheckBox chkAddInitial, chkLowercase, chkPosix, chkTreatEdAsAuthor, chkYearLetter;
  @FXML private ComboBox<String> cbComponent1, cbComponent2, cbComponent3, cbComponent4, cbComponent5;
  @FXML private Label lblExample;
  @FXML private TextField tfExample, tfMaxChar, tfTest1, tfTest2, tfTest3, tfTest4, tfTest5,
                tfSepAfter1, tfSepAfter2, tfSepAfter3, tfSepAfter4, tfSepAfter5,
                tfSepBefore1, tfSepBefore2, tfSepBefore3, tfSepBefore4, tfSepBefore5,
                tfSepWithin1, tfSepWithin2, tfSepWithin3, tfSepWithin4, tfSepWithin5;
  @FXML private VBox vbox;

  private final Map<String, Integer> componentMap = new LinkedHashMap<>();

  @Override public void save() { return; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(Window owner, boolean noDB)
  {
    componentMap.put("Author last names", AUTHOR_FN_COMPONENT);
    componentMap.put("Year", YEAR_FN_COMPONENT);
    componentMap.put("Title (no subtitle)", TITLE_FN_COMPONENT);
    componentMap.put("Translators", TRANS_FN_COMPONENT);
    componentMap.put("Editors", EDITOR_FN_COMPONENT);
    componentMap.put("", BLANK_FN_COMPONENT);

    lblExample.setOnMouseClicked(event -> refreshExample());

    if (noDB) return;

    initDBTextField(tfSepWithin1, PREF_KEY_FN_WITHIN_SEP_1);
    initDBTextField(tfSepWithin2, PREF_KEY_FN_WITHIN_SEP_2);
    initDBTextField(tfSepWithin3, PREF_KEY_FN_WITHIN_SEP_3);
    initDBTextField(tfSepWithin4, PREF_KEY_FN_WITHIN_SEP_4);
    initDBTextField(tfSepWithin5, PREF_KEY_FN_WITHIN_SEP_5);

    initDBTextField(tfSepBefore1, PREF_KEY_FN_BEFORE_SEP_1);
    initDBTextField(tfSepBefore2, PREF_KEY_FN_BEFORE_SEP_2);
    initDBTextField(tfSepBefore3, PREF_KEY_FN_BEFORE_SEP_3);
    initDBTextField(tfSepBefore4, PREF_KEY_FN_BEFORE_SEP_4);
    initDBTextField(tfSepBefore5, PREF_KEY_FN_BEFORE_SEP_5);

    initDBTextField(tfSepAfter1, PREF_KEY_FN_AFTER_SEP_1);
    initDBTextField(tfSepAfter2, PREF_KEY_FN_AFTER_SEP_2);
    initDBTextField(tfSepAfter3, PREF_KEY_FN_AFTER_SEP_3);
    initDBTextField(tfSepAfter4, PREF_KEY_FN_AFTER_SEP_4);
    initDBTextField(tfSepAfter5, PREF_KEY_FN_AFTER_SEP_5);

    initDBTextField(tfTest1, PREF_KEY_FN_TEST_1);
    initDBTextField(tfTest2, PREF_KEY_FN_TEST_2);
    initDBTextField(tfTest3, PREF_KEY_FN_TEST_3);
    initDBTextField(tfTest4, PREF_KEY_FN_TEST_4);
    initDBTextField(tfTest5, PREF_KEY_FN_TEST_5);

    initCheckBox(chkTreatEdAsAuthor, PREF_KEY_FN_TREAT_ED_AS_AUTHOR, true);
    initCheckBox(chkAddInitial, PREF_KEY_FN_ADD_INITIAL, false);
    initCheckBox(chkYearLetter, PREF_KEY_FN_YEAR_LETTER, false);
    initCheckBox(chkPosix, PREF_KEY_FN_POSIX, false);
    initCheckBox(chkLowercase, PREF_KEY_FN_LOWERCASE, false);

    initMaxChar(tfMaxChar, PREF_KEY_FN_MAX_CHAR);

    initComponentCB(cbComponent1, PREF_KEY_FN_COMPONENT_1, AUTHOR_FN_COMPONENT);
    initComponentCB(cbComponent2, PREF_KEY_FN_COMPONENT_2, EDITOR_FN_COMPONENT);
    initComponentCB(cbComponent3, PREF_KEY_FN_COMPONENT_3, TRANS_FN_COMPONENT);
    initComponentCB(cbComponent4, PREF_KEY_FN_COMPONENT_4, YEAR_FN_COMPONENT);
    initComponentCB(cbComponent5, PREF_KEY_FN_COMPONENT_5, TITLE_FN_COMPONENT);

    if (app.debugging == false)
      vbox.getChildren().remove(1, 3);

    refreshExample();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initCheckBox(CheckBox chk, String prefKey, boolean defValue)
  {
    chk.setSelected(db.prefs.getBoolean(prefKey, defValue));
    chk.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      db.prefs.putBoolean(prefKey, newValue);
      refreshExample();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initComponentCB(ComboBox<String> cb, String prefKey, int defValue)
  {
    ObservableList<String> choices = FXCollections.observableArrayList(componentMap.keySet());

    cb.setItems(null);
    cb.setItems(choices);

    int selNdx = 0, selCode = db.prefs.getInt(prefKey, defValue);
    db.prefs.putInt(prefKey, selCode);

    for (Entry<String, Integer> entry : componentMap.entrySet())
      if (entry.getValue() == selCode)
        selNdx = cb.getItems().indexOf(entry.getKey());

    cb.getSelectionModel().select(selNdx);

    cb.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      db.prefs.putInt(prefKey, componentMap.get(newValue));
      refreshExample();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initMaxChar(TextField tf, String prefKey)
  {
    tf.setText(String.valueOf(db.prefs.getInt(prefKey, 255)));

    tf.setTextFormatter(new TextFormatter<>(change ->
    {
      if (change.getText().matches(".*[^0-9].*") && change.isAdded())
        change.setText("");

      return change;
    }));

    tf.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      int intVal = parseInt(newValue, -1);
      if (intVal < 1)
        intVal = 255;

      if (intVal < 14)
        intVal = 14;

      db.prefs.putInt(prefKey, intVal);
      refreshExample();
    });

    tf.focusedProperty().addListener((obs, ov, nv) ->
    {
      if (Boolean.FALSE.equals(nv))
        tf.setText(String.valueOf(db.prefs.getInt(prefKey, 255)));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshExample()
  {
    String author = "", title = "", year = "", trans = "", editor = "";

    for (int ndx = 1; ndx <= 5; ndx++)
    {
      int code = BLANK_FN_COMPONENT;
      String value = "";

      switch (ndx)
      {
        case 1 :
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_1, BLANK_FN_COMPONENT);
          value = db.prefs.get(PREF_KEY_FN_TEST_1, "");
          break;

        case 2 :
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_2, BLANK_FN_COMPONENT);
          value = db.prefs.get(PREF_KEY_FN_TEST_2, "");
          break;

        case 3 :
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_3, BLANK_FN_COMPONENT);
          value = db.prefs.get(PREF_KEY_FN_TEST_3, "");
          break;

        case 4 :
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_4, BLANK_FN_COMPONENT);
          value = db.prefs.get(PREF_KEY_FN_TEST_4, "");
          break;

        case 5 :
          code = db.prefs.getInt(PREF_KEY_FN_COMPONENT_5, BLANK_FN_COMPONENT);
          value = db.prefs.get(PREF_KEY_FN_TEST_5, "");
          break;
      }

      switch (code)
      {
        case AUTHOR_FN_COMPONENT : author = ultraTrim(value); break;
        case TITLE_FN_COMPONENT  : title  = ultraTrim(value); break;
        case YEAR_FN_COMPONENT   : year   = ultraTrim(value); break;
        case TRANS_FN_COMPONENT  : trans  = ultraTrim(value); break;
        case EDITOR_FN_COMPONENT : editor = ultraTrim(value); break;
      }
    }

    List<FileNameAuthor> authors = new ArrayList<>();

    for (String authorStr : author.split(";"))
    {
      String trAuthorStr = ultraTrim(authorStr);
      if (trAuthorStr.length() > 0)
        authors.add(new FileNameAuthor(trAuthorStr, false, false));
    }

    for (String transStr : trans.split(";"))
    {
      String trTransStr = ultraTrim(transStr);
      if (trTransStr.length() > 0)
        authors.add(new FileNameAuthor(trTransStr, false, true));
    }

    for (String edStr : editor.split(";"))
    {
      String trEdStr = ultraTrim(edStr);
      if (trEdStr.length() > 0)
        authors.add(new FileNameAuthor(trEdStr, true, false));
    }

    tfExample.setText(HDT_WorkFile.makeFileName(authors, year, title, "pdf"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initDBTextField(TextField tf, String prefKey)
  {
    tf.setText(db.prefs.get(prefKey, ""));

    tf.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;
      db.prefs.put(prefKey, newValue);
      refreshExample();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
