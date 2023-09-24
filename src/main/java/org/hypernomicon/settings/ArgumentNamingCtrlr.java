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

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.hypernomicon.settings.SettingsDlgCtrlr.SettingsControl;
import org.hypernomicon.settings.WorkSearchKeySettings.FinalConjunctionSymbol;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.stage.Window;

//---------------------------------------------------------------------------

public class ArgumentNamingCtrlr implements SettingsControl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private CheckBox chkLowerCaseTargetNames, chkMultipleAuthors, chkOxfordComma;
  @FXML private TextArea taExamples;
  @FXML private TextField tfTruncateNum, tfTruncationIndicator, tfNumToShowWhenTruncating;
  @FXML private ToggleButton btnAnd, btnAmpersand;
  @FXML private ToggleGroup grpConj;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(Window owner, boolean noDB)
  {
    if (noDB) return;

    ArgumentNamingSettings settings = new ArgumentNamingSettings();

    chkLowerCaseTargetNames.setSelected(settings.lowerCaseTargetNames);
    chkMultipleAuthors     .setSelected(settings.multipleAuthors);
    chkOxfordComma         .setSelected(settings.oxfordSeparator);

    chkLowerCaseTargetNames.selectedProperty().addListener((ob, ov, nv) -> refreshExamples());
    chkMultipleAuthors     .selectedProperty().addListener((ob, ov, nv) -> refreshExamples());
    chkOxfordComma         .selectedProperty().addListener((ob, ov, nv) -> refreshExamples());

    tfTruncationIndicator.setText(settings.truncationIndicator);

    tfTruncationIndicator.textProperty().addListener((ob, ov, nv) -> refreshExamples());

    initTruncateNum  (tfTruncateNum            , settings.authorNumToTruncate        , this::refreshExamples);
    initAuthorsToShow(tfNumToShowWhenTruncating, settings.authorsToShowWhenTruncating, this::refreshExamples);

    grpConj.selectToggle(Map.of(FinalConjunctionSymbol.and, btnAnd, FinalConjunctionSymbol.ampersand, btnAmpersand).getOrDefault(settings.finalConjSymbol, null));

    grpConj.selectedToggleProperty().addListener((ob, ov, nv) -> refreshExamples());

    refreshExamples();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void save(boolean noDB)
  {
    if (noDB) return;

    getSettingsFromUI().saveToPrefNode();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ArgumentNamingSettings getSettingsFromUI()
  {
    String truncationIndicator = tfTruncationIndicator.getText();

    boolean lowerCaseTargetNames = chkLowerCaseTargetNames.isSelected(),
            multipleAuthors = chkMultipleAuthors.isSelected(),
            oxfordSeparator = chkOxfordComma.isSelected();

    FinalConjunctionSymbol finalConjSymbol = nullSwitch(grpConj.getSelectedToggle(), FinalConjunctionSymbol.none, Map.of(btnAnd, FinalConjunctionSymbol.and, btnAmpersand, FinalConjunctionSymbol.ampersand )::get);

    int authorNumToTruncate = parseInt(tfTruncateNum.getText(), -1);
    if (authorNumToTruncate < 2)
      authorNumToTruncate = -1;

    int authorsToShowWhenTruncating = parseInt(tfNumToShowWhenTruncating.getText(), -1);
    if (authorsToShowWhenTruncating < 1)
      authorsToShowWhenTruncating = 2;

    return new ArgumentNamingSettings(truncationIndicator, lowerCaseTargetNames, multipleAuthors, oxfordSeparator, finalConjSymbol, authorNumToTruncate, authorsToShowWhenTruncating);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshExamples()
  {
    Platform.runLater(() ->
    {
      String lastPart = "'s argument that...";

      ArgumentNamingSettings settings = getSettingsFromUI();

      List<String> strList = new ArrayList<>();
      strList.add("1 author: " + settings.format(List.of("Smith")) + lastPart);

      if (settings.multipleAuthors)
      {
        strList.add("2 authors: " + settings.format(List.of("Smith", "Jones"                           )) + lastPart);
        strList.add("3 authors: " + settings.format(List.of("Smith", "Jones", "Nguyen"                 )) + lastPart);
        strList.add("4 authors: " + settings.format(List.of("Smith", "Jones", "Nguyen", "Garcia"       )) + lastPart);
        strList.add("5 authors: " + settings.format(List.of("Smith", "Jones", "Nguyen", "Garcia", "Kim")) + lastPart);
      }

      taExamples.setText(strListToStr(strList, false));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void initAuthorsToShow(TextField tf, int startVal, Runnable refreshHandler)
  {
    if (startVal < 1) startVal = 1;
    tf.setText(String.valueOf(startVal));

    tf.setTextFormatter(new TextFormatter<>(change ->
    {
      if (change.getText().matches(".*[^0-9].*") && change.isAdded())
        change.setText("");

      return change;
    }));

    tf.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      refreshHandler.run();
    });

    tf.focusedProperty().addListener((obs, ov, nv) ->
    {
      if (Boolean.FALSE.equals(nv))
      {
        int intVal = parseInt(tf.getText(), 2);
        if (intVal < 1)
          intVal = 2;

        tf.setText(String.valueOf(intVal));
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void initTruncateNum(TextField tf, int startVal, Runnable refreshHandler)
  {
    tf.setText(startVal > 1 ? String.valueOf(startVal) : "");

    tf.setTextFormatter(new TextFormatter<>(change ->
    {
      if (change.getText().matches(".*[^0-9].*") && change.isAdded())
        change.setText("");

      return change;
    }));

    tf.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      refreshHandler.run();
    });

    tf.focusedProperty().addListener((obs, ov, nv) ->
    {
      if (Boolean.FALSE.equals(nv))
      {
        int intVal = parseInt(tf.getText(), -1);
        if (intVal < 2)
          tf.setText("");
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
