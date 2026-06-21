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

package org.hypernomicon.settings;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;

import org.hypernomicon.settings.SettingsDlgCtrlr.SettingsControl;
import org.hypernomicon.settings.WorkSearchKeySettings.FinalConjunctionSymbol;
import org.hypernomicon.view.wrappers.IntSpinnerWrapper;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.*;

//---------------------------------------------------------------------------

public class ArgumentNamingCtrlr implements SettingsControl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private CheckBox chkLowerCaseTargetNames, chkMultipleAuthors, chkOxfordComma, chkNeverTruncate;
  @FXML private Spinner<Integer> spnTruncateNum, spnNumToShowWhenTruncating;
  @FXML private TextArea taExamples;
  @FXML private TextField tfTruncationIndicator;
  @FXML private ToggleButton btnAnd, btnAmpersand;
  @FXML private ToggleGroup grpConj;

  private IntSpinnerWrapper truncateNumWrapper, numToShowWrapper;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(boolean noDB)
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

    truncateNumWrapper = initTruncateNum  (spnTruncateNum            , chkNeverTruncate, settings.authorNumToTruncate        , this::refreshExamples);
    numToShowWrapper   = initAuthorsToShow(spnNumToShowWhenTruncating,                   settings.authorsToShowWhenTruncating, this::refreshExamples);

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
            multipleAuthors      = chkMultipleAuthors     .isSelected(),
            oxfordSeparator      = chkOxfordComma         .isSelected();

    FinalConjunctionSymbol finalConjSymbol = nullSwitch(grpConj.getSelectedToggle(), FinalConjunctionSymbol.none, Map.of(btnAnd, FinalConjunctionSymbol.and, btnAmpersand, FinalConjunctionSymbol.ampersand )::get);

    int authorNumToTruncate         = truncateNumWrapper.getValue(),
        authorsToShowWhenTruncating = numToShowWrapper  .getValue();

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

  static IntSpinnerWrapper initAuthorsToShow(Spinner<Integer> spinner, int startVal, Runnable refreshHandler)
  {
    return IntSpinnerWrapper.of(spinner, 1, 99, startVal < 1 ? 2 : startVal, refreshHandler);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Wires the author-truncation threshold to a spinner paired with a "Never" checkbox: checking the box
   * disables the spinner and represents the "never truncate" state (the {@code -1} sentinel). The spinner
   * retains its last numeric value while disabled, so unchecking the box restores a sensible threshold.
   */
  static IntSpinnerWrapper initTruncateNum(Spinner<Integer> spinner, CheckBox chkNever, int startVal, Runnable refreshHandler)
  {
    return IntSpinnerWrapper.withNone(spinner, 2, 99, startVal < 2 ? 3 : startVal, chkNever, -1, startVal < 2, refreshHandler);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
