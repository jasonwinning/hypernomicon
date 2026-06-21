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

import static org.hypernomicon.util.Util.*;

import java.util.Map;

import org.hypernomicon.settings.WorkSearchKeySettings.*;
import org.hypernomicon.view.wrappers.IntSpinnerWrapper;

import javafx.fxml.FXML;
import javafx.scene.control.*;

//---------------------------------------------------------------------------

public class WorkSearchKeyCtrlr extends Tab
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private CheckBox chkMultipleAuthors, chkSepAfterPenultimate, chkNeverTruncate;
  @FXML TextArea taExamples;
  @FXML private Spinner<Integer> spnTruncateNum, spnNumToShowWhenTruncating;
  @FXML private TextField tfBeforeYearSep, tfAfterNameSep, tfTruncationIndicator;
  @FXML private ToggleButton btnAroundAll, btnAroundYear, btnAnd, btnAmpersand;
  @FXML private ToggleGroup grpParen, grpConj;

  private IntSpinnerWrapper truncateNumWrapper, numToShowWrapper;
  private WorkSearchKeysCtrlr settingsPage;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void init(WorkSearchKeysCtrlr settingsPage, WorkSearchKeyConfig keyConfig)
  {
    this.settingsPage = settingsPage;

    tfBeforeYearSep      .setText(keyConfig.beforeYearSep      );
    tfAfterNameSep       .setText(keyConfig.afterNameSep       );
    tfTruncationIndicator.setText(keyConfig.truncationIndicator);

    tfBeforeYearSep      .textProperty().addListener((ob, ov, nv) -> refreshExamples());
    tfAfterNameSep       .textProperty().addListener((ob, ov, nv) -> refreshExamples());
    tfTruncationIndicator.textProperty().addListener((ob, ov, nv) -> refreshExamples());

    chkMultipleAuthors    .setSelected(keyConfig.multipleAuthors);
    chkSepAfterPenultimate.setSelected(keyConfig.oxfordSeparator);

    chkMultipleAuthors    .selectedProperty().addListener((ob, ov, nv) -> refreshExamples());
    chkSepAfterPenultimate.selectedProperty().addListener((ob, ov, nv) -> refreshExamples());

    grpConj .selectToggle(Map.of(FinalConjunctionSymbol   .and      , btnAnd      , FinalConjunctionSymbol   .ampersand , btnAmpersand ).getOrDefault(keyConfig.finalConjSymbol, null));
    grpParen.selectToggle(Map.of(CitationParenthesesOption.aroundAll, btnAroundAll, CitationParenthesesOption.aroundYear, btnAroundYear).getOrDefault(keyConfig.parentheses    , null));

    grpConj .selectedToggleProperty().addListener((ob, ov, nv) -> refreshExamples());
    grpParen.selectedToggleProperty().addListener((ob, ov, nv) -> refreshExamples());

    truncateNumWrapper = ArgumentNamingCtrlr.initTruncateNum  (spnTruncateNum            , chkNeverTruncate, keyConfig.authorNumToTruncate        , this::refreshExamples);
    numToShowWrapper   = ArgumentNamingCtrlr.initAuthorsToShow(spnNumToShowWhenTruncating,                   keyConfig.authorsToShowWhenTruncating, this::refreshExamples);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public WorkSearchKeyConfig save()
  {
    String beforeYearSep       = tfBeforeYearSep      .getText(),
           afterNameSep        = tfAfterNameSep       .getText(),
           truncationIndicator = tfTruncationIndicator.getText();

    FinalConjunctionSymbol    finalConjSymbol = nullSwitch(grpConj .getSelectedToggle(), FinalConjunctionSymbol   .none, Map.of(btnAnd      , FinalConjunctionSymbol   .and      , btnAmpersand , FinalConjunctionSymbol   .ampersand )::get);
    CitationParenthesesOption parentheses     = nullSwitch(grpParen.getSelectedToggle(), CitationParenthesesOption.none, Map.of(btnAroundAll, CitationParenthesesOption.aroundAll, btnAroundYear, CitationParenthesesOption.aroundYear)::get);

    boolean multipleAuthors = chkMultipleAuthors    .isSelected(),
            oxfordSeparator = chkSepAfterPenultimate.isSelected();

    int authorNumToTruncate         = truncateNumWrapper.getValue(),
        authorsToShowWhenTruncating = numToShowWrapper  .getValue();

    return new WorkSearchKeyConfig(beforeYearSep, afterNameSep, truncationIndicator, multipleAuthors, oxfordSeparator, finalConjSymbol, parentheses, authorNumToTruncate, authorsToShowWhenTruncating);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshExamples()
  {
    settingsPage.refreshExamples();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
