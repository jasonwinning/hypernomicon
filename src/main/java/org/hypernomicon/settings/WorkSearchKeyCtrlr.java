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

import java.util.Map;

import org.hypernomicon.settings.WorkSearchKeySettings.CitationParenthesesOption;
import org.hypernomicon.settings.WorkSearchKeySettings.FinalConjunctionSymbol;
import org.hypernomicon.settings.WorkSearchKeySettings.WorkSearchKeyConfig;

import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Tab;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;

//---------------------------------------------------------------------------

public class WorkSearchKeyCtrlr extends Tab
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private CheckBox chkMultipleAuthors, chkSepAfterPenultimate;
  @FXML TextArea taExamples;
  @FXML private TextField tfBeforeYearSep, tfAfterNameSep, tfTruncateNum, tfTruncationIndicator, tfNumToShowWhenTruncating;
  @FXML private ToggleButton btnAroundAll, btnAroundYear, btnAnd, btnAmpersand;
  @FXML private ToggleGroup grpParen, grpConj;

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

    ArgumentNamingCtrlr.initTruncateNum  (tfTruncateNum            , keyConfig.authorNumToTruncate        , this::refreshExamples);
    ArgumentNamingCtrlr.initAuthorsToShow(tfNumToShowWhenTruncating, keyConfig.authorsToShowWhenTruncating, this::refreshExamples);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public WorkSearchKeyConfig save()
  {
    String beforeYearSep = tfBeforeYearSep.getText(),
           afterNameSep = tfAfterNameSep.getText(),
           truncationIndicator = tfTruncationIndicator.getText();

    FinalConjunctionSymbol    finalConjSymbol = nullSwitch(grpConj .getSelectedToggle(), FinalConjunctionSymbol   .none, Map.of(btnAnd      , FinalConjunctionSymbol   .and      , btnAmpersand , FinalConjunctionSymbol   .ampersand )::get);
    CitationParenthesesOption parentheses     = nullSwitch(grpParen.getSelectedToggle(), CitationParenthesesOption.none, Map.of(btnAroundAll, CitationParenthesesOption.aroundAll, btnAroundYear, CitationParenthesesOption.aroundYear)::get);

    boolean multipleAuthors = chkMultipleAuthors    .isSelected(),
            oxfordSeparator = chkSepAfterPenultimate.isSelected();

    int authorNumToTruncate = parseInt(tfTruncateNum.getText(), -1);
    if (authorNumToTruncate < 2)
      authorNumToTruncate = -1;

    int authorsToShowWhenTruncating = parseInt(tfNumToShowWhenTruncating.getText(), -1);
    if (authorsToShowWhenTruncating < 1)
      authorsToShowWhenTruncating = 2;

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
