/*
 * Copyright 2015-2025 Jason Winning
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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.*;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ArgumentVerdict;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PositionVerdict;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.settings.ArgumentNamingSettings;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.HybridSubjectPopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.SimpleSelector;

import javafx.application.Platform;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public class NewArgDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private CheckBox chkIncludeAuth, chkLowerCaseTargetName;
  @FXML private ComboBox<HyperTableCell> cbPerson, cbVerdict, cbWork;
  @FXML private ComboBox<Ternary> cbArgOrStance;
  @FXML private Label lblTargetName, lblTargetDesc, lblDoesWhat;
  @FXML private RadioButton rbArgName1, rbArgName2, rbArgName3, rbArgName4, rbArgName5, rbArgName6, rbArgName7, rbArgName8, rbExisting, rbNew;
  @FXML private TextField tfArgName1, tfArgName2, tfArgName3, tfArgName4, tfArgName5, tfArgName6, tfArgName7, tfArgName8, tfTargetName, tfTitle, tfPages;
  @FXML private WebView webView;

  private final HDT_RecordWithMainText target;
  private final HyperCB hcbPerson, hcbVerdict, hcbWork;
  private final MutableBoolean alreadyChangingTitle = new MutableBoolean(false);

  private HDT_Argument argument;
  private boolean revising = false, programmaticWorkChange = false, programmaticVerdictChange = false;

  private final Map<TextField, String> textFieldToLastGen = new HashMap<>();

  public HDT_Argument getArgument() { return argument; }

//---------------------------------------------------------------------------

  public NewArgDlgCtrlr(HDT_Position position)
  {
    this("New Argument/Stance", position);
  }

  public NewArgDlgCtrlr(HDT_Argument targetArg)
  {
    this("New Response Argument", targetArg);
  }

  private NewArgDlgCtrlr(String title, HDT_RecordWithMainText target)
  {
    super("NewArgDlg", title, true);

    this.target = target;
    RecordType verdictType = target.getType() == hdtPosition ? hdtPositionVerdict : hdtArgumentVerdict;

    hcbPerson  = new HyperCB(cbPerson , ctEditableLimitedDropDown, new StandardPopulator(hdtPerson));

    hcbVerdict = new HyperCB(cbVerdict, ctEditableLimitedDropDown, new StandardPopulator(verdictType));

    hcbWork    = new HyperCB(cbWork   , ctEditableLimitedDropDown, new HybridSubjectPopulator(rtAuthorOfWork));

    rbArgName1.setSelected(true);

    hcbWork.addListener((oldCell, newCell) ->
    {
      if (programmaticWorkChange) return;

      rbExisting.setSelected(true);

      HDT_Work work = HyperTableCell.getRecord(newCell);

      if ((work != null) && (hcbPerson.selectedID() == -1))
      {
        HDT_Person person = findFirst(work.getAuthors(), author -> (author.getPerson() != null) && author.getInFileName().isTrue(), Author::getPerson);

        if (person == null)
        {
          person = findFirst(work.getAuthors(), author -> (author.getPerson() != null) && (author.getInFileName() != Ternary.False), Author::getPerson);

          if (person == null)
            person = findFirst(work.getAuthors(), author -> author.getPerson() != null, Author::getPerson);
        }

        if (person != null)
        {
          hcbPerson.selectIDofRecord(person);
          Platform.runLater(() -> hcbWork.selectIDofRecord(work));
        }
      }

      programmaticWorkChange = false;

      reviseSuggestions(cbArgOrStance.getValue());
    });

    String noun = target.getType() == hdtPosition ? "Position" : "Target argument";
    lblTargetName.setText(noun + ':');
    lblTargetDesc.setText(noun + " description:");

    tfTitle.setTextFormatter(WorkDlgCtrlr.titleFormatter(alreadyChangingTitle));

    tfTitle.textProperty().addListener((ob, ov, nv) -> rbNew.setSelected(true));

    chkIncludeAuth.selectedProperty().addListener((ob, ov, nv) -> reviseSuggestions(cbArgOrStance.getValue()));

    chkLowerCaseTargetName.setSelected(db.prefs.getBoolean(PrefKey.LOWER_CASE_TARGET_NAMES, false));

    chkLowerCaseTargetName.selectedProperty().addListener((ob, ov, nv) -> reviseSuggestions(cbArgOrStance.getValue()));

    int verdictID = verdictType == hdtPositionVerdict ? HDT_Argument.truePositionVerdictID : HDT_Argument.failsArgumentVerdictID;

    hcbVerdict.selectID(verdictID);

    if (verdictType == hdtPositionVerdict) hcbVerdict.addListener((ov, nv) ->
    {
      if (programmaticVerdictChange) return;

      int posVerdictID = HyperTableCell.getCellID(nv);
      if (posVerdictID < 1) return;

      if (HDT_Argument.posVerdictIDIsInFavor(posVerdictID))
      {
        if      (rbArgName5.isSelected() && tfArgName5.getText().equals(textFieldToLastGen.get(tfArgName5))) rbArgName1.setSelected(true);
        else if (rbArgName6.isSelected() && tfArgName6.getText().equals(textFieldToLastGen.get(tfArgName6))) rbArgName2.setSelected(true);
        else if (rbArgName7.isSelected() && tfArgName7.getText().equals(textFieldToLastGen.get(tfArgName7))) rbArgName3.setSelected(true);
        else if (rbArgName8.isSelected() && tfArgName8.getText().equals(textFieldToLastGen.get(tfArgName8))) rbArgName4.setSelected(true);
      }
      else
      {
        if      (rbArgName1.isSelected() && tfArgName1.getText().equals(textFieldToLastGen.get(tfArgName1))) rbArgName5.setSelected(true);
        else if (rbArgName2.isSelected() && tfArgName2.getText().equals(textFieldToLastGen.get(tfArgName2))) rbArgName6.setSelected(true);
        else if (rbArgName3.isSelected() && tfArgName3.getText().equals(textFieldToLastGen.get(tfArgName3))) rbArgName7.setSelected(true);
        else if (rbArgName4.isSelected() && tfArgName4.getText().equals(textFieldToLastGen.get(tfArgName4))) rbArgName8.setSelected(true);
      }
    });

    tfTargetName.setText(target.name());

    webView.getEngine().setUserStyleSheetLocation(cssStrToDataURI(EMPTY_FONT_CSS));

    MainTextWrapper.setReadOnlyHTML(target.getMainText().getHtml(), webView.getEngine());

    webView.setOnDragOver   (Event::consume);
    webView.setOnDragDropped(Event::consume);

    rbNew.setSelected(true);

    rbExisting.getToggleGroup().selectedToggleProperty().addListener((ob, ov, nv) -> reviseSuggestions(cbArgOrStance.getValue()));

    alreadyChangingTitle.setTrue();
    tfTitle.setText(target.name() + (target.getType() == hdtPosition ? " Argument/Stance Stem" : " Counterargument Stem"));
    alreadyChangingTitle.setFalse();

    addListeners(tfArgName1, rbArgName1, true ); addListeners(tfArgName2, rbArgName2, true );
    addListeners(tfArgName3, rbArgName3, true ); addListeners(tfArgName4, rbArgName4, true );
    addListeners(tfArgName5, rbArgName5, false); addListeners(tfArgName6, rbArgName6, false);
    addListeners(tfArgName7, rbArgName7, false); addListeners(tfArgName8, rbArgName8, false);

    hcbPerson.addListener((oldCell, newCell) ->
    {
      if (HyperTableCell.getCellID(oldCell) == HyperTableCell.getCellID(newCell)) return;

      Platform.runLater(() ->
      {
        ((HybridSubjectPopulator) hcbWork.getPopulator()).setObj(HyperTableCell.getRecord(newCell));
        hcbWork.populate(true);
        hcbWork.selectID(-1);
        rbNew.setSelected(true);

        reviseSuggestions(cbArgOrStance.getValue());
      });
    });

    chkIncludeAuth.setSelected(true);

    SequencedMap<Ternary, String> strMap = new LinkedHashMap<>();

    strMap.put(Ternary.False, "Stance");
    strMap.put(Ternary.True , "Argument");

    SimpleSelector.init(cbArgOrStance, strMap);

    cbArgOrStance.setValue(Ternary.True);

    if (target.getType() != hdtPosition)
      cbArgOrStance.setDisable(true);

    cbArgOrStance.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) ->
    {
      if (Ternary.isEmpty(nv) == false)
        reviseSuggestions(nv);
    });

    reviseSuggestions(Ternary.True);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addListeners(TextField tf, RadioButton rb, boolean proArg)
  {
    rb.selectedProperty().addListener((ob, oldSelected, newSelected) -> argNameSelect(newSelected, proArg));

    tf.textProperty().addListener((ob, oldText, newText) ->
    {
      if (revising == false)
        rb.setSelected(true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void argNameSelect(Boolean newSelected, boolean proArg)
  {
    if (target.getType() != hdtPosition)
      return;

    if (Boolean.TRUE.equals(newSelected))
    {
      programmaticVerdictChange = true;
      hcbVerdict.selectID(proArg ? HDT_Argument.truePositionVerdictID : HDT_Argument.falsePositionVerdictID);
      programmaticVerdictChange = false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void reviseSuggestions(Ternary isArgument)
  {
    String peoplePart = "";

    revising = true;

    if      (isArgument == Ternary.True ) lblDoesWhat.setText("Argues that:");
    else if (isArgument == Ternary.False) lblDoesWhat.setText("Holds that:" );

    if (chkIncludeAuth.isSelected())
      peoplePart = getAuthorNamesForSuggestions();

    String targetName = target.name();
    if (targetName.startsWith("The "))
      targetName = "the " + targetName.substring(4);

    if (chkLowerCaseTargetName.isSelected())
      targetName = targetName.toLowerCase();

    if (target.getType() == hdtPosition)
    {
      chkLowerCaseTargetName.setText("Lower case position name");

      if (isArgument == Ternary.False)
      {
        setArgTextField(tfArgName1, peoplePart, "Endorsement of "              , targetName);
        setArgTextField(tfArgName2, peoplePart, "Endorsement of the "          , targetName);
        setArgTextField(tfArgName3, peoplePart, "Stance that "                 , targetName);
        setArgTextField(tfArgName4, peoplePart, "Stance affirming that "       , targetName);
        setArgTextField(tfArgName5, peoplePart, "Rejection of "                , targetName);
        setArgTextField(tfArgName6, peoplePart, "Rejection of the "            , targetName);
        setArgTextField(tfArgName7, peoplePart, "Rejection of the claim that " , targetName);
        setArgTextField(tfArgName8, peoplePart, "Stance against the view that ", targetName);
      }
      else
      {
        setArgTextField(tfArgName1, peoplePart, "Argument for "                  , targetName);
        setArgTextField(tfArgName2, peoplePart, "Argument for the "              , targetName);
        setArgTextField(tfArgName3, peoplePart, "Argument that "                 , targetName);
        setArgTextField(tfArgName4, peoplePart, "Argument for the view that "    , targetName);
        setArgTextField(tfArgName5, peoplePart, "Argument against "              , targetName);
        setArgTextField(tfArgName6, peoplePart, "Argument against the "          , targetName);
        setArgTextField(tfArgName7, peoplePart, "Argument that it is false that ", targetName);
        setArgTextField(tfArgName8, peoplePart, "Argument against the view that ", targetName);
      }
    }
    else
    {
      chkLowerCaseTargetName.setText("Lower case target name");

      setArgTextField(tfArgName1, peoplePart, "Counterargument against "               , targetName);
      setArgTextField(tfArgName2, peoplePart, "Counterargument against the "           , targetName);
      setArgTextField(tfArgName3, peoplePart, "Counterargument against the claim that ", targetName);

      setArgTextField(tfArgName4, peoplePart, "Response to "    , targetName);
      setArgTextField(tfArgName5, peoplePart, "Response to the ", targetName);

      setArgTextField(tfArgName6, peoplePart, "Objection to "               , targetName);
      setArgTextField(tfArgName7, peoplePart, "Objection to the "           , targetName);
      setArgTextField(tfArgName8, peoplePart, "Objection to the claim that ", targetName);
    }

    revising = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setArgTextField(TextField tf, String peoplePart, String desc, String targetName)
  {
    String str = peoplePart + (strNullOrBlank(peoplePart) ? desc : desc.toLowerCase()) + targetName;

    textFieldToLastGen.put(tf, str);
    tf.setText(str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getAuthorNamesForSuggestions()
  {
    HDT_Person person = hcbPerson.selectedRecord();
    HDT_Work work = hcbWork.selectedRecord();

    ArgumentNamingSettings settings = new ArgumentNamingSettings();

    if ((settings.multipleAuthors == false) || (rbExisting.isSelected() == false) || (work == null))
      return person == null ? "" : person.getLastName() + "'s ";

    if (work.getAuthors().isEmpty()) return "";

    return settings.format(work.getAuthors().stream().filter(Author::getIsAuthor).map(Author::singleName).toList()) + "'s ";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    HDT_Record verdict = hcbVerdict.selectedRecord();

    if (verdict == null)
      return falseWithErrorPopup("You must select a verdict.", cbVerdict);

    Ternary isArgument = cbArgOrStance.getValue();

    if (Ternary.isEmpty(isArgument))
      return falseWithErrorPopup("You must select either Argument or Stance.", cbArgOrStance);

    argument = db.createNewBlankRecord(hdtArgument);

    argument.setIsArgument(isArgument);

    if (verdict.getType() == hdtPositionVerdict)
      argument.addPosition((HDT_Position)target, (HDT_PositionVerdict)verdict);
    else
    {
      HDT_Argument targetArg = (HDT_Argument)target;

      try { argument.addTargetArg(targetArg, (HDT_ArgumentVerdict)verdict); } catch (RelationCycleException e) { throw newAssertionError(e); }
      targetArg.positions.forEach(position -> argument.addPosition(position, null));
    }

    if      (rbArgName1.isSelected()) argument.setName(tfArgName1.getText());
    else if (rbArgName2.isSelected()) argument.setName(tfArgName2.getText());
    else if (rbArgName3.isSelected()) argument.setName(tfArgName3.getText());
    else if (rbArgName4.isSelected()) argument.setName(tfArgName4.getText());
    else if (rbArgName5.isSelected()) argument.setName(tfArgName5.getText());
    else if (rbArgName6.isSelected()) argument.setName(tfArgName6.getText());
    else if (rbArgName7.isSelected()) argument.setName(tfArgName7.getText());
    else                              argument.setName(tfArgName8.getText());

    HDT_Work work;

    if (rbNew.isSelected())
    {
      work = db.createNewBlankRecord(hdtWork);

      work.setName(tfTitle.getText());
      nullSwitch(hcbPerson.selectedRecord(), (HDT_Person person) -> work.getAuthors().add(person));
    }
    else if (rbExisting.isSelected())
      work = hcbWork.selectedRecord();
    else
      work = null;

    if (work != null)
    {
      argument.works.add(work);
      argument.setPagesInWork(work, tfPages.getText());
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
