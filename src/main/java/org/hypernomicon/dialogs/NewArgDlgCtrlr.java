/*
 * Copyright 2015-2024 Jason Winning
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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.stream.Collectors;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Argument;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ArgumentVerdict;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PositionVerdict;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.settings.ArgumentNamingSettings;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.HybridSubjectPopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.application.Platform;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.CheckBox;
import javafx.scene.web.WebView;

public class NewArgDlgCtrlr extends HyperDlg
{
  @FXML private CheckBox chkIncludeAuth, chkLowerCaseTargetName;
  @FXML private ComboBox<HyperTableCell> cbPerson, cbVerdict, cbWork;
  @FXML private Label lblTargetName, lblTargetDesc;
  @FXML private RadioButton rbArgName1, rbArgName2, rbArgName3, rbArgName4, rbArgName5, rbArgName6, rbArgName7, rbArgName8, rbExisting, rbNew;
  @FXML private TextField tfArgName1, tfArgName2, tfArgName3, tfArgName4, tfArgName5, tfArgName6, tfArgName7, tfArgName8, tfTargetName, tfTitle, tfPages;
  @FXML private WebView webView;

  private final HDT_RecordWithMainText target;
  private final HyperCB hcbPerson, hcbVerdict, hcbWork;
  private final MutableBoolean alreadyChangingTitle = new MutableBoolean(false);

  private HDT_Argument argument;
  private boolean revising = false, programmaticWorkChange = false, programmaticVerdictChange = false;
  private String argName1 = "", argName2 = "", argName3 = "", argName4 = "", argName5 = "", argName6 = "", argName7 = "", argName8 = "";

  public HDT_Argument getArgument() { return argument; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public NewArgDlgCtrlr(HDT_Position position)
  {
    this("New Argument", position);
  }

  public NewArgDlgCtrlr(HDT_Argument counteredArg)
  {
    this("New Counterargument", counteredArg);
  }

  private NewArgDlgCtrlr(String title, HDT_RecordWithMainText target)
  {
    super("NewArgDlg", title, true);

    this.target = target;
    RecordType verdictType = target.getType() == hdtPosition ? hdtPositionVerdict : hdtArgumentVerdict;

    hcbPerson = new HyperCB(cbPerson, ctDropDownList, new StandardPopulator(hdtPerson));
    hcbVerdict = new HyperCB(cbVerdict, ctDropDownList, new StandardPopulator(verdictType));
    hcbWork = new HyperCB(cbWork, ctDropDownList, new HybridSubjectPopulator(rtAuthorOfWork));

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
          person = findFirst(work.getAuthors(), author -> (author.getPerson() != null) && (author.getInFileName() != Ternary.False), Author::getPerson);

        if (person == null)
          person = findFirst(work.getAuthors(), author -> author.getPerson() != null, Author::getPerson);

        if (person != null)
        {
          hcbPerson.selectID(person.getID());
          Platform.runLater(() -> hcbWork.selectID(work.getID()));
        }
      }

      programmaticWorkChange = false;

      reviseSuggestions();
    });

    String noun = target.getType() == hdtPosition ? "Position" : "Target argument";
    lblTargetName.setText(noun + ':');
    lblTargetDesc.setText(noun + " description:");

    tfTitle.setTextFormatter(WorkDlgCtrlr.titleFormatter(alreadyChangingTitle));

    tfTitle.textProperty().addListener((ob, ov, nv) -> rbNew.setSelected(true));

    chkIncludeAuth.selectedProperty().addListener((ob, ov, nv) -> reviseSuggestions());

    chkLowerCaseTargetName.setSelected(db.prefs.getBoolean(PREF_KEY_LOWER_CASE_TARGET_NAMES, false));

    chkLowerCaseTargetName.selectedProperty().addListener((ob, ov, nv) -> reviseSuggestions());

    int verdictID = verdictType == hdtPositionVerdict ? HDT_Argument.truePositionVerdictID : HDT_Argument.failsArgumentVerdictID;

    hcbVerdict.selectID(verdictID);

    if (verdictType == hdtPositionVerdict) hcbVerdict.addListener((ov, nv) ->
    {
      if (programmaticVerdictChange) return;

      int posVerdictID = HyperTableCell.getCellID(nv);
      if (posVerdictID < 1) return;

      if (HDT_Argument.posVerdictIDIsInFavor(posVerdictID))
      {
        if      (rbArgName5.isSelected() && tfArgName5.getText().equals(argName5)) rbArgName1.setSelected(true);
        else if (rbArgName6.isSelected() && tfArgName6.getText().equals(argName6)) rbArgName2.setSelected(true);
        else if (rbArgName7.isSelected() && tfArgName7.getText().equals(argName7)) rbArgName3.setSelected(true);
        else if (rbArgName8.isSelected() && tfArgName8.getText().equals(argName8)) rbArgName4.setSelected(true);
      }
      else
      {
        if      (rbArgName1.isSelected() && tfArgName1.getText().equals(argName1)) rbArgName5.setSelected(true);
        else if (rbArgName2.isSelected() && tfArgName2.getText().equals(argName2)) rbArgName6.setSelected(true);
        else if (rbArgName3.isSelected() && tfArgName3.getText().equals(argName3)) rbArgName7.setSelected(true);
        else if (rbArgName4.isSelected() && tfArgName4.getText().equals(argName4)) rbArgName8.setSelected(true);
      }
    });

    tfTargetName.setText(target.name());

    MainTextWrapper.setReadOnlyHTML(target.getMainText().getHtml(), webView.getEngine());

    webView.setOnDragOver(Event::consume);
    webView.setOnDragDropped(Event::consume);

    rbNew.setSelected(true);

    rbExisting.getToggleGroup().selectedToggleProperty().addListener((ob, ov, nv) -> reviseSuggestions());

    reviseSuggestions();

    alreadyChangingTitle.setTrue();
    tfTitle.setText(target.name() + (target.getType() == hdtPosition ? " Argument Stem" : " Counterargument Stem"));
    alreadyChangingTitle.setFalse();

    addListeners(tfArgName1, rbArgName1, true ); addListeners(tfArgName2, rbArgName2, true );
    addListeners(tfArgName3, rbArgName3, true ); addListeners(tfArgName4, rbArgName4, true );
    addListeners(tfArgName5, rbArgName5, false); addListeners(tfArgName6, rbArgName6, false);
    addListeners(tfArgName7, rbArgName7, false); addListeners(tfArgName8, rbArgName8, false);

    hcbPerson.addListener((oldCell, newCell) ->
    {
      if (HyperTableCell.getCellID(oldCell) == HyperTableCell.getCellID(newCell)) return;

      ((HybridSubjectPopulator) hcbWork.getPopulator()).setObj(HyperTableCell.getRecord(newCell));
      hcbWork.selectID(-1);
      rbNew.setSelected(true);

      reviseSuggestions();
    });

    chkIncludeAuth.setSelected(true);
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

  private void reviseSuggestions()
  {
    String part1 = "";

    revising = true;

    if (chkIncludeAuth.isSelected())
      part1 = getAuthorNamesForSuggestions();

    String targetName = target.name();
    if (targetName.startsWith("The "))
      targetName = "the " + targetName.substring(4);

    if (chkLowerCaseTargetName.isSelected())
      targetName = targetName.toLowerCase();

    if (target.getType() == hdtPosition)
    {
      chkLowerCaseTargetName.setText("Lower case position name");

      String part2 = part1.isEmpty() ? "Argument " : "argument ";

      argName1 = part1 + part2 + "for "                   + targetName; tfArgName1.setText(argName1);
      argName2 = part1 + part2 + "for the "               + targetName; tfArgName2.setText(argName2);
      argName3 = part1 + part2 + "that "                  + targetName; tfArgName3.setText(argName3);
      argName4 = part1 + part2 + "for the view that "     + targetName; tfArgName4.setText(argName4);
      argName5 = part1 + part2 + "against "               + targetName; tfArgName5.setText(argName5);
      argName6 = part1 + part2 + "against the "           + targetName; tfArgName6.setText(argName6);
      argName7 = part1 + part2 + "that it is false that " + targetName; tfArgName7.setText(argName7);
      argName8 = part1 + part2 + "against the view that " + targetName; tfArgName8.setText(argName8);
    }
    else
    {
      chkLowerCaseTargetName.setText("Lower case target name");

      String part2 = part1.isEmpty() ? "Counterargument " : "counterargument ";

      argName1 = part1 + part2 + "against "                + targetName; tfArgName1.setText(argName1);
      argName2 = part1 + part2 + "against the "            + targetName; tfArgName2.setText(argName2);
      argName3 = part1 + part2 + "against the claim that " + targetName; tfArgName3.setText(argName3);

      part2 = part1.isEmpty() ? "Response " : "response ";

      argName4 = part1 + part2 + "to "     + targetName; tfArgName4.setText(argName4);
      argName5 = part1 + part2 + "to the " + targetName; tfArgName5.setText(argName5);

      part2 = part1.isEmpty() ? "Objection " : "objection ";

      argName6 = part1 + part2 + "to "                + targetName; tfArgName6.setText(argName6);
      argName7 = part1 + part2 + "to the "            + targetName; tfArgName7.setText(argName7);
      argName8 = part1 + part2 + "to the claim that " + targetName; tfArgName8.setText(argName8);
    }

    revising = false;
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

    return settings.format(work.getAuthors().stream().map(Author::singleName).collect(Collectors.toList())) + "'s ";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    HDT_Record verdict = hcbVerdict.selectedRecord();

    if (verdict == null)
      return falseWithErrorMessage("You must select a verdict.", cbVerdict);

    argument = db.createNewBlankRecord(hdtArgument);

    if (verdict.getType() == hdtPositionVerdict)
      argument.addPosition((HDT_Position)target, (HDT_PositionVerdict)verdict);
    else
    {
      HDT_Argument counteredArg = (HDT_Argument)target;

      try { argument.addCounteredArg(counteredArg, (HDT_ArgumentVerdict)verdict); } catch (RelationCycleException e) { throw new AssertionError(getThrowableMessage(e), e); }
      counteredArg.positions.forEach(position -> argument.addPosition(position, null));
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
