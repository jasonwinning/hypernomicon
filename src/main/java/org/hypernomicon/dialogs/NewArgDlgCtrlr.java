/*
 * Copyright 2015-2022 Jason Winning
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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Argument;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PositionVerdict;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.HybridSubjectPopulator;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.CheckBox;
import javafx.scene.web.WebView;

public class NewArgDlgCtrlr extends HyperDlg
{
  @FXML private CheckBox chkIncludeAuth;
  @FXML private ComboBox<HyperTableCell> cbPerson, cbPositionVerdict, cbWork;
  @FXML private RadioButton rbArgName1, rbArgName2, rbArgName3, rbArgName4, rbArgName5, rbArgName6, rbArgName7, rbArgName8, rbExisting, rbNew;
  @FXML private TextField tfArgName1, tfArgName2, tfArgName3, tfArgName4, tfArgName5, tfArgName6, tfArgName7, tfArgName8, tfPosition, tfTitle;
  @FXML private WebView view;

  private HDT_Position position;
  private HDT_Argument argument;
  private HyperCB hcbPerson, hcbPositionVerdict, hcbWork;
  private boolean revising = false, programmaticWorkChange = false, programmaticVerdictChange = false;
  private MutableBoolean alreadyChangingTitle = new MutableBoolean(false);
  private String argName1 = "", argName2 = "", argName3 = "", argName4 = "", argName5 = "", argName6 = "", argName7 = "", argName8 = "";

  public HDT_Argument getArgument() { return argument; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewArgDlgCtrlr build(HDT_Position curPosition)
  {
    return ((NewArgDlgCtrlr) create("NewArgDlg", "New Argument", true)).init(curPosition);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private NewArgDlgCtrlr init(HDT_Position position)
  {
    this.position = position;

    hcbPerson = new HyperCB(cbPerson, ctDropDownList, new StandardPopulator(hdtPerson));
    hcbPositionVerdict = new HyperCB(cbPositionVerdict, ctDropDownList, new StandardPopulator(hdtPositionVerdict));
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
    });

    tfTitle.setTextFormatter(WorkDlgCtrlr.titleFormatter(alreadyChangingTitle));

    tfTitle.textProperty().addListener((ob, oldText, newText) -> rbNew.setSelected(true));

    chkIncludeAuth.selectedProperty().addListener((ob, oldSelected, newSelected) -> reviseSuggestions());

    hcbPerson.addBlankEntry();
    hcbPositionVerdict.addAndSelectEntry(db.positionVerdicts.getByID(1), HDT_Record::getCBText);
    hcbPositionVerdict.populate(false);
    hcbWork.addBlankEntry();

    hcbPositionVerdict.addListener((ov, nv) ->
    {
      if (programmaticVerdictChange) return;

      int verdictID = HyperTableCell.getCellID(nv);
      if (verdictID < 1) return;

      if (HDT_Argument.posVerdictIDIsInFavor(verdictID))
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

    tfPosition.setText(position.name());

    MainTextWrapper.setReadOnlyHTML(position.getMainText().getHtml(), view.getEngine(), new TextViewInfo(), null);

    reviseSuggestions();

    rbExisting.setSelected(false);
    rbNew.setSelected(true);

    alreadyChangingTitle.setTrue();
    tfTitle.setText(position.name() + " Argument Stem");
    alreadyChangingTitle.setFalse();

    addListeners(tfArgName1, rbArgName1, true ); addListeners(tfArgName2, rbArgName2, true );
    addListeners(tfArgName3, rbArgName3, true ); addListeners(tfArgName4, rbArgName4, true );
    addListeners(tfArgName5, rbArgName5, false); addListeners(tfArgName6, rbArgName6, false);
    addListeners(tfArgName7, rbArgName7, false); addListeners(tfArgName8, rbArgName8, false);

    hcbPerson.addListener((oldCell, newCell) ->
    {
      if (HyperTableCell.getCellID(oldCell) == HyperTableCell.getCellID(newCell)) return;

      ((HybridSubjectPopulator) hcbWork.getPopulator()).setObj(Populator.dummyRow, HyperTableCell.getRecord(newCell));
      hcbWork.selectID(-1);
      rbNew.setSelected(true);

      reviseSuggestions();
    });

    chkIncludeAuth.setSelected(true);

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addListeners(TextField tf, RadioButton rb, boolean proArg)
  {
    rb.selectedProperty().addListener((ob, oldSelected, newSelected) -> argNameSelect(newSelected, proArg));

    tf.textProperty().addListener((ob, oldText, newText) ->
    {
      if (!revising)
        rb.setSelected(true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void argNameSelect(Boolean newSelected, boolean proArg)
  {
    if ((newSelected != null) && newSelected.booleanValue())
    {
      programmaticVerdictChange = true;
      hcbPositionVerdict.selectID(proArg ? 1 : 2);
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
    {
      HDT_Person person = hcbPerson.selectedRecord();

      if (person != null) part1 = person.getLastName() + "'s ";
    }

    String part2 = part1.isEmpty() ? "Argument " : "argument ",
           positionName = position.name();

    if (positionName.startsWith("The "))
      positionName = "the " + positionName.substring(4);

    argName1 = part1 + part2 + "for "                   + positionName; tfArgName1.setText(argName1);
    argName2 = part1 + part2 + "for the "               + positionName; tfArgName2.setText(argName2);
    argName3 = part1 + part2 + "that "                  + positionName; tfArgName3.setText(argName3);
    argName4 = part1 + part2 + "for the view that "     + positionName; tfArgName4.setText(argName4);
    argName5 = part1 + part2 + "against "               + positionName; tfArgName5.setText(argName5);
    argName6 = part1 + part2 + "against the "           + positionName; tfArgName6.setText(argName6);
    argName7 = part1 + part2 + "that it is false that " + positionName; tfArgName7.setText(argName7);
    argName8 = part1 + part2 + "against the view that " + positionName; tfArgName8.setText(argName8);

    revising = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    HDT_PositionVerdict verdict = hcbPositionVerdict.selectedRecord();

    if (verdict == null)
      return falseWithErrorMessage("You must select a verdict.", cbPositionVerdict);

    argument = db.createNewBlankRecord(hdtArgument);
    argument.addPosition(position, verdict);

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
      nullSwitch(hcbPerson.selectedRecord(), person -> work.getAuthors().add((HDT_Person) person));
    }
    else if (rbExisting.isSelected())
      work = hcbWork.selectedRecord();
    else
      work = null;

    if (work != null)
      argument.works.add(work);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
