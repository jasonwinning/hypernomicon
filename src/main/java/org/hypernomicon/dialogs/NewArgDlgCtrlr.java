/*
 * Copyright 2015-2020 Jason Winning
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
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Argument;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_Work;
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
  private boolean revising = false, changingWorkProgrammatically = false;

  public HDT_Argument getArgument() { return argument; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewArgDlgCtrlr build(HDT_Position curPosition)
  {
    return ((NewArgDlgCtrlr) create("NewArgDlg.fxml", "New Argument", true)).init(curPosition);
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

    cbWork.getSelectionModel().selectedItemProperty().addListener((ob, oldCell, newCell) ->
    {
      if (changingWorkProgrammatically) return;

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

      changingWorkProgrammatically = false;
    });

    tfTitle.textProperty().addListener((ob, oldText, newText) -> rbNew.setSelected(true));

    chkIncludeAuth.selectedProperty().addListener((ob, oldSelected, newSelected) -> reviseSuggestions());

    hcbPerson.addBlankEntry();
    hcbPositionVerdict.addAndSelectEntry(db.positionVerdicts.getByID(1), HDT_Record::getCBText);
    hcbPositionVerdict.populate(false);
    hcbWork.addBlankEntry();

    tfPosition.setText(position.name());

    MainTextWrapper.setReadOnlyHTML(position.getMainText().getHtml(), view.getEngine(), new TextViewInfo(), null);

    reviseSuggestions();

    rbExisting.setSelected(false);
    rbNew.setSelected(true);

    tfTitle.setText(position.name() + " Argument Stem");

    addListeners(tfArgName1, rbArgName1, true);  addListeners(tfArgName2, rbArgName2, true);
    addListeners(tfArgName3, rbArgName3, true);  addListeners(tfArgName4, rbArgName4, true);
    addListeners(tfArgName5, rbArgName5, false); addListeners(tfArgName6, rbArgName6, false);
    addListeners(tfArgName7, rbArgName7, false); addListeners(tfArgName8, rbArgName8, false);

    cbPerson.getSelectionModel().selectedItemProperty().addListener((ob, oldCell, newCell) ->
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
      hcbPositionVerdict.selectID(proArg ? 1 : 2);
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

    String part2 = part1.isEmpty() ? "Argument " : "argument ";

    tfArgName1.setText(part1 + part2 + "for " + position.name());
    tfArgName2.setText(part1 + part2 + "for the " + position.name());
    tfArgName3.setText(part1 + part2 + "that " + position.name());
    tfArgName4.setText(part1 + part2 + "for the view that " + position.name());
    tfArgName5.setText(part1 + part2 + "against " + position.name());
    tfArgName6.setText(part1 + part2 + "against the " + position.name());
    tfArgName7.setText(part1 + part2 + "that it is false that " + position.name());
    tfArgName8.setText(part1 + part2 + "against the view that " + position.name());

    revising = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    argument = db.createNewBlankRecord(hdtArgument);
    argument.addPosition(position, hcbPositionVerdict.selectedRecord());

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
    else
      work = hcbWork.selectedRecord();

    if (work != null)
      argument.works.add(work);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
