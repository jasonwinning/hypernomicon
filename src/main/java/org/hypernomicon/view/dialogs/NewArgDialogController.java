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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Base;
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
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.CheckBox;
import javafx.scene.web.WebView;

public class NewArgDialogController extends HyperDialog
{
  @FXML TextField tfPosition;
  @FXML WebView view;
  @FXML public TextField tfArgName1;
  @FXML public TextField tfArgName2;
  @FXML public TextField tfArgName3;
  @FXML public TextField tfArgName4;
  @FXML public TextField tfArgName5;
  @FXML public TextField tfArgName6;
  @FXML public TextField tfArgName7;
  @FXML public TextField tfArgName8;
  @FXML ComboBox<HyperTableCell> cbPerson;
  @FXML ComboBox<HyperTableCell> cbPositionVerdict;
  @FXML public RadioButton rbNew;
  @FXML private RadioButton rbExisting;
  @FXML public RadioButton rbArgName1;
  @FXML public RadioButton rbArgName2;
  @FXML public RadioButton rbArgName3;
  @FXML public RadioButton rbArgName4;
  @FXML public RadioButton rbArgName5;
  @FXML public RadioButton rbArgName6;
  @FXML public RadioButton rbArgName7;
  @FXML public RadioButton rbArgName8;
  @FXML private CheckBox cbIncludeAuth;
  @FXML public TextField tfTitle;
  @FXML ComboBox<HyperTableCell> cbWork;
  @FXML Button btnOK;
  @FXML Button btnCancel;

  private HDT_Position curPosition;
  public HyperCB hcbPerson, hcbPositionVerdict, hcbWork;
  private boolean revising = false;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewArgDialogController create(String title, HDT_Position curPosition)
  {
    NewArgDialogController nad = HyperDialog.create("NewArgDialog.fxml", title, true);
    nad.init(curPosition);
    return nad;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean changingWorkProgrammatically = false;

  private void init(HDT_Position curPosition)
  {
    this.curPosition = curPosition;

    hcbPerson = new HyperCB(cbPerson, ctDropDownList, new StandardPopulator(hdtPerson), null, false);
    hcbPositionVerdict = new HyperCB(cbPositionVerdict, ctDropDownList, new StandardPopulator(hdtPositionVerdict), null);
    hcbWork = new HyperCB(cbWork, ctDropDownList, new HybridSubjectPopulator(rtAuthorOfWork), null);

    rbArgName1.setSelected(true);

    cbWork.getSelectionModel().selectedItemProperty().addListener((observable, oldCell, newCell) ->
    {
      if (changingWorkProgrammatically) return;

      rbExisting.setSelected(true);

      HDT_Work work = HyperTableCell.getRecord(newCell);

      if ((work != null) && (hcbPerson.selectedID() == -1))
      {
        HDT_Person person = null;

        for (Author author : work.getAuthors()) if ((author.getPerson() != null) && (author.getInFileName() == Ternary.True))
        { person = author.getPerson(); break; }

        if (person == null)
          for (Author author : work.getAuthors()) if ((author.getPerson() != null) && (author.getInFileName() != Ternary.False))
          { person = author.getPerson(); break; }

        if (person == null)
          for (Author author : work.getAuthors()) if (author.getPerson() != null)
          { person = author.getPerson(); break; }

        if (person != null)
        {
          hcbPerson.selectID(person.getID());
          Platform.runLater(() -> hcbWork.selectID(work.getID()));
        }
      }

      changingWorkProgrammatically = false;
    });

    tfTitle.textProperty().addListener((observable, oldText, newText) -> rbNew.setSelected(true));

    cbIncludeAuth.selectedProperty().addListener((observable, oldSelected, newSelected) -> reviseSuggestions());

    hcbPerson.addBlankEntry();
    hcbPositionVerdict.addAndSelectEntry(db.positionVerdicts.getByID(1), HDT_Base::getCBText);
    hcbPositionVerdict.populate(false);
    hcbWork.addBlankEntry();

    tfPosition.setText(curPosition.name());

    MainTextWrapper.setReadOnlyHTML(getHtmlEditorText(curPosition.getMainText().getHtml()), view.getEngine(), new TextViewInfo(), null);

    reviseSuggestions();

    rbExisting.setSelected(false);
    rbNew.setSelected(true);

    tfTitle.setText(curPosition.name() + " Argument Stem");

    addListeners(tfArgName1, rbArgName1, true);  addListeners(tfArgName2, rbArgName2, true);
    addListeners(tfArgName3, rbArgName3, true);  addListeners(tfArgName4, rbArgName4, true);
    addListeners(tfArgName5, rbArgName5, false); addListeners(tfArgName6, rbArgName6, false);
    addListeners(tfArgName7, rbArgName7, false); addListeners(tfArgName8, rbArgName8, false);

    cbPerson.getSelectionModel().selectedItemProperty().addListener((observable, oldCell, newCell) ->
    {
      if (HyperTableCell.getCellID(oldCell) == HyperTableCell.getCellID(newCell)) return;

      HybridSubjectPopulator subjPop = hcbWork.getPopulator();
      subjPop.setObj(Populator.dummyRow, HyperTableCell.getRecord(newCell));
      hcbWork.selectID(-1);
      rbNew.setSelected(true);

      reviseSuggestions();
    });

    cbIncludeAuth.setSelected(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addListeners(TextField tf, RadioButton rb, boolean proArg)
  {
    rb.selectedProperty().addListener((observable, oldSelected, newSelected) -> argNameSelect(newSelected, proArg));

    tf.textProperty().addListener((observable, oldText, newText) ->
    {
      if (!revising)
        rb.setSelected(true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void argNameSelect(Boolean newSelected, boolean proArg)
  {
    {
      if (newSelected == null) return;

      if (newSelected.booleanValue())
      {
        if (proArg)
          hcbPositionVerdict.selectID(1);
        else
          hcbPositionVerdict.selectID(2);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void reviseSuggestions()
  {
    String part1 = "";

    revising = true;

    if (cbIncludeAuth.isSelected())
    {
      HDT_Person person = hcbPerson.selectedRecord();

      if (person != null) part1 = person.getLastName() + "'s ";
    }

    String part2 = part1.length() == 0 ? "Argument " : "argument ";

    tfArgName1.setText(part1 + part2 + "for " + curPosition.name());
    tfArgName2.setText(part1 + part2 + "for the " + curPosition.name());
    tfArgName3.setText(part1 + part2 + "that " + curPosition.name());
    tfArgName4.setText(part1 + part2 + "for the view that " + curPosition.name());
    tfArgName5.setText(part1 + part2 + "against " + curPosition.name());
    tfArgName6.setText(part1 + part2 + "against the " + curPosition.name());
    tfArgName7.setText(part1 + part2 + "that it is false that " + curPosition.name());
    tfArgName8.setText(part1 + part2 + "against the view that " + curPosition.name());

    revising = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
