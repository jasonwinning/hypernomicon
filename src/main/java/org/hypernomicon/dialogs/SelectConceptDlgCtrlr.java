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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Glossary;
import org.hypernomicon.model.records.HDT_Term;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.populators.CustomPopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;

public class SelectConceptDlgCtrlr extends HyperDlg
{
  @FXML private ComboBox<HyperTableCell> cbTerm, cbGlossary;
  @FXML private Button btnCreate;
  @FXML private TextField tfSearchKey;

  private HyperCB hcbTerm, hcbGlossary;
  private boolean createNew, alreadyChanging = false;
  private HDT_Term term;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static SelectConceptDlgCtrlr build(HDT_Concept oldConcept)
  {
    return ((SelectConceptDlgCtrlr) create("SelectConceptDlg", "Term Select", true)).init(oldConcept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private SelectConceptDlgCtrlr init(HDT_Concept oldConcept)
  {
    hcbTerm = new HyperCB(cbTerm, ctDropDownList, new StandardPopulator(hdtTerm));

    CustomPopulator pop = new CustomPopulator(hdtGlossary, (row, force) ->
    {
      HDT_Term tempTerm = hcbTerm.selectedRecord();
      if (tempTerm == null) return Stream.empty();

      if (oldConcept == null)
        return tempTerm.concepts.stream().map(curConcept -> curConcept.glossary.get());

      List<HDT_Glossary> termGlossaries = tempTerm.getGlossaries();

      return db.glossaries.stream().filter(Predicate.not(termGlossaries::contains));
    });

    hcbGlossary = new HyperCB(cbGlossary, ctDropDownList, pop);

    hcbTerm.addBlankEntry();

    hcbTerm.addListener((oldCell, newCell) ->
    {
      if (alreadyChanging) return;

      alreadyChanging = true;

      List<HyperTableCell> glossaryCells = hcbGlossary.populate(true);

      boolean selectedGlossary = false;

      if (HyperTableCell.getRecord(newCell) != null)
      {
        if (oldConcept == null)
        {
          if (glossaryCells.stream().anyMatch(cell -> cell.getID() == 1))
          {
            hcbGlossary.selectID(1);
            selectedGlossary = true;
          }
        }
        else if (glossaryCells.stream().anyMatch(cell -> cell.getID() == oldConcept.glossary.getID()))
        {
          hcbGlossary.selectID(oldConcept.glossary.getID());
          selectedGlossary = true;
        }

        if (selectedGlossary == false)
          cbGlossary.getSelectionModel().select(glossaryCells.isEmpty() ? null : glossaryCells.get(0));
      }
      else
        cbGlossary.getSelectionModel().select(null);

      alreadyChanging = false;
    });

    MainCtrlr.setSearchKeyToolTip(tfSearchKey);

    btnCreate.setOnAction(event -> btnCreateClick());
    createNew = false;

    return this;
  }

  public HDT_Term     getTerm()      { return term; }
  public boolean      getCreateNew() { return createNew; }
  public HDT_Glossary getGlossary()  { return hcbGlossary.selectedRecord(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnCreateClick()
  {
    if (tfSearchKey.getText().isEmpty())
    {
      falseWithErrorMessage("Unable to create term record: search key of term cannot be zero-length.", tfSearchKey);
      return;
    }

    term = db.createNewBlankRecord(hdtTerm);

    try
    {
      term.setSearchKey(tfSearchKey.getText(), true, true);
    }
    catch (SearchKeyException e)
    {
      falseWithErrorMessage(e.getTooShort() ?
        "Unable to create term record: search key must be at least 3 characters."
      :
        "Unable to create term record: search key already exists.");

      db.deleteRecord(term);
      term = null;

      safeFocus(tfSearchKey);

      return;
    }

    okClicked = true;
    createNew = true;
    dialogStage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (hcbTerm.selectedRecord() == null)
      return falseWithErrorMessage("You must select a term.", cbTerm);

    if (hcbGlossary.selectedRecord() == null)
      return falseWithErrorMessage("You must select a glossary.", cbGlossary);

    term = hcbTerm.selectedRecord();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
