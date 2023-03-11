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

package org.hypernomicon.dialogs;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.List;
import java.util.stream.Stream;

import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Glossary;
import org.hypernomicon.model.records.HDT_Term;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;
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
  @FXML private ComboBox<HyperTableCell> cbTerm, cbGlossary, cbSense;
  @FXML private Button btnCreate;
  @FXML private TextField tfSearchKey;

  private final HyperCB hcbTerm, hcbGlossary, hcbSense;
  private final HDT_Concept oldConcept;

  private HDT_Glossary glossary;
  private boolean createNew, alreadyChanging = false;
  private HDT_Term term;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Term         getTerm()      { return term; }
  public boolean          getCreateNew() { return createNew; }
  public HDT_Glossary     getGlossary()  { return glossary != null ? glossary : hcbGlossary.selectedRecord(); }
  public HDT_ConceptSense getSense()     { return hcbSense.selectedRecord(); }
  public String           getSenseText() { return ultraTrim(hcbSense.getText()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public SelectConceptDlgCtrlr(HDT_Concept oldConcept)
  {
    super("SelectConceptDlg", "Term Select", true);

    this.oldConcept = oldConcept;

    hcbTerm = new HyperCB(cbTerm, ctDropDownList, new StandardPopulator(hdtTerm));

    CustomPopulator glossaryPop = new CustomPopulator(hdtGlossary, (row, force) ->
    {
      HDT_Term tempTerm = hcbTerm.selectedRecord();
      if (tempTerm == null) return Stream.empty();

      return tempTerm.concepts.stream().map(curConcept -> curConcept.glossary.get()).distinct();
    });

    hcbGlossary = new HyperCB(cbGlossary, ctDropDownList, oldConcept == null ? glossaryPop : new StandardPopulator(hdtGlossary));

    CustomPopulator sensePop = new CustomPopulator(hdtConceptSense, (row, force) ->
    {
      HDT_Term tempTerm = hcbTerm.selectedRecord();
      if (tempTerm == null) return Stream.empty();

      HDT_Glossary tempGlossary = hcbGlossary.selectedRecord();
      if (tempGlossary == null) return Stream.empty();

      if (oldConcept == null)
        return tempTerm.concepts.stream().filter(curConcept -> curConcept.glossary.get() == tempGlossary)
                                         .map(curConcept -> curConcept.sense.get());

      return db.conceptSenses.stream().filter(curSense -> tempTerm.getConcept(tempGlossary, curSense) == null);
    });

    hcbSense = new HyperCB(cbSense, oldConcept == null ? ctDropDownList : ctDropDown, sensePop);

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
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnCreateClick()
  {
    if (tfSearchKey.getText().isEmpty())
    {
      falseWithErrorMessage("Unable to create term record: search key of term cannot be zero-length.", tfSearchKey);
      return;
    }

    HDT_Glossary generalGlossary = db.glossaries.getByID(1);
    term = HDT_Term.create(generalGlossary);

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

    glossary = generalGlossary;

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

    glossary = hcbGlossary.selectedRecord();

    if (glossary == null)
      return falseWithErrorMessage("You must select a glossary.", cbGlossary);

    term = hcbTerm.selectedRecord();

    if ((oldConcept != null) && (getSense() == null))
    {
      String senseText = getSenseText();

      if (senseText.isBlank())
      {
        if (term.getConcept(glossary, null) != null)
          return falseWithErrorMessage("The term already has a definition for that glossary and sense.", cbSense);
      }
      else
      {
        for (HDT_ConceptSense sense : db.conceptSenses)
        {
          if (sense.name().equalsIgnoreCase(senseText))
            return falseWithErrorMessage("The term already has a definition for that glossary and sense.", cbSense);
        }
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
