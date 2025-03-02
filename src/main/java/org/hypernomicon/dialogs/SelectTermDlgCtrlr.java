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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.Exceptions.SearchKeyTooShortException;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Glossary;
import org.hypernomicon.model.records.HDT_Term;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.CustomPopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;

//---------------------------------------------------------------------------

public class SelectTermDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private ComboBox<HyperTableCell> cbTerm, cbGlossary, cbSense;
  @FXML private Button btnCreate;
  @FXML private TextField tfSearchKey;

  private final HyperCB hcbTerm, hcbGlossary, hcbSense;
  private final HDT_Concept concept;

  private HDT_Glossary glossaryToUse;
  private HDT_ConceptSense senseToUse;
  private boolean creatingNewTerm, alreadyChanging = false;
  private HDT_Term term;

//---------------------------------------------------------------------------

  public SelectTermDlgCtrlr(HDT_Concept concept, HDT_RecordWithMainText unitingWith)
  {
    super("SelectTermDlg", "Term Select", true);

    this.concept = concept;

    StringBuilder sb = new StringBuilder();

    Predicate<Integer> termIDFilter = termID ->
    {
      if ((concept != null) && (concept.term.getID() == termID))
        return false;

      if (unitingWith == null)
        return true;

      for (HDT_Concept otherConcept : db.terms.getByID(termID).concepts)
        if (HDT_Hub.canUnite(otherConcept, unitingWith, sb))
          return true;

      return false;
    };

    hcbTerm = new HyperCB(cbTerm, ctDropDownList, new StandardPopulator(hdtTerm, termIDFilter));

    CustomPopulator glossaryPop = new CustomPopulator(hdtGlossary, (row, force) ->
    {
      HDT_Term tempTerm = hcbTerm.selectedRecord();
      if (tempTerm == null) return Stream.empty();

      return tempTerm.concepts.stream().filter(curConcept -> (unitingWith == null) || HDT_Hub.canUnite(curConcept, unitingWith, sb))
                                       .map(curConcept -> curConcept.glossary.get()).distinct();
    });

    hcbGlossary = new HyperCB(cbGlossary, ctDropDownList, concept == null ? glossaryPop : new StandardPopulator(hdtGlossary));

    CustomPopulator sensePop = new CustomPopulator(hdtConceptSense, (row, force) ->
    {
      HDT_Term tempTerm = hcbTerm.selectedRecord();
      if (tempTerm == null) return Stream.empty();

      HDT_Glossary tempGlossary = hcbGlossary.selectedRecord();
      if (tempGlossary == null) return Stream.empty();

      if (concept == null)
        return tempTerm.concepts.stream().filter(curConcept -> curConcept.glossary.get() == tempGlossary)
                                         .filter(curConcept -> (unitingWith == null) || HDT_Hub.canUnite(curConcept, unitingWith, sb))
                                         .map(curConcept -> curConcept.sense.get());

      return db.conceptSenses.stream().filter(curSense -> tempTerm.getConcept(tempGlossary, curSense) == null);
    });

    hcbSense = new HyperCB(cbSense, concept == null ? ctDropDownList : ctDropDown, sensePop);

    hcbTerm.addListener((oldCell, newCell) ->
    {
      if (alreadyChanging) return;

      alreadyChanging = true;

      List<? extends HyperTableCell> glossaryCells = hcbGlossary.populate(true);

      if (HyperTableCell.getRecord(newCell) != null)
      {
        boolean selectedGlossary = false;

        if (concept == null)
        {
          if (glossaryCells.stream().anyMatch(cell -> cell.getID() == 1))
          {
            hcbGlossary.selectID(1);
            selectedGlossary = true;
          }
        }
        else if (glossaryCells.stream().anyMatch(cell -> cell.getID() == concept.glossary.getID()))
        {
          hcbGlossary.selectID(concept.glossary.getID());
          selectedGlossary = true;
        }

        if (selectedGlossary == false)
          hcbGlossary.select(glossaryCells.isEmpty() ? null : glossaryCells.get(0));
      }
      else
        hcbGlossary.select(null);

      alreadyChanging = false;
    });

    MainCtrlr.setSearchKeyToolTip(tfSearchKey);

    btnCreate.setOnAction(event -> btnCreateClick());
    creatingNewTerm = false;
  }

//---------------------------------------------------------------------------

  public HDT_Term         getTerm           () { return term; }
  public boolean          getCreatingNewTerm() { return creatingNewTerm; }
  public HDT_Glossary     getGlossary       () { return glossaryToUse != null ? glossaryToUse : hcbGlossary.selectedRecord(); }
  public HDT_ConceptSense getSense          () { return senseToUse    != null ? senseToUse    : hcbSense   .selectedRecord(); }
  public String           getSenseText      () { return ultraTrim(hcbSense.getText()); }

  public void             moveConcept       () { concept.moveToDifferentTerm(term, creatingNewTerm, getGlossary(), getSense(), getSenseText()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnCreateClick()
  {
    if (ultraTrim(tfSearchKey.getText()).isBlank())
    {
      falseWithErrorPopup("Unable to create term record: search key of term cannot be blank.", tfSearchKey);
      return;
    }

    HDT_Glossary generalGlossary = db.glossaries.getByID(1);

    term = concept == null ?
      HDT_Term.create(generalGlossary)
    :
      db.createNewBlankRecord(hdtTerm); // Old concept gets attached to term record by caller

    try
    {
      term.setSearchKey(tfSearchKey.getText(), true, true);
    }
    catch (SearchKeyException e)
    {
      errorPopup(e instanceof SearchKeyTooShortException ?
        "Unable to create term record. Search key must be at least 3 characters: " + e.getKey()
      :
        "Unable to create term record. Search key already exists: " + e.getKey());

      db.deleteRecord(term);
      term = null;

      safeFocus(tfSearchKey);

      return;
    }

    if (concept != null)
    {
      glossaryToUse = concept.glossary.get();
      senseToUse = concept.sense.get();
    }
    else
      glossaryToUse = generalGlossary;

    okClicked = true;
    creatingNewTerm = true;
    dialogStage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (hcbTerm.selectedRecord() == null)
      return falseWithErrorPopup("You must select a term.", cbTerm);

    glossaryToUse = hcbGlossary.selectedRecord();

    if (glossaryToUse == null)
      return falseWithErrorPopup("You must select a glossary.", cbGlossary);

    term = hcbTerm.selectedRecord();

    if (concept != null)
    {
      if (getSense() == null)
      {
        String senseText = getSenseText();

        if (senseText.isBlank())
        {
          if (term.getConcept(glossaryToUse, null) != null)
            return falseWithErrorPopup("The term already has a definition for that glossary and sense.", cbSense);
        }
        else
        {
          for (HDT_ConceptSense sense : db.conceptSenses)
          {
            if (ultraTrim(sense.name()).equalsIgnoreCase(ultraTrim(senseText)))
              return falseWithErrorPopup("The term already has a definition for that glossary and sense.", cbSense);
          }
        }
      }

      if ((concept.glossary.get() != getGlossary()) &&
          ((concept.parentConcepts.isEmpty() == false) || (concept.subConcepts.isEmpty() == false)) &&
          (confirmDialog("This will unassign any parent or child concepts for Term \"" + concept.listName() + "\", Glossary \"" + concept.glossary.get().name() + "\" since the conept is being moved to a different glossary. Proceed?", false) == false))
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
