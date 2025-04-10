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

import static org.hypernomicon.App.ui;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.HyperCB;

import javafx.beans.property.Property;
import javafx.fxml.FXML;
import javafx.scene.control.*;

//---------------------------------------------------------------------------

/**
 * This popup window is used in two scenarios:<br>
 * 1) The user wants to move an existing concept to another term<br>
 * 2) The user wants to unite a non-Term record to a Term record<br>
 * <br>
 * In scenario 1, the user either picks a glossary/sense for an existing
 * term that <u><b>doesn't</b></u> have a concept for that glossary/sense yet, or chooses
 * to move the concept to the General glossary of a new Term.<br>
 * <br>
 * In scenario 2, the user either picks a glossary/sense for which there
 * <u><b>is</b></u> an existing Concept for an existing Term, or chooses to unite
 * the record to a new Term.
 */
public final class SelectTermDlgCtrlr extends ModalDialog
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

  /**
   * Show popup to allow user to select either to<br>
   * 1) Select an unused Glossary/Sense combination of an existing Term to move the Concept to<br>
   * 2) Create a new Term record to move the Concept to<br>
   * <br>
   * In the second scenario, the Glossary/Sense of the Concept doesn't change.
   * @param conceptToMove HDT_Concept record to be moved
   * @return Popup window
   * @throws NullPointerException if <code>conceptToMove</code> is null
   */
  public static SelectTermDlgCtrlr showPopupToMoveConceptToADifferentTerm(HDT_Concept conceptToMove)
  {
    Objects.requireNonNull(conceptToMove);

    return new SelectTermDlgCtrlr(conceptToMove, null);
  }

  /**
   * Show popup to allow user to select/create a Term record to unite another record to.<br>
   * <br>
   * The user can either:<br>
   * 1) Select a Glossary/Sense for an existing Term for which there is an existing Concept record
   * they want to unite the record with.<br>
   * 2) Create a new Term record. An associated Concept will be created in the General glossary
   * to unite the record with.<br>
   * Creating a new Concept record to unite with under an existing Term record is not currently supported.
   * @param nonTermRecord Record to unite the Concept with.
   * @return Popup window
   * @throws NullPointerException if <code>nonTermRecord</code> is null
   */
  public static SelectTermDlgCtrlr showPopupToChooseTermToUniteWith(HDT_RecordWithMainText nonTermRecord)
  {
    Objects.requireNonNull(nonTermRecord);

    return new SelectTermDlgCtrlr(null, nonTermRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * <p>If <code>concept</code> is null, then we are in the scenario of choosing a Concept to unite with.</p>
   * <p>If <code>unitingWith</code> is null, then we are in the scenario of choosing a Term to move the Concept to.</p>
   * <p>Exactly one of <code>concept</code> or <code>unitingWith</code> must be null, and the other must be non-null.</p>
   * @param concept The Concept to be moved (nullable).
   * @param unitingWith The non-Term record with which a Concept is to be united (nullable).
   * @throws IllegalArgumentException if both <code>concept</code> and <code>unitingWith</code> are null or both are non-null.
   */
  private SelectTermDlgCtrlr(HDT_Concept concept, HDT_RecordWithMainText unitingWith)
  {
    super("SelectTermDlg", "Term Select", true);

    if ((concept == null) == (unitingWith == null))
      throw new IllegalArgumentException("Exactly one of 'concept' or 'unitingWith' must be non-null, but not both.");

    this.concept = concept;

    StringBuilder sb = new StringBuilder();

    Predicate<Integer> termIDFilter = termID ->
    {
      if ((concept != null) && (concept.term.getID() == termID))
        return false;

      if (unitingWith == null)
        return true;

      return db.terms.getByID(termID).concepts.stream().anyMatch(otherConcept -> HDT_Hub.canUnite(otherConcept, unitingWith, sb));
    };

    hcbTerm = new HyperCB(cbTerm, ctEditableLimitedDropDown, new StandardPopulator(hdtTerm, termIDFilter));

    CustomPopulator glossaryPop = new CustomPopulator(hdtGlossary, (row, force) ->
    {
      HDT_Term tempTerm = hcbTerm.selectedRecord();
      if (tempTerm == null) return Stream.empty();

      return tempTerm.concepts.stream().filter(curConcept -> (unitingWith == null) || HDT_Hub.canUnite(curConcept, unitingWith, sb))
                                       .map(curConcept -> curConcept.glossary.get()).distinct();
    });

    hcbGlossary = concept == null ?
      new HyperCB(cbGlossary, ctNoneditableDropDown, glossaryPop)
    :
      new HyperCB(cbGlossary, ctEditableLimitedDropDown, new StandardPopulator(hdtGlossary));

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

    hcbSense = new HyperCB(cbSense, concept == null ? ctNoneditableDropDown : ctEditableUnlimitedDropDown, sensePop);

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

  private HDT_Glossary     getGlossary() { return glossaryToUse != null ? glossaryToUse : hcbGlossary.selectedRecord(); }
  private HDT_ConceptSense getSense   () { return senseToUse    != null ? senseToUse    : hcbSense   .selectedRecord(); }

  public void              moveConcept() { concept.moveToDifferentTerm(term, creatingNewTerm, getGlossary(), getSense(), ultraTrim(hcbSense.getText())); }

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
      HDT_Term.create(generalGlossary)  // We are in the scenario of uniting a non-Term record with a new Term; always default to General glossary
    :
      db.createNewBlankRecord(hdtTerm); // We are in the scenario of moving a concept to a new Term;
                                        // old concept gets attached to Term record in HDT_Concept.moveToDifferentTerm
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
      glossaryToUse = concept.glossary.get();  // We are in the scenario of moving a concept to a new Term;
      senseToUse = concept.sense.get();        // Glossary and Sense stay the same
    }
    else
      glossaryToUse = generalGlossary;  // We are in the scenario of uniting a non-Term record with a new Term; always default to General glossary

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

    HDT_Glossary glossary = hcbGlossary.selectedRecord();

    if (glossary == null)
      return falseWithErrorPopup("You must select a glossary.", cbGlossary);

    term = hcbTerm.selectedRecord();

    if (concept == null)  // The user is choosing a Term to unite another record with
    {
      HDT_ConceptSense sense = hcbSense.selectedRecord();
      HDT_Concept conceptToUnite = term.getConcept(glossary, sense);

      if (conceptToUnite == null)
      {
        if (sense == null)
          return falseWithErrorPopup("You must select a sense.", cbSense);

        return falseWithInternalErrorPopup(89680);
      }

      senseToUse = sense;
    }
    else  // The user is choosing a Term to move an existing Concept to
    {
      if (getSense() == null)
      {
        String senseText = ultraTrim(hcbSense.getText());

        if (senseText.isBlank())
        {
          if (term.getConcept(glossary, null) != null)
            return falseWithErrorPopup("The term already has a definition for that glossary and sense.", cbSense);
        }
        else
        {
          for (HDT_ConceptSense sense : db.conceptSenses)
          {
            if (ultraTrim(sense.name()).equalsIgnoreCase(ultraTrim(senseText)))
              return falseWithErrorPopup("The term already has a definition for that glossary and sense.", cbSense);
          }

          // In this case, we don't create a new HDT_ConceptSense record and set senseToUse here;
          // creating the HDT_ConceptSense record gets done in HDT_Concept.moveToDifferentTerm
        }
      }

      return (concept.glossary.get() == getGlossary()) ||
             (concept.parentConcepts.isEmpty() && concept.subConcepts.isEmpty()) ||
             confirmDialog("This will unassign any parent or child concepts for Term \"" + concept.listName() + "\", Glossary \"" + concept.glossary.get().name() + "\" since the conept is being moved to a different glossary. Proceed?", false);
    }

    glossaryToUse = glossary;
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean uniteWith(HDT_RecordWithMainText otherSpoke, MutableBoolean createdNewTerm, Property<HDT_Concept> conceptProp) throws HyperDataException
  {
    HDT_Concept concept = term.getConcept(getGlossary(), getSense());

    if (concept == null)
      throw new HDB_InternalError(89681);

    if (ui.uniteRecords(otherSpoke, concept) == false)
      return false;

    createdNewTerm.setValue(creatingNewTerm);
    conceptProp   .setValue(concept);

    if (creatingNewTerm)
      concept.term.get().setName(otherSpoke.listName());

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
