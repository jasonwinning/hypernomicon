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

package org.hypernomicon.view.tabs;

import org.hypernomicon.view.HyperView;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.cellValues.AbstractHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.tableCells.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.input.KeyEvent;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.io.IOException;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

import org.hypernomicon.dialogs.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;

//---------------------------------------------------------------------------

public final class TermTabCtrlr extends HyperNodeTab<HDT_Term, HDT_Concept>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class ParentConceptsCell extends AbstractHTC
  {
    private final List<HDT_Concept> parentConcepts;

    ParentConceptsCell(Collection<HDT_Concept> parentConcepts)
    {
      super(false);

      this.parentConcepts = List.copyOf(parentConcepts);
    }

    @Override public int getID()                { return -1; }
    @Override public RecordType getRecordType() { return hdtConcept; }

    @Override public boolean isEmpty()          { return parentConcepts.isEmpty(); }
    @Override public String getText()           { return parentConcepts.stream().map(parentConcept -> parentConcept.extendedName(false))
                                                                                .collect(Collectors.joining("; ")); }

    @Override public HyperTableCell getCopyWithID(int newID) { throw new UnsupportedOperationException("copy"); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class ConceptTab extends Tab
  {
    private HDT_Concept concept;

  //---------------------------------------------------------------------------

    private ConceptTab(HDT_Concept concept)
    {
      super(tabName(concept));

      this.concept = concept;
      setClosable(false);
    }

  //---------------------------------------------------------------------------

    private ConceptTab(Node content)
    {
      super(HDT_Glossary.ROOT_GLOSSARY_NAME, content);
      setClosable(false);
    }

  //---------------------------------------------------------------------------

    private void setConcept(HDT_Concept concept)
    {
      this.concept = concept;
      updateName();
    }

  //---------------------------------------------------------------------------

    private void updateName()
    {
      setText(tabName(concept));
    }

  //---------------------------------------------------------------------------

    private static String tabName(HDT_Concept concept)
    {
      String glossaryName = concept.glossary.get().name(),
             senseName = concept.sense.isNull() ? "" : concept.sense.get().name();

      if (concept.glossary.getID() == 1)
        return senseName.isBlank() ? glossaryName : senseName;

      if (senseName.isBlank())
        return glossaryName;

      return senseName + " (" + glossaryName + ')';
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final class ConceptRow
  {
    private final List<HDT_Concept> parentConcepts;
    private final HDT_Glossary glossary;
    private final HDT_ConceptSense sense;
    private final HDT_Concept concept;
    private final String senseText;

  //---------------------------------------------------------------------------

    private ConceptRow(HyperTableRow row, boolean validate)
    {
      HDT_Glossary tempGlossary = row.getRecord(2);
      HDT_ConceptSense tempSense = row.getRecord(3);
      senseText = row.getText(3);

      if ((tempSense == null) && (senseText.isBlank() == false))
        concept = null;
      else
        concept = curTerm.getConcept(tempGlossary, tempSense);

      if (validate && (concept == null))
      {
        tempGlossary = null;
        sense = null;
      }
      else
        sense = row.getRecord(3);

      glossary = tempGlossary;

      parentConcepts = getParentConcepts(row).stream().filter(parentConcept -> parentConcept.glossary.get() == row.getRecord(2))
                                                      .filter(parentConcept -> (validate == false) || concept.parentConcepts.contains(parentConcept))
                                                      .toList();
    }

  //---------------------------------------------------------------------------

    private ConceptRow()
    {
      glossary = null;
      concept = null;
      sense = null;
      senseText = "";
      parentConcepts = List.of();
    }

  //---------------------------------------------------------------------------

    private ConceptRow(HDT_Concept concept)
    {
      this.concept = concept;
      glossary = concept.glossary.get();
      sense = concept.sense.get();
      senseText = sense == null ? "" : sense.name();
      parentConcepts = List.copyOf(concept.parentConcepts);
    }

  //---------------------------------------------------------------------------

    private void populateTableRow(HyperTableRow row)
    {
      boolean wasUpdatingConcepts = updatingConcepts;
      updatingConcepts = true;

      if (glossary == null)
        row.setCellValue(2, "", hdtGlossary);
      else
        row.setCellValue(2, glossary, glossary.name());

      if (sense == null)
        row.setCellValue(3, "", hdtConceptSense);
      else
        row.setCellValue(3, sense, sense.listName());

      row.setCellValue(4, new ParentConceptsCell(parentConcepts));

      updatingConcepts = wasUpdatingConcepts;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperTable htConcepts, htSubConcepts, htDisplayers;
  private final TabPane tpConcepts;
  private final Map<HyperTableRow, ConceptRow> conceptRows = new HashMap<>();
  private final Map<HDT_Concept, TextViewInfo> conceptToTextViewInfo = new HashMap<>();

  private HDT_Term curTerm;
  private HDT_Concept curConcept;
  private Instant lastArrowKey = Instant.EPOCH;
  private boolean alreadyChangingTab = false, updatingConcepts = false;

//---------------------------------------------------------------------------

  public TermTabCtrlr(Tab tab) throws IOException
  {
    super(termTabEnum, tab);

    lblParentCaption.setText("Concepts:");

    TableColumn<HyperTableRow, HyperTableCell> senseCol = new TableColumn<>("Sense");
    senseCol.setPrefWidth(150.0);
    tvParents.getColumns().add(3, senseCol);

    tvParents.getColumns().get(2).setText("Glossary");
    tvParents.getColumns().get(4).setText("Parent Concept(s)");

    tvLeftChildren.getColumns().get(1).setText("Sub-Concepts Under This Concept");
    tvLeftChildren.getColumns().get(2).setText("Definition");

    spMain.getItems().remove(1);
    tpConcepts = new TabPane(new ConceptTab(apDescription));
    spMain.getItems().add(1, tpConcepts);
    tpConcepts.getTabs().getFirst().setClosable(false);

    tvRightChildren.getColumns().get(0).setText("Type");
    tvRightChildren.getColumns().get(1).setText("Name of record showing this definition");
    tvRightChildren.getColumns().add(new TableColumn<>("Description"));
    spMain.setDividerPosition(1, 0.85);

    htConcepts = new HyperTable(tvParents, 2, true, TablePrefKey.TERM_GLOSSARIES);

    htConcepts.addActionColWithButtonHandler(ctGoBtn, 2,
      (row, colNdx) ->
      {
        HDT_Concept parentConcept = getParentConcepts(row).stream().findFirst().orElse(null);
        ui.goToRecord(parentConcept != null ? parentConcept : row.getRecord(2), true);
      })
      .setTooltip(ButtonAction.baGo, row ->
      {
        HDT_Concept parentConcept = getParentConcepts(row).stream().findFirst().orElse(null);
        if (parentConcept != null) return "Go to Concept record: " + parentConcept.extendedName(true);

        HDT_Glossary glossary = row.getRecord(2);
        return glossary == null ? null : "Go to Glossary record \"" + glossary.name() + "\" in Tree";
      });

    htConcepts.addActionColWithButtonHandler(ctBrowseBtn, 2,
      (row, colNdx) ->
      {
        HDT_Glossary glossary = row.getRecord(2);
        HDT_Concept concept = glossary == null ? null : curTerm.getConcept(glossary, row.getRecord(3));

        ui.treeSelector.reset(concept == null ? curTerm : concept, true);

        ui.treeSelector.addTargetType(hdtGlossary);
        ui.treeSelector.addTargetType(hdtConcept);

        ui.treeSelector.setTarget(null);  // Target is not used for rtGlossaryOfConcept and rtParentConceptOfConcept

        HDT_Glossary generalGlossary = db.glossaries.getByID(1);         // If these two lines are combined into one, there will be
        ui.goToTreeRecord(concept == null ? generalGlossary : concept);  // false-positive build errors

      }).setTooltip(ButtonAction.baBrowse, "Select a Glossary or add a parent Concept from the Tree");

    htConcepts.addColWithUpdateHandler(hdtGlossary, ctEditableLimitedDropDown, (row, cellVal, nextColNdx, nextPopulator) -> handleGlossaryEdit(row))
              .setDontCreateNewRecord(true);

    htConcepts.addColWithUpdateHandler(hdtConceptSense, ctEditableUnlimitedDropDown, (row, cellVal, nextColNdx, nextPopulator) -> handleSenseEdit(row, cellVal));

    htConcepts.addClickToEditCol(hdtConcept, makeTooltip("Click cell to modify parent concepts"), (row, colNdx) -> showParentConceptSelectDialog(row));

    for (int ndx = 0; ndx < 8; ndx++)
    {
      final int finalNdx = ndx;

      htConcepts.addContextMenuItem
      (
        row -> (getParentConcepts(row).size() > finalNdx) ? ("Go to Concept record: " + getParentConcepts(row).get(finalNdx).extendedName(false)) : "",
        row -> getParentConcepts(row).size() > finalNdx,
        row -> ui.goToRecord(getParentConcepts(row).get(finalNdx), true)
      );
    }

    htConcepts.addContextMenuItem
    (
      row -> (row.getRecord(2) != null) ? ("Go to Glossary record \"" + row.getRecord(2).name() + "\" in Tree") : "",
      row -> row.getRecord(2) != null,
      row -> ui.goToTreeRecord(row.getRecord(2))
    );

    htConcepts.addContextMenuItem("Change order of concepts",
      row -> (row.getRecord(2) != null) && (curTerm.concepts.size() > 1),

      row ->
      {
        TableView<HyperTableRow> tv = new TableView<>();
        HyperTable ht = new HyperTable(tv, 0, false, "");

        TableColumn<HyperTableRow, HyperTableCell> tc = new TableColumn<>();
        tc.setText("Glossary Name");
        tv.getColumns().add(tc);

        tc = new TableColumn<>();
        tc.setText("Sense");
        tv.getColumns().add(tc);

        ht.addLabelCol(hdtGlossary);
        ht.addLabelCol(hdtConceptSense);

        ht.buildRows(curTerm.concepts, (innerRow, concept) ->
        {
          HDT_Glossary glossary = concept.glossary.get();
          innerRow.setCellValue(0, glossary, glossary.name());

          HDT_ConceptSense sense = concept.sense.get();
          if (sense == null)
            innerRow.setCellValue(1, "", hdtConceptSense);
          else
            innerRow.setCellValue(1, sense, sense.listName());
        });

        Runnable completeHndlr = () ->
        {
          db.<HDT_Concept, HDT_Term>getObjectList(rtConceptOfTerm, curTerm, true).reorder(ht.dataRowStream().map(tableRow -> curTerm.getConcept(tableRow.getRecord(0), tableRow.getRecord(1))).toList());
          ui.update();
        };

        ht.triggerChangeOrder(false, completeHndlr);
      });

    htConcepts.addContextMenuItem("Remove this row",
      row -> (conceptRows.get(row) != null) && (curTerm.concepts.size() > 1),

      this::removeRow);

    htSubConcepts = new HyperTable(tvLeftChildren, 2, true, TablePrefKey.CONCEPT_SUB);

    htSubConcepts.addGoNewCol(hdtConcept, 2);
    htSubConcepts.addLabelCol(hdtConcept);
    htSubConcepts.addLabelCol(hdtConcept, smTextSimple);

    htDisplayers = new HyperTable(tvRightChildren, 1, false, TablePrefKey.TERM_DISPLAYERS);

    htDisplayers.addIconCol();
    htDisplayers.addLabelCol(hdtNone);
    htDisplayers.addLabelCol(hdtNone);

    htSubConcepts.addDefaultMenuItems();
    htDisplayers .addDefaultMenuItems();

    tpConcepts.addEventFilter(KeyEvent.ANY, event ->
    {
      if (event.getCode().isArrowKey())
        lastArrowKey = Instant.now();
    });

    tpConcepts.getSelectionModel().selectedItemProperty().addListener((ob, oldTab, newTab) ->
    {
      if (alreadyChangingTab) return;

      if ((milliDiff(Instant.now(), lastArrowKey) < IGNORE_ARROW_KEYS_IN_TAB_PANE_MS) || !super.saveToRecord())
      {
        alreadyChangingTab = true;
        tpConcepts.getSelectionModel().select(oldTab);
        alreadyChangingTab = false;

        return;
      }

      oldTab.setContent(null);
      newTab.setContent(apDescription);

      conceptToTextViewInfo.put(curConcept, mainTextInfo(curConcept));
      curConcept = ((ConceptTab) newTab).concept;

      ui.viewSequence.saveViewToCurrentSlotAndTab(new HyperView<>(getTabEnum(), curConcept, conceptToTextViewInfo.get(curConcept)));

      HDT_Glossary glossary = curConcept.glossary.get();
      if (glossary.getID() > 1) glossary.viewNow();
      super.updateFromRecord();

      updateDisplayersAndSubConcepts();

      ui.updateFavorites();
    });
  }

//---------------------------------------------------------------------------

  @Override protected RecordType type()                { return hdtTerm; }
  @Override protected void setRecord(HDT_Concept rec)  { curConcept = rec; curTerm = curConcept == null ? null : curConcept.term.get(); }
  @Override protected HDT_Concept getNodeRecord()      { return curConcept; }

  private ConceptTab curTab()                          { return (ConceptTab) tpConcepts.getSelectionModel().getSelectedItem(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateFromRecord()
  {
    boolean first = true;
    for (HDT_Concept concept : curTerm.concepts)
    {
      conceptToTextViewInfo.put(concept, concept == curConcept ? getView().getTextInfo() : new TextViewInfo(concept));

      if (first)
      {
        ((ConceptTab) tpConcepts.getTabs().getFirst()).setConcept(concept);
        first = false;
      }
      else
        tpConcepts.getTabs().add(new ConceptTab(concept));
    }

    alreadyChangingTab = true;
    tpConcepts.getSelectionModel().select(getConceptTab(curConcept));
    alreadyChangingTab = false;

    tpConcepts.getTabs().getFirst().setContent(null);
    getConceptTab(curConcept).setContent(apDescription);

    super.updateFromRecord();

    clearAndRepopulateConceptsTable();

    updateDisplayersAndSubConcepts();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearAndRepopulateConceptsTable()
  {
    updatingConcepts = true;

    htConcepts .clear();
    conceptRows.clear();

    htConcepts.buildRows(curTerm.concepts.stream().map(ConceptRow::new), (row, conceptRow) ->
    {
      conceptRows.put(row, conceptRow);

      conceptRow.populateTableRow(row);
    });

    updatingConcepts = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateDisplayersAndSubConcepts()
  {
    htSubConcepts.clear();
    htDisplayers .clear();

    htDisplayers.buildRows(db.displayerStream(curConcept), (row, displayer) ->
    {
      row.setIconCellValue(0, displayer);
      row.setCellValue(1, displayer, displayer.getCBText());
      row.setCellValue(2, displayer, displayer.getMainText().getPlainForDisplay());
    });

    htSubConcepts.buildRows(curConcept.subConcepts, (row, subConcept) ->
    {
      row.setCellValue(1, subConcept, subConcept.name());
      row.setCellValue(2, subConcept, subConcept.getMainText().getPlainForDisplay());
    });

    htSubConcepts.sortAscending(1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeRow(HyperTableRow row)
  {
    ConceptRow conceptRow = conceptRows.get(row);
    if (conceptRow == null) return;

    if (removeConcept(conceptRow.glossary, conceptRow.sense))
    {
      conceptRows.remove(row);
      htConcepts.removeRow(row);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void handleSenseEdit(HyperTableRow editedRow, HyperTableCell newCell)
  {
    if (updatingConcepts) return;

    ConceptRow oldConceptRow = nullSwitch(conceptRows.get(editedRow), new ConceptRow());

    HDT_Concept concept = oldConceptRow.concept;

    // The only way concept would be null here is if the user edited the Sense for a blank row.
    if (concept == null)
    {
      if (HyperTableCell.getCellText(newCell).isBlank())
      {
        // The user must have edited the Sense for a blank row and changed it back to blank so do nothing.

        return;
      }

      // Glossary is currently blank. Set it to General if the current term is already in
      // the General glossary; otherwise set it to whichever glossary the first concept is in
      HDT_Glossary glossary = curTerm.concepts.stream().noneMatch(koncept -> koncept.glossary.get().getID() == 1) ?
        curTerm.concepts.getFirst().glossary.get()
      :
        db.glossaries.getByID(1);

      // This will cause handleGlossaryEdit to get called, which will create a concept record with the chosen sense
      editedRow.setCellValue(2, glossary, glossary.name());

      return;
    }

    HDT_ConceptSense sense = newCell.getRecord();
    String newText = HyperTableCell.getCellText(newCell).strip();

    if ((oldConceptRow.sense != null) && ((sense == oldConceptRow.sense) || newText.equalsIgnoreCase(oldConceptRow.sense.name())))
    {
      // The user typed text that matches the sense that was already assigned;
      // just update the row with the existing data

      oldConceptRow.populateTableRow(editedRow);
      return;
    }

    for (HyperTableRow row : htConcepts.dataRows())
    {
      if (row != editedRow)
      {
        HDT_Concept otherConcept = curTerm.getConcept(row.getRecord(2), row.getRecord(3));
        if ((concept != otherConcept) && (concept.glossary.get() == otherConcept.glossary.get()) && row.getText(3).equalsIgnoreCase(newText))
        {
          errorPopup("This term already has a concept in the same glossary with the same sense.");
          oldConceptRow.populateTableRow(editedRow);
          return;
        }
      }
    }

    if ((sense == null) && (newText.length() > 0))
    {
      sense = db.createNewBlankRecord(hdtConceptSense);
      sense.setName(newText);
    }

    concept.sense.set(sense);
    conceptRows.put(editedRow, new ConceptRow(concept));
    clearAndRepopulateConceptsTable();
    tpConcepts.getTabs().forEach(tab -> ((ConceptTab) tab).updateName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void handleGlossaryEdit(HyperTableRow editedRow)
  {
    if (updatingConcepts) return;

    ConceptRow oldConceptRow = conceptRows.get(editedRow),
               newConceptRow = new ConceptRow(editedRow, false);

    if ((conceptRows.size() == 1) && (newConceptRow.glossary == null))
    {
      oldConceptRow.populateTableRow(editedRow);
      return;
    }

    if (oldConceptRow == null)
      oldConceptRow = new ConceptRow();

    HDT_Concept newConcept = newConceptRow.concept;

// Check to see if existing glossary for row being edited should be removed/replaced
// ---------------------------------------------------------------------------------

    if (newConceptRow.glossary != oldConceptRow.glossary)
    {
      if (oldConceptRow.concept != null)
      {
        if ((newConceptRow.glossary == null) || (newConceptRow.concept != null))
        {
          if (removeConcept(oldConceptRow.glossary, oldConceptRow.sense) == false)
          {
            oldConceptRow.populateTableRow(editedRow);
            return;
          }
        }
        else
        {
          if (replaceGlossary(oldConceptRow.glossary, oldConceptRow.sense, newConceptRow.glossary) == false)
          {
            oldConceptRow.populateTableRow(editedRow);
            return;
          }

          newConcept = oldConceptRow.concept;
        }
      }

// Add new concept for row that was edited if needed
// -------------------------------------------------

      if ((newConceptRow.glossary != null) && (newConcept == null))
        addConceptInGlossary(newConceptRow.glossary, newConceptRow.sense);
    }

    newConceptRow = new ConceptRow(editedRow, true);
    conceptRows.put(editedRow, newConceptRow);
    newConceptRow.populateTableRow(editedRow);
    tpConcepts.getTabs().forEach(tab -> ((ConceptTab) tab).updateName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void merge()
  {
    if (ui.cantSaveRecord()) return;

    RecordDropdownDlgCtrlr<HDT_Term> rdd = new RecordDropdownDlgCtrlr<>(hdtTerm, id -> id != curTerm.getID());

    if (rdd.showModal() == false) return;

    HDT_Term otherTerm = rdd.getRecord();

    if ((otherTerm == null) || (otherTerm == curTerm)) return;

    for (HDT_Concept concept : curTerm.concepts)
      if (otherTerm.getConcept(concept.glossary.get(), concept.sense.get()) != null)
      {
        String msg = "Both terms already have definitions for ";

        if ((concept.sense.get() != null) || curTerm.hasMultipleSensesInGlossary(concept.glossary.get())
                                          || otherTerm.hasMultipleSensesInGlossary(concept.glossary.get()))
          msg = msg + "sense " + (concept.sense.isNull() ? "(unspecified)" : ('"' + concept.sense.get().name() + '"')) + " in ";

        msg = msg + "glossary \"" + concept.glossary.get().name() + '"';

        errorPopup(msg);
        return;
      }

    if (new MergeTermDlgCtrlr(curTerm, otherTerm).showModal())
      ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void moveConcept()
  {
    if (curTerm.concepts.size() < 2)
    {
      if (confirmDialog("There is only one definition for this Term. Do you want to choose another Term to merge with this one?", false))
        merge();

      return;
    }

    if (ui.cantSaveRecord()) return;

    HDT_Concept concept = curConcept;

    SelectTermDlgCtrlr selectTermDlgCtrlr = SelectTermDlgCtrlr.showPopupToMoveConceptToADifferentTerm(concept);

    if (selectTermDlgCtrlr.showModal() == false)
    {
      ui.update();
      return;
    }

    switchToDifferentTab();

    tpConcepts.getTabs().remove(getConceptTab(concept));

    conceptToTextViewInfo.remove(concept);

    selectTermDlgCtrlr.moveConcept();

    ui.goToRecord(concept, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void switchToDifferentTab()
  {
    tpConcepts.getSelectionModel().select(tpConcepts.getTabs().indexOf(curTab()) == 0 ? 1 : 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean replaceGlossary(HDT_Glossary oldGlossary, HDT_ConceptSense sense, HDT_Glossary newGlossary)
  {
    HDT_Concept concept = curTerm.getConcept(oldGlossary, sense);

    if (concept.replaceGlossaryInteractive(newGlossary) == false)
      return false;

    updateDisplayersAndSubConcepts();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean removeConcept(HDT_Glossary glossary, HDT_ConceptSense sense)
  {
    HDT_Concept concept = curTerm.getConcept(glossary, sense);

    if (concept == null) return true;

    if (ui.cantSaveRecord()) return false;

    if ((concept.getMainText().isEmpty() == false) || concept.hasHub())
    {
      String prompt = "Are you sure you want to remove the concept definition associated with the glossary \"" + glossary.name() + '"';

      if (confirmDialog(prompt + (sense == null ? "?" : (", sense \"" + sense.name() + "\"?")), false) == false)
        return false;
    }

    if (curTab().concept == concept)
      switchToDifferentTab();

    tpConcepts.getTabs().remove(getConceptTab(concept));
    conceptToTextViewInfo.remove(concept);
    db.deleteRecord(concept);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ConceptTab getConceptTab(HDT_Concept concept)
  {
    return (ConceptTab) findFirst(tpConcepts.getTabs(), tab -> ((ConceptTab) tab).concept == concept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addConceptInGlossary(HDT_Glossary glossary, HDT_ConceptSense sense)
  {
    HDT_Concept concept = db.createNewBlankRecord(hdtConcept);

    curTerm.concepts.add(concept);
    concept.glossary.set(glossary);
    concept.sense.set(sense);

    conceptToTextViewInfo.put(concept, new TextViewInfo(concept));

    tpConcepts.getTabs().add(new ConceptTab(concept));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    super.clear(resetRecord);

    conceptToTextViewInfo.clear();

    tpConcepts.getTabs().stream().filter(tab -> tab.getContent() == apDescription).forEach(tab -> tab.setContent(null));

    alreadyChangingTab = true;
    while (tpConcepts.getTabs().size() > 1)
      tpConcepts.getTabs().remove(1);
    alreadyChangingTab = false;

    tpConcepts.getTabs().getFirst().setContent(apDescription);

    curTerm    = resetRecord ? null : HDT_Record.getCurrentInstance(curTerm);
    curConcept = resetRecord ? null : HDT_Record.getCurrentInstance(curConcept);

    htConcepts   .clear();
    htSubConcepts.clear();
    htDisplayers .clear();

    conceptRows.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void newClick(RecordType objType, HyperTableRow row)
  {
    if (ui.cantSaveRecord()) return;

    switch (objType)
    {
      case hdtConcept :

        if (curConcept != null)
          ui.goToRecord(curConcept.addNewSubConcept(), false);

        break;

      default:
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showParentConceptSelectDialog(HyperTableRow tableRow)
  {
    ConceptRow conceptRow = conceptRows.get(tableRow);

    HDT_Concept concept = conceptRow == null ? null : conceptRow.concept;

    if ((concept == null) || (concept.glossary.isNull()) || ui.cantSaveRecord()) return;

    if (new ParentConceptDlgCtrlr(concept).showModal())
      ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<HDT_Concept> getParentConcepts(HyperTableRow row)
  {
    return row.getCell(4) instanceof ParentConceptsCell parentConceptsCell ?
      parentConceptsCell.parentConcepts
    :
      List.of();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spMain    , DividerPositionPrefKey.TERM_TOP_VERT    , 0);
    setDividerPosition(spMain    , DividerPositionPrefKey.TERM_BOTTOM_VERT , 1);
    setDividerPosition(spChildren, DividerPositionPrefKey.TERM_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spMain    , DividerPositionPrefKey.TERM_TOP_VERT    , 0);
    getDividerPosition(spMain    , DividerPositionPrefKey.TERM_BOTTOM_VERT , 1);
    getDividerPosition(spChildren, DividerPositionPrefKey.TERM_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
