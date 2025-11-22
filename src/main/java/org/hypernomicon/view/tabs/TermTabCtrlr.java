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
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.cellValues.*;
import org.hypernomicon.view.tableCells.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableColumn.CellTestHandler;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.input.KeyEvent;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.io.IOException;
import java.time.Instant;
import java.util.*;
import java.util.stream.Collectors;

import org.hypernomicon.dialogs.*;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.Exceptions.SearchKeyTooShortException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;
import org.hypernomicon.testTools.FXTestSequencer;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.PopupRobot;

import static org.junit.jupiter.api.Assertions.*;

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
    private final HDT_Glossary glossary;
    private final HDT_ConceptSense sense;
    private final HDT_Concept concept;
    private final String senseText;
    private final boolean isDuplicate;  // If true, the parent concepts and search key should be blank and read-only

  //---------------------------------------------------------------------------

    private ConceptRow(HyperTableRow row, boolean validate, boolean isDuplicate)
    {
      HDT_Glossary tempGlossary = row.getRecord(GLOSSARY_COL_NDX);
      HDT_ConceptSense tempSense = row.getRecord(SENSE_COL_NDX);
      senseText = row.getText(SENSE_COL_NDX);

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
        sense = row.getRecord(SENSE_COL_NDX);

      glossary = tempGlossary;

      this.isDuplicate = isDuplicate;
    }

  //---------------------------------------------------------------------------

    private ConceptRow()
    {
      glossary = null;
      concept = null;
      sense = null;
      senseText = "";
      isDuplicate = false;
    }

  //---------------------------------------------------------------------------

    private ConceptRow(HDT_Concept concept)
    {
      this.concept = concept;
      glossary = concept.glossary.get();
      sense = concept.sense.get();
      senseText = sense == null ? "" : sense.name();
      isDuplicate = false;
    }

  //---------------------------------------------------------------------------

    private void populateTableRow(HyperTableRow row)
    {
      boolean wasUpdatingConcepts = updatingConcepts;
      updatingConcepts = true;

      if (glossary == null)
        row.setCellValue(GLOSSARY_COL_NDX, "", hdtGlossary);
      else
        row.setCellValue(GLOSSARY_COL_NDX, glossary);

      if (sense == null)
        row.setCellValue(SENSE_COL_NDX, "", hdtConceptSense);
      else
        row.setCellValue(SENSE_COL_NDX, sense);

      row.setCellValue(PARENTS_COL_NDX, new ParentConceptsCell((concept == null) || isDuplicate ? List.of() : concept.parentConcepts));

      if ((concept == null) || isDuplicate)
        row.setCellValue(SEARCHKEY_COL_NDX, "", hdtConcept);
      else
        row.setCellValue(SEARCHKEY_COL_NDX, concept, concept.getSearchKey());


      updatingConcepts = wasUpdatingConcepts;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final int GLOSSARY_COL_NDX = 2,
                    SENSE_COL_NDX = 3,
                    PARENTS_COL_NDX = 4,
                    SEARCHKEY_COL_NDX = 5;

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
    lblSearchKey.setText("Term Search Key:");

    TableColumn<HyperTableRow, HyperTableCell> col = new TableColumn<>("Sense");
    col.setPrefWidth(150.0);
    tvParents.getColumns().add(SENSE_COL_NDX, col);

    col = new TableColumn<>("Search Key");
    col.setPrefWidth(150.0);
    tvParents.getColumns().add(col);

    tvParents.getColumns().get(GLOSSARY_COL_NDX).setText("Glossary");
    tvParents.getColumns().get(PARENTS_COL_NDX ).setText("Parent Concept(s)");

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

    htConcepts = new HyperTable(tvParents, GLOSSARY_COL_NDX, true, TablePrefKey.TERM_GLOSSARIES);

    htConcepts.addActionColWithButtonHandler(ctGoBtn, GLOSSARY_COL_NDX,
      (row, colNdx) ->
      {
        HDT_Concept parentConcept = getParentConcepts(row).stream().findFirst().orElse(null);
        ui.goToRecord(parentConcept != null ? parentConcept : row.getRecord(GLOSSARY_COL_NDX), true);
      })
      .setButtonTooltip(ButtonAction.baGo, row ->
      {
        HDT_Concept parentConcept = getParentConcepts(row).stream().findFirst().orElse(null);
        if (parentConcept != null) return "Go to Concept record: " + parentConcept.extendedName(true);

        HDT_Glossary glossary = row.getRecord(GLOSSARY_COL_NDX);
        return glossary == null ? null : "Go to Glossary record \"" + glossary.name() + "\" in Tree";
      });

    htConcepts.addActionColWithButtonHandler(ctBrowseBtn, GLOSSARY_COL_NDX,
      (row, colNdx) ->
      {
        HDT_Glossary glossary = row.getRecord(GLOSSARY_COL_NDX);
        HDT_Concept concept = glossary == null ? null : curTerm.getConcept(glossary, row.getRecord(SENSE_COL_NDX));

        ui.treeSelector.reset(concept == null ? curTerm : concept, true);

        ui.treeSelector.addTargetType(hdtGlossary);

        if (concept != null)
          ui.treeSelector.addTargetType(hdtConcept);  // Can't assign a parent concept if there's no child concept yet

        ui.treeSelector.setTarget(null);  // Target is not used for rtGlossaryOfConcept and rtParentConceptOfConcept

        HDT_Glossary generalGlossary = db.glossaries.getByID(1);         // If these two lines are combined into one, there will be
        ui.goToTreeRecord(concept == null ? generalGlossary : concept);  // false-positive build errors
      })
      .setButtonTooltip(ButtonAction.baBrowse, row ->
      {
        HDT_Glossary glossary = row.getRecord(GLOSSARY_COL_NDX);
        HDT_Concept concept = glossary == null ? null : curTerm.getConcept(glossary, row.getRecord(SENSE_COL_NDX));

        return concept == null ? "Select a Glossary or create a new one" : "Select a Glossary, create a new Glossary, or add a parent Concept from the Tree";
      });

    htConcepts.addColWithUpdateHandler(hdtGlossary, ctEditableLimitedDropDown, (row, cellVal, nextColNdx, nextPopulator) -> handleGlossaryEdit(row))
      .setDontCreateNewRecord(true);

    htConcepts.addColWithUpdateHandler(hdtConceptSense, ctEditableUnlimitedDropDown, (row, cellVal, nextColNdx, nextPopulator) -> handleSenseEdit(row, cellVal));

    CellTestHandler cellTestHandler = (row, colNdx) ->
    {
      if (conceptRows.get(row).isDuplicate)
      {
        errorPopup("Enter a Sense first.");
        htConcepts.edit(row, SENSE_COL_NDX);
        return false;
      }

      return true;
    };

    htConcepts.addClickToEditCol(hdtConcept, makeTooltip("Click cell to modify parent concepts"), (row, colNdx) -> showParentConceptSelectDialog(row))
      .setCellToolTipHndlr(row -> makeTooltip(row.getText(PARENTS_COL_NDX)))
      .beginEditHandler.setValue(cellTestHandler);

    htConcepts.addTextEditColWithUpdateHandler(hdtConcept, false, (row, cellVal, nextColNdx, nextPopulator) -> handleSearchKeyEdit(row, cellVal))
      .setHeaderTooltip(MainCtrlr.getSearchKeyToolTip())
      .setCellToolTipHndlr(row -> MainCtrlr.getSearchKeyToolTip())
      .beginEditHandler.setValue(cellTestHandler);

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
      row -> (row.getRecord(GLOSSARY_COL_NDX) != null) ? ("Go to Glossary record \"" + row.getRecord(2).name() + "\" in Tree") : "",
      row -> row.getRecord(GLOSSARY_COL_NDX) != null,
      row -> ui.goToTreeRecord(row.getRecord(GLOSSARY_COL_NDX))
    );

    htConcepts.addContextMenuItem("Move this concept to a different term",
      row -> (nullSwitch(conceptRows.get(row), null, conceptRow -> conceptRow.concept) != null) && (curTerm.concepts.size() > 1),
      row -> moveConcept(conceptRows.get(row).concept));

    htConcepts.addContextMenuItem("Change order of concepts",
      row -> (row.getRecord(GLOSSARY_COL_NDX) != null) && (curTerm.concepts.size() > 1),

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
          innerRow.setCellValue(0, glossary);

          HDT_ConceptSense sense = concept.sense.get();
          if (sense == null)
            innerRow.setCellValue(1, "", hdtConceptSense);
          else
            innerRow.setCellValue(1, sense);
        });

        Runnable completeHndlr = () ->
        {
          db.<HDT_Concept, HDT_Term>getObjectList(rtConceptOfTerm, curTerm, true).reorder(ht.dataRowStream().map(tableRow -> curTerm.getConcept(tableRow.getRecord(0), tableRow.getRecord(1))).toList());
          ui.update();
        };

        ht.triggerChangeOrder(false, completeHndlr);
      });

    htConcepts.addContextMenuItem("Remove this row",
      row -> (conceptRows.get(row) != null) && ((curTerm.concepts.size() > 1) || conceptRows.get(row).isDuplicate),
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
      row.setCellValue(1, displayer, displayer.defaultChoiceText());
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

  private boolean removeRow(HyperTableRow selectedRow)
  {
    return removeRow(conceptRows.get(selectedRow), selectedRow);
  }

  private boolean removeRow(ConceptRow conceptRow)
  {
    return removeRow(conceptRow, null);
  }

  private boolean removeRow(ConceptRow conceptRow, HyperTableRow selectedRow)
  {
    if ((conceptRow.isDuplicate == false) || (selectedRow == null))
    {
      HDT_Concept concept = conceptRow.concept;

      if (concept != null)
      {
        if (ui.cantSaveRecord()) return false;

        if ((concept.getMainText().isEmpty() == false) || concept.hasHub())
        {
          String prompt = "Are you sure you want to remove the concept definition associated with the glossary \"" + conceptRow.glossary.name() + '"';

          if (confirmDialog(prompt + (conceptRow.sense == null ? "?" : (", sense \"" + conceptRow.sense.name() + "\"?")), false) == false)
            return false;
        }

        if (curTab().concept == concept)
          switchToDifferentTab();

        tpConcepts.getTabs().remove(getConceptTab(concept));
        conceptToTextViewInfo.remove(concept);
        db.deleteRecord(concept);
      }
    }

    clearAndRepopulateConceptsTable();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void handleGlossaryEdit(HyperTableRow editedRow)
  {
    // A lot of the complexity of this function is due to the fact that the user is
    // allowed to enter "duplicate" rows, which means a row where the glossary is the
    // same as an existing row with a blank sense. This is for convenience, so that
    // the user can enter data in the table one cell at a time.

    if (updatingConcepts) return;

    ConceptRow oldConceptRow = nullSwitch(conceptRows.get(editedRow), new ConceptRow()),
               newConceptRow = new ConceptRow(editedRow, false, false);

    if (newConceptRow.glossary == oldConceptRow.glossary) return;

    if ((oldConceptRow.isDuplicate == false) && (oldConceptRow.concept != null) && (curTerm.concepts.size() == 1) && (newConceptRow.glossary == null))
    {
      // Don't let user delete the last remaining concept.

      oldConceptRow.populateTableRow(editedRow);
      return;
    }

    HDT_Concept newConcept = newConceptRow.concept;

    boolean isDuplicate = false;

    if (oldConceptRow.concept == null)
    {
      if (newConceptRow.concept != null)
      {
        // As a result of changing the glossary for the row, the row's glossary and
        // sense now matches an existing concept, which means it must be a duplicate
        // of one of the other rows.

        isDuplicate = true;
      }
    }
    else  // oldConceptRow.concept != null
    {
      if (newConceptRow.glossary == null)
      {
        // The glossary was deleted (a concept can't exist without a glossary).

        if (removeRow(oldConceptRow, editedRow) == false)
          oldConceptRow.populateTableRow(editedRow);

        return;
      }

      if ((newConceptRow.concept != null) && (newConceptRow.concept != oldConceptRow.concept))
      {
        errorPopup("This term already has a concept in the same glossary with " + (oldConceptRow.sense == null ? "a blank" : "the same") + " sense.");
        oldConceptRow.populateTableRow(editedRow);

        return;
      }

      if (oldConceptRow.isDuplicate)
      {
        // The user created a duplicate row (same glossary as existing row with a blank sense), and
        // then changed the glossary of that row so that it was no longer a duplicate. Need to create
        // a separate concept record for it. Refresh the table because we don't care if there were
        // any other duplicates.

        addConceptInGlossary(newConceptRow.glossary, newConceptRow.sense);
        clearAndRepopulateConceptsTable();
        tpConcepts.getTabs().forEach(tab -> ((ConceptTab) tab).updateName());
        return;
      }

      boolean thereWasADup = false;

      for (HyperTableRow row : htConcepts.dataRows())
      {
        if (row != editedRow)
        {
          HDT_Concept otherConcept = curTerm.getConcept(row.getRecord(GLOSSARY_COL_NDX), row.getRecord(SENSE_COL_NDX));
          if (otherConcept == oldConceptRow.concept)
          {
            thereWasADup = true;
            break;
          }
        }
      }

      if (replaceGlossary(oldConceptRow.glossary, oldConceptRow.sense, newConceptRow.glossary) == false)
      {
        oldConceptRow.populateTableRow(editedRow);
        return;
      }

      newConcept = oldConceptRow.concept;

      if (thereWasADup)
      {
        // The row whose glossary was changed had a duplicate. Now that we changed
        // the glossary of the original concept, the rows are no longer duplicates,
        // so the former duplicate row now needs to have its own distinct concept
        // record. Refresh the table and exit because we only allow the user the
        // convenience of having one "active" duplicate at a time.

        addConceptInGlossary(oldConceptRow.glossary, oldConceptRow.sense);
        clearAndRepopulateConceptsTable();
        tpConcepts.getTabs().forEach(tab -> ((ConceptTab) tab).updateName());
        return;
      }
    }

    if ((newConceptRow.glossary != null) && (newConcept == null))
    {
      // Add new concept for row that was edited if needed.

      addConceptInGlossary(newConceptRow.glossary, newConceptRow.sense);
    }

    newConceptRow = new ConceptRow(editedRow, true, isDuplicate);
    conceptRows.put(editedRow, newConceptRow);
    newConceptRow.populateTableRow(editedRow);
    tpConcepts.getTabs().forEach(tab -> ((ConceptTab) tab).updateName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void handleSenseEdit(HyperTableRow editedRow, HyperTableCell newCell)
  {
    if (updatingConcepts) return;

    ConceptRow oldConceptRow = nullSwitch(conceptRows.get(editedRow), new ConceptRow());

    HDT_Concept concept = oldConceptRow.concept;
    HDT_Glossary glossary;
    HDT_ConceptSense sense = newCell.getRecord();
    String newText = HyperTableCell.getCellText(newCell).strip();

    boolean createNewConcept, editingOriginal;

    // The only way concept would be null here is if the user edited the Sense for a blank row.
    if (concept == null)
    {
      if (newText.isBlank())
      {
        // The user must have edited the Sense for a blank row and changed it back to blank so do nothing.

        return;
      }

      for (HyperTableRow row : htConcepts.dataRows())
      {
        // We need to create a new concept and default in a glossary for it. If there is already a concept
        // in the General glossary with the same sense, treat that as a conflict.

        if (row != editedRow)
        {
          HDT_Concept otherConcept = curTerm.getConcept(row.getRecord(GLOSSARY_COL_NDX), row.getRecord(SENSE_COL_NDX));
          if ((otherConcept.glossary.getID() == 1) && row.getText(SENSE_COL_NDX).equalsIgnoreCase(newText))
          {
            errorPopup("This term already has a concept in the General glossary with the same sense.");
            removeRow(oldConceptRow);
            return;
          }
        }
      }

      // Glossary is currently blank. Set it to General if the current term is already in
      // the General glossary; otherwise set it to whichever glossary the first concept is in and
      // see if there is a conflict with that glossary and the user-chosen sense. If not,
      // use that glossary. If so, use General (since the earlier loop made sure there wouldn't be a conflict).

      glossary = null;

      if (curTerm.concepts.stream().noneMatch(koncept -> koncept.glossary.get().getID() == 1))
      {
        glossary = curTerm.concepts.getFirst().glossary.get();

        for (HDT_Concept otherConcept : curTerm.concepts)
        {
          if ((otherConcept.glossary.get() == glossary) && otherConcept.sense.isNotNull() &&
              ((otherConcept.sense.get() == sense) || newText.equalsIgnoreCase(otherConcept.sense.get().name())))
          {
            glossary = null;
            break;
          }
        }
      }

      if (glossary == null) glossary = db.glossaries.getByID(1);

      createNewConcept = true;
      editingOriginal = false;
    }
    else  // concept != null; user is editing a row with an existing concept record
    {
      if ((oldConceptRow.sense != null) && ((sense == oldConceptRow.sense) || newText.equalsIgnoreCase(oldConceptRow.sense.name())))
      {
        // The user typed text that matches the sense that was already assigned;
        // just update the row with the existing data

        oldConceptRow.populateTableRow(editedRow);
        return;
      }

      createNewConcept = false;
      editingOriginal = false;  // True if there were duplicates but the original is being edited, not the new one

      for (HyperTableRow row : htConcepts.dataRows())
      {
        if (row == editedRow)
        {
          // The only way there can be duplicates is if 2 rows have the same glossary and
          // a blank sense. If createNewConcept is false, we haven't encountered the duplicate
          // yet, so this must be the original.

          if (createNewConcept == false)
            editingOriginal = true;
        }
        else
        {
          HDT_Concept otherConcept = curTerm.getConcept(row.getRecord(GLOSSARY_COL_NDX), row.getRecord(SENSE_COL_NDX));
          if ((concept != otherConcept) && (concept.glossary.get() == otherConcept.glossary.get()) && row.getText(SENSE_COL_NDX).equalsIgnoreCase(newText))
          {
            HDT_ConceptSense otherSense = row.getRecord(SENSE_COL_NDX);

            errorPopup("This term already has a concept in the same glossary with " + (otherSense == null ? "a blank" : "the same") + " sense.");
            oldConceptRow.populateTableRow(editedRow);
            return;
          }

          if ((concept == otherConcept) && (row.getText(SENSE_COL_NDX).equalsIgnoreCase(newText) == false))
          {
            // Need to create new concept because there were previously duplicate rows.

            createNewConcept = true;
          }
        }
      }

      glossary = concept.glossary.get();
    }

    if ((sense == null) && strNotNullOrEmpty(newText))
    {
      sense = db.createNewBlankRecord(hdtConceptSense);
      sense.setName(newText);
    }

    if (createNewConcept)
    {
      if (editingOriginal)
      {
        // The only way there could have been a duplicate is if they both have a blank sense.

        // If the original is being edited, not the duplicate, add the new sense to the original
        // and create the duplicate with a blank sense.

        concept.sense.set(sense);
        addConceptInGlossary(glossary, null);
      }
      else
      {
        // If a duplicate row was being edited, add the sense to the new one.
        // Or the user may be adding a sense to a blank row.

        addConceptInGlossary(glossary, sense);
      }
    }
    else
      concept.sense.set(sense);

    clearAndRepopulateConceptsTable();
    tpConcepts.getTabs().forEach(tab -> ((ConceptTab) tab).updateName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void handleSearchKeyEdit(HyperTableRow editedRow, HyperTableCell newCell)
  {
    if (updatingConcepts) return;

    ConceptRow oldConceptRow = conceptRows.get(editedRow);
    HDT_Concept concept = oldConceptRow.concept;
    String newText = HyperTableCell.getCellText(newCell).strip();

    try
    {
      concept.setSearchKey(newText);
    }
    catch (SearchKeyException e)
    {
      if (ui.isShuttingDown() == false)
      {
        errorPopup(e instanceof SearchKeyTooShortException ?
          "Search key must be at least 3 characters: " + e.getKey()
        :
          "Search key already exists: " + e.getKey());
      }

      oldConceptRow.populateTableRow(editedRow);
      return;
    }

    clearAndRepopulateConceptsTable();
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
    moveConcept(curConcept);
  }

  private void moveConcept(HDT_Concept concept)
  {
    if (curTerm.concepts.size() < 2)
    {
      if (confirmDialog("There is only one definition for this Term. Do you want to choose another Term to merge with this one?", false))
        merge();

      return;
    }

    if (ui.cantSaveRecord()) return;

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

  public void addGlossaryToNextBlankRow(HDT_Glossary glossary)
  {
    HyperTableRow row = htConcepts.getRow(htConcepts.dataRowCount());
    row.setCellValue(GLOSSARY_COL_NDX, row.getPopulator(GLOSSARY_COL_NDX).getChoiceByID(row, glossary.getID()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<HDT_Concept> getParentConcepts(HyperTableRow row)
  {
    return nullSwitch(conceptRows.get(row), List.of(), conceptRow ->
           nullSwitch(conceptRow.concept  , List.of(), concept    -> List.copyOf(concept.parentConcepts)));
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

  public void runTests()
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    if (db.terms     .isEmpty() == false) { errorPopup("The tests can only be run if there are no Term records."); return; }
    if (db.concepts  .isEmpty() == false) { errorPopup("There should not be any Concept records since there are no Term records."); return; }
    if (db.glossaries.isEmpty())          { errorPopup("There should be at least one Glossary."); return; }

    while (db.glossaries.size() > 1)
      db.deleteRecord(findFirst(db.glossaries, glossary -> glossary.getID() != 1));

    HDT_Glossary generalGlossary = db.glossaries.getByID(1);

    if (generalGlossary == null) { errorPopup("General glossary does not exist."); return; }

    if ("General".equals(generalGlossary.name()) == false) { errorPopup("Name of General glossary should be \"General\"."); return; }

    if (ui.cantSaveRecord()) return;

    HDT_Glossary physicsGlossary = db.createNewBlankRecord(hdtGlossary);
    physicsGlossary.setActive(true);
    physicsGlossary.setName("Physics");
    physicsGlossary.parentGlossaries.add(generalGlossary);

    HDT_Term term1 = HDT_Term.create(generalGlossary),
             term2 = HDT_Term.create(generalGlossary);

    term1.setName("Term 1");
    term2.setName("Term 2");

    try
    {
      term1.setSearchKey("Term 1");
      term2.setSearchKey("Term 2");
    }
    catch (SearchKeyException e)
    {
      e.printStackTrace();
      return;
    }

    ui.goToRecord(term1, false);

    PopupRobot.setActive(true);

    FXTestSequencer queue = new FXTestSequencer();

    queue.setFinalizer(() -> runDelayedInFXThread(1, 1000, () -> PopupRobot.setActive(false)));

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    /* ************************************************************************ */
    /*                                                                          */
    /*   Tests start here.                                                      */
    /*                                                                          */
    /* ************************************************************************ */

    queue
      .setDelayMS(400)

      .thenRun(() -> htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      .thenRunAfterDelay(() -> htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      /* ************************************************************************ */
      /*                                                                          */
      /*   Now 2 additional rows have been added with General as the glossary.    */
      /*   Sense is blank on all 3.                                               */
      /*                                                                          */
      /* ************************************************************************ */

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      /* ************************************************************************ */
      /*                                                                          */
      /*   Try editing the parent concepts on the first duplicate row and         */
      /*   get an error popup.                                                    */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() -> htConcepts.edit(htConcepts.getRow(1), PARENTS_COL_NDX))

      .thenExpectPopupAfterDelay("Enter a Sense first.", AlertType.ERROR)

      .thenRun(htConcepts::cancelEditing)

      //---------------------------------------------------------------------------
      //---------------------------------------------------------------------------

      /* ************************************************************************ */
      /*                                                                          */
      /*   Try editing the search key on the second duplicate row and             */
      /*   get an error popup.                                                    */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() -> htConcepts.edit(htConcepts.getRow(2), SEARCHKEY_COL_NDX))

      .thenExpectPopupAfterDelay("Enter a Sense first.", AlertType.ERROR)

      .thenRun(htConcepts::cancelEditing)

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      .thenRunAfterDelay(() ->
      {
        assertEquals(3, htConcepts.dataRowCount(), "Data row count not 3.");

        ui.btnSaveClick();
      })

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      /* ************************************************************************ */
      /*                                                                          */
      /*   Click "Accept Edits". That should remove the 2 duplicate rows.         */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        assertEquals(1, htConcepts.dataRowCount(), "Data row count not 1 after clicking \"Accept Edits\".");

        htConcepts.getRow(0).setCellValue(GLOSSARY_COL_NDX, GenericNonRecordHTC.blankCell);
      })

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      /* ************************************************************************ */
      /*                                                                          */
      /*   Now just the original row remains. Try clearing the glossary. It       */
      /*   should restore the value; there has to be at least one                 */
      /*   concept/glossary.                                                      */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        assertSame(generalGlossary, htConcepts.getRow(0).getRecord(GLOSSARY_COL_NDX), "If you clear out the glossary on the last remaining row, it should restore the value.");

        assertDoesNotThrow(() -> term1.concepts.getFirst().addParentConcept(term2.concepts.getFirst()));

        ui.btnSaveClick();
      })

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      /* ************************************************************************ */
      /*                                                                          */
      /*   Set glossary to General on second row after adding parent concept for  */
      /*   first row.                                                             */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() -> htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      /* ************************************************************************ */
      /*                                                                          */
      /*   Set sense on first row.                                                */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() -> htConcepts.getRow(0).setCellValue(SENSE_COL_NDX, "New sense", hdtConceptSense))

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      /* ************************************************************************ */
      /*                                                                          */
      /*   Verify that there are now 2 concepts; one with the new sense and one   */
      /*   without.                                                               */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        assertEquals ("New sense"    , htConcepts.getRow(0).getText      (SENSE_COL_NDX), "Sense should be populated in first row.");
        assertEquals (hdtConceptSense, htConcepts.getRow(0).getRecordType(SENSE_COL_NDX), "Sense should be populated in first row.");
        assertNotEquals(-1           , htConcepts.getRow(0).getID        (SENSE_COL_NDX), "Sense should be populated in first row.");

        assertEquals (""             , htConcepts.getRow(1).getText      (SENSE_COL_NDX), "Sense should not be populated in second row.");
        assertEquals (hdtNone        , htConcepts.getRow(1).getRecordType(SENSE_COL_NDX), "Sense should not be populated in second row.");
        assertEquals (-1             , htConcepts.getRow(1).getID        (SENSE_COL_NDX), "Sense should not be populated in second row.");

        assertEquals(2, term1.concepts.size(), "Term 1 should now have 2 concepts.");
        assertEquals(2, tpConcepts.getTabs().size(), "Term 1 should now have 2 concepts.");
        assertEquals("New sense", tpConcepts.getTabs().get(0).getText(), "First concept tab should say \"New sense\"");
        assertEquals("General", tpConcepts.getTabs().get(1).getText(), "Second concept tab should say \"General\"");

        PopupRobot.setDefaultResponse(DialogResult.mrNo);

        htConcepts.getRow(0).setCellValue(GLOSSARY_COL_NDX, physicsGlossary, physicsGlossary.name());
      })

      /* ************************************************************************ */
      /*                                                                          */
      /*   Try changing glossary on row that has one or more parent concepts. Get */
      /*   confirmation popup.                                                    */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectPopupAfterDelay("This will unassign any parent or child concepts for Term \"Term 1 (New sense)\", Glossary \"General\". Proceed?", AlertType.CONFIRMATION)

      .thenRun(() ->
      {
        assertEquals(2, term1.concepts.size(), "Term 1 should now have 2 concepts.");

        term1.concepts.getFirst().removeParent(term2.concepts.getFirst());
        assertDoesNotThrow(() -> term2.concepts.getFirst().addParentConcept(term1.concepts.getFirst()));

        ui.btnSaveClick();
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(0).setCellValue(GLOSSARY_COL_NDX, physicsGlossary))

      /* ************************************************************************ */
      /*                                                                          */
      /*   Try changing glossary on row where the concept has one or more         */
      /*   sub-concepts. Get confirmation popup.                                  */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectPopupAfterDelay("This will unassign any parent or child concepts for Term \"Term 1 (New sense)\", Glossary \"General\". Proceed?", AlertType.CONFIRMATION)

      .thenRun(() ->
      {
        assertEquals(2, term1.concepts.size(), "Term 1 should now have 2 concepts.");

        term2.concepts.getFirst().removeParent(term1.concepts.getFirst());

        term1.concepts.getFirst().getMainText().setHtml("Here is my text.");

        ui.update();
      })

      /* ************************************************************************ */
      /*                                                                          */
      /*   Try clearing glossary on row where the concept has a non-blank main    */
      /*   text. Get confirmation popup (and click yes).                          */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        PopupRobot.setDefaultResponse(DialogResult.mrYes);

        htConcepts.getRow(0).setCellValue(GLOSSARY_COL_NDX, GenericNonRecordHTC.blankCell);
      })

      .thenExpectPopupAfterDelay("Are you sure you want to remove the concept definition associated with the glossary \"General\", sense \"New sense\"?", AlertType.CONFIRMATION)

      .thenRun(() ->
      {
        assertEquals(1, term1.concepts.size(), "Term 1 should now have 1 concept.");
        assertEquals(1, htConcepts.dataRowCount(), "Term 1 should now have 1 concept.");

        assertEquals (""     , htConcepts.getRow(0).getText      (SENSE_COL_NDX), "Sense should not be populated in first row.");
        assertEquals (hdtNone, htConcepts.getRow(0).getRecordType(SENSE_COL_NDX), "Sense should not be populated in first row.");
        assertEquals  (-1     , htConcepts.getRow(0).getID        (SENSE_COL_NDX), "Sense should not be populated in first row.");

        htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, physicsGlossary);
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, physicsGlossary))

      .thenRunAfterDelay(() -> htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, GenericNonRecordHTC.blankCell))

      /* ************************************************************************ */
      /*                                                                          */
      /*   If you clear out the glossary for a duplicate row, the original row    */
      /*   should remain and the original concept should still be there.          */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, term1.concepts.size(), "Term 1 should now have 2 concepts.");
        assertEquals(2, htConcepts.dataRowCount(), "There should now be 2 concept rows.");

        htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, physicsGlossary);
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, GenericNonRecordHTC.blankCell))

      /* ************************************************************************ */
      /*                                                                          */
      /*   There is a concept in General and a concept in Physics. A duplicate    */
      /*   row of the Physics row is added. If the glossary is cleared out on the */
      /*   original Physics row, that Physics concept and its duplicate row       */
      /*   should be removed, leaving only the remaining General concept row.     */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        assertEquals(1, term1.concepts.size(), "Term 1 should now have 1 concept.");
        assertSame(generalGlossary, term1.concepts.getFirst().glossary.get(), "The concept in the General glossary should remain.");
        assertEquals(1, htConcepts.dataRowCount(), "There should now be 1 concept row.");
        assertEquals(generalGlossary.name(), htConcepts.getRow(0).getText(GLOSSARY_COL_NDX), "The concept in the General glossary should remain.");

        htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, physicsGlossary);
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      /* ************************************************************************ */
      /*                                                                          */
      /*   If the glossary was previously populated, but is changed such that the */
      /*   row is now a match for an existing row (blank sense), it should show   */
      /*   an error message and revert the row.                                   */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectPopupAfterDelay("This term already has a concept in the same glossary with a blank sense.", AlertType.ERROR)

      .thenRun(() ->
      {
        assertEquals(physicsGlossary.name(), htConcepts.getRow(1).getText(GLOSSARY_COL_NDX), "Second row should still have \"Physics\" as the glossary.");
        assertEquals(2, term1.concepts.size(), "Term 1 should now have 2 concepts.");
        assertSame(generalGlossary, term1.concepts.get(0).glossary.get(), "First concept should be General");
        assertSame(physicsGlossary, term1.concepts.get(1).glossary.get(), "Second concept should be Physics");

        assertTrue(db.conceptSenses.isEmpty(), "No sense records should exist.");

        htConcepts.getRow(0).setCellValue(SENSE_COL_NDX, "Sense 1", hdtConceptSense);
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, term1.concepts.getFirst().sense.get()))

      .thenExpectLackOfPopupAfterDelay()

      .thenRun(() -> htConcepts.getRow(0).setCellValue(GLOSSARY_COL_NDX, physicsGlossary))

      /* ************************************************************************ */
      /*                                                                          */
      /*   If the glossary was previously populated, but is changed such that the */
      /*   row is now a match for an existing row (non-blank sense), it should    */
      /*   show an error message and revert the row.                              */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectPopupAfterDelay("This term already has a concept in the same glossary with the same sense.", AlertType.ERROR)

      .thenRun(() ->
      {
        assertEquals(2, term1.concepts.size(), "Term 1 should now have 2 concepts");
        assertSame(generalGlossary, term1.concepts.get(0).glossary.get(), "First concept should be General");
        assertSame(physicsGlossary, term1.concepts.get(1).glossary.get(), "Second concept should be Physics");

        db.deleteRecord(term1.concepts.get(1));
        term1.concepts.getFirst().sense.set(null);

        ui.update();
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      .thenRunAfterDelay(() -> htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      .thenRunAfterDelay(() ->
      {
        assertEquals(1, term1.concepts.size());

        htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, physicsGlossary);
      })

      /* ************************************************************************** */
      /*                                                                            */
      /*   If the row was previously a duplicate, and the glossary was changed so   */
      /*   that the row is no longer a duplicate, a new concept should be created   */
      /*   and other duplicate rows should be removed.                              */
      /*                                                                            */
      /* ************************************************************************** */

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, htConcepts.dataRowCount());
        assertEquals(2, term1.concepts.size());

        db.deleteRecord(term1.concepts.get(1));

        ui.update();
      })

      .thenRunAfterDelay(() ->
      {
        assertEquals(1, term1.concepts.size());

        htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, generalGlossary);
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      .thenRunAfterDelay(() -> htConcepts.getRow(0).setCellValue(GLOSSARY_COL_NDX, physicsGlossary))

      /* **************************************************************************** */
      /*                                                                              */
      /*   If the row being edited (changed from one glossary to another) previously  */
      /*   had a duplicate, verify that the duplicate row now has its own distinct    */
      /*   concept record. If there were additional duplicates, those rows should be  */
      /*   removed.                                                                   */
      /*                                                                              */
      /* **************************************************************************** */

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, htConcepts.dataRowCount(), "There should be 2 concept rows");
        assertEquals(2, term1.concepts.size(), "There should be 2 concepts for Term 1");

        PopupRobot.setDefaultResponse(DialogResult.mrNo);
        assertTrue(removeRow(htConcepts.getRow(0)), "Removing the first row should succeed");
      })

      .thenExpectLackOfPopupAfterDelay()

      .thenRun(() ->
      {
        assertEquals(0, db.conceptSenses.size(), "No sense records should exist");
        assertEquals(2, db.terms.size(), "2 terms should exist");
        assertEquals(2, db.concepts.size(), "2 total concepts should exist in the database");
        assertEquals(1, htConcepts.dataRowCount(), "There should be 1 concept row");
        assertEquals(1, term1.concepts.size(), "There should be 1 concept for Term 1");

        htConcepts.getRow(0).setCellValue(SENSE_COL_NDX, "New sense", hdtConceptSense);
      })

      /* ************************************************************************ */
      /*                                                                          */
      /*                    SENSE UPDATE TESTING STARTS HERE.                     */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() -> htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, "nEw SeNsE", hdtConceptSense))

      /* ************************************************************************ */
      /*                                                                          */
      /*   User edits the sense for a blank row. If there is already a concept    */
      /*   in the General glossary with the same (ignoring case) sense, show      */
      /*   error popup and back out the change.                                   */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectPopupAfterDelay("This term already has a concept in the General glossary with the same sense.", AlertType.ERROR)

      .thenRun(() ->
      {
        assertEquals(1, htConcepts.dataRowCount(), "There should be 1 concept row");

        term1.concepts.getFirst().sense.set(null);

        htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, "Ordinary", hdtConceptSense);
      })

      /* ************************************************************************ */
      /*                                                                          */
      /*   User enters a non-blank sense (for which there isn't a record yet)     */
      /*   in a blank row. If there is already a concept in the General glossary  */
      /*   with a blank sense, use General for this row and create a concept.     */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, htConcepts.dataRowCount(), "There should be 2 concept rows");
        assertEquals(2, db.terms.size(), "2 terms should exist");
        assertEquals(3, db.concepts.size(), "3 total concepts should exist");
        assertEquals(1, db.conceptSenses.size(), "One sense record should exist");
        assertEquals("Ordinary", term1.concepts.get(1).sense.get().name(), "The Ordinary sense should be assigned to the second concept");
        assertSame(generalGlossary, term1.concepts.get(1).glossary.get(), "The General glossary should be assigned to the second concept");

        htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, physicsGlossary);
      })

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, htConcepts.dataRowCount(), "There should be 2 concept rows");
        assertEquals(2, db.terms.size(), "2 terms should exist");
        assertEquals(3, db.concepts.size(), "3 total concepts should exist");
        assertEquals(1, db.conceptSenses.size(), "One sense record should exist");
        assertEquals("Ordinary", term1.concepts.get(1).sense.get().name(), "The Ordinary sense should be assigned to the second concept");
        assertSame(physicsGlossary, term1.concepts.get(1).glossary.get(), "The Physics glossary should be assigned to the second concept");

        htConcepts.getRow(2).setCellValue(SENSE_COL_NDX, term1.concepts.get(1).sense.get());
      })

      /* ************************************************************************** */
      /*                                                                            */
      /*   User enters a non-blank sense (for which there already exists a record)  */
      /*   in a blank row. If there is already a concept in the General glossary    */
      /*   with a blank sense, use General for this row and create a concept.       */
      /*                                                                            */
      /* ************************************************************************** */

      .thenRunAfterDelay(() ->
      {
        assertEquals(3, htConcepts.dataRowCount(), "There should be 3 concept rows");
        assertEquals(4, db.concepts.size(), "4 total concepts should exist");
        assertEquals(1, db.conceptSenses.size(), "One sense record should exist");
        assertEquals("Ordinary", term1.concepts.get(2).sense.get().name(), "The Ordinary sense should be assigned to the third concept");
        assertSame(physicsGlossary, term1.concepts.get(1).glossary.get(), "The Physics glossary should be assigned to the second concept");
        assertSame(generalGlossary, term1.concepts.get(2).glossary.get(), "The General glossary should be assigned to the third concept");

        assertTrue(removeRow(htConcepts.getRow(0)), "Removing the first row should succeed");
      })

      .thenRunAfterDelay(() -> assertTrue(removeRow(htConcepts.getRow(0)), "Removing the first row should succeed"))

      .thenRunAfterDelay(() ->
      {
        assertEquals(1, htConcepts.dataRowCount(), "There should be 1 concept row");
        assertEquals(1, term1.concepts.size(), "There should be 1 concept for Term 1");
        assertEquals(1, db.conceptSenses.size(), "There should only be 1 sense record in the database");
        assertEquals("Ordinary", term1.concepts.getFirst().sense.get().name(), "The Ordinary sense should be assigned to the only remaining concept");

        htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, "Enlightened", hdtConceptSense);
      })

      /* ************************************************************************ */
      /*                                                                          */
      /*   User enters a non-blank sense (for which there isn't a record yet)     */
      /*   in a blank row. If there is already a concept in the General glossary  */
      /*   with a different (non-blank) sense, use General for this row and       */
      /*   create a concept.                                                      */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, htConcepts.dataRowCount(), "There should be 2 concept rows");
        assertEquals(2, term1.concepts.size(), "Term 1 should have 2 concepts");
        assertEquals(2, db.conceptSenses.size(), "There should be 2 sense records in the database");
        assertEquals("Enlightened", term1.concepts.get(1).sense.get().name(), "The Enlightened sense should be assigned to the second concept");
        assertSame(generalGlossary, term1.concepts.get(1).glossary.get(), "The General glossary should be assigned to the second concept");

        term2.concepts.getFirst().sense.set(term1.concepts.get(1).sense.get());  // Set non-active term's concept to be Enlightened so that sense doesn't get deleted in the next operation

        assertTrue(removeRow(htConcepts.getRow(1)), "Removing the second row should succeed");
      })

      .thenExpectLackOfPopupAfterDelay()

      .thenRun(() ->
      {
        assertEquals(1, htConcepts.dataRowCount(), "There should be 1 concept row");
        assertEquals(1, term1.concepts.size(), "There should be 1 concept for Term 1");
        assertEquals(2, db.conceptSenses.size(), "There should only be 2 sense records in the database");
        assertEquals("Ordinary", term1.concepts.getFirst().sense.get().name(), "The Ordinary sense should be assigned to the only remaining concept");

        htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, term2.concepts.getFirst().sense.get());
      })

      /* ************************************************************************** */
      /*                                                                            */
      /*   User enters a non-blank sense (for which there already exists a record)  */
      /*   in a blank row. If there is already a concept in the General glossary    */
      /*   with a different (non-blank) sense, use General for this row and create  */
      /*   a concept.                                                               */
      /*                                                                            */
      /* ************************************************************************** */

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, htConcepts.dataRowCount(), "There should be 2 concept rows");
        assertEquals(2, term1.concepts.size(), "Term 1 should have 2 concepts");
        assertEquals(2, db.conceptSenses.size(), "There should be 2 sense records in the database");
        assertEquals("Enlightened", term1.concepts.get(1).sense.get().name(), "The Enlightened sense should be assigned to the second concept");
        assertSame(generalGlossary, term1.concepts.get(1).glossary.get(), "The General glossary should be assigned to the second concept");

        assertTrue(removeRow(htConcepts.getRow(1)), "Removing the second row should succeed");
      })

      .thenRunAfterDelay(() ->
      {
        assertEquals(1, htConcepts.dataRowCount(), "There should be 1 concept row");
        assertEquals(1, term1.concepts.size(), "There should be 1 concept for Term 1");
        assertEquals(2, db.conceptSenses.size(), "There should be 2 sense records in the database");
        assertEquals("Ordinary", term1.concepts.getFirst().sense.get().name(), "The Ordinary sense should be assigned to the only remaining concept");

        htConcepts.getRow(0).setCellValue(GLOSSARY_COL_NDX, physicsGlossary);
      })

      .thenRunAfterDelay(() ->
      {
        assertSame(physicsGlossary, term1.concepts.getFirst().glossary.get(), "The last remaining concept should be in the Physics glossary");

        htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, term2.concepts.getFirst().sense.get());
      })

      /* ************************************************************************** */
      /*                                                                            */
      /*   User enters a non-blank sense (for which there already exists a record)  */
      /*   in a blank row. If the term is not in the General glossary, and no       */
      /*   existing concept (first or otherwise) has the same glossary/sense        */
      /*   combination, use the same glossary as the first concept and create a     */
      /*   concept.                                                                 */
      /*                                                                            */
      /* ************************************************************************** */

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, htConcepts.dataRowCount(), "There should be 2 concept rows");
        assertEquals(2, term1.concepts.size(), "2 concepts should exist for Term 1");
        assertEquals(2, db.conceptSenses.size(), "There should be 2 sense records in the database");
        assertEquals("Enlightened", term1.concepts.get(1).sense.get().name(), "The Enlightened sense should be assigned to the second concept");
        assertSame(physicsGlossary, term1.concepts.get(1).glossary.get(), "The Physics glossary should be assigned to the second concept");

        assertTrue(removeRow(htConcepts.getRow(1)), "Removing the second row should succeed");
      })

      .thenRunAfterDelay(() ->
      {
        db.deleteRecord(term2.concepts.getFirst().sense.get());

        assertEquals(1, htConcepts.dataRowCount(), "There should be 1 concept row");
        assertEquals(1, term1.concepts.size(), "There should be 1 concept for Term 1");
        assertEquals(1, db.conceptSenses.size(), "There should only be 1 sense record in the database");
        assertEquals("Ordinary", term1.concepts.getFirst().sense.get().name(), "The Ordinary sense should be assigned to the only remaining concept");
        assertSame(physicsGlossary, term1.concepts.getFirst().glossary.get(), "The last remaining concept should be in the Physics glossary");

        htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, "Enlightened", hdtConceptSense);
      })

      /* ************************************************************************ */
      /*                                                                          */
      /*   User enters a non-blank sense (for which there isn't a record yet)     */
      /*   in a blank row. If the term is not in the General glossary, and no     */
      /*   existing concept (first or otherwise) has the same glossary/sense      */
      /*   combination, use the same glossary as the first concept and create a   */
      /*   concept.                                                               */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRunAfterDelay(() ->
      {
        assertEquals(2, htConcepts.dataRowCount(), "There should be 2 concept rows");
        assertEquals(2, term1.concepts.size(), "2 concepts should exist for Term 1");
        assertEquals(2, db.conceptSenses.size(), "There should be 2 sense records in the database");
        assertEquals("Enlightened", term1.concepts.get(1).sense.get().name(), "The Enlightened sense should be assigned to the second concept");
        assertSame(physicsGlossary, term1.concepts.get(1).glossary.get(), "The Physics glossary should be assigned to the second concept");

        htConcepts.getRow(2).setCellValue(SENSE_COL_NDX, term1.concepts.get(1).sense.get());
      })

      /* ************************************************************************ */
      /*                                                                          */
      /*   User edits the sense for a blank row. If the term is not in the        */
      /*   General glossary, and there is an existing concept (first or           */
      /*   otherwise) that has the same glossary/sense combination, use the       */
      /*   General glossary and create a concept.                                 */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectLackOfPopupAfterDelay()

      .thenRun(() ->
      {
        assertEquals(3, htConcepts.dataRowCount(), "There should be 3 concept rows");
        assertEquals(3, term1.concepts.size(), "3 concepts should exist for Term 1");
        assertEquals(2, db.conceptSenses.size(), "There should be 2 sense records in the database");
        assertEquals("Enlightened", term1.concepts.get(2).sense.get().name(), "The Enlightened sense should be assigned to the third concept");
        assertSame(term1.concepts.get(1).sense.get(), term1.concepts.get(2).sense.get(), "The second and third concepts should have the same sense record");
        assertSame(generalGlossary, term1.concepts.get(2).glossary.get(), "The General glossary should be assigned to the third concept");

        htConcepts.getRow(0).setCellValue(SENSE_COL_NDX, GenericNonRecordHTC.blankCell);
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, GenericNonRecordHTC.blankCell))

      /* ************************************************************************ */
      /*                                                                          */
      /*   User edits the sense for an existing concept row, clearing out the     */
      /*   existing text so that this row will match another row's glossary and   */
      /*   blank sense. There should be an error popup and the row should be      */
      /*   reverted.                                                              */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectPopupAfterDelay("This term already has a concept in the same glossary with a blank sense.", AlertType.ERROR)

      .thenRun(() ->
      {
        assertNotNull(htConcepts.getRow(1).getRecord(SENSE_COL_NDX), "The Enlightened sense should have been reverted in the second row");
        assertEquals("Enlightened", term1.concepts.get(1).sense.get().name(), "The Enlightened sense should be assigned to the second concept");

        htConcepts.getRow(0).setCellValue(SENSE_COL_NDX, term1.concepts.get(1).sense.get());
      })

      /* ************************************************************************ */
      /*                                                                          */
      /*   User edits the sense for an existing concept row, so that this row     */
      /*   will match another row's glossary and sense. There should be an        */
      /*   error popup and the row should be reverted.                            */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectPopupAfterDelay("This term already has a concept in the same glossary with the same sense.", AlertType.ERROR)

      .thenRun(() ->
      {
        assertEquals("", htConcepts.getRow(0).getText(SENSE_COL_NDX), "Sense should not be populated in first row.");
        assertEquals(-1, htConcepts.getRow(0).getID  (SENSE_COL_NDX), "Sense should not be populated in first row.");

        htConcepts.getRow(3).setCellValue(GLOSSARY_COL_NDX, physicsGlossary);
      })

      .thenExpectLackOfPopupAfterDelay()

      .thenRun(() -> htConcepts.getRow(3).setCellValue(SENSE_COL_NDX, term1.concepts.get(1).sense.get()))

      /* ************************************************************************ */
      /*                                                                          */
      /*   User edits the sense for a duplicate row, so that this row will        */
      /*   match another row's glossary and sense. There should be an error       */
      /*   popup and the row should be reverted.                                  */
      /*                                                                          */
      /* ************************************************************************ */

      .thenExpectPopupAfterDelay("This term already has a concept in the same glossary with the same sense.", AlertType.ERROR)

      .thenRun(() ->
      {
        assertEquals("", htConcepts.getRow(3).getText(SENSE_COL_NDX), "Sense should not be populated in fourth row.");
        assertEquals(-1, htConcepts.getRow(3).getID  (SENSE_COL_NDX), "Sense should not be populated in fourth row.");

        db.deleteRecord(term1.concepts.get(1));
        db.deleteRecord(term1.concepts.get(1));

        ui.update();
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(1).setCellValue(GLOSSARY_COL_NDX, physicsGlossary))

      .thenRunAfterDelay(() ->
      {
        assertEquals(0, db.conceptSenses.size(), "There should not be any sense records");

        htConcepts.getRow(1).setCellValue(SENSE_COL_NDX, "Strict", hdtConceptSense);
      })

      .thenExpectLackOfPopupAfterDelay()

      /* ************************************************************************ */
      /*                                                                          */
      /*   User enters a non-blank sense (where a new sense will need to be       */
      /*   created) for a duplicate row. A new concept should be created for      */
      /*   this row.                                                              */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRun(() ->
      {
        assertEquals(2, htConcepts.dataRowCount());
        assertEquals(2, term1.concepts.size());
        assertEquals(1, db.conceptSenses.size());
        assertEquals("Strict", htConcepts.getRow(1).getText(SENSE_COL_NDX), "Sense should be populated in second row.");
        assertNotEquals(-1, htConcepts.getRow(1).getID(SENSE_COL_NDX), "Sense should be populated in second row.");
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      .thenRunAfterDelay(() -> htConcepts.getRow(3).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      .thenRunAfterDelay(() ->
      {
        assertEquals(3, term1.concepts.size());
        assertFalse(conceptRows.get(htConcepts.getRow(2)).isDuplicate);
        assertTrue (conceptRows.get(htConcepts.getRow(3)).isDuplicate);

        htConcepts.getRow(2).setCellValue(SENSE_COL_NDX, "Relaxed", hdtConceptSense);
      })

      .thenExpectLackOfPopupAfterDelay()

      /* ************************************************************************ */
      /*                                                                          */
      /*   User enters a non-blank sense (where a new sense will need to be       */
      /*   created) for a row that has a duplicate. The new sense should be       */
      /*   assigned to the original concept, and a new concept should be created  */
      /*   for the duplicate row.                                                 */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRun(() ->
      {
        assertEquals(4, term1.concepts.size());
        assertEquals(2, db.conceptSenses.size());
        assertEquals("Relaxed", term1.concepts.get(2).sense.get().name());
        assertEquals("Relaxed", htConcepts.getRow(2).getText(SENSE_COL_NDX), "Sense should be populated in third row.");
        assertNotEquals(-1, htConcepts.getRow(2).getID(SENSE_COL_NDX), "Sense should be populated in third row.");
        assertEquals("", htConcepts.getRow(3).getText(SENSE_COL_NDX), "Sense should not be populated in fourth row.");
        assertEquals(-1, htConcepts.getRow(3).getID  (SENSE_COL_NDX), "Sense should not be populated in fourth row.");

        db.deleteRecord(term1.concepts.get(2));
        db.deleteRecord(term1.concepts.get(2));

        ui.update();
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(2).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      .thenRunAfterDelay(() -> htConcepts.getRow(3).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      .thenRunAfterDelay(() ->
      {
        assertEquals(3, term1.concepts.size());
        assertFalse(conceptRows.get(htConcepts.getRow(2)).isDuplicate);
        assertTrue (conceptRows.get(htConcepts.getRow(3)).isDuplicate);

        htConcepts.getRow(3).setCellValue(SENSE_COL_NDX, term1.concepts.get(1).sense.get());
      })

      .thenExpectLackOfPopupAfterDelay()

      /* ************************************************************************ */
      /*                                                                          */
      /*   User enters a non-blank sense (where there is an existing sense        */
      /*   record) for a duplicate row. A new concept should be created for       */
      /*   this row.                                                              */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRun(() ->
      {
        assertEquals(4, htConcepts.dataRowCount());
        assertEquals(4, term1.concepts.size());
        assertEquals(1, db.conceptSenses.size());
        assertEquals("Strict", htConcepts.getRow(3).getText(SENSE_COL_NDX), "Sense should be populated in fourth row.");
        assertNotEquals(-1, htConcepts.getRow(3).getID(SENSE_COL_NDX), "Sense should be populated in fourth row.");
        assertEquals(htConcepts.getRow(1).getID(SENSE_COL_NDX), htConcepts.getRow(3).getID(SENSE_COL_NDX), "Sense in second and fourth rows should be the same");

        assertTrue(removeRow(htConcepts.getRow(3)), "Removing the fourth row should succeed");
      })

      .thenRunAfterDelay(() -> htConcepts.getRow(3).setCellValue(GLOSSARY_COL_NDX, generalGlossary))

      .thenRunAfterDelay(() ->
      {
        assertEquals(3, term1.concepts.size());
        assertFalse(conceptRows.get(htConcepts.getRow(2)).isDuplicate);
        assertTrue (conceptRows.get(htConcepts.getRow(3)).isDuplicate);

        htConcepts.getRow(2).setCellValue(SENSE_COL_NDX, term1.concepts.get(1).sense.get());
      })

      .thenExpectLackOfPopupAfterDelay()

      /* ************************************************************************ */
      /*                                                                          */
      /*   User enters a non-blank sense (where there is an existing sense        */
      /*   record) for a row that has a duplicate. The new sense should be        */
      /*   assigned to the original concept, and a new concept should be created  */
      /*   for the duplicate row.                                                 */
      /*                                                                          */
      /* ************************************************************************ */

      .thenRun(() ->
      {
        assertEquals(4, term1.concepts.size());
        assertEquals(1, db.conceptSenses.size());
        assertNotEquals(-1, htConcepts.getRow(2).getID(SENSE_COL_NDX), "Sense should be populated in third row.");
        assertEquals("", htConcepts.getRow(3).getText(SENSE_COL_NDX), "Sense should not be populated in fourth row.");
        assertEquals(-1, htConcepts.getRow(3).getID  (SENSE_COL_NDX), "Sense should not be populated in fourth row.");
      });

    queue.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
