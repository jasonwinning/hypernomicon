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

package org.hypernomicon.view.tabs;

import org.hypernomicon.view.HyperView;
import org.hypernomicon.view.populators.Populator.DisplayKind;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.scene.control.TabPane;
import javafx.application.Platform;
import javafx.scene.Node;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.input.KeyEvent;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.hypernomicon.dialogs.MergeTermDlgCtrlr;
import org.hypernomicon.dialogs.RecordDropdownDlgCtrlr;
import org.hypernomicon.dialogs.SelectConceptDlgCtrlr;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;

//---------------------------------------------------------------------------

public final class TermTab extends HyperNodeTab<HDT_Term, HDT_Concept>
{
  private static final class ConceptTab extends Tab
  {
    private HDT_Concept concept;

    private ConceptTab(HDT_Concept concept)
    {
      super(tabName(concept));

      this.concept = concept;
      setClosable(false);
    }

    private ConceptTab(String text, Node content)
    {
      super(text, content);
      setClosable(false);
    }

    private void setConcept(HDT_Concept concept)
    {
      this.concept = concept;
      updateName();
    }

    private void updateName()
    {
      setText(tabName(concept));
    }

    private static String tabName(HDT_Concept concept)
    {
      String glossaryName = concept.glossary.get().name(),
             senseName = concept.sense.isNull() ? "" : concept.sense.get().name();

      if (concept.glossary.getID() == 1)
        return senseName.isBlank() ? glossaryName : senseName;

      if (senseName.isBlank())
        return glossaryName;

      return senseName + " (" + glossaryName + ")";
    }
  }

//---------------------------------------------------------------------------

  private final class GlossaryRow
  {
    private final HDT_Glossary glossary;
    private final HDT_ConceptSense sense;
    private final String senseText;
    private final HDT_Concept childConcept, parentConcept;

    private GlossaryRow(HyperTableRow row, boolean validate)
    {
      HDT_Glossary tempGlossary = row.getRecord(2);
      HDT_ConceptSense tempSense = row.getRecord(3);
      senseText = row.getText(3);

      if ((tempSense == null) && (senseText.isBlank() == false))
        childConcept = null;
      else
        childConcept = curTerm.getConcept(tempGlossary, tempSense);

      if (validate && (childConcept == null))
      {
        tempGlossary = null;
        sense = null;
      }
      else
        sense = row.getRecord(3);

      glossary = tempGlossary;

      HDT_Concept tempParentConcept = row.getRecord(4);

      if (tempParentConcept != null)
        if (tempParentConcept.glossary.get() != glossary)
          tempParentConcept = null;

      if ((tempParentConcept != null) && validate)
        if (childConcept.parentConcepts.contains(tempParentConcept) == false)
          tempParentConcept = null;

      parentConcept = tempParentConcept;
    }

    private GlossaryRow(HDT_Glossary glossary, HDT_ConceptSense sense)
    {
      this.glossary = glossary;
      childConcept = curTerm.getConcept(glossary, sense);
      this.sense = sense;
      senseText = sense == null ? "" : sense.name();
      parentConcept = null;
    }

    private GlossaryRow(HDT_Concept childConcept, HDT_Concept parentConcept)
    {
      this.parentConcept = parentConcept;
      glossary = parentConcept.glossary.get();
      this.childConcept = childConcept;
      sense = childConcept == null ? null : childConcept.sense.get();
      senseText = sense == null ? "" : sense.name();
    }

    public void populateTableRow(HyperTableRow row)
    {
      boolean wasUpdatingGlossaries = updatingGlossaries;
      updatingGlossaries = true;

      if (glossary == null)
        row.setCellValue(2, "", hdtGlossary);
      else
        row.setCellValue(2, glossary, glossary.name());

      if (sense == null)
        row.setCellValue(3, "", hdtConceptSense);
      else
        row.setCellValue(3, sense, sense.listName());

      SubjectPopulator parentConceptPop = htGlossaries.getPopulator(4);
      parentConceptPop.setObj(row, glossary);

      if (parentConcept == null)
        row.setCellValue(4, "", hdtConcept);
      else
        row.setCellValue(4, parentConcept, parentConcept.listName());

      updatingGlossaries = wasUpdatingGlossaries;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTable htGlossaries, htSubConcepts, htDisplayers;
  private HDT_Term curTerm;
  private HDT_Concept curConcept;
  private TabPane tpConcepts;
  private long lastArrowKey = 0L;
  private boolean alreadyChangingTab = false, updatingGlossaries = false;

  private final Map<HyperTableRow, GlossaryRow> glossaryRows = new HashMap<>();

  @Override protected RecordType type()             { return hdtTerm; }
  @Override public void setRecord(HDT_Concept rec)  { curConcept = rec; curTerm = curConcept == null ? null : curConcept.term.get(); }
  @Override public boolean saveToRecord()           { return ctrlr.saveToRecord(curConcept); }

  private ConceptTab curTab()                       { return (ConceptTab) tpConcepts.getSelectionModel().getSelectedItem(); }

  private TermTab(Tab tab) throws IOException           { super(tab); }
  public static void create(Tab tab) throws IOException { new TermTab(tab).baseInit(termTabEnum, tab); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void update()
  {
    boolean first = true;
    for (HDT_Concept concept : curTerm.concepts)
    {
      if (first)
      {
        ((ConceptTab) tpConcepts.getTabs().get(0)).setConcept(concept);
        first = false;
      }
      else
        tpConcepts.getTabs().add(new ConceptTab(concept));
    }

    alreadyChangingTab = true;
    tpConcepts.getSelectionModel().select(getConceptTab(curConcept));
    alreadyChangingTab = false;

    tpConcepts.getTabs().get(0).setContent(null);
    getConceptTab(curConcept).setContent(ctrlr.apDescription);

    ctrlr.update(curConcept);

    populateGlossaries();

    populateDisplayersAndSubConcepts();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateGlossaries()
  {
    updatingGlossaries = true;

    htGlossaries.clear();
    glossaryRows.clear();

    List<GlossaryRow> glossaryRowList = new ArrayList<>();
    for (HDT_Concept childConcept : curTerm.concepts)
    {
      if (childConcept.parentConcepts.isEmpty())
        glossaryRowList.add(new GlossaryRow(childConcept.glossary.get(), childConcept.sense.get()));
      else
      {
        for (HDT_Concept parentConcept : childConcept.parentConcepts)
        {
          if (childConcept.glossary.get() != parentConcept.glossary.get())
            messageDialog("Internal error #38436", mtError);
          else
            glossaryRowList.add(new GlossaryRow(childConcept, parentConcept));
        }
      }
    }

    htGlossaries.buildRows(glossaryRowList, (row, glossaryRow) ->
    {
      glossaryRows.put(row, glossaryRow);

      glossaryRow.populateTableRow(row);
    });

    updatingGlossaries = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateDisplayersAndSubConcepts()
  {
    htDisplayers.buildRows(db.displayerStream(curConcept), (row, displayer) ->
    {
      row.setCellValue(0, displayer, "");
      row.setCellValue(1, displayer, displayer.getCBText());
      row.setCellValue(2, displayer, displayer.getMainText().getPlainForDisplay());
    });

    htSubConcepts.buildRows(curConcept.subConcepts, (row, subConcept) ->
    {
      row.setCellValue(1, subConcept, subConcept.name());
      row.setCellValue(2, subConcept, subConcept.getMainText().getPlainForDisplay());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void init()
  {
    ctrlr.init(hdtConcept, this);

    TableColumn<HyperTableRow, HyperTableCell> senseCol = new TableColumn<>("Sense");
    senseCol.setPrefWidth(150.0);
    ctrlr.tvParents.getColumns().add(3, senseCol);

    ctrlr.tvParents.getColumns().get(2).setText("Glossary");
    ctrlr.tvParents.getColumns().get(4).setText("Parent Concept");

    ctrlr.tvLeftChildren.getColumns().get(1).setText("Sub-Concepts Under This Concept");
    ctrlr.tvLeftChildren.getColumns().get(2).setText("Definition");

    ctrlr.spMain.getItems().remove(1);
    tpConcepts = new TabPane(new ConceptTab("General", ctrlr.apDescription));
    ctrlr.spMain.getItems().add(1, tpConcepts);
    tpConcepts.getTabs().get(0).setClosable(false);

    tpConcepts.addEventFilter(KeyEvent.ANY,
      event ->
      {
        if (event.getCode().isArrowKey())
          lastArrowKey = Instant.now().toEpochMilli();
      });

    tpConcepts.getSelectionModel().selectedItemProperty().addListener(
      (ob, oldTab, newTab) ->
      {
        if (alreadyChangingTab) return;

        if (((Instant.now().toEpochMilli() - lastArrowKey) < IGNORE_ARROW_KEYS_IN_TAB_PANE_MS) || !ctrlr.saveToRecord(curConcept))
        {
          alreadyChangingTab = true;
          tpConcepts.getSelectionModel().select(oldTab);
          alreadyChangingTab = false;

          return;
        }

        oldTab.setContent(null);
        newTab.setContent(ctrlr.apDescription);

        curConcept = ((ConceptTab) newTab).concept;

        ui.viewSequence.saveViewToCurrentSlotAndTab(new HyperView<>(termTabEnum, curConcept, mainTextInfo()));

        HDT_Glossary glossary = curConcept.glossary.get();
        if (glossary.getID() > 1) glossary.viewNow();
        ctrlr.update(curConcept);

        htSubConcepts.clear();
        htDisplayers .clear();
        populateDisplayersAndSubConcepts();

        ui.updateFavorites();
      });

    ctrlr.tvRightChildren.getColumns().get(0).setText("Type");
    ctrlr.tvRightChildren.getColumns().get(1).setText("Name of record showing this definition");
    ctrlr.tvRightChildren.getColumns().add(new TableColumn<HyperTableRow, HyperTableCell>("Description"));
    ctrlr.spMain.setDividerPosition(1, 0.85);

    htGlossaries = new HyperTable(ctrlr.tvParents, 2, true, PREF_KEY_HT_TERM_GLOSSARIES);

    htGlossaries.addActionColWithButtonHandler(ctGoBtn, 2,
      (row, colNdx) ->
      {
        HDT_Concept parentConcept = row.getRecord(4);
        ui.goToRecord(parentConcept != null ? parentConcept : row.getRecord(2), true);
      });

    htGlossaries.addActionColWithButtonHandler(ctBrowseBtn, 2,
      (row, colNdx) ->
      {
        HDT_Glossary glossary = row.getRecord(2);
        HDT_Concept childConcept = glossary == null ? null : curTerm.getConcept(glossary, row.getRecord(3)),
                    parentConcept = row.getRecord(4);

        ui.treeSelector.reset(childConcept == null ? curTerm : childConcept, true, row);

        ui.treeSelector.addTargetType(hdtGlossary);
        ui.treeSelector.addTargetType(hdtConcept);

        ui.treeSelector.setTarget(nullSwitch(parentConcept, glossary));

        ui.goToTreeRecord(childConcept == null ? db.glossaries.getByID(1) : childConcept);
      });

    htGlossaries.addColWithUpdateHandler(hdtGlossary, ctDropDownList, (row, cellVal, nextColNdx, nextPopulator) -> updateGlossaryRow(row))
                .setDontCreateNewRecord(true);

    htGlossaries.addColWithUpdateHandler(hdtConceptSense, ctDropDown, (row, cellVal, nextColNdx, nextPopulator) -> updateSense(row, cellVal));

    htGlossaries.addColAltPopulatorWithUpdateHandler(hdtConcept, ctDropDownList, new SubjectPopulator(rtGlossaryOfConcept, true,
      id -> // Populator ID filter
      {
        if ((id < 1) || HDT_Record.isEmpty(curTerm)) return false;

        return curTerm.concepts.contains(db.concepts.getByID(id)) == false;
      }, DisplayKind.listName),

      (row, cellVal, nextColNdx, nextPopulator) -> updateGlossaryRow(row));

    htGlossaries.addContextMenuItem("Remove this row",
      row ->
      {
        GlossaryRow glossaryRow = glossaryRows.get(row);

        if (glossaryRow == null)
          return false;

        if (curTerm.concepts.size() > 1)
          return true;

        for (GlossaryRow otherGlossaryRow : glossaryRows.values())
          if ((otherGlossaryRow.childConcept == glossaryRow.childConcept) && (otherGlossaryRow.parentConcept != glossaryRow.parentConcept))
            return true;

        return false;
      },

      this::removeRow);

    htGlossaries.addContextMenuItem("Change order of concepts",
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

        ht.addCol(hdtGlossary, ctNone);
        ht.addCol(hdtConceptSense, ctNone);

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
          db.getObjectList(rtConceptOfTerm, curTerm, true).reorder(ht.dataRowStream().map(tableRow -> curTerm.getConcept(tableRow.getRecord(0), tableRow.getRecord(1)))
                                                                                     .collect(Collectors.toList()));

          ui.update();
        };

        ht.triggerChangeOrder(false, completeHndlr);
      });

    htSubConcepts = new HyperTable(ctrlr.tvLeftChildren, 2, true, PREF_KEY_HT_CONCEPT_SUB);

    htSubConcepts.addActionCol(ctGoNewBtn, 2);
    htSubConcepts.addCol(hdtConcept, ctNone);
    htSubConcepts.addCol(hdtConcept, ctNone);

    htDisplayers = new HyperTable(ctrlr.tvRightChildren, 1, false, PREF_KEY_HT_TERM_DISPLAYERS);

    htDisplayers.addIconCol();
    htDisplayers.addCol(hdtNone, ctNone);
    htDisplayers.addCol(hdtNone, ctNone);

    htSubConcepts.addDefaultMenuItems();
    htDisplayers .addDefaultMenuItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeRow(HyperTableRow row)
  {
    GlossaryRow glossaryRow = glossaryRows.get(row);
    if (glossaryRow == null) return;

    boolean deleteConcept = false;

    if (glossaryRow.parentConcept != null)
    {
      if (glossaryRow.childConcept.parentConcepts.size() == 1)
        deleteConcept = true;
      else
        glossaryRow.childConcept.removeParent(glossaryRow.parentConcept);
    }

    if (deleteConcept || glossaryRow.childConcept.parentConcepts.isEmpty())
      if (removeConcept(glossaryRow.glossary, glossaryRow.sense) == false)
        return;

    glossaryRows.remove(row);
    htGlossaries.removeRow(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectFromTree(HyperTableRow row, HDT_Glossary glossary, HDT_ConceptSense sense, HDT_Concept parentConcept)
  {
    GlossaryRow glossaryRow = parentConcept != null ? new GlossaryRow(curTerm.getConcept(glossary, sense), parentConcept) : new GlossaryRow(glossary, sense);

    glossaryRow.populateTableRow(row);
    updateGlossaryRow(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateSense(HyperTableRow editedRow, HyperTableCell newCell)
  {
    if (updatingGlossaries) return;

    GlossaryRow oldGlossaryRow = glossaryRows.get(editedRow);
    if (oldGlossaryRow == null)
      oldGlossaryRow = new GlossaryRow((HDT_Glossary)null, null);

    HDT_Concept concept = oldGlossaryRow.childConcept;

    if (concept == null)
    {
      if (HyperTableCell.getCellText(newCell).isBlank()) return;

      oldGlossaryRow.populateTableRow(editedRow);

      HDT_Glossary glossary = curTerm.concepts.stream().noneMatch(koncept -> koncept.glossary.get().getID() == 1) ?
        curTerm.concepts.get(0).glossary.get()
      :
        db.glossaries.getByID(1);

      editedRow.setCellValue(2, glossary, glossary.name());
      Platform.runLater(() -> editedRow.setCellValue(3, newCell));

      return;
    }

    HDT_ConceptSense sense = newCell.getRecord();

    if ((oldGlossaryRow.sense != null) && ((sense == oldGlossaryRow.sense) || editedRow.getText(3).equalsIgnoreCase(oldGlossaryRow.sense.name())))
    {
      oldGlossaryRow.populateTableRow(editedRow);
      return;
    }

    for (HyperTableRow row : htGlossaries.dataRows())
    {
      if (row != editedRow)
      {
        HDT_Concept otherConcept = curTerm.getConcept(row.getRecord(2), row.getRecord(3));
        if ((concept != otherConcept) && (concept.glossary.get() == otherConcept.glossary.get()) && row.getText(3).equalsIgnoreCase(HyperTableCell.getCellText(newCell)))
        {
          messageDialog("This term already has a concept in the same glossary with the same sense.", mtError);
          oldGlossaryRow.populateTableRow(editedRow);
          return;
        }
      }
    }

    if ((sense == null) && (HyperTableCell.getCellText(newCell).length() > 0))
    {
      sense = db.createNewBlankRecord(hdtConceptSense);
      sense.setName(HyperTableCell.getCellText(newCell));
    }

    for (Entry<HyperTableRow, GlossaryRow> entry : glossaryRows.entrySet())
    {
      if (entry.getKey() != editedRow)
      {
        GlossaryRow glossaryRow = entry.getValue();
        if ((oldGlossaryRow.glossary == glossaryRow.glossary) &&
            (oldGlossaryRow.sense == glossaryRow.sense) &&
            (oldGlossaryRow.parentConcept == null))
        {
          concept = addConceptInGlossary(oldGlossaryRow.glossary, sense);
          break;
        }
      }
    }

    concept.sense.set(sense);
    glossaryRows.put(editedRow, new GlossaryRow(oldGlossaryRow.glossary, sense));
    populateGlossaries();
    tpConcepts.getTabs().forEach(tab -> ((ConceptTab) tab).updateName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateGlossaryRow(HyperTableRow row)
  {
    if (updatingGlossaries) return;

    GlossaryRow oldGlossaryRow = glossaryRows.get(row),
                newGlossaryRow = new GlossaryRow(row, false);

    if ((glossaryRows.size() == 1) && (newGlossaryRow.glossary == null))
    {
      oldGlossaryRow.populateTableRow(row);
      return;
    }

    if (oldGlossaryRow == null)
      oldGlossaryRow = new GlossaryRow((HDT_Glossary)null, null);

    // First, check to see if parent concept from row being edited should be removed
    // -----------------------------------------------------------------------------

    if ((oldGlossaryRow.parentConcept != null) && (newGlossaryRow.parentConcept != oldGlossaryRow.parentConcept))
      oldGlossaryRow.childConcept.removeParent(oldGlossaryRow.parentConcept);

    HDT_Concept newChildConcept = newGlossaryRow.childConcept;

    // Second, check to see if existing glossary for row being edited should be removed/replaced
    // -----------------------------------------------------------------------------------------

    if (newGlossaryRow.glossary != oldGlossaryRow.glossary)
    {
      if ((oldGlossaryRow.childConcept != null) && oldGlossaryRow.childConcept.parentConcepts.isEmpty())
      {
        if ((newGlossaryRow.glossary == null) || (newGlossaryRow.childConcept != null))
        {
          if (removeConcept(oldGlossaryRow.glossary, oldGlossaryRow.sense) == false)
          {
            oldGlossaryRow.populateTableRow(row);
            return;
          }
        }
        else
        {
          if (replaceGlossary(oldGlossaryRow.glossary, oldGlossaryRow.sense, newGlossaryRow.glossary) == false)
          {
            oldGlossaryRow.populateTableRow(row);
            return;
          }

          newChildConcept = oldGlossaryRow.childConcept;
        }
      }

    // Third, add new concept for row that was edited if needed
    // --------------------------------------------------------

      if ((newGlossaryRow.glossary != null) && (newChildConcept == null))
        newChildConcept = addConceptInGlossary(newGlossaryRow.glossary, newGlossaryRow.sense);
    }

    // Fourth, add parent concept for row that was edited if needed
    // ------------------------------------------------------------

    if ((newChildConcept != null) && (newGlossaryRow.parentConcept != null))
    {
      try
      {
        newChildConcept.addParentConcept(newGlossaryRow.parentConcept);
      }
      catch (RelationCycleException e)
      {
        messageDialog("Unable to add parent concept: A cycle would result.", mtError);

        oldGlossaryRow.populateTableRow(row);
        return;
      }
    }

    newGlossaryRow = new GlossaryRow(row, true);
    glossaryRows.put(row, newGlossaryRow);
    newGlossaryRow.populateTableRow(row);
    tpConcepts.getTabs().forEach(tab -> ((ConceptTab) tab).updateName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void merge()
  {
    if (ui.cantSaveRecord()) return;

    RecordDropdownDlgCtrlr<HDT_Term> rdd = RecordDropdownDlgCtrlr.build(hdtTerm, id -> id != curTerm.getID());

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

        messageDialog(msg, mtError);
        return;
      }

    MergeTermDlgCtrlr mtd = MergeTermDlgCtrlr.build(curTerm, otherTerm);
    if (mtd.showModal() == false) return;

    String oldKey1 = curTerm.getSearchKey();
    String oldKey2 = otherTerm.getSearchKey();

    try
    {
      otherTerm.setSearchKey("");
      curTerm.setSearchKey(mtd.getKey());
    }
    catch (SearchKeyException e)
    {
      messageDialog(e.getMessage(), mtError);

      try
      {
        curTerm.setSearchKey(oldKey1);
        otherTerm.setSearchKey(oldKey2);
      }
      catch (SearchKeyException e1) { noOp(); }

      ui.update();
      return;
    }

    Iterator<HDT_Concept> it = otherTerm.concepts.iterator();
    while (it.hasNext())
    {
      HDT_Concept concept = it.next();
      it.remove();
      curTerm.concepts.add(concept);
    }

    curTerm.setName(mtd.getName());

    db.deleteRecord(otherTerm);
    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void moveConcept()
  {
    if (curTerm.concepts.size() < 2)
    {
      messageDialog("There is only one definition for this term; click Merge command instead.", mtInformation);
      return;
    }

    if (ui.cantSaveRecord()) return;

    HDT_Concept concept = curConcept;

    SelectConceptDlgCtrlr frmSelectConcept = SelectConceptDlgCtrlr.build(concept);

    if (frmSelectConcept.showModal())
    {
      if (concept.glossary.get() != frmSelectConcept.getGlossary() &&
          ((concept.parentConcepts.isEmpty() == false) || (concept.subConcepts.isEmpty() == false)))
      {
        if (confirmDialog("This will unassign any parent or child concepts for Term \"" + concept.listName() + "\", Glossary \"" + concept.glossary.get().name() + "\". Proceed?") == false)
        {
          ui.update();
          return;
        }

        List.copyOf(concept.parentConcepts).forEach(concept::removeParent);
        List.copyOf(concept.subConcepts).forEach(subConcept -> subConcept.removeParent(concept));
      }

      switchToDifferentTab();

      tpConcepts.getTabs().remove(getConceptTab(concept));

      curTerm.concepts.remove(concept);
      frmSelectConcept.getTerm().concepts.add(concept);

      concept.glossary.set(frmSelectConcept.getGlossary());

      HDT_ConceptSense sense = frmSelectConcept.getSense();
      if (sense == null)
      {
        String senseText = frmSelectConcept.getSenseText();
        if (senseText.isBlank() == false)
        {
          sense = db.createNewBlankRecord(hdtConceptSense);
          sense.setName(senseText);
        }
      }

      concept.sense.set(sense);

      ui.goToRecord(concept, false);
    }
    else
      ui.update();
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

    if ((concept.parentConcepts.isEmpty() == false) || (concept.subConcepts.isEmpty() == false))
      if (confirmDialog("This will unassign any parent or child concepts for Term \"" + concept.listName() + "\", Glossary \"" + oldGlossary.name() + "\". Proceed?") == false)
        return false;

    concept.glossary.set(newGlossary);

    List.copyOf(concept.parentConcepts).forEach(concept::removeParent);
    List.copyOf(concept.subConcepts).forEach(subConcept -> subConcept.removeParent(concept));

    getConceptTab(concept).setText(newGlossary.name());

    htSubConcepts.clear();
    htDisplayers .clear();
    populateDisplayersAndSubConcepts();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean removeConcept(HDT_Glossary glossary, HDT_ConceptSense sense)
  {
    HDT_Concept concept = curTerm.getConcept(glossary, sense);
    if ((concept.getMainText().isEmpty() == false) || concept.hasHub())
    {
      if (sense == null)
      {
        if (confirmDialog("Are you sure you want to remove the concept definition associated with the glossary \"" + glossary.name() + "\"?") == false)
          return false;
      }
      else
      {
        if (confirmDialog("Are you sure you want to remove the concept definition associated with the glossary \"" + glossary.name() +
                          "\", sense \"" + sense.name() + "\"?") == false)
          return false;
      }
    }

    if (curTab().concept == concept)
      switchToDifferentTab();

    tpConcepts.getTabs().remove(getConceptTab(concept));
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

  public HDT_Concept addConceptInGlossary(HDT_Glossary glossary, HDT_ConceptSense sense)
  {
    HDT_Concept concept = db.createNewBlankRecord(hdtConcept);

    curTerm.concepts.add(concept);
    concept.glossary.set(glossary);
    concept.sense.set(sense);

    tpConcepts.getTabs().add(new ConceptTab(concept));

    return concept;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    ctrlr.clear();

    tpConcepts.getTabs().stream().filter(tab -> tab.getContent() == ctrlr.apDescription).forEach(tab -> tab.setContent(null));

    alreadyChangingTab = true;
    while (tpConcepts.getTabs().size() > 1)
      tpConcepts.getTabs().remove(1);
    alreadyChangingTab = false;

    tpConcepts.getTabs().get(0).setContent(ctrlr.apDescription);

    htGlossaries .clear();
    htSubConcepts.clear();
    htDisplayers .clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void newClick(RecordType objType, HyperTableRow row)
  {
    if (ui.cantSaveRecord()) return;

    switch (objType)
    {
      case hdtConcept :

        if (curConcept == null) return;

        HDT_Glossary glossary = curConcept.glossary.get();
        HDT_Term term = HDT_Term.create(glossary);
        term.getConcept(glossary, null).parentConcepts.add(curConcept);

        ui.goToRecord(term, false);
        break;

      default:
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(ctrlr.spMain, PREF_KEY_TERM_TOP_VERT, 0);
    setDividerPosition(ctrlr.spMain, PREF_KEY_TERM_BOTTOM_VERT, 1);
    setDividerPosition(ctrlr.spChildren, PREF_KEY_TERM_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(ctrlr.spMain, PREF_KEY_TERM_TOP_VERT, 0);
    getDividerPosition(ctrlr.spMain, PREF_KEY_TERM_BOTTOM_VERT, 1);
    getDividerPosition(ctrlr.spChildren, PREF_KEY_TERM_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
