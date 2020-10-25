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

package org.hypernomicon.view.tabs;

import org.hypernomicon.view.HyperView;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.scene.control.TabPane;
import javafx.scene.Node;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;
import javafx.scene.input.KeyEvent;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

import org.hypernomicon.dialogs.MergeTermDlgCtrlr;
import org.hypernomicon.dialogs.RecordDropdownDlgCtrlr;
import org.hypernomicon.dialogs.SelectConceptDlgCtrlr;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;

//---------------------------------------------------------------------------

public class TermTab extends HyperNodeTab<HDT_Term, HDT_Concept>
{
  private static class ConceptTab extends Tab
  {
    private HDT_Concept concept;

    private ConceptTab(HDT_Concept concept)
    {
      super(concept.glossary.get().name());

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
      setText(concept.glossary.get().name());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTable htGlossaries, htDisplayers;
  private HDT_Term curTerm;
  private HDT_Concept curConcept;
  private TabPane tpConcepts;
  private long lastArrowKey = 0;
  private boolean alreadyChangingTab = false, updatingGlossaries = false;

  @Override protected RecordType type()             { return hdtTerm; }
  @Override public void enable(boolean enabled)     { ui.tabTerms.getContent().setDisable(enabled == false); }
  @Override public void findWithinDesc(String text) { ctrlr.hilite(text); }
  @Override public TextViewInfo mainTextInfo()      { return ctrlr.mainTextInfo(); }
  @Override public void setRecord(HDT_Concept rec)  { curConcept = rec; curTerm = curConcept == null ? null : curConcept.term.get(); }
  @Override public boolean saveToRecord()           { return ctrlr.saveToRecord(curConcept); }

  private ConceptTab curTab()      { return (ConceptTab) tpConcepts.getSelectionModel().getSelectedItem(); }

  private TermTab() throws IOException
  {
    super(ui.tabTerms);
    baseInit(termTabEnum, ui.tabTerms);
  }

  @SuppressWarnings("unused") public static void create() throws IOException { new TermTab(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean update()
  {
    boolean first = true;
    for (HDT_Concept concept : curTerm.concepts)
    {
      if (first)
      {
        ConceptTab.class.cast(tpConcepts.getTabs().get(0)).setConcept(concept);
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

    populateDisplayers();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateGlossaries()
  {
    updatingGlossaries = true;

    htGlossaries.clear();

    htGlossaries.buildRows(curTerm.getGlossaries(), (row, glossary) -> row.setCellValue(2, glossary, glossary.name()));

    updatingGlossaries = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateDisplayers()
  {
    htDisplayers.buildRows(curConcept.getMainText().getDisplayers(), (row, displayer) ->
    {
      row.setCellValue(0, displayer.getSpoke(), "");
      row.setCellValue(1, displayer.getSpoke(), displayer.getSpoke().getCBText());
      row.setCellValue(2, displayer.getSpoke(), displayer.getMainText().getPlainForDisplay());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void init()
  {
    ctrlr.init(hdtConcept, this);

    ctrlr.tvParents.getColumns().remove(2);
    ctrlr.apLowerPane.getChildren().setAll(ctrlr.tvRightChildren);

    ctrlr.spMain.getItems().remove(1);
    tpConcepts = new TabPane(new ConceptTab("General", ctrlr.apDescription));
    ctrlr.spMain.getItems().add(1, tpConcepts);
    tpConcepts.getTabs().get(0).setClosable(false);

    tpConcepts.addEventFilter(KeyEvent.ANY, event ->
    {
      if (event.getCode().isArrowKey())
        lastArrowKey = Instant.now().toEpochMilli();
    });
    
    tpConcepts.getSelectionModel().selectedItemProperty().addListener((ob, oldTab, newTab) ->
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

      curConcept = ConceptTab.class.cast(newTab).concept;

      ui.viewSequence.saveViewToCurrentSlotAndTab(new HyperView<>(termTabEnum, curConcept, mainTextInfo()));

      HDT_Glossary glossary = curConcept.glossary.get();
      if (glossary.getID() > 1) glossary.viewNow();
      ctrlr.update(curConcept);

      htDisplayers.clear();
      populateDisplayers();

      ui.updateFavorites();
    });

    ctrlr.tvRightChildren.getColumns().get(0).setText("Type");
    ctrlr.tvRightChildren.getColumns().get(1).setText("Name of record showing this definition");
    ctrlr.tvRightChildren.getColumns().add(new TableColumn<HyperTableRow, HyperTableCell>("Description"));
    ctrlr.spMain.setDividerPosition(1, 0.85);

    ctrlr.lblParentCaption.setText("Glossaries:");

    htGlossaries = new HyperTable(ctrlr.tvParents, 2, true, PREF_KEY_HT_TERM_GLOSSARIES);

    htGlossaries.addActionCol(ctGoBtn, 2);
    htGlossaries.addActionCol(ctBrowseBtn, 2);

    htGlossaries.addColWithUpdateHandler(hdtGlossary, ctDropDownList, (row, cellVal, nextColNdx, nextPopulator) -> updateFromGlossaryHT())
                .setDontCreateNewRecord(true);

    htGlossaries.addRemoveMenuItem(this::updateFromGlossaryHT);
    htGlossaries.addChangeOrderMenuItem(true, this::updateFromGlossaryHT);

    htDisplayers = new HyperTable(ctrlr.tvRightChildren, 1, false, PREF_KEY_HT_TERM_DISPLAYERS);

    htDisplayers.addCol(hdtNone, ctIcon);
    htDisplayers.addCol(hdtNone, ctNone);
    htDisplayers.addCol(hdtNone, ctNone);

    htDisplayers.addDefaultMenuItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateFromGlossaryHT()
  {
    if (updatingGlossaries) return;

    List<HDT_Glossary> oldList = curTerm.getGlossaries(),
                       newList = htGlossaries.saveToList(2, hdtGlossary);

    Set<HDT_Glossary> set = new HashSet<>();

    newList.removeIf(glossary ->
    {
      if (set.contains(glossary)) return true;

      set.add(glossary);
      return false;
    });

    if (newList.size() > oldList.size())
      nullSwitch(findFirst(newList, Predicate.not(oldList::contains)), glossary -> addGlossary(glossary, newList.indexOf(glossary)));
    else if (newList.size() < oldList.size())
      nullSwitch(findFirst(oldList, glossary -> (newList.contains(glossary) == false) && (newList.size() > 0)), this::removeGlossary);
    else
    {
      HDT_Glossary glossary = findFirst(newList, Predicate.not(oldList::contains));

      if (glossary != null)
      {
        int ndx = newList.indexOf(glossary);
        replaceGlossary(oldList.get(ndx), newList.get(ndx));
      }
      else
      {
        if (oldList.equals(newList) == false)
          reorderGlossaries(newList);
      }
    }

    populateGlossaries();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void merge()
  {
    if (ui.cantSaveRecord()) return;

    RecordDropdownDlgCtrlr<HDT_Term> rdd = RecordDropdownDlgCtrlr.build(hdtTerm);
    if (rdd.showModal() == false) return;

    HDT_Term otherTerm = rdd.getRecord();

    if ((otherTerm == null) || (otherTerm == curTerm)) return;

    List<HDT_Glossary> otherGlossaries = otherTerm.getGlossaries();

    for (HDT_Glossary glossary : curTerm.getGlossaries())
      if (otherGlossaries.contains(glossary))
      {
        messageDialog("Both terms already have definitions for glossary \"" + glossary.name() + "\"", MessageDialogType.mtError);
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
      switchToDifferentTab();

      tpConcepts.getTabs().remove(getConceptTab(concept));

      curTerm.concepts.remove(concept);
      frmSelectConcept.getTerm().concepts.add(concept);
      concept.glossary.set(frmSelectConcept.getGlossary());

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

  private void reorderGlossaries(List<HDT_Glossary> newGlossaryList)
  {
    List<HDT_Concept> newConceptList = new ArrayList<>();

    newGlossaryList.forEach(glossary -> newConceptList.add(curTerm.getConcept(glossary)));

    HyperObjList<HDT_Term, HDT_Concept> objList = db.getObjectList(RelationType.rtConceptOfTerm, curTerm, true);

    objList.reorder(newConceptList);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void replaceGlossary(HDT_Glossary oldGlossary, HDT_Glossary newGlossary)
  {
    HDT_Concept concept = curTerm.getConcept(oldGlossary);

    concept.glossary.set(newGlossary);
    getConceptTab(concept).setText(newGlossary.name());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeGlossary(HDT_Glossary glossary)
  {
    if (confirmDialog("Are you sure you want to remove the concept definition associated with the glossary \"" + glossary.name() + "\"?") == false)
      return;

    HDT_Concept concept = curTerm.getConcept(glossary);

    if (curTab().concept == concept)
      switchToDifferentTab();

    tpConcepts.getTabs().remove(getConceptTab(concept));
    db.deleteRecord(concept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ConceptTab getConceptTab(HDT_Concept concept)
  {
    return (ConceptTab) findFirst(tpConcepts.getTabs(), tab -> ConceptTab.class.cast(tab).concept == concept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addGlossary(HDT_Glossary glossary)
  {
    addGlossary(glossary, curTerm.getGlossaries().size());
  }

  private void addGlossary(HDT_Glossary glossary, int ndx)
  {
    HyperObjList<HDT_Term, HDT_Concept> objList = db.getObjectList(rtConceptOfTerm, curTerm, true);

    HDT_Concept concept = db.createNewBlankRecord(hdtConcept);

    List<HDT_Concept> newList = new ArrayList<>(objList);
    newList.add(ndx, concept);

    curTerm.concepts.add(ndx, concept);
    concept.glossary.set(glossary);

    objList.reorder(newList);

    tpConcepts.getTabs().add(ndx, new ConceptTab(concept));
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

    htGlossaries.clear();
    htDisplayers.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(ctrlr.spMain, PREF_KEY_TERM_TOP_VERT, 0);
    setDividerPosition(ctrlr.spMain, PREF_KEY_TERM_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(ctrlr.spMain, PREF_KEY_TERM_TOP_VERT, 0);
    getDividerPosition(ctrlr.spMain, PREF_KEY_TERM_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
