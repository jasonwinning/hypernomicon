/*
 * Copyright 2015-2018 Jason Winning
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
import org.hypernomicon.view.dialogs.MergeTermDialogController;
import org.hypernomicon.view.dialogs.RecordDropdownDialogController;
import org.hypernomicon.view.dialogs.SelectConceptDialogController;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import javafx.scene.control.TabPane;
import javafx.scene.Node;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;

//---------------------------------------------------------------------------

public class TermTab extends HyperNodeTab<HDT_Term, HDT_Concept>
{
  public static class ConceptTab extends Tab
  {
    public HDT_Concept concept;
    
    public ConceptTab(HDT_Concept concept)
    {      
      super(concept.glossary.get().name());
      
      this.concept = concept;
      setClosable(false);
    }
    
    public ConceptTab(String text, Node content)
    {
      super(text, content);
      setClosable(false);
    }
    
    public void setConcept(HDT_Concept concept)
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
  private boolean alreadyChangingTab = false, updatingGlossaries = false;

  @Override public HDT_RecordType getType()                  { return hdtTerm; }
  @Override public void enable(boolean enabled)              { ui.tabTerms.getContent().setDisable(enabled == false); }
  @Override public void focusOnSearchKey()                   { ctrlr.focusOnSearchKey(); }
  @Override public void findWithinDesc(String text)          { ctrlr.hilite(text); }
  @Override public TextViewInfo getMainTextInfo()            { return ctrlr.getMainTextInfo(); }
  @Override public void setRecord(HDT_Concept activeRecord)  { curConcept = activeRecord; curTerm = curConcept == null ? null : curConcept.term.get(); }
  @Override public boolean saveToRecord(boolean showMessage) { return ctrlr.save(curConcept, showMessage, this); }

  @Override public void newClick(HDT_RecordType objType, HyperTableRow row) { return; }
  
  public ConceptTab curTab()      { return (ConceptTab) tpConcepts.getSelectionModel().getSelectedItem(); }

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
    
    htGlossaries.buildRows(curTerm.getGlossaries(), (row, glossary) -> row.setCellValue(1, glossary, glossary.name()));  
    
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
  
  @Override protected void init(TabEnum tabEnum)
  {
    this.tabEnum = tabEnum;
    ctrlr.init(hdtConcept, this);
    
    ctrlr.tvParents.getColumns().remove(2);
    ctrlr.apLowerPane.getChildren().setAll(ctrlr.tvRightChildren);
    
    ctrlr.spMain.getItems().remove(1);
    tpConcepts = new TabPane(new ConceptTab("General", ctrlr.apDescription));
    ctrlr.spMain.getItems().add(1, tpConcepts);
    tpConcepts.getTabs().get(0).setClosable(false);
    
    tpConcepts.getSelectionModel().selectedItemProperty().addListener((observable, oldTab, newTab) ->
    {
      if (alreadyChangingTab) return;
      
      if (!ctrlr.save(curConcept, true, this))
      {
        alreadyChangingTab = true;
        tpConcepts.getSelectionModel().select(oldTab);
        alreadyChangingTab = false;

        return;
      }
      
      oldTab.setContent(null);
      newTab.setContent(ctrlr.apDescription);
      
      curConcept = ConceptTab.class.cast(newTab).concept;
      
      ui.viewSequence.updateCurrentView(new HyperView<>(TabEnum.termTab, curConcept, getMainTextInfo()));
      
      HDT_Glossary glossary = curConcept.glossary.get();
      if (glossary.getID() > 1) glossary.viewNow();
      ctrlr.update(curConcept);
      
      htDisplayers.clear();
      populateDisplayers();
    });
    
    ctrlr.tvRightChildren.getColumns().get(0).setText("Type");
    ctrlr.tvRightChildren.getColumns().get(1).setText("Name of record showing this definition");
    ctrlr.tvRightChildren.getColumns().add(new TableColumn<HyperTableRow, HyperTableCell>("Description"));
    ctrlr.spMain.setDividerPosition(1, 0.85);
    
    ctrlr.lblParentCaption.setText("Glossaries:");
    
    ctrlr.tvParents.getColumns().remove(1);
    htGlossaries = new HyperTable(ctrlr.tvParents, 1, true, PREF_KEY_HT_TERM_GLOSSARIES);
    
    htGlossaries.addActionCol(ctGoBtn, 1);
    
    htGlossaries.addColWithUpdateHandler(hdtGlossary, ctDropDownList, (row, cellVal, nextColNdx, nextPopulator) -> updateFromGlossaryHT());
    
    htGlossaries.getColumn(1).setDontCreateNewRecord(true);
    
    htGlossaries.addRemoveMenuItem(this::updateFromGlossaryHT);
    htGlossaries.addChangeOrderMenuItem(true, this::updateFromGlossaryHT);
    
    htDisplayers = new HyperTable(ctrlr.tvRightChildren, 1, false, PREF_KEY_HT_TERM_DISPLAYERS);
    
    htDisplayers.addCol(hdtNone, ctIcon);
    htDisplayers.addCol(hdtNone, ctNone);
    htDisplayers.addCol(hdtNone, ctNone);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void updateFromGlossaryHT()
  {
    if (updatingGlossaries) return;
    
    List<HDT_Glossary> oldList = curTerm.getGlossaries(),
                       newList = htGlossaries.saveToList(1, hdtGlossary);
    
    Set<HDT_Glossary> set = new HashSet<HDT_Glossary>();
    
    newList.removeIf(glossary ->
    {
      if (set.contains(glossary)) return true;

      set.add(glossary);
      return false;
    });
    
    if (newList.size() > oldList.size())
    {
      for (HDT_Glossary glossary : newList)
        if (oldList.contains(glossary) == false)
        {
          addGlossary(glossary, newList.indexOf(glossary));
          break;
        }
    }    
    else if (newList.size() < oldList.size())
    {
      for (HDT_Glossary glossary : oldList)
        if ((newList.contains(glossary) == false) && (newList.size() > 0))
        {
          removeGlossary(glossary);
          break;
        }
    }
    else
    {
      boolean noneReplaced = true;     
 
      for (HDT_Glossary glossary : newList)
        if (oldList.contains(glossary) == false)
        {
          int ndx = newList.indexOf(glossary);
          replaceGlossary(oldList.get(ndx), newList.get(ndx));
          noneReplaced = false;
          break;
        }
      
      if (noneReplaced)
        if (oldList.equals(newList) == false)
          reorderGlossaries(newList);
    }
    
    populateGlossaries();
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public void merge()
  {
    if (ui.cantSaveRecord(true)) return;
    
    RecordDropdownDialogController<HDT_Term> rdd = RecordDropdownDialogController.create("Select a Term Record to Merge With", hdtTerm);    
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
        
    MergeTermDialogController mtd = MergeTermDialogController.create("Specify How to Merge Fields", curTerm, otherTerm);    
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
    
    db.deleteRecord(hdtTerm, otherTerm.getID());
    ui.update();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void moveConcept()
  {
    if (curTerm.concepts.size() < 2)
    {
      messageDialog("There is only one definition for this term; click Merge command instead.", mtInformation);
      return;
    }
    
    if (ui.cantSaveRecord(true)) return;

    HDT_Concept concept = curConcept;
    
    SelectConceptDialogController frmSelectConcept = SelectConceptDialogController.create("Term select", concept);
    
    if (frmSelectConcept.showModal())
    {      
      switchToDifferentTab();
      
      tpConcepts.getTabs().remove(getConceptTab(concept));
      
      HDT_Term newTerm = frmSelectConcept.getTerm();
      
      curTerm.concepts.remove(concept);
      newTerm.concepts.add(concept);       
      
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
    ArrayList<HDT_Concept> newConceptList = new ArrayList<>();
    
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
    db.deleteRecord(hdtConcept, concept.getID());
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private ConceptTab getConceptTab(HDT_Concept concept)
  {
    for (Tab tab : tpConcepts.getTabs())
    {
      ConceptTab conceptTab = (ConceptTab) tab;
      if (conceptTab.concept == concept)
        return conceptTab;
    }
    
    return null;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void addGlossary(HDT_Glossary glossary, int ndx)
  {       
    HyperObjList<HDT_Term, HDT_Concept> objList = db.getObjectList(rtConceptOfTerm, curTerm, true);

    HDT_Concept concept = db.createNewBlankRecord(hdtConcept);
    
    ArrayList<HDT_Concept> newList = new ArrayList<>();
    newList.addAll(objList);
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
    
    for (Tab tab : tpConcepts.getTabs())
      if (tab.getContent() == ctrlr.apDescription)
        tab.setContent(null);
    
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
