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

package org.hypernomicon.view;

import java.util.List;

import org.hypernomicon.model.records.*;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.TreeTabController;
import org.hypernomicon.view.wrappers.ClickHoldButton;

import static org.hypernomicon.App.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.tabs.HyperTab.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;

import javafx.scene.control.TabPane;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class HyperViewSequence
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  private TabPane tabPane;
  private ViewList viewList;
  private boolean alreadyChangingTab = false;
  private ClickHoldButton chbBack, chbForward;
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public HyperViewSequence(TabPane tabPane, ClickHoldButton chbForward, ClickHoldButton chbBack)
  {
    this.tabPane = tabPane;
    this.chbForward = chbForward;
    this.chbBack = chbBack;
    viewList = new ViewList(this);
    
    tabPane.getSelectionModel().selectedItemProperty().addListener((observable, oldTab, newTab) ->
    {
      HyperTab<? extends HDT_Base, ? extends HDT_Base> hyperTab;
      
      if (db.isLoaded() == false) return;      
      if (alreadyChangingTab) return;
      
      if (ui.cantSaveRecord(true))
      {
        alreadyChangingTab = true;
        tabPane.getSelectionModel().select(oldTab);
        alreadyChangingTab = false;

        return;
      }
      
      hyperTab = getHyperTabByTab(newTab);
      TabEnum tabEnum = hyperTab.getTabEnum();
      
      if (tabEnum != TabEnum.workTab)
        bibManagerDlg.workRecordToAssign.set(null);
      
      forwardToNewSlot(tabEnum);    
    });   
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTab<? extends HDT_Base, ? extends HDT_Base> curHyperTab() { return getHyperTab(curTabEnum()); }
  public HyperView<? extends HDT_Base> curHyperView()                   { return viewList.getView(); }
  public boolean isEmpty()                                              { return viewList.isEmpty(); }  
  public HyperTab.TabEnum curTabEnum()                                  { return curHyperView().getTabEnum();  }  
  public void updateCurrentView(HyperView<? extends HDT_Base> view)     { viewList.setView(view); setTabView(view); }
  public void stepForward()                                             { saveViewToSequence(false); viewList.goForward(false); update(); }
  public void stepBack()                                                { saveViewToSequence(false); viewList.goBack(); update(); }
  public void removeRecord(HDT_Base record)                             { viewList.removeRecord(record); }
  public void refresh()                                                 { viewList.refreshAll(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void activateCurrentSlot()             
  { 
    if (viewList.isEmpty())
    {
      forwardToNewSlot(HyperTab.getHyperTabByTab(tabPane.getSelectionModel().getSelectedItem()).getTabEnum());
      return;
    }
    
    saveViewToSequence(true); 
    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void forwardToNewSlot(TabEnum tabEnum)
  {
    forwardToNewSlotAndView(getHyperTab(tabEnum).getView());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void forwardToNewSlotAndView(HyperView<? extends HDT_Base> hyperView)
  {
    saveViewToSequence(false);    
    HyperTab.setTabView(hyperView);    
    
    boolean dontAdvance = false;
    
    if (viewList.isEmpty() == false)
    {
      HyperView<? extends HDT_Base> view = viewList.getView();
      
      if ((view.getTabEnum() != queryTab) && (view.getTabEnum() != treeTab))
      {
        if (view.getViewRecord() == null)
          dontAdvance = true;
      }
    }
    
    viewList.clearFollowingViews();

    if (dontAdvance == false)
      viewList.goForward(true);

    viewList.setView(getHyperTab(hyperView.getTabEnum()).getView());
    
    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  void saveViewToSequence(boolean okToInsert)
  {
    if (viewList.isEmpty() && (okToInsert == false)) return;

    HyperTab<? extends HDT_Base, ? extends HDT_Base> hyperTab = curHyperTab();
    HDT_Base record = hyperTab.activeRecord();
    
    if (record != null)
      if (HDT_Record.isEmpty(record))
        return;
    
    updateTabView(hyperTab);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> void updateTabView(HyperTab<HDT_RT, HDT_CT> hyperTab)
  {
    updateCurrentView(new HyperView<HDT_CT>(curTabEnum(), hyperTab.viewRecord(), hyperTab.getMainTextInfo()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void update()
  {
    HyperView<? extends HDT_Base> curView = curHyperView();
    
    HyperTab<? extends HDT_Base, ? extends HDT_Base> curHyperTab = setTabView(curView);
    
    alreadyChangingTab = true;
    tabPane.getSelectionModel().select(curHyperTab.getTab());
    alreadyChangingTab = false;
    
    ui.setSelectorTab(ui.tabOmniSelector);
    
    chbBack.setDisable(viewList.canGoBack() == false);
    chbForward.setDisable(viewList.canGoForward() == false);
    
    viewList.refreshNavMenu(chbBack.getMenu(), false);
    viewList.refreshNavMenu(chbForward.getMenu(), true);
    
    ui.update();
    HDT_Base activeRecord = ui.activeRecord();
    
    if (curHyperTab.getTabEnum() != queryTab)
      if (activeRecord != null)
      {
        if (activeRecord.getType() == hdtPerson)
        {
          HDT_Person person = (HDT_Person) activeRecord;
          if (person.getLastName().length() > 0)
            ui.omniFocus();
        }
        else if (activeRecord.name().length() > 0)
          ui.omniFocus();
      }
    
    if (curHyperTab.getTabEnum() != workTab)
      bibManagerDlg.workRecordToAssign.set(null);
    
    if (curHyperTab.getTabEnum() == treeTab)
    {
      TreeTabController.class.cast(HyperTab.getHyperTab(treeTab)).selectRecord(curView.getViewRecord(), true);
    }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void init()
  {
    List<HDT_Base> initialNavList = db.getInitialNavList();
    int listSize = initialNavList.size();
    
    viewList.clear();
    
    for (int ndx = INITIAL_NAV_LIST_SIZE; ndx > 0; ndx--)
    {
      if ((listSize - ndx) >= 0)
      {
        HDT_Base record = initialNavList.get(listSize - ndx);
        
        if (record.getType() == hdtInvestigation)
          record = HDT_Investigation.class.cast(record).person.get();
        else if (record.getType() == hdtTerm)
          record = HDT_Term.class.cast(record).concepts.get(0);
        
        if (record != null)
        {
          HyperView<? extends HDT_Base> view;
          
          if (record.getType() == hdtWorkLabel)
            view = new HyperView<HDT_Base>(treeTab, record);
          else
            view = createViewForRecord(record);
          
          viewList.goForward(true);
          viewList.setView(view);
        }          
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <HDT_RT extends HDT_Base> HyperView<HDT_RT> createViewForRecord(HDT_RT record)
  {
    return new HyperView<HDT_RT>(getTabEnumByRecordType(record.getType()), record);
  }

  public static <HDT_RT extends HDT_Base> HyperView<HDT_RT> createViewForRecord(TabEnum tabEnum, HDT_RT record)
  {
    return new HyperView<HDT_RT>(tabEnum, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
