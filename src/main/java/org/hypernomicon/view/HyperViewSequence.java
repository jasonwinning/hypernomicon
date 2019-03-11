/*
 * Copyright 2015-2019 Jason Winning
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

import org.hypernomicon.model.records.*;

import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.TreeTabController;
import org.hypernomicon.view.wrappers.ClickHoldButton;

import static org.hypernomicon.App.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.tabs.HyperTab.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import javafx.scene.control.TabPane;

public class HyperViewSequence
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final TabPane tabPane;
  private final ViewList viewList;
  private boolean alreadyChangingTab = false;
  private final ClickHoldButton chbBack, chbForward;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperViewSequence(TabPane tabPane, ClickHoldButton chbForward, ClickHoldButton chbBack)
  {
    this.tabPane = tabPane;
    this.chbForward = chbForward;
    this.chbBack = chbBack;
    viewList = new ViewList(this);

    tabPane.getSelectionModel().selectedItemProperty().addListener((observable, oldTab, newTab) ->
    {
      if ((db.isLoaded() == false) || alreadyChangingTab) return;

      if (ui.cantSaveRecord(true))
      {
        alreadyChangingTab = true;
        tabPane.getSelectionModel().select(oldTab);
        alreadyChangingTab = false;

        return;
      }

      TabEnum tabEnum = getHyperTabByTab(newTab).getTabEnum();

      if (tabEnum != TabEnum.workTab)
        bibManagerDlg.workRecordToAssign.set(null);

      forwardToNewSlot(tabEnum);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperTab<? extends HDT_Base, ? extends HDT_Base> curHyperTab()    { return getHyperTab(curTabEnum()); }
  HyperView<? extends HDT_Base> curHyperView()                      { return viewList.getView(); }
  boolean isEmpty()                                                 { return viewList.isEmpty(); }
  HyperTab.TabEnum curTabEnum()                                     { return curHyperView().getTabEnum();  }
  public void updateCurrentView(HyperView<? extends HDT_Base> view) { viewList.setView(view); setTabView(view); }
  void stepForward()                                                { saveViewToSequence(false); viewList.goForward(false); update(); }
  void stepBack()                                                   { saveViewToSequence(false); viewList.goBack(); update(); }
  void removeRecord(HDT_Base record)                                { viewList.removeRecord(record); }
  void refresh()                                                    { viewList.refreshAll(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void activateCurrentSlot()
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

  void forwardToNewSlot(TabEnum tabEnum)
  {
    forwardToNewSlotAndView(getHyperTab(tabEnum).getView());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void forwardToNewSlotAndView(HyperView<? extends HDT_Base> hyperView)
  {
    saveViewToSequence(false);
    HyperTab.setTabView(hyperView);

    boolean advance = true;

    if (viewList.isEmpty() == false)
    {
      HyperView<? extends HDT_Base> view = viewList.getView();

      if ((view.getTabEnum() != queryTab) && (view.getTabEnum() != treeTab) && (view.getViewRecord() == null))
        advance = false;
    }

    viewList.clearFollowingViews();

    if (advance)
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
    if ((record != null) && HDT_Record.isEmpty(record)) // Make sure active record was not just deleted
      return;

    record = hyperTab.viewRecord();                     // Make sure view record was not just deleted
    if ((record != null) && HDT_Record.isEmpty(record)) // If concept was just deleted, active record (term) will be null
      return;                                           // so we also have to check view record (concept)

    updateCurrentView(new HyperView<>(curTabEnum(), record, hyperTab.getMainTextInfo()));
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

    if (curHyperTab.getTabEnum() != queryTab) nullSwitch(ui.activeRecord(), activeRecord ->
    {
      if (activeRecord.getType() == hdtPerson)
      {
        if (HDT_Person.class.cast(activeRecord).getLastName().length() > 0)
          ui.omniFocus();
      }
      else if (activeRecord.name().length() > 0)
        ui.omniFocus();
    });

    if (curHyperTab.getTabEnum() != workTab)
      bibManagerDlg.workRecordToAssign.set(null);

    if (curHyperTab.getTabEnum() == treeTab)
      TreeTabController.class.cast(HyperTab.getHyperTab(treeTab)).selectRecord(curView.getViewRecord(), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void init()
  {
    viewList.clear();

    db.getInitialNavList().forEach(record ->
    {
      if (record.getType() == hdtInvestigation)
        record = HDT_Investigation.class.cast(record).person.get();
      else if (record.getType() == hdtTerm)
        record = HDT_Term.class.cast(record).concepts.get(0);

      if (record == null) return;

      HyperView<? extends HDT_Base> view = record.getType() == hdtWorkLabel ? new HyperView<>(treeTab, record) : createViewForRecord(record);

      viewList.goForward(true);
      viewList.setView(view);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static <HDT_RT extends HDT_Base> HyperView<HDT_RT> createViewForRecord(HDT_RT record)
  {
    return createViewForRecord(getTabEnumByRecordType(record.getType()), record);
  }

  static <HDT_RT extends HDT_Base> HyperView<HDT_RT> createViewForRecord(TabEnum tabEnum, HDT_RT record)
  {
    return new HyperView<>(tabEnum, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
