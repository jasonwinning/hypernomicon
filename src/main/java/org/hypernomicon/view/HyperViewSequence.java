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
import org.hypernomicon.view.tabs.HyperTab.TabEnum;
import org.hypernomicon.view.wrappers.ClickHoldButton;

import com.google.common.collect.Iterators;

import static org.hypernomicon.App.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static org.hypernomicon.view.tabs.HyperTab.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import javafx.collections.ObservableList;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TabPane;

public class HyperViewSequence
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int curNdx = -1;
  private final List<HyperView<? extends HDT_Base>> viewList = new ArrayList<>();
  private final TabPane tabPane;
  private boolean alreadyChangingTab = false;
  private final ClickHoldButton chbBack, chbForward;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperViewSequence(TabPane tabPane, ClickHoldButton chbForward, ClickHoldButton chbBack)
  {
    this.tabPane = tabPane;
    this.chbForward = chbForward;
    this.chbBack = chbBack;

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

      HyperTab<? extends HDT_Base, ? extends HDT_Base> hyperTab = getHyperTabByTab(newTab);

      if (hyperTab.getTabEnum() != workTab)
        bibManagerDlg.workRecordToAssign.set(null);

      forwardToNewSlotAndView(hyperTab.getView());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperView<? extends HDT_Base> curHyperView()                      { return viewList.get(curNdx); }
  HyperTab<? extends HDT_Base, ? extends HDT_Base> curHyperTab()    { return curHyperView().getHyperTab(); }
  TabEnum curTabEnum()                                              { return curHyperView().getTabEnum();  }
  public void updateCurrentView(HyperView<? extends HDT_Base> view) { setView(view); setTabView(view); }
  boolean isEmpty()                                                 { return viewList.isEmpty(); }
  void refreshAll()                                                 { viewList.forEach(HyperView::refresh); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void saveViewToSequence(boolean okToInsert)
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

  void stepForward()
  {
    saveViewToSequence(false);
    goForward(false);
    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void stepBack()
  {
    saveViewToSequence(false);

    curNdx--;
    if (curNdx < 0)
      curNdx = 0;

    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void activateCurrentView()
  {
    if (viewList.isEmpty())
    {
      forwardToNewSlotAndView(getHyperTabByTab(tabPane.getSelectionModel().getSelectedItem()).getView());
      return;
    }

    saveViewToSequence(true);
    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void forwardToNewSlotAndView(HyperView<? extends HDT_Base> hyperView)
  {
    saveViewToSequence(false);
    setTabView(hyperView);

    boolean advance = true;

    if (viewList.isEmpty() == false)
    {
      HyperView<? extends HDT_Base> view = curHyperView();

      if ((view.getTabEnum() != queryTab) && (view.getTabEnum() != treeTab) && (view.getViewRecord() == null))
        advance = false;
    }

    while (viewList.size() > (curNdx + 1))
      viewList.remove(curNdx + 1);

    if (advance)
      goForward(true);

    setView(hyperView.getHyperTab().getView());

    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void update()
  {
    HyperView<? extends HDT_Base> curView = curHyperView();
    HyperTab<? extends HDT_Base, ? extends HDT_Base> curHyperTab = setTabView(curView);

    alreadyChangingTab = true;
    tabPane.getSelectionModel().select(curHyperTab.getTab());
    alreadyChangingTab = false;

    ui.setSelectorTab(ui.tabOmniSelector);

    chbBack.setDisable(curNdx < 1);
    chbForward.setDisable(curNdx >= (viewList.size() - 1));

    refreshNavMenu(chbBack.getMenu(), false);
    refreshNavMenu(chbForward.getMenu(), true);

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
      TreeTabController.class.cast(getHyperTab(treeTab)).selectRecord(curView.getViewRecord(), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void init(TabEnum activeTabEnum)
  {
    viewList.clear();
    curNdx = -1;

    db.getInitialNavList().forEach(record ->
    {
      if (record.getType() == hdtInvestigation)
        record = HDT_Investigation.class.cast(record).person.get();
      else if (record.getType() == hdtTerm)
        record = HDT_Term.class.cast(record).concepts.get(0);

      if (record == null) return;

      goForward(true);
      setView(new HyperView<>(record));
    });

    forwardToNewSlotAndView(getHyperTab(activeTabEnum).getView());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void goForward(boolean canAdd)
  {
    curNdx++;

    if ((canAdd == false) && (curNdx >= viewList.size()))
      curNdx = viewList.size() - 1;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void refreshNavMenu(ObservableList<MenuItem> menu, boolean isForward)
  {
    if (db.isLoaded() == false) return;

    menu.clear();

    if (isForward)
    {
      for (int ndx = curNdx + 1; ndx < viewList.size(); ndx++)
        if (addMenuItem(menu, ndx)) return;
    }
    else
    {
      for (int ndx = curNdx - 1; ndx >= 0; ndx--)
        if (addMenuItem(menu, ndx)) return;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean addMenuItem(ObservableList<MenuItem> menu, int ndx)
  {
    MenuItem item;
    HyperView<? extends HDT_Base> view = viewList.get(ndx);
    HDT_Base record = view.getViewRecord();

    if (record == null)
    {
      item = new MenuItem("(" + view.getHyperTab().getTab().getText() + " tab)");
    }
    else
    {
      String beforePart = "";

      switch (view.getTabEnum())
      {
        case queryTab : beforePart = "(Queries tab) "; break;
        case treeTab  : beforePart = "(Tree tab) "; break;
        default       : break;
      }

      String typeName = db.getTypeName(record.getType());

      if (record.getType() == hdtWork)
      {
        HDT_Work work = (HDT_Work) record;
        if (work.workType.isNotNull())
          typeName = work.workType.get().listName();
      }
      item = new MenuItem(beforePart + typeName + ": " + record.getCBText());
    }

    item.setOnAction(event ->
    {
      if (ui.cantSaveRecord(true)) return;

      saveViewToSequence(false);
      curNdx = ndx;
      update();
    });

    menu.add(0, item);
    return menu.size() == 20;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setView(HyperView<? extends HDT_Base> view)
  {
    if (curNdx == -1) curNdx = 0;

    if (curNdx == viewList.size())
      viewList.add(view);
    else
      viewList.set(curNdx, view);

    // This next part prevents duplicate adjacent entries

    Iterator<HyperView<? extends HDT_Base>> it = viewList.iterator();
    HyperView<? extends HDT_Base> lastView = null;

    int ndx = 0;
    while (it.hasNext())
    {
      view = it.next();

      if (lastView != null)
      {
        if (view.getTabEnum() == lastView.getTabEnum())
          if (view.getViewRecord() == lastView.getViewRecord())
          {
            it.remove();
            if (ndx <= curNdx) curNdx--;
            view = null;
          }
      }

      if (view != null)
      {
        lastView = view;
        ndx++;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void removeRecord(HDT_Base record)
  {
    // Do not change the following code to use ArrayList.removeIf. The line that checks whether curNdx should be decremented will not work
    // because the ArrayList does not actually get modified until all of the removeIf checks are completed.

    Iterators.removeIf(viewList.iterator(), view ->
    {
      if (view.getViewRecord() != record) return false;

      if (curNdx >= viewList.indexOf(view)) curNdx--;
      return true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
