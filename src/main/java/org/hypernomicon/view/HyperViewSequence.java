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

package org.hypernomicon.view;

import org.hypernomicon.model.records.*;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.HyperTab.TabEnum;
import org.hypernomicon.view.wrappers.ClickHoldButton;

import com.google.common.collect.Iterators;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import static org.hypernomicon.view.tabs.HyperTab.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import javafx.scene.control.MenuItem;
import javafx.scene.control.TabPane;
import javafx.scene.input.KeyEvent;

public class HyperViewSequence
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int curNdx = -1;
  private long lastArrowKey = 0;
  private final List<HyperView<? extends HDT_Record>> slots = new ArrayList<>();
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

    tabPane.addEventFilter(KeyEvent.ANY, event ->
    {
      if (event.getCode().isArrowKey())
        lastArrowKey = Instant.now().toEpochMilli();
    });
    
    tabPane.getSelectionModel().selectedItemProperty().addListener((ob, oldTab, newTab) ->
    {
      if ((db.isLoaded() == false) || alreadyChangingTab) return;

      if (((Instant.now().toEpochMilli() - lastArrowKey) < IGNORE_ARROW_KEYS_IN_TAB_PANE_MS) || ui.cantSaveRecord()) // Ignore arrow keys
      {
        alreadyChangingTab = true;
        tabPane.getSelectionModel().select(oldTab);
        alreadyChangingTab = false;

        return;
      }

      HyperTab<? extends HDT_Record, ? extends HDT_Record> hyperTab = getHyperTabByTab(newTab);

      if (hyperTab.getTabEnum() != workTabEnum)
        bibManagerDlg.workRecordToAssign.set(null);

      saveViewFromUItoSlotAdvanceCursorAndLoadNewViewToUI(hyperTab.getView());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperView<? extends HDT_Record> getViewInCurrentSlot()                        { return slots.get(curNdx); }
  HyperTab<? extends HDT_Record, ? extends HDT_Record> tabOfViewInCurrentSlot() { return getViewInCurrentSlot().getHyperTab(); }
  TabEnum tabEnumOfViewInCurrentSlot()                                          { return getViewInCurrentSlot().getTabEnum();  }
  boolean isEmpty()                                                             { return slots.isEmpty(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refreshAll()
  {
    // Update record pointers after saving/reloading database
    slots.forEach(HyperView::refresh);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveViewToCurrentSlotAndTab(HyperView<? extends HDT_Record> view)
  {
    // Save the view to the currently active slot, and delete duplicate adjacent entries
    saveViewToCurrentSlot(view);

    // Set the tab's current view
    saveViewToViewsTab(view);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Get updated View object from the Tab and store it in current slot

  private void saveViewFromUItoCurrentSlotAndTab(boolean okToAddSlotIfNeeded)
  {
    if (slots.isEmpty() && (okToAddSlotIfNeeded == false)) return;

    HyperTab<? extends HDT_Record, ? extends HDT_Record> hyperTab = tabOfViewInCurrentSlot();

    HDT_Record record = hyperTab.activeRecord();
    if ((record != null) && HDT_Record.isEmpty(record)) // Make sure active record was not just deleted
      return;

    record = hyperTab.viewRecord();                     // Make sure view record was not just deleted
    if ((record != null) && HDT_Record.isEmpty(record)) // If concept was just deleted, active record (term) will be null
      return;                                           // so we also have to check view record (concept)

    saveViewToCurrentSlotAndTab(new HyperView<>(tabEnumOfViewInCurrentSlot(), record, hyperTab.mainTextInfo()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void stepForward()
  {
    saveViewFromUItoCurrentSlotAndTab(false);
    goForward(false);
    showCurrentViewInUI();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void stepBack()
  {
    saveViewFromUItoCurrentSlotAndTab(false);

    if (--curNdx < 0)
      curNdx = 0;

    showCurrentViewInUI();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void loadViewFromCurrentSlotToUI()
  {
    if (slots.isEmpty())
    {
      saveViewFromUItoSlotAdvanceCursorAndLoadNewViewToUI(getHyperTabByTab(tabPane.getSelectionModel().getSelectedItem()).getView());
      return;
    }

    showCurrentViewInUI();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void saveViewFromUItoSlotAdvanceCursorAndLoadNewViewToUI(HyperView<? extends HDT_Record> hyperView)
  {
    // Save the view from UI to the current slot in the sequence (unless there is no current slot),
    saveViewFromUItoCurrentSlotAndTab(false);

    // Determine whether we were already on a record tab that previously had a record
    boolean advance = true;

    if (slots.isEmpty() == false)
    {
      HyperView<? extends HDT_Record> view = getViewInCurrentSlot();

      if ((view.getTabEnum() != queryTabEnum) && (view.getTabEnum() != treeTabEnum) && (view.getViewRecord() == null))
        advance = false;
    }

    // delete any later slots
    while (slots.size() > (curNdx + 1))
      slots.remove(curNdx + 1);

    // advance the slot cursor (if we were already on a record tab that previously had a record)
    if (advance)
      goForward(true); // This only changes curNdx

    // Save the new view to the current slot
    saveViewToCurrentSlot(hyperView);

    // Load the new view to the UI
    showCurrentViewInUI();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showCurrentViewInUI()
  {
    HyperView<? extends HDT_Record> curView = getViewInCurrentSlot();
    HyperTab<? extends HDT_Record, ? extends HDT_Record> curHyperTab = saveViewToViewsTab(curView);

    alreadyChangingTab = true;
    tabPane.getSelectionModel().select(curHyperTab.getTab());
    alreadyChangingTab = false;

    ui.setSelectorTab(ui.tabOmniSelector);

    chbBack.setDisable(curNdx < 1);
    chbForward.setDisable(curNdx >= (slots.size() - 1));

    rebuildNavMenu(chbBack.getMenu(), false);
    rebuildNavMenu(chbForward.getMenu(), true);

    ui.update();

    if (curHyperTab.getTabEnum() != queryTabEnum) nullSwitch(ui.activeRecord(), activeRecord ->
    {
      if (activeRecord.getType() == hdtPerson)
      {
        if (((HDT_Person)activeRecord).getLastName().length() > 0)
          ui.omniFocus();
      }
      else if (activeRecord.name().length() > 0)
        ui.omniFocus();
    });

    if (curHyperTab.getTabEnum() != workTabEnum)
      bibManagerDlg.workRecordToAssign.set(null);

    if (curHyperTab.getTabEnum() == treeTabEnum)
      ui.treeHyperTab().selectRecord(curView.getViewRecord(), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void init(TabEnum activeTabEnum)
  {
    slots.clear();
    curNdx = -1;

    db.getInitialNavList().forEach(record ->
    {
      if (record.getType() == hdtInvestigation)
        record = ((HDT_Investigation)record).person.get();
      else if (record.getType() == hdtTerm)
        record = ((HDT_Term)record).concepts.get(0);

      if (record == null) return;

      goForward(true);
      saveViewToCurrentSlot(new HyperView<>(record));
    });

    saveViewFromUItoSlotAdvanceCursorAndLoadNewViewToUI(getHyperTab(activeTabEnum).getView());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void goForward(boolean okToAddSlotIfNeeded)
  {
    curNdx++;

    if ((okToAddSlotIfNeeded == false) && (curNdx >= slots.size()))
      curNdx = slots.size() - 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Save the passed-in view to the currently active slot, and then delete duplicate adjacent entries

  private void saveViewToCurrentSlot(HyperView<? extends HDT_Record> view)
  {
    if (curNdx == -1) curNdx = 0;

    if (curNdx == slots.size())
      slots.add(view);
    else
      slots.set(curNdx, view);

    // This next part prevents duplicate adjacent entries

    Iterator<HyperView<? extends HDT_Record>> it = slots.iterator();
    HyperView<? extends HDT_Record> lastView = null;

    int ndx = 0;
    while (it.hasNext())
    {
      view = it.next();

      if ((lastView != null) && (view.getTabEnum() == lastView.getTabEnum()) && (view.getViewRecord() == lastView.getViewRecord()))
      {
        it.remove();
        if (ndx <= curNdx) curNdx--;
        view = null;
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

  private void rebuildNavMenu(List<MenuItem> menu, boolean isForward)
  {
    if (db.isLoaded() == false) return;

    menu.clear();

    if (isForward)
    {
      for (int ndx = curNdx + 1; ndx < slots.size(); ndx++)
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

  private boolean addMenuItem(List<MenuItem> menu, int ndx)
  {
    MenuItem item;
    HyperView<? extends HDT_Record> view = slots.get(ndx);
    HDT_Record record = view.getViewRecord();

    if (record == null)
    {
      item = new MenuItem("(" + view.getHyperTab().getTab().getText() + " tab)");
    }
    else
    {
      String beforePart = "";

      switch (view.getTabEnum())
      {
        case queryTabEnum : beforePart = "(Queries tab) "; break;
        case treeTabEnum  : beforePart = "(Tree tab) ";    break;
        default           :                                break;
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
      if (ui.cantSaveRecord()) return;

      saveViewFromUItoCurrentSlotAndTab(false);
      curNdx = ndx;
      showCurrentViewInUI();
    });

    menu.add(0, item);
    return menu.size() == 20;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void removeRecord(HDT_Record record)
  {
    // Do not change the following code to use ArrayList.removeIf. The line that checks whether curNdx should be decremented will not work
    // because the ArrayList does not actually get modified until all of the removeIf checks are completed.

    Iterators.removeIf(slots.iterator(), view ->
    {
      if (view.getViewRecord() != record) return false;

      if (curNdx >= slots.indexOf(view)) curNdx--;
      return true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
