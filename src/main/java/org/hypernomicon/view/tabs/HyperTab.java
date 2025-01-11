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

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.io.IOException;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.prefs.Preferences;

import org.hypernomicon.App;
import org.hypernomicon.model.records.*;
import org.hypernomicon.view.HyperView;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Exceptions.*;
import static org.hypernomicon.model.records.RecordType.*;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitMenuButton;
import javafx.scene.control.Tab;
import javafx.scene.control.TextField;

@SuppressWarnings("unused")
public abstract class HyperTab<HDT_RT extends HDT_Record, HDT_CT extends HDT_Record>
{
  public enum TabEnum
  {
    personTabEnum, instTabEnum, workTabEnum,  fileTabEnum, debateTabEnum, positionTabEnum, argumentTabEnum,
    noteTabEnum,   termTabEnum, queryTabEnum, treeTabEnum, omniTabEnum,   listTabEnum
  }

  private static final EnumMap<TabEnum, HyperTab<? extends HDT_Record, ? extends HDT_Record>> enumToHyperTab = new EnumMap<>(TabEnum.class);
  private static final Map<Tab, HyperTab<? extends HDT_Record, ? extends HDT_Record>> tabToHyperTab = new HashMap<>();

  private final Tab tab;
  private final TabEnum tabEnum;

  private HyperView<HDT_CT> view = null;
  private boolean useTextViewInfo = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected HyperTab(TabEnum tabEnum, Tab tab, String ctrlrFilename) throws IOException
  {
    FXMLLoader loader = new FXMLLoader(App.class.getResource(ctrlrFilename + ".fxml"), null, null, klass -> this);
    tab.setContent(loader.load());

    this.tab = tab;
    this.tabEnum = tabEnum;

    enumToHyperTab.put(tabEnum, this);
    tabToHyperTab.put(tab, this);

    if ((tabEnum != treeTabEnum) && (tabEnum != queryTabEnum))
      ui.addSelectorTab(tabEnum);
  }

//---------------------------------------------------------------------------

  protected abstract RecordType type();
  protected abstract void setRecord(HDT_CT record);
  protected abstract void updateFromRecord();

  public abstract String recordName();
  public abstract void clear(boolean resetRecord);
  public abstract boolean saveToRecord();
  public abstract void setDividerPositions();
  public abstract void getDividerPositions();

  public MainTextWrapper mainTextWrapper() { return null; }
  public boolean getUseTextViewInfo()      { return useTextViewInfo; }
  public void rescale()                    { return; }
  public int recordCount()                 { return db.records(type()).size(); }
  public final int activeID()              { return nullSwitch(activeRecord(), -1, HDT_Record::getID); }
  public int recordNdx()                   { return recordCount() < 1 ? -1 : nullSwitch(activeRecord(), -1, HDT_Record::keyNdx); }
  public final HyperView<HDT_CT> getView() { return view; }
  public HDT_CT viewRecord()               { return view.getViewRecord(); }
  public final Tab getTab()                { return tab; }
  public final TabEnum getTabEnum()        { return tabEnum; }
  public void enable(boolean enabled)      { getTab().getContent().setDisable(enabled == false); }
  void updateWebButtons(Preferences node)  { return; }

  public void nextSearchResult    ()       { nullSwitch(mainTextWrapper(), MainTextWrapper::nextSearchResult    ); }
  public void previousSearchResult()       { nullSwitch(mainTextWrapper(), MainTextWrapper::previousSearchResult); }

  public void findWithinDesc()             { mainTextWrapper().hilite(); }

  public void newClick(RecordType objType, HyperTableRow row) { }
  public TextViewInfo mainTextInfo(HDT_Record record)         { return nullSwitch(mainTextWrapper(), new TextViewInfo(record), mainText -> new TextViewInfo(mainText.getViewInfo(record))); }

  @SuppressWarnings("unchecked")
  public final HyperView<HDT_CT> newView(HDT_Record record)   { return new HyperView<>(getTabEnum(), (HDT_CT)record, mainTextInfo(record)); }

  public static void forEachHyperTab(Consumer<HyperTab<? extends HDT_Record, ? extends HDT_Record>> a) { enumToHyperTab.values().forEach(a); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void update(boolean updateRecord)
  {
    if (updateRecord)
    {
      useTextViewInfo = true;
      setRecord(view.getViewRecord());
    }

    updateFromRecord();
    useTextViewInfo = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static boolean saveSearchKey(HDT_Record record, TextField tfSearchKey)
  {
    try
    {
      record.setSearchKey(tfSearchKey.getText());
    }
    catch (SearchKeyTooShortException e)
    {
      return falseWithErrorPopup("Unable to modify record. Search key must be at least 3 characters: " + e.getKey(), tfSearchKey);
    }
    catch (DuplicateSearchKeyException e)
    {
      return falseWithErrorPopup("Unable to modify record. Search key already exists: " + e.getKey(), tfSearchKey);
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static RecordType getRecordTypeByTabEnum(TabEnum tabEnum)
  {
    return nullSwitch(enumToHyperTab.get(tabEnum), hdtNone, HyperTab::type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <HDT_RT extends HDT_Record,
                 HDT_CT extends HDT_Record,
                 HyperTabType extends HyperTab<HDT_RT, HDT_CT>>

    HyperTabType getHyperTab(TabEnum tabEnum) { return (HyperTabType) enumToHyperTab.get(tabEnum); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static TabEnum getTabEnumByRecordType(RecordType recordType) { return switch (recordType)
  {
    case hdtTerm,
         hdtConcept     -> termTabEnum;

    case hdtInstitution -> instTabEnum;
    case hdtDebate      -> debateTabEnum;
    case hdtPosition    -> positionTabEnum;
    case hdtArgument    -> argumentTabEnum;
    case hdtWork        -> workTabEnum;
    case hdtMiscFile    -> fileTabEnum;
    case hdtNote        -> noteTabEnum;
    case hdtWorkLabel   -> treeTabEnum;

    default             -> personTabEnum;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <HDT_CT extends HDT_Record> HyperTab<? extends HDT_Record, HDT_CT> saveViewToViewsTab(HyperView<HDT_CT> hyperView)
  {
    HyperTab<? extends HDT_Record, HDT_CT> hyperTab = hyperView.getHyperTab();
    hyperTab.setView(hyperView);
    return hyperTab;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HyperTab<? extends HDT_Record, ? extends HDT_Record> getHyperTabByTab(Tab tab)
  {
    return tabToHyperTab.get(tab);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setView(HyperView<HDT_CT> hyperView)
  {
    view = hyperView;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_RT activeRecord()
  {
    return getActiveRecordForViewRecord(viewRecord());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <ActiveType extends HDT_Record> ActiveType getActiveRecordForViewRecord(HDT_Record viewRecord)
  {
    return viewRecord == null ? null : (ActiveType) switch (viewRecord.getType())
    {
      case hdtConcept       -> ((HDT_Concept) viewRecord).term.get();
      case hdtInvestigation -> ((HDT_Investigation) viewRecord).person.get();

      default               -> viewRecord;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Update record pointers after saving/reloading database

  public void refreshRecordPtr()
  {
    nullSwitch(getView(), HyperView::refreshRecordPtr);

    nullSwitch(mainTextWrapper(), MainTextWrapper::refreshRecordPtr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearTabAndView()
  {
    setView(new HyperView<>(tabEnum, null));
    clear(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void clearAllTabsAndViews()
  {
    enumToHyperTab.values().forEach(HyperTab::clearTabAndView);
    previewWindow.clearAll();
    fileManagerDlg.clearHistory();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void updateAllWebButtons(Preferences node)
  {
    enumToHyperTab.values().forEach(hyperTab -> hyperTab.updateWebButtons(node));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void updateWebButtons(Preferences node, String prefKey, int numDef, Button btn, SplitMenuButton smb, Function<String, EventHandler<ActionEvent>> eventHndlr)
  {
    int count = node.getInt(prefKey + "Count", numDef);

    if (count > numDef)
    {
      btn.setVisible(false);
      smb.setVisible(true);

      smb.setText(ui.webButtonMap.get(prefKey + '1').getCaption());

      smb.getItems().clear();

      for (int ndx = numDef + 1; ndx <= count; ndx++)
      {
        String indexedPrefKey = prefKey + ndx;

        MenuItem item = new MenuItem(ui.webButtonMap.get(indexedPrefKey).getCaption());
        item.setOnAction(eventHndlr.apply(indexedPrefKey));
        smb.getItems().add(item);
      }
    }
    else
    {
      smb.setVisible(false);
      btn.setVisible(true);

      btn.setText(ui.webButtonMap.get(prefKey + '1').getCaption());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
