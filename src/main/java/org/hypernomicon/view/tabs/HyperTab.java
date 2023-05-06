/*
 * Copyright 2015-2023 Jason Winning
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
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Investigation;
import org.hypernomicon.model.records.RecordType;
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

  public abstract String recordName();
  public abstract void updateFromRecord();
  public abstract void clear();
  public abstract boolean saveToRecord();
  public abstract void setDividerPositions();
  public abstract void getDividerPositions();
  public abstract void setRecord(HDT_CT record);

  public TextViewInfo mainTextInfo()       { return nullSwitch(mainTextWrapper(), new TextViewInfo(), MainTextWrapper::getViewInfo); }
  public MainTextWrapper mainTextWrapper() { return null; }
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

  public static void forEachHyperTab(Consumer<HyperTab<? extends HDT_Record, ? extends HDT_Record>> a) { enumToHyperTab.values().forEach(a); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static boolean saveSearchKey(HDT_Record record, TextField tfSearchKey)
  {
    try
    {
      record.setSearchKey(tfSearchKey.getText());
    }
    catch (SearchKeyException e)
    {
      if (e.getTooShort())
        return falseWithErrorMessage("Unable to modify record: search key must be at least 3 characters.", tfSearchKey);

      return falseWithErrorMessage("Unable to modify record: search key already exists.", tfSearchKey);
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

  public static TabEnum getTabEnumByRecordType(RecordType recordType)
  {
    switch (recordType)
    {
      case hdtTerm        :
      case hdtConcept     : return termTabEnum;

      case hdtInstitution : return instTabEnum;
      case hdtDebate      : return debateTabEnum;
      case hdtPosition    : return positionTabEnum;
      case hdtArgument    : return argumentTabEnum;
      case hdtWork        : return workTabEnum;
      case hdtMiscFile    : return fileTabEnum;
      case hdtNote        : return noteTabEnum;
      case hdtWorkLabel   : return treeTabEnum;

      default             : return personTabEnum;
    }
  }

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
    setRecord(view.getViewRecord());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public HDT_RT activeRecord()
  {
    return (HDT_RT) getActiveRecordForViewRecord(viewRecord());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <ActiveType extends HDT_Record> ActiveType getActiveRecordForViewRecord(HDT_Record viewRecord)
  {
    if (viewRecord == null) return null;

    switch (viewRecord.getType())
    {
      case hdtConcept :

        return (ActiveType) ((HDT_Concept) viewRecord).term.get();

      case hdtInvestigation :

        return (ActiveType) ((HDT_Investigation) viewRecord).person.get();

      default :

        return (ActiveType) viewRecord;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Update record pointers after saving/reloading database

  public void refreshRecordPtr()
  {
    nullSwitch(getView(), thisView ->
    {
      thisView.refreshRecordPtr();
      nullSwitch(thisView.getViewRecord(), this::setRecord);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearTabAndView()
  {
    setView(new HyperView<>(tabEnum, null));
    clear();
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

  static void updateWebButtons(Preferences node, String prefKey, int numDef, Button btn, SplitMenuButton smb, String toolTipPrefix,
                               Function<String, EventHandler<ActionEvent>> eventHndlr)
  {
    int count = node.getInt(prefKey + "Count", numDef);

    if (count > numDef)
    {
      btn.setVisible(false);
      smb.setVisible(true);

      smb.setText(ui.webButtonMap.get(prefKey + '1').getCaption());
      setToolTip(smb, toolTipPrefix + smb.getText());

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
      setToolTip(btn, toolTipPrefix + btn.getText());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
