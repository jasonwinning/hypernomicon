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

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.hypernomicon.App;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.HyperView;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.tabs.HyperTab.TabEnum;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Exceptions.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import javafx.fxml.FXMLLoader;
import javafx.scene.control.SplitPane;
import javafx.scene.control.Tab;
import javafx.scene.control.TextField;

@SuppressWarnings("unused")
public abstract class HyperTab<HDT_RT extends HDT_Base, HDT_CT extends HDT_Base>
{
  public static enum TabEnum
  {
    personTab,   institutionTab,   workTab,   miscFileTab,   debateTab,   positionTab,   argumentTab, 
    noteTab,     termTab,          queryTab,  treeTab,       omniTab,     listTab
  }

  private static EnumMap<TabEnum, HyperTab<? extends HDT_Base, ? extends HDT_Base>> enumToHyperTab = new EnumMap<>(TabEnum.class);
  private static Map<Tab, HyperTab<? extends HDT_Base, ? extends HDT_Base>> tabToHyperTab = new HashMap<>();
  private Tab tab;
  private HyperView<HDT_CT> view = null;
  
  protected TabEnum tabEnum;
  
//---------------------------------------------------------------------------

  protected abstract void init(TabEnum tabEnum);
  
  public abstract boolean update();
  public abstract void clear();
  public abstract boolean saveToRecord(boolean showMessage);
  public abstract HDT_RecordType getType();
  public abstract void enable(boolean enabled);
  public abstract void focusOnSearchKey();
  public abstract void newClick(HDT_RecordType objType, HyperTableRow row);
  public abstract void setDividerPositions();
  public abstract void getDividerPositions();
  public abstract void setRecord(HDT_CT record);
 
  public void findWithinDesc(String text)     { messageDialog("Internal error #52009", mtError); }
  public TextViewInfo getMainTextInfo()       { return new TextViewInfo(); }
  public MainTextWrapper getMainTextWrapper() { return null; }
  public void rescale()                       { return; }
  public int getRecordCount()                 { return db.records(getRecordTypeByTabEnum(tabEnum)).size(); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final HyperView<HDT_CT> getView()    { return view; }
  public final HDT_CT viewRecord()            { return getView().getViewRecord(); }
  public final Tab getTab()                   { return tab; }
  public final TabEnum getTabEnum()           { return tabEnum; }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final Collection<HyperTab<? extends HDT_Base, ? extends HDT_Base>> getHyperTabs()
  {
    return Collections.unmodifiableCollection(enumToHyperTab.values());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
    
  public static <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> void addHyperTab(TabEnum tabEnum, Tab tab, String ctrlrFilename) throws IOException
  {
    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/tabs/" + ctrlrFilename));
    tab.setContent(loader.load());      
    HyperTab<HDT_RT, HDT_CT> hyperTab = loader.getController();
    
    baseInit(tabEnum, tab, hyperTab);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> void baseInit(TabEnum tabEnum, Tab tab, HyperTab<HDT_RT, HDT_CT> hyperTab)
  {
    hyperTab.tab = tab;
    hyperTab.init(tabEnum);
    
    enumToHyperTab.put(tabEnum, hyperTab);
    tabToHyperTab.put(tab, hyperTab);
    
    if ((tabEnum != treeTab) && (tabEnum != queryTab))
      ui.addSelectorTab(tabEnum);
  }
   
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> boolean saveSearchKey(HDT_Base record, TextField tfSearchKey, boolean showMessage, HyperTab<HDT_RT, HDT_CT> hyperTab)
  {
    try 
    {
      record.setSearchKey(tfSearchKey.getText());
    } 
    catch (SearchKeyException e)
    {
      if (showMessage)
      {
        if (e.getTooShort())
          messageDialog("Unable to modify record: search key must be at least 3 characters.", mtError);
        else
          messageDialog("Unable to modify record: search key already exists.", mtError);
      }
      
      hyperTab.focusOnSearchKey();
      return false;
    }
    
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final HDT_RecordType getRecordTypeByTabEnum(TabEnum tabEnum)
  {
    HyperTab<? extends HDT_Base, ? extends HDT_Base> hyperTab = enumToHyperTab.get(tabEnum);
    return hyperTab == null ? hdtNone : hyperTab.getType();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @SuppressWarnings("unchecked")
  public static final <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base, HyperTabType extends HyperTab<HDT_RT, HDT_CT>> HyperTabType getHyperTab(TabEnum tabEnum)
  {
    return (HyperTabType) enumToHyperTab.get(tabEnum);
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static final TabEnum getTabEnumByRecordType(HDT_RecordType recordType)
  {
    switch (recordType)
    {
      case hdtTerm : case hdtConcept : return termTab;
      
      case hdtInstitution : return institutionTab;
      case hdtDebate :      return debateTab;
      case hdtPosition :    return positionTab;
      case hdtArgument :    return argumentTab;
      case hdtWork :        return workTab;
      case hdtMiscFile :    return miscFileTab;
      case hdtNote :        return noteTab;
      
      default :             return personTab;
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static final <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> HyperTab<HDT_RT, HDT_CT> setTabView(HyperView<HDT_CT> hyperView)
  {
    HyperTab<HDT_RT, HDT_CT> hyperTab = getHyperTab(hyperView.getTabEnum());
    hyperTab.setView(hyperView);
    return hyperTab;    
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public static final HyperTab<? extends HDT_Base, ? extends HDT_Base> getHyperTabByTab(Tab tab)
  {
    return tabToHyperTab.get(tab);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private final void setView(HyperView<HDT_CT> hyperView)
  {
    this.view = hyperView;
    setRecord(view.getViewRecord());
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public HDT_RT activeRecord()
  {
    HDT_CT viewRecord = view.getViewRecord();
    
    if (viewRecord == null) return null;
    
    switch (viewRecord.getType())
    {
      case hdtConcept :
        return (HDT_RT) HDT_Concept.class.cast(viewRecord).term.get();
        
      default :
        return (HDT_RT) viewRecord;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final int getActiveID()
  {
    HDT_RT record = activeRecord();
    return (record == null) ? -1 : record.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getRecordNdx()
  {
    int count = getRecordCount();
    
    if ((count > 0) && (activeRecord() != null))
    {
      HDT_RT record = activeRecord();
      return db.records(record.getType()).getKeyNdxByID(record.getID());
    }        
    else
      return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
