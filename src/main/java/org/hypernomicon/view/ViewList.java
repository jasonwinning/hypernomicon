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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.tabs.HyperTab.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Work;
import javafx.collections.ObservableList;
import javafx.scene.control.MenuItem;

public class ViewList
{
  private int curNdx;
  private List<HyperView<? extends HDT_Base>> viewList;
  private HyperViewSequence hvs;

  public ViewList(HyperViewSequence hvs)                   
  { 
    this.hvs = hvs;
    clear(); 
  }
  
  public boolean canGoBack()                     { return curNdx >= 1; }
  public boolean canGoForward()                  { return curNdx < (viewList.size() - 1); }
  public boolean isEmpty()                       { return viewList.isEmpty(); }
  public void clear()                            { viewList = new ArrayList<HyperView<? extends HDT_Base>>(); curNdx = -1; }
  public void goBack()                           { curNdx--; if (curNdx < 0) curNdx = 0; }
  public HyperView<? extends HDT_Base> getView() { return viewList.get(curNdx); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goForward(boolean canAdd)             
  { 
    curNdx++;
    
    if (canAdd == false)
    {
      if (curNdx >= viewList.size()) 
        curNdx = viewList.size() - 1;
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private boolean addMenuItem(ObservableList<MenuItem> menu, int ndx)
  {
    MenuItem item = getMenuItemForNavNdx(ndx);
    
    if (item != null)
      menu.add(0, item);
    
    return menu.size() == 20;
  }

  //---------------------------------------------------------------------------  
  //---------------------------------------------------------------------------   

  void refreshNavMenu(ObservableList<MenuItem> menu, boolean isForward)
  {      
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

  private MenuItem getMenuItemForNavNdx(int ndx)
  {
    MenuItem item;
    String typeName;

    HyperView<? extends HDT_Base> view = viewList.get(ndx);
    
    HDT_Base record = view.getViewRecord();
    
    if (record == null)
    {
      item = new MenuItem("(" + getHyperTab(view.getTabEnum()).getTab().getText() + " tab)");
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
      
      typeName = db.getTypeName(record.getType());
      
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
      
      hvs.saveViewToSequence(false);
      curNdx = ndx;
      hvs.update();
    });
    
    return item;
  }

  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setView(HyperView<? extends HDT_Base> view)     
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

  public void refreshAll()
  {
    for (HyperView<? extends HDT_Base> hyperView : viewList)
      hyperView.refresh();
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clearFollowingViews()
  {
    while (viewList.size() > (curNdx + 1))
      viewList.remove(curNdx + 1);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeRecord(HDT_Base record)
  {
    int count = viewList.size();
    Iterator<HyperView<? extends HDT_Base>> it = viewList.iterator();
    
    while (it.hasNext() && (count > 1))
    {
      HyperView<? extends HDT_Base> view = it.next();
      
      if (view.getViewRecord() == record)
      {
        if (curNdx >= viewList.indexOf(view)) curNdx--;
        it.remove();
        count--;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
}
