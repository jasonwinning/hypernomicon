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

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.MainController.FavMenuItem;
import org.hypernomicon.view.wrappers.HyperTableCell;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javafx.collections.ObservableList;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;

//---------------------------------------------------------------------------

public class HyperFavorites
{
  ObservableList<MenuItem> mainList, queryList;
  public static final int FIRST_FAV_MENU_ITEM_NDX = 4;
  
//---------------------------------------------------------------------------
  
  public static class QueryRow
  {
    public HyperTableCell[] cells = new HyperTableCell[6];
  }
  
//---------------------------------------------------------------------------
  
  public static class QueryFavorite
  {
    public ArrayList<QueryRow> rows = new ArrayList<QueryRow>();
    public String name;
    public boolean autoexec;
    
    public void removeFromList(ObservableList<MenuItem> items)
    {
      Iterator<MenuItem> it = items.iterator();
      
      while (it.hasNext())
      {
        MenuItem item = it.next();
        
        FavMenuItem fav = (FavMenuItem) item;
        if (fav.isQuery)
          if (fav.query == this)
          {
            it.remove();
            return;
          }
      }
    }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public HyperFavorites(Menu mnuFavorites, Menu mnuQueries)
  {
    mainList = mnuFavorites.getItems();
    queryList = mnuQueries.getItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void saveToPrefNode() throws BackingStoreException
  {
    Preferences node = db.prefs.node("favorites");
    int favNdx, rowNdx, colNdx, mainCount;
    HDT_RecordType type;
    String typeStr;
    FavMenuItem favItem;
    
    node.node("recordIDs").clear();
    node.node("recordTypes").clear();
    node.node("favTypes").clear();
    node.node("queries").removeNode();
    
    mainCount = mainList.size() - FIRST_FAV_MENU_ITEM_NDX;
    
    node.putInt("count", mainCount + queryList.size());
    
    for (favNdx = 0; favNdx < mainCount; favNdx++)
    {
      favItem = (FavMenuItem) mainList.get(favNdx + FIRST_FAV_MENU_ITEM_NDX);
      
      node.node("favTypes").put("" + favNdx, "record");
      HyperTableCell cell = favItem.favRecord;
      
      node.node("ids").putInt("" + favNdx, cell.getID());
      node.node("types").put("" + favNdx, db.getTypeTagStr(cell.getType()));
    }
    
    for (int queryNdx = 0; queryNdx < queryList.size(); queryNdx++)
    {
      favItem = (FavMenuItem) queryList.get(queryNdx);
      
      favNdx = mainCount + queryNdx;
      
      node.node("favTypes").put("" + favNdx, "query");
      QueryFavorite query = favItem.query;
      
      node.node("queries").node("query" + favNdx).put("name", query.name);
      node.node("queries").node("query" + favNdx).putInt("rowCount", query.rows.size());
      node.node("queries").node("query" + favNdx).putBoolean("autoexec", query.autoexec);
      
      for (rowNdx = 0; rowNdx < query.rows.size(); rowNdx++)
      {
        for (colNdx = 0; colNdx < 6; colNdx++)
        {
          HyperTableCell cell = query.rows.get(rowNdx).cells[colNdx];
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).putInt("id", getCellID(cell));
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).put("text", getCellText(cell));
          
          type = getCellType(cell);
          typeStr = (type == hdtNone) ? "all" : db.getTypeTagStr(type);
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).put("type", typeStr);
        }
      }        
    }
    
    node.flush();
  }  
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void loadFromPrefNode()
  {
    int count, id, ndx, rowNdx, colNdx, rowCount;
    HDT_RecordType type;
    HDT_Base record;
    Preferences node = db.prefs.node("favorites");
    QueryFavorite query;
    QueryRow row;
    String text, typeStr;
    
    clear();
    
    count = node.getInt("count", 0);
    
    for (ndx = 0; ndx < count; ndx++)
    {
      if (node.node("favTypes").get("" + ndx, "").equals("record"))
      {     
        id = node.node("ids").getInt("" + ndx, -1);
        type = db.parseTypeTagStr(node.node("types").get("" + ndx, ""));
        
        record = db.records(type).getByID(id);
        if (record != null) mainList.add(ui.new FavMenuItem(record));
      }
      else if (node.node("favTypes").get("" + ndx, "").equals("query"))
      {
        query = new QueryFavorite();
        
        query.name = node.node("queries").node("query" + ndx).get("name", "query" + ndx);
        rowCount = node.node("queries").node("query" + ndx).getInt("rowCount", 0);
        query.autoexec = node.node("queries").node("query" + ndx).getBoolean("autoexec", false);
  
        for (rowNdx = 0; rowNdx < rowCount; rowNdx++)
        {
          row = new QueryRow();
          
          for (colNdx = 0; colNdx < 6; colNdx++)
          {
            id = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).getInt("id", -1);
            text = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).get("text", "");
            
            typeStr = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).get("type", "all");
            if (typeStr.equals("all"))
              type = hdtNone;
            else
              type = db.parseTypeTagStr(typeStr);
            
            row.cells[colNdx] = new HyperTableCell(id, text, type);
          }
          
          query.rows.add(row);
        }
        
        queryList.add(ui.new FavMenuItem(query));
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    while (mainList.size() > FIRST_FAV_MENU_ITEM_NDX)
      mainList.remove(FIRST_FAV_MENU_ITEM_NDX);
    
    queryList.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int indexOfRecord(HDT_Base record)
  {
    for (int ndx = FIRST_FAV_MENU_ITEM_NDX; ndx < mainList.size(); ndx++)
    {
      FavMenuItem item = (FavMenuItem) mainList.get(ndx);
      
      if (item.isQuery == false)
      {
        if (getCellID(item.favRecord) == record.getID())
          if (getCellType(item.favRecord) == record.getType())
            return ndx;
      }
    }
    
    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void changeRecordID(HDT_RecordType changedType, int oldID, int newID)
  {
    for (int ndx = FIRST_FAV_MENU_ITEM_NDX; ndx < mainList.size(); ndx++)
    {
      FavMenuItem item = (FavMenuItem) mainList.get(ndx);
      
      if (item.isQuery == false)
      {
        if (getCellID(item.favRecord) == oldID)
          if (getCellType(item.favRecord) == changedType)
            item.favRecord = item.favRecord.getCopyWithID(newID);
      }
    }    
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
}
