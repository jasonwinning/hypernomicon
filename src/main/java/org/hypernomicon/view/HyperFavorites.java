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

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.MainCtrlr.FavMenuItem;
import org.hypernomicon.view.wrappers.HyperTableCell;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;

//---------------------------------------------------------------------------

public class HyperFavorites
{
  final List<MenuItem> mainList, queryList;
  public static final int FIRST_FAV_MENU_ITEM_NDX = 4;

//---------------------------------------------------------------------------

  public static class QueryRow
  {
    public final HyperTableCell[] cells = new HyperTableCell[6];
  }

//---------------------------------------------------------------------------

  public static class QueryFavorite
  {
    public final List<QueryRow> rows = new ArrayList<>();
    public String name;
    public boolean autoexec;

    public void removeFromList(List<MenuItem> items)
    {
      nullSwitch(items.stream().map(item -> (FavMenuItem)item).filter(fav -> fav.isQuery && (fav.query == this)).findFirst().orElse(null),
                 items::remove);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperFavorites(Menu mnuFavorites, Menu mnuQueries)
  {
    mainList = mnuFavorites.getItems();
    queryList = mnuQueries.getItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveToPrefNode() throws BackingStoreException
  {
    Preferences node = db.prefs.node("favorites");

    node.node("recordIDs").clear();
    node.node("recordTypes").clear();
    node.node("favTypes").clear();
    node.node("queries").removeNode();

    int mainCount = mainList.size() - FIRST_FAV_MENU_ITEM_NDX;

    node.putInt("count", mainCount + queryList.size());

    for (int favNdx = 0; favNdx < mainCount; favNdx++)
    {
      FavMenuItem favItem = (FavMenuItem) mainList.get(favNdx + FIRST_FAV_MENU_ITEM_NDX);

      node.node("favTypes").put(String.valueOf(favNdx), "record");
      HyperTableCell cell = favItem.favRecord;

      node.node("ids").putInt(String.valueOf(favNdx), cell.getID());
      node.node("types").put(String.valueOf(favNdx), db.getTypeTagStr(cell.getType()));
    }

    for (int queryNdx = 0; queryNdx < queryList.size(); queryNdx++)
    {
      FavMenuItem favItem = (FavMenuItem) queryList.get(queryNdx);

      int favNdx = mainCount + queryNdx;

      node.node("favTypes").put(String.valueOf(favNdx), "query");
      QueryFavorite query = favItem.query;

      node.node("queries").node("query" + favNdx).put("name", query.name);
      node.node("queries").node("query" + favNdx).putInt("rowCount", query.rows.size());
      node.node("queries").node("query" + favNdx).putBoolean("autoexec", query.autoexec);

      for (int rowNdx = 0; rowNdx < query.rows.size(); rowNdx++)
      {
        for (int colNdx = 0; colNdx < 6; colNdx++)
        {
          HyperTableCell cell = query.rows.get(rowNdx).cells[colNdx];
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).putInt("id", getCellID(cell));
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).put("text", getCellText(cell));

          RecordType type = getCellType(cell);
          String typeStr = type == hdtNone ? "all" : db.getTypeTagStr(type);
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
    clear();

    Preferences node = db.prefs.node("favorites");
    int count = node.getInt("count", 0);

    for (int ndx = 0; ndx < count; ndx++)
    {
      if (node.node("favTypes").get(String.valueOf(ndx), "").equals("record"))
      {
        int id = node.node("ids").getInt(String.valueOf(ndx), -1);
        RecordType type = db.parseTypeTagStr(node.node("types").get(String.valueOf(ndx), ""));

        nullSwitch((HDT_Record)db.records(type).getByID(id), record -> mainList.add(ui.new FavMenuItem(record)));
      }
      else if (node.node("favTypes").get(String.valueOf(ndx), "").equals("query"))
      {
        QueryFavorite query = new QueryFavorite();

        query.name = node.node("queries").node("query" + ndx).get("name", "query" + ndx);
        int rowCount = node.node("queries").node("query" + ndx).getInt("rowCount", 0);
        query.autoexec = node.node("queries").node("query" + ndx).getBoolean("autoexec", false);

        for (int rowNdx = 0; rowNdx < rowCount; rowNdx++)
        {
          QueryRow row = new QueryRow();

          for (int colNdx = 0; colNdx < 6; colNdx++)
          {
            int id = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).getInt("id", -1);
            String text    = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).get("text", ""),
                   typeStr = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).get("type", "all");

            row.cells[colNdx] = new HyperTableCell(id, text, typeStr.equals("all") ? hdtNone : db.parseTypeTagStr(typeStr));
          }

          query.rows.add(row);
        }

        queryList.add(ui.new FavMenuItem(query));
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    while (mainList.size() > FIRST_FAV_MENU_ITEM_NDX)
      mainList.remove(FIRST_FAV_MENU_ITEM_NDX);

    queryList.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int indexOfRecord(HDT_Record record)
  {
    for (int ndx = FIRST_FAV_MENU_ITEM_NDX; ndx < mainList.size(); ndx++)
    {
      FavMenuItem item = (FavMenuItem) mainList.get(ndx);

      if (item.isQuery == false)
        if (getCellID(item.favRecord) == record.getID())
          if (getCellType(item.favRecord) == record.getType())
            return ndx;
    }

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void changeRecordID(RecordType changedType, int oldID, int newID)
  {
    for (int ndx = FIRST_FAV_MENU_ITEM_NDX; ndx < mainList.size(); ndx++)
    {
      FavMenuItem item = (FavMenuItem) mainList.get(ndx);

      if (item.isQuery == false)
        if (getCellID(item.favRecord) == oldID)
          if (getCellType(item.favRecord) == changedType)
            item.favRecord = item.favRecord.getCopyWithID(newID);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
