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

package org.hypernomicon.view;

import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import java.util.ArrayList;
import java.util.List;
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;

//---------------------------------------------------------------------------

public class HyperFavorites
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class FavMenuItem extends MenuItem
  {
    public FavMenuItem(HDT_Record record)
    {
      super(getRecordText(record));
      isQuery = false;
      favRecord = new HyperTableCell(record, record.getCBText());
      query = null;
      setOnAction(event -> ui.goToRecord(record, true));
    }

    public FavMenuItem(QueryFavorite query)
    {
      super("Query: " + query.name);
      isQuery = true;
      this.query = query;
      favRecord = null;
      setOnAction(event -> ui.showSearch(query.autoexec, null, -1, query, null, null, query.name));
    }

  //---------------------------------------------------------------------------

    final private boolean isQuery;
    final private QueryFavorite query;
    private HyperTableCell favRecord;

  //---------------------------------------------------------------------------

    private static String getRecordText(HDT_Record record)
    {
      return getTypeName(record.getType()) + ": " + record.getCBText();
    }

  //---------------------------------------------------------------------------

    public void update()
    {
      if (isQuery) return;

      HDT_Record record = getRecord(favRecord);

      if (favRecord != null)
        setText(getRecordText(record));
    }
  }

//---------------------------------------------------------------------------

  public static class QueryRow
  {
    public final HyperTableCell[] cells = new HyperTableCell[QUERY_FAV_ROW_COLUMN_COUNT];
  }

//---------------------------------------------------------------------------

  public static class QueryFavorite
  {
    public final List<QueryRow> rows = new ArrayList<>();
    public String name, customLogic;
    public boolean autoexec, orLogic;

    public void removeFromList(List<MenuItem> items)
    {
      nullSwitch(items.stream().map(item -> (FavMenuItem)item).filter(fav -> fav.isQuery && (fav.query == this)).findFirst().orElse(null),
                 items::remove);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<MenuItem> mainList, queryList;
  private static final int QUERY_FAV_ROW_COLUMN_COUNT = 5;
  public static final int FIRST_FAV_MENU_ITEM_NDX = 4;

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
      node.node("types").put(String.valueOf(favNdx), Tag.getTypeTagStr(cell.type));
    }

    for (int queryNdx = 0; queryNdx < queryList.size(); queryNdx++)
    {
      FavMenuItem favItem = (FavMenuItem) queryList.get(queryNdx);

      int favNdx = mainCount + queryNdx;

      node.node("favTypes").put(String.valueOf(favNdx), "query");
      QueryFavorite query = favItem.query;

      node.node("queries").node("query" + favNdx).put("name", query.name);
      node.node("queries").node("query" + favNdx).put("customLogic", query.customLogic);
      node.node("queries").node("query" + favNdx).putInt("rowCount", query.rows.size());
      node.node("queries").node("query" + favNdx).putBoolean("autoexec", query.autoexec);
      node.node("queries").node("query" + favNdx).putBoolean("orLogic", query.orLogic);

      for (int rowNdx = 0; rowNdx < query.rows.size(); rowNdx++)
      {
        for (int colNdx = 0; colNdx < QUERY_FAV_ROW_COLUMN_COUNT; colNdx++)
        {
          HyperTableCell cell = query.rows.get(rowNdx).cells[colNdx];
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).putInt("id", getCellID(cell));
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).put("text", getCellText(cell));

          RecordType type = getCellType(cell);
          String typeStr = type == hdtNone ? "all" : Tag.getTypeTagStr(type);
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).put("type", typeStr);
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void loadFromPrefNode()
  {
    clear();

    boolean showedMessage = false;

    Preferences node = db.prefs.node("favorites");

    for (int count = node.getInt("count", 0), ndx = 0; ndx < count; ndx++)
    {
      if ("record".equals(node.node("favTypes").get(String.valueOf(ndx), "")))
      {
        int id = node.node("ids").getInt(String.valueOf(ndx), -1);
        RecordType type = Tag.parseTypeTagStr(node.node("types").get(String.valueOf(ndx), ""));

        nullSwitch((HDT_Record)db.records(type).getByID(id), record -> mainList.add(new FavMenuItem(record)));
      }
      else if ("query".equals(node.node("favTypes").get(String.valueOf(ndx), "")))
      {
        QueryFavorite query = new QueryFavorite();

        query.name = node.node("queries").node("query" + ndx).get("name", "query" + ndx);
        query.customLogic = node.node("queries").node("query" + ndx).get("customLogic", "");

        int rowCount = node.node("queries").node("query" + ndx).getInt("rowCount", 0);
        query.autoexec = node.node("queries").node("query" + ndx).getBoolean("autoexec", false);
        query.orLogic = node.node("queries").node("query" + ndx).getBoolean("orLogic", false);

        for (int rowNdx = 0; rowNdx < rowCount; rowNdx++)
        {
          QueryRow row = new QueryRow();

          // This try block is for backwards compatibility (displaying a warning message at least) with v1.24.2
          try
          {
            if ((rowCount > 1) && (showedMessage == false) && node.node("queries").node("query" + ndx).node("row" + rowNdx).nodeExists("col" + QUERY_FAV_ROW_COLUMN_COUNT))
            {
              messageDialog("You may need to review the logic settings for one or more query favorites. The logic settings may not have loaded properly because they were saved in an older format.", mtWarning);
              showedMessage = true;
            }
          }
          catch (BackingStoreException e) { noOp(); }

          for (int colNdx = 0; colNdx < QUERY_FAV_ROW_COLUMN_COUNT; colNdx++)
          {
            int id = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).getInt("id", -1);
            String text    = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).get("text", ""),
                   typeStr = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).get("type", "all");

            row.cells[colNdx] = new HyperTableCell(id, text, "all".equals(typeStr) ? hdtNone : Tag.parseTypeTagStr(typeStr));
          }

          query.rows.add(row);
        }

        queryList.add(new FavMenuItem(query));
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

      if (item.isQuery) continue;

      if (getCellID(item.favRecord) == oldID)
        if (getCellType(item.favRecord) == changedType)
          item.favRecord = item.favRecord.getCopyWithID(newID);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateItems()
  {
    for (int ndx = FIRST_FAV_MENU_ITEM_NDX; ndx < mainList.size(); ndx++)
    {
      FavMenuItem item = (FavMenuItem) mainList.get(ndx);

      item.update();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
