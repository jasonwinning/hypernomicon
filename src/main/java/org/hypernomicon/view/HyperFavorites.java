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

  public static class RecordFavorite extends MenuItem
  {
    public RecordFavorite(HDT_Record record)
    {
      super(getRecordText(record));
      this.record = record;
      setOnAction(event -> ui.goToRecord(record, true));
    }

  //---------------------------------------------------------------------------

    private final HDT_Record record;

  //---------------------------------------------------------------------------

    private static String getRecordText(HDT_Record record)
    {
      return getTypeName(record.getType()) + ": " + record.getCBText();
    }

  //---------------------------------------------------------------------------

    private void update()
    {
      setText(getRecordText(record));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class QueryFavorite extends MenuItem
  {
    public QueryFavorite(String name, boolean autoexec, String customLogic, boolean orLogic)
    {
      super("Query: " + name);

      this.name = name;
      this.autoexec = autoexec;
      this.customLogic = customLogic;
      this.orLogic = orLogic;

      setOnAction(event -> ui.showSearch(autoexec, null, -1, this, null, null, name));
    }

  //---------------------------------------------------------------------------

    public final List<QueryRow> rows = new ArrayList<>();
    public final String name, customLogic;
    public final boolean autoexec, orLogic;

  //---------------------------------------------------------------------------

    public void removeFromList(List<MenuItem> items)
    {
      nullSwitch(items.stream().map(item -> (QueryFavorite)item).filter(fav -> fav == this).findFirst().orElse(null),
                 items::remove);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class QueryRow
  {
    public final HyperTableCell[] cells = new HyperTableCell[QUERY_FAV_ROW_COLUMN_COUNT];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<MenuItem> recordFavList, queryFavList;
  private static final int QUERY_FAV_ROW_COLUMN_COUNT = 5;
  public static final int FIRST_FAV_MENU_ITEM_NDX = 4;

  HyperFavorites(Menu mnuFavorites, Menu mnuQueries)
  {
    recordFavList = mnuFavorites.getItems();
    queryFavList  = mnuQueries  .getItems();
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

    int mainCount = recordFavList.size() - FIRST_FAV_MENU_ITEM_NDX;

    node.putInt("count", mainCount + queryFavList.size());

    for (int favNdx = 0; favNdx < mainCount; favNdx++)
    {
      RecordFavorite favItem = (RecordFavorite) recordFavList.get(favNdx + FIRST_FAV_MENU_ITEM_NDX);

      node.node("favTypes").put(String.valueOf(favNdx), "record");

      node.node("ids").putInt(String.valueOf(favNdx), favItem.record.getID());
      node.node("types").put(String.valueOf(favNdx), Tag.getTypeTagStr(favItem.record.getType()));
    }

    for (int queryNdx = 0; queryNdx < queryFavList.size(); queryNdx++)
    {
      QueryFavorite favItem = (QueryFavorite) queryFavList.get(queryNdx);

      int favNdx = mainCount + queryNdx;

      node.node("favTypes").put(String.valueOf(favNdx), "query");

      node.node("queries").node("query" + favNdx).put       ("name"       , favItem.name       );
      node.node("queries").node("query" + favNdx).put       ("customLogic", favItem.customLogic);
      node.node("queries").node("query" + favNdx).putInt    ("rowCount"   , favItem.rows.size());
      node.node("queries").node("query" + favNdx).putBoolean("autoexec"   , favItem.autoexec   );
      node.node("queries").node("query" + favNdx).putBoolean("orLogic"    , favItem.orLogic    );

      for (int rowNdx = 0; rowNdx < favItem.rows.size(); rowNdx++)
      {
        for (int colNdx = 0; colNdx < QUERY_FAV_ROW_COLUMN_COUNT; colNdx++)
        {
          HyperTableCell cell = favItem.rows.get(rowNdx).cells[colNdx];
          RecordType type = getCellType(cell);
          String typeStr = type == hdtNone ? "all" : Tag.getTypeTagStr(type);

          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).putInt("id"  , getCellID  (cell));
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).put   ("text", getCellText(cell));
          node.node("queries").node("query" + favNdx).node("row" + rowNdx).node("col" + colNdx).put   ("type", typeStr          );
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

        nullSwitch((HDT_Record)db.records(type).getByID(id), record -> recordFavList.add(new RecordFavorite(record)));
      }
      else if ("query".equals(node.node("favTypes").get(String.valueOf(ndx), "")))
      {
        String  name        = node.node("queries").node("query" + ndx).get       ("name"       , "query" + ndx),
                customLogic = node.node("queries").node("query" + ndx).get       ("customLogic", ""           );
        boolean autoexec    = node.node("queries").node("query" + ndx).getBoolean("autoexec"   , false        ),
                orLogic     = node.node("queries").node("query" + ndx).getBoolean("orLogic"    , false        );
        int     rowCount    = node.node("queries").node("query" + ndx).getInt    ("rowCount"   , 0            );

        QueryFavorite query = new QueryFavorite(name, autoexec, customLogic, orLogic);

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
            int id         = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).getInt("id", -1);
            String text    = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).get("text", ""),
                   typeStr = node.node("queries").node("query" + ndx).node("row" + rowNdx).node("col" + colNdx).get("type", "all");

            row.cells[colNdx] = new HyperTableCell(id, text, "all".equals(typeStr) ? hdtNone : Tag.parseTypeTagStr(typeStr));
          }

          query.rows.add(row);
        }

        queryFavList.add(query);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    while (recordFavList.size() > FIRST_FAV_MENU_ITEM_NDX)
      recordFavList.remove(FIRST_FAV_MENU_ITEM_NDX);

    queryFavList.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int indexOfRecord(HDT_Record record)
  {
    if (record != null)
    {
      for (int ndx = FIRST_FAV_MENU_ITEM_NDX; ndx < recordFavList.size(); ndx++)
      {
        RecordFavorite item = (RecordFavorite) recordFavList.get(ndx);

        if (item.record == record)
          return ndx;
      }
    }

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void changeRecordID(RecordType changedType, int oldID, int newID)
  {
    queryFavList.stream().map(menuItem -> (QueryFavorite)menuItem).forEach(favItem -> favItem.rows.forEach(row ->
    {
      for (int colNdx = 0; colNdx < QUERY_FAV_ROW_COLUMN_COUNT; colNdx++)
      {
        HyperTableCell cell = row.cells[colNdx];

        if ((getCellID(cell) == oldID) && (getCellType(cell) == changedType))
          row.cells[colNdx] = cell.getCopyWithID(newID);
      }
    }));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateItems()
  {
    for (int ndx = FIRST_FAV_MENU_ITEM_NDX; ndx < recordFavList.size(); ndx++)
      ((RecordFavorite) recordFavList.get(ndx)).update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
