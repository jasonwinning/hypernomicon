/*
 * Copyright 2015-2026 Jason Winning
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

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.GeneralQueries.*;
import static org.hypernomicon.query.WorkQueries.QUERY_WHERE_BIB_FIELD;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;

import org.junit.jupiter.api.*;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.HyperFavorites.QueryRow;
import org.hypernomicon.view.cellValues.*;

//---------------------------------------------------------------------------

/**
 * How query favorites keep the operands that name records current when a record is deleted or its ID
 * changes, and leave alone the cells that carry the same record type and number without naming a record.
 */
class HyperFavoritesTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int QUERY_WHERE_RELATIVE = 5;  // "where set of records related by being": its first operand is a relation

  private static TestHyperDB db;

  private Menu queryMenu;
  private HyperFavorites favorites;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    db.closeAndOpen();  // Leave no records behind for other test classes
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp()
  {
    Menu recordMenu = new Menu();

    for (int ndx = 0; ndx < HyperFavorites.FIRST_FAV_MENU_ITEM_NDX; ndx++)  // The menu's own items, which come before the favorites
      recordMenu.getItems().add(new MenuItem());

    queryMenu = new Menu();
    favorites = new HyperFavorites(recordMenu, queryMenu);
  }

//---------------------------------------------------------------------------

  /** A query favorite with one row of the given query, of works, with the given operands */
  private QueryFavorite addFavorite(int queryID, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    QueryFavorite favorite = new QueryFavorite("favorite", false, "", false);
    QueryRow row = new QueryRow();

    row.cells[0] = new GenericNonRecordHTC(1, "Works", hdtWork);
    row.cells[1] = new GenericNonRecordHTC(queryID, "query", hdtWork);
    row.cells[2] = op1;
    row.cells[3] = op2;
    row.cells[4] = op3;

    favorite.rows.add(row);
    queryMenu.getItems().add(favorite);

    return favorite;
  }

//---------------------------------------------------------------------------

  private static HyperTableCell operand(QueryFavorite favorite, int opNum) { return favorite.rows.getFirst().cells[opNum + 1]; }

  private static HyperTableCell recordCell(HDT_Work work) { return new RecordHTC(work.getID(), work.name(), hdtWork); }

  private static HyperTableCell blank() { return new GenericNonRecordHTC("", hdtNone); }

//---------------------------------------------------------------------------

  /** A relation's cell carries the query's record type and the relation's code, which a record can have as its ID */
  private static HyperTableCell relationCell(int code) { return new GenericNonRecordHTC(code, "Child Work(s)", hdtWork); }

  /** A bibliographic field's cell carries the work record type and the field's number, which a work can have as its ID */
  private static HyperTableCell bibFieldCell(int fieldNum) { return new GenericNonRecordHTC(fieldNum, "Title", hdtWork); }

//---------------------------------------------------------------------------

  @Test
  void deletingRecordEmptiesOperandsThatNameIt()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    work.setName("Work");

    QueryFavorite linking  = addFavorite(QUERY_MATCHING_WORK_OR_SUBWORKS, recordCell(work), blank(), blank()),
                  relative = addFavorite(QUERY_WHERE_RELATIVE, relationCell(work.getID()), blank(), recordCell(work)),
                  bibField = addFavorite(QUERY_WHERE_BIB_FIELD, bibFieldCell(work.getID()), blank(), blank());

    favorites.removeRecordFromQueries(work);

    assertEquals(-1, getCellID(operand(linking, 1)), "the work as the first operand of the query that takes a work there is emptied");
    assertEquals("", getCellText(operand(linking, 1)));
    assertEquals(-1, getCellID(operand(relative, 3)), "the work as the third operand is emptied");

    assertEquals(work.getID(), getCellID(operand(relative, 1)), "a relation with the work's ID as its code is left alone");
    assertEquals("Child Work(s)", getCellText(operand(relative, 1)));
    assertEquals(work.getID(), getCellID(operand(bibField, 1)), "a bibliographic field with the work's ID as its number is left alone");
  }

//---------------------------------------------------------------------------

  @Test
  void changingRecordIDChangesOnlyOperandsThatNameIt()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    work.setName("Work");

    int oldID = work.getID(), newID = oldID + 1000;

    QueryFavorite relative = addFavorite(QUERY_WHERE_RELATIVE, relationCell(oldID), blank(), recordCell(work)),
                  bibField = addFavorite(QUERY_WHERE_BIB_FIELD, bibFieldCell(oldID), blank(), blank());

    relative.rows.getFirst().cells[0] = new GenericNonRecordHTC(oldID, "Works", hdtWork);  // A query type whose code is the work's ID

    favorites.changeRecordID(hdtWork, oldID, newID);

    assertEquals(newID, getCellID(operand(relative, 3)), "the work as the third operand gets the new ID");
    assertEquals(oldID, getCellID(operand(relative, 1)), "a relation with the work's ID as its code is left alone");
    assertEquals(oldID, getCellID(relative.rows.getFirst().cells[0]), "the query type is left alone");
    assertEquals(oldID, getCellID(operand(bibField, 1)), "a bibliographic field with the work's ID as its number is left alone");
  }

//---------------------------------------------------------------------------

  /** A favorite saved before deletions emptied operands can name a record that is gone; loading it empties that operand */
  @Test
  void loadingEmptiesOperandsNamingRecordsThatAreGone() throws Exception
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    work.setName("Work");

    int missingID = work.getID() + 1000;

    addFavorite(QUERY_LINKING_TO_RECORD, new GenericNonRecordHTC("Work", hdtWork), new RecordHTC(missingID, "Deleted work", hdtWork), blank());
    addFavorite(QUERY_LINKING_TO_RECORD, new GenericNonRecordHTC("Work", hdtWork), recordCell(work), blank());

    favorites.saveToPrefNode();
    favorites.loadFromPrefNode();

    assertEquals(2, queryMenu.getItems().size());

    QueryFavorite missing  = (QueryFavorite) queryMenu.getItems().get(0),
                  existing = (QueryFavorite) queryMenu.getItems().get(1);

    assertEquals(-1, getCellID(operand(missing, 2)), "an operand naming a record that is gone is emptied");
    assertEquals(work.getID(), getCellID(operand(existing, 2)), "an operand naming a record that exists is kept");
  }

//---------------------------------------------------------------------------

}
