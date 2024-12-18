/*
 * Copyright 2015-2024 Jason Winning
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

package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.util.EnumBasedTable;
import org.junit.jupiter.api.Test;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

//---------------------------------------------------------------------------

public class EnumBasedTableTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private enum RowType
  {
    ROW1, ROW2, ROW3
  }

  private enum ColumnType
  {
    COL1, COL2, COL3
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testPutAndGet()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    assertNull(table.put(RowType.ROW1, ColumnType.COL1, "Value1"));
    assertEquals("Value1", table.get(RowType.ROW1, ColumnType.COL1));
  }

  @Test
  void testRemove()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    assertEquals("Value1", table.remove(RowType.ROW1, ColumnType.COL1));
    assertNull(table.get(RowType.ROW1, ColumnType.COL1));
  }

  @Test
  void testContains()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    assertTrue(table.contains(RowType.ROW1, ColumnType.COL1));
    assertFalse(table.contains(RowType.ROW2, ColumnType.COL2));
  }

  @Test
  void testContainsRowAndColumn()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    assertTrue(table.containsRow(RowType.ROW1));
    assertFalse(table.containsRow(RowType.ROW2));
    assertTrue(table.containsColumn(ColumnType.COL1));
    assertFalse(table.containsColumn(ColumnType.COL2));
  }

  @Test
  void testRowAndColumnKeySet()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW2, ColumnType.COL2, "Value2");

    Set<RowType> rowKeys = table.rowKeySet();
    assertEquals(2, rowKeys.size());
    assertTrue(rowKeys.contains(RowType.ROW1));
    assertTrue(rowKeys.contains(RowType.ROW2));

    Set<ColumnType> colKeys = table.columnKeySet();
    assertEquals(2, colKeys.size());
    assertTrue(colKeys.contains(ColumnType.COL1));
    assertTrue(colKeys.contains(ColumnType.COL2));
  }

  @Test
  void testValues()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW2, ColumnType.COL2, "Value2");

    assertEquals(2, table.values().size());
    assertTrue(table.values().contains("Value1"));
    assertTrue(table.values().contains("Value2"));
  }

  @Test
  void testClear()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW2, ColumnType.COL2, "Value2");
    table.clear();
    assertEquals(0, table.size());
    assertNull(table.get(RowType.ROW1, ColumnType.COL1));
    assertNull(table.get(RowType.ROW2, ColumnType.COL2));
  }

  @Test
  void testSize()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW2, ColumnType.COL2, "Value2");
    assertEquals(2, table.size());
  }

  @Test
  void testOperationsOnEmptyTable()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    assertNull(table.get(RowType.ROW1, ColumnType.COL1));
    assertNull(table.remove(RowType.ROW1, ColumnType.COL1));
    assertFalse(table.contains(RowType.ROW1, ColumnType.COL1));
    assertFalse(table.containsRow(RowType.ROW1));
    assertFalse(table.containsColumn(ColumnType.COL1));
    assertTrue(table.rowKeySet().isEmpty());
    assertTrue(table.columnKeySet().isEmpty());
    assertTrue(table.values().isEmpty());
    assertEquals(0, table.size());
  }

  @Test
  void testDuplicateEntries()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    assertEquals("Value1", table.put(RowType.ROW1, ColumnType.COL1, "Value2"));
    assertEquals("Value2", table.get(RowType.ROW1, ColumnType.COL1));
  }

  @Test
  void testDenseData()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    for (RowType row : RowType.values())
    {
      for (ColumnType col : ColumnType.values())
      {
        table.put(row, col, "Value");
      }
    }

    assertEquals(9, table.size()); // 3 rows x 3 columns

    for (RowType row : RowType.values())
    {
      for (ColumnType col : ColumnType.values())
      {
        assertEquals("Value", table.get(row, col));
      }
    }
  }

  @Test
  void testSparseData()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW3, ColumnType.COL3, "Value2");
    assertEquals(2, table.size());
    assertEquals("Value1", table.get(RowType.ROW1, ColumnType.COL1));
    assertEquals("Value2", table.get(RowType.ROW3, ColumnType.COL3));
    assertNull(table.get(RowType.ROW2, ColumnType.COL2));
  }

  @Test
  void testDataConsistency()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW1, ColumnType.COL2, "Value2");
    table.put(RowType.ROW2, ColumnType.COL1, "Value3");

    // Check initial state
    assertEquals("Value1", table.get(RowType.ROW1, ColumnType.COL1));
    assertEquals("Value2", table.get(RowType.ROW1, ColumnType.COL2));
    assertEquals("Value3", table.get(RowType.ROW2, ColumnType.COL1));

    // Remove an item and check state
    table.remove(RowType.ROW1, ColumnType.COL1);
    assertNull(table.get(RowType.ROW1, ColumnType.COL1));
    assertEquals("Value2", table.get(RowType.ROW1, ColumnType.COL2));
    assertEquals("Value3", table.get(RowType.ROW2, ColumnType.COL1));

    // Clear the table and check state
    table.clear();
    assertNull(table.get(RowType.ROW1, ColumnType.COL1));
    assertNull(table.get(RowType.ROW1, ColumnType.COL2));
    assertNull(table.get(RowType.ROW2, ColumnType.COL1));
    assertEquals(0, table.size());
  }

  @Test
  void testColumnConsistencyAfterRemove()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW1, ColumnType.COL2, "Value2");
    table.put(RowType.ROW2, ColumnType.COL1, "Value3");

    // Check initial column values
    Collection<String> columnValues = table.column(ColumnType.COL1).values();
    assertEquals(2, columnValues.size());
    assertTrue(columnValues.contains("Value1"));
    assertTrue(columnValues.contains("Value3"));

    // Remove an item and check column consistency
    table.remove(RowType.ROW1, ColumnType.COL1);
    columnValues = table.column(ColumnType.COL1).values();
    assertEquals(1, columnValues.size());
    assertFalse(columnValues.contains("Value1"));
    assertTrue(columnValues.contains("Value3"));
  }

  @Test
  void testRowMethod()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW1, ColumnType.COL2, "Value2");
    table.put(RowType.ROW2, ColumnType.COL1, "Value3");

    Map<ColumnType, String> rowMap = table.row(RowType.ROW1);
    assertEquals(2, rowMap.size());
    assertEquals("Value1", rowMap.get(ColumnType.COL1));
    assertEquals("Value2", rowMap.get(ColumnType.COL2));

    rowMap = table.row(RowType.ROW3);
    assertTrue(rowMap.isEmpty());
  }

  @Test
  void testColumnMethod()
  {
    EnumBasedTable<RowType, ColumnType, String> table = new EnumBasedTable<>(RowType.class, ColumnType.class);
    table.put(RowType.ROW1, ColumnType.COL1, "Value1");
    table.put(RowType.ROW2, ColumnType.COL1, "Value2");
    table.put(RowType.ROW1, ColumnType.COL2, "Value3");

    Map<RowType, String> columnMap = table.column(ColumnType.COL1);
    assertEquals(2, columnMap.size());
    assertEquals("Value1", columnMap.get(RowType.ROW1));
    assertEquals("Value2", columnMap.get(RowType.ROW2));

    columnMap = table.column(ColumnType.COL3);
    assertTrue(columnMap.isEmpty());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
