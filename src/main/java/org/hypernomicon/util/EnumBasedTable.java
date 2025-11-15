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

package org.hypernomicon.util;

import java.util.*;
import java.util.stream.Collectors;

import org.jspecify.annotations.Nullable;

import com.google.common.collect.Table;
import com.google.common.collect.Tables;
import com.google.errorprone.annotations.*;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * EnumBasedTable is a specialized table implementation that uses two Enum types for its rows and columns.
 * This class provides efficient access and management of values stored in a table structure, leveraging
 * EnumMaps for performance.
 *
 * @param <R> the type of the enum representing the rows
 * @param <C> the type of the enum representing the columns
 * @param <V> the type of the values stored in the table
 */
public class EnumBasedTable<R extends Enum<R>, C extends Enum<C>, V> implements Table<R, C, V>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final EnumMap<R, EnumMap<C, V>> rowToColumnToValue;
  private final EnumMap<C, EnumMap<R, V>> columnToRowToValue;
  private final Class<R> rowType;
  private final Class<C> columnType;

//---------------------------------------------------------------------------

  public EnumBasedTable(Class<R> rowType, Class<C> columnType)
  {
    rowToColumnToValue = new EnumMap<>(rowType);
    columnToRowToValue = new EnumMap<>(columnType);

    this.rowType = rowType;
    this.columnType = columnType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<R> rowKeySet()                            { return rowToColumnToValue.keySet(); }
  @Override public Set<C> columnKeySet()                         { return columnToRowToValue.keySet(); }
  @Override public boolean contains(Object row, Object column)   { return get(row, column) != null; }
  @Override public boolean containsRow(Object row)               { return rowToColumnToValue.containsKey(row); }
  @Override public boolean containsColumn(Object column)         { return columnToRowToValue.containsKey(column); }
  @Override public boolean containsValue(@Nullable Object value) { return rowToColumnToValue.values().stream().anyMatch(columnMap -> columnMap.containsValue(value)); }
  @Override public int size()                                    { return rowToColumnToValue.values().stream().mapToInt(EnumMap::size).sum(); }
  @Override public boolean isEmpty()                             { return size() == 0; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override @Nullable
  public V get(@Nullable Object rowKey, @Nullable Object columnKey)
  {
    return nullSwitch(rowToColumnToValue.get(rowKey), null, columnToValue -> columnToValue.get(columnKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public V put(R rowKey, C columnKey, V newValue)
  {
    V oldValue = get(rowKey, columnKey);

    rowToColumnToValue.computeIfAbsent(rowKey   , _ -> new EnumMap<>(columnType)).put(columnKey, newValue);
    columnToRowToValue.computeIfAbsent(columnKey, _ -> new EnumMap<>(rowType   )).put(rowKey   , newValue);

    return oldValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    rowToColumnToValue.clear();
    columnToRowToValue.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Collection<V> values()
  {
    return rowToColumnToValue.values().stream().flatMap(map -> map.values().stream()).collect(Collectors.toSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void putAll(Table<? extends R, ? extends C, ? extends V> table)
  {
    table.cellSet().forEach(cell -> put(cell.getRowKey(), cell.getColumnKey(), cell.getValue()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<Cell<R, C, V>> cellSet()
  {
    Set<Cell<R, C, V>> cellSet = new HashSet<>();

    for (R row : rowToColumnToValue.keySet())
      for (C column : rowToColumnToValue.get(row).keySet())
        cellSet.add(Tables.immutableCell(row, column, get(row, column)));

    return cellSet;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override
  @Nullable
  @CanIgnoreReturnValue
  public V remove(@Nullable Object rowKey, @Nullable Object columnKey)
  {
    R row = rowType.cast(rowKey);
    C column = columnType.cast(columnKey);
    V oldValue = get(row, column);

    if (oldValue != null)
    {
      rowToColumnToValue.get(row).remove(column);
      columnToRowToValue.get(column).remove(row);
    }

    return oldValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a view of all mappings that have the given row key. For each row key / column key /
   * value mapping in the table with that row key, the returned map associates the column key with
   * the value. If no mappings in the table have the provided row key, an empty map is returned.
   *
   * <p>The returned map is not modifiable.
   *
   * @param rowKey key of row to search for in the table
   * @return the corresponding map from column keys to values
   */
  @Override public Map<C, V> row(R rowKey)
  {
    EnumMap<C, V> columnToValue = rowToColumnToValue.get(rowKey);
    return columnToValue == null ? Collections.emptyMap() : Collections.unmodifiableMap(columnToValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a view of all mappings that have the given column key. For each row key / column key /
   * value mapping in the table with that column key, the returned map associates the row key with
   * the value. If no mappings in the table have the provided column key, an empty map is returned.
   *
   * <p>The returned map is not modifiable.
   *
   * @param columnKey key of column to search for in the table
   * @return the corresponding map from row keys to values
   */
  @Override public Map<R, V> column(C columnKey)
  {
    EnumMap<R, V> rowToValue = columnToRowToValue.get(columnKey);
    return rowToValue == null ? Collections.emptyMap() : Collections.unmodifiableMap(rowToValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Guaranteed to throw an exception and leave the table unmodified.
   *
   * @throws UnsupportedOperationException always
   * @deprecated Unsupported operation.
   */
  @CanIgnoreReturnValue
  @Deprecated
  @DoNotCall("Always throws UnsupportedOperationException")
  @Override public Map<R, Map<C, V>> rowMap()
  {
    throw new UnsupportedOperationException("Internal error: EnumMap does not support rowMap().");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Guaranteed to throw an exception and leave the table unmodified.
   *
   * @throws UnsupportedOperationException always
   * @deprecated Unsupported operation.
   */
  @CanIgnoreReturnValue
  @Deprecated
  @DoNotCall("Always throws UnsupportedOperationException")
  @Override public Map<C, Map<R, V>> columnMap()
  {
    throw new UnsupportedOperationException("Internal error: EnumMap does not support columnMap().");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
