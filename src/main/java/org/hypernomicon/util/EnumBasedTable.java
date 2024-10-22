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

package org.hypernomicon.util;

import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.annotation.CheckForNull;

import org.checkerframework.checker.nullness.qual.Nullable;

import com.google.common.collect.Table;
import com.google.common.collect.Tables;
import com.google.errorprone.annotations.CanIgnoreReturnValue;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

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
  @Override public Map<R, Map<C, V>> rowMap()                    { return Collections.unmodifiableMap(rowToColumnToValue); }
  @Override public Map<C, Map<R, V>> columnMap()                 { return Collections.unmodifiableMap(columnToRowToValue); }
  @Override public boolean containsValue(@Nullable Object value) { return rowToColumnToValue.values().stream().anyMatch(columnMap -> columnMap.containsValue(value)); }
  @Override public int size()                                    { return rowToColumnToValue.values().stream().mapToInt(EnumMap::size).sum(); }
  @Override public boolean isEmpty()                             { return size() == 0; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override @Nullable @CheckForNull
  public V get(@Nullable Object rowKey, @Nullable Object columnKey)
  {
    return nullSwitch(rowToColumnToValue.get(rowKey), null, columnToValue -> columnToValue.get(columnKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public V put(R row, C column, V newValue)
  {
    V oldValue = get(row, column);

    EnumMap<C, V> columnToValue = rowToColumnToValue.get(row);
    if (columnToValue == null)
      rowToColumnToValue.put(row, columnToValue = new EnumMap<>(columnType));

    columnToValue.put(column, newValue);

    EnumMap<R, V> rowToValue = columnToRowToValue.get(column);
    if (rowToValue == null)
      columnToRowToValue.put(column, rowToValue = new EnumMap<>(rowType));

    rowToValue.put(row, newValue);

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
  @CheckForNull
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

  @Override public Map<C, V> row(R rowKey)
  {
    EnumMap<C, V> columnToValue = rowToColumnToValue.get(rowKey);
    return columnToValue == null ? Collections.emptyMap() : Collections.unmodifiableMap(columnToValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Map<R, V> column(C columnKey)
  {
    EnumMap<R, V> rowToValue = columnToRowToValue.get(columnKey);
    return rowToValue == null ? Collections.emptyMap() : Collections.unmodifiableMap(rowToValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
