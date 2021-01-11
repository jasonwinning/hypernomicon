/*
 * Copyright 2015-2021 Jason Winning
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
import java.util.EnumMap;

import static org.hypernomicon.util.Util.*;

public class EnumBasedTable<R extends Enum<R>, C extends Enum<C>, V>
{
  private final EnumMap<R, EnumMap<C, V>> rowToColumnToValue;
  private final EnumMap<C, EnumMap<R, V>> columnToRowToValue;
  private final Class<R> rowType;
  private final Class<C> columnType;

  public EnumBasedTable(Class<R> rowType, Class<C> columnType)
  {
    rowToColumnToValue = new EnumMap<>(rowType);
    columnToRowToValue = new EnumMap<>(columnType);

    this.rowType = rowType;
    this.columnType = columnType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public V get(R row, C column)            { return nullSwitch(rowToColumnToValue.get(row), null, columnToValue -> columnToValue.get(column)); }
  public Collection<V> getRow(R row)       { return nullSwitch(rowToColumnToValue.get(row), null, EnumMap::values); }
  public Collection<V> getColumn(C column) { return nullSwitch(columnToRowToValue.get(column), null, EnumMap::values); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public V put(R row, C column, V newValue)
  {
    V oldValue = get(row, column);

    EnumMap<C, V> columnToValue = rowToColumnToValue.get(row);
    if (columnToValue == null)
    {
      columnToValue = new EnumMap<>(columnType);
      rowToColumnToValue.put(row, columnToValue);
    }

    columnToValue.put(column, newValue);

    EnumMap<R, V> rowToValue = columnToRowToValue.get(column);
    if (rowToValue == null)
    {
      rowToValue = new EnumMap<>(rowType);
      columnToRowToValue.put(column, rowToValue);
    }

    rowToValue.put(row, newValue);

    return oldValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
