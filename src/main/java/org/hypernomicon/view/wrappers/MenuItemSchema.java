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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.model.records.RecordType.*;

import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

public final class MenuItemSchema<HDT_T extends HDT_Record, RowType extends AbstractRow<? extends HDT_Record, RowType>>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final RecordType recordType;
  private final Predicate<HDT_T> condRecordHandler;
  private final Consumer<HDT_T> recordHandler;
  private final Predicate<RowType> condRowHandler;
  private final Consumer<RowType> rowHandler;
  private final Supplier<String> caption;

  public boolean disabled = false;

//---------------------------------------------------------------------------

  MenuItemSchema(Supplier<String> caption, Class<HDT_T> klass, Predicate<HDT_T> condRecordHandler, Consumer<HDT_T> recordHandler)
  { this(caption, typeByRecordClass(klass), condRecordHandler, recordHandler, null, null); }

  MenuItemSchema(Supplier<String> caption, Class<HDT_T> klass, Consumer<HDT_T> recordHandler)
  { this(caption, typeByRecordClass(klass), record -> true, recordHandler, null, null); }

  MenuItemSchema(Supplier<String> caption, Predicate<RowType> condRowHandler, Consumer<RowType> rowHandler)
  { this(caption, hdtNone, null, null, condRowHandler, rowHandler); }

  MenuItemSchema(Supplier<String> caption, Consumer<RowType> rowHandler)
  { this(caption, hdtNone, null, null, row -> true, rowHandler); }

  private MenuItemSchema(Supplier<String> caption, RecordType recordType, Predicate<HDT_T  > condRecordHandler, Consumer<HDT_T  > recordHandler,
                                                                          Predicate<RowType> condRowHandler   , Consumer<RowType> rowHandler)
  {
    this.caption = caption;
    this.recordType = recordType;
    this.condRecordHandler = condRecordHandler;
    this.recordHandler = recordHandler;
    this.condRowHandler = condRowHandler;
    this.rowHandler = rowHandler;
  }

//---------------------------------------------------------------------------

  public String getCaption() { return caption.get(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public void doAction(RowType row)
  {
    if (recordHandler == null)
      rowHandler.accept(row);
    else
      recordHandler.accept((HDT_T)(recordType == hdtNone ? row.getRecord() : row.getRecordByType(recordType))); // The cast is necessary to avoid Maven false-positive build error
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public boolean testWhetherToShow(RowType row)
  {
    if (condRecordHandler == null)
      return condRowHandler.test(row);

    HDT_Record record = recordType == hdtNone ? row.getRecord() : row.getRecordByType(recordType);
    return (record != null) && condRecordHandler.test((HDT_T) record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
