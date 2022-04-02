/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.query.sources;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.function.Predicate;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

public abstract class FilteredQuerySource extends QuerySource
{
  protected final List<HDT_Record> list = new ArrayList<>();
  private final HyperTableCell op1, op2, op3;
  private final QuerySource origSource;

  private boolean generated = false;

  public FilteredQuerySource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2) { this(origSource, op1 , op2 , null); }
  public FilteredQuerySource(QuerySource origSource, HyperTableCell op1)                     { this(origSource, op1 , null, null); }
  public FilteredQuerySource(QuerySource origSource)                                         { this(origSource, null, null, null); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilteredQuerySource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    this.origSource = origSource;
    this.op1 = op1;
    this.op2 = op2;
    this.op3 = op3;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int size()                      { ensureGenerated(); return list.size(); }
  @Override public Iterator<HDT_Record> iterator() { ensureGenerated(); return list.iterator(); }
  @Override public QuerySourceType sourceType()    { return QuerySourceType.QST_filteredRecords; }
  @Override public boolean contains(Object record) { ensureGenerated(); return list.contains(record); }
  @Override public RecordType recordType()         { return origSource.recordType(); }

  protected abstract void runFilter(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void ensureGenerated()
  {
    if (!generated)
    {
      try
      {
        runFilter(op1, op2, op3);
        list.removeIf(Predicate.not(origSource::contains));
      }
      catch(HyperDataException e)
      {
        messageDialog(e.getMessage(), mtError);
        list.clear();
      }

      generated = true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
