/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.querySources;

import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.EnumSet;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;

public class AllQuerySource implements QuerySource
{
  private final CombinedUnfilteredQuerySource source;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public AllQuerySource()
  {
    EnumSet<HDT_RecordType> types = EnumSet.allOf(HDT_RecordType.class);
    types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub));

    source = new CombinedUnfilteredQuerySource(types);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int count()                               { return source.count(); }
  @Override public QuerySourceType sourceType()              { return QuerySourceType.QST_allRecords; }
  @Override public boolean containsRecord(HDT_Record record) { return true; }
  @Override public HDT_Record getRecord(int ndx)             { return source.getRecord(ndx); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
