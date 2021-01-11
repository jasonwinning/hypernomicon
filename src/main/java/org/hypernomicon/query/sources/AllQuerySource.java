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

package org.hypernomicon.query.sources;

import static org.hypernomicon.model.records.RecordType.*;

import java.util.EnumSet;

import org.hypernomicon.model.records.RecordType;

public class AllQuerySource extends CombinedUnfilteredQuerySource
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public AllQuerySource() { super(types()); }

  @Override public QuerySourceType sourceType() { return QuerySourceType.QST_allRecords; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EnumSet<RecordType> types()
  {
    EnumSet<RecordType> types = EnumSet.allOf(RecordType.class);
    types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub));
    return types;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
