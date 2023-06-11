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

package org.hypernomicon.query.sources;

import java.util.EnumSet;

import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public class DatasetQuerySource extends CombinedUnfilteredQuerySource
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final RecordType type;

//---------------------------------------------------------------------------

  public DatasetQuerySource(RecordType type)
  {
    super(EnumSet.of(type));
    this.type = type;
  }

//---------------------------------------------------------------------------

  @Override public RecordType recordType()      { return type; }
  @Override public QuerySourceType sourceType() { return QuerySourceType.QST_recordsByType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
