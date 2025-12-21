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

package org.hypernomicon.model.items;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HDI_Schema.HyperDataCategory;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public class HDI_Base
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HDI_Schema schema;

//---------------------------------------------------------------------------

  HDI_Base(HDI_Schema schema)
  {
    this.schema = schema;
  }

//---------------------------------------------------------------------------

  public HyperDataCategory category() { return schema.category(); }
  public HDI_Schema getSchema()       { return schema; }

  RecordType nestedTargetType()       { return schema.nestedTargetType(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
