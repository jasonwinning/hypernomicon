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

package org.hypernomicon.model.items;

import java.util.List;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory;

public class HDI_Base
{
  protected final HDI_Schema schema;
  protected final Tag mainTag;

  public HDI_Base(HDI_Schema newSchema)
  {
    schema = newSchema;
    mainTag = newSchema.getTag();
  }

  public List<Tag> getTags()             { return schema.getTags(); }
  public HyperDataCategory getCategory() { return schema.getCategory(); }
  public HDI_Schema getSchema()          { return schema; }
}
