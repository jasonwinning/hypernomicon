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

package org.hypernomicon.model;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.List;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory;
import org.hypernomicon.model.relations.RelationSet.RelationType;

import static org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory.*;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;

public final class HDI_Schema
{
  private final List<Tag> tags;
  private final HyperDataCategory category;
  private final RelationType relType;

//---------------------------------------------------------------------------

  public List<Tag> getTags()              { return tags; }
  public HyperDataCategory getCategory()  { return category; }
  public RelationType getRelType()        { return relType; }
  public RecordType getNestedTargetType() { return db.getNestedTargetType(relType, tags.get(0)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HDI_Schema(HyperDataCategory category, Tag... tags) { this(category, rtNone, tags); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDI_Schema(HyperDataCategory category, RelationType relType, Tag... tags)
  {
    Builder<Tag> builder = ImmutableList.builder();
    builder.add(tags);

    if (category == hdcConnector)
      builder.add(tagMainText);

    this.tags = builder.build();
    this.category = category;
    this.relType = relType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
