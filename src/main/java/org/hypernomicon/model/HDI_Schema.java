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

package org.hypernomicon.model;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.List;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;

import static org.hypernomicon.model.HDI_Schema.HyperDataCategory.*;
import static org.hypernomicon.model.Tag.*;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;

public record HDI_Schema(HyperDataCategory category, RelationType relType, RecordType nestedTargetType, List<Tag> tags)
{

//---------------------------------------------------------------------------

  public enum HyperDataCategory
  {
    hdcPointerSingle, hdcPointerMulti,   hdcNestedPointer,
    hdcString,        hdcMainTextAndHub, hdcBoolean,
    hdcTernary,       hdcPath,           hdcPersonName,
    hdcBibEntryKey,   hdcAuthors,        hdcBibDate,
    hdcHubSpokes
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HDI_Schema(HyperDataCategory category, Tag... tags) { this(category, rtNone, hdtNone, tags); }

  public HDI_Schema(HyperDataCategory category, RelationType relType, Tag... tags) { this(category, relType, hdtNone, tags); }

  public HDI_Schema(HyperDataCategory category, RelationType relType, RecordType nestedTargetType, Tag... tags)
  {
    this(category, relType, nestedTargetType, buildTagsList(category, nestedTargetType, tags));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<Tag> buildTagsList(HyperDataCategory category, RecordType nestedTargetType, Tag[] tags)
  {
    if (category == hdcNestedPointer)
    {
      assert(tags.length == 1);
      assert(nestedTargetType != hdtNone);
    }
    else
      assert(nestedTargetType == hdtNone);

    Builder<Tag> builder = ImmutableList.builder();
    builder.add(tags);

    if (category == hdcMainTextAndHub)
      builder.add(tagMainText);

    return builder.build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
