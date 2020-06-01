/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.query;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.RelationSet;
import org.hypernomicon.query.SelectColumnsDlgCtrlr.TypeCheckBox;

public final class ColumnGroup
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  final HDT_RecordType recordType;
  final String caption;
  final List<ColumnGroupItem> items = new ArrayList<>();
  TypeCheckBox checkBox;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  ColumnGroup(HDT_RecordType recordType, Set<Tag> tags)
  {
    this.recordType = recordType;
    caption = db.getTypeName(recordType);

    tags.forEach(tag -> items.add(new ColumnGroupItem(tag)));

    RelationSet.getRelationsForObjType(recordType).forEach(relType -> items.add(new ColumnGroupItem(relType)));
  }

  ColumnGroup(String caption)
  {
    this.caption = caption;
    recordType = hdtNone;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
}