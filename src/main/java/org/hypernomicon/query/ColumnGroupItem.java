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
import static org.hypernomicon.model.HyperDB.Tag.tagNone;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.ResultsTable.ResultColumn;

public final class ColumnGroupItem
{

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  ColumnGroupItem(Tag tag)
  {
    this.tag = tag;
    caption = db.getTagHeader(tag);
    relType = RelationType.rtNone;
  }

  ColumnGroupItem(String caption)
  {
    tag = tagNone;
    this.caption = caption;
    relType = RelationType.rtNone;
  }

  ColumnGroupItem(RelationType relType)
  {
    this.relType = relType;
    tag = relType.getSubjTag();
    caption = relType.getSubjTitle();
  }

  final Tag tag;
  final RelationType relType; // If relType != rtNone, then this is a column showing subjects for the row record (the object)
  final String caption;
  ResultColumn<? extends Comparable<?>> col;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
}