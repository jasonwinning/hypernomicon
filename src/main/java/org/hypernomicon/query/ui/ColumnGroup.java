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

package org.hypernomicon.query.ui;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet;
import org.hypernomicon.query.ui.SelectColumnsDlgCtrlr.TypeCheckBox;

import com.google.common.collect.ForwardingCollection;

final class ColumnGroup extends ForwardingCollection<ColumnGroupItem>
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  final RecordType recordType;
  final String caption;
  private final List<ColumnGroupItem> items = new ArrayList<>();
  TypeCheckBox checkBox;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  ColumnGroup()
  {
    recordType = hdtNone;
    caption = "General";
  }

  //---------------------------------------------------------------------------

  ColumnGroup(RecordType recordType, Set<Tag> tags)
  {
    this.recordType = recordType;
    caption = getTypeName(recordType);

    tags.forEach(tag -> items.add(new ColumnGroupItem(db.mainTextTagForRecordType(recordType) == tag ? tagMainText : tag)));

    RelationSet.getRelationsForObjType(recordType, false).forEach(relType -> items.add(new ColumnGroupItem(relType)));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override protected Collection<ColumnGroupItem> delegate() { return items; }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
