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

package org.hypernomicon.query.ui;

import org.hypernomicon.model.Tag;
import org.hypernomicon.model.relations.RelationSet.RelationType;

/**
 * Multiple ColumnGroupItems can be associated with a single column if multiple record types have the same tag.
 * <br>These ColumnGroupItems will only be in a RecordTypeColumnGroup.
 * <br>There will be one ColumnGroupItem for each tag/record type combination.
 * <br>The same column will show the results for a given tag for all of those record types.
 *
 */
class ColumnGroupItem
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final String caption;
  ResultColumn col;

  static final double RESULT_COL_MAX_WIDTH = 600.0;

//---------------------------------------------------------------------------

  // Constructor for all items in the General and BibField column groups

  ColumnGroupItem(ResultColumn col)
  {
    this.col = col;
    caption = col.getText();
    col.setMaxWidth(RESULT_COL_MAX_WIDTH);
  }

  private ColumnGroupItem(String caption)
  {
    this.caption = caption;
  }

//---------------------------------------------------------------------------

  static class NonGeneralColumnGroupItem extends ColumnGroupItem
  {
    final Tag tag;
    final RelationType relType; // If relType != rtNone, then this is a column showing subjects for the row record (the object)

    // Constructor for all RecordType column group items that are not subject items

    NonGeneralColumnGroupItem(Tag tag)
    {
      super(tag.header);

      this.tag = tag;
      relType = RelationType.rtNone;
    }

  //---------------------------------------------------------------------------

    // Constructor for all and only subject items. The column is invisible by default unless shared by a non-subject ColumnGroupItem

    NonGeneralColumnGroupItem(RelationType relType)
    {
      super(relType.getSubjTitle());

      tag = relType.getSubjTag();
      this.relType = relType;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
