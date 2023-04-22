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

import static org.hypernomicon.model.Tag.*;

import org.hypernomicon.model.Tag;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.ui.ResultsTable.ResultColumn;

import javafx.scene.control.TableView;

final class ColumnGroupItem
{

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  final Tag tag;
  final RelationType relType; // If relType != rtNone, then this is a column showing subjects for the row record (the object)
  final String caption;
  ResultColumn col;

  static final double RESULT_COL_MAX_WIDTH = 600.0;

  //---------------------------------------------------------------------------

  // Constructor for all and only items in the general column group

  ColumnGroupItem(ResultColumn col, TableView<ResultsRow> tv, int colNdx)
  {
    tag = tagNone;
    relType = RelationType.rtNone;

    this.col = col;
    caption = col.getText();
    col.setMaxWidth(RESULT_COL_MAX_WIDTH);

    if (colNdx < 0)
      tv.getColumns().add(col);
    else
      tv.getColumns().add(colNdx, col);
  }

  //---------------------------------------------------------------------------

  ColumnGroupItem(Tag tag)
  {
    this.tag = tag;
    relType = RelationType.rtNone;
    caption = tag.header;
  }

  //---------------------------------------------------------------------------

  ColumnGroupItem(RelationType relType)
  {
    tag = relType.getSubjTag();
    this.relType = relType;
    caption = relType.getSubjTitle();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
