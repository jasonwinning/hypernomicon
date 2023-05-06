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

package org.hypernomicon.tree;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;

import javafx.scene.Cursor;
import javafx.scene.control.Label;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableCell;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;

//---------------------------------------------------------------------------

// Do NOT override hashCode or equals. Multiple TreeRows with the same text/record can be in the tree and collections that don't allow
// "duplicates" (as ascertained by equals) are used to track them.

public class TreeRow extends AbstractTreeRow<HDT_Record, TreeRow>
{
  private final String text;
  private final HDT_Record record;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  TreeRow(HDT_Record record, TreeModel<TreeRow> treeModel)
  {
    super(treeModel);

    this.record = record;
    text = "";

    if (record == null) return;

    treeItem = new TreeItem<>(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  TreeRow(String text)
  {
    super(null);

    this.text = text;
    record = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  TreeCellValue getNameCell() { return new TreeCellValue(this); }
  String getCBText()          { return record == null ? text : '(' + getTypeName(getRecordType()) + ") " + getName(); }
  String getName()            { return record == null ? text : (record.getType() == hdtWork ? record.getCBText() : record.listName()); }
  ImageView getGraphic()      { return graphic != null ? graphic : imgViewForRecord(record); }

  String getDescString()
  {
    return (record != null) && record.hasDesc() ? ((HDT_RecordWithDescription)record).getDesc().getPlainForDisplay() : "";
  }

  @Override public String toString()    { return getCBText(); }

  @Override public int compareTo(TreeRow o)
  {
    String str = record == null ? text : record.getSortKey(),
           oStr = o.record == null ? o.text : o.record.getSortKey();

    return str.compareTo(oStr);
  }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_Record> HDT_T getRecord() { return (HDT_T) record; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static TreeTableCell<TreeRow, TreeRow> typeCellFactory()
  {
    return new TreeTableCell<>()
    {
      @Override protected void updateItem(TreeRow treeRow, boolean empty)
      {
        super.updateItem(treeRow, empty);

        setText("");

        if (empty || (treeRow == null) || (getTableRow().getItem() == null)) { setGraphic(null); return; }

        HDT_Record rowRecord = treeRow.getRecord();

        if ((rowRecord.getType() == hdtWork) || (rowRecord.getType() == hdtMiscFile))
        {
          String indicator = HDT_Work.getFileIndicator((HDT_RecordWithPath)rowRecord);

          if (indicator.isBlank() == false)
          {
            if ("web".equals(indicator))
              indicator = "Web";
            else
              indicator = titleCase(indicator) + " file";

            super.setGraphic(new Label(indicator));
            return;
          }
        }

        if (rowRecord.isUnitable() == false) { setGraphic(null); return; }

        HDT_RecordWithMainText uRecord = (HDT_RecordWithMainText)rowRecord;
        if (uRecord.hasHub() == false) { setGraphic(null); return; }

        TreeWrapper treeWrapper = (TreeWrapper) treeRow.getTreeWrapper();

        HBox hBox = new HBox();
        uRecord.getHub().getSpokes().filter(record -> record != uRecord).forEachOrdered(spokeRecord ->
        {
          if ((spokeRecord.getType() == hdtConcept) && (treeWrapper.getHasTerms() == false))
            return;

          Label label = new Label("", imgViewForRecord(spokeRecord));
          label.setOnMouseClicked(event -> treeRow.getTreeWrapper().selectRecord(spokeRecord, 0, false));
          label.setCursor(Cursor.HAND);
          setToolTip(label, "Go to " + getTypeName(spokeRecord.getType()) + " record \"" + spokeRecord.name() + '"');
          hBox.getChildren().add(label);
        });

        super.setGraphic(hBox);
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
