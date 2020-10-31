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

package org.hypernomicon.tree;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import javafx.scene.control.TreeItem;
import javafx.scene.image.ImageView;

//---------------------------------------------------------------------------

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

  HyperTreeCellValue getNameCell() { return new HyperTreeCellValue(this); }
  String getCBText()               { return record == null ? text : "(" + db.getTypeName(getRecordType()) + ") " + getName(); }
  String getName()                 { return record == null ? text : (record.getType() == hdtWork ? record.getCBText() : record.listName()); }

  String getDescString()
  {
    return (record != null) && record.hasDesc() ? ((HDT_RecordWithDescription)record).getDesc().getPlainForDisplay() : "";
  }

  @Override public String toString()        { return getCBText(); }
  @Override public ImageView getGraphic()   { return graphic != null ? graphic : imgViewForRecord(record); }
  @Override public int compareTo(TreeRow o) { return record.getSortKey().compareTo(o.record.getSortKey()); }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_Record> HDT_T getRecord() { return (HDT_T) record; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getTypeString()
  {
    if (record == null) return "";

    treeItem.setGraphic(getGraphic());

    String typeName;

    if (getRecordType() != hdtWork)
    {
      typeName = db.getTypeName(record.getType());
      if (record.isUnitable() && ((HDT_RecordWithConnector)record).isLinked())
        typeName = typeName + " (linked)";

      return typeName;
    }

    HDT_Work work = (HDT_Work)record;

    switch (HDT_WorkType.getEnumVal(work.workType.get()))
    {
      case wtPaper     : typeName = "Paper"   ; break;
      case wtBook      : typeName = "Book"    ; break;
      case wtChapter   : typeName = "Chapter" ; break;
      case wtRecording : typeName = "Lecture" ; break;
      case wtWebPage   : typeName = "Web Page"; break;
      default          : typeName = "Work";
    }

    return HDT_Work.addFileIndicator(typeName, work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
