/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import javafx.scene.control.TreeItem;
import javafx.scene.image.ImageView;

//---------------------------------------------------------------------------

public class TreeRow extends AbstractTreeRow<TreeRow>
{
  private String text;
  private HDT_Base record;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TreeRow(HDT_Base record, TreeModel<TreeRow> treeModel)
  {
    this.record = record;    
    this.treeModel = treeModel;
    text = "";

    if (record == null) return;
       
    treeItem = new TreeItem<TreeRow>(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TreeRow(String text)
  {
    this.text = text;
    record = null;
    treeModel = null;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTreeCellValue getNameCell() { return new HyperTreeCellValue(this); }
  public String getCBText()               { return record == null ? text : "(" + db.getTypeName(getRecordType()) + ") " + getName(); }
  public String getName()                 { return nullSwitch(record, text, () -> record.getType() == hdtWork ? record.getCBText() : record.listName()); }

  public String getDescString() 
  { 
    return (record != null) && record.hasDesc() ? HDT_RecordWithDescription.class.cast(record).getDesc().getPlainForDisplay() : "";
  }
  
  @Override public String toString()               { return getCBText(); }
  @Override public ImageView getGraphic()          { return graphic != null ? graphic : getImageViewForRelativePath(ui.getGraphicRelativePath(record)); }
  @Override public HDT_RecordType getRecordType()  { return record == null ? hdtNone : record.getType(); }
  @Override public int getRecordID()               { return record == null ? -1 : record.getID(); }
  @Override public HDT_Base getRecord()            { return record; }
  @Override public int compareTo(TreeRow o)        { return record.getSortKey().compareTo(o.record.getSortKey()); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public String getTypeString()
  {    
    if (record == null) return "";

    treeItem.setGraphic(getGraphic());

    String typeName;
    
    if (getRecordType() != hdtWork)
    {
      typeName = db.getTypeName(record.getType());
      if (record.isUnitable())
        if (((HDT_RecordWithConnector)record).isLinked())
          typeName = typeName + " (linked)";
      
      return typeName;
    }
    
    HDT_Work work = (HDT_Work)record;
    
    HDT_WorkType workType = work.workType.get();
    if (workType == null) return HDT_Work.addFileIndicator("Work", work);
    
    switch (workType.getEnumVal())
    {
      case wtPaper :    typeName = "Paper"; break;
      case wtBook :     typeName = "Book"; break;
      case wtChapter:   typeName = "Chapter"; break;
      case wtRecording: typeName = "Lecture"; break;
      case wtWebPage:   typeName = "Web Page"; break;
      default:          typeName = "Work";
    }
      
    return HDT_Work.addFileIndicator(typeName, work);          
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
