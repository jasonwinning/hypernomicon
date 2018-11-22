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

import static org.hypernomicon.model.records.HDT_RecordType.hdtNone;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import javafx.scene.control.TreeItem;
import javafx.scene.image.ImageView;

public abstract class AbstractTreeRow<RowType extends AbstractTreeRow<RowType>> implements Comparable<RowType>
{
  protected TreeItem<RowType> treeItem = null;
  protected TreeModel<RowType> treeModel;
  protected ImageView graphic = null;
  
//---------------------------------------------------------------------------
 
  public abstract ImageView getGraphic();
  public abstract HDT_RecordType getRecordType();
  public abstract int getRecordID();  
  public abstract HDT_Base getRecord();

  public final TreeModel<RowType> getTreeModel() { return treeModel; }
  public final TreeItem<RowType> getTreeItem()   { return treeItem; }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final HDT_RecordType getParentType()
  {
    return nullSwitch(nullSwitch(treeItem.getParent(), null, parent -> parent.getValue()), hdtNone, RowType::getRecordType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final int getParentID()
  {
    return nullSwitch(nullSwitch(treeItem.getParent(), null, parent -> parent.getValue()), -1, RowType::getRecordID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final boolean hasAncestorOfType(HDT_RecordType recordType)
  {
    if (treeItem.getParent() == null) return false;
    
    RowType parent = treeItem.getParent().getValue();
    if (parent.getRecordType() == recordType) return true;
    
    return parent.hasAncestorOfType(recordType);
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 

}
