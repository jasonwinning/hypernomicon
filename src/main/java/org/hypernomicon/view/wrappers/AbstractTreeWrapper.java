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

import java.util.ArrayList;

import org.hypernomicon.model.records.HDT_Base;
import javafx.scene.control.SelectionModel;
import javafx.scene.control.TreeItem;

public abstract class AbstractTreeWrapper<RowType extends AbstractTreeRow<RowType>>
{
  protected boolean selectingFromCB = false;
  
  public abstract RowType newRow(HDT_Base rootRecord, TreeModel<RowType> treeModel);
  public abstract TreeItem<RowType> getTreeItem(RowType treeRow);
  public abstract TreeItem<RowType> getRoot();
  public abstract SelectionModel<TreeItem<RowType>> getSelectionModel();
  public abstract void scrollToNdx(int ndx);
  public abstract void clear();  
  public abstract ArrayList<RowType> getRowsForRecord(HDT_Base record);  
  public abstract void focusOnTreeCtrl();  
  public abstract void expandMainBranches();  
  public abstract void removeRecord(HDT_Base record);
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public void reset()                           { clear(); }
  public final TreeItem<RowType> selectedItem() { return getSelectionModel().getSelectedItem(); }
  
//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
 
  protected final void setAllExpanded(TreeItem<RowType> item, boolean expanded)
  {
    item.setExpanded(expanded);
    if (item.getChildren() == null) return;
    if (item.getChildren().size() == 0) return;
    
    for (TreeItem<RowType> child : item.getChildren())
      setAllExpanded(child, expanded);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  
  
  public final HDT_Base selectedRecord()
  {
    TreeItem<RowType> item = selectedItem();
    if (item != null)
      return item.getValue().getRecord();
    
    return null;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public final void selectRecord(HDT_Base record, int ndx, boolean fromCB)
  {
    if (record == null) return;
          
    TreeItem<RowType> selItem = getSelectionModel().getSelectedItem();
    RowType row = null;
    
    if (selItem != null)
    {
      row = selItem.getValue();
      if ((row.getRecord() == record) && (ndx == -1))
      {
        focusOnTreeCtrl();
        return;
      }          
    }
    
    if (ndx == -1) ndx = 0;
    
    ArrayList<RowType> list = getRowsForRecord(record);
    if (list == null) return;
    if (list.size() == 0) return;
    if (list.size() <= ndx) ndx = 0;
    
    if (fromCB)
      selectingFromCB = true;
    
    row = list.get(ndx);
    TreeItem<RowType> item = getTreeItem(row);
 
    showItem(item);
    
    if (getSelectionModel().getSelectedItem() != item)
    {      
      getSelectionModel().clearSelection();
  
      while (getSelectionModel().getSelectedItem() != item) // It takes a while for the "showItem" operation to work. Before it's done,
        getSelectionModel().select(item);                   // "select(item)" will instead select a *different* item (?!?!)
    }

    scrollToNdx(getSelectionModel().getSelectedIndex());
    
    focusOnTreeCtrl();
  
    selectingFromCB = false;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private final void showItem(TreeItem<RowType> item)
  {
    if (item.getParent() != null)
      showItem(item.getParent());
    
    item.setExpanded(true);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

}
