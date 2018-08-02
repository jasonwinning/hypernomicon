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

import org.apache.commons.lang3.mutable.MutableDouble;
import org.apache.commons.lang3.mutable.MutableLong;

import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.control.Cell;
import javafx.scene.control.ScrollBar;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableRow;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DataFormat;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;

import static org.hypernomicon.util.Util.*;

public class DragNDropHoverHelper<RowType extends AbstractTreeRow<RowType>>
{
  private MutableLong ctr; 
  private MutableDouble lastX, lastY;
  private ScrollBar scrollBar = null;
  private static final DataFormat HYPERNOMICON_DATA_FORMAT = new DataFormat("application/Hypernomicon");

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public DragNDropHoverHelper()
  {
    ctr = new MutableLong();
    lastX = new MutableDouble();
    lastY = new MutableDouble();
    
    reset();
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private void ensureScrollBarNotNull(Node parent)
  {
    if (scrollBar != null) return;

    for (Node child: parent.lookupAll(".scroll-bar")) 
      if (child instanceof ScrollBar)
      {
        scrollBar = (ScrollBar) child;
        if (scrollBar.getOrientation().equals(Orientation.VERTICAL))
          return;
        else
          scrollBar = null;
      }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public void reset()
  {
    ctr.setValue(0);
    lastX.setValue(-1);
    lastY.setValue(-1);    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public void scroll(DragEvent dragEvent, Node node)
  {
    ensureScrollBarNotNull(node);
    
    if ((lastX.doubleValue() != dragEvent.getSceneX()) || (lastY.doubleValue() != dragEvent.getSceneY()))
    {
      ctr.setValue(System.currentTimeMillis());
      lastX.setValue(dragEvent.getSceneX());
      lastY.setValue(dragEvent.getSceneY());
    }
        
    if ((lastY.doubleValue() - 35) < node.localToScene(node.getBoundsInLocal()).getMinY())
    {
      scrollBar.decrement();
      ctr.setValue(System.currentTimeMillis());
    }

    if ((lastY.doubleValue() + 35) > node.localToScene(node.getBoundsInLocal()).getMaxY())
    {
      scrollBar.increment();
      ctr.setValue(System.currentTimeMillis());
    }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public void expand(TreeItem<RowType> treeItem)
  {
    if (ctr.longValue() != 0)
    {
      runDelayedInFXThread(1, 650, event ->
      {
        long diff = System.currentTimeMillis() - ctr.longValue();
        
        if ((diff > 650) && (treeItem.isExpanded() == false))
        {
          treeItem.setExpanded(true);
          ctr.setValue(System.currentTimeMillis());
        }
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public static interface DragNDropContainer<RowType extends AbstractTreeRow<RowType>>
  {
    void startDrag(RowType row);
    void dragDone();
    boolean acceptDrag(RowType item, DragEvent event, TreeItem<RowType> treeItem);
    void dragDroppedOnto(RowType item);
    DragNDropHoverHelper<RowType> getHelper();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public static <RowType extends AbstractTreeRow<RowType>> void setupHandlers(Cell<RowType> cell, DragNDropContainer<RowType> container)
  {
    cell.setOnDragDetected(event ->
    {
      if (cell.getItem() == null) { return; }

      Dragboard dragBoard = cell.startDragAndDrop(TransferMode.ANY);
      container.startDrag(cell.getItem());
      ClipboardContent content = new ClipboardContent();
      content.put(HYPERNOMICON_DATA_FORMAT, "");
      dragBoard.setContent(content);
      event.consume();
    });
    
    cell.setOnDragDone(event ->
    {
      container.dragDone();
      event.consume();
    });

    if (cell instanceof TreeCell<?>)
    {
      TreeCell<RowType> treeCell = (TreeCell<RowType>)cell;
      
      treeCell.setOnDragOver(event ->
      {
        if (container.acceptDrag(treeCell.getItem(), event, treeCell.getTreeItem()))
          event.acceptTransferModes(TransferMode.ANY);
  
        event.consume();
      });
    }
    else if (cell instanceof TreeTableRow<?>)
    {
      TreeTableRow<RowType> treeTableRow = (TreeTableRow<RowType>)cell;
      
      treeTableRow.setOnDragOver(event ->
      {
        if (container.acceptDrag(treeTableRow.getItem(), event, treeTableRow.getTreeItem()))
          event.acceptTransferModes(TransferMode.ANY);
  
        event.consume();
      });
    }
    else
    {
      cell.setOnDragOver(event ->
      {
        if (container.acceptDrag(cell.getItem(), event, null))
          event.acceptTransferModes(TransferMode.ANY);

        event.consume();
      });
    }
    
    cell.setOnDragDropped(event ->
    {
      container.dragDroppedOnto(cell.getItem());
      event.consume();
    });
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

}
