/*
 * Copyright 2015-2019 Jason Winning
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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Record;

import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.control.Cell;
import javafx.scene.control.Control;
import javafx.scene.control.ScrollBar;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableRow;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;

public abstract class DragNDropContainer<RowType extends AbstractTreeRow<? extends HDT_Record, RowType>> extends HasRightClickableRows<RowType>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private long dragMilliCtr;
  private double lastDragX, lastDragY;
  private ScrollBar scrollBar = null;
  private final Control ctrl;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected DragNDropContainer(Control ctrl)
  {
    this.ctrl = ctrl;
    dragReset();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  abstract public void startDrag(RowType row);
  abstract public void dragDone();
  abstract public boolean acceptDrag(RowType item, DragEvent event, TreeItem<RowType> treeItem);
  abstract public void dragDroppedOnto(RowType item);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ScrollBar getScrollBar()
  {
    for (Node child: ctrl.lookupAll(".scroll-bar")) if (child instanceof ScrollBar)
    {
      ScrollBar sb = (ScrollBar) child;
      if (sb.getOrientation().equals(Orientation.VERTICAL))
        return sb;
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void dragReset()
  {
    dragMilliCtr = 0;
    lastDragX = -1;
    lastDragY = -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void scroll(DragEvent dragEvent)
  {
    if (scrollBar == null) scrollBar = getScrollBar();

    if ((lastDragX != dragEvent.getSceneX()) || (lastDragY != dragEvent.getSceneY()))
    {
      dragMilliCtr = System.currentTimeMillis();
      lastDragX = dragEvent.getSceneX();
      lastDragY = dragEvent.getSceneY();
    }

    if ((lastDragY - 35) < ctrl.localToScene(ctrl.getBoundsInLocal()).getMinY())
    {
      scrollBar.decrement();
      dragMilliCtr = System.currentTimeMillis();
    }

    if ((lastDragY + 35) > ctrl.localToScene(ctrl.getBoundsInLocal()).getMaxY())
    {
      scrollBar.increment();
      dragMilliCtr = System.currentTimeMillis();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void expand(TreeItem<RowType> treeItem)
  {
    if (dragMilliCtr == 0) return;

    runDelayedInFXThread(1, 650, () ->
    {
      long diff = System.currentTimeMillis() - dragMilliCtr;

      if ((diff > 650) && (treeItem.isExpanded() == false))
      {
        treeItem.setExpanded(true);
        dragMilliCtr = System.currentTimeMillis();
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setupDragHandlers(Cell<RowType> cell)
  {
    cell.setOnDragDetected(event ->
    {
      if (cell.getItem() == null) return;

      Dragboard dragBoard = cell.startDragAndDrop(TransferMode.ANY);
      startDrag(cell.getItem());
      ClipboardContent content = new ClipboardContent();
      content.put(HYPERNOMICON_DATA_FORMAT, "");
      dragBoard.setContent(content);
      event.consume();
    });

    cell.setOnDragDone(event ->
    {
      dragDone();
      event.consume();
    });

    if (cell instanceof TreeCell<?>)
    {
      TreeCell<RowType> treeCell = (TreeCell<RowType>)cell;

      treeCell.setOnDragOver(event ->
      {
        if (acceptDrag(treeCell.getItem(), event, treeCell.getTreeItem()))
          event.acceptTransferModes(TransferMode.ANY);

        event.consume();
      });
    }
    else if (cell instanceof TreeTableRow<?>)
    {
      TreeTableRow<RowType> treeTableRow = (TreeTableRow<RowType>)cell;

      treeTableRow.setOnDragOver(event ->
      {
        if (acceptDrag(treeTableRow.getItem(), event, treeTableRow.getTreeItem()))
          event.acceptTransferModes(TransferMode.ANY);

        event.consume();
      });
    }
    else
    {
      cell.setOnDragOver(event ->
      {
        if (acceptDrag(cell.getItem(), event, null))
          event.acceptTransferModes(TransferMode.ANY);

        event.consume();
      });
    }

    cell.setOnDragDropped(event ->
    {
      dragDroppedOnto(cell.getItem());
      event.consume();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}