/*
 * Copyright 2015-2026 Jason Winning
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

import org.hypernomicon.App;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.tree.AbstractTreeRow;

import java.util.concurrent.*;

import javafx.application.Platform;
import javafx.geometry.Orientation;
import javafx.scene.control.*;
import javafx.scene.input.*;

//---------------------------------------------------------------------------

public abstract class DragNDropContainer<RowType extends AbstractTreeRow<? extends HDT_Record, RowType>> extends HasRightClickableRows<RowType>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private long dragMilliCtr;
  private double lastDragX, lastDragY;
  private ScrollBar scrollBar = null;
  private ScheduledFuture<?> expandFuture;
  private ScheduledExecutorService expandExecutor;
  private TreeItem<?> pendingExpandItem;

//---------------------------------------------------------------------------

  protected DragNDropContainer()
  {
    dragReset();
  }

//---------------------------------------------------------------------------

  protected abstract void startDrag(RowType row);
  protected abstract void dragDone();
  protected abstract boolean isValidDragTarget(RowType item, DragEvent event, TreeItem<RowType> treeItem);
  protected abstract void dragDroppedOnto(RowType item);
  protected abstract Control getControl();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ScrollBar getScrollBar()
  {
    return getControl().lookupAll(".scroll-bar").stream().filter(child -> child instanceof ScrollBar)
                                                         .map(child -> (ScrollBar) child)
                                                         .filter(sb -> sb.getOrientation() == Orientation.VERTICAL)
                                                         .findFirst().orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final protected void dragReset()
  {
    dragMilliCtr = 0L;
    lastDragX = -1;
    lastDragY = -1;

    if (expandFuture != null)
    {
      expandFuture.cancel(false);
      expandFuture = null;
    }

    if (expandExecutor != null)
    {
      expandExecutor.shutdownNow();
      expandExecutor = null;
    }

    pendingExpandItem = null;
    App.dragInProgress = false;
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

    Control ctrl = getControl();

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
    if ((dragMilliCtr == 0) || (treeItem == pendingExpandItem)) return;

    pendingExpandItem = treeItem;

    if (expandFuture != null)
      expandFuture.cancel(false);

    if (expandExecutor == null)
      expandExecutor = Executors.newSingleThreadScheduledExecutor(r ->
      { Thread t = new Thread(r, "ExpandTimer"); t.setDaemon(true); return t; });

    expandFuture = expandExecutor.schedule(() -> Platform.runLater(() ->
    {
      pendingExpandItem = null;

      if (treeItem.isExpanded() == false)
      {
        treeItem.setExpanded(true);
        dragMilliCtr = System.currentTimeMillis();
      }
    }), 650, TimeUnit.MILLISECONDS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setupDragHandlers(Cell<RowType> cell)
  {
    cell.setOnDragDetected(event ->
    {
      try
      {
        if ((cell.getItem() == null) || event.isBackButtonDown() || event.isForwardButtonDown()) return;

        App.dragInProgress = true;

        Dragboard dragBoard = cell.startDragAndDrop(TransferMode.COPY, TransferMode.MOVE);
        startDrag(cell.getItem());
        ClipboardContent content = new ClipboardContent();
        content.put(HYPERNOMICON_DATA_FORMAT, "");
        dragBoard.setContent(content);
      }
      catch (Throwable th)
      {
        logThrowable(th);
      }

      event.consume();
    });

    cell.setOnDragDone(event ->
    {
      try
      {
        dragDone();
      }
      catch (Throwable th)
      {
        logThrowable(th);
      }

      App.dragInProgress = false;

      event.consume();
    });

    if (cell instanceof TreeCell<RowType> treeCell)
    {
      treeCell.setOnDragOver(event ->
      {
        try
        {
          if (isValidDragTarget(treeCell.getItem(), event, treeCell.getTreeItem()))
            event.acceptTransferModes(TransferMode.COPY, TransferMode.MOVE);
        }
        catch (Throwable th)
        {
          logThrowable(th);
        }

        event.consume();
      });
    }
    else if (cell instanceof TreeTableRow<RowType> treeTableRow)
    {
      treeTableRow.setOnDragOver(event ->
      {
        try
        {
          if (isValidDragTarget(treeTableRow.getItem(), event, treeTableRow.getTreeItem()))
            event.acceptTransferModes(TransferMode.COPY, TransferMode.MOVE);
        }
        catch (Throwable th)
        {
          logThrowable(th);
        }

        event.consume();
      });
    }
    else
    {
      cell.setOnDragOver(event ->
      {
        try
        {
          if (isValidDragTarget(cell.getItem(), event, null))
            event.acceptTransferModes(TransferMode.COPY, TransferMode.MOVE);
        }
        catch (Throwable th)
        {
          logThrowable(th);
        }

        event.consume();
      });
    }

    cell.setOnDragDropped(event ->
    {
      try
      {
        dragDroppedOnto(cell.getItem());
      }
      catch (Throwable th)
      {
        logThrowable(th);
      }

      event.consume();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
