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

package org.hypernomicon.tree;

import java.util.function.DoubleSupplier;

import javafx.scene.control.*;
import javafx.scene.control.skin.TreeTableViewSkin;
import javafx.scene.control.skin.VirtualFlow;

//---------------------------------------------------------------------------

/**
 * A {@link TreeTableViewSkin} whose {@link VirtualFlow} sizes each row to a STABLE breadth, working around a JavaFX
 * {@code VirtualFlow} bug (the same family as https://bugs.openjdk.org/browse/JDK-8255436 and
 * https://bugs.openjdk.org/browse/JDK-8282091) that pins the JavaFX application thread at a steady, indefinite CPU load
 * whenever the columns' total width exceeds the viewport, even when the control is idle.
 *
 * <p>The stock {@code VirtualFlow.resizeCell} sizes each row to {@code max(getMaxPrefBreadth(), getViewportBreadth())}.
 * During a cell rebuild the flow transiently resets {@code maxPrefBreadth} to -1, so the first row is momentarily resized
 * to the (smaller) viewport breadth and then back to the content breadth. That changes the row geometry, marks the scene
 * dirty, and schedules another pulse, which rebuilds again, forever. The loop stops the instant the columns are made to
 * fit. Computing the breadth directly from the visible leaf column widths (a value that does not change between layout
 * passes) removes the oscillation, while keeping native horizontal scrolling, the native (pinned, always-visible)
 * vertical scrollbar, and row virtualization all intact.
 *
 * <p>A {@code TreeTableView}'s flow is always vertical, so the breadth axis is the width axis throughout.
 */
class StableBreadthTreeTableViewSkin<S> extends TreeTableViewSkin<S>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  StableBreadthTreeTableViewSkin(TreeTableView<S> control)
  {
    super(control);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected VirtualFlow<TreeTableRow<S>> createVirtualFlow()
  {
    return new StableBreadthVirtualFlow<>(() -> visibleLeafColumnWidthSum(getSkinnable()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static double visibleLeafColumnWidthSum(TreeTableView<?> treeTableView)
  {
    double total = 0;

    for (var column : treeTableView.getVisibleLeafColumns())
      total += column.getWidth();

    return total;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class StableBreadthVirtualFlow<I extends IndexedCell<?>> extends VirtualFlow<I>
  {
    private final DoubleSupplier contentBreadth;

  //---------------------------------------------------------------------------

    private StableBreadthVirtualFlow(DoubleSupplier contentBreadth)
    {
      this.contentBreadth = contentBreadth;
    }

  //---------------------------------------------------------------------------

    // Same shape as the stock (vertical) resizeCell, except the breadth comes from the stable visible-leaf-column-width
    // sum rather than the oscillating getMaxPrefBreadth(). That stability is what breaks the perpetual layout loop.

    @Override protected void resizeCell(I cell)
    {
      if (cell == null) return;

      double breadth = Math.max(contentBreadth.getAsDouble(), viewportBreadth()),
             length  = getFixedCellSize() > 0 ? getFixedCellSize() : boundedSize(cell.prefHeight(breadth), cell.minHeight(breadth), cell.maxHeight(breadth));

      cell.resize(breadth, length);
    }

  //---------------------------------------------------------------------------

    // Reconstructs the package-private VirtualFlow.getViewportBreadth(): the flow's width minus the vertical scrollbar
    // (when shown) and the horizontal insets. In the overflow case that triggers the bug, the content breadth wins the
    // max() above anyway, so any sub-pixel imprecision here is harmless.

    private double viewportBreadth()
    {
      ScrollBar vScrollBar = getVbar();
      double vScrollBarWidth = ((vScrollBar != null) && vScrollBar.isVisible()) ? vScrollBar.prefWidth(-1) : 0.0;

      return getWidth() - vScrollBarWidth - snappedLeftInset() - snappedRightInset();
    }

  //---------------------------------------------------------------------------

    // Mirrors com.sun.javafx.util.Utils.boundedSize (not public API): clamp value into [min, max], with min winning
    // when max < min. Not Math.clamp: this must match what the stock VirtualFlow.resizeCell does, and Math.clamp throws
    // IllegalArgumentException when min > max, whereas Utils.boundedSize (and therefore stock resizeCell) returns min.

    private static double boundedSize(double pref, double min, double max)
    {
      return Math.min(Math.max(pref, min), Math.max(min, max));
    }

  //---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
