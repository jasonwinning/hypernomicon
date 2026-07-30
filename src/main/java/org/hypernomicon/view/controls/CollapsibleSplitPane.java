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

package org.hypernomicon.view.controls;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.Node;
import javafx.scene.control.SplitPane;
import javafx.scene.layout.Region;

//---------------------------------------------------------------------------

/**
 * A two-item horizontal split pane whose right-hand (detail) node collapses to
 * a {@value #COLLAPSED_DETAIL_WIDTH}px sliver instead of being removed from
 * the scene graph, used by the dialogs that host a preview pane
 * (WorkDlgCtrlr, MergeWorksDlgCtrlr).
 * <p>
 * This replaced ControlsFX's {@code MasterDetailPane}, whose skin removes the
 * detail node from its internal SplitPane when the detail is hidden and
 * re-adds it when shown again. A JxBrowser hardware-accelerated browser view
 * inside the detail pane cannot survive that round trip: the native surface is
 * re-attached with only a size (position is corrected exclusively by later
 * change events on the view's transform, its size, and the window's location),
 * and a pane restored to exactly its former position and size fires none of
 * those, leaving the surface parked at the window origin (pdf.js floating
 * over the left side of the dialog until the window was moved). Keeping the
 * detail node in the scene at all times means the browser view is attached
 * exactly once and only ever resized, which the surface tracks reliably.
 * <p>
 * Collapsing pins the detail node's min/max width to the sliver width rather
 * than zero: JxBrowser ignores bounds updates with a zero dimension, so a
 * zero-width pane would leave the full-size native surface painting over the
 * dialog (the surface does not clip to JavaFX bounds). The pinning also makes
 * the collapsed state stable: layout re-asserts the sliver no matter where the
 * divider is dragged.
 * <p>
 * Conceptual cousin of {@link org.hypernomicon.view.wrappers.OneTouchExpandableWrapper},
 * which also collapses by divider position without removing nodes from the
 * scene; that wrapper's state model and machinery are built around the
 * expand/collapse buttons it installs on the divider, whereas this control is
 * driven by an external toggle through {@link #detailShowingProperty()} and
 * enforces its collapsed state through width pinning.
 */
public final class CollapsibleSplitPane extends SplitPane
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Width in px of the collapsed detail node. Must be nonzero; see the class
   *  comment. Consumers sizing against the detail pane (DialogPreviewHost)
   *  treat anything at or below this as not laid out. */
  public static final double COLLAPSED_DETAIL_WIDTH = 1.0;

  private final Region detailNode;
  private final BooleanProperty detailShowing = new SimpleBooleanProperty(false);

  /** Divider position to restore on expand; updated from the live divider on
   *  each collapse so a user adjustment survives the round trip. */
  private double expandedDividerPosition = 0.55;

//---------------------------------------------------------------------------

  public CollapsibleSplitPane(Node masterNode, Region detailNode)
  {
    this.detailNode = detailNode;

    getItems().addAll(masterNode, detailNode);

    // Both items keep the default resizable-with-parent flag, so a stage
    // resize preserves the divider's relative position rather than the detail
    // node's width in pixels. That matters because the dialogs widen their
    // stage in the same call that first shows the detail node, and the window
    // manager applies the resize asynchronously: a pixel-preserving resize
    // would land after the divider is positioned and pin the detail pane back
    // to (near) the sliver width it had while collapsed.

    detailShowing.addListener((ob, ov, nv) ->
    {
      if (Boolean.TRUE.equals(nv)) expand();
      else                         collapse(false);
    });

    // The divider node is created by the skin, which does not exist yet when
    // the initial collapse below runs; re-apply the divider style once it does
    // (a pulse later, so the skin has built its children).

    skinProperty().addListener((ob, oldSkin, newSkin) -> Platform.runLater(this::updateDividerStyle));

    collapse(true);  // initial state
  }

//---------------------------------------------------------------------------

  public BooleanProperty detailShowingProperty()  { return detailShowing; }
  public boolean isDetailShowing()                { return detailShowing.get(); }
  public void setDetailShowing(boolean showing)   { detailShowing.set(showing); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets the divider position the expanded state uses, applying it immediately
   * if the detail node is currently showing.
   */
  public void setExpandedDividerPosition(double position)
  {
    expandedDividerPosition = position;

    if (isDetailShowing())
      setDividerPositions(position);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void collapse(boolean initial)
  {
    if (initial == false)
      expandedDividerPosition = getDividerPositions()[0];

    detailNode.setMinWidth(COLLAPSED_DETAIL_WIDTH);
    detailNode.setMaxWidth(COLLAPSED_DETAIL_WIDTH);

    updateDividerStyle();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void expand()
  {
    detailNode.setMinWidth(USE_COMPUTED_SIZE);
    detailNode.setMaxWidth(Double.MAX_VALUE);

    setDividerPositions(expandedDividerPosition);

    updateDividerStyle();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Hides the divider while collapsed (zero width, no fill): with the detail
   * node pinned to its sliver there is nothing to drag, and a visible divider
   * reads as a stray bar on the window edge. Restored on expand.
   */
  private void updateDividerStyle()
  {
    Node divider = lookup(".split-pane-divider");

    if (divider != null)
      divider.setStyle(isDetailShowing() ? "" : "-fx-padding: 0; -fx-background-color: transparent;");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
