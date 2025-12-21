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

import java.util.*;
import java.util.Map.Entry;

import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.ToolBar;
import javafx.scene.layout.Pane;

import static org.hypernomicon.util.UIUtil.*;

//---------------------------------------------------------------------------

/**
 * Public API for managing visibility in sequential layout containers
 * without leaving gaps when nodes are hidden.
 */
public final class SequentialLayoutWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final ObservableList<Node> nodeList;
  private final Map<Node, Boolean> nodeToVisibleMap;

  private boolean changed             = true,
                  listenToVisibleProp = true,
                  syncVisibleProp     = true;

//---------------------------------------------------------------------------

  private SequentialLayoutWrapper(ObservableList<Node> nodeList)
  {
    this.nodeList = nodeList;

    nodeToVisibleMap = new LinkedHashMap<>();

    nodeList.forEach(node ->
    {
      nodeToVisibleMap.put(node, node.isVisible());

      node.visibleProperty().addListener((obs, ov, nv) ->
      {
        if (listenToVisibleProp)
        {
          syncVisibleProp = false;
          setVisible(nv, node);
          syncVisibleProp = true;
        }
      });
    });

    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setVisibleNoUpdate(boolean newValue, Node... nodes)
  {
    for (Node node : nodes)
      if (nodeToVisibleMap.containsKey(node) && (Boolean.TRUE.equals(nodeToVisibleMap.put(node, newValue)) != newValue))
      {
        changed = true;

        if (syncVisibleProp)
        {
          listenToVisibleProp = false;
          node.setVisible(newValue);
          listenToVisibleProp = true;
        }

        if (newValue)             // If the node was in a popup window and not visible
          scaleNodeForDPI(node);  // originally, it may not have been scaled yet
      }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void update()
  {
    if (changed == false) return;

    List<Node> newList = nodeToVisibleMap.entrySet().stream().filter(Entry::getValue).map(Entry::getKey).toList();

    if (newList.equals(nodeList)) return;

    nodeList.setAll(newList);

    changed = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setVisible(boolean newValue, Node... nodes)
  {
    setVisibleNoUpdate(newValue, nodes);

    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Factory methods

  /**
   * Creates a {@code SequentialLayoutWrapper} for managing the visible state of
   * nodes within a {@link ToolBar} without leaving empty gaps when nodes are hidden.
   * <p>
   * This factory method adapts the given {@code ToolBar} to the sequential layout
   * management API, allowing you to toggle the visibility of its child nodes
   * while preserving their order and automatically collapsing space for hidden nodes.
   * </p>
   *
   * @param toolBar the {@code ToolBar} whose child nodes will be managed;
   *                must not be {@code null}
   * @return a new {@code SequentialLayoutWrapper} bound to the specified {@code ToolBar}
   */
  public static SequentialLayoutWrapper forToolBar(ToolBar toolBar)
  {
    return new SequentialLayoutWrapper(toolBar.getItems());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates a {@code SequentialLayoutWrapper} for managing the visible state of
   * nodes within a generic {@link Pane} without leaving empty gaps when nodes are hidden.
   * <p>
   * This factory method adapts the given {@code Pane} to the sequential layout
   * management API, allowing you to toggle the visibility of its child nodes
   * while preserving their order and automatically collapsing space for hidden nodes.
   * </p>
   *
   * @param pane the {@code Pane} whose child nodes will be managed;
   *             must not be {@code null}
   * @return a new {@code SequentialLayoutWrapper} bound to the specified {@code Pane}
   */
  public static SequentialLayoutWrapper forPane(Pane pane)
  {
    return new SequentialLayoutWrapper(pane.getChildren());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
