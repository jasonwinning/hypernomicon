/*
 * Copyright 2015-2025 Jason Winning
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

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import javafx.scene.Node;
import javafx.scene.control.ToolBar;

import static org.hypernomicon.util.UIUtil.*;

//---------------------------------------------------------------------------

public class ToolBarWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final ToolBar toolBar;
  private final Map<Node, Boolean> allItems;

  private boolean changed = false;

//---------------------------------------------------------------------------

  public ToolBarWrapper(ToolBar toolBar)
  {
    this.toolBar = toolBar;

    allItems = new LinkedHashMap<>();

    toolBar.getItems().forEach(node -> allItems.put(node, node.isVisible()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setVisibleNoUpdate(boolean newValue, Node... nodes)
  {
    for (Node node : nodes)
      if (allItems.containsKey(node) && (allItems.put(node, newValue) != newValue))
      {
        changed = true;

        if (newValue)             // If the node was in a popup window and not visible
          scaleNodeForDPI(node);  // originally, it may not have been scaled yet
      }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void update()
  {
    if (changed == false) return;

    toolBar.getItems().setAll(allItems.entrySet().stream().filter(Entry::getValue).map(Entry::getKey).toList());

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

}
