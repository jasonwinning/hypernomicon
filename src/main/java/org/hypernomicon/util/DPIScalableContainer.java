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

package org.hypernomicon.util;

import java.util.List;

import javafx.scene.Node;

//---------------------------------------------------------------------------

/**
 * A node that holds content through its own API rather than as plain
 * scene-graph children, declaring that content so {@link UIUtil#scaleNodeForDPI}
 * can reach it.
 * <p>
 * The scaling walk descends through {@code Parent.getChildrenUnmodifiable()},
 * but a {@code Control}'s children are created by its skin, which usually does
 * not exist until the control is first shown, after the walk has run. Content a
 * control holds through its own property (a ScrollPane's content, a TabPane's
 * tabs, a SplitPane's items) is therefore invisible to the walk, and every such
 * type needs an explicit branch in the scaling chain. A missing branch fails
 * silently: the control's own sizes are scaled, its content is not, and nothing
 * reports it. Composite controls of this codebase implement this interface so
 * the chain does not need a branch per house type (JavaFX and third-party types
 * still need one); {@link DPIScaleAudit} catches whatever both miss.
 */
public interface DPIScalableContainer
{
  /**
   * The nodes this container holds through its own API. Null entries are
   * permitted (an unset slot) and skipped.
   */
  List<Node> dpiScalableChildren();
}
