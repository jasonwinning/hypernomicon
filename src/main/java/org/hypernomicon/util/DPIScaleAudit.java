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

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.HashSet;
import java.util.Set;

import javafx.beans.property.DoubleProperty;
import javafx.collections.ListChangeListener;
import javafx.css.StyleOrigin;
import javafx.css.StyleableProperty;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Region;
import javafx.stage.*;

//---------------------------------------------------------------------------

/**
 * Debug-mode check that {@link UIUtil#scaleNodeForDPI} reached every node it
 * should have. Installed once (when the application runs under a debugger); it
 * then walks the live scene graph of every window shortly after the window is
 * shown, by which time skins exist and {@code Parent.getChildrenUnmodifiable()}
 * reaches everything, and logs each node that carries explicit geometry (a
 * min/pref/max size set in FXML or code, or a positive anchor constraint) but
 * never received the scaled marker. Such a node was missed by the scaling walk,
 * almost always because a container between it and the nearest scaled ancestor
 * holds it through its own API and has no branch in the scaling chain; see
 * {@link DPIScalableContainer}.
 * <p>
 * Each finding is reported once, keyed by the path of class names (with fx:ids)
 * from the nearest scaled ancestor down to the node, so the same structural
 * miss in a dialog opened repeatedly logs a single line, and that line names the
 * container that needs a branch. Sizes that come from a stylesheet are not
 * counted as explicit (they are re-applied by CSS and are not what the walk
 * scales), which keeps skin-internal regions out of the report. Nodes under a
 * {@code noScale} node are exempt, as they are from scaling; a window that is
 * left unscaled on purpose (About, Welcome) carries that class on its root.
 * <p>
 * Limits: a node positioned only by layoutX/layoutY is not detectable after
 * layout has run (layout writes those on every node), so a missed subtree is
 * reported through whichever of its nodes has a size or anchor; and content
 * added to a window after it was shown is only checked if the window is shown
 * again.
 */
public final class DPIScaleAudit
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Set<String> reported = new HashSet<>();

  private DPIScaleAudit() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Audits every window from now on, a couple of pulses after it is shown.
   * Popup windows (menus, tooltips, combo box lists) are skipped: nothing in
   * them is scaled or meant to be.
   */
  public static void install()
  {
    Window.getWindows().addListener((ListChangeListener<Window>) change ->
    {
      while (change.next())
      {
        if (change.wasAdded() == false) continue;

        change.getAddedSubList().forEach(window ->
        {
          if ((window instanceof PopupWindow) == false)
            runInFXThreadAfterPulses(2, () -> audit(window));
        });
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void audit(Window window)
  {
    if ((window.getScene() == null) || (window.getScene().getRoot() == null)) return;

    String windowDesc = (window instanceof Stage stage) && strNotNullOrBlank(stage.getTitle()) ?
      '"' + stage.getTitle() + '"'
    :
      window.getClass().getSimpleName();

    visit(window.getScene().getRoot(), "", windowDesc);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void visit(Node node, String pathFromScaledAncestor, String windowDesc)
  {
    if (node.getStyleClass().contains("noScale")) return;

    boolean scaled = getIsScaled(node);

    String path = scaled || pathFromScaledAncestor.isEmpty() ? describe(node) : pathFromScaledAncestor + " > " + describe(node);

    if ((scaled == false) && hasExplicitGeometry(node) && reported.add(path))
      System.out.println("DPI scale audit: node not scaled in window " + windowDesc + ": " + path);

    if (node instanceof Parent parent)
      parent.getChildrenUnmodifiable().forEach(child -> visit(child, path, windowDesc));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Mirrors what the scaling walk would have changed: a positive anchor (a zero
   * anchor scales to itself) or a positive, unbound size set from FXML or code.
   */
  private static boolean hasExplicitGeometry(Node node)
  {
    if (isPositive(AnchorPane.getTopAnchor (node)) || isPositive(AnchorPane.getBottomAnchor(node)) ||
        isPositive(AnchorPane.getLeftAnchor(node)) || isPositive(AnchorPane.getRightAnchor (node)))
      return true;

    if (node instanceof Region region)
      return isExplicit(region.prefWidthProperty()) || isExplicit(region.prefHeightProperty()) ||
             isExplicit(region.minWidthProperty ()) || isExplicit(region.minHeightProperty ()) ||
             isExplicit(region.maxWidthProperty ()) || isExplicit(region.maxHeightProperty ());

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean isPositive(Double anchor)
  {
    return (anchor != null) && (anchor > 0.0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * True for a positive size set from FXML or code (style origin USER), which
   * is what the scaling walk multiplies. Sizes from a stylesheet, the
   * computed-size sentinels, and bound sizes (the walk leaves those alone; they
   * derive from live layout) do not count.
   */
  private static boolean isExplicit(DoubleProperty sizeProp)
  {
    return (sizeProp.get() > 0.0) && (sizeProp.isBound() == false) &&
           (sizeProp instanceof StyleableProperty<?> styleable) &&
           (styleable.getStyleOrigin() == StyleOrigin.USER);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String describe(Node node)
  {
    String name = node.getClass().getSimpleName();

    if (name.isEmpty())
      name = node.getClass().getName();  // anonymous class

    return strNullOrBlank(node.getId()) ? name : name + '#' + node.getId();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
