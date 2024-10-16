/*
 * Copyright 2015-2024 Jason Winning
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

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.HyperTask.HyperThread;

import static org.hypernomicon.view.wrappers.OneTouchExpandableWrapper.CollapsedState.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import javafx.beans.binding.Bindings;
import javafx.beans.binding.DoubleBinding;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Orientation;
import javafx.geometry.Pos;
import javafx.scene.Cursor;
import javafx.scene.control.Button;
import javafx.scene.control.SplitPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.shape.Polygon;
import javafx.stage.Window;

//---------------------------------------------------------------------------

public final class OneTouchExpandableWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Thickness of the divider in pixels.
   */
  private static final double DIVIDER_THICKNESS = 12.0;

  /**
   * Length of button in pixels.
   */
  private static final double BUTTON_LENGTH = 70.0;

  /**
   * Space between buttons in pixels.
   */
  private static final double BUTTON_SPACING = 3.0;

  /**
   * Property key whose value is the divider position when it
   * was last in expanded state
   */
  private static final Object EXPANDED_POSITION = OneTouchExpandableWrapper.class.getName() + ".expandedPosition";

  /**
   * Percentage of button thickness which the triangle takes up, as a value
   * from 0.0 to 1.0.
   */
  private static final double TRIANGLE_THICKNESS = 0.6;

  /**
   * Delta for comparing divider position (a double) with the min and max
   * divider values, which in practice do not reach 0.0 or 1.0.
   */
  private static final double END_TOLERANCE = 0.025;

  private final SplitPane splitPane;
  private final DoubleBinding dividerPosition;
  private final HyperThread thread;

  private Button button1, button2;

//---------------------------------------------------------------------------

  private OneTouchExpandableWrapper(SplitPane splitPane, double expandedPos, CollapsedState collapsedState)
  {
    this.splitPane = splitPane;

    dividerPosition = Bindings.selectDouble(Bindings.valueAt(splitPane.getDividers(), 0), "position");

    thread = addOneTouchExpansion(expandedPos, collapsedState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * State of whether the SplitPane regions are expanded or collapsed.
   * <p>
   * Values:<br>
   *   ShowingOnlyFirstRegion,<br>
   *   ShowingOnlySecondRegion,<br>
   *   Expanded
   */
  public enum CollapsedState
  {
    ShowingOnlyFirstRegion,
    ShowingOnlySecondRegion,
    Expanded
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Wrapper to add buttons to SplitPane to expand/collapse
   * @param splitPane The SplitPane
   * @param expandedPos The position of the divider when expanded, from 0.0 to 1.0
   * @param collapsedState Whether to start expanded or collapsed
   * @return The wrapper
   */
  public static OneTouchExpandableWrapper wrap(SplitPane splitPane, double expandedPos, CollapsedState collapsedState)
  {
    return new OneTouchExpandableWrapper(splitPane, expandedPos, collapsedState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static StackPane findDivider(SplitPane splitPane)
  {
    return splitPane.lookupAll(".split-pane-divider").stream().filter(node -> (node instanceof StackPane stackPane) && (stackPane.getParent() == splitPane))
                                                              .map(node -> (StackPane) node)
                                                              .findFirst().orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final MutableBoolean oneTouchAdded = new MutableBoolean(false);

  /**
   * Adds one-touch-expand buttons to a SplitPane's first divider.<br>
   * Similar to ResultsTable.reset
   */
  private HyperThread addOneTouchExpansion(double expandedPos, CollapsedState collapsedState)
  {
    splitPane.setDividerPosition(0, expandedPos);

    HyperThread thread = new HyperThread("OneTouchAdder")
    {

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      @Override public void run()
      {
        boolean oneTouchNotAdded;

        synchronized (oneTouchAdded) { oneTouchNotAdded = oneTouchAdded.isFalse(); }

        while (oneTouchNotAdded)
        {
          runInFXThread(() ->
          {
            synchronized (oneTouchAdded)
            {
              oneTouchAdded.setValue(oneTouchAdded.booleanValue() || !nullSwitch(splitPane.getScene(), false, scene ->
                                                                      nullSwitch(scene.getWindow(), false, Window::isShowing)));
              if (oneTouchAdded.isTrue()) return;

              nullSwitch(findDivider(splitPane), divider -> initDivider(splitPane, divider));
            }
          }, true);

          synchronized (oneTouchAdded) { oneTouchNotAdded = oneTouchAdded.isFalse(); }

          if (oneTouchNotAdded)
            sleepForMillis(50);
        }
      }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

      private void initDivider(SplitPane splitPane, StackPane divider)
      {
        oneTouchAdded.setTrue();

        BorderPane borderPane = new BorderPane();

        divider.setPrefSize(DIVIDER_THICKNESS, DIVIDER_THICKNESS);
        divider.getChildren().add(borderPane);

        borderPane.minWidthProperty  ().bind(divider.widthProperty ());
        borderPane.prefWidthProperty ().bind(divider.widthProperty ());
        borderPane.maxWidthProperty  ().bind(divider.widthProperty ());

        borderPane.minHeightProperty ().bind(divider.heightProperty());
        borderPane.prefHeightProperty().bind(divider.heightProperty());
        borderPane.maxHeightProperty ().bind(divider.heightProperty());

        if (splitPane.getOrientation() == Orientation.VERTICAL)
        {
          ObservableValue<Number> triangleScaleFactor = divider.heightProperty().multiply(TRIANGLE_THICKNESS);

          button1 = createTriangleButton(triangleScaleFactor, 0, DIVIDER_THICKNESS, BUTTON_LENGTH, 1, 0, 0, 1, 2, 1);
          button2 = createTriangleButton(triangleScaleFactor, 1, DIVIDER_THICKNESS, BUTTON_LENGTH, 1, 1, 0, 0, 2, 0);

          HBox expandButtonsPane = new HBox(button1, button2);
          expandButtonsPane.setAlignment(Pos.CENTER);
          expandButtonsPane.setSpacing(BUTTON_SPACING);

          borderPane.setCenter(expandButtonsPane);
        }
        else
        {
          ObservableValue<Number> triangleScaleFactor = divider.widthProperty().multiply(TRIANGLE_THICKNESS);

          button1 = createTriangleButton(triangleScaleFactor, 0, BUTTON_LENGTH, DIVIDER_THICKNESS, 0, 1, 1, 0, 1, 2);
          button2 = createTriangleButton(triangleScaleFactor, 1, BUTTON_LENGTH, DIVIDER_THICKNESS, 1, 1, 0, 0, 0, 2);

          VBox expandButtonsPane = new VBox(button1, button2);
          expandButtonsPane.setAlignment(Pos.CENTER);
          expandButtonsPane.setSpacing(BUTTON_SPACING);

          borderPane.setCenter(expandButtonsPane);
        }

        if (collapsedState != Expanded)
          setDividerPosition(splitPane, collapsedState == ShowingOnlySecondRegion ? 0 : 1);
      }
    };

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    thread.setDaemon(true);
    thread.start();

    return thread;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Button createTriangleButton(ObservableValue<Number> triangleScaleFactor, int endPos, double height, double width, double... points)
  {
    Polygon triangle = new Polygon(points);

    triangle.scaleXProperty().bind(triangleScaleFactor);
    triangle.scaleYProperty().bind(triangleScaleFactor);
    triangle.setStyle("-fx-fill: -fx-text-base-color;");

    Button button = new Button("", triangle);

    button.setStyle("-fx-base: -fx-default-button;");

    setHeights(button, height);
    setWidths (button, width );

    button.setCursor(Cursor.DEFAULT);
    button.setOnMouseClicked(e -> setDividerPosition(splitPane, endPos));
    button.disableProperty().bind(dividerPosition.isEqualTo(endPos, END_TOLERANCE));

    return button;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Checks whether double values are nearly equal.
   */
  private static boolean isNearly(double value, double target)
  {
    return Math.abs(target - value) < END_TOLERANCE;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setCollapsedState(CollapsedState newCollapsedState)
  {
    if (HyperThread.isRunning(thread))
    {
      try { thread.join(); } catch (InterruptedException e) { throw new AssertionError(e); }
    }

    setCollapsedStateIteration(newCollapsedState);
    setCollapsedStateIteration(newCollapsedState);  // May need to simulate clicking the button twice if going from one extreme to the other
  }

  private void setCollapsedStateIteration(CollapsedState newCollapsedState)
  {
    CollapsedState curCollapsedState;

    if      (button1.isDisabled()) curCollapsedState = ShowingOnlySecondRegion;
    else if (button2.isDisabled()) curCollapsedState = ShowingOnlyFirstRegion;
    else                           curCollapsedState = Expanded;

    if (newCollapsedState == curCollapsedState) return;

    if      (newCollapsedState == Expanded)               setDividerPosition(splitPane, curCollapsedState == ShowingOnlyFirstRegion ? 0 : 1);
    else if (newCollapsedState == ShowingOnlyFirstRegion) setDividerPosition(splitPane, 1);
    else                                                  setDividerPosition(splitPane, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Executes a one-touch expand/collapse
   *
   * @param splitPane SplitPane to perform expand/collapse on
   * @param endPos farthest divider position in direction of expand/collapse, either 0 or 1
   */
  private static void setDividerPosition(SplitPane splitPane, double endPos)
  {
    double expandedPos = splitPane.getDividers().get(0).getPosition(),
           startPos = 1 - endPos;

    if (isNearly(expandedPos, startPos))
    {
      Object savedPosObj = splitPane.getProperties().get(EXPANDED_POSITION);

      splitPane.setDividerPosition(0, savedPosObj instanceof Number savedPos ?
        savedPos.doubleValue()
      :
        0.5);
    }
    else if (isNearly(expandedPos, endPos) == false)
    {
      splitPane.getProperties().put(EXPANDED_POSITION, expandedPos);
      splitPane.setDividerPosition(0, endPos);
    }

    splitPane.requestLayout();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
