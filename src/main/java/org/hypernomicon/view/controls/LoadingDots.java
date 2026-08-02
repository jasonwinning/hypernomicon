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

import javafx.animation.*;
import javafx.geometry.Pos;
import javafx.scene.layout.HBox;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.util.Duration;

//---------------------------------------------------------------------------

/**
 * Animated three-dot loading indicator shown in place of a button's normal
 * graphic: the dots fade in and out in sequence, each one fading in as its
 * predecessor fades out, for as long as the animation plays. Sized to stand
 * in for a standard 16x16 button icon.
 */
public final class LoadingDots extends HBox
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final double DOT_RADIUS = 2.0, DOT_SPACING = 2.0;
  private static final Duration STEP = Duration.millis(300);

  private final Timeline timeline = new Timeline();

//---------------------------------------------------------------------------

  public LoadingDots()
  {
    super(DOT_SPACING);

    setAlignment(Pos.CENTER);
    setPrefSize(16.0, 16.0);

    Circle dotA = newDot(), dotB = newDot(), dotC = newDot();

    // Opacity keyframes, one column per step boundary: each dot fades in over
    // one step and back out over the next, offset one step from its
    // predecessor. The third dot's fade-out wraps into the start of the next
    // cycle, which is why it peaks at the cycle boundaries.

    Duration step2 = STEP.multiply(2), step3 = STEP.multiply(3);

    timeline.getKeyFrames().addAll
    (
      new KeyFrame(Duration.ZERO, new KeyValue(dotA.opacityProperty(), 0.0),
                                  new KeyValue(dotB.opacityProperty(), 0.0),
                                  new KeyValue(dotC.opacityProperty(), 1.0)),
      new KeyFrame(STEP         , new KeyValue(dotA.opacityProperty(), 1.0),
                                  new KeyValue(dotB.opacityProperty(), 0.0),
                                  new KeyValue(dotC.opacityProperty(), 0.0)),
      new KeyFrame(step2        , new KeyValue(dotA.opacityProperty(), 0.0),
                                  new KeyValue(dotB.opacityProperty(), 1.0),
                                  new KeyValue(dotC.opacityProperty(), 0.0)),
      new KeyFrame(step3        , new KeyValue(dotA.opacityProperty(), 0.0),
                                  new KeyValue(dotB.opacityProperty(), 0.0),
                                  new KeyValue(dotC.opacityProperty(), 1.0))
    );

    timeline.setCycleCount(Animation.INDEFINITE);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Circle newDot()
  {
    Circle dot = new Circle(DOT_RADIUS, Color.GRAY);
    dot.setOpacity(0.0);
    getChildren().add(dot);
    return dot;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void play() { timeline.play(); }
  public void stop() { timeline.stop(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
