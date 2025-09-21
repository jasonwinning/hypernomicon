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

package org.hypernomicon;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.layout.HBox;

import org.hypernomicon.view.wrappers.SequentialLayoutWrapper;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.FxTestUtil.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

//---------------------------------------------------------------------------

class SequentialLayoutWrapperTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @BeforeAll
  static void setupFx()
  {
    initJfx();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void externalChangeUpdatesNodeList()
  {
    HBox box = new HBox(new Button("A"), new Button("B"));
    SequentialLayoutWrapper wrapper = SequentialLayoutWrapper.forPane(box);
    noOp(wrapper);

    Node b = box.getChildren().get(1);

    runFxAndWait(() -> b.setVisible(false));

    assertFalse(box.getChildren().contains(b), "B should be removed from nodeList");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void internalChangeUpdatesPropertyAndNodeList()
  {
    HBox box = new HBox(new Button("A"), new Button("B"));
    SequentialLayoutWrapper wrapper = SequentialLayoutWrapper.forPane(box);

    Node b = box.getChildren().get(1);

    runFxAndWait(() -> wrapper.setVisible(false, b));

    assertFalse(b.isVisible());
    assertFalse(box.getChildren().contains(b));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void boundPropertyExternalChangeDoesNotThrow()
  {
    HBox box = new HBox(new Button("A"));
    SequentialLayoutWrapper wrapper = SequentialLayoutWrapper.forPane(box);
    noOp(wrapper);

    Node a = box.getChildren().getFirst();
    BooleanProperty model = new SimpleBooleanProperty(true);

    runFxAndWait(() -> a.visibleProperty().bind(model));

    assertDoesNotThrow(() -> runFxAndWait(() -> model.set(false)));
    assertFalse(box.getChildren().contains(a));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void boundPropertyInternalChangeDoesThrow()
  {
    HBox box = new HBox(new Button("A"));
    SequentialLayoutWrapper wrapper = SequentialLayoutWrapper.forPane(box);

    Node a = box.getChildren().getFirst();
    BooleanProperty model = new SimpleBooleanProperty(true);

    runFxAndWait(() -> a.visibleProperty().bind(model));

    assertThrows(RuntimeException.class,
        () -> runFxAndWait(() -> wrapper.setVisible(false, a)),
        "Calling setVisible on a bound node should throw"
      );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
