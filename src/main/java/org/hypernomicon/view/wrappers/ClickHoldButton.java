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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Side;
import javafx.scene.Parent;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.*;

//---------------------------------------------------------------------------

public class ClickHoldButton
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface
  public interface MenuFactory { void rebuildMenu(List<MenuItem> menu); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean mouseDown = false;
  private EventHandler<ActionEvent> actionHndlr = null;
  private MenuFactory menuFactory = null;
  private final MenuButton btnMenu;
  private final Button btn;
  private int mouseDownCtr = 0;

//---------------------------------------------------------------------------

  public void setDisable(boolean disable)                  { btn.setDisable(disable);    }
  public void setOnAction(EventHandler<ActionEvent> hndlr) { this.actionHndlr = hndlr;   }
  public void setMenuFactory(MenuFactory factory)          { this.menuFactory = factory; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ClickHoldButton(Button btn, Side side)
  {
    btnMenu = new MenuButton("");
    btnMenu.setPopupSide(side);
    this.btn = btn;

    Parent parent = btn.getParent();

    if (parent == null)
    {
      btn.parentProperty().addListener(new ChangeListener<>()
      {
        // This is cleaner than using a lambda because it allows referring to "this"

        @Override public void changed(ObservableValue<? extends Parent> obs, Parent ov, Parent nv)
        {
          btn.parentProperty().removeListener(this);  // Remove this listener so it only runs once

          waitForParent();
        }

        private void waitForParent()
        {
          Parent p = btn.getParent();
          if (p != null)
            // Wait two pulses so ToolBarSkin finishes re-parenting/CSS/layout before modifying children
            runInFXThreadAfterPulses(2, () -> addMenuButtonToParent(p));
          else
            runInFXThreadAfterPulses(1, this::waitForParent);
        }
      });
    }
    else
    {
      addMenuButtonToParent(parent);
    }

    copyRegionLayout(btn, btnMenu);

    btnMenu.visibleProperty().bind(btn.disabledProperty().not());

    lockWidthTo (btn, btnMenu);
    lockHeightTo(btn, btnMenu);

    btnMenu.getItems().clear();

    repositionPopupListWorkaround(btnMenu);

    btn.setOnAction(event ->
    {
      if (btnMenu.isShowing()) return;

      if (actionHndlr != null)
        actionHndlr.handle(event);
    });

    btn.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton() == MouseButton.SECONDARY)
      {
        showMenu();
        mouseDown = false;
      }
    });

    btn.setOnMousePressed(mouseEvent ->
    {
      if (mouseEvent.getButton() != MouseButton.PRIMARY)
      {
        mouseDown = false;
        return;
      }

      mouseDown = true;
      int curMouseDownCtr = mouseDownCtr;

      runDelayedInFXThread(1, BUTTON_MENU_DELAY_MS, () ->
      {
        if (mouseDown)
          if (mouseDownCtr == curMouseDownCtr) // Prevent popup from getting shown when user clicks in rapid succession
            showMenu();
      });
    });

    btn.addEventFilter(MouseEvent.MOUSE_RELEASED, event ->
    {
      // This needs to be an event filter because if clicking the button
      // opens a modal popup, the onMouseReleased event won't fire until
      // after the modal popup closes.

      mouseDown = false;
      mouseDownCtr++;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addMenuButtonToParent(Parent parent)
  {
    if (parent == null) return;

    if ((parent instanceof Pane pane) && !(parent instanceof GridPane))
    {
      // Find the index of the button in the parent's children list
      int ndxInParent = pane.getChildren().indexOf(btn);

      if (ndxInParent == -1)
        throw new IllegalArgumentException("Button is not a child of the given parent");

      // Create the StackPane
      StackPane stackPane = new StackPane();

      // Replace the button with the stack in the parent
      pane.getChildren().set(ndxInParent, stackPane);

      // Add MenuButton first, then Button on top
      stackPane.getChildren().addAll(btnMenu, btn);

      // Copy layout constraints from the original button to the stack
      copyRegionLayout(btn, stackPane);

      btnMenu.setFocusTraversable(false);

      lockWidthTo (btn, stackPane);
      lockHeightTo(btn, stackPane);
    }
    else
      throw new IllegalArgumentException("Unsupported parent type for ClickHoldButton");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void showMenu()
  {
    btnMenu.getItems().clear();

    if (menuFactory != null)
      menuFactory.rebuildMenu(btnMenu.getItems());

    btnMenu.show();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
