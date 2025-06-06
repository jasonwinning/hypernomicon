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

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Pane;

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

    if ((parent instanceof Pane pane) && !(parent instanceof GridPane))
    {
      List<Node> children = pane.getChildren();
      children.add(children.indexOf(btn), btnMenu);
    }
    else
      errorPopup("Unsupported parent type for ClickHoldButton");

    copyRegionLayout(btn, btnMenu);

    btnMenu.visibleProperty().bind(btn.disabledProperty().not());

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
