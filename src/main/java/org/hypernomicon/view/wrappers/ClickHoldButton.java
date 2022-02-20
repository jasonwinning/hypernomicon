/*
 * Copyright 2015-2022 Jason Winning
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
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
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
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Pane;

public class ClickHoldButton
{
  private boolean mouseDown = false;
  private EventHandler<ActionEvent> hndlr = null;
  private final MenuButton btnMenu;
  private final Button btn;
  private int mouseDownCtr = 0;

//---------------------------------------------------------------------------

  public void setDisable(boolean disable)                  { btn.setDisable(disable); }
  public void setOnAction(EventHandler<ActionEvent> hndlr) { this.hndlr = hndlr; }
  public List<MenuItem> getMenu()                          { return btnMenu.getItems(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ClickHoldButton(Button btn, Side side)
  {
    btnMenu = new MenuButton("");
    btnMenu.setPopupSide(side);
    this.btn = btn;
    Parent parent = btn.getParent();

    if ((parent instanceof Pane) && !(parent instanceof GridPane))
    {
      List<Node> children = Pane.class.cast(parent).getChildren();
      children.add(children.indexOf(btn), btnMenu);
    }
    else
      messageDialog("Unsupported parent type for ClickHoldButton", mtError);

    copyRegionLayout(btn, btnMenu);

    btnMenu.visibleProperty().bind(btn.disabledProperty().not());

    getMenu().clear();

    repositionPopupListWorkaround(btnMenu);

    btn.setOnAction(event ->
    {
      if (btnMenu.isShowing()) return;

      if (hndlr != null)
        hndlr.handle(event);
    });

    btn.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton() == MouseButton.SECONDARY)
      {
        btnMenu.show();
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
            btnMenu.show();
      });
    });

    btn.setOnMouseReleased(mouseEvent ->
    {
      mouseDown = false;
      mouseDownCtr++;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
