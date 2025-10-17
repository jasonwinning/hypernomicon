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

package org.hypernomicon.view.controls;

import static org.hypernomicon.App.app;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.CustomMenuItem;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;

//---------------------------------------------------------------------------

public class CheckBoxMenuItem extends CustomMenuItem
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final BooleanProperty selectedProperty;

//---------------------------------------------------------------------------

  public CheckBoxMenuItem(String text, ReadOnlyBooleanProperty menuShowingProperty)
  {
    super(new HBox(), false);

    CheckBox checkBox = new CheckBox(text);
    selectedProperty = checkBox.selectedProperty();

    HBox hbox = (HBox) getContent();
    hbox.getChildren().add(checkBox);
    HBox.setHgrow(checkBox, Priority.ALWAYS);
    checkBox.setMaxWidth(Double.MAX_VALUE);

    hbox.addEventFilter(MouseEvent.MOUSE_PRESSED, event ->
    {
      checkBox.setSelected(!checkBox.isSelected());
      event.consume();
    });

    menuShowingProperty.addListener((obs, ov, nv) ->
    {
      if ((Boolean.TRUE.equals(nv) == false) || (nv.equals(ov))) return;

      Platform.runLater(() ->
      {
        ContextMenu popup = getParentPopup();
        if (popup == null)
        {
          if (app.debugging)
            System.out.println("Queries tab File Actions button checkbox menu failed to get reference to parent menu");

          return;
        }

        double width = popup.getWidth();
        if (width == 0.0)
        {
          if (app.debugging)
            System.out.println("Queries tab File Actions button checkbox menu failed to get parent menu width");

          return;
        }

        hbox.setPrefWidth(width);
      });
    });
  }

//---------------------------------------------------------------------------

  public boolean isSelected()               { return selectedProperty.get(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
