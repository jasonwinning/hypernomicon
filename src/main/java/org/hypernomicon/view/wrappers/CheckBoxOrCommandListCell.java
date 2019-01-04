/*
 * Copyright 2015-2019 Jason Winning
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

import org.hypernomicon.util.Util;

import com.sun.javafx.scene.control.skin.ComboBoxListViewSkin;

import org.hypernomicon.view.wrappers.CheckBoxOrCommandListCell.CheckBoxOrCommand;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.ListCell;
import javafx.scene.control.Skin;
import javafx.util.StringConverter;

@SuppressWarnings("restriction")
public class CheckBoxOrCommandListCell extends ListCell<CheckBoxOrCommand>
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static class CheckBoxOrCommand
  {
    public CheckBoxOrCommand(String text, ObservableValue<Boolean> booleanProperty) { this.text = text; this.booleanProperty = booleanProperty; }
    public CheckBoxOrCommand(String text, Runnable hndlr)                           { this.text = text; this.hndlr = hndlr; }

    private ObservableValue<Boolean> booleanProperty = null;
    private String text;
    private Runnable hndlr = null;

    public String getText() { return text; }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    public static ComboBox<CheckBoxOrCommand> createComboBox(ObservableList<CheckBoxOrCommand> items, String caption)
    {
      ComboBox<CheckBoxOrCommand> cb = new ComboBox<CheckBoxOrCommand>(items)
      {
        @Override protected Skin<?> createDefaultSkin()
        {
          return new ComboBoxListViewSkin<CheckBoxOrCommand>(this)
          {
            // overridden to prevent the popup from disappearing
            @Override protected boolean isHideOnClickEnabled() { return false; }
          };
        }
      };

      cb.setConverter(new StringConverter<CheckBoxOrCommand>()
      {
        @Override public String toString(CheckBoxOrCommand object)   { return caption; }
        @Override public CheckBoxOrCommand fromString(String string) { return new CheckBoxOrCommand("", Util::noOp); }
      });

      cb.setCellFactory(listView -> new CheckBoxOrCommandListCell());

      cb.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
      {
        if (newValue.getText().length() == 0) return;

        Platform.runLater(() -> cb.getSelectionModel().select(new CheckBoxOrCommand("", Util::noOp)));
      });

      cb.getSelectionModel().select(items.get(0));

      return cb;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private CheckBox checkBox;
  private ObservableValue<Boolean> booleanProperty = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public CheckBoxOrCommandListCell()
  {
    checkBox = new CheckBox();

    setAlignment(Pos.CENTER_LEFT);
    setContentDisplay(ContentDisplay.LEFT);

    // by default the graphic is null until the cell stops being empty
    setGraphic(null);

    //clicking on the label checks/unchecks the item
    setOnMouseClicked(event ->
    {
      CheckBoxOrCommand item = getItem();

      if (item.booleanProperty != null)
        checkBox.setSelected(!booleanProperty.getValue());
      else
        item.hndlr.run();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** {@inheritDoc} */
  @Override public void updateItem(CheckBoxOrCommand item, boolean empty)
  {
    super.updateItem(item, empty);

    if (empty)
    {
      setGraphic(null);
      setText(null);
      return;
    }

    setText(item.text);
    setGraphic(checkBox);

    if (booleanProperty != null)
      checkBox.selectedProperty().unbindBidirectional((BooleanProperty) booleanProperty);

    booleanProperty = item.booleanProperty;

    if (booleanProperty != null)
    {
      checkBox.setVisible(true);
      checkBox.setSelected(booleanProperty.getValue());
      checkBox.selectedProperty().bindBidirectional((BooleanProperty) booleanProperty);
    }
    else
      checkBox.setVisible(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
