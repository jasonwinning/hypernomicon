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

import javafx.beans.property.Property;
import org.hypernomicon.util.Util;
import org.hypernomicon.view.wrappers.CheckBoxOrCommandListCell.CheckBoxOrCommand;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.geometry.Pos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.ListCell;
import javafx.scene.control.Skin;
import javafx.scene.control.skin.ComboBoxListViewSkin;
import javafx.util.StringConverter;

public final class CheckBoxOrCommandListCell extends ListCell<CheckBoxOrCommand>
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static class CheckBoxOrCommand
  {
    public CheckBoxOrCommand(String text, ObservableValue<Boolean> booleanProperty) { this(text, booleanProperty, null); }
    public CheckBoxOrCommand(String text, Runnable hndlr)                           { this(text, null, hndlr); }

    private CheckBoxOrCommand(String text, ObservableValue<Boolean> booleanProperty, Runnable hndlr)
    {
      this.text = text;
      this.booleanProperty = booleanProperty;
      this.hndlr = hndlr;
    }

    private final ObservableValue<Boolean> booleanProperty;
    private final String text;
    private final Runnable hndlr;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    public static ComboBox<CheckBoxOrCommand> createComboBox(ObservableList<CheckBoxOrCommand> items, final String caption)
    {
      ComboBox<CheckBoxOrCommand> cb = new ComboBox<>(items)
      {
        @Override protected Skin<?> createDefaultSkin()
        {
          ComboBoxListViewSkin<CheckBoxOrCommand> skin = new ComboBoxListViewSkin<>(this);
          skin.setHideOnClick(false);
          return skin;
        }
      };

      cb.setConverter(new StringConverter<>()
      {
        @Override public String toString(CheckBoxOrCommand object)   { return caption; }
        @Override public CheckBoxOrCommand fromString(String string) { return new CheckBoxOrCommand(caption, Util::noOp); }
      });

      cb.setCellFactory(listView -> new CheckBoxOrCommandListCell());

      return cb;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final CheckBox checkBox;
  private ObservableValue<Boolean> booleanProperty = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private CheckBoxOrCommandListCell()
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
        checkBox.setSelected(booleanProperty.getValue() == false);
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
      checkBox.selectedProperty().unbindBidirectional((Property<Boolean>) booleanProperty);

    booleanProperty = item.booleanProperty;

    if (booleanProperty != null)
    {
      checkBox.setVisible(true);
      checkBox.setSelected(booleanProperty.getValue());
      checkBox.selectedProperty().bindBidirectional((Property<Boolean>) booleanProperty);
    }
    else
      checkBox.setVisible(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
