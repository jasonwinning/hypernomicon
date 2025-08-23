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

import java.util.SequencedMap;

import org.hypernomicon.util.EventFilters;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import com.google.common.collect.HashBiMap;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.scene.Cursor;
import javafx.scene.control.*;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseEvent;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

public final class SimpleSelector<T>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean suppressPressOpen = false;

//---------------------------------------------------------------------------

  public static <T> void init(ComboBox<T> cb, SequencedMap<T, String> strMap)
  {
    noOp(new SimpleSelector<>(cb, strMap));
  }

  private SimpleSelector(ComboBox<T> cb, SequencedMap<T, String> strMap)
  {
    cb.setEditable(true);

    var strBiMap = HashBiMap.create(strMap);

    cb.setConverter(new StringConverter<>()
    {
      @Override public String toString  (T      val   ) { return strBiMap          .get(val   ); }
      @Override public T      fromString(String string) { return strBiMap.inverse().get(string); }
    });

    cb.setItems(null);
    cb.setItems(FXCollections.observableArrayList(strMap.sequencedKeySet()));

    var editor = cb.getEditor();

    editor.setEditable(false);
    editor.setCursor(Cursor.DEFAULT);
    editor.setContextMenu(new ContextMenu());

    cb.setOnHiding(event ->
    {
      suppressPressOpen = true;

      Platform.runLater(() -> suppressPressOpen = false);
    });

    EventFilters.addFilterIfAbsent(cb, MouseEvent.MOUSE_PRESSED, event ->
    {
      if (event.isPrimaryButtonDown())
      {
        if (suppressPressOpen)
        {
          event.consume();
          return;
        }

        if (cb.isShowing() == false)
        {
          cb.show();
          safeFocus(cb);
          event.consume();
        }
      }
    });

    EventFilters.replaceFilter(cb, KeyEvent.KEY_PRESSED, keyEvent ->
    {
      if (keyEvent.isAltDown() || keyEvent.isShortcutDown())
        return;

      String targetClass = keyEvent.getTarget().getClass().getName();
      if (targetClass.endsWith("FakeFocusTextField"))
        return;

      var items = cb.getItems();
      int size = items.size();
      if (size == 0)
        return;

      var selectionModel = cb.getSelectionModel();

      switch (keyEvent.getCode())
      {
        case DOWN ->
        {
          int newNdx = (selectionModel.getSelectedIndex() + 1 + size) % size;

          if ((selectionModel.getSelectedIndex() < 0) && strNullOrBlank(strBiMap.get(items.get(newNdx))))
            newNdx = (newNdx + 1 + size) % size;

          selectionModel.select(newNdx);

          keyEvent.consume();
        }
        case UP ->
        {
          int newNdx = (selectionModel.getSelectedIndex() - 1 + size) % size;

          if ((selectionModel.getSelectedIndex() < 0) && strNullOrBlank(strBiMap.get(items.get(newNdx))))
            newNdx = (newNdx - 1 + size) % size;

          selectionModel.select(newNdx);

          keyEvent.consume();
        }
        case SPACE, DELETE ->
        {
          // Select blank entry if present

          for (int i = 0; i < size; i++)
          {
            var label = strBiMap.get(items.get(i));

            if (strNullOrBlank(label))
            {
              selectionModel.select(i);
              keyEvent.consume();
              return;
            }
          }

          // no blank option: do nothing
        }
        default ->
        {
          // Letter cycling

          String text = keyEvent.getText();

          if (strNotNullOrEmpty(text))
          {
            char c = text.charAt(0);

            if (Character.isLetter(c))
            {
              int start = selectionModel.getSelectedIndex(),
                  from = Math.max(-1, start);

              char needle = Character.toLowerCase(c);

              for (int i = 1; i <= size; i++)
              {
                int idx = (from + i) % size;
                var label = strBiMap.get(items.get(idx));

                if (strNotNullOrBlank(label) && Character.toLowerCase(label.charAt(0)) == needle)
                {
                  selectionModel.select(idx);
                  keyEvent.consume();
                  break;
                }
              }
            }
          }
        }
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
