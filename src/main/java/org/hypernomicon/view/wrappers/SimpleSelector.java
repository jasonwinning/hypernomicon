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
  private final ComboBox<T> cb;

//---------------------------------------------------------------------------

  public static <T> void init(ComboBox<T> cb, SequencedMap<T, String> strMap)
  {
    noOp(new SimpleSelector<>(cb, strMap));
  }

//---------------------------------------------------------------------------

  private SimpleSelector(ComboBox<T> cb, SequencedMap<T, String> strMap)
  {
    this.cb = cb;

    cb.setEditable(true);

    var strBiMap = HashBiMap.create(strMap);

    cb.setConverter(new StringConverter<>()
    {
      @Override public String toString  (T      val   ) { return strBiMap          .get(val   ); }
      @Override public T      fromString(String string) { return strBiMap.inverse().get(string); }
    });

    cb.setItems(null);
    cb.setItems(FXCollections.observableArrayList(strMap.sequencedKeySet()));

    cb.getSelectionModel().selectedIndexProperty().addListener((obs, ov, nv) ->
    {
      if (nv != null)
        scrollToNdx(nv.intValue());
    });

    var editor = cb.getEditor();

    editor.setEditable(false);
    editor.setCursor(Cursor.DEFAULT);
    editor.setContextMenu(new ContextMenu());

    cb.setOnShown(event -> scrollToNdx(cb.getSelectionModel().getSelectedIndex()));

    cb.setOnHiding(event ->
    {
      suppressPressOpen = true;

      Platform.runLater(() -> suppressPressOpen = false);
    });

  //---------------------------------------------------------------------------

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

  //---------------------------------------------------------------------------

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

          for (int newNdx = 0; newNdx < size; newNdx++)
          {
            var label = strBiMap.get(items.get(newNdx));

            if (strNullOrBlank(label))
            {
              selectionModel.select(newNdx);
              keyEvent.consume();
              return;
            }
          }

          // no blank option: do nothing
        }
        default -> { /* ignore here; handled in KEY_TYPED */ }
      }
    });

  //---------------------------------------------------------------------------

    // Letter/digit cycling
    EventFilters.replaceFilter(cb, KeyEvent.KEY_TYPED, keyEvent ->
    {
      String targetClass = keyEvent.getTarget().getClass().getName();
      if (targetClass.endsWith("FakeFocusTextField"))
        return;

      String ch = keyEvent.getCharacter();
      if (strNullOrBlank(ch))
        return;

      // Normalize to lower case for matching
      String typed = ch.toLowerCase();

      var items = cb.getItems();
      int size = items.size();
      if (size == 0)
        return;

      var selectionModel = cb.getSelectionModel();
      int start = selectionModel.getSelectedIndex(),
          from = Math.max(-1, start);

      for (int i = 1; i <= size; i++)
      {
        int ndx = (from + i) % size;
        String label = strBiMap.get(items.get(ndx));
        if (strNotNullOrBlank(label) && label.toLowerCase().startsWith(typed))
        {
          selectionModel.select(ndx);
          keyEvent.consume();
          break;
        }
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void scrollToNdx(int ndx)
  {
    if (cb.getItems().size() < 1)
      return;

    ListView<T> lv = getCBListView(cb);
    if (lv == null) return;

    if (lv.getItems().size() > ndx)
    {
      lv.getSelectionModel().clearAndSelect(ndx);
      lv.scrollTo(ndx);
    }
    else
      lv.getSelectionModel().clearSelection();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
