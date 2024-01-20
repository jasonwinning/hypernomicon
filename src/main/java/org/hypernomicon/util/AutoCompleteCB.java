/*
 * Copyright 2015-2024 Jason Winning
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

package org.hypernomicon.util;

import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.event.EventHandler;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListView;
import javafx.scene.control.SingleSelectionModel;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.RecordType.*;

import java.util.Iterator;
import java.util.List;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Helper class for ComboBox autocompletion
 * <p>
 * Based on code from the following blog post:
 * <p>
 * <a href="https://web.archive.org/web/20190402042956/http://tech.chitgoks.com/2013/08/20/how-to-create-autocomplete-combobox-or-textfield-in-java-fx-2/">
 * http://tech.chitgoks.com/2013/08/20/how-to-create-autocomplete-combobox-or-textfield-in-java-fx-2/</a>
 *
 * @since 1.0
 */

public class AutoCompleteCB implements EventHandler<KeyEvent>
{
  private final ComboBox<HyperTableCell> cb;
  private final HyperCB hcb;
  private final boolean limitToChoices;
  private HyperTableCell startValue;
  private boolean gotKeyPressedYet = false;

//---------------------------------------------------------------------------

  public AutoCompleteCB(HyperCB newHCB, boolean limitToChoices)
  {
    this.limitToChoices = limitToChoices;

    hcb = newHCB;
    cb = hcb.getComboBox();

    cb.setEditable(true);

    // add a focus listener such that if not in focus, reset the filtered typed keys
    cb.getEditor().focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      hcb.somethingWasTyped = false;

      if (Boolean.TRUE.equals(newValue))
        startValue = hcb.selectedHTC();
      else
        selectClosestResultBasedOnTextFieldValue(false, false);
    });

    cb.setOnMouseClicked(event ->
    {
      hcb.somethingWasTyped = false;
      selectClosestResultBasedOnTextFieldValue(true, true);
    });

    cb.addEventFilter(KeyEvent.KEY_PRESSED, event ->
    {
      gotKeyPressedYet = true;

      if ((event.getCode() == KeyCode.ENTER) && (hcb.somethingWasTyped == false))
      {
        hcb.triggerOnAction();
        event.consume();
      }
    });

    cb.setOnAction(event ->
    {
      if ((hcb.listenForActionEvents == false) || (hcb.somethingWasTyped == false))
        return;

      hcb.listenForActionEvents = false;
      hcb.triggerOnAction(event);
      hcb.listenForActionEvents = true;

      hcb.somethingWasTyped = false;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void handle(KeyEvent event)
  {
    if (gotKeyPressedYet == false)
      return;

    KeyCode keyCode = event.getCode();

    if (shortcutKeyIsDown(event)  || keyCode == KeyCode.BACK_SPACE ||
        keyCode == KeyCode.RIGHT  || keyCode == KeyCode.LEFT       ||
        keyCode == KeyCode.DELETE || keyCode == KeyCode.HOME       ||
        keyCode == KeyCode.END    || keyCode == KeyCode.TAB        ||
        keyCode == KeyCode.ESCAPE)
    {
      hcb.typedMatch = null;
      return;
    }

    hcb.somethingWasTyped = true;

    List<HyperTableCell> items = cb.getItems();

    if (collEmpty(items))
    {
      hcb.typedMatch = null;
      return;
    }

    TextField editor = cb.getEditor();
    String typed = editor.getText().substring(0, editor.getSelection().getStart()), // Get unselected text
           typedLC = convertToEnglishChars(typed).toLowerCase();
    boolean match = false;

    Iterator<HyperTableCell> it = items.iterator();

    while (it.hasNext() && (match == false))
    {
      HyperTableCell cell = it.next();

      HDT_Record record = HyperTableCell.getRecord(cell);
      String cellText = convertToEnglishChars(HyperTableCell.getCellText(cell));

      if (cellText.toLowerCase().startsWith(typedLC))
      {
        if ((cell.type != hdtPerson) || (record != null))
        {
          match = true;
          editor.setText(typed + cellText.substring(typed.length()));
        }
      }

      if ((match == false) && (record != null))
      {
        if (record.getType() == hdtPerson)
        {
          HDT_Person person = (HDT_Person)record;
          if (person.getFullName(true).toLowerCase().startsWith(typedLC))
          {
            match = true;
            editor.setText(typed + person.getFullName(true).substring(typed.length()));
          }
        }
        else if (record.getNameEngChar().toLowerCase().startsWith(typedLC))
        {
          match = true;
          editor.setText(typed + record.getNameEngChar().substring(typed.length()));
        }
      }

      if (match)
      {
        hcb.typedMatch = cell.clone();

        editor.positionCaret(typed.length());
        editor.selectEnd();
      }
    }

    if (match == false)
      hcb.typedMatch = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean scrollToValue(ComboBox<HyperTableCell> cb)
  {
    List<HyperTableCell> items = cb.getItems();
    if (items == null) return false;

    String editorText = cb.getEditor().getText();

    if (editorText.isBlank()) return false;

    for (int ndx = 0; ndx < items.size(); ndx++)
    {
      String cellText = HyperTableCell.getCellText(items.get(ndx));

      if (editorText.equalsIgnoreCase(cellText))
      {
        ListView<HyperTableCell> lv = getCBListView(cb);

        if (lv.getItems().size() > ndx)
        {
          lv.getSelectionModel().clearAndSelect(ndx);
          lv.scrollTo(lv.getSelectionModel().getSelectedIndex());
          return true;
        }

        lv.getSelectionModel().clearSelection();
        return false;
      }
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Selects the item and scrolls to it when the popup is shown.
   * @param affect true if combobox is clicked to show popup so text and caret position will be readjusted.
   * @param inFocus true if combobox has focus. If not, programmatically press enter key to add new entry to list.
   */
  private void selectClosestResultBasedOnTextFieldValue(boolean affect, boolean inFocus)
  {
    SingleSelectionModel<HyperTableCell> selectionModel = cb.getSelectionModel();
    TextField editor = cb.getEditor();
    String editorText = editor.getText();
    boolean found = scrollToValue(cb);

    if ((found == false) && affect)
    {
      selectionModel.clearSelection();
      editor.setText(editorText);
      editor.end();
    }

    if ((inFocus == false) && (editorText.trim().length() > 0))
    {
      if (limitToChoices && (found == false))
      {
        selectionModel.clearSelection();
        selectionModel.select(startValue);
        return;
      }

      // press enter key programmatically to have this entry added
      cb.fireEvent(new KeyEvent(null, cb, KeyEvent.KEY_RELEASED, KeyCode.ENTER.toString(), KeyCode.ENTER.getName(), KeyCode.ENTER, false, false, false, false));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
