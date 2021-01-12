/*
 * Copyright 2015-2021 Jason Winning
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
 *
 * Based on code from the following blog post:
 * http://tech.chitgoks.com/2013/08/20/how-to-create-autocomplete-combobox-or-textfield-in-java-fx-2/
 *
 * @since   1.0
 */

public class AutoCompleteCB implements EventHandler<KeyEvent>
{
  private final ComboBox<HyperTableCell> cb;
  private final HyperCB hcb;
  private boolean limitToChoices;
  private HyperTableCell startValue;

  public AutoCompleteCB(HyperCB newHCB, boolean limitToChoices)
  {
    this.limitToChoices = limitToChoices;

    cb = newHCB.getComboBox();
    hcb = newHCB;

    cb.setEditable(true);

    // add a focus listener such that if not in focus, reset the filtered typed keys
    cb.getEditor().focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      newHCB.somethingWasTyped = false;

      if (Boolean.TRUE.equals(newValue))
        startValue = hcb.selectedHTC();
      else
        selectClosestResultBasedOnTextFieldValue(false, false);
    });

    cb.setOnMouseClicked(event ->
    {
      newHCB.somethingWasTyped = false;
      selectClosestResultBasedOnTextFieldValue(true, true);
    });

    cb.addEventFilter(KeyEvent.KEY_PRESSED, event ->
    {
      if ((event.getCode() == KeyCode.ENTER) && (hcb.somethingWasTyped == false))
        hcb.triggerOnAction();
    });

    cb.setOnAction(event ->
    {
      if (hcb.listenForActionEvents == false) return;

      hcb.listenForActionEvents = false;

      if (hcb.somethingWasTyped)
      {
        hcb.getOnAction().handle(event);
        hcb.somethingWasTyped = false;
      }

      hcb.listenForActionEvents = true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void handle(KeyEvent event)
  {
    if (event.isControlDown()             || event.getCode() == KeyCode.BACK_SPACE ||
        event.getCode() == KeyCode.RIGHT  || event.getCode() == KeyCode.LEFT       ||
        event.getCode() == KeyCode.DELETE || event.getCode() == KeyCode.HOME       ||
        event.getCode() == KeyCode.END    || event.getCode() == KeyCode.TAB)
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
        if ((cell.getType() != hdtPerson) || (record != null))
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
        hcb.typedMatch = cell;

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
        try
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
        catch (Exception e) { noOp(); }
      }
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/*
 * selectClosestResultBasedOnTextFieldValue() - selects the item and scrolls to it when
 * the popup is shown.
 *
 * parameters:
 *  affect - true if combobox is clicked to show popup so text and caret position will be readjusted.
 *  inFocus - true if combobox has focus. If not, programmatically press enter key to add new entry to list.
 *
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
      KeyEvent ke = new KeyEvent(null, cb, KeyEvent.KEY_RELEASED, KeyCode.ENTER.toString(), KeyCode.ENTER.getName(), KeyCode.ENTER, false, false, false, false);
      cb.fireEvent(ke);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
