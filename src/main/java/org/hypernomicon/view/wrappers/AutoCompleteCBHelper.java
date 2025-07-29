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

import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.Populator.CellValueType;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.App.ui;
import static org.hypernomicon.model.records.RecordType.*;

import org.apache.commons.lang3.mutable.MutableBoolean;

import java.util.*;
import java.util.function.Function;
import java.util.regex.Pattern;

import org.hypernomicon.dialogs.ValueSelectDlgCtrlr;
import org.hypernomicon.model.authors.RecordAuthor;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;

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
public class AutoCompleteCBHelper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final ComboBox<? extends HyperTableCell> cb;
  private final boolean limitToChoices;

  private Runnable enterKeyHandler;

  private HyperTableCell typedMatch,
                         startValue; // This is the previous value that gets restored if no match is found for what the user typed
                                     // and the user must select one of the existing values

  private boolean somethingWasTyped = false, listenForActionEvents = true;

  // Both somethingWasTyped and typedMatch are needed because somethingWasTyped determines whether it pops up the list of
  // options even if typedMatch was not set to a match.

//---------------------------------------------------------------------------

  public <T extends HyperTableCell> AutoCompleteCBHelper(ComboBox<T> cb, boolean limitToChoices, Function<String, T> cellFactory)
  {
    this(cb, null, limitToChoices, cellFactory);
  }

  AutoCompleteCBHelper(HyperCB hcb, boolean limitToChoices)
  {
    this(hcb.getComboBox(), hcb, limitToChoices, null);
  }

  private <T extends HyperTableCell> AutoCompleteCBHelper(ComboBox<T> cb, HyperCB hcb, boolean limitToChoices, Function<String, T> cellFactory)
  {
    this.cb = cb;
    this.limitToChoices = limitToChoices;

    cb.setEditable(true);

    // add a focus listener such that if not in focus, reset the filtered typed keys
    cb.getEditor().focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      somethingWasTyped = false;

      if (Boolean.TRUE.equals(newValue))
      {
        startValue = hcb == null ? HyperCB.selectedHTC(cb, cellFactory) : hcb.selectedHTC();
        Platform.runLater(() -> cb.getEditor().selectAll());
      }
      else
        selectClosestResultBasedOnTextFieldValue(false, false);
    });

    cb.setOnMouseClicked(event ->
    {
      somethingWasTyped = false;
      selectClosestResultBasedOnTextFieldValue(true, true);
    });

    cb.addEventFilter(KeyEvent.KEY_PRESSED, event ->
    {
      if (event.getCode() == KeyCode.ENTER)
      {
        triggerOnAction(hcb);
        event.consume();

        if (enterKeyHandler != null) enterKeyHandler.run();
      }
    });

    cb.setOnAction(event ->
    {
      if ((listenForActionEvents == false) || (somethingWasTyped == false))
        return;

      triggerOnAction(hcb, event);

      somethingWasTyped = false;
    });

    cb.setOnKeyReleased(this::handleKeyReleased);
  }

//---------------------------------------------------------------------------

  void resetTypedMatch()         { somethingWasTyped = false; }
  HyperTableCell getTypedMatch() { return somethingWasTyped ? typedMatch : null; }

  void setEnterKeyHandler(Runnable handler) { enterKeyHandler = handler; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void triggerOnAction(HyperCB hcb)
  {
    triggerOnAction(hcb, new ActionEvent(null, cb));
  }

  @SuppressWarnings("unchecked")
  private void triggerOnAction(HyperCB hcb, ActionEvent event)
  {
    if (listenForActionEvents == false) return;

    listenForActionEvents = false;

    HyperTableCell actualTypedMatch = getTypedMatch();

    if (HyperTableCell.getCellID(actualTypedMatch) >= 1)
    {
      HyperCB.select((ComboBox<HyperTableCell>)cb, actualTypedMatch);
    }
    else
    {
      String str = convertToEnglishChars(cb.getEditor().getText()).strip().toLowerCase();

      if (str.length() > 0)
      {
        HyperTableCell selectedCell = selectedCellByText(str, hcb);
        if (selectedCell != null)
          HyperCB.select((ComboBox<HyperTableCell>)cb, selectedCell);
      }
    }

    if (hcb != null)
      hcb.endEditModeIfInTable(event);

    listenForActionEvents = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTableCell selectedCellByText(String str, HyperCB hcb)
  {
    List<HyperTableCell> cells = new ArrayList<>();
    MutableBoolean atLeastOneStrongMatch = new MutableBoolean((hcb == null) || hcb.creatingPersonNotAllowed());

    HyperTableCell selection = selectedCellOrCells(cells, str, atLeastOneStrongMatch);
    if (selection != null)
      return selection;

    // There was no exact match

    if (atLeastOneStrongMatch.isTrue())
    {
      if ((cells.size() > 1) && (ui.isShuttingDown() == false))
      {
        if ((hcb != null) && (hcb.getPopulator().getValueType(hcb.getRow()) == CellValueType.cvtRecord))
        {
          selection = hcb.showPopupToSelectFromMatches(cells);  // If the populator is StandardPopulator or RecordByTypePopulator, this ignores cells
        }                                                       // and does a different search using the OmniFinder criteria; if that produces no results,
        else                                                    // cells is used as a fallback
        {
          ValueSelectDlgCtrlr ctrlr = new ValueSelectDlgCtrlr(cells);
          selection = ctrlr.showModal() ? ctrlr.listView.getSelectionModel().getSelectedItem() : null;
        }

        if (hcb != null)                      // If we are in a table context, by the time we get back here, the
          hcb.selectValueInTable(selection);  // ComboBox is gone and the table is already out of edit mode

        return selection;
      }

      if (cells.size() == 1)
      {
        selection = cells.get(0);

        if (HyperTableCell.getCellID(selection) > 0)
          return selection;
      }
    }

    return hcb == null ? null : hcb.handleLackOfStrongMatch();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Similar to OmniFinder.FinderThread.isMatch

  private HyperTableCell selectedCellOrCells(Collection<HyperTableCell> cells, String str, MutableBoolean atLeastOneStrongMatch)
  {
    boolean containsNum = Pattern.compile("\\d").matcher(str).find();
    PersonName personName = null;

    List<KeywordLink> linkList = KeywordLinkList.generate(str);

    cbItemsLoop: for (HyperTableCell cell : cb.getItems())
    {
      HDT_Record record = HyperTableCell.getRecord(cell);
      String lcCellText = HyperTableCell.getCellText(cell).toLowerCase();

      if (lcCellText.equals(str) || ((record != null) && record.getNameEngChar().toLowerCase().equals(str)))
        return cell;

      if (record != null)
      {
        if (record.getSearchKey().toLowerCase().contains(str))
        {
          cells.add(cell);
          atLeastOneStrongMatch.setTrue();
          continue cbItemsLoop;
        }

        if ((record.getType() == hdtWork) && (containsNum == false))
        {
          if (personName == null)
            personName = new PersonName(str).toLowerCase();

          for (RecordAuthor author : ((HDT_Work)record).getAuthors())
          {
            if ((personName.getFirst().length() > 0) &&
                (author.firstName(true).toLowerCase().contains(personName.getFirst()) ||
                 author.lastName (true).toLowerCase().contains(personName.getFirst())))
            {
              cells.add(cell);
              continue cbItemsLoop;
            }

            if ((personName.getLast().length() > 0) &&
                (author.firstName(true).toLowerCase().contains(personName.getLast()) ||
                 author.lastName (true).toLowerCase().contains(personName.getLast())))
            {
              cells.add(cell);
              continue cbItemsLoop;
            }
          }
        }

        if (linkList.size() > 0)
          for (KeywordLink keyLink : linkList)
            if (keyLink.key().record == record)
            {
              cells.add(cell);
              continue cbItemsLoop;
            }

        if (record.getNameEngChar().strip().toLowerCase().contains(str))
        {
          cells.add(cell);
          atLeastOneStrongMatch.setTrue();
          continue cbItemsLoop;
        }
      }

      if (lcCellText.contains(str) &&
          ((HyperTableCell.getCellType(cell) != hdtPerson) || (record != null))) // Don't use non-record author partial matches
      {
        cells.add(cell);
        atLeastOneStrongMatch.setTrue();
        continue cbItemsLoop;
      }
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void handleKeyReleased(KeyEvent event)
  {
    if (notNormalTypingEvent(event))  // arrow keys, page up/down, escape, etc.
    {
      typedMatch = null;
      return;
    }

    somethingWasTyped = true;

    List<? extends HyperTableCell> items = cb.getItems();

    if (collEmpty(items))
    {
      typedMatch = null;
      return;
    }

    TextField editor = cb.getEditor();
    String typed = editor.getText().substring(0, editor.getSelection().getStart()), // Get unselected text
           typedLC = convertToEnglishChars(typed).toLowerCase();

    typedMatch = findMatch(items, editor, typed, typedLC);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HyperTableCell findMatch(List<? extends HyperTableCell> items, TextField editor, String typed, String typedLC)
  {
    if (typed.isEmpty())
      return null;

    for (HyperTableCell cell : items)
    {
      HDT_Record record = HyperTableCell.getRecord(cell);
      String cellText = convertToEnglishChars(HyperTableCell.getCellText(cell));

      if (cellText.toLowerCase().startsWith(typedLC))
      {
        if ((HyperTableCell.getCellType(cell) != hdtPerson) || (record != null))
        {
          updateEditorText(editor, typed, cellText);
          return HyperTableCell.clone(cell);
        }
      }

      if (record != null)
      {
        if (record.getType() == hdtPerson)
        {
          HDT_Person person = (HDT_Person)record;
          if (person.getFullName(true).toLowerCase().startsWith(typedLC))
          {
            updateEditorText(editor, typed, person.getFullName(true));
            return HyperTableCell.clone(cell);
          }
        }
        else if (record.getNameEngChar().toLowerCase().startsWith(typedLC))
        {
          updateEditorText(editor, typed, record.getNameEngChar());
          return HyperTableCell.clone(cell);
        }
      }
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void updateEditorText(TextField editor, String typed, String matchText)
  {
    editor.setText(typed + matchText.substring(typed.length()));
    editor.positionCaret(typed.length());
    editor.selectEnd();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Set<KeyCode> IGNORED_KEYS = EnumSet.of(

      KeyCode.BACK_SPACE, KeyCode.RIGHT, KeyCode.LEFT   , KeyCode.DELETE ,
      KeyCode.HOME      , KeyCode.END  , KeyCode.TAB    , KeyCode.ESCAPE,
      KeyCode.UP        , KeyCode.DOWN , KeyCode.PAGE_UP, KeyCode.PAGE_DOWN);

  private static boolean notNormalTypingEvent(KeyEvent event)
  {
    return shortcutKeyIsDown(event) || IGNORED_KEYS.contains(event.getCode());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static boolean scrollToValue(ComboBox<? extends HyperTableCell> cb)
  {
    List<? extends HyperTableCell> items = cb.getItems();
    if (items == null) return false;

    String editorText = cb.getEditor().getText();

    if (editorText.isBlank()) return false;

    for (int ndx = 0; ndx < items.size(); ndx++)
    {
      String cellText = HyperTableCell.getCellText(items.get(ndx));

      if (editorText.equalsIgnoreCase(cellText))
      {
        ListView<? extends HyperTableCell> lv = getCBListView(cb);

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
  @SuppressWarnings("unchecked")
  private void selectClosestResultBasedOnTextFieldValue(boolean affect, boolean inFocus)
  {
    SingleSelectionModel<HyperTableCell> selectionModel = (SingleSelectionModel<HyperTableCell>) cb.getSelectionModel();
    TextField editor = cb.getEditor();
    String editorText = editor.getText();
    boolean found = scrollToValue(cb);

    if ((found == false) && affect)
    {
      selectionModel.clearSelection();
      editor.setText(editorText);
      editor.end();
    }

    if ((inFocus == false) && (editorText.strip().length() > 0))
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
