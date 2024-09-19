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

package org.hypernomicon.view.wrappers;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.dialogs.RecordSelectDlgCtrlr;
import org.hypernomicon.dialogs.ValueSelectDlgCtrlr;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.util.AutoCompleteCB;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ComboBox;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

public class HyperCB implements CommitableWrapper
{
  private final ComboBox<HyperTableCell> cb;
  private final Populator populator;
  private final HyperTable table;
  private final HyperTableRow row;
  private final List<HTCListener> listeners = new ArrayList<>();
  private final int colNdx;

  final boolean autoCommitBeforeRecordSave;

  public HyperTableCell typedMatch;
  private HyperTableCell preShowingValue;
  private EventHandler<ActionEvent> onAction;
  private Runnable enterKeyHandler;
  private MutableBoolean adjusting;
  public boolean somethingWasTyped, listenForActionEvents = true, dontCreateNewRecord = false;
  public Supplier<HDT_Work> workSupplier;
  private boolean silentMode = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface
  public interface HTCListener { void changed(HyperTableCell oldValue, HyperTableCell newValue); }

  public void setChoicesChanged()                    { populator.setChanged(row); }
  public ComboBox<HyperTableCell> getComboBox()      { return cb; }
  public void addListener(HTCListener listener)      { listeners.add(listener); }
  public void addAndSelectEntry(int id, String text) { select(populator.addEntry(row, id, text)); }
  public void triggerEnterKeyHandler()               { if (enterKeyHandler != null) enterKeyHandler.run(); }

  private boolean isInTable()                        { return (cb != null) && (cb.getParent() instanceof ComboBoxCell); }

  public void setOnAction(EventHandler<ActionEvent> onAction) { if (onAction != null) this.onAction = onAction; }
  public void setEnterKeyHandler(Runnable handler)            { enterKeyHandler = handler; }

  @SuppressWarnings("unchecked")
  public <PopType extends Populator> PopType getPopulator() { return (PopType) populator; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Constructor for dropdown entry field wrapper
   * @param cb The combobox control to use
   * @param ctrlType Should be ctDropDownList (user can edit) or ctDropDown
   * @param newPopulator The Populator to use
   */
  public HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator)
  {
    this(cb, ctrlType, newPopulator, null, false, null, -1);
  }

  /**
   * Constructor for dropdown entry field wrapper
   * @param cb The combobox control to use
   * @param ctrlType Should be ctDropDownList (user can edit) or ctDropDown
   * @param newPopulator The Populator to use
   * @param autoCommitBeforeRecordSave Whether the field should be auto-committed (basically, this is like the user hitting enter; could cause popup window to show) when record save is triggered.
   */
  public HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator, boolean autoCommitBeforeRecordSave)
  {
    this(cb, ctrlType, newPopulator, null, autoCommitBeforeRecordSave, null, -1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator, HyperTableRow row, boolean autoCommitBeforeRecordSave, HyperTable table, int colNdx)
  {
    this.cb = cb;
    this.autoCommitBeforeRecordSave = autoCommitBeforeRecordSave;
    this.colNdx = colNdx;
    populator = newPopulator;
    this.table = table;
    this.row = nullSwitch(row, Populator.dummyRow);

    if ((ctrlType != ctDropDown) && (ctrlType != ctDropDownList))
    {
      internalErrorPopup(42852);
      return;
    }

    setNodeUserObj(cb, NodeUserDataType.HypercCB, this);
    somethingWasTyped = false;

  //---------------------------------------------------------------------------

    cb.setConverter(new StringConverter<>()
    {
      @Override public String toString(HyperTableCell cell)
      {
        return HyperTableCell.getCellText(cell);
      }

      @Override public HyperTableCell fromString(String string)
      {
        HyperTableCell cell = nullSwitch(cb.getItems(), null, items -> findFirst(items, htc -> string.equals(htc.text)));
        return cell == null ? new HyperTableCell(string, populator.getRecordType(row)) : cell;
      }
    });

  //---------------------------------------------------------------------------

    cb.setOnKeyReleased(new AutoCompleteCB(this, ctrlType == ctDropDownList));

  //---------------------------------------------------------------------------

    cb.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        populate(false);
    });

  //---------------------------------------------------------------------------

    cb.setOnShowing(event -> preShowingValue = cb.getValue());

  //---------------------------------------------------------------------------

    adjusting = repositionPopupListWorkaround(cb);

  //---------------------------------------------------------------------------

    cb.setOnHidden(event ->
    {
      if (adjusting.isTrue() || (preShowingValue == null) || (table == null) || (table.autoCommitListSelections == false)) return;

      String newText = HyperTableCell.getCellText(cb.getValue());
      if (! (newText.isEmpty() || newText.equals(HyperTableCell.getCellText(preShowingValue))))
        endEditModeIfInTable(null);
    });

  //---------------------------------------------------------------------------

    cb.getSelectionModel().selectedItemProperty().addListener((obs, oldValue, newValue) ->
    {
      if (silentMode == false)
        listeners.forEach(listener -> listener.changed(oldValue, newValue));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableCell selectedHTC()
  {
    HyperTableCell htc = cb.getValue();
    String str = cb.getEditor().getText();

    return (htc == null) || (htc.text.equals(str) == false) ?
      new HyperTableCell(str, selectedType())
    :
      htc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<HyperTableCell> populate(boolean force)
  {
    HyperTableCell cell = cb.getValue();

    silentMode = true;

    List<HyperTableCell> choices = populator.populate(row, force);
    cb.setItems(null);
    cb.setItems(FXCollections.observableList(choices));
    select(cell);

    silentMode = false;

    if ((choices.size() > 0) && HyperTableCell.isEmpty(cell))
      nullSwitch(getCBListView(cb), lv -> lv.scrollTo(0));

    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This not only clears the text box and selected item, but also calls the populator's clear method
   * so that the list of choices will also be blank.
   */
  public void clear()
  {
    cb.getSelectionModel().clearSelection();
    cb.getEditor().clear();
    cb.setValue(null);
    populator.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public <HDT_T extends HDT_Record> HDT_T selectedRecord()
  {
    return HyperTableCell.getRecord(cb.getValue());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int selectedID()
  {
    int id = HyperTableCell.getCellID(cb.getValue());
    return id > 0 ? id : -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getText()
  {
    populate(false);

    return cb.isEditable() ? safeStr(cb.getEditor().getText()) : HyperTableCell.getCellText(cb.getValue());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public RecordType selectedType()
  {
    if (populator != null)
    {
      RecordType type = populator.getRecordType(row);

      if ((type != null) && (type != hdtNone))
        return type;
    }

    return HyperTableCell.getCellType(cb.getValue());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void select(HyperTableCell cell)
  {
    cb.setValue(cell);

    if (cell == null)
      cb.getSelectionModel().clearSelection();
    else
      cb.getSelectionModel().select(cell);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectIDofRecord(HyperObjPointer<? extends HDT_Record, ? extends HDT_Record> pntr)
  {
    selectID(pntr.getID());
  }

  public void selectIDofRecord(HDT_Record record)
  {
    selectID(record == null ? -1 : record.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectID(int objID)
  {
    if (objID < 1)
      select(null);
    else
      select(populator.getChoiceByID(row, objID));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectType(RecordType objType)
  {
    populate(false);

    select(findFirst(cb.getItems(), choice -> choice.type == objType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commit()
  {
    if (somethingWasTyped && (typedMatch != null))
    {
      somethingWasTyped = false;
      selectID(typedMatch.getID());
    }

    cb.commitValue();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void endEditModeIfInTable(ActionEvent event)
  {
    if (isInTable()) ((CommitableWrapper) cb.getParent()).commit();

    if ((event != null) && (onAction != null))
      onAction.handle(event);  // activates the "Execute" button in the queries hyperTab
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void triggerOnAction()
  {
    triggerOnAction(new ActionEvent(null, cb));
  }

  public void triggerOnAction(ActionEvent event)
  {
    if (listenForActionEvents == false) return;

    listenForActionEvents = false;

    if (somethingWasTyped && (HyperTableCell.getCellID(typedMatch) >= 1))
    {
      select(typedMatch);
    }
    else
    {
      String str = convertToEnglishChars(cb.getEditor().getText()).trim().toLowerCase();

      if (str.length() > 0)
        nullSwitch(selectedCellByText(str), this::select);
    }

    endEditModeIfInTable(event);

    listenForActionEvents = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTableCell selectedCellByText(String str)
  {
    List<HyperTableCell> cells = new ArrayList<>();
    MutableBoolean atLeastOneStrongMatch = new MutableBoolean((populator.getRecordType(row) != hdtPerson) || dontCreateNewRecord);

    HyperTableCell selection = selectedCellOrCells(cells, str, atLeastOneStrongMatch);
    if (selection != null)
      return selection;

    // There was no exact match

    if (atLeastOneStrongMatch.isTrue())
    {
      if ((cells.size() > 1) && (ui.isShuttingDown() == false))
      {
        if (populator.getValueType(row) == CellValueType.cvtRecord)
        {
          RecordSelectDlgCtrlr ctrlr = new RecordSelectDlgCtrlr(populator, cells, convertToEnglishChars(cb.getEditor().getText()).trim());
          selection = ctrlr.showModal() ? populator.getChoiceByID(row, ctrlr.getRecord().getID()) : null;
        }
        else
        {
          ValueSelectDlgCtrlr ctrlr = new ValueSelectDlgCtrlr(cells);
          selection = ctrlr.showModal() ? ctrlr.listView.getSelectionModel().getSelectedItem() : null;
        }

        if ((table != null) && (selection != null))     // By the time we get back here, the ComboBox is gone
          row.setCellValue(colNdx, selection);          // and the table is already out of edit mode

        return selection;
      }

      if (cells.size() == 1)
      {
        selection = cells.get(0);

        if (HyperTableCell.getCellID(selection) > 0)
          return selection;
      }
    }

    if (dontCreateNewRecord || ui.isShuttingDown())
      return null;

    switch (populator.getRecordType(row))
    {
      case hdtPerson :

        HDT_Person otherPerson = HDT_Person.lookUpByName(new PersonName(cb.getEditor().getText()));

        if (otherPerson != null)
          for (HyperTableCell cell : cb.getItems())
            if (cell.getID() == otherPerson.getID())
              return cell;

        String text = cb.getEditor().getText();
        HDT_Work work = workSupplier == null ? null : workSupplier.get();

        NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(work == null, text, null, work);

        if (npdc.showModal())
        {
          if (table == null)
          {
            populate(true);                  // A new record has been created so force it to repopulate
            return populator.getChoiceByID(row, npdc.getPerson().getID());
          }

          Populator pop = table.getPopulator(colNdx);

          if (npdc.getPerson() != null)    // By the time we get back here, the ComboBox is gone
          {                                // and the table is already out of edit mode
            pop.setChanged(row);           // A new record has been created so force it to repopulate
            table.selectID(colNdx, row, npdc.getPerson().getID());
          }
          else
          {
            pop.populate(row, false);
            row.setCellValue(colNdx, pop.addEntry(row, npdc.getNameLastFirst()));

            table.cancelEditing(); // For some reason in this case the keystroke event from earlier causes
          }                        // the table to enter edit mode again
        }

        break;

      case hdtInstitution :

        if (table != null)
          ui.personHyperTab().newInstClick(row, cb.getEditor().getText(), colNdx);

        break;

      default: break;
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTableCell selectedCellOrCells(List<HyperTableCell> cells, String str, MutableBoolean atLeastOneStrongMatch)
  {
    boolean containsNum = Pattern.compile("\\d").matcher(str).find();
    PersonName personName = null;

    List<KeywordLink> linkList = KeywordLinkList.generate(str);

    cbItemsLoop: for (HyperTableCell cell : cb.getItems())
    {
      HDT_Record record = HyperTableCell.getRecord(cell);
      String lcCellText = cell.text.toLowerCase();

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

          for (Author author : ((HDT_Work)record).getAuthors())
          {
            if ((personName.getFirst().length() > 0) &&
                (author.getFirstName(true).toLowerCase().contains(personName.getFirst()) ||
                 author.getLastName (true).toLowerCase().contains(personName.getFirst())))
            {
              cells.add(cell);
              continue cbItemsLoop;
            }

            if ((personName.getLast().length() > 0) &&
                (author.getFirstName(true).toLowerCase().contains(personName.getLast()) ||
                 author.getLastName (true).toLowerCase().contains(personName.getLast())))
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

        if (record.getNameEngChar().trim().toLowerCase().contains(str))
        {
          cells.add(cell);
          atLeastOneStrongMatch.setTrue();
          continue cbItemsLoop;
        }
      }

      if (lcCellText.contains(str) &&
          ((cell.type != hdtPerson) || (record != null))) // Don't use non-record author partial matches
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

}
