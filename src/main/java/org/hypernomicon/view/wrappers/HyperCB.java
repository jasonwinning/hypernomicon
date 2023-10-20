/*
 * Copyright 2015-2023 Jason Winning
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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.regex.Pattern;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.dialogs.RecordSelectDlgCtrlr;
import org.hypernomicon.dialogs.ValueSelectDlgCtrlr;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.util.AutoCompleteCB;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
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
  private final HyperTableRow row;
  private final EventHandler<ActionEvent> internalOnAction;

  public HyperTableCell typedMatch;
  private HyperTableCell preShowingValue;
  private EventHandler<ActionEvent> onAction;
  private MutableBoolean adjusting;
  public boolean somethingWasTyped, listenForActionEvents = true, dontCreateNewRecord = false;
  private boolean silentMode = false;

  private final List<HTCListener> listeners = new ArrayList<>();

  static final Map<ComboBox<HyperTableCell>, HyperCB> cbRegistry = new HashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface
  public interface HTCListener { void changed(HyperTableCell oldValue, HyperTableCell newValue); }

  public void setChoicesChanged()                { populator.setChanged(row); }
  public ComboBox<HyperTableCell> getComboBox()  { return cb; }
  public void addListener(HTCListener listener)  { listeners.add(listener); }
  public void triggerOnAction()                  { internalOnAction.handle(new ActionEvent(null, cb)); }
  public void triggerOnAction(ActionEvent event) { internalOnAction.handle(event); }
  private boolean isInTable()                    { return (cb != null) && (cb.getParent() instanceof ComboBoxCell); }
  public void addBlankEntry()                    { addEntry(-1, "", false); }

  public void setOnAction(EventHandler<ActionEvent> onAction) { if (onAction != null) this.onAction = onAction; }

  @SuppressWarnings("unchecked")
  public <PopType extends Populator> PopType getPopulator() { return (PopType) populator; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator)
  {
    this(cb, ctrlType, newPopulator, null, false, null);
  }

  public HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator, boolean addToRegistry)
  {
    this(cb, ctrlType, newPopulator, null, addToRegistry, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator, HyperTableRow row, boolean addToRegistry, HyperTable table)
  {
    this.cb = cb;
    populator = newPopulator;
    this.row = nullSwitch(row, Populator.dummyRow);

    if ((ctrlType != ctDropDown) && (ctrlType != ctDropDownList))
    {
      messageDialog("Internal error #42852", mtError);
      internalOnAction = null;
      return;
    }

    somethingWasTyped = false;

    if (addToRegistry)
      cbRegistry.put(cb, this);

  //---------------------------------------------------------------------------

    internalOnAction = event ->
    {
      if (somethingWasTyped && (HyperTableCell.getCellID(typedMatch) >= 1))
      {
        select(typedMatch);
      }
      else
      {
        String str = convertToEnglishChars(cb.getEditor().getText()).trim().toLowerCase();

        if (str.length() > 0)
          nullSwitch(selectedCellByText(str, table), this::select);
      }

      endEditModeIfInTable(event);
    };

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

  public void addAndSelectEntry(HyperObjPointer<? extends HDT_Record, ? extends HDT_Record> pntr, Function<HDT_Record, String> rs)
  {
    if (pntr.isNotNull())
      addAndSelectEntry(pntr.get(), rs);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addAndSelectEntryOrBlank(HyperObjPointer<? extends HDT_Record, ? extends HDT_Record> pntr, Function<HDT_Record, String> rs)
  {
    if (pntr.isNotNull())
      addAndSelectEntry(pntr.get(), rs);
    else
      addBlankEntry();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addAndSelectEntry(HDT_Record record, Function<HDT_Record, String> rs)
  {
    if (record != null)
      addEntry(record.getID(), rs.apply(record), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addAndSelectEntryOrBlank(HDT_Record record, Function<HDT_Record, String> rs)
  {
    if (record != null)
      addEntry(record.getID(), rs.apply(record), true);
    else
      addBlankEntry();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addEntry(int id, String text, boolean select)
  {
    HyperTableCell cell = populator.addEntry(row, id, text);
    if (select && ((id > 0) || (text.isBlank() == false)))
      select(cell);
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
      triggerOnAction();
    }
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

  private HyperTableCell selectedCellByText(String str, HyperTable table)
  {
    List<HyperTableCell> cells = new ArrayList<>();
    MutableBoolean atLeastOneStrongMatch = new MutableBoolean((populator.getRecordType(row) != hdtPerson) || dontCreateNewRecord);

    HyperTableCell selection = selectedCellOrCells(cells, str, atLeastOneStrongMatch);
    if (selection != null)
      return selection;

    int colNdx = (table != null) && (cb.getParent() instanceof ComboBoxCell) ?
      table.getTV().getColumns().indexOf(((ComboBoxCell)cb.getParent()).getTableColumn())
    :
      -1;

    // There was no exact match

    if (atLeastOneStrongMatch.isTrue())
    {
      if (cells.size() > 1)
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
          row.setCellValue(colNdx, selection.clone());  // and the table is already out of edit mode

        return selection;
      }

      if (cells.size() == 1)
      {
        selection = cells.get(0);

        if (HyperTableCell.getCellID(selection) > 0)
          return selection;
      }
    }

    if (dontCreateNewRecord)
      return null;

    switch (populator.getRecordType(row))
    {
      case hdtPerson :

        HDT_Person otherPerson = HDT_Person.lookUpByName(new PersonName(cb.getEditor().getText()));

        if (otherPerson != null)
          for (HyperTableCell cell : cb.getItems())
            if (cell.getID() == otherPerson.getID())
              return cell;

        NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(table == null, cb.getEditor().getText(), null);

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
            if (keyLink.key.record == record)
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
