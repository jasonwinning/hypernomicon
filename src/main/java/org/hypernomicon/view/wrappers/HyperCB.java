/*
 * Copyright 2015-2020 Jason Winning
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

import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.queryEngines.AllQueryEngine;
import org.hypernomicon.util.AutoCompleteCB;
import org.hypernomicon.view.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.view.dialogs.RecordSelectDlgCtrlr;
import org.hypernomicon.view.dialogs.ValueSelectDlgCtrlr;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListView;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

public class HyperCB implements CommitableWrapper
{
  private final ComboBox<HyperTableCell> cb;
  private final Populator populator;
  final HyperTableRow row;

  public HyperTableCell typedMatch;
  private HyperTableCell preShowingValue;
  private EventHandler<ActionEvent> onAction, innerOnAction;
  private boolean adjusting = false;
  public boolean somethingWasTyped, listenForActionEvents = true, dontCreateNewRecord = false;

  static final Map<ComboBox<HyperTableCell>, HyperCB> cbRegistry = new HashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EventHandler<ActionEvent> getOnAction() { return onAction; }
  public void setChoicesChanged()                { populator.setChanged(null); }
  public ComboBox<HyperTableCell> getComboBox()  { return cb; }

  @SuppressWarnings("unchecked")
  public <PopType extends Populator> PopType getPopulator() { return (PopType) populator; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableCell selectedHTC()
  {
    HyperTableCell htc = cb.getValue();
    String str = cb.getEditor().getText();

    if ((htc == null) || (htc.getText().equals(str) == false))
      return new HyperTableCell(-1, str, selectedType());

    return htc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setInnerOnAction(EventHandler<ActionEvent> onAction)
  {
    if (onAction == null) return;
    innerOnAction = onAction;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void triggerOnAction()
  {
    getOnAction().handle(new ActionEvent());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setOnAction(EventHandler<ActionEvent> onAction)
  {
    if (onAction == null) return;

    if (populator instanceof VariablePopulator)
      innerOnAction = onAction;
    else
      this.onAction = onAction;
  }

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
      return;
    }

    somethingWasTyped = false;

    if (addToRegistry)
      cbRegistry.put(cb, this);

    // When user hits enter, if no record is selected, try to find record with name containing what was typed
    onAction = event -> cbOnAction(event, table);

  //---------------------------------------------------------------------------

    cb.setConverter(new StringConverter<>()
    {
      @Override public String toString(HyperTableCell cell)
      {
        return HyperTableCell.getCellText(cell);
      }

      @Override public HyperTableCell fromString(String string)
      {
        HyperTableCell cell = nullSwitch(cb.getItems(), null, items -> findFirst(items, htc -> string.equals(htc.getText())));
        return cell == null ? new HyperTableCell(-1, string, populator.getRecordType(row)) : cell;
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

    cb.setOnShown(event ->               // This is a workaround for the fact that sometimes, when you show
    {                                    // the popup list from a combox box that is inside a tableview, the popup list
      if (adjusting) return;             // initially is covering the cb's editor.

      adjusting = true;

      cb.hide();
      cb.show();

      adjusting = false;
    });

  //---------------------------------------------------------------------------

    cb.setOnHidden(event ->
    {
      if (adjusting || (preShowingValue == null) || (table == null) || (table.autoCommitListSelections == false)) return;

      String newText = HyperTableCell.getCellText(cb.getValue());
      if (newText.isEmpty()) return;

      if (newText.equals(HyperTableCell.getCellText(preShowingValue)) == false)
        endEditModeIfInTable();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean isInTable()
  {
    return cb == null ? false : cb.getParent() instanceof ComboBoxCell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void endEditModeIfInTable()
  {
    if (isInTable())
      ComboBoxCell.class.cast(cb.getParent()).commit();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cbOnAction(ActionEvent event, HyperTable table)
  {
    int colNdx = -1;

    if (table != null)
    {
      if (cb.getParent() instanceof ComboBoxCell)
      {
        ComboBoxCell cbc = (ComboBoxCell)cb.getParent();
        colNdx = table.getTV().getColumns().indexOf(cbc.getTableColumn());
      }
    }

    if ((HyperTableCell.getCellID(typedMatch) < 1) || (somethingWasTyped == false))
    {
      String str = convertToEnglishChars(cb.getEditor().getText()).trim().toLowerCase();

      if (str.length() > 0)
      {
        if (collEmpty(cb.getItems()))
        {
          if (innerOnAction != null) innerOnAction.handle(event); // activates the "Execute" button in the queries hyperTab
          return;
        }

        List<HyperTableCell> cells = new ArrayList<>();
        boolean alreadyParsedName = false, atLeastOneRealMatch = (populator.getRecordType(row) != hdtPerson) || dontCreateNewRecord,
                containsNum = Pattern.compile("\\d").matcher(str).find();
        PersonName personName = null;

        AllQueryEngine.linkList.generate(str);

        for (HyperTableCell cell : cb.getItems())
        {
          HDT_Record record = HyperTableCell.getRecord(cell);
          boolean added = false, match = (record != null) && record.getNameEngChar().toLowerCase().equals(str);

          if (cell.getText().toLowerCase().equals(str) || match)
          {
            cb.getSelectionModel().select(cell);
            cb.setValue(cell);
            endEditModeIfInTable();

            if (innerOnAction != null) innerOnAction.handle(event);  // activates the "Execute" button in the queries hyperTab
            return;
          }

          if (record != null)
          {
            String key = record.getSearchKey();

            if (key.toLowerCase().contains(str))
            {
              cells.add(cell);
              atLeastOneRealMatch = true;
              added = true;
            }

            if ((added == false) && (record.getType() == hdtWork) && (containsNum == false))
            {
              HDT_Work work = (HDT_Work) record;

              if (alreadyParsedName == false)
              {
                personName = new PersonName(str).toLowerCase();
                alreadyParsedName = true;
              }

              for (Author author : work.getAuthors())
              {
                if (personName.getFirst().length() > 0)
                {
                  if (author.getFirstName(true).toLowerCase().contains(personName.getFirst()) ||
                      author.getLastName(true).toLowerCase().contains(personName.getFirst()))
                  {
                    cells.add(cell);
                    added = true;
                  }
                }

                if (personName.getLast().length() > 0)
                {
                  if (author.getFirstName(true).toLowerCase().contains(personName.getLast()) ||
                      author.getLastName(true).toLowerCase().contains(personName.getLast()))
                  {
                    cells.add(cell);
                    added = true;
                  }
                }
              }
            }

            if (added == false)
            {
              if (AllQueryEngine.linkList.getLinks().size() > 0)
              {
                for (KeywordLink keyLink : AllQueryEngine.linkList.getLinks())
                  if (keyLink.key.record == record)
                  {
                    cells.add(cell);
                    added = true;
                  }
              }
            }

            if (added == false)
            {
              if (record.getNameEngChar().trim().toLowerCase().contains(str))
              {
                cells.add(cell);
                atLeastOneRealMatch = true;
                added = true;
              }
            }
          }

          if (added == false)
            if (cell.getText().toLowerCase().contains(str))
            {
              if ((cell.getType() != hdtPerson) || (record != null)) // Don't use non-record author partial matches
              {
                cells.add(cell);
                atLeastOneRealMatch = true;
              }
            }
        }

        // There was no exact match

        HyperTableCell target = null;

        if ((cells.size() > 1) && atLeastOneRealMatch)
        {
          if (populator.getValueType() == CellValueType.cvtRecord)
          {
            RecordSelectDlgCtrlr ctrlr = RecordSelectDlgCtrlr.create(populator, cells, convertToEnglishChars(cb.getEditor().getText()).trim());

            if (ctrlr.showModal())
            {
              int id = ctrlr.getRecord().getID();
              selectID(id);

              if (table != null)
                table.selectID(colNdx, row, id);
            }
            else
              return;
          }
          else
          {
            ValueSelectDlgCtrlr ctrlr = ValueSelectDlgCtrlr.create(cells);

            if (ctrlr.showModal())
            {
              target = ctrlr.listView.getSelectionModel().getSelectedItem();
              select(target);

              if (table != null)
                row.setCellValue(colNdx, target);
            }
            else
              return;
          }
        }
        else if ((cells.size() == 1) && atLeastOneRealMatch)
        {
          target = cells.get(0);

          if (HyperTableCell.getCellID(target) > 0)
          {
            selectID(HyperTableCell.getCellID(target));
            endEditModeIfInTable();
            if (innerOnAction != null) innerOnAction.handle(event);

            return;
          }
        }
        else if ((populator.getRecordType(row) == hdtPerson) && (dontCreateNewRecord == false))
        {
          HDT_Person otherPerson = HDT_Person.lookUpByName(new PersonName(cb.getEditor().getText()));

          if (otherPerson != null)
          {
            for (HyperTableCell cell : cb.getItems())
              if (cell.getID() == otherPerson.getID())
              {
                selectID(otherPerson.getID());
                endEditModeIfInTable();
                if (innerOnAction != null) innerOnAction.handle(event);

                return;
              }
          }

          NewPersonDlgCtrlr npdc = NewPersonDlgCtrlr.create(false, cb.getEditor().getText(), null);

          if (npdc.showModal())
          {
            if (table == null)
            {
              setChoicesChanged();            // A new record has been created so force it to repopulate
              selectID(npdc.getPerson().getID());
            }
            else
            {
              Populator pop = table.getPopulator(colNdx);

              if (npdc.getPerson() != null)    // By the time we get back here, the ComboBox is gone
              {                                // and the table is already out of edit mode
                pop.setChanged(row);           // A new record has been created so force it to repopulate
                table.selectID(colNdx, row, npdc.getPerson().getID());
              }
              else
              {
                pop.populate(row, false);
                row.setCellValue(colNdx, pop.addEntry(row, -1, npdc.getNameLastFirst()));

                table.cancelEditing(); // For some reason in this case the keystroke event from earlier causes
              }                        // the table to enter edit mode again
            }
          }
        }
        else if ((populator.getRecordType(row) == hdtInstitution) && (dontCreateNewRecord == false))
        {
          ui.personHyperTab().newInstClick(row, cb.getEditor().getText(), colNdx);
        }
      }
    }
    else
    {
      cb.getSelectionModel().select(typedMatch);
      cb.setValue(typedMatch);
    }

    endEditModeIfInTable();

    if (innerOnAction != null) innerOnAction.handle(event);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<HyperTableCell> populate(boolean force)
  {
    HyperTableCell cell = cb.getValue();
    List<HyperTableCell> choices = populator.populate(row, force);
    cb.setItems(null);
    cb.setItems(FXCollections.observableList(choices));
    cb.setValue(cell);
    if (cell != null)
      cb.getSelectionModel().select(cell);

    if ((choices.size() > 0) && HyperTableCell.isEmpty(cell))
    {
      ListView<HyperTableCell> lv = getCBListView(cb);
      if (lv != null) lv.scrollTo(0);
    }

    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addBlankEntry() { addEntry(-1, "", false); }

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

  public void addEntry(int id, String value, boolean select)
  {
    HyperTableCell cell = populator.addEntry(row, id, value);
    if (select && (id > 0))
    {
      cb.setValue(cell);
      cb.getSelectionModel().select(cell);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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

  public HDT_RecordType selectedType()
  {
    if (populator != null)
    {
      HDT_RecordType type = populator.getRecordType(row);

      if ((type != null) && (type != hdtNone))
        return type;
    }

    return HyperTableCell.getCellType(cb.getValue());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void select(HyperTableCell cell)
  {
    cb.getSelectionModel().select(cell);
    cb.setValue(cell);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectID(int objID)
  {
    nullSwitch(populator.getChoiceByID(row, objID), choice ->
    {
      cb.getSelectionModel().select(choice);
      cb.setValue(choice);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectType(HDT_RecordType objType)
  {
    populate(false);

    nullSwitch(findFirst(cb.getItems(), choice -> choice.getType() == objType), choice ->
    {
      cb.getSelectionModel().select(choice);
      cb.setValue(choice);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commit()
  {
    if (somethingWasTyped && (typedMatch != null))
    {
      somethingWasTyped = false;
      selectID(typedMatch.getID());
      getOnAction().handle(new ActionEvent());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
