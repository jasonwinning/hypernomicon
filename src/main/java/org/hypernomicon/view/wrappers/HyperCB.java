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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static javafx.scene.input.MouseButton.*;

import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.queryEngines.AllQueryEngine;
import org.hypernomicon.util.AutoCompleteCB;
import org.hypernomicon.view.dialogs.NewPersonDialogController;
import org.hypernomicon.view.dialogs.RecordSelectDialogController;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.HyperTab.TabEnum;
import org.hypernomicon.view.tabs.PersonTabController;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ComboBox;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------    

public class HyperCB implements CommitableWrapper
{
  @FunctionalInterface public interface CellTextHandler { public String getText(HyperTableRow row); }
  
  private final ComboBox<HyperTableCell> cb;
  private final Populator populator;  
  final HyperTableRow row;
    
  public HyperTableCell typedMatch;
  private HyperTableCell preShowingValue;
  private EventHandler<ActionEvent> onAction, innerOnAction;
  private boolean adjusting = false;
  public boolean somethingWasTyped, listenForActionEvents = true, dontCreateNewRecord = false;
  
  public static final HashMap<ComboBox<HyperTableCell>, HyperCB> cbRegistry = new HashMap<>();
  
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
    this.innerOnAction = onAction;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void triggerOnAction()
  {
    getOnAction().handle(new ActionEvent());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void setOnAction(EventHandler<ActionEvent> onAction) 
  { 
    if (onAction == null) return;
    
    if (populator instanceof VariablePopulator)
      this.innerOnAction = onAction;
    else
      this.onAction = onAction; 
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  public HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator, HyperTableRow row)
  {
    this(cb, ctrlType, newPopulator, row, true, null);
  }

  public HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator, HyperTableRow row, boolean addToRegistry)
  {
    this(cb, ctrlType, newPopulator, row, addToRegistry, null);
  }
 
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  public HyperCB(ComboBox<HyperTableCell> cb, HyperCtrlType ctrlType, Populator newPopulator, HyperTableRow row, boolean addToRegistry, HyperTable table)
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
    
    cb.setConverter(new StringConverter<HyperTableCell>() 
    {
      @Override public String toString(HyperTableCell cell) 
      {
        return nullSwitch(cell, "", HyperTableCell::getText);
      }

      @Override public HyperTableCell fromString(String string) 
      {
        if (cb.getItems() == null)
          return new HyperTableCell(-1, string, populator.getRecordType(row));
        
        for (HyperTableCell cell : cb.getItems()) 
          if (string.equals(cell.getText())) 
            return cell;
        
        return new HyperTableCell(-1, string, populator.getRecordType(row));
      }
    });

  //---------------------------------------------------------------------------  
           
    cb.setOnKeyReleased(new AutoCompleteCB(this, ctrlType == ctDropDownList));

  //---------------------------------------------------------------------------  
       
    cb.focusedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue)
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
      if (adjusting) return;
      if ((preShowingValue == null) || (table == null) || (table.autoCommitListSelections == false)) return;
      
      String newText = HyperTableCell.getCellText(cb.getValue());      
      if (newText.length() == 0) return;
      
      if (newText.equals(HyperTableCell.getCellText(preShowingValue)) == false)
        endEditModeIfInTable();
    });
    
    cb.getEditor().setOnMouseReleased(event -> 
    {
      if (event.getButton() == PRIMARY)
        if (cb.getEditor().getSelectedText().length() == 0)
          cb.getEditor().selectAll(); 
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

  public void endEditModeIfInTable()
  {
    if (isInTable())
      ComboBoxCell.class.cast(cb.getParent()).commit();
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void cbOnAction(ActionEvent event, HyperTable table)
  {    
    boolean added, alreadyParsedName = false;
    PersonName personName = null;
    
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
        boolean match, atLeastOneRealMatch = ((populator.getRecordType(row) != hdtPerson) || dontCreateNewRecord);
        AllQueryEngine.linkList.generate(str);
        
        for (HyperTableCell cell : cb.getItems()) 
        {
          HDT_Base record = HyperTableCell.getRecord(cell);
          added = false;
          
          if (record != null)
            match = record.getNameEngChar().toLowerCase().equals(str);
          else
            match = false;
          
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
            
            if (added == false)
            {
              if (record.getType() == hdtWork)
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
                    if ((author.getFirstName(true).toLowerCase().contains(personName.getFirst())) ||
                        (author.getLastName(true).toLowerCase().contains(personName.getFirst())))
                    {
                      cells.add(cell);
                      added = true;
                    }
                  }
                  
                  if (personName.getLast().length() > 0)
                  {
                    if ((author.getFirstName(true).toLowerCase().contains(personName.getLast())) ||
                        (author.getLastName(true).toLowerCase().contains(personName.getLast())))
                    {
                      cells.add(cell);
                      added = true;
                    }
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
              cells.add(cell);
              atLeastOneRealMatch = true;
            }
        }
        
        // There was no exact match
        
        HyperTableCell target = null;
        
        if ((cells.size() > 1) && atLeastOneRealMatch)
        {           
          RecordSelectDialogController ctrlr = RecordSelectDialogController.create("Choose a record", cells); 

          if (ctrlr.showModal())
          {
            target = ctrlr.listView.getSelectionModel().getSelectedItem();
            selectID(HyperTableCell.getCellID(target));
          
            if (table != null)
              table.selectID(colNdx, row, HyperTableCell.getCellID(target));
          }
          else
            return;
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
          
          NewPersonDialogController npdc = NewPersonDialogController.create(false, cb.getEditor().getText(), null);
          
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
              }
            }
          }
        }
        else if ((populator.getRecordType(row) == hdtInstitution) && (dontCreateNewRecord == false))
        {
          PersonTabController.class.cast(HyperTab.getHyperTab(TabEnum.personTab)).newInstClick(row, cb.getEditor().getText(), colNdx);
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
    
    return choices;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public static interface RecordString { public String get(HDT_Base record); }
  
  public void addBlankEntry() { addEntry(-1, "", false); }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void addAndSelectEntry(HyperObjPointer<? extends HDT_Base, ? extends HDT_Base> pntr, RecordString rs) 
  { 
    if (pntr.isNotNull())
      addAndSelectEntry(pntr.get(), rs); 
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void addAndSelectEntryOrBlank(HyperObjPointer<? extends HDT_Base, ? extends HDT_Base> pntr, RecordString rs) 
  { 
    if (pntr.isNotNull())
      addAndSelectEntry(pntr.get(), rs);
    else
      addBlankEntry();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void addAndSelectEntry(HDT_Base record, RecordString rs) 
  { 
    if (record != null)
      addEntry(record.getID(), rs.get(record), true); 
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void addAndSelectEntryOrBlank(HDT_Base record, RecordString rs)
  {
    if (record != null)
      addEntry(record.getID(), rs.get(record), true);
    else
      addBlankEntry();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void addEntry(int id, String value, boolean select)
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

  public <HDT_T extends HDT_Base> HDT_T selectedRecord()
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
  
  public void selectID(int objID)
  {
    populate(false);
    
    for (HyperTableCell choice : cb.getItems())
    {
      if ((choice.getID() == objID) || ((choice.getID() < 1) && (objID < 1)))
      {
        cb.getSelectionModel().select(choice);
        cb.setValue(choice);
        return;
      }
    }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public void selectType(HDT_RecordType objType)
  {
    populate(false);
    
    for (HyperTableCell choice : cb.getItems())
    {
      if (choice.getType() == objType)
      {
        cb.getSelectionModel().select(choice);
        cb.setValue(choice);
        return;
      }
    }
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
