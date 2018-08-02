/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.view.tabs;

import java.util.EnumSet;

import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_Position.PositionSource;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PositionVerdict;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.NewArgDialogController;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.RecordListView;
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;

//---------------------------------------------------------------------------

public class PositionTab extends HyperNodeTab<HDT_Position, HDT_Position>
{
  HyperTable htParents, htArguments, htSubpositions;
  private HDT_Position curPosition;
  
  @Override public HDT_RecordType getType()                  { return hdtPosition; }
  @Override public void enable(boolean enabled)              { ui.tabPositions.getContent().setDisable(enabled == false); }
  @Override public void focusOnSearchKey()                   { ctrlr.focusOnSearchKey(); }
  @Override public void findWithinDesc(String text)          { ctrlr.hilite(text); }  
  @Override public TextViewInfo getMainTextInfo()            { return ctrlr.getMainTextInfo(); }
  @Override public void setRecord(HDT_Position activeRecord) { curPosition = activeRecord; }    

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  @Override public boolean update()
  {
    int ndx;
    HDT_Work work;
    
    if (db.isLoaded() == false) return false;
    
    clear();
    
    if (curPosition == null)
    {
      enable(false);
      return false;
    }
    
    curPosition.addParentDisplayRecord();
    
    if (!ctrlr.update(curPosition)) return false;
    
 // Select parent records in ComboBoxes
 // -----------------------------------

    ndx = 0; for (HDT_Position otherPos : curPosition.largerPositions)
    {
      htParents.setDataItem(2, ndx, -1, db.getTypeName(hdtPosition), hdtPosition);
      htParents.setDataItem(3, ndx, otherPos.getID(), otherPos.listName(), hdtPosition);
      ndx++;
    }

    ndx = 0; for (HDT_Debate debate : curPosition.debates)
    {
      htParents.setDataItem(2, ndx + curPosition.largerPositions.size(), -1, db.getTypeName(hdtDebate), hdtDebate);
      htParents.setDataItem(3, ndx + curPosition.largerPositions.size(), debate.getID(), debate.listName(), hdtDebate);
      ndx++;
    }
   
// Populate arguments
// ------------------

    ndx = 0; for (HDT_Argument argument : curPosition.arguments)
    { 
      work = null;
      
      if (argument.works.size() > 0)
      {
        work = argument.works.get(0);
        if (work.authorRecords.size() > 0)
          htArguments.setDataItem(1, ndx, work.authorRecords.get(0).getID(), work.getShortAuthorsStr(true), hdtPerson);
        else
          htArguments.setDataItem(1, ndx, work.getID(), work.getShortAuthorsStr(true), hdtWork);
      }
      
      if (work != null)
      {
        htArguments.setDataItem(3, ndx, argument.getID(), work.getYear(), hdtArgument, HyperCellSortMethod.hsmNumeric);
        htArguments.setDataItem(4, ndx, work.getID(), work.name(), hdtWork);
      }
      else
        htArguments.setDataItem(3, ndx, argument.getID(), "", hdtArgument);
  
      HDT_PositionVerdict verdict = argument.getPosVerdict(curPosition);
      if (verdict != null)
        htArguments.setDataItem(2, ndx, argument.getID(), verdict.listName(), hdtArgument);
      
      htArguments.setDataItem(5, ndx, argument.getID(), argument.listName(), hdtArgument);
      
      ndx++;
    }
    
 // Populate subpositions
 // ---------------------

    ndx = 0; for (HDT_Position subPos : curPosition.subPositions)
    {
      htSubpositions.setDataItem(1, ndx, subPos.getID(), subPos.getCBText(), hdtPosition);

      PositionSource ps = subPos.getWorkWithAuthor();
      if (ps != null)
        htSubpositions.setDataItem(2, ndx, ps.author.getID(), Authors.getShortAuthorsStr(subPos.getPeople(), true, true), hdtPerson);
      else
        htSubpositions.setDataItem(2, ndx, -1, Authors.getShortAuthorsStr(subPos.getPeople(), true, true), hdtPerson);
      
      ndx++;
    }
    
    return true;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override protected void init(TabEnum tabEnum)
  {
    this.tabEnum = tabEnum;
    ObservableList<TableColumn<HyperTableRow, ?>> cols;
    
    ctrlr.init(hdtPosition, this);
    
    cols = ctrlr.tvLeftChildren.getColumns();
    
    cols.get(2).setText("Title of Work");
    cols.add(2, new TableColumn<HyperTableRow, HyperTableCell>("Year"));
    cols.add(2, new TableColumn<HyperTableRow, HyperTableCell>("Argues Position Is"));
    cols.add(new TableColumn<HyperTableRow, HyperTableCell>("Arg. Name"));
    
    ctrlr.spChildren.setDividerPositions(0.6);
    
    cols = ctrlr.tvRightChildren.getColumns();
    
    cols.add(1, new TableColumn<HyperTableRow, HyperTableCell>("Sub-Position Name"));
    cols.get(2).setText("Person");
    
    htParents = new HyperTable(ctrlr.tvParents, 3, true, PREF_KEY_HT_POS_PARENTS);
    
    htParents.addActionCol(ctGoBtn, 3);
    htParents.addActionCol(ctBrowseBtn, 3);
    
    RecordTypePopulator rtp = new RecordTypePopulator();
    EnumSet<HDT_RecordType> types = EnumSet.noneOf(HDT_RecordType.class);
    
    types.add(hdtDebate);
    types.add(hdtPosition);
    
    rtp.setTypes(types);
    
    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, rtp, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      RecordByTypePopulator rbtp = (RecordByTypePopulator)nextPopulator;
      
      HDT_RecordType parentType = cellVal.getType();
      rbtp.setRecordType(row, parentType);
      rbtp.setChanged(row);
      row.updateCell(nextColNdx, new HyperTableCell(-1, "", parentType));
    });
    
    htParents.addColAltPopulator(hdtNone, ctDropDownList, new RecordByTypePopulator());
    
    htParents.addRemoveMenuItem();
    htParents.addChangeOrderMenuItem(true);
    
    htArguments = new HyperTable(ctrlr.tvLeftChildren, 3, true, PREF_KEY_HT_POS_ARG);
    
    htArguments.addActionCol(ctGoNewBtn, 3);
    htArguments.addCol(hdtPerson, ctNone);
    htArguments.addCol(hdtPositionVerdict, ctNone);
    htArguments.addCol(hdtArgument, ctNone);
    htArguments.addCol(hdtWork, ctNone);
    htArguments.addCol(hdtArgument, ctNone);
    
    htSubpositions = new HyperTable(ctrlr.tvRightChildren, 1, true, PREF_KEY_HT_POS_SUB);
    
    htSubpositions.addActionCol(ctGoNewBtn, 1);
    htSubpositions.addCol(hdtPosition, ctNone);
    htSubpositions.addCol(hdtPerson, ctNone);
    
    initArgContextMenu();
    ui.initPositionContextMenu(htSubpositions);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void initArgContextMenu()
  {
    RecordListView.addDefaultMenuItems(htArguments);
       
    htArguments.addContextMenuItem(hdtWork, "Go to work record", 
      record -> ui.goToRecord(record, true));
    
    htArguments.addContextMenuItem(hdtPerson, "Go to person record",
      record -> ui.goToRecord(record, true));
    
    htArguments.addContextMenuItem(hdtArgument, "Go to argument record",
      record -> ui.goToRecord(record, true));
  }
 
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public void clear()
  {
    ctrlr.clear();
    
    htParents.clear();
    htArguments.clear();
    htSubpositions.clear();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public boolean saveToRecord(boolean showMessage)
  {   
    if (!ctrlr.save(curPosition, showMessage, this)) return false;
    
    curPosition.setLargerPositions(htParents);
    curPosition.setDebates(htParents);
    
    ui.attachOrphansToRoots();
    
    return true;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public void newClick(HDT_RecordType objType, HyperTableRow row)
  {
    if (ui.cantSaveRecord(true)) return;
    
    switch (objType)
    {
      case hdtPosition :
        
        HDT_Position newPos = db.createNewBlankRecord(hdtPosition);

        newPos.largerPositions.add(curPosition);

        ui.goToRecord(newPos, false);
        
        break;
        
      case hdtArgument :
        
        newArgumentClick();
        break;
      
      default:
        break;
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public void newArgumentClick()
  {
    HDT_Argument argument;
    HDT_Work work;
    HDT_Person person;
    
    NewArgDialogController newArgDialog = NewArgDialogController.create("New Argument", curPosition);
      
    if (newArgDialog.showModal())
    {
      argument = db.createNewBlankRecord(hdtArgument);

      HDT_PositionVerdict verdict = newArgDialog.hcbPositionVerdict.selectedRecord();

      argument.addPosition(curPosition, verdict);
      
      if      (newArgDialog.rbArgName1.isSelected()) argument.setName(newArgDialog.tfArgName1.getText());
      else if (newArgDialog.rbArgName2.isSelected()) argument.setName(newArgDialog.tfArgName2.getText());
      else if (newArgDialog.rbArgName3.isSelected()) argument.setName(newArgDialog.tfArgName3.getText());
      else if (newArgDialog.rbArgName4.isSelected()) argument.setName(newArgDialog.tfArgName4.getText());
      else if (newArgDialog.rbArgName5.isSelected()) argument.setName(newArgDialog.tfArgName5.getText());
      else if (newArgDialog.rbArgName6.isSelected()) argument.setName(newArgDialog.tfArgName6.getText());
      else if (newArgDialog.rbArgName7.isSelected()) argument.setName(newArgDialog.tfArgName7.getText());
      else                                           argument.setName(newArgDialog.tfArgName8.getText());

      if (newArgDialog.rbNew.isSelected())
      {
        work = db.createNewBlankRecord(hdtWork);

        work.setName(newArgDialog.tfTitle.getText());
        person = newArgDialog.hcbPerson.selectedRecord();
        if (person != null)
          work.getAuthors().add(person);
      }
      else
        work = newArgDialog.hcbWork.selectedRecord();

      if (work != null)
        argument.works.add(work);

      ui.goToRecord(argument, false);       
    }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  @Override public void setDividerPositions()
  {
    setDividerPosition(ctrlr.spMain, PREF_KEY_POS_TOP_VERT, 0);
    setDividerPosition(ctrlr.spMain, PREF_KEY_POS_BOTTOM_VERT, 1);
    setDividerPosition(ctrlr.spChildren, PREF_KEY_POS_BOTTOM_HORIZ, 0);
  }  

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void getDividerPositions()
  {
    getDividerPosition(ctrlr.spMain, PREF_KEY_POS_TOP_VERT, 0);
    getDividerPosition(ctrlr.spMain, PREF_KEY_POS_BOTTOM_VERT, 1);
    getDividerPosition(ctrlr.spChildren, PREF_KEY_POS_BOTTOM_HORIZ, 0);
  }  

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
}
