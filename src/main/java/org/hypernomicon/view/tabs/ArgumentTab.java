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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.App;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;

import java.io.IOException;
import java.util.EnumSet;

import javafx.fxml.FXMLLoader;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public class ArgumentTab extends HyperNodeTab<HDT_Argument, HDT_Argument>
{
  public ArgumentLowerPaneController lowerCtrlr;
  public HyperTable htParents, htWhereMade, htCounters;
  private RecordByTypePopulator verdictPopulator;
  private HDT_Argument curArgument;
   
  @Override public HDT_RecordType getType()                  { return hdtArgument; }
  @Override public void enable(boolean enabled)              { ui.tabArguments.getContent().setDisable(enabled == false); }
  @Override public void findWithinDesc(String text)          { ctrlr.hilite(text); }  
  @Override public TextViewInfo getMainTextInfo()            { return ctrlr.getMainTextInfo(); }
  @Override public void setRecord(HDT_Argument activeRecord) { this.curArgument = activeRecord; }
  @Override public void focusOnSearchKey()                   { ctrlr.focusOnSearchKey(); }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  @Override public boolean update()
  {
    int ndx;
    boolean noWorks = true, noCounters = true;
    HDT_ArgumentVerdict argVerdict;
    HDT_PositionVerdict posVerdict;
    
    if (db.isLoaded() == false) return false;
    
    clear();
    
    if (curArgument == null)
    {
      enable(false);
      return false;
    }
    
    curArgument.addParentDisplayRecord();
    
    if (!ctrlr.update(curArgument)) return false;
   
    // Select parent records in ComboBoxes
    // -----------------------------------

     ndx = 0; for (HDT_Position position : curArgument.positions)
     {
       htParents.setDataItem(2, ndx, -1, db.getTypeName(hdtPosition), hdtPosition);
       htParents.setDataItem(3, ndx, position.getID(), position.listName(), hdtPosition);
       
       posVerdict = curArgument.getPosVerdict(position);
       
       if (posVerdict != null)
         htParents.setDataItem(4, ndx, posVerdict.getID(), posVerdict.getCBText(), hdtPositionVerdict);
       
       ndx++;
     }

     ndx = 0; for (HDT_Argument counteredArg : curArgument.counteredArgs)
     {
       int rowNdx = ndx + curArgument.positions.size(); 
       
       htParents.setDataItem(2, rowNdx, -1, db.getTypeName(hdtArgument), hdtArgument);
       htParents.setDataItem(3, rowNdx, counteredArg.getID(), counteredArg.listName(), hdtArgument);

       argVerdict = curArgument.getArgVerdict(counteredArg);
       
       if (argVerdict != null)
         htParents.setDataItem(4, rowNdx, argVerdict.getID(), argVerdict.getCBText(), hdtArgumentVerdict);
       
       ndx++;
     }    

  // Populate the authors, works, and years
  // --------------------------------------
    
    ndx = 0; for (HDT_Work work : curArgument.works)
    {
      if (work.authorRecords.size() > 0)
        htWhereMade.setDataItem(1, ndx, work.authorRecords.get(0).getID(), work.getLongAuthorsStr(true), hdtPerson);
      else
        htWhereMade.setDataItem(1, ndx, work.getID(), work.getLongAuthorsStr(true), hdtWork);

      htWhereMade.setDataItem(2, ndx, work.getID(), work.getCBText(), hdtWork);
      htWhereMade.setDataItem(3, ndx, work.getID(), work.getYear(), hdtWork, HyperCellSortMethod.hsmNumeric);
      
      ndx++;
    }

    noWorks = curArgument.works.isEmpty();

  // Populate the counterarguments
  // -----------------------------

    ndx = 0; for (HDT_Argument counterArg : curArgument.counterArgs)
    {
      noCounters = false;

      if (counterArg.works.size() > 0)
      {
        HDT_Work work = counterArg.works.get(0);
        
        if (work.authorRecords.size() > 0)
          htCounters.setDataItem(1, ndx, work.authorRecords.get(0).getID(), work.getLongAuthorsStr(true), hdtPerson);
        else
          htCounters.setDataItem(1, ndx, work.getID(), work.getLongAuthorsStr(true), hdtWork);
      }  

      argVerdict = counterArg.getArgVerdict(curArgument);
      if (argVerdict != null)
        htCounters.setDataItem(2, ndx, counterArg.getID(), argVerdict.listName(), hdtArgument);

      htCounters.setDataItem(3, ndx, counterArg.getID(), counterArg.listName(), hdtArgument);
      ndx++;
    }

    lowerCtrlr.tabCounters.setText("Counterarguments (" + curArgument.counterArgs.size() + ")");

  // Set active tab
  // --------------

    Tab tab = lowerCtrlr.tabPane.getSelectionModel().getSelectedItem();
    
    if (((tab == lowerCtrlr.tabWhereMade) && noWorks) ||
        ((tab == lowerCtrlr.tabCounters) && noCounters))
    {
      if (noCounters == false) tab = lowerCtrlr.tabCounters;
      else if (noWorks == false) tab = lowerCtrlr.tabWhereMade;
      
      lowerCtrlr.tabPane.getSelectionModel().select(tab);
    }
    
    updateArgCounts();
     
    return true;  
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void updateArgCounts()
  {
    lowerCtrlr.tabWhereMade.setText("Where made (" + htWhereMade.getDataRowCount() + ")");
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override protected void init(TabEnum tabEnum)
  { 
    this.tabEnum = tabEnum;
    ctrlr.init(hdtArgument, this);
    
    AnchorPane aP = new AnchorPane();

    ctrlr.lblParentCaption.setText("Responds to:");
       
    ctrlr.gpToolBar.getChildren().set(0, aP);
    
    TableColumn<HyperTableRow, HyperTableCell> verdictCol = new TableColumn<>("Argues that");
    verdictCol.setPrefWidth(250.0);
    ctrlr.tvParents.getColumns().add(verdictCol);
    
    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/tabs/ArgumentLowerPane.fxml"));
    
    try { ctrlr.spMain.getItems().set(2, loader.load()); } catch (IOException e) { noOp(); }
    
    lowerCtrlr = loader.getController();
       
    htParents = new HyperTable(ctrlr.tvParents, 3, true, PREF_KEY_HT_ARG_PARENTS);
    
    htParents.addActionCol(ctGoBtn, 3);
    htParents.addActionCol(ctBrowseBtn, 3);
    
    verdictPopulator = new RecordByTypePopulator();
    
    RecordTypePopulator rtp = new RecordTypePopulator();
    EnumSet<HDT_RecordType> types = EnumSet.noneOf(HDT_RecordType.class);
    
    types.add(hdtPosition);
    types.add(hdtArgument);
    
    rtp.setTypes(types);
    
    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, rtp, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      RecordByTypePopulator rbtp = (RecordByTypePopulator)nextPopulator;
      
      HDT_RecordType parentType = cellVal.getType();
      rbtp.setRecordType(row, parentType);
      rbtp.setChanged(row);
      row.updateCell(nextColNdx, new HyperTableCell(-1, "", parentType));

      if (parentType == hdtPosition)
        verdictPopulator.setRecordType(row, hdtPositionVerdict);
      else if (parentType == hdtArgument)
        verdictPopulator.setRecordType(row, hdtArgumentVerdict);
      else
        verdictPopulator.setRecordType(row, hdtNone);
      
      verdictPopulator.populate(row, true);
      row.updateCell(nextColNdx + 1, new HyperTableCell(-1, "", verdictPopulator.getRecordType(row)));
    });
    
    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, new RecordByTypePopulator(), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) < 1)
        row.updateCell(nextColNdx, new HyperTableCell(-1, "", verdictPopulator.getRecordType(row)));
    });
    
    htParents.addColAltPopulator(hdtNone, ctDropDownList, verdictPopulator);
    
    htParents.addRemoveMenuItem();
    htParents.addChangeOrderMenuItem(true);
    
    htWhereMade = new HyperTable(lowerCtrlr.tvWhereMade, 2, true, PREF_KEY_HT_ARG_SRC);
    
    htWhereMade.addActionCol(ctGoNewBtn, 2);
    HyperTableColumn col = htWhereMade.addColWithUpdateHandler(hdtPerson, ctDropDownList, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      HDT_Base obj = HyperTableCell.getRecord(cellVal);
      HybridSubjectPopulator hsPop = (HybridSubjectPopulator)nextPopulator;
      
      if (hsPop.getObj(row) == obj) return;
      
      hsPop.setObj(row, obj);
      row.updateCell(nextColNdx, new HyperTableCell(-1, "", hsPop.getRecordType(row)));
    });
    
    col.textHndlr = row -> 
    {
      HDT_Base work = row.getRecord(2);
      
      if (work == null)
        return HyperTableCell.getCellText(row.getCell(1));        
      
      return HDT_Work.class.cast(work).getLongAuthorsStr(true);
    };
    
    htWhereMade.addColAltPopulatorWithUpdateHandler(hdtWork, ctDropDownList, new HybridSubjectPopulator(rtAuthorOfWork), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) > 0)
      { 
        HDT_Work work = db.works.getByID(HyperTableCell.getCellID(cellVal));
        row.updateCell(nextColNdx, new HyperTableCell(work.getID(), work.getYear(), hdtWork, HyperCellSortMethod.hsmNumeric));
      }
      else
        row.updateCell(nextColNdx, new HyperTableCell(-1, "", hdtWork, HyperCellSortMethod.hsmNumeric));      
    });
    
    htWhereMade.addCol(hdtWork, ctNone);
    
    htWhereMade.addRemoveMenuItem();
    htWhereMade.addChangeOrderMenuItem(true);
    
    htCounters = new HyperTable(lowerCtrlr.tvCounters, 3, true, PREF_KEY_HT_ARG_COUNTERS);
    
    htCounters.addActionCol(ctGoNewBtn, 3);
    htCounters.addCol(hdtPerson, ctNone);
    htCounters.addCol(hdtArgumentVerdict, ctNone);
    htCounters.addCol(hdtArgument, ctNone);
       
    htWhereMade.tv.focusedProperty().addListener((observable, oldValue, newValue) -> updateArgCounts());
    
    initContextMenus();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void initContextMenus()
  {
    RecordListView.addDefaultMenuItems(htWhereMade);
          
    htWhereMade.addContextMenuItem(hdtWork, "Go to work record", 
      record -> ui.goToRecord(record, true));
    
    htWhereMade.addContextMenuItem(hdtPerson, "Go to person record",
      person -> ui.goToRecord(person, true));
    
    RecordListView.CondRecordHandler launchCondHandler = 
      record ->
      {
        if (record.getType() != hdtArgument) return false;
        HDT_Argument argument = (HDT_Argument)record;
        
        for (HDT_Work work : argument.works)
        {
          if (work.getPath().isEmpty() == false)
            return true;
        }
        
        return false;          
      };
    
    RecordListView.RecordHandler launchHandler = 
      record ->
      {
        HDT_Argument argument = (HDT_Argument)record;
        
        for (HDT_Work work : argument.works)
        {
          if (work.getPath().isEmpty() == false)
          {
            work.launch(-1);
            return;
          }
        }
      };
    
    RecordListView.CondRecordHandler workGoToCondHandler = 
      record ->
      {
        if (record.getType() != hdtArgument) return false;
        HDT_Argument argument = (HDT_Argument)record;
        return argument.works.size() > 0;
      };
    
    RecordListView.RecordHandler workGoToHandler = 
      record ->
      {
        HDT_Argument argument = (HDT_Argument)record;
        
        for (HDT_Work work : argument.works)
        {
          if (work.getPath().isEmpty() == false)
          {
            ui.goToRecord(work, true);
            return;
          }
        }      
        
        ui.goToRecord(argument.works.get(0), true);
      };
      
    htCounters.addCondContextMenuItem(hdtArgument, "Launch work file", launchCondHandler, launchHandler);
    htCounters.addCondContextMenuItem(hdtArgument, "Go to work record", workGoToCondHandler, workGoToHandler);
    htCounters.addContextMenuItem(hdtPerson, "Go to person record",
        person -> ui.goToRecord(person, true));
    
    htCounters.addContextMenuItem(hdtArgument, "Go to argument record", 
        argument -> ui.goToRecord(argument, true));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void clear()
  {
    ctrlr.clear();
    
    htParents.clear();
    htWhereMade.clear();
    htCounters.clear();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public boolean saveToRecord(boolean showMessage)
  {
    boolean okToSave = true;
    
    if (!ctrlr.save(curArgument, showMessage, this)) return false;
       
    for (int rowNdx = 0; rowNdx < htParents.getDataRowCount(); rowNdx++)
    {
      if ((htParents.getID(3, rowNdx) > 0) && (htParents.getID(4,  rowNdx) < 1))
        okToSave = false;
      
      if ((htParents.getID(4, rowNdx) > 0) && (htParents.getID(3,  rowNdx) < 1))
        okToSave = false;      
    }
    
    if (okToSave == false)
    {
      messageDialog("Unable to modify record: There must be a corresponding verdict for every position/argument targeted by this record.", mtError);
      return false;
    }
    
    curArgument.setPositions(htParents);
    curArgument.setCounterArgs(htParents);

    curArgument.setWorks(htWhereMade);
    
    return true;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void newClick(HDT_RecordType objType, HyperTableRow row)
  {
    if (ui.cantSaveRecord(true)) return;
    
    switch (objType)
    {
      case hdtArgument :
       
        HDT_Argument counterArg = db.createNewBlankRecord(hdtArgument);

        counterArg.addCounterArg(curArgument, null);

        for (HDT_Position position : curArgument.positions)
          counterArg.addPosition(position, null);

        ui.goToRecord(counterArg, false);
        
        lowerCtrlr.tabPane.getSelectionModel().select(lowerCtrlr.tabWhereMade);
        
        break;
        
      case hdtWork :
        
        HDT_Work work = db.createNewBlankRecord(hdtWork);
        
        HDT_Person author = db.persons.getByID(row.getID(1));
        if (author != null) work.getAuthors().add(author);
        
        curArgument.works.add(work);
        
        ui.goToRecord(work, false);
        break;
        
      default:
        break;
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void setDividerPositions()
  {
    setDividerPosition(ctrlr.spMain, PREF_KEY_ARG_TOP_VERT, 0);
    setDividerPosition(ctrlr.spMain, PREF_KEY_ARG_BOTTOM_VERT, 1);
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void getDividerPositions()
  {
    getDividerPosition(ctrlr.spMain, PREF_KEY_ARG_TOP_VERT, 0);
    getDividerPosition(ctrlr.spMain, PREF_KEY_ARG_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}

