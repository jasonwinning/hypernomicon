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
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.App;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;

import java.io.IOException;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;

import javafx.fxml.FXMLLoader;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public class ArgumentTab extends HyperNodeTab<HDT_Argument, HDT_Argument>
{
  private ArgumentLowerPaneController lowerCtrlr;
  private HyperTable htParents, htWhereMade, htCounters;
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
    curArgument.addParentDisplayRecord();
    
    ctrlr.update(curArgument);
   
    // Select parent records in ComboBoxes
    // -----------------------------------

    htParents.buildRows(curArgument.positions, (row, position) ->
    {
      row.setCellValue(2, -1, db.getTypeName(hdtPosition), hdtPosition);
      row.setCellValue(3, position.getID(), position.listName(), hdtPosition);
      
      HDT_PositionVerdict posVerdict = curArgument.getPosVerdict(position);
      
      if (posVerdict != null)
        row.setCellValue(4, posVerdict.getID(), posVerdict.getCBText(), hdtPositionVerdict);
    });
   
    htParents.buildRows(curArgument.counteredArgs, (row, counteredArg) ->
    {
      row.setCellValue(2, -1, db.getTypeName(hdtArgument), hdtArgument);
      row.setCellValue(3, counteredArg, counteredArg.listName());
   
      HDT_ArgumentVerdict argVerdict = curArgument.getArgVerdict(counteredArg);
      
      if (argVerdict != null)
        row.setCellValue(4, argVerdict, argVerdict.getCBText());
    });

  // Populate the authors, works, and years
  // --------------------------------------
    
    htWhereMade.buildRows(curArgument.works, (row, work) ->
    {
      if (work.authorRecords.size() > 0)
        row.setCellValue(1, work.authorRecords.get(0), work.getLongAuthorsStr(true));
      else
        row.setCellValue(1, work, work.getLongAuthorsStr(true));

      row.setCellValue(2, work, work.getCBText());
      row.setCellValue(3, work, work.getYear(), HyperCellSortMethod.hsmNumeric);
    });

  // Populate the counterarguments
  // -----------------------------

    htCounters.buildRows(curArgument.counterArgs, (row, counterArg) ->
    {
      if (counterArg.works.size() > 0)
      {
        HDT_Work work = counterArg.works.get(0);
        
        if (work.authorRecords.size() > 0)
          row.setCellValue(1, work.authorRecords.get(0), work.getLongAuthorsStr(true));
        else
          row.setCellValue(1, work, work.getLongAuthorsStr(true));
      }  

      HDT_ArgumentVerdict argVerdict = counterArg.getArgVerdict(curArgument);
      if (argVerdict != null)
        row.setCellValue(2, counterArg, argVerdict.listName());

      row.setCellValue(3, counterArg, counterArg.listName());
    });
    
    lowerCtrlr.tabCounters.setText("Counterarguments (" + curArgument.counterArgs.size() + ")");

  // Set active tab
  // --------------

    boolean noWorks    = curArgument.works.isEmpty(),
            noCounters = curArgument.counterArgs.isEmpty();
    
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
      row.setCellValue(nextColNdx, new HyperTableCell(-1, "", parentType));

      if (parentType == hdtPosition)
        verdictPopulator.setRecordType(row, hdtPositionVerdict);
      else if (parentType == hdtArgument)
        verdictPopulator.setRecordType(row, hdtArgumentVerdict);
      else
        verdictPopulator.setRecordType(row, hdtNone);
      
      verdictPopulator.populate(row, true);
      row.setCellValue(nextColNdx + 1, new HyperTableCell(-1, "", verdictPopulator.getRecordType(row)));
    });
    
    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, new RecordByTypePopulator(), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) < 1)
        row.setCellValue(nextColNdx, new HyperTableCell(-1, "", verdictPopulator.getRecordType(row)));
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
      row.setCellValue(nextColNdx, new HyperTableCell(-1, "", hsPop.getRecordType(row)));
    });
    
    col.textHndlr = row -> nullSwitch((HDT_Work)row.getRecord(2), HyperTableCell.getCellText(row.getCell(1)), work -> work.getLongAuthorsStr(true));
    
    htWhereMade.addColAltPopulatorWithUpdateHandler(hdtWork, ctDropDownList, new HybridSubjectPopulator(rtAuthorOfWork), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) > 0)
      { 
        HDT_Work work = db.works.getByID(HyperTableCell.getCellID(cellVal));
        row.setCellValue(nextColNdx, new HyperTableCell(work.getID(), work.getYear(), hdtWork, HyperCellSortMethod.hsmNumeric));
      }
      else
        row.setCellValue(nextColNdx, new HyperTableCell(-1, "", hdtWork, HyperCellSortMethod.hsmNumeric));      
    });
    
    htWhereMade.addCol(hdtWork, ctNone);
    
    htWhereMade.addRemoveMenuItem();
    htWhereMade.addChangeOrderMenuItem(true);
    
    htCounters = new HyperTable(lowerCtrlr.tvCounters, 3, true, PREF_KEY_HT_ARG_COUNTERS);
    
    htCounters.addActionCol(ctGoNewBtn, 3);
    htCounters.addCol(hdtPerson, ctNone);
    htCounters.addCol(hdtArgumentVerdict, ctNone);
    htCounters.addCol(hdtArgument, ctNone);
       
    htWhereMade.getTV().focusedProperty().addListener((observable, oldValue, newValue) -> updateArgCounts());
    
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
       
    for (HyperTableRow row : htParents.getDataRows())
    {
      if ((row.getID(3) > 0) && (row.getID(4) < 1))
        okToSave = false;
      
      if ((row.getID(4) > 0) && (row.getID(3) < 1))
        okToSave = false;      
    }
    
    if (okToSave == false)
    {
      messageDialog("Unable to modify record: There must be a corresponding verdict for every position/argument targeted by this record.", mtError);
      return false;
    }

    saveObjectGroups(tagPositionVerdict, rtPositionOfArgument);
    saveObjectGroups(tagArgumentVerdict, rtCounterOfArgument);

    curArgument.setWorks(htWhereMade.saveToList(2, hdtWork));
    
    return true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void saveObjectGroups(Tag tag, RelationType relType)
  {
    HashMap<Integer, Tag> colNdxToTag = new HashMap<>();
    colNdxToTag.put(4, tag);
    
    List<ObjectGroup> tableGroups  = htParents.getObjectGroupList(curArgument, relType, 3, colNdxToTag);
    curArgument.updateObjectGroups(relType, tableGroups, colNdxToTag.values());
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

