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

import org.hypernomicon.model.records.HDT_Institution;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.mtError;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;

//---------------------------------------------------------------------------

public class InstitutionTabController extends HyperTab<HDT_Institution, HDT_Institution>
{
  private boolean alreadyChangingLocation;
  public HyperTable htSubInstitutions, htPersons;
  public HyperCB hcbState, hcbCountry, hcbType, hcbParentInst;
  private HDT_Institution curInstitution;
  
  @FXML private TextField tfCity;
  @FXML private TextField tfName;
  @FXML private Button btnLink;
  @FXML private Button btnParent;
  @FXML private TextField tfLink;
  @FXML private ComboBox<HyperTableCell> cbType;
  @FXML private ComboBox<HyperTableCell> cbParentInst;
  @FXML private ComboBox<HyperTableCell> cbState;
  @FXML private ComboBox<HyperTableCell> cbCountry;
  @FXML private TableView<HyperTableRow> tvSubInstitutions;
  @FXML private TableView<HyperTableRow> tvPersons;
  @FXML private Hyperlink hlGoogleMaps;
  @FXML private SplitPane spHoriz;
  
  @Override public HDT_RecordType getType()                                 { return hdtInstitution; }
  @Override public void enable(boolean enabled)                             { ui.tabInstitutions.getContent().setDisable(enabled == false); }
  @Override public void focusOnSearchKey()                                  { return; }
  @Override public void newClick(HDT_RecordType objType, HyperTableRow row) { return; }
  @Override public void setRecord(HDT_Institution activeRecord)             { curInstitution = activeRecord; }
  @Override public void setDividerPositions()                               { setDividerPosition(spHoriz, PREF_KEY_INST_MID_HORIZ, 0); }
  @Override public void getDividerPositions()                               { getDividerPosition(spHoriz, PREF_KEY_INST_MID_HORIZ, 0); }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  @Override public boolean update()
  {
    HashMap<HDT_Person, HashSet<HDT_Institution>> peopleMap = new HashMap<>();
    int subInstRowNdx, personRowNdx;
    
    if (db.isLoaded() == false) return false;
    
    clear();
    
    if (curInstitution == null)
    {
      enable(false);
      return false;
    }

    tfName.setText(curInstitution.name());
    tfCity.setText(curInstitution.getCity());
    tfLink.setText(curInstitution.getWebLink());
    
    if (curInstitution.state.isNotNull())
      hcbState.addEntry(curInstitution.state.getID(), curInstitution.state.get().name(), curInstitution.state.getID());
    else
      hcbState.addEntry(-1, "", -1);
    
    if (curInstitution.country.isNotNull())
      hcbCountry.addEntry(curInstitution.country.getID(), curInstitution.country.get().name(), curInstitution.country.getID());
    else
      hcbCountry.addEntry(-1, "", -1);
    
    if (curInstitution.instType.isNotNull())
      hcbType.addEntry(curInstitution.instType.getID(), curInstitution.instType.get().name(), curInstitution.instType.getID());
    else
      hcbType.addEntry(-1, "", -1);
    
    if (curInstitution.parentInst.isNotNull())
      hcbParentInst.addEntry(curInstitution.parentInst.getID(), curInstitution.parentInst.get().name(), curInstitution.parentInst.getID());
    else
      hcbParentInst.addEntry(-1, "", -1);
    
 // Populate departments and people
 // -------------------------------
    
    subInstRowNdx = 0;
    
    addPersonsFromInst(curInstitution, curInstitution, peopleMap);
    
    for (HDT_Institution subInst : curInstitution.subInstitutions)
    {  
      htSubInstitutions.setDataItem(1, subInstRowNdx, subInst.getID(), subInst.name(), hdtInstitution);      
      if (subInst.instType.isNotNull())
        htSubInstitutions.setDataItem(2, subInstRowNdx, subInst.instType.getID(), subInst.instType.get().name(), hdtInstitutionType);
      htSubInstitutions.setDataItem(3, subInstRowNdx, subInst.getID(), subInst.getWebLink(), hdtInstitution);
            
      subInstRowNdx++;
    }    
    
    if (curInstitution.isDeptOrFaculty() && curInstitution.parentInst.isNotNull())
    {
      for (HDT_Institution sibInst : curInstitution.parentInst.get().subInstitutions)
      {
        if (sibInst != curInstitution)
          addSiblingInsts(sibInst, sibInst, peopleMap);
      }
    }
    
    personRowNdx = 0;
    
    for (HDT_Person person : peopleMap.keySet())
    {
      htPersons.setDataItem(0, personRowNdx, person.getID(), person.listName(), hdtPerson);
      htPersons.setDataItem(1, personRowNdx, person.rank.getID(), (person.rank.isNotNull()) ? person.rank.get().name() : "", hdtRank);
      htPersons.setDataItem(2, personRowNdx, person.field.getID(), (person.field.isNotNull()) ? person.field.get().name() : "", hdtField);
      
      ArrayList<HDT_Institution> instList = new ArrayList<>();
      instList.addAll(peopleMap.get(person));
      instList.sort((inst1, inst2) -> inst1.name().compareTo(inst2.name()));      
      
      String instStr = "";
      int instID = -1;
      
      for (HDT_Institution inst : instList)
      {
        if (instID == -1)
        {
          instID = inst.getID();
          instStr = inst.name();
        }
        else
          instStr = instStr + ", " + inst.name();
      }
            
      htPersons.setDataItem(3, personRowNdx, instID, instStr, hdtInstitution);
            
      personRowNdx++;
    }
    
    htPersons.tv.getSortOrder().clear();
    htPersons.tv.getSortOrder().add(htPersons.tv.getColumns().get(0));
    
    safeFocus(tfName);
    
    return true; 
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void addSiblingInsts(HDT_Institution sibInst, HDT_Institution cousinInst, HashMap<HDT_Person, HashSet<HDT_Institution>> peopleMap)
  {
    if (cousinInst.subInstitutions.isEmpty() == false)
    {
      for (HDT_Institution subInst : cousinInst.subInstitutions)
        addSiblingInsts(sibInst, subInst, peopleMap);
    }
    
    for (HDT_Person person : cousinInst.persons)
    {
      if (peopleMap.containsKey(person))
        peopleMap.get(person).add(sibInst);  
    }    
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  private void addPersonsFromInst(HDT_Institution nearestChildInst, HDT_Institution inst, HashMap<HDT_Person, HashSet<HDT_Institution>> peopleMap)
  {
    if (inst.subInstitutions.isEmpty() == false)
    {
      for (HDT_Institution subInst : inst.subInstitutions)
      {
        if (nearestChildInst == curInstitution)
          addPersonsFromInst(subInst, subInst, peopleMap);
        else
          addPersonsFromInst(nearestChildInst, subInst, peopleMap);
      }
    }
          
    for (HDT_Person person : inst.persons)
    {
      if (peopleMap.containsKey(person) == false)
        peopleMap.put(person, new HashSet<HDT_Institution>());
        
      peopleMap.get(person).add(nearestChildInst);        
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override protected void init(TabEnum tabEnum)
  {
    this.tabEnum = tabEnum;
    alreadyChangingLocation = false;
    
    htSubInstitutions = new HyperTable(tvSubInstitutions, 1, true, PREF_KEY_HT_INST_SUB);

    htSubInstitutions.addActionColWithButtonHandler(ctLinkBtn, 3, (row, colNdx) ->
    {
      String link = row.getText(colNdx);
      
      if (link.length() == 0)
        searchGoogle(tfName.getText() + " " + row.getText(1), true);
      else
        openWebLink(row.getText(colNdx));  
    });
    
    htSubInstitutions.addTextEditCol(hdtInstitution, true, false);
    htSubInstitutions.addCol(hdtInstitutionType, ctDropDownList);
    htSubInstitutions.addTextEditCol(hdtInstitution, true, false);
    
    htSubInstitutions.addContextMenuItem(hdtInstitution, "Go to this record", record ->
    {
      ui.goToRecord(record, true);
    });
    
    htSubInstitutions.addContextMenuItem(hdtInstitution, "Delete this institution record", record ->
    {
      if (ui.cantSaveRecord(true)) return;
      if (confirmDialog("Are you sure you want to delete this record?") == false) return;
      db.deleteRecord(hdtInstitution, record.getID());
      ui.update();
    });
    
    htSubInstitutions.addChangeOrderMenuItem(true, () ->
    {
      ArrayList<HDT_Institution> list = htSubInstitutions.saveToList(1, hdtInstitution);
      curInstitution.subInstitutions.reorder(list, true);
    });
    
    htPersons = new HyperTable(tvPersons, 0, false, PREF_KEY_HT_INST_PEOPLE);
    
    htPersons.addCol(hdtPerson, ctNone);
    htPersons.addCol(hdtRank, ctNone);
    htPersons.addCol(hdtField, ctNone);
    htPersons.addCol(hdtInstitution, ctNone);
    
    hcbState = new HyperCB(cbState, ctDropDownList, new StandardPopulator(hdtState), null);
    hcbCountry = new HyperCB(cbCountry, ctDropDownList, new StandardPopulator(hdtCountry), null);
    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtInstitutionType), null);
    hcbParentInst = new HyperCB(cbParentInst, ctDropDownList, new StandardPopulator(hdtInstitution), null);
    
    cbState.valueProperty().addListener((observable, oldValue, newValue) -> 
    {
      if (alreadyChangingLocation) return;
      
      if (HyperTableCell.getCellID(newValue) > 0)
      {
        alreadyChangingLocation = true;   
        hcbCountry.selectID(1);
        alreadyChangingLocation = false;
      }
    });
    
    cbCountry.valueProperty().addListener((observable, oldValue, newValue) -> 
    {
      if (alreadyChangingLocation) return;
      
      if (HyperTableCell.getCellID(newValue) > 1)
      {
        alreadyChangingLocation = true;
        hcbState.selectID(-1);
        alreadyChangingLocation = false;
      }
    });
    
    btnParent.setOnAction(event -> 
    {
      ui.goToRecord(HyperTableCell.getRecord(hcbParentInst.selectedHTC()), true);
    });
    
    btnLink.setOnAction(event -> openWebLink(tfLink.getText()));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  @FXML private void linkClick()
  {
    String link = "https://maps.google.com/maps?q=" + tfName.getText() + ",+" + 
                  tfCity.getText() + ",+" + hcbState.getText() + ",+" + 
                  hcbCountry.getText() + "&hl=en";    

    openWebLink(link.replace(' ', '+'));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void clear()
  {
    tfName.clear();
    tfCity.clear();
    tfLink.clear();
    
    hcbState.clear();
    hcbCountry.clear();
    hcbParentInst.clear();
    hcbType.clear();
    
    htSubInstitutions.clear();
    htPersons.clear();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  @Override public boolean saveToRecord(boolean showMessage)
  {
    int ndx, subInstID, numRows;
    HDT_Institution subInst;
    boolean locationChanged = false;
    
    if (hcbType.selectedID() < 1)
    {
      if (showMessage)
        messageDialog("You must select a type.", mtError);
      
      safeFocus(this.cbType);
      return false;
    }
    
    if (tfCity.getText().trim().length() > 0)
      if (curInstitution.getCity().trim().equalsIgnoreCase(tfCity.getText().trim()) == false)
        locationChanged = true;
    
    if (hcbState.selectedRecord() != null)
      if (curInstitution.state.get() != hcbState.selectedRecord())
        locationChanged = true;
    
    if (hcbCountry.selectedRecord() != null)
      if (curInstitution.country.get() != hcbCountry.selectedRecord())
        locationChanged = true;
      
    curInstitution.setCity(tfCity.getText());
    curInstitution.setName(tfName.getText());
    curInstitution.setWebLink(tfLink.getText());
    curInstitution.state.setID(hcbState.selectedID());
    curInstitution.country.setID(hcbCountry.selectedID());
    curInstitution.instType.setID(hcbType.selectedID());
    curInstitution.parentInst.setID(hcbParentInst.selectedID());

    numRows = htSubInstitutions.getDataRowCount();
       
    for (ndx = 0; ndx < numRows; ndx++)
    {
      if ((htSubInstitutions.getID(1, ndx) > 0) || (htSubInstitutions.getText(1, ndx).length() > 0) || (htSubInstitutions.getText(3, ndx).length() > 0))
      {
        subInstID = htSubInstitutions.getID(1, ndx);

        if (subInstID < 1)
          subInst = db.createNewBlankRecord(hdtInstitution);
        else
          subInst = db.institutions.getByID(subInstID);

        subInstID = subInst.getID();
        subInst.setName(htSubInstitutions.getText(1, ndx));
        subInst.parentInst.setID(curInstitution.getID());
        subInst.instType.setID(htSubInstitutions.getID(2, ndx));
        subInst.setWebLink(htSubInstitutions.getText(3, ndx));

        if ((subInst.name().length() == 0) &&
            (subInst.getWebLink().length() == 0) &&
            (subInst.persons.isEmpty()))
              db.deleteRecord(hdtInstitution, subInstID);
      }
    }
    
    if (locationChanged == false) return true;
    
    if (hasSubInstWithDifferentLocation(curInstitution, curInstitution))
      if (!confirmDialog("One or more sub-institutions have a different location than the present one. Should they also be updated?"))
        return true;

    curInstitution.overwriteSubInstLocations();
    
    return true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private boolean differentLocation(HDT_Institution instToCheck, HDT_Institution baseInst)
  {
    if (instToCheck.getCity().trim().length() > 0)
      if (instToCheck.getCity().trim().equalsIgnoreCase(baseInst.getCity().trim()) == false)
        return true;
    
    if (instToCheck.state.get() != null)
      if (instToCheck.state.get() != baseInst.state.get())
        return true;
    
    if (instToCheck.country.get() != null)
      if (instToCheck.country.get() != baseInst.country.get())
        return true;
    
    return hasSubInstWithDifferentLocation(instToCheck, baseInst);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private boolean hasSubInstWithDifferentLocation(HDT_Institution instToCheck, HDT_Institution baseInst)
  {
    for (HDT_Institution subInst : instToCheck.subInstitutions)
    {
      if (differentLocation(subInst, baseInst)) 
        return true;
    }
    
    return false;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
}
