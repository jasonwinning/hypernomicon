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

package org.hypernomicon.view.tabs;

import org.hypernomicon.dialogs.NewRegionDlgCtrlr;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_Country;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.prefs.Preferences;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;

//---------------------------------------------------------------------------

public class InstTabCtrlr extends HyperTab<HDT_Institution, HDT_Institution>
{
  private HyperTable htSubInst, htPersons;
  private HyperCB hcbRegion, hcbCountry, hcbType, hcbParentInst;
  private HDT_Institution curInst;

  @FXML private TextField tfCity, tfName, tfURL;
  @FXML private Button btnURL, btnParent, btnNewRegion;
  @FXML private ComboBox<HyperTableCell> cbType, cbParentInst, cbRegion, cbCountry;
  @FXML private TableView<HyperTableRow> tvSubInstitutions, tvPersons;
  @FXML private SplitPane spHoriz;
  @FXML private Hyperlink hlMaps;

  @Override public String recordName()                 { return tfName.getText(); }
  @Override protected RecordType type()                { return hdtInstitution; }
  @Override public void enable(boolean enabled)        { ui.tabInst.getContent().setDisable(enabled == false); }
  @Override public void setRecord(HDT_Institution rec) { curInst = rec; }
  @Override public void setDividerPositions()          { setDividerPosition(spHoriz, PREF_KEY_INST_MID_HORIZ, 0); }
  @Override public void getDividerPositions()          { getDividerPosition(spHoriz, PREF_KEY_INST_MID_HORIZ, 0); }
  @Override public void findWithinDesc(String text)    { messageDialog("Internal error #52009", mtError); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean update()
  {
    Map<HDT_Person, Set<HDT_Institution>> peopleMap = new HashMap<>();

    tfName.setText(curInst.name   ());
    tfCity.setText(curInst.getCity());
    tfURL .setText(curInst.getURL ());

    hcbCountry   .addAndSelectEntryOrBlank(curInst.country    , HDT_Record::name);
    hcbRegion    .addAndSelectEntryOrBlank(curInst.region     , HDT_Record::name);
    hcbType      .addAndSelectEntryOrBlank(curInst.instType   , HDT_Record::name);
    hcbParentInst.addAndSelectEntryOrBlank(curInst.parentInst , HDT_Record::name);

 // Populate departments and people
 // -------------------------------

    addPersonsFromInst(curInst, curInst, peopleMap);

    htSubInst.buildRows(curInst.subInstitutions, (row, subInst) ->
    {
      row.setCellValue(0, subInst, subInst.name());

      if (subInst.instType.isNotNull())
        row.setCellValue(1, subInst.instType.get(), subInst.instType.get().name());

      row.setCellValue(3, subInst, subInst.getURL());
    });

    if (curInst.isDeptOrFaculty() && curInst.parentInst.isNotNull())
    {
      curInst.parentInst.get().subInstitutions.forEach(sibInst ->
      {
        if (sibInst != curInst)
          addSiblingInsts(sibInst, sibInst, peopleMap);
      });
    }

    htPersons.buildRows(peopleMap.keySet(), (row, person) ->
    {
      row.setCellValue(0, person, person.listName());

      if (person.rank.isNotNull())
        row.setCellValue(1, person.rank.get(), person.rank.get().name());

      if (person.field.isNotNull())
        row.setCellValue(2, person.field.get(), person.field.get().name());

      List<HDT_Institution> instList = new ArrayList<>(peopleMap.get(person));
      instList.sort(sortBasis(HDT_Record::name));

      String instStr = instList.stream().map(HDT_Institution::name).reduce((name1, name2) -> name1 + ", " + name2).orElse("");
      int instID = instList.isEmpty() ? -1 : instList.get(0).getID();

      row.setCellValue(3, instID, instStr, hdtInstitution);
    });

    htPersons.getTV().getSortOrder().setAll(List.of(htPersons.getTV().getColumns().get(0)));

    safeFocus(tfName);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addSiblingInsts(HDT_Institution sibInst, HDT_Institution cousinInst, Map<HDT_Person, Set<HDT_Institution>> peopleMap)
  {
    cousinInst.subInstitutions.forEach(subInst -> addSiblingInsts(sibInst, subInst, peopleMap));

    cousinInst.persons.forEach(person ->
    {
      if (peopleMap.containsKey(person))
        peopleMap.get(person).add(sibInst);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addPersonsFromInst(HDT_Institution nearestChildInst, HDT_Institution inst, Map<HDT_Person, Set<HDT_Institution>> peopleMap)
  {
    inst.subInstitutions.forEach(nearestChildInst == curInst ?
      subInst -> addPersonsFromInst(subInst, subInst, peopleMap)
    :
      subInst -> addPersonsFromInst(nearestChildInst, subInst, peopleMap));

    inst.persons.forEach(person ->
    {
      if (peopleMap.containsKey(person) == false)
        peopleMap.put(person, new HashSet<HDT_Institution>());

      peopleMap.get(person).add(nearestChildInst);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void init()
  {
    htSubInst = new HyperTable(tvSubInstitutions, 0, true, PREF_KEY_HT_INST_SUB);

    htSubInst.addTextEditCol(hdtInstitution, true, false);
    htSubInst.addCol(hdtInstitutionType, ctDropDownList);

    htSubInst.addActionColWithButtonHandler(ctUrlBtn, 3, (row, colNdx) ->
    {
      String link = row.getText(colNdx);

      if (link.isBlank())
      {
        ui.webButtonMap.get(PREF_KEY_INST_SRCH).first(WebButtonField.Name, tfName.getText())
                                               .next (WebButtonField.DivisionName, row.getText(0))
                                               .go();
      }
      else
        DesktopUtil.openWebLink(row.getText(colNdx));
    });

    htSubInst.addTextEditCol(hdtInstitution, true, false);

    htSubInst.addContextMenuItem("Go to this record", HDT_Institution.class, inst -> ui.goToRecord(inst, true));

    htSubInst.addContextMenuItem("Delete this institution record", HDT_Institution.class, inst ->
    {
      if (ui.cantSaveRecord()) return;
      if (confirmDialog("Are you sure you want to delete this record?") == false) return;
      db.deleteRecord(inst);
      ui.update();
    });

    htSubInst.addChangeOrderMenuItem(true, () -> curInst.subInstitutions.reorder(htSubInst.saveToList(0, hdtInstitution), true));

    htPersons = new HyperTable(tvPersons, 0, false, PREF_KEY_HT_INST_PEOPLE);

    htPersons.addCol(hdtPerson     , ctNone);
    htPersons.addCol(hdtRank       , ctNone);
    htPersons.addCol(hdtField      , ctNone);
    htPersons.addCol(hdtInstitution, ctNone);

    hcbCountry = new HyperCB(cbCountry, ctDropDownList, new StandardPopulator(hdtCountry), true);
    hcbRegion = new HyperCB(cbRegion, ctDropDownList, new SubjectPopulator(rtCountryOfRegion, false), true);

    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtInstitutionType), true);
    hcbParentInst = new HyperCB(cbParentInst, ctDropDownList, new StandardPopulator(hdtInstitution), true);

    hcbCountry.addListener((oldValue, newValue) ->
    {
      if (newValue == null) return;

      if (getCellID(oldValue) != getCellID(newValue))
      {
        ((SubjectPopulator)hcbRegion.getPopulator()).setObj(null, getRecord(newValue));
        if (getCellID(oldValue) > 0)
          hcbRegion.selectID(-1);
      }
    });

    btnNewRegion.setOnAction(event ->
    {
      HDT_Country country = hcbCountry.selectedRecord();

      if (country == null)
      {
        messageDialog("Select a country.", mtWarning);
        safeFocus(cbCountry);
        return;
      }

      NewRegionDlgCtrlr nrdc = NewRegionDlgCtrlr.build(country);
      if (nrdc.showModal() == false) return;

      hcbRegion.populate(true);
      hcbRegion.selectID(nrdc.getRegion().getID());
    });

    btnParent.setOnAction(event -> ui.goToRecord(getRecord(hcbParentInst.selectedHTC()), true));

    btnURL.setOnAction(event ->
    {
      if (tfURL.getText().isBlank() == false)
        DesktopUtil.openWebLink(tfURL.getText());
      else
        ui.webButtonMap.get(PREF_KEY_INST_SRCH).first(WebButtonField.Name, tfName.getText()).go();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void linkClick()
  {
    ui.webButtonMap.get(PREF_KEY_INST_MAP_SRCH).first(WebButtonField.Name, tfName.getText())
                                               .next (WebButtonField.City, tfCity.getText())
                                               .next (WebButtonField.Region, hcbRegion.getText())
                                               .next (WebButtonField.Country, hcbCountry.getText())
                                               .go();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    tfName.clear();
    tfCity.clear();
    tfURL .clear();

    hcbRegion    .clear();
    hcbCountry   .clear();
    hcbParentInst.clear();
    hcbType      .clear();

    htSubInst.clear();
    htPersons.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord()
  {
    if (hcbType.selectedID() < 1)
      return falseWithErrorMessage("You must select a type.", cbType);

    boolean locationChanged = false;

    if (tfCity.getText().trim().length() > 0)
      if (curInst.getCity().trim().equalsIgnoreCase(tfCity.getText().trim()) == false)
        locationChanged = true;

    if (hcbRegion.selectedRecord() != null)
      if (curInst.region.get() != hcbRegion.selectedRecord())
        locationChanged = true;

    if (hcbCountry.selectedRecord() != null)
      if (curInst.country.get() != hcbCountry.selectedRecord())
        locationChanged = true;

    curInst.setCity(tfCity.getText());
    curInst.setName(tfName.getText());
    curInst.setURL(tfURL.getText());
    curInst.region.setID(hcbRegion.selectedID());
    curInst.country.setID(hcbCountry.selectedID());
    curInst.instType.setID(hcbType.selectedID());
    curInst.parentInst.setID(hcbParentInst.selectedID());

    htSubInst.getDataRows().forEach(row ->
    {
      int subInstID = row.getID(0);

      if ((subInstID > 0) || (row.getText(0).length() > 0) || (row.getText(3).length() > 0))
      {
        HDT_Institution subInst = subInstID < 1 ? db.createNewBlankRecord(hdtInstitution) : db.institutions.getByID(subInstID);

        subInstID = subInst.getID();
        subInst.setName(row.getText(0));
        subInst.parentInst.setID(curInst.getID());
        subInst.instType.setID(row.getID(1));
        subInst.setURL(row.getText(3));

        if (subInst.name()  .isEmpty() &&
            subInst.getURL().isEmpty() &&
            subInst.persons .isEmpty())
          db.deleteRecord(subInst);
      }
    });

    if (locationChanged == false) return true;

    if (hasSubInstWithDifferentLocation(curInst, curInst))
      if (!confirmDialog("One or more sub-institutions have a different location than the present one. Should they also be updated?"))
        return true;

    curInst.overwriteSubInstLocations();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean differentLocation(HDT_Institution instToCheck, HDT_Institution baseInst)
  {
    String city = instToCheck.getCity().trim();

    if ((city.length() > 0) && (city.equalsIgnoreCase(baseInst.getCity().trim()) == false))
      return true;

    HDT_Region region = instToCheck.region.get();

    if ((region != null) && (region != baseInst.region.get()))
      return true;

    HDT_Country country = instToCheck.country.get();

    return (country != null) && (country != baseInst.country.get()) ?
      true
    :
      hasSubInstWithDifferentLocation(instToCheck, baseInst);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean hasSubInstWithDifferentLocation(HDT_Institution instToCheck, HDT_Institution baseInst)
  {
    return instToCheck.subInstitutions.stream().anyMatch(subInst -> differentLocation(subInst, baseInst));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void updateWebButtons(Preferences node)
  {
    hlMaps.setText(ui.webButtonMap.get(PREF_KEY_INST_MAP_SRCH).getCaption());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
