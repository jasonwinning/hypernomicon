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

package org.hypernomicon.view.tabs;

import org.hypernomicon.dialogs.NewRegionDlgCtrlr;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_Country;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.WebButton;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.Populator.DisplayKind;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.wrappers.ButtonCell;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import com.google.common.collect.Ordering;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_Institution.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.io.IOException;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.prefs.Preferences;
import java.util.stream.Collectors;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.SplitPane;
import javafx.scene.control.Tab;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;

//---------------------------------------------------------------------------

public class InstTabCtrlr extends HyperTab<HDT_Institution, HDT_Institution>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TextField tfCity, tfName, tfURL;
  @FXML private Button btnURL, btnParent, btnNewRegion, btnNewCountry;
  @FXML private ComboBox<HyperTableCell> cbType, cbParentInst, cbRegion, cbCountry;
  @FXML private TableView<HyperTableRow> tvSubInstitutions, tvPersons;
  @FXML private SplitPane spHoriz;
  @FXML private Hyperlink hlMaps;

  private final HyperTable htSubInst, htPersons;
  private final HyperCB hcbRegion, hcbCountry, hcbType, hcbParentInst;

  private HDT_Institution curInst;

//---------------------------------------------------------------------------

  public InstTabCtrlr(Tab tab) throws IOException
  {
    super(instTabEnum, tab, "view/tabs/InstTab");

    htSubInst = new HyperTable(tvSubInstitutions, 0, true, TablePrefKey.INST_SUB);

    htSubInst.addTextEditCol(hdtInstitution, true);
    htSubInst.addCol(hdtInstitutionType, ctEditableLimitedDropDown);

    htSubInst.addActionColWithButtonHandler(ctUrlBtn, 3, (row, colNdx) ->
    {
      String link = row.getText(colNdx);

      if (link.isBlank())
      {
        ui.webButtonMap.get(WebButtonContextPrefKey.INST).first(WebButtonField.Name, tfName.getText())
                                                         .next (WebButtonField.DivisionName, row.getText(0))
                                                         .go();
      }
      else
        DesktopUtil.openWebLink(row.getText(colNdx));
    });

    htSubInst.addTextEditCol(hdtInstitution, true, smTextSimple);

    htSubInst.addContextMenuItem("Go to this record", HDT_Institution.class, inst -> ui.goToRecord(inst, true));

    htSubInst.addContextMenuItem("Delete this institution record", HDT_Institution.class, inst ->
    {
      if (ui.cantSaveRecord()) return;
      if (confirmDialog("Are you sure you want to delete this record?", false) == false) return;
      db.deleteRecord(inst);
      ui.update();
    });

    htSubInst.addChangeOrderMenuItem(true, () -> curInst.subInstitutions.reorder(htSubInst.saveToList(0, hdtInstitution), true));

    htPersons = new HyperTable(tvPersons, 0, false, TablePrefKey.INST_PEOPLE);

    htPersons.addLabelCol(hdtPerson             );
    htPersons.addLabelCol(hdtRank               );
    htPersons.addLabelCol(hdtField, smTextSimple);
    htPersons.addLabelCol(hdtInstitution        );
    htPersons.addLabelCol(hdtPerson             );

    hcbCountry = new HyperCB(cbCountry, ctEditableLimitedDropDown, new StandardPopulator(hdtCountry), true);
    hcbRegion  = new HyperCB(cbRegion , ctEditableLimitedDropDown, new SubjectPopulator(rtCountryOfRegion, false), true);

    hcbType       = new HyperCB(cbType      , ctEditableLimitedDropDown, new StandardPopulator(hdtInstitutionType), true);
    hcbParentInst = new HyperCB(cbParentInst, ctEditableLimitedDropDown, new StandardPopulator(hdtInstitution, parentPopFilter, DisplayKind.name), true);

    hcbCountry.addListener((oldValue, newValue) ->
    {
      if (newValue == null) return;

      if (getCellID(oldValue) != getCellID(newValue))
      {
        ((SubjectPopulator)hcbRegion.getPopulator()).setObj(getRecord(newValue));
        if (getCellID(oldValue) > 0)
          hcbRegion.selectID(-1);
      }
    });

    btnNewCountry.setOnAction(event ->
    {
      HDT_Country country = ui.mnuNewCategoryClick(hdtCountry, false, false);
      if (country == null) return;

      hcbCountry.populate(true);
      hcbCountry.selectIDofRecord(country);
    });

    setToolTip(btnNewCountry, "Add new Country record to the list");

    btnNewRegion.setOnAction(event ->
    {
      HDT_Country country = hcbCountry.selectedRecord();

      if (country == null)
      {
        warningPopup("Select a country.");
        safeFocus(cbCountry);
        return;
      }

      NewRegionDlgCtrlr nrdc = new NewRegionDlgCtrlr(country);
      if (nrdc.showModal() == false) return;

      hcbRegion.populate(true);
      hcbRegion.selectIDofRecord(nrdc.getRegion());
    });

    setToolTip(btnNewRegion, "Add new State/Region record for the currently selected Country");

    btnParent.setOnAction(event -> ui.goToRecord(getRecord(hcbParentInst.selectedHTC()), true));

    setToolTip(btnParent, "Go to parent record");

    btnURL.setOnAction(event ->
    {
      if (tfURL.getText().isBlank() == false)
        DesktopUtil.openWebLink(tfURL.getText());
      else
        ui.webButtonMap.get(WebButtonContextPrefKey.INST).first(WebButtonField.Name, tfName.getText()).go();
    });

    setToolTip(btnURL, ButtonCell.URL_BUTTON_TOOLTIP);
  }

//---------------------------------------------------------------------------

  @Override protected RecordType type()                   { return hdtInstitution; }
  @Override protected void setRecord(HDT_Institution rec) { curInst = rec; }

  @Override public String recordName()                    { return tfName.getText(); }
  @Override public void setDividerPositions()             { setDividerPosition(spHoriz, DividerPositionPrefKey.INST_MID_HORIZ, 0); }
  @Override public void getDividerPositions()             { getDividerPosition(spHoriz, DividerPositionPrefKey.INST_MID_HORIZ, 0); }
  @Override public void findWithinDesc()                  { internalErrorPopup(52009); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateFromRecord()
  {
    Map<HDT_Person, Set<HDT_Institution>> peopleMap = new HashMap<>();

    tfName.setText(curInst.name   ());
    tfCity.setText(curInst.getCity());
    tfURL .setText(curInst.getURL ());

    hcbCountry   .selectIDofRecord(curInst.country);
    hcbRegion    .selectIDofRecord(curInst.region);
    hcbType      .selectIDofRecord(curInst.instType);
    hcbParentInst.selectIDofRecord(curInst.parentInst);

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

    if (((curInst.instType.getID() == FACULTY_INST_TYPE_ID) || (curInst.instType.getID() == DEPARTMENT_INST_TYPE_ID)) && curInst.parentInst.isNotNull())
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

      List<HDT_Institution> instList = Ordering.from(Comparator.comparing(HDT_Record::name)).immutableSortedCopy(peopleMap.get(person));

      String instStr = instList.stream().map(HDT_Institution::name).collect(Collectors.joining(", "));
      int instID = instList.isEmpty() ? -1 : instList.get(0).getID();

      row.setCellValue(3, instID, instStr, hdtInstitution);

      row.setCellValue(4, instList.stream().noneMatch(Predicate.not(person::instIsPast)) ? "Past" : "", hdtPerson);
    });

    // Always sort persons table by person name on tab update

    htPersons.sortAscending(0);

    safeFocus(tfName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addSiblingInsts(HDT_Institution sibInst, HDT_Institution cousinInst, Map<HDT_Person, Set<HDT_Institution>> peopleMap)
  {
    cousinInst.subInstitutions.forEach(subInst -> addSiblingInsts(sibInst, subInst, peopleMap));

    cousinInst.persons.stream().filter(peopleMap::containsKey).forEach(person -> peopleMap.get(person).add(sibInst));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addPersonsFromInst(HDT_Institution nearestChildInst, HDT_Institution inst, Map<HDT_Person, Set<HDT_Institution>> peopleMap)
  {
    inst.subInstitutions.forEach(nearestChildInst == curInst ?
      subInst -> addPersonsFromInst(subInst, subInst, peopleMap)
    :
      subInst -> addPersonsFromInst(nearestChildInst, subInst, peopleMap));

    inst.persons.forEach(person -> peopleMap.computeIfAbsent(person, _person -> new HashSet<>()).add(nearestChildInst));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final Predicate<Integer> parentPopFilter = id ->
  {
    HDT_Institution inst = db.institutions.getByID(id);
    int typeID = inst.instType.getID();

    if ((typeID != FACULTY_INST_TYPE_ID   )  &&
        (typeID != DEPARTMENT_INST_TYPE_ID)  &&
        (typeID != SCHOOL_INST_TYPE_ID    ))
          return true;

    return inst.parentInst.isNull() || (inst.subInstitutions.size() > 0);
  };

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void linkClick()
  {
    ui.webButtonMap.get(WebButtonContextPrefKey.INST_MAP).first(WebButtonField.Name, tfName.getText())
                                                         .next (WebButtonField.City, tfCity.getText())
                                                         .next (WebButtonField.Region, hcbRegion.getText())
                                                         .next (WebButtonField.Country, hcbCountry.getText())
                                                         .go();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    curInst = resetRecord ? null : HDT_Record.getCurrentInstance(curInst);

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

  @Override public boolean saveToRecord(boolean saveNameIfBlank)
  {
    if (hcbType.selectedID() < 1)
      return falseWithErrorPopup("You must select a type.", cbType);

    if (saveNameIfBlank && (nameCheck(tfName, "institution name") == false))
      return false;

    boolean locationChanged = false;

    if (tfCity.getText().strip().length() > 0)
      if (curInst.getCity().strip().equalsIgnoreCase(tfCity.getText().strip()) == false)
        locationChanged = true;

    if (hcbRegion.selectedRecord() != null)
      if (curInst.region.get() != hcbRegion.selectedRecord())
        locationChanged = true;

    if (hcbCountry.selectedRecord() != null)
      if (curInst.country.get() != hcbCountry.selectedRecord())
        locationChanged = true;

    curInst.setCity(tfCity.getText());

    if (saveNameIfBlank || (tfName.getText().isBlank() == false))
    curInst.setName(tfName.getText());

    curInst.setURL(tfURL.getText());
    curInst.region.setID(hcbRegion.selectedID());
    curInst.country.setID(hcbCountry.selectedID());
    curInst.instType.setID(hcbType.selectedID());
    curInst.parentInst.setID(hcbParentInst.selectedID());

    htSubInst.dataRows().forEach(row ->
    {
      int subInstID = row.getID(0);

      if ((subInstID > 0) || (row.getText(0).length() > 0) || (row.getText(3).length() > 0))
      {
        HDT_Institution subInst = subInstID < 1 ? db.createNewBlankRecord(hdtInstitution) : db.institutions.getByID(subInstID);

        subInst.setName(row.getText(0));
        subInst.parentInst.set(curInst);
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
      if (confirmDialog("One or more sub-institutions have a different location than the present one. Should they also be updated?", true))
        curInst.overwriteSubInstLocations();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean differentLocation(HDT_Institution instToCheck, HDT_Institution baseInst)
  {
    String city = instToCheck.getCity().strip();

    if ((city.length() > 0) && (city.equalsIgnoreCase(baseInst.getCity().strip()) == false))
      return true;

    HDT_Region region = instToCheck.region.get();

    if ((region != null) && (region != baseInst.region.get()))
      return true;

    HDT_Country country = instToCheck.country.get();

    return ((country != null) && (country != baseInst.country.get())) || hasSubInstWithDifferentLocation(instToCheck, baseInst);
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
    WebButton mapButton = ui.webButtonMap.get(WebButtonContextPrefKey.INST_MAP);

    hlMaps.setText(mapButton.getCaption());

    setToolTip(hlMaps, "Show location of institution in " + mapButton.getName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
