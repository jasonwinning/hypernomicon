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

package org.hypernomicon.dialogs.workMerge;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.dialogs.WorkDlgCtrlr.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.scene.control.ComboBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;

class WorkToMerge
{
  private final BibData bibData;
  private final HyperCB hcbType;
  private final HyperTable htAuthors;
  private final boolean creatingNewWork;

  public List<ObjectGroup> getAuthorGroups(HDT_Work work) { return htAuthors.getAuthorGroups(work, 0, -1, 2, 3); }
  public HDT_WorkType getWorkType()                       { return hcbType.selectedRecord(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  WorkToMerge(BibData bibData, RadioButton rbTitle, TextField tfTitle, RadioButton rbType, ComboBox<HyperTableCell> cbType,
              RadioButton rbYear, TextField tfYear, RadioButton rbAuthors, TableView<HyperTableRow> tvAuthors,
              HDT_Work destWork, boolean creatingNewWork, MutableBoolean alreadyChangingTitle)
  {
    this.bibData = bibData;
    this.creatingNewWork = creatingNewWork;

    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtWorkType));

    htAuthors = new HyperTable(tvAuthors, 0, true, "");

    htAuthors.initConstrainedResize();

    htAuthors.addCol(hdtPerson, ctDropDownList);

    HDT_Work workRecord = nullSwitch(bibData.getWork(), destWork);

    htAuthors.addCheckboxColWithUpdateHandler(createAuthorRecordHandler(htAuthors, () -> workRecord));

    htAuthors.addCheckboxCol();
    htAuthors.addCheckboxCol();

    htAuthors.addRemoveMenuItem();
    htAuthors.addChangeOrderMenuItem(true);

    htAuthors.addContextMenuItem("Remove this row",
      row -> (row.getText(1).length() > 0) && (row.getID(1) < 1),
      htAuthors::removeRow);

    tfTitle.setText(bibData.getStr(bfTitle));
    if (tfTitle.getText().isEmpty() == false) rbTitle.setSelected(true);

    tfTitle.setTextFormatter(titleFormatter(alreadyChangingTitle, rbTitle));

    tfYear.setText(bibData.getStr(bfYear));
    if (tfYear.getText().isEmpty() == false) rbYear.setSelected(true);

    tfYear.textProperty().addListener((obs, ov, nv) -> rbYear.setSelected(true));

    if (bibData.getWork() != null)
      loadFromWork(bibData.getWork(), rbType);
    else
      loadFromBibData(rbType, destWork);

    if (htAuthors.dataRowCount() > 0)
      rbAuthors.setSelected(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadFromWork(HDT_Work workRecord, RadioButton rbType)
  {
    if (creatingNewWork) // Work type row is removed when creatingNewWork is false
    {
      hcbType.selectIDofRecord(workRecord.workType);

      if (workRecord.workType.isNotNull())
        rbType.setSelected(true);
    }

    htAuthors.buildRows(workRecord.getAuthors(), (row, author) ->
    {
      HDT_Person authorRecord = author.getPerson();

      if (authorRecord == null)
      {
        Populator pop = htAuthors.getPopulator(0);
        pop.populate(false);
        pop.addEntry(author.getNameLastFirst());
        row.setCellValue(0, author.getNameLastFirst(), hdtPerson);
      }
      else
      {
        row.setCellValue(0, authorRecord, authorRecord.listName());
        row.setCheckboxValue(1, true);
      }

      row.setCheckboxValue(2, author.getIsEditor());
      row.setCheckboxValue(3, author.getIsTrans());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadFromBibData(RadioButton rbType, HDT_Work destWork)
  {
    nullSwitch(bibData.getWorkType(), workType ->
    {
      hcbType.selectIDofRecord(workType);
      rbType.setSelected(true);
    });

    htAuthors.getPopulator(0).populate(false);

    loadFromBibAuthors(bibData.getAuthors(), htAuthors, false, destWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
