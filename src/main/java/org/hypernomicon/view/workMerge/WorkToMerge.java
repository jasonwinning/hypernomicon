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

package org.hypernomicon.view.workMerge;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.dialogs.WorkDlgCtrlr.createAuthorRecordHandler;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.view.dialogs.WorkDlgCtrlr;
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

public class WorkToMerge
{
  private final BibData bibData;
  private final HyperCB hcbType;
  private final HyperTable htAuthors;
  private final boolean creatingNewWork;

  public List<ObjectGroup> getAuthorGroups(HDT_Work work) { return htAuthors.getAuthorGroups(work, 0, -1, 2, 3); }
  public HDT_WorkType getWorkType()                       { return hcbType.selectedRecord(); }
  public BibData getBibData()                             { return bibData; }
  public boolean hasField(BibFieldEnum bibFieldEnum)      { return bibData == null ? false : bibData.fieldNotEmpty(bibFieldEnum); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public WorkToMerge(BibData bibData, RadioButton rbTitle, TextField tfTitle, RadioButton rbType, ComboBox<HyperTableCell> cbType,
                                      RadioButton rbYear, TextField tfYear, RadioButton rbAuthors, TableView<HyperTableRow> tvAuthors,
                                      HDT_Work destWork, boolean creatingNewWork)
  {
    this.bibData = bibData;
    this.creatingNewWork = creatingNewWork;

    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtWorkType), null);

    htAuthors = new HyperTable(tvAuthors, 0, true, "");

    htAuthors.addCol(hdtPerson, ctDropDownList);

    HDT_Work workRecord = nullSwitch(bibData.getWork(), destWork, work -> work);

    htAuthors.addCheckboxColWithUpdateHandler(createAuthorRecordHandler(htAuthors, () -> workRecord));

    htAuthors.addCheckboxCol();
    htAuthors.addCheckboxCol();

    tfTitle.setText(bibData.getStr(bfTitle));
    if (tfTitle.getText().isEmpty() == false) rbTitle.setSelected(true);

    tfYear.setText(bibData.getStr(bfYear));
    if (tfYear.getText().isEmpty() == false) rbYear.setSelected(true);

    if (bibData.getWork() != null)
      loadFromWork(bibData.getWork(), rbType);
    else
      loadFromBibData(rbType, destWork);

    if (htAuthors.getDataRowCount() > 0)
      rbAuthors.setSelected(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadFromWork(HDT_Work workRecord, RadioButton rbType)
  {
    if (creatingNewWork)
    {
      hcbType.addAndSelectEntryOrBlank(workRecord.workType, HDT_Record::name);
      rbType.setSelected(true);
    }

    htAuthors.buildRows(workRecord.getAuthors(), (row, author) ->
    {
      HDT_Person authorRecord = author.getPerson();

      if (authorRecord == null)
      {
        Populator pop = htAuthors.getPopulator(0);
        pop.populate(null, false);
        pop.addEntry(null, -1, author.getNameLastFirst());
        row.setCellValue(0, -1, author.getNameLastFirst(), hdtPerson);
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
      hcbType.addAndSelectEntry(workType, HDT_Record::name);
      rbType.setSelected(true);
    });

    htAuthors.getPopulator(0).populate(null, false);

    WorkDlgCtrlr.loadFromBibAuthors(bibData.getAuthors(), htAuthors, false, destWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}