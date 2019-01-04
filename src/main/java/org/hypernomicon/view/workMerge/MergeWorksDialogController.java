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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.bib.BibData.BibFieldEnum.*;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;

import static java.util.Objects.*;

import java.io.IOException;

import org.hypernomicon.bib.BibData;
import org.hypernomicon.bib.BibData.BibFieldEnum;
import org.hypernomicon.bib.BibData.EntryType;
import org.hypernomicon.bib.BibField;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.view.dialogs.HyperDialog;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.RowConstraints;
import javafx.stage.Modality;
import javafx.stage.StageStyle;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class MergeWorksDialogController extends HyperDialog
{
  @FXML private RadioButton rbTitle1;
  @FXML private RadioButton rbTitle2;
  @FXML private RadioButton rbTitle3;
  @FXML private RadioButton rbTitle4;

  @FXML private TextField tfTitle1;
  @FXML private TextField tfTitle2;
  @FXML private TextField tfTitle3;
  @FXML private TextField tfTitle4;

  @FXML private RadioButton rbType1;
  @FXML private RadioButton rbType2;
  @FXML private RadioButton rbType3;
  @FXML private RadioButton rbType4;

  @FXML private ComboBox<HyperTableCell> cbType1;
  @FXML private ComboBox<HyperTableCell> cbType2;
  @FXML private ComboBox<HyperTableCell> cbType3;
  @FXML private ComboBox<HyperTableCell> cbType4;

  @FXML private RadioButton rbYear1;
  @FXML private RadioButton rbYear2;
  @FXML private RadioButton rbYear3;
  @FXML private RadioButton rbYear4;

  @FXML private TextField tfYear1;
  @FXML private TextField tfYear2;
  @FXML private TextField tfYear3;
  @FXML private TextField tfYear4;

  @FXML private RadioButton rbAuthors1;
  @FXML private RadioButton rbAuthors2;
  @FXML private RadioButton rbAuthors3;
  @FXML private RadioButton rbAuthors4;

  @FXML private TableView<HyperTableRow> tvAuthors1;
  @FXML private TableView<HyperTableRow> tvAuthors2;
  @FXML private TableView<HyperTableRow> tvAuthors3;
  @FXML private TableView<HyperTableRow> tvAuthors4;

  @FXML private GridPane gpMain;
  @FXML private GridPane gpTitle;
  @FXML private GridPane gpType;
  @FXML private GridPane gpYear;
  @FXML private GridPane gpAuthors;

  private final EnumMap<BibFieldEnum, BibField> singleFields = new EnumMap<>(BibFieldEnum.class);
  private final ArrayList<WorkToMerge> works = new ArrayList<>(4);
  private final HashMap<BibFieldEnum, BibFieldRow> extraRows = new HashMap<>();
  private int nextRowNdx = 4;
  private boolean creatingNewWork, creatingNewEntry;

  public EntryType getEntryType() { return nullSwitch(extraRows.get(bfEntryType), null, row -> MergeWorksCBController.class.cast(row).getEntryType()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MergeWorksDialogController create(String title, BibData bd1, BibData bd2, BibData bd3, BibData bd4,
                                                  HDT_Work destWork, boolean creatingNewWork, boolean creatingNewEntry) throws IOException
  {
    MergeWorksDialogController mwd = HyperDialog.createUsingFullPath("view/workMerge/MergeWorksDialog.fxml", title, true, StageStyle.UTILITY, Modality.APPLICATION_MODAL);
    mwd.init(bd1, bd2, bd3, bd4, destWork, creatingNewWork, creatingNewEntry);
    return mwd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(BibData bd1, BibData bd2, BibData bd3, BibData bd4, HDT_Work destWork, boolean creatingNewWork, boolean creatingNewEntry) throws IOException
  {
    this.creatingNewWork = creatingNewWork;
    this.creatingNewEntry = creatingNewEntry;

    if (creatingNewWork == false)
    {
      deleteGridPaneRow(gpMain, 1);
      nextRowNdx--;
    }

    ArrayList<BibData> bdList = new ArrayList<>();

    if (bd1 != null) bdList.add(bd1);
    if (bd2 != null) bdList.add(bd2);
    if (bd3 != null) bdList.add(bd3);
    if (bd4 != null) bdList.add(bd4);

    while (bdList.size() < 4) bdList.add(null);

    bd1 = bdList.get(0);
    bd2 = bdList.get(1);
    bd3 = bdList.get(2);
    bd4 = bdList.get(3);

    if (nonNull(bd4))
      works.add(0, new WorkToMerge(bd4, rbTitle4, tfTitle4, rbType4, cbType4, rbYear4, tfYear4, rbAuthors4, tvAuthors4, destWork, creatingNewWork));

    if (nonNull(bd3))
      works.add(0, new WorkToMerge(bd3, rbTitle3, tfTitle3, rbType3, cbType3, rbYear3, tfYear3, rbAuthors3, tvAuthors3, destWork, creatingNewWork));

    works.add(0, new WorkToMerge(bd2, rbTitle2, tfTitle2, rbType2, cbType2, rbYear2, tfYear2, rbAuthors2, tvAuthors2, destWork, creatingNewWork));
    works.add(0, new WorkToMerge(bd1, rbTitle1, tfTitle1, rbType1, cbType1, rbYear1, tfYear1, rbAuthors1, tvAuthors1, destWork, creatingNewWork));

    if (isNull(bd4))
    {
      deleteGridPaneRow(gpTitle, 4);
      deleteGridPaneColumn(gpType, 3);
      deleteGridPaneColumn(gpYear, 3);
      deleteGridPaneColumn(gpAuthors, 3);
    }

    if (isNull(bd3))
    {
      deleteGridPaneRow(gpTitle, 3);
      deleteGridPaneColumn(gpType, 2);
      deleteGridPaneColumn(gpYear, 2);
      deleteGridPaneColumn(gpAuthors, 2);
    }

    int cnt = 0;

    if (creatingNewEntry)
      addField(bfEntryType, bd1, bd2, bd3, bd4);

    for (BibFieldEnum bibFieldEnum : BibFieldEnum.values())
    {
      cnt = 0;
      BibData singleBD = null;

      switch (bibFieldEnum)
      {
        case bfAuthors : case bfEditors : case bfTranslators : case bfTitle : case bfYear : case bfWorkType : case bfEntryType :
          continue;

        default : break;
      }

      boolean fieldsEqual = true;

      if (bd1.fieldNotEmpty(bibFieldEnum))
      {
        singleBD = bd1;
        cnt++;
      }

      if (bd2.fieldNotEmpty(bibFieldEnum))
      {
        if (singleBD != null)
        {
          if (singleBD.fieldsAreEqual(bibFieldEnum, bd2) == false)
            fieldsEqual = false;
        }
        else
          singleBD = bd2;

        cnt++;
      }

      if ((bd3 != null) && (bd3.fieldNotEmpty(bibFieldEnum)))
      {
        if (singleBD != null)
        {
          if (singleBD.fieldsAreEqual(bibFieldEnum, bd3) == false)
            fieldsEqual = false;
        }
        else
          singleBD = bd3;

        cnt++;
      }

      if ((bd4 != null) && (bd4.fieldNotEmpty(bibFieldEnum)))
      {
        if (singleBD != null)
        {
          if (singleBD.fieldsAreEqual(bibFieldEnum, bd4) == false)
            fieldsEqual = false;
        }
        else
          singleBD = bd4;

        cnt++;
      }

      if ((bibFieldEnum == bfMisc) || ((cnt > 0) && ((fieldsEqual == false) || (cnt < works.size()))))
        addField(bibFieldEnum, bd1, bd2, bd3, bd4);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addField(BibFieldEnum bibFieldEnum, BibData bd1, BibData bd2, BibData bd3, BibData bd4) throws IOException
  {
    BibFieldRow row = BibFieldRow.create(bibFieldEnum, bd1, bd2, bd3, bd4);
    extraRows.put(bibFieldEnum, row);
    AnchorPane ap = row.getAnchorPane();

    GridPane.setRowIndex(ap, nextRowNdx);

    nextRowNdx++;

    gpMain.getChildren().add(ap);

    RowConstraints rc;

    if (BibData.bibFieldIsMultiLine(bibFieldEnum))
      rc = new RowConstraints(10.0, 150.0, Region.USE_COMPUTED_SIZE);
    else
    {
      rc = new RowConstraints(45.0, 45.0, 45.0);
      rc.setVgrow(Priority.NEVER);
    }

    gpMain.getRowConstraints().add(rc);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (creatingNewWork)
    {
      HDT_WorkType workType = getMergedWorkType();

      if (workType == null)
      {
        messageDialog("Select a work type.", mtWarning);
        return false;
      }
    }

    if (creatingNewEntry)
    {
      EntryType entryType = getEntryType();

      if (entryType != null)
      {
        switch (entryType)
        {
          case etUnentered : case etOther : case etNone : entryType = null; break;
          default: break;
        }
      }

      if (entryType == null)
      {
        messageDialog("Select an entry type.", mtWarning);
        return false;
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_WorkType getMergedWorkType()
  {
    if      (rbType1.isSelected()) return works.get(0).getWorkType();
    else if (rbType2.isSelected()) return works.get(1).getWorkType();
    else if (rbType3.isSelected()) return works.get(2).getWorkType();
    else                           return works.get(3).getWorkType();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void mergeInto(BibData mergedBD)
  {
    String title;
    HDT_Work work = mergedBD.getWork();
    HDT_WorkType workType = getMergedWorkType();

    if      (rbTitle1.isSelected()) title = tfTitle1.getText();
    else if (rbTitle2.isSelected()) title = tfTitle2.getText();
    else if (rbTitle3.isSelected()) title = tfTitle3.getText();
    else                            title = tfTitle4.getText();

    String year;

    if      (rbYear1.isSelected()) year = tfYear1.getText();
    else if (rbYear2.isSelected()) year = tfYear2.getText();
    else if (rbYear3.isSelected()) year = tfYear3.getText();
    else                           year = tfYear4.getText();

    List<ObjectGroup> authGroups;

    if      (rbAuthors1.isSelected()) authGroups = works.get(0).getAuthorGroups(work);
    else if (rbAuthors2.isSelected()) authGroups = works.get(1).getAuthorGroups(work);
    else if (rbAuthors3.isSelected()) authGroups = works.get(2).getAuthorGroups(work);
    else                              authGroups = works.get(3).getAuthorGroups(work);

    mergedBD.setTitle(title);
    mergedBD.setStr(bfYear, year);

    if (work != null)
    {
      if (creatingNewWork)
        work.setWorkType(workType.getEnumVal());

      work.setAuthors(authGroups);
    }
    else
    {
      mergedBD.getAuthors().setAllFromTable(authGroups);
    }

    for (BibFieldEnum bibFieldEnum : BibFieldEnum.values())
    {
      switch (bibFieldEnum)
      {
        case bfAuthors : case bfEditors : case bfTranslators : case bfTitle : case bfYear :
          continue;

        default : break;
      }

      BibFieldRow bibFieldRow = extraRows.get(bibFieldEnum);

      if (bibFieldRow != null)
        bibFieldRow.mergeInto(mergedBD); // assign data from bibFieldRow to work and bd
      else
      {
        // assign data from original work/bd that had this field, if any (must have been zero or one of them)

        BibField field = singleFields.get(bibFieldEnum);

        if (field != null)
        {
          if ((bibFieldEnum == bfEntryType) || (bibFieldEnum == bfWorkType))
            noOp();
          else if (field.isMultiStr())
            mergedBD.setMultiStr(bibFieldEnum, field.getMultiStr());
          else
            mergedBD.setStr(bibFieldEnum, field.getStr());
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
