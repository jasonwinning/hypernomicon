/*
 * Copyright 2015-2020 Jason Winning
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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.io.IOException;

import org.controlsfx.control.MasterDetailPane;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.data.GUIBibData;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.dialogs.HyperDlg;
import org.hypernomicon.view.dialogs.WorkDlgCtrlr;
import org.hypernomicon.view.previewWindow.PDFJSWrapper;
import org.hypernomicon.view.previewWindow.PreviewWrapper;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.RowConstraints;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class MergeWorksDlgCtrlr extends HyperDlg
{
  @FXML private AnchorPane apMain;
  @FXML private ComboBox<HyperTableCell> cbType1, cbType2, cbType3, cbType4;
  @FXML private GridPane gpAuthors, gpMain, gpTitle, gpType, gpYear;
  @FXML private RadioButton rbAuthors1, rbAuthors2, rbAuthors3, rbAuthors4,
                            rbTitle1, rbTitle2, rbTitle3, rbTitle4, rbType1, rbType2, rbType3, rbType4, rbYear1, rbYear2, rbYear3, rbYear4;
  @FXML private TableView<HyperTableRow> tvAuthors1, tvAuthors2, tvAuthors3, tvAuthors4;
  @FXML private TextField tfTitle1, tfTitle2, tfTitle3, tfTitle4, tfYear1, tfYear2, tfYear3, tfYear4;
  @FXML private ToggleButton btnPreview;
  @FXML private Hyperlink hlFixCase;
  @FXML private CheckBox chkNewEntry;
  @FXML private Button btnLaunch;

  private AnchorPane apPreview;
  private MasterDetailPane mdp;
  private PDFJSWrapper jsWrapper = null;
  private final EnumMap<BibFieldEnum, BibField> singleFields = new EnumMap<>(BibFieldEnum.class);
  private final List<WorkToMerge> works = new ArrayList<>(4);
  private final Map<BibFieldEnum, BibFieldRow> extraRows = new HashMap<>();
  private int nextRowNdx = 4;
  private boolean creatingNewWork, previewInitialized = false;

  public EntryType getEntryType()   { return nullSwitch(extraRows.get(bfEntryType), null, row -> EntryTypeCtrlr.class.cast(row).getEntryType()); }
  public boolean creatingNewEntry() { return chkNewEntry.isVisible() && chkNewEntry.isSelected(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MergeWorksDlgCtrlr create(String title, BibData bd1, BibData bd2, BibData bd3, BibData bd4, HDT_Work destWork,
                                          boolean creatingNewWork, boolean showNewEntry, boolean newEntryChecked, FilePath filePath) throws IOException
  {
    MergeWorksDlgCtrlr mwd = HyperDlg.createUsingFullPath("view/workMerge/MergeWorksDlg.fxml", title, true);
    mwd.init(bd1, bd2, bd3, bd4, destWork, creatingNewWork, showNewEntry, newEntryChecked, filePath);
    return mwd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MergeWorksDlgCtrlr create(String title, BibData bd1, BibData bd2, BibData bd3, BibData bd4, HDT_Work destWork,
                                          boolean creatingNewWork, boolean showNewEntry, boolean newEntryChecked) throws IOException
  {
    return create(title, bd1, bd2, bd3, bd4, destWork, creatingNewWork, showNewEntry, newEntryChecked,
                  nullSwitch(destWork, null, HDT_Work::filePath));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(BibData bd1, BibData bd2, BibData bd3, BibData bd4, HDT_Work destWork,
                    boolean creatingNewWork, boolean showNewEntry, boolean newEntryChecked, FilePath filePath) throws IOException
  {
    apPreview = new AnchorPane();
    mdp = WorkDlgCtrlr.addPreview(stagePane, apMain, apPreview, btnPreview);

    btnLaunch.setOnAction(event ->
    {
      if (FilePath.isEmpty(filePath) || filePath.equals(destWork.filePath()))
        destWork.launch(-1);
      else
        launchFile(filePath);
    });

    mdp.showDetailNodeProperty().addListener((ob, ov, nv) ->
    {
      if ((Boolean.TRUE.equals(nv) == false) || previewInitialized || jxBrowserDisabled) return;

      WorkDlgCtrlr.accommodatePreview(dialogStage, apMain, mdp);

      jsWrapper = new PDFJSWrapper(apPreview, null, null, null);

      if (jxBrowserDisabled) return;

      previewInitialized = true;

      if (FilePath.isEmpty(filePath) || filePath.equals(destWork.filePath()))
        PreviewWrapper.showFile(destWork.filePath(), 1, jsWrapper);
      else
        PreviewWrapper.showFile(filePath, 1, jsWrapper);
    });

    this.creatingNewWork = creatingNewWork;

    if (creatingNewWork == false)
    {
      deleteGridPaneRow(gpMain, 1);
      nextRowNdx--;
    }

    if (FilePath.isEmpty(filePath) && ((destWork == null) || (destWork.pathNotEmpty() == false)))
      disableAll(btnLaunch, btnPreview);

    List<BibData> bdList = new ArrayList<>();

    if (bd1 != null) bdList.add(bd1);
    if (bd2 != null) bdList.add(bd2);
    if (bd3 != null) bdList.add(bd3);
    if (bd4 != null) bdList.add(bd4);

    while (bdList.size() < 4) bdList.add(null);

    bd1 = bdList.get(0);
    bd2 = bdList.get(1);
    bd3 = bdList.get(2);
    bd4 = bdList.get(3);

    if (bd4 != null)
      works.add(0, new WorkToMerge(bd4, rbTitle4, tfTitle4, rbType4, cbType4, rbYear4, tfYear4, rbAuthors4, tvAuthors4, destWork, creatingNewWork));

    if (bd3 != null)
      works.add(0, new WorkToMerge(bd3, rbTitle3, tfTitle3, rbType3, cbType3, rbYear3, tfYear3, rbAuthors3, tvAuthors3, destWork, creatingNewWork));

    works.add(0, new WorkToMerge(bd2, rbTitle2, tfTitle2, rbType2, cbType2, rbYear2, tfYear2, rbAuthors2, tvAuthors2, destWork, creatingNewWork));
    works.add(0, new WorkToMerge(bd1, rbTitle1, tfTitle1, rbType1, cbType1, rbYear1, tfYear1, rbAuthors1, tvAuthors1, destWork, creatingNewWork));

    if (bd4 == null)
    {
      deleteGridPaneRow(gpTitle, 4);
      deleteGridPaneColumn(gpType, 3);
      deleteGridPaneColumn(gpYear, 3);
      deleteGridPaneColumn(gpAuthors, 3);
    }

    if (bd3 == null)
    {
      deleteGridPaneRow(gpTitle, 3);
      deleteGridPaneColumn(gpType, 2);
      deleteGridPaneColumn(gpYear, 2);
      deleteGridPaneColumn(gpAuthors, 2);
    }

    if ((db.bibLibraryIsLinked() == false) || (showNewEntry == false) || (destWork.getBibEntryKey().isBlank() == false))
      chkNewEntry.setVisible(false);
    else
    {
      chkNewEntry.setSelected(newEntryChecked);
      chkNewEntry.setText("Create new " + db.getBibLibrary().type().getUserFriendlyName() + " entry");
      addField(bfEntryType, bd1, bd2, bd3, bd4);
    }

    for (BibFieldEnum bibFieldEnum : BibFieldEnum.values())
    {
      switch (bibFieldEnum)
      {
        case bfAuthors : case bfEditors : case bfTranslators : case bfTitle : case bfYear : case bfWorkType : case bfEntryType :
          continue;

        default : break;
      }

      int cnt = 0;
      BibData singleBD = null;

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
      else if (singleBD != null)
      {
        BibField bibField;

        if ((bibFieldEnum == bfEntryType) || (bibFieldEnum == bfWorkType))
          noOp();
        else if (bibFieldEnum.isMultiLine())
        {
          bibField = new BibField(bibFieldEnum);
          bibField.setAll(singleBD.getMultiStr(bibFieldEnum));
        }
        else
        {
          bibField = new BibField(bibFieldEnum);
          bibField.setStr(singleBD.getStr(bibFieldEnum));
        }
      }

      hlFixCase.setOnAction(event ->
      {
        TextField tf;

        if      (rbTitle1.isSelected()) tf = tfTitle1;
        else if (rbTitle2.isSelected()) tf = tfTitle2;
        else if (rbTitle3.isSelected()) tf = tfTitle3;
        else                            tf = tfTitle4;

        tf.setText(HDT_Work.fixCase(tf.getText()));
      });

      onShown = () -> safeFocus(rbTitle1);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean showModal()
  {
    boolean rv = super.showModal();

    if (previewInitialized)
      jsWrapper.cleanup();

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addField(BibFieldEnum bibFieldEnum, BibData bd1, BibData bd2, BibData bd3, BibData bd4) throws IOException
  {
    BibFieldRow row = BibFieldRow.create(bibFieldEnum, bd1, bd2, bd3, bd4);
    extraRows.put(bibFieldEnum, row);
    AnchorPane ap = row.getAnchorPane();

    GridPane.setRowIndex(ap, nextRowNdx++);
    addToParent(ap, gpMain);

    RowConstraints rc;

    if (bibFieldEnum.isMultiLine())
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
        return falseWithWarningMessage("Select a work type.");
    }

    if (chkNewEntry.isVisible())
    {
      BibData mergedBD = new GUIBibData();

      mergeInto(mergedBD);

      WorkDlgCtrlr.promptToCreateBibEntry(mergedBD, chkNewEntry);

      if (chkNewEntry.isSelected())
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
          return falseWithWarningMessage("Select an entry type.");
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
        work.workType.set(workType);

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
