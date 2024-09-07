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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.BibField.BibFieldType.*;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.io.IOException;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.controlsfx.control.MasterDetailPane;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.dialogs.WorkDlgCtrlr;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.data.GUIBibData;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.previewWindow.PreviewWrapper;
import org.hypernomicon.util.filePath.FilePath;
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
                            rbTitle1, rbTitle2, rbTitle3, rbTitle4, rbType1, rbType2, rbType3, rbType4, rbDate1, rbDate2, rbDate3, rbDate4;
  @FXML private TableView<HyperTableRow> tvAuthors1, tvAuthors2, tvAuthors3, tvAuthors4;
  @FXML private TextField tfTitle1, tfTitle2, tfTitle3, tfTitle4, tfDate1, tfDate2, tfDate3, tfDate4;
  @FXML private ToggleButton btnPreview;
  @FXML private Hyperlink hlFixCase;
  @FXML private CheckBox chkNewEntry;
  @FXML private Button btnLaunch;

  private final AnchorPane apPreview;
  private final MasterDetailPane mdp;
  private final Map<BibFieldEnum, BibField> singleFields = new EnumMap<>(BibFieldEnum.class);
  private final Map<BibFieldEnum, BibFieldRow> extraRows = new EnumMap<>(BibFieldEnum.class);
  public final List<BibData> bibDataList;
  private final List<WorkToMerge> works = new ArrayList<>(4);
  private final boolean creatingNewWork;
  private final MutableBoolean alreadyChangingTitle = new MutableBoolean(false);

  private PDFJSWrapper jsWrapper = null;
  private int nextRowNdx = 4;
  private boolean previewInitialized = false;
  private Ternary newEntryChoice;

  public EntryType getEntryType()   { return nullSwitch(extraRows.get(bfEntryType), null, row -> ((EntryTypeCtrlr) row).getEntryType()); }
  public Ternary creatingNewEntry() { return chkNewEntry.isVisible() == false ? Ternary.Unset : (chkNewEntry.isSelected() ? Ternary.True : newEntryChoice); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Show popup window to merge information about a work or bibliographic entry from multiple sources
   * @param title Title of the popup
   * @param newBibDataList List of BibData sources. Use Arrays.asList to build the list because it needs to be able to contain null values.
   * @param destWork The work we are merging information into, if any
   * @param creatingNewWork Whether a new work record is being created
   * @param showNewEntry Whether to show the checkbox to create a new bibliographic entry
   * @param newEntryChoice Whether the user has already expressed a preference about creating a new entry
   * @throws IOException if unable to load the FXML
   */
  public MergeWorksDlgCtrlr(String title, List<BibData> newBibDataList, HDT_Work destWork, boolean creatingNewWork, boolean showNewEntry, Ternary newEntryChoice) throws IOException
  {
    this(title, newBibDataList, destWork, creatingNewWork, showNewEntry, newEntryChoice, nullSwitch(destWork, null, HDT_Work::filePath));
  }

  /**
   * Show popup window to merge information about a work or bibliographic entry from multiple sources
   * @param title Title of the popup
   * @param newBibDataList List of BibData sources. Use Arrays.asList to build the list because it needs to be able to contain null values.
   * @param destWork The work we are merging information into, if any
   * @param creatingNewWork Whether a new work record is being created
   * @param showNewEntry Whether to show the checkbox to create a new bibliographic entry
   * @param newEntryChoice Whether the user has already expressed a preference about creating a new entry
   * @param filePath Path of file to use for new work record
   * @throws IOException
   */
  public MergeWorksDlgCtrlr(String title, List<BibData> newBibDataList, HDT_Work destWork, boolean creatingNewWork, boolean showNewEntry, Ternary newEntryChoice, FilePath filePath) throws IOException
  {
    super("dialogs/workMerge/MergeWorksDlg", title, true, true);

    this.newEntryChoice = newEntryChoice;
    apPreview = new AnchorPane();
    mdp = WorkDlgCtrlr.addPreview(stagePane, apMain, apPreview, btnPreview);

    bibDataList = newBibDataList.stream().filter(Objects::nonNull).toList();  // Make a copy to make sure it is unmodifiable

    assert((bibDataList.size() > 1) && (bibDataList.size() < 5));

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

      jsWrapper = new PDFJSWrapper(apPreview);

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

    if (bibDataList.size() > 3)
      works.add(0, new WorkToMerge(bibDataList.get(3), rbTitle4, tfTitle4, rbType4, cbType4, rbDate4, tfDate4, rbAuthors4, tvAuthors4,
                                   destWork, creatingNewWork, alreadyChangingTitle));

    if (bibDataList.size() > 2)
      works.add(0, new WorkToMerge(bibDataList.get(2), rbTitle3, tfTitle3, rbType3, cbType3, rbDate3, tfDate3, rbAuthors3, tvAuthors3,
                                   destWork, creatingNewWork, alreadyChangingTitle));

    works.add(0, new WorkToMerge(bibDataList.get(1), rbTitle2, tfTitle2, rbType2, cbType2, rbDate2, tfDate2, rbAuthors2, tvAuthors2,
                                 destWork, creatingNewWork, alreadyChangingTitle));
    works.add(0, new WorkToMerge(bibDataList.get(0), rbTitle1, tfTitle1, rbType1, cbType1, rbDate1, tfDate1, rbAuthors1, tvAuthors1,
                                 destWork, creatingNewWork, alreadyChangingTitle));

    if (bibDataList.size() < 4)
    {
      deleteGridPaneRow(gpTitle, 4);
      deleteGridPaneColumn(gpType, 3);
      deleteGridPaneColumn(gpYear, 3);
      deleteGridPaneColumn(gpAuthors, 3);
    }

    if (bibDataList.size() < 3)
    {
      deleteGridPaneRow(gpTitle, 3);
      deleteGridPaneColumn(gpType, 2);
      deleteGridPaneColumn(gpYear, 2);
      deleteGridPaneColumn(gpAuthors, 2);
    }

    if ((db.bibLibraryIsLinked() == false) || (showNewEntry == false) || (destWork.getBibEntryKey().isBlank() == false) || HDT_Work.isUnenteredSet(destWork))
      chkNewEntry.setVisible(false);
    else
    {
      chkNewEntry.setSelected(newEntryChoice.isTrue());
      chkNewEntry.setText("Create new " + db.getBibLibrary().type().getUserFriendlyName() + " entry");
      addField(bfEntryType);
    }

    for (BibFieldEnum bibFieldEnum : BibFieldEnum.values())
    {
      switch (bibFieldEnum)
      {
        case bfAuthors : case bfEditors : case bfTranslators : case bfTitle : case bfDate : case bfWorkType : case bfEntryType :
          continue;

        default : break;
      }

      int cnt = 0;
      BibData singleBD = null;
      boolean fieldsEqual = true;

      for (BibData bd : bibDataList)
      {
        if (bd.fieldNotEmpty(bibFieldEnum))
        {
          if (singleBD != null)
          {
            if (singleBD.fieldsAreEqual(bibFieldEnum, bd, false) == false)
              fieldsEqual = false;
          }
          else
            singleBD = bd;

          cnt++;
        }
      }

      if ((bibFieldEnum == bfMisc) || ((cnt > 0) && ((fieldsEqual == false) || (cnt < bibDataList.size()))))
      {
        if ((bibFieldEnum == bfPublisher) && (extraRows.containsKey(bfPubLoc) == false))
        {
          HDT_WorkType bookWorkType = HDT_WorkType.get(WorkTypeEnum.wtBook);

          if (bibDataList.stream().anyMatch(bd -> (bd.fieldNotEmpty(bfPublisher) && (EntryType.toWorkType(bd.getEntryType()) == bookWorkType))))
          {
            singleFields.remove(bfPubLoc);

            addField(bfPubLoc);
          }
        }

        addField(bibFieldEnum);
      }
      else if (singleBD != null)
      {
        if (bibFieldEnum.isMultiLine())
        {
          BibField bibField = new BibField(bibFieldEnum);
          bibField.setAll(singleBD.getMultiStr(bibFieldEnum));
          singleFields.put(bibFieldEnum, bibField);
        }
        else if (bibFieldEnum.getType() == bftString)
        {
          BibField bibField = new BibField(bibFieldEnum);
          bibField.setStr(singleBD.getStr(bibFieldEnum));
          singleFields.put(bibFieldEnum, bibField);
        }
      }
    }

    hlFixCase.setOnAction(event ->
    {
      TextField tf;

      if      (rbTitle1.isSelected()) tf = tfTitle1;
      else if (rbTitle2.isSelected()) tf = tfTitle2;
      else if (rbTitle3.isSelected()) tf = tfTitle3;
      else                            tf = tfTitle4;

      alreadyChangingTitle.setTrue();
      tf.setText(HDT_Work.fixCase(tf.getText()));
      alreadyChangingTitle.setFalse();
    });

    onShown = () -> safeFocus(rbTitle1);
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

  private void addField(BibFieldEnum bibFieldEnum) throws IOException
  {
    BibFieldRow row = BibFieldRow.create(bibFieldEnum, bibDataList);
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
        return falseWithWarningPopup("Select a work type.");

      if (workType.enumVal() == WorkTypeEnum.wtUnenteredSet)
        return falseWithWarningPopup("Select a different work type.");
    }

    if (chkNewEntry.isVisible())
    {
      GUIBibData mergedBD = new GUIBibData();

      mergeInto(mergedBD);

      if (newEntryChoice.isFalse())
        newEntryChoice = Ternary.Unset;

      newEntryChoice = WorkDlgCtrlr.promptToCreateBibEntry(mergedBD, chkNewEntry, newEntryChoice, null);

      if (chkNewEntry.isSelected())
      {
        EntryType entryType = getEntryType();

        if (entryType != null)
        {
          switch (entryType)
          {
            case etUnentered : case etOther : case etNone : entryType = null; break;
            default : break;
          }
        }

        if (entryType == null)
          return falseWithWarningPopup("Select an entry type.");
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_WorkType getMergedWorkType()
  {
    if (rbType1.isSelected()) return works.get(0).getWorkType();
    if (rbType2.isSelected()) return works.get(1).getWorkType();
    if (rbType3.isSelected()) return works.get(2).getWorkType();
                              return works.get(3).getWorkType();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void mergeInto(BibData mergedBD)
  {
    String title, dateRawStr;
    List<ObjectGroup> authGroups;
    HDT_Work work = mergedBD.getWork();
    HDT_WorkType workType = getMergedWorkType();

    if      (rbTitle1.isSelected()) title = tfTitle1.getText();
    else if (rbTitle2.isSelected()) title = tfTitle2.getText();
    else if (rbTitle3.isSelected()) title = tfTitle3.getText();
    else                            title = tfTitle4.getText();

    if      (rbDate1.isSelected()) dateRawStr = tfDate1.getText();
    else if (rbDate2.isSelected()) dateRawStr = tfDate2.getText();
    else if (rbDate3.isSelected()) dateRawStr = tfDate3.getText();
    else                           dateRawStr = tfDate4.getText();

    if      (rbAuthors1.isSelected()) authGroups = works.get(0).getAuthorGroups(work);
    else if (rbAuthors2.isSelected()) authGroups = works.get(1).getAuthorGroups(work);
    else if (rbAuthors3.isSelected()) authGroups = works.get(2).getAuthorGroups(work);
    else                              authGroups = works.get(3).getAuthorGroups(work);

    mergedBD.setTitle(title);
    mergedBD.setDate(BibliographicDate.fromUserStr(dateRawStr));

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
        case bfAuthors : case bfEditors : case bfTranslators : case bfTitle : case bfDate :
          continue;

        default : break;
      }

      BibFieldRow bibFieldRow = extraRows.get(bibFieldEnum);

      if (bibFieldRow != null)
      {
        bibFieldRow.mergeInto(mergedBD); // assign data from bibFieldRow to work and bd
        continue;
      }

      // assign data from original work/bd that had this field, if any (must have been zero or one of them)

      nullSwitch(singleFields.get(bibFieldEnum), field ->
      {
        if (bibFieldEnum.getType() == bftMultiString)
          mergedBD.setMultiStr(bibFieldEnum, field.getMultiStr());
        else if (bibFieldEnum.getType() == bftString)
          mergedBD.setStr(bibFieldEnum, field.getStr());
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
