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

import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.io.IOException;

import org.controlsfx.control.MasterDetailPane;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.dialogs.WorkDlgCtrlr;
import org.hypernomicon.dialogs.workMerge.BibFieldRow.*;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.data.GUIBibData;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.previewWindow.PreviewWrapper;
import org.hypernomicon.util.filePath.FilePath;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class MergeWorksDlgCtrlr extends HyperDlg
{
  @FXML private AnchorPane apMain;
  @FXML private GridPane gpMain;
  @FXML private ToggleButton btnPreview;
  @FXML private CheckBox chkNewEntry;
  @FXML private Button btnLaunch;

  private final AnchorPane apPreview;
  private final MasterDetailPane mdp;
  private final Map<BibFieldEnum, BibField> singleFields = new EnumMap<>(BibFieldEnum.class);
  private final Map<BibFieldEnum, BibFieldRow<?>> rows = new EnumMap<>(BibFieldEnum.class);
  private final List<BibData> bibDataList;
  private final boolean creatingNewWork;

  private PDFJSWrapper jsWrapper = null;
  private int nextRowNdx = 0;
  private boolean previewInitialized = false;
  private Ternary newEntryChoice;

  public EntryType getEntryType()   { return nullSwitch(rows.get(bfEntryType), null, row -> ((EntryTypeRow)row).selectedCtrlr().getEntryType()); }
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
   * @throws IOException if unable to load the FXML
   */
  public MergeWorksDlgCtrlr(String title, List<BibData> newBibDataList, HDT_Work destWork, boolean creatingNewWork, boolean showNewEntry, Ternary newEntryChoice, FilePath filePath) throws IOException
  {
    super("dialogs/workMerge/MergeWorksDlg", title, true, true);

    this.newEntryChoice = newEntryChoice;
    this.creatingNewWork = creatingNewWork;

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

    if (FilePath.isEmpty(filePath) && ((destWork == null) || (destWork.pathNotEmpty() == false)))
      disableAll(btnLaunch, btnPreview);

    addRow(bfTitle);

    if (creatingNewWork)
      addRow(bfWorkType);

    addRow(bfDate);
    addRow(bfAuthors, destWork);

    if ((db.bibLibraryIsLinked() == false) || (showNewEntry == false) || ((destWork != null) && (destWork.getBibEntryKey().isBlank() == false)) || HDT_Work.isUnenteredSet(destWork))
      chkNewEntry.setVisible(false);
    else
    {
      chkNewEntry.setSelected(newEntryChoice.isTrue());
      chkNewEntry.setText("Create new " + db.getBibLibrary().type().getUserFriendlyName() + " entry");

      addRow(bfEntryType);
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
        if ((bibFieldEnum == bfPublisher) && (rows.containsKey(bfPubLoc) == false))
        {
          HDT_WorkType bookWorkType = HDT_WorkType.get(WorkTypeEnum.wtBook);

          if (bibDataList.stream().anyMatch(bd -> (bd.fieldNotEmpty(bfPublisher) && (EntryType.toWorkType(bd.getEntryType()) == bookWorkType))))
          {
            singleFields.remove(bfPubLoc);

            addRow(bfPubLoc);
          }
        }

        addRow(bibFieldEnum);
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

    onShown = () -> ((TitleRow)rows.get(bfTitle)).focus();
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

  private void addRow(BibFieldEnum bibFieldEnum) throws IOException
  {
    addRow(bibFieldEnum, null);
  }

  @SuppressWarnings({ "rawtypes", "unchecked" })
  private void addRow(BibFieldEnum bibFieldEnum, HDT_Work destWork) throws IOException
  {
    BibFieldRow row = BibFieldRow.create(bibFieldEnum, bibDataList, destWork);
    rows.put(bibFieldEnum, row);
    AnchorPane ap = row.getAnchorPane();

    GridPane.setRowIndex(ap, nextRowNdx++);
    addToParent(ap, gpMain);

    GridPane.setVgrow(ap, Priority.ALWAYS);

    Double height = row.getHeight();

    gpMain.getRowConstraints().add(height == null ? new RowConstraints() : new RowConstraints(height, height, height));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (creatingNewWork)
    {
      WorkTypeRow workTypeRow = (WorkTypeRow)rows.get(bfWorkType);

      HDT_WorkType workType = workTypeRow.selectedCtrlr().getWorkType();

      if (workType == null)
        return falseWithWarningPopup("Select a work type.", workTypeRow.selectedCtrlr().getCB());
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
        EntryTypeRow entryTypeRow = (EntryTypeRow)rows.get(bfEntryType);
        EntryType entryType = entryTypeRow.selectedCtrlr().getEntryType();

        if (entryType != null)
        {
          switch (entryType)
          {
            case etUnentered : case etOther : case etNone : entryType = null; break;
            default : break;
          }
        }

        if (entryType == null)
          return falseWithWarningPopup("Select an entry type.", entryTypeRow.selectedCtrlr().getCB());
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void mergeInto(BibData mergedBD)
  {
    for (BibFieldEnum bibFieldEnum : BibFieldEnum.values())
    {
      switch (bibFieldEnum)
      {
        case bfEditors : case bfTranslators :
          continue;

        default : break;
      }

      BibFieldRow<?> bibFieldRow = rows.get(bibFieldEnum);

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
