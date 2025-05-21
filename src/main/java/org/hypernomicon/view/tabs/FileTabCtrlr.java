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

import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_FileType;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.*;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;

import org.hypernomicon.dialogs.FileDlgCtrlr;
import org.hypernomicon.fileManager.FileManager;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public class FileTabCtrlr extends HyperTab<HDT_MiscFile, HDT_MiscFile>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apDescription;
  @FXML private Button btnLaunch, btnManage, btnTree, btnWork;
  @FXML private CheckBox checkAnnotated;
  @FXML private ComboBox<HyperTableCell> cbType, cbWork;
  @FXML private SplitMenuButton btnShow;
  @FXML private SplitPane spBottomVert, spRightHoriz, spRightVert;
  @FXML private TableView<HyperTableRow> tvAuthors, tvKeyMentions, tvLabels;
  @FXML private TextField tfFileName, tfName, tfSearchKey;

  private final MainTextWrapper mainText;
  private final HyperTable htLabels, htAuthors, htMentioners;
  private final HyperCB hcbWork, hcbType;

  public FileDlgCtrlr fdc = null;
  private HDT_MiscFile curMiscFile;

//---------------------------------------------------------------------------

  public FileTabCtrlr(Tab tab) throws IOException
  {
    super(fileTabEnum, tab, "view/tabs/FileTab");

    mainText = new MainTextWrapper(apDescription);
    tfFileName.setEditable(false);

    addShowMenuItem("Show in System Explorer", event -> { if (tfFileName.getText().length() > 0) highlightFileInExplorer(curMiscFile.filePath()); });
    addShowMenuItem("Show in File Manager"   , event -> { if (tfFileName.getText().length() > 0) FileManager.show(curMiscFile.filePath()); });
    addShowMenuItem("Copy Path to Clipboard" , event -> { if (tfFileName.getText().length() > 0) copyToClipboard(curMiscFile.getPath().toString()); });

    addShowMenuItem("Unassign File", event ->
    {
      if (ui.cantSaveRecord()) return;
      curMiscFile.getPath().clear();
      ui.update();
    });

    htAuthors = new HyperTable(tvAuthors, 1, true, TablePrefKey.FILE_AUTHORS);

    htAuthors.addActionCol(ctGoBtn, 1);
    htAuthors.addAuthorEditCol(null, null);

    htAuthors.addRemoveMenuItem();
    htAuthors.addChangeOrderMenuItem(true);

    htLabels = new HyperTable(tvLabels, 2, true, TablePrefKey.FILE_LABELS);

    htLabels.addActionCol(ctGoBtn    , 2).setTooltip(ButtonAction.baGo    , "Go to this record");
    htLabels.addActionCol(ctBrowseBtn, 2).setTooltip(ButtonAction.baBrowse, "Select a Label from the Tree");
    htLabels.addCol(hdtWorkLabel, ctEditableLimitedDropDown);

    htLabels.addRemoveMenuItem();
    htLabels.addChangeOrderMenuItem(true);

    htMentioners = new HyperTable(tvKeyMentions, 1, false, TablePrefKey.FILE_MENTIONERS);

    htMentioners.addDefaultMenuItems();

    htMentioners.addIconCol();
    htMentioners.addLabelCol(hdtNone);
    htMentioners.addLabelCol(hdtNone);

    hcbType = new HyperCB(cbType, ctEditableUnlimitedDropDown, new StandardPopulator(hdtFileType), true);
    hcbWork = new HyperCB(cbWork, ctEditableLimitedDropDown  , new StandardPopulator(hdtWork    ), true);

    hcbWork.addListener((ov, nv) -> cbWorkChange());

    btnWork  .setOnAction(event -> ui.goToRecord(HyperTableCell.getRecord(hcbWork.selectedHTC()), true));
    btnTree  .setOnAction(event -> ui.goToTreeRecord(curMiscFile));
    btnLaunch.setOnAction(event -> { if (tfFileName.getText().length() > 0) launchFile(curMiscFile.filePath()); });
    btnShow  .setOnAction(event -> { if (tfFileName.getText().length() > 0) highlightFileInExplorer(curMiscFile.filePath()); });

    setToolTip(btnShow  , "Show file in system explorer");
    setToolTip(btnManage, "Update or rename file");
    setToolTip(btnTree  , "Go to this record in Tree tab");
    setToolTip(btnWork  , "Go to this Work record");

    MainCtrlr.setSearchKeyToolTip(tfSearchKey);

    db.addMentionsNdxCompleteHandler(() -> NoteTabCtrlr.updateMentioners(curMiscFile, htMentioners));
  }

//---------------------------------------------------------------------------

  @Override protected RecordType type()                   { return hdtMiscFile; }
  @Override protected void setRecord(HDT_MiscFile record) { curMiscFile = record; }

  @Override public String recordName()                    { return tfName.getText(); }
  @Override public MainTextWrapper mainTextWrapper()      { return mainText; }

  @FXML public boolean btnManageClick()                   { return showFileDialog(null, true); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateFromRecord()
  {
    btnTree.setDisable(ui.tree().getRowsForRecord(curMiscFile).isEmpty());

    tfName.setText(curMiscFile.name());
    tfSearchKey.setText(curMiscFile.getSearchKey());
    checkAnnotated.setSelected(curMiscFile.getAnnotated());

    refreshFile();

    mainText.loadFromRecord(curMiscFile, true, getView().getTextInfo());

    hcbType.selectIDofRecord(curMiscFile.fileType);

  // Populate mentioners
  // -------------------

    NoteTabCtrlr.updateMentioners(curMiscFile, htMentioners);

  // Populate authors
  // ----------------

    hcbWork.selectIDofRecord(curMiscFile.work);

    cbWorkChange();

    htLabels.buildRows(curMiscFile.labelStream(), (row, label) -> row.setCellValue(2, label, label.extendedText()));

    safeFocus(tfName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refreshFile()
  {
    if (curMiscFile.pathNotEmpty())
    {
      FilePath filePath = curMiscFile.filePath(),
               relPath = db.getRootPath().relativize(filePath);

      tfFileName.setText(relPath == null ? filePath.getNameOnly().toString() : relPath.toString());
    }
    else
      tfFileName.setText("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cbWorkChange()
  {
    HDT_Work work = hcbWork.selectedRecord();

    htAuthors.setCanAddRows(work == null);
    htAuthors.clear();

    if (curMiscFile == null) return;

    htAuthors.buildRows(work == null ? curMiscFile.authorRecords() : work.authorRecords,
      (row, author) -> row.setCellValue(1, author, author.getCBText()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addShowMenuItem(String text, EventHandler<ActionEvent> handler)
  {
    MenuItem menuItem = new MenuItem(text);
    menuItem.setOnAction(handler);
    btnShow.getItems().add(menuItem);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    curMiscFile = resetRecord ? null : HDT_Record.getCurrentInstance(curMiscFile);

    mainText.clear();

    tfName     .setText("");
    tfFileName .setText("");
    tfSearchKey.setText("");

    checkAnnotated.setSelected(false);

    htAuthors      .clear();
    htLabels       .clear();
    htMentioners.clear();

    hcbType.clear();
    hcbWork.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord(boolean saveNameIfBlank)
  {
    int fileTypeID = hcbType.selectedID();
    if (saveNameIfBlank && (fileTypeID < 1) && hcbType.getText().isEmpty())
      return falseWithErrorPopup("You must enter a file type.", cbType);

    if (saveNameIfBlank && tfName.getText().isBlank())
      return falseWithErrorPopup("The record name is blank.", tfName);

    if (saveSearchKey(curMiscFile, tfSearchKey) == false)
      return false;

    mainText.save();

    curMiscFile.work.setID(hcbWork.selectedID());

    if (curMiscFile.work.isNull())
    {
      if (curMiscFile.setAuthors(htAuthors.saveToList(1, hdtPerson)) == false)
        return false;
    }
    else
      db.getObjectList(rtAuthorOfFile, curMiscFile, false).clear();

    MainText.setKeyWorkMentioners(curMiscFile, htLabels.saveToList(2, hdtWorkLabel), HDT_WorkLabel.class);

  // Start file type

    if ((fileTypeID < 1) && (hcbType.getText().length() > 0))
    {
      HDT_FileType fileType = db.createNewBlankRecord(hdtFileType);
      fileTypeID = fileType.getID();
      fileType.setName(hcbType.getText());
    }

    HDT_FileType oldFileType = curMiscFile.fileType.get();
    curMiscFile.fileType.setID(fileTypeID);

    if ((oldFileType != null) && oldFileType.miscFiles.isEmpty())
      db.deleteRecord(oldFileType);

  // End file type

    if (saveNameIfBlank || (tfName.getText().isBlank() == false))
      curMiscFile.setName(tfName.getText());

    curMiscFile.setAnnotated(checkAnnotated.isSelected());

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showFileDialog(FilePath srcFilePath, boolean saveFirst)
  {
    if (saveFirst && ui.cantSaveRecord(false)) return false;

    fdc = new FileDlgCtrlr("Miscellaneous file", curMiscFile, tfName.getText(), false);

    if (FilePath.isEmpty(srcFilePath) == false)
      fdc.setSrcFilePath(srcFilePath, true);

    boolean result = fdc.showModal();

    if (result)
    {
      tfFileName.setText(curMiscFile.pathNotEmpty() ? db.getRootPath().relativize(curMiscFile.filePath()).toString() : "");

      tfName.setText(fdc.tfRecordName.getText());

      hcbType.clear();
      HyperTableCell cell = fdc.hcbType.selectedHTC();
      if (GenericNonRecordHTC.isEmpty(cell) == false)
      {
        hcbType.populate(false);
        hcbType.addAndSelectEntry(cell.getID(), HyperTableCell.getCellText(cell));
      }
    }

    fdc = null;
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spBottomVert, DividerPositionPrefKey.FILE_BOTTOM_VERT, 0);
    setDividerPosition(spRightHoriz, DividerPositionPrefKey.FILE_RIGHT_HORIZ, 0);
    setDividerPosition(spRightVert , DividerPositionPrefKey.FILE_RIGHT_VERT , 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spBottomVert, DividerPositionPrefKey.FILE_BOTTOM_VERT, 0);
    getDividerPosition(spRightHoriz, DividerPositionPrefKey.FILE_RIGHT_HORIZ, 0);
    getDividerPosition(spRightVert , DividerPositionPrefKey.FILE_RIGHT_VERT , 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
