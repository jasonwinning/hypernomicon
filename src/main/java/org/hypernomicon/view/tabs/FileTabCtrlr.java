/*
 * Copyright 2015-2023 Jason Winning
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

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkLabel;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_FileType;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.fileTabEnum;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;

import org.hypernomicon.dialogs.FileDlgCtrlr;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitMenuButton;
import javafx.scene.control.SplitPane;
import javafx.scene.control.Tab;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public class FileTabCtrlr extends HyperTab<HDT_MiscFile, HDT_MiscFile>
{
  @FXML private AnchorPane apDescription;
  @FXML private Button btnLaunch, btnManage, btnTree, btnWork;
  @FXML private CheckBox checkAnnotated;
  @FXML private ComboBox<HyperTableCell> cbType, cbWork;
  @FXML private SplitMenuButton btnShow;
  @FXML private SplitPane spBottomVert, spRightHoriz, spRightVert;
  @FXML private TableView<HyperTableRow> tvAuthors, tvKeyMentions, tvLabels;
  @FXML private TextField tfFileName, tfName, tfSearchKey;

  private final MainTextWrapper mainText;
  private final HyperTable htLabels, htAuthors, htKeyMentioners;
  private final HyperCB hcbWork, hcbType;

  public FileDlgCtrlr fdc = null;
  private HDT_MiscFile curMiscFile;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FileTabCtrlr(Tab tab) throws IOException
  {
    super(fileTabEnum, tab, "view/tabs/FileTab");

    mainText = new MainTextWrapper(apDescription);
    tfFileName.setEditable(false);

    addShowMenuItem("Show in system explorer", event -> { if (tfFileName.getText().length() > 0) highlightFileInExplorer(curMiscFile.filePath()); });
    addShowMenuItem("Show in file manager"   , event -> { if (tfFileName.getText().length() > 0) ui.goToFileInManager(curMiscFile.filePath()); });
    addShowMenuItem("Copy path to clipboard" , event -> { if (tfFileName.getText().length() > 0) copyToClipboard(curMiscFile.getPath().toString()); });

    addShowMenuItem("Unassign file", event ->
    {
      if (ui.cantSaveRecord()) return;
      curMiscFile.getPath().clear();
      ui.update();
    });

    htAuthors = new HyperTable(tvAuthors, 1, true, PREF_KEY_HT_FILE_AUTHORS);

    htAuthors.addActionCol(ctGoBtn, 1);
    htAuthors.addCol(hdtPerson, ctDropDownList);

    htAuthors.addRemoveMenuItem();
    htAuthors.addChangeOrderMenuItem(true);

    htLabels = new HyperTable(tvLabels, 2, true, PREF_KEY_HT_FILE_LABELS);

    htLabels.addActionCol(ctGoBtn, 2);
    htLabels.addActionCol(ctBrowseBtn, 2);
    htLabels.addCol(hdtWorkLabel, ctDropDownList);

    htLabels.addRemoveMenuItem();
    htLabels.addChangeOrderMenuItem(true);

    htKeyMentioners = new HyperTable(tvKeyMentions, 1, false, PREF_KEY_HT_FILE_MENTIONERS);

    htKeyMentioners.addDefaultMenuItems();

    htKeyMentioners.addIconCol();
    htKeyMentioners.addLabelCol(hdtNone);
    htKeyMentioners.addLabelCol(hdtNone);

    hcbType = new HyperCB(cbType, ctDropDown, new StandardPopulator(hdtFileType), true);
    hcbWork = new HyperCB(cbWork, ctDropDownList, new StandardPopulator(hdtWork), true);

    hcbWork.addListener((ov, nv) -> cbWorkChange());

    btnWork  .setOnAction(event -> ui.goToRecord(HyperTableCell.getRecord(hcbWork.selectedHTC()), true));
    btnTree  .setOnAction(event -> ui.goToTreeRecord(curMiscFile));
    btnLaunch.setOnAction(event -> { if (tfFileName.getText().length() > 0) launchFile(curMiscFile.filePath()); });
    btnShow  .setOnAction(event -> { if (tfFileName.getText().length() > 0) highlightFileInExplorer(curMiscFile.filePath()); });

    setToolTip(btnManage, "Update or rename file");

    MainCtrlr.setSearchKeyToolTip(tfSearchKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String recordName()                 { return tfName.getText(); }
  @Override protected RecordType type()                { return hdtMiscFile; }
  @Override public MainTextWrapper mainTextWrapper()   { return mainText; }
  @Override public void setRecord(HDT_MiscFile record) { curMiscFile = record; }

  @FXML public boolean btnManageClick()                { return showFileDialog(null); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateFromRecord()
  {
    btnTree.setDisable(ui.tree().getRowsForRecord(curMiscFile).isEmpty());

    tfName.setText(curMiscFile.name());
    tfSearchKey.setText(curMiscFile.getSearchKey());
    checkAnnotated.setSelected(curMiscFile.getAnnotated());

    refreshFile();

    mainText.loadFromRecord(curMiscFile, true, getView().getTextInfo());

    hcbType.addAndSelectEntry(curMiscFile.fileType, HDT_Record::getCBText);

  // Populate key mentioners
  // -----------------------

    WorkTabCtrlr.populateDisplayersAndKeyMentioners(curMiscFile, htKeyMentioners);

 // populate authors
 // ----------------

    hcbWork.addAndSelectEntry(curMiscFile.work, HDT_Record::getCBText);

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

  @Override public void clear()
  {
    mainText.clear();
    tfName.setText("");
    tfFileName.setText("");
    tfSearchKey.setText("");
    checkAnnotated.setSelected(false);

    htAuthors.clear();
    htLabels.clear();
    htKeyMentioners.clear();

    hcbType.clear();
    hcbWork.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord()
  {
    if (saveSearchKey(curMiscFile, tfSearchKey) == false) return false;

    int fileTypeID = hcbType.selectedID();
    if ((fileTypeID < 1) && hcbType.getText().isEmpty())
      return falseWithErrorMessage("You must enter a file type.", cbType);

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

    curMiscFile.setName(tfName.getText());
    curMiscFile.setAnnotated(checkAnnotated.isSelected());

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showFileDialog(FilePath srcFilePath)
  {
    fdc = new FileDlgCtrlr("Miscellaneous file", curMiscFile, tfName.getText(), false);

    if (FilePath.isEmpty(srcFilePath) == false)
      fdc.setSrcFilePath(srcFilePath, true);

    boolean result = fdc.showModal();

    if (result)
    {
      tfFileName.setText(curMiscFile.pathNotEmpty() ? db.getRootPath().relativize(curMiscFile.filePath()).toString() : "");

      tfName.setText(fdc.tfRecordName.getText());

      hcbType.clear();
      HyperTableCell cell = fdc.cbType.getValue();
      if (HyperTableCell.isEmpty(cell) == false)
      {
        hcbType.addEntry(cell.getID(), cell.getText(), false);
        cbType.setValue(cell);
        cbType.getSelectionModel().select(cell);
      }
    }

    fdc = null;
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spBottomVert, PREF_KEY_FILE_BOTTOM_VERT, 0);
    setDividerPosition(spRightHoriz, PREF_KEY_FILE_RIGHT_HORIZ, 0);
    setDividerPosition(spRightVert , PREF_KEY_FILE_RIGHT_VERT , 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spBottomVert, PREF_KEY_FILE_BOTTOM_VERT, 0);
    getDividerPosition(spRightHoriz, PREF_KEY_FILE_RIGHT_HORIZ, 0);
    getDividerPosition(spRightVert , PREF_KEY_FILE_RIGHT_VERT , 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
