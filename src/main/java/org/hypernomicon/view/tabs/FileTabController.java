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

package org.hypernomicon.view.tabs;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_FileType;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.FileDialogController;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitMenuButton;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public class FileTabController extends HyperTab<HDT_MiscFile, HDT_MiscFile>
{
  @FXML private AnchorPane apDescription;
  @FXML private Button btnLaunch, btnManage, btnTree, btnWork;
  @FXML private CheckBox checkAnnotated;
  @FXML private ComboBox<HyperTableCell> cbFileType, cbWork;
  @FXML private SplitMenuButton btnShow;
  @FXML private SplitPane spBottomVert, spRightHoriz, spRightVert;
  @FXML private TableView<HyperTableRow> tvAuthors, tvKeyMentions, tvLabels;
  @FXML private TextField tfFileName, tfName, tfSearchKey;

  private MainTextWrapper mainText;
  private HyperTable htLabels, htAuthors, htKeyMentioners;
  private HyperCB hcbWork, hcbFileType;
  public FileDialogController fdc = null;
  private HDT_MiscFile curMiscFile;

  @Override public String getRecordName()                    { return tfName.getText(); }
  @Override HDT_RecordType getType()                         { return hdtMiscFile; }
  @Override public void enable(boolean enabled)              { ui.tabFiles.getContent().setDisable(enabled == false); }
  @Override public void findWithinDesc(String text)          { mainText.hilite(text); }
  @Override public TextViewInfo getMainTextInfo()            { return mainText.getViewInfo(); }
  @Override public MainTextWrapper getMainTextWrapper()      { return mainText; }
  @Override void focusOnSearchKey()                          { safeFocus(tfSearchKey); }
  @Override public void setRecord(HDT_MiscFile activeRecord) { curMiscFile = activeRecord; }

  @FXML public boolean btnManageClick()                      { return showFileDialog(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean update()
  {
    btnTree.setDisable(ui.getTree().getRowsForRecord(curMiscFile).size() == 0);

    tfName.setText(curMiscFile.name());
    tfSearchKey.setText(curMiscFile.getSearchKey());
    checkAnnotated.setSelected(curMiscFile.getAnnotated());

    refreshFile();

    mainText.loadFromRecord(curMiscFile, true, getView().getTextInfo());

    hcbFileType.addAndSelectEntry(curMiscFile.fileType, HDT_Base::getCBText);

  // Populate key mentioners
  // -----------------------

    WorkTabController.populateDisplayersAndKeyMentioners(curMiscFile, htKeyMentioners);

 // populate authors
 // ----------------

   hcbWork.addAndSelectEntry(curMiscFile.work, HDT_Base::getCBText);

   cbWorkChange();

   safeFocus(tfName);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refreshFile()
  {
    if (curMiscFile.getPath().isEmpty() == false)
    {
      FilePath filePath = curMiscFile.getPath().getFilePath();
      FilePath relPath = db.getRootFilePath().relativize(filePath);

      if (relPath == null)
        tfFileName.setText(filePath.getNameOnly().toString());
      else
        tfFileName.setText(relPath.toString());
    }
    else
      tfFileName.setText("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cbWorkChange()
  {
    int workID = hcbWork.selectedID();

    htLabels.setCanAddRows(workID < 1);
    htAuthors.setCanAddRows(workID < 1);

    htLabels.clear();
    htAuthors.clear();

    if (curMiscFile == null) return;

    if (workID > 0)
    {
      HDT_Work work = db.works.getByID(workID);

      htAuthors.buildRows(work.authorRecords, (row, author) -> row.setCellValue(1, author, author.getCBText()));
      htLabels.buildRows(work.labels,         (row, label)  -> row.setCellValue(2, label, label.getExtendedText()));
    }
    else
    {
      htAuthors.buildRows(curMiscFile.authors, (row, author) -> row.setCellValue(1, author, author.getCBText()));
      htLabels.buildRows(curMiscFile.labels,   (row, label)  -> row.setCellValue(2, label, label.getExtendedText()));
    }
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

  @Override public void init(TabEnum tabEnum)
  {
    this.tabEnum = tabEnum;
    mainText = new MainTextWrapper(apDescription);
    tfFileName.setEditable(false);

    addShowMenuItem("Show in system explorer", event -> { if (tfFileName.getText().length() > 0) highlightFileInExplorer(curMiscFile.getPath().getFilePath()); });
    addShowMenuItem("Show in file manager", event ->    { if (tfFileName.getText().length() > 0) ui.goToFileInManager(curMiscFile.getPath().getFilePath()); });
    addShowMenuItem("Copy path to clipboard", event ->  { if (tfFileName.getText().length() > 0) copyToClipboard(curMiscFile.getPath().toString()); });

    addShowMenuItem("Unassign file", event ->
    {
      if (ui.cantSaveRecord(true)) return;
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

    htKeyMentioners.addCol(hdtNone, ctNone);
    htKeyMentioners.addCol(hdtNone, ctNone);
    htKeyMentioners.addCol(hdtNone, ctNone);

    hcbFileType = new HyperCB(cbFileType, ctDropDown, new StandardPopulator(hdtFileType), null);
    hcbWork = new HyperCB(cbWork, ctDropDownList, new StandardPopulator(hdtWork), null);

    hcbWork.getComboBox().getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      cbWorkChange();
    });

    btnWork  .setOnAction(event -> ui.goToRecord(HyperTableCell.getRecord(hcbWork.selectedHTC()), true));
    btnTree  .setOnAction(event -> ui.goToTreeRecord(curMiscFile));
    btnLaunch.setOnAction(event -> { if (tfFileName.getText().length() > 0) launchFile(curMiscFile.getPath().getFilePath()); });
    btnShow  .setOnAction(event -> { if (tfFileName.getText().length() > 0) highlightFileInExplorer(curMiscFile.getPath().getFilePath()); });

    btnManage.setTooltip(new Tooltip("Update or rename file"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    mainText.clear(true);
    tfName.setText("");
    tfFileName.setText("");
    tfSearchKey.setText("");
    checkAnnotated.setSelected(false);

    htAuthors.clear();
    htLabels.clear();
    htKeyMentioners.clear();

    hcbFileType.clear();
    hcbWork.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord(boolean showMessage)
  {
    if (!saveSearchKey(curMiscFile, tfSearchKey, showMessage)) return false;

    int fileTypeID = hcbFileType.selectedID();
    if ((fileTypeID < 1) && (hcbFileType.getText().length() == 0))
    {
      messageDialog("You must enter a file type.", mtError);
      safeFocus(this.cbFileType);
      return false;
    }

    mainText.save();

    curMiscFile.work.setID(hcbWork.selectedID());

    if (curMiscFile.work.isNull())
    {
      curMiscFile.setAuthors(htAuthors.saveToList(1, hdtPerson));
      curMiscFile.setWorkLabels(htLabels.saveToList(2, hdtWorkLabel));
    }

  // Start file type

    if ((fileTypeID < 1) && (hcbFileType.getText().length() > 0))
    {
      HDT_FileType fileType = db.createNewBlankRecord(hdtFileType);
      fileTypeID = fileType.getID();
      fileType.setName(hcbFileType.getText());
    }

    HDT_FileType oldFileType = curMiscFile.fileType.get();
    curMiscFile.fileType.setID(fileTypeID);

    if ((oldFileType != null) && oldFileType.miscFiles.isEmpty())
      db.deleteRecord(hdtFileType, oldFileType.getID());

  // End file type

    curMiscFile.setName(tfName.getText());
    curMiscFile.setAnnotated(checkAnnotated.isSelected());

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showFileDialog()
  {
    fdc = FileDialogController.create("Miscellaneous file", hdtMiscFile, curMiscFile, (HDT_Work) getHyperTab(workTab).activeRecord(), tfName.getText());

    boolean result = fdc.showModal();

    if (result)
    {
      if (curMiscFile.getPath().isEmpty())
        tfFileName.setText("");
      else
        tfFileName.setText(db.getRootFilePath().relativize(curMiscFile.getPath().getFilePath()).toString());

      tfName.setText(fdc.tfRecordName.getText());
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
    setDividerPosition(spRightVert, PREF_KEY_FILE_RIGHT_VERT, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spBottomVert, PREF_KEY_FILE_BOTTOM_VERT, 0);
    getDividerPosition(spRightHoriz, PREF_KEY_FILE_RIGHT_HORIZ, 0);
    getDividerPosition(spRightVert, PREF_KEY_FILE_RIGHT_VERT, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
