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

package org.hypernomicon.bib;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static java.util.Objects.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.google.common.collect.EnumHashBiMap;

import org.hypernomicon.bib.CollectionTree.BibCollectionType;
import org.hypernomicon.bib.LibraryWrapper.SyncTask;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.HyperDlg;
import org.hypernomicon.view.dialogs.SelectWorkDlgCtrlr;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.view.workMerge.MergeWorksDlgCtrlr;

import javafx.animation.Animation;
import javafx.animation.Interpolator;
import javafx.animation.RotateTransition;
import javafx.animation.SequentialTransition;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.concurrent.Worker.State;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableView;
import javafx.scene.control.ToolBar;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import javafx.scene.web.WebView;
import javafx.stage.Modality;
import javafx.stage.StageStyle;
import javafx.util.Duration;
import javafx.util.StringConverter;

public class BibManager extends HyperDlg
{
  @FXML private Button btnCreateNew, btnDelete, btnMainWindow, btnPreviewWindow, btnSelect, btnStop, btnSync;
  @FXML private ComboBox<EntryType> cbNewType;
  @FXML private Label lblSelect;
  @FXML private SplitPane spMain;
  @FXML private TableView<BibEntryRow> tableView;
  @FXML private ToolBar toolBar;
  @FXML private TreeView<BibCollectionRow> treeView;
  @FXML private WebView webView;

  private static final String dialogTitle = "Bibliographic Entry Manager";

  private BibEntryTable entryTable;
  private CollectionTree collTree;
  private LibraryWrapper<? extends BibEntry, ? extends BibCollection> libraryWrapper = null;
  private SyncTask syncTask = null;

  public final ObjectProperty<HDT_Work> workRecordToAssign = new SimpleObjectProperty<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveToDisk() { libraryWrapper.saveToDisk(db.xmlPath(BIB_FILE_NAME)); }

  @Override public boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setLibrary(LibraryWrapper<? extends BibEntry, ? extends BibCollection> libraryWrapper)
  {
    this.libraryWrapper = libraryWrapper;

    if (libraryWrapper != null)
      initCB(cbNewType);

    if (shownAlready() == false) return;

    collTree.clear();
    entryTable.clear();

    if (libraryWrapper == null) return;

    collTree.rebuild(libraryWrapper.getKeyToColl());
    treeView.getSelectionModel().clearAndSelect(0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibManager create()
  {
    BibManager bibManager = HyperDlg.createUsingFullPath("bib/BibManager.fxml", dialogTitle, true, StageStyle.DECORATED, Modality.NONE);
    bibManager.init();
    return bibManager;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void sync()
  {
    if (ui.cantSaveRecord()) return;

    if (btnStop.isDisabled() == false) return;

    RotateTransition rt1 = new RotateTransition(Duration.millis(1000), btnSync.getGraphic());
    rt1.setByAngle(360);
    rt1.setCycleCount(1);
    rt1.setInterpolator(Interpolator.EASE_IN);

    RotateTransition rt2 = new RotateTransition(Duration.millis(1000), btnSync.getGraphic());
    rt2.setByAngle(360);
    rt2.setCycleCount(Animation.INDEFINITE);
    rt2.setInterpolator(Interpolator.LINEAR);

    SequentialTransition seqT = new SequentialTransition(rt1, rt2);
    seqT.play();

    btnStop.setDisable(false);

    libraryWrapper.setKeyChangeHandler(entryTable::updateKey);

    syncTask = libraryWrapper.createNewSyncTask();

    syncTask.runningProperty().addListener((ob, wasRunning, isRunning) ->
    {
      if ((wasRunning == false) || isRunning) return;

      btnStop.setDisable(true);
      seqT.stop();

      ImageView iv1 = getImageViewForRelativePath("resources/images/refresh.png");
      iv1.setFitWidth(16);
      iv1.setFitHeight(16);
      btnSync.setGraphic(iv1);

      if ((syncTask.getState() == State.FAILED) || (syncTask.getState() == State.CANCELLED))
      {
        Throwable ex = syncTask.getException();

        if (ex instanceof HyperDataException)
          messageDialog(ex.getMessage(), mtError);
      }

      boolean changed = syncTask.getChanged();
      syncTask = null;

      if (changed == false) return;

      collTree.refresh(libraryWrapper.getKeyToColl());
      refresh();

      ui.update();
      ui.saveAllToDisk(true, true, true);
    });

    Thread thread = new Thread(syncTask);
    thread.setDaemon(true);
    syncTask.setThread(thread);
    thread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void hideBottomControls()
  {
    setAllVisible(false, lblSelect, btnSelect, btnCreateNew, cbNewType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    entryTable = new BibEntryTable(tableView, PREF_KEY_HT_BIB_ENTRIES);
    collTree = new CollectionTree(treeView);

    toolBar.getItems().remove(btnDelete);         // These are not
    toolBar.getItems().remove(btnPreviewWindow);  // yet supported. (Not sure if the preview button is needed?)

    btnMainWindow.setOnAction(event -> ui.windows.focusStage(app.getPrimaryStage()));
    btnSync.setOnAction(event -> sync());

    btnStop.setOnAction(event ->
    {
      if (syncTask == null)
        messageDialog("Internal error #43949", mtError);
      else
        syncTask.cancel();

      libraryWrapper.stop();
    });

    workRecordToAssign.addListener((ob, oldValue, newValue) ->
    {
      if (newValue != oldValue)
      {
        cbNewType.getSelectionModel().clearSelection();

        if (newValue != null)
        {
          switch (newValue.getWorkTypeValue())
          {
            case wtBook    : cbNewType.getSelectionModel().select(EntryType.etBook          ); break;
            case wtChapter : cbNewType.getSelectionModel().select(EntryType.etBookChapter   ); break;
            case wtPaper   : cbNewType.getSelectionModel().select(EntryType.etJournalArticle); break;
            case wtWebPage : cbNewType.getSelectionModel().select(EntryType.etWebPage       ); break;

            default        : break;
          }
        }
      }

      if ((newValue != null) && (newValue.getBibEntryKey().length() == 0))
      {
        lblSelect.setText("Assigning to work record: " + newValue.getCBText());
        setAllVisible(true, lblSelect, btnSelect, btnCreateNew, cbNewType);
        return;
      }

      hideBottomControls();
    });

    btnSelect.setDisable(true);

    btnSelect.setOnAction(event -> btnSelectClick());
    btnCreateNew.setOnAction(event -> btnCreateNewClick());

    tableView.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) -> refresh());

    entryTable.addContextMenuItem("View this entry on the web", row -> row.getURL().length() > 0, row -> openWebLink(row.getURL()));

    entryTable.addContextMenuItem("Go to work record", HDT_Work.class, work -> ui.goToRecord(work, true));

    entryTable.addContextMenuItem("Unassign work record", row -> nonNull(row.getWork()), row ->
    {
      if (confirmDialog("Are you sure you want to unassign the work record?") == false) return;
      if (ui.cantSaveRecord()) return;

      HDT_Work work = row.getWork();

      row.getEntry().unassignWork();

      if (workRecordToAssign.get() == work)
      {
        workRecordToAssign.set(null);
        workRecordToAssign.set(work);
      }

      refresh();
      ui.update();
    });

    entryTable.addContextMenuItem("Create new work record for this entry", row -> isNull(row.getWork()), row ->
    {
      if (ui.cantSaveRecord()) return;

      HDT_Work work = db.createNewBlankRecord(hdtWork);

      work.getBibData().copyAllFieldsFrom(row.getEntry(), false, false);
      work.getAuthors().setAll(row.getEntry().getAuthors());
      work.setBibEntryKey(row.getEntry().getEntryKey());

      ui.goToRecord(work, false);
    });

    entryTable.addContextMenuItem("Assign this entry to an existing work", row -> isNull(row.getWork()), row ->
    {
      if (ui.cantSaveRecord()) return;

      HDT_Person person = findFirstHaving(row.getEntry().getAuthors(), bibAuthor -> HDT_Person.lookUpByName(bibAuthor.getName()));

      SelectWorkDlgCtrlr dlg = SelectWorkDlgCtrlr.create("Select a work record", person);

      if (dlg.showModal())
        assignEntryToWork(dlg.getWork(), row.getEntry());
    });

    entryTable.addContextMenuItem("Launch work file", HDT_Work.class, HDT_Work::canLaunch, work -> work.launch(-1));

    entryTable.addContextMenuItem("Show in Preview Window", HDT_Work.class, HDT_Work::canLaunch, work ->
    {
      PreviewSource src = ui.determinePreviewContext();
      previewWindow.setPreview(src, work.filePath(), work.getStartPageNum(), work.getEndPageNum(), work);
      ui.openPreviewWindow(src);
    });

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      MainTextWrapper.handleJSEvent("", webView.getEngine(), new TextViewInfo());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    MainTextWrapper.webViewAddZoom(webView, PREF_KEY_BIBMGR_ZOOM);

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    onShown = () ->
    {
      if (shownAlready() == false)
      {
        hideBottomControls();

        collTree.clear();
        entryTable.clear();

        collTree.rebuild(libraryWrapper.getKeyToColl());

        treeView.getSelectionModel().clearAndSelect(0);

        setDividerPositions();
      }
      else
        refresh();

      ui.windows.push(dialogStage);
    };

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    treeView.getSelectionModel().selectedItemProperty().addListener((ob, oldTreeItem, newTreeItem) ->
    {
      if (newTreeItem == null)
      {
        entryTable.clear();
        return;
      }

      if (newTreeItem == oldTreeItem)
        return;

      entryTable.update(getViewForTreeItem(newTreeItem));
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    dialogStage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus()) return;

      if ((newValue == null) || (newValue == false)) return;

      ui.windows.push(dialogStage);

      refresh();
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    dialogStage.setOnHidden(event -> ui.windows.focusStage(app.getPrimaryStage()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateRightPane()
  {
    if (libraryWrapper == null)
    {
      webView.getEngine().loadContent("");
      return;
    }

    webView.getEngine().loadContent(libraryWrapper.getHtml(tableView.getSelectionModel().getSelectedItem()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnCreateNewClick()
  {
    EntryType et = cbNewType.getValue();
    if (et == null)
    {
      falseWithWarningMessage("You must select an entry type.", cbNewType);
      return;
    }

    HDT_Work work = workRecordToAssign.get();
    BibEntry entry = libraryWrapper.addEntry(et);

    work.setBibEntryKey(entry.getEntryKey());

    workRecordToAssign.set(null);

    refresh();

    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnSelectClick()
  {
    if (ui.cantSaveRecord()) return;

    HDT_Work work = workRecordToAssign.get();
    BibEntry entry = tableView.getSelectionModel().getSelectedItem().getEntry();

    assignEntryToWork(work, entry);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void assignEntryToWork(HDT_Work work, BibEntry entry)
  {
    if ((work == null) || (entry == null)) return;

    MergeWorksDlgCtrlr mwd = null;

    try
    {
      mwd = MergeWorksDlgCtrlr.create("Import Into Existing Work Record", work.getBibData(), entry, null, null, work, false, false);
    }
    catch (IOException e)
    {
      messageDialog("Unable to initialize merge dialog window.", mtError);
      return;
    }

    if (mwd.showModal() == false) return;

    work.setBibEntryKey(entry.getEntryKey());
    mwd.mergeInto(entry);

    workRecordToAssign.set(null);
    refresh();

    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Set<? extends BibEntry> getViewForTreeItem(TreeItem<BibCollectionRow> item)
  {
    BibCollectionRow row = nullSwitch(item, null, TreeItem::getValue);
    BibCollectionType type = nullSwitch(row, null, BibCollectionRow::getType);

    if (type == null) return Collections.emptySet();

    switch (type)
    {
      case bctAll   : return libraryWrapper.getNonTrashEntries();
      case bctTrash : return libraryWrapper.getTrash();
      case bctUser  : return libraryWrapper.getCollectionEntries(row.getKey());
      default       : return libraryWrapper.getUnsorted();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToWork(HDT_Work work)
  {
    workRecordToAssign.set(work);

    String key = work.getBibEntryKey();

    if (key.length() == 0) return;

    if (entryTable.containsKey(key))
    {
      entryTable.selectKey(key);
      return;
    }

    if (libraryWrapper.getTrash().contains(libraryWrapper.getEntryByKey(key)))
      collTree.selectTrash();
    else
      collTree.selectAllEntries();

    entryTable.selectKey(key);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refresh()
  {
    entryTable.refresh(getViewForTreeItem(treeView.getSelectionModel().getSelectedItem()));

    tableView.refresh();
    updateRightPane();

    BibEntryRow row = tableView.getSelectionModel().getSelectedItem();

    btnSelect.setDisable((row == null) || (row.getWork() != null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void initCB(ComboBox<EntryType> cb)
  {
    EnumHashBiMap<EntryType, String> map = libraryWrapper.getEntryTypeMap();

    cb.setConverter(new StringConverter<>()
    {
      @Override public String toString(EntryType et)
      {
        if (map.containsKey(et))
          return et.getUserFriendlyName();

        return "";
      }

      @Override public EntryType fromString(String string)
      {
        EntryType et = EntryType.parse(string);
        if (map.containsKey(et))
          return et;

        return null;
      }
    });

    List<EntryType> choices = new ArrayList<>(map.keySet());

    choices.sort((et1, et2) -> et1.getUserFriendlyName().compareTo(et2.getUserFriendlyName()));

    cb.setItems(null);
    cb.setItems(FXCollections.observableList(choices));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setDividerPositions()
  {
    setDividerPosition(spMain, PREF_KEY_BIB_LEFT_HORIZ, 0);
    setDividerPosition(spMain, PREF_KEY_BIB_RIGHT_HORIZ, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getDividerPositions()
  {
    if (shownAlready() == false) return;

    getDividerPosition(spMain, PREF_KEY_BIB_LEFT_HORIZ, 0);
    getDividerPosition(spMain, PREF_KEY_BIB_RIGHT_HORIZ, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
