/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.query.ui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.concurrent.Worker;
import javafx.concurrent.Worker.State;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.query.*;
import org.hypernomicon.query.reports.ReportEngine;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.CheckBoxOrCommandListCell.CheckBoxOrCommand;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.DesktopUtil.*;

//---------------------------------------------------------------------------

public class QueriesTabCtrlr extends HyperTab<HDT_Record, HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final int TRUE_ID  = 1,
                          FALSE_ID = 2,
                          UNSET_ID = 3;

  public static final HyperTableCell trueCell  = new HyperTableCell(TRUE_ID , "True" , hdtNone),
                                     falseCell = new HyperTableCell(FALSE_ID, "False", hdtNone),
                                     unsetCell = new HyperTableCell(UNSET_ID, "Unset", hdtNone);

  @FXML private AnchorPane apOrigDescription;
  @FXML private Button btnClear, btnToggleFavorite;
  @FXML private CheckBox chkShowFields;
  @FXML private ComboBox<CheckBoxOrCommand> cbFile;
  @FXML private Tab tabNew;
  @FXML private TabPane tabPane;
  @FXML private TextField tfFavName;
  @FXML private WebView webView;

  @FXML Button btnExecute;
  @FXML CheckBox chkShowDesc;

  private ComboBox<CheckBoxOrCommand> fileBtn = null;
  private String textToHilite = "";
  private ObjectProperty<ObservableList<ResultsRow>> propToUnbind = null;
  private ChangeListener<ResultsRow> cbListenerToRemove = null, tvListenerToRemove = null;
  private ComboBox<ResultsRow> cb;
  private boolean clearingViews = false;

  private final BooleanProperty includeEdited = new SimpleBooleanProperty(),
                                excludeAnnots = new SimpleBooleanProperty(),
                                entirePDF     = new SimpleBooleanProperty();

  private final List<QueryCtrlr> queryCtrlrs = new ArrayList<>();

  private static final List<Query<?>> allQueries = new ArrayList<>();

  public static List<ResultsRow> results()          { return (curQC == null) ? List.of() : curQC.results(); }
  public void setTextToHilite(String text)          { textToHilite = text; }
  public void refreshTables()                       { queryCtrlrs.forEach(qc -> qc.resultsTable.getTV().refresh()); }
  public void setCB(ComboBox<ResultsRow> cb)        { this.cb = cb; updateCB(curQC); }
  public static void btnExecuteClick()              { curQC.btnExecuteClick(true); }

  @Override protected RecordType type()             { return hdtNone; }
  @Override public void update()                    { curQC.refreshView(true); }
  @Override public void setRecord(HDT_Record rec)   { if (curQC != null) curQC.setRecord(rec); }
  @Override public int recordCount()                { return results().size(); }
  @Override public TextViewInfo mainTextInfo()      { return new TextViewInfo(MainTextUtil.webEngineScrollPos(webView.getEngine())); }
  @Override public void setDividerPositions()       { return; }
  @Override public void getDividerPositions()       { return; }
  @Override public boolean saveToRecord()           { return false; }
  @Override public HDT_Record activeRecord()        { return curQC == null ? null : curQC.getRecord(); }
  @Override public HDT_Record viewRecord()          { return activeRecord(); }
  @Override public String recordName()              { return nullSwitch(activeRecord(), "", HDT_Record::getCBText); }
  @Override public int recordNdx()                  { return recordCount() > 0 ? curQC.resultsTable.getTV().getSelectionModel().getSelectedIndex() : -1; }
  @Override public void findWithinDesc(String text) { if (activeRecord() != null) MainTextWrapper.hiliteText(text, webView.getEngine()); }

  @FXML private void mnuCopyToFolderClick()         { copyFilesToFolder(true); }
  @FXML private void mnuShowSearchFolderClick()     { if (db.isLoaded()) launchFile(db.resultsPath()); }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void removeRecord(HDT_Record record)
  {
    queryCtrlrs.forEach(qc -> qc.resultsTable.getTV().getItems().removeIf(row -> row.getRecord() == record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void init()
  {
    GeneralQueries.addQueries(allQueries);
    FolderQueries .addQueries(allQueries);
    PersonQueries .addQueries(allQueries);
    WorkQueries   .addQueries(allQueries);

  //---------------------------------------------------------------------------

    btnExecute.setOnAction(event -> btnExecuteClick());
    btnClear.setOnAction(event -> curQC.resetFields());
    btnToggleFavorite.setOnAction(event -> curQC.btnFavoriteClick());

    tabPane.getTabs().addListener((Change<? extends Tab> c) -> Platform.runLater(tabPane::requestLayout));

    tabPane.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (clearingViews == false) tabPaneChange(newValue);
      Platform.runLater(tabPane::requestLayout);
    });

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      if (curQC.inReportMode())
      {
        MainTextUtil.handleJSEvent("", webView.getEngine(), new TextViewInfo());
        return;
      }

      HDT_Record record = curQC.resultsTable.selectedRecord();
      if (record == null) return;

      textToHilite = curQC.getTextToHilite();
      String mainText = "";

      if (record.hasDesc())
        mainText = ((HDT_RecordWithDescription) record).getDesc().getHtml();

      MainTextUtil.handleJSEvent(MainTextUtil.prepHtmlForDisplay(mainText), webView.getEngine(), new TextViewInfo());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.getEngine().getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
    {
      if (newState == Worker.State.SUCCEEDED)
      {
        if (textToHilite.length() > 0)
          MainTextWrapper.hiliteText(textToHilite, webView.getEngine());

        textToHilite = "";
      }
    });

    MainTextUtil.webViewAddZoom(webView, PREF_KEY_QUERYTAB_ZOOM);

    tfFavName.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || newValue.equals(safeStr(oldValue)) || (curQC == null)) return;

      curQC.favNameChange();
    });

    addFilesButton();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addFilesButton()
  {
    ObservableList<Node> children = ((AnchorPane) getTab().getContent()).getChildren();
    ObservableList<CheckBoxOrCommand> items;

    if (fileBtn != null)
    {
      items = fileBtn.getItems();
      fileBtn.setItems(null);
      children.remove(fileBtn);
    }
    else
    {
      items = FXCollections.observableArrayList(

        new CheckBoxOrCommand("Include edited works", includeEdited),
        new CheckBoxOrCommand("Copy files without annotations", excludeAnnots),
        new CheckBoxOrCommand("Always copy entire PDF file", entirePDF),
        new CheckBoxOrCommand("Clear Search Results Folder and Add All Results", () -> { mnuCopyAllClick          (); fileBtn.hide(); }),
        new CheckBoxOrCommand("Clear Search Results Folder",                     () -> { mnuClearSearchFolderClick(); fileBtn.hide(); }),
        new CheckBoxOrCommand("Copy Selected to Search Results Folder",          () -> { mnuCopyToFolderClick     (); fileBtn.hide(); }),
        new CheckBoxOrCommand("Show Search Results Folder",                      () -> { mnuShowSearchFolderClick (); fileBtn.hide(); }));

      cbFile.setVisible(false);
    }

    fileBtn = CheckBoxOrCommand.createComboBox(items, "Files");

    copyRegionLayout(cbFile, fileBtn);

    children.add(children.indexOf(cbFile), fileBtn);

    fileBtn.setOnHidden(event -> addFilesButton()); // This is necessary to deselect list item without making the button caption disappear
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void tabPaneChange(Tab newValue)
  {
    QueryCtrlr qC;

    if (newValue == tabNew)
    {
      qC = addQueryCtrlr();
      tabPane.getSelectionModel().select(qC.getTab());
      qC.focusOnFields();
    }
    else
    {
      qC = findFirst(queryCtrlrs, view -> view.getTab() == newValue);
      if (qC == null) return;
    }

    if (curQC != null)
      curQC.deactivate();

    qC.activate(chkShowFields, chkShowDesc);

    curQC = qC;
    updateCB(curQC);

    qC.refreshView(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void deleteView(Tab tab)
  {
    QueryCtrlr qC = findFirst(queryCtrlrs, view -> view.getTab() == tab);
    if (qC == null) return;

    qC.saveColumnWidths();
    queryCtrlrs.remove(qC);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private QueryCtrlr addQueryCtrlr()
  {
    QueryCtrlr newQC = new QueryCtrlr(this, webView, tabPane, tfFavName);

    queryCtrlrs.add(newQC);
    newQC.resetFields();

    return newQC;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    clearingViews = true;

    if (curQC != null)
      curQC.deactivate();

    removeFromParent(webView);
    addToParent(webView, apOrigDescription);

    queryCtrlrs.removeIf(queryCtrlr ->
    {
      queryCtrlr.saveColumnWidths();
      tabPane.getTabs().remove(queryCtrlr.getTab());
      return true;
    });

    clearingViews = false;

    if (ui.isShuttingDown() == false)
      webView.getEngine().loadContent("");

    QueryCtrlr newQC = addQueryCtrlr();
    tabPane.getSelectionModel().select(newQC.getTab());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueriesToPopulator(QueryPopulator pop, HyperTableRow row, QueryType newType)
  {
    if (newType == qtReport)
    {
      ReportEngine.addQueries(pop, row);
      return;
    }

    allQueries.stream().filter(query -> query.show(newType, newType.getRecordType())).forEach(query -> pop.addQuery(row, query));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite fav, HyperTableCell op1, HyperTableCell op2, String caption)
  {
    if ((type != qtReport) && (db.isLoaded() == false)) return false;

    QueryCtrlr qC = addQueryCtrlr();
    tabPane.getSelectionModel().select(qC.getTab());

    return qC.showSearch(doSearch, type, query, fav, op1, op2, caption);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuCopyAllClick()
  {
    boolean startWatcher = folderTreeWatcher.stop();

    mnuClearSearchFolderClick();

    if (copyFilesToFolder(false))
      mnuShowSearchFolderClick();

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean copyFilesToFolder(boolean onlySelected)
  {
    SearchResultFileList fileList = new SearchResultFileList(entirePDF.get(), includeEdited.get());

    if ((db.isLoaded() == false) || (results().size() < 1)) return false;

    HyperTask task = new HyperTask("BuildListOfFilesToCopy") { @Override protected void call() throws CancelledTaskException
    {
      updateMessage("Building list...");

      updateProgress(0, 1);

      List<ResultsRow> resultRowList = onlySelected ? curQC.resultsTable.getTV().getSelectionModel().getSelectedItems() : results();

      int ndx = 0; for (ResultsRow row : resultRowList)
      {
        HDT_Record record = row.getRecord();
        if (record instanceof HDT_RecordWithPath)
          fileList.addRecord((HDT_RecordWithPath)record);

        if (isCancelled())
          throw new CancelledTaskException();

        updateProgress(ndx++, resultRowList.size());
      }
    }};

    if (task.runWithProgressDialog() != State.SUCCEEDED)
      return false;

    boolean startWatcher = folderTreeWatcher.stop();

    task = new HyperTask("CopyingFiles") { @Override protected void call() throws CancelledTaskException
    {
      updateMessage("Copying files...");

      updateProgress(0, 1);

      fileList.copyAll(excludeAnnots.getValue(), this);
    }};

    task.runWithProgressDialog();

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();

    fileList.showErrors();

    return true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @FXML private void mnuClearSearchFolderClick()
  {
    if (db.isLoaded() == false) return;

    HDT_Folder resultsFolder = db.getResultsFolder();

    if ((resultsFolder.getPath().getRecordsString().length() > 0) ||
        resultsFolder.childFolders.stream().anyMatch(childFolder -> childFolder.isSpecial(true)))
    {
      messageDialog("One or more file(s)/folder(s) in the search results folder are in use by the database.", mtError);
      return;
    }

    boolean startWatcher = folderTreeWatcher.stop();

    try
    {
      FileUtils.cleanDirectory(db.resultsPath().toFile());

      fileManagerDlg.pruneAndRefresh();
    }
    catch (IOException e) { messageDialog("One or more files were not deleted. Reason: " + e.getMessage(), mtError); }

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void closeCurrentView()
  {
    int ndx = tabPane.getSelectionModel().getSelectedIndex(), nextNdx = ndx + 1;
    deleteView(tabPane.getSelectionModel().getSelectedItem());

    if ((nextNdx + 1) == tabPane.getTabs().size())
      nextNdx = ndx - 1;

    tabPane.getSelectionModel().select(nextNdx);

    tabPane.getTabs().remove(ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void enable(boolean enabled)
  {
    ((Parent)getTab().getContent()).getChildrenUnmodifiable().stream().filter(node -> node != tabPane)
                                                                      .forEach(node -> node.setDisable(enabled == false));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void updateCB(QueryCtrlr queryCtrlr)
  {
    if ((cb == null) || (queryCtrlr == null)) return;

    TableView<ResultsRow> tvResults = queryCtrlr.resultsTable.getTV();

    if (propToUnbind != null)
    {
      cb.itemsProperty().unbindBidirectional(propToUnbind);
      propToUnbind = null;
    }

    if (tvListenerToRemove != null)
    {
      tvResults.getSelectionModel().selectedItemProperty().removeListener(tvListenerToRemove);
      tvListenerToRemove = null;
    }

    if (cbListenerToRemove != null)
    {
      cb.getSelectionModel().selectedItemProperty().removeListener(cbListenerToRemove);
      cbListenerToRemove = null;
    }

    if (queryCtrlr.inReportMode())
    {
      cb.setItems(null);
      return;
    }

    propToUnbind = tvResults.itemsProperty();
    cb.itemsProperty().bindBidirectional(propToUnbind);

    cb.getSelectionModel().select(tvResults.getSelectionModel().getSelectedItem());

    cbListenerToRemove = (ob, oldValue, newValue) ->
    {
      if (alreadySettingSelection || (newValue == null) || (newValue.getRecord() == null)) return;

      alreadySettingSelection = true;

      tvResults.getSelectionModel().clearSelection();
      tvResults.getSelectionModel().select(newValue);
      HyperTable.scrollToSelection(tvResults, false);

      alreadySettingSelection = false;
    };

    cb.getSelectionModel().selectedItemProperty().addListener(cbListenerToRemove);

    tvListenerToRemove = (ob, oldValue, newValue) ->
    {
      if (alreadySettingSelection) return;

      alreadySettingSelection = true;
      cb.getSelectionModel().select(newValue);
      alreadySettingSelection = false;
    };

    tvResults.getSelectionModel().selectedItemProperty().addListener(tvListenerToRemove);
  }

  private boolean alreadySettingSelection = false;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void setFavNameToggle(boolean selected)
  {
    btnToggleFavorite.setText(selected ? "Remove from favorites" : "Add to favorites");
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
