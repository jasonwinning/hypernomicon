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

package org.hypernomicon.query.ui;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;

import javafx.application.Platform;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.concurrent.Worker;
import javafx.concurrent.Worker.State;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.Parent;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.query.*;
import org.hypernomicon.query.reports.ReportEngine;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.controls.CheckBoxMenuItem;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.mainText.Highlighter;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.wrappers.*;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.DesktopUtil.*;

//---------------------------------------------------------------------------

public class QueriesTabCtrlr extends HyperTab<HDT_Record, HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apOrigDescription;
  @FXML private Button btnToggleFavorite, btnFileActionsHelp;
  @FXML private MenuButton btnFileActions;
  @FXML private MenuItem mnuClear, mnuClearAndAdd, mnuAddSelected, mnuShowInSysExplorer, mnuShowInFileMgr;
  @FXML private Tab tabNew;
  @FXML private TabPane tabPane;
  @FXML private TextField tfFavName;
  @FXML private WebView webView;

  @FXML Button btnExecute;

  private CheckBoxMenuItem mnuIncludeEdited, mnuExcludeAnnots, mnuEntirePDF;

  private Property<ObservableList<ResultRow>> propToUnbind = null;
  private ChangeListener<ResultRow> cbListenerToRemove = null, tvListenerToRemove = null;
  private ComboBox<ResultRow> cb;
  private boolean clearingViews = false;

  private final List<QueryCtrlr> queryCtrlrs = new ArrayList<>();
  private final Highlighter highlighter;

  private QueryCtrlr curQueryCtrlr;

  private static final List<Query<?>> allQueries = new ArrayList<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean inReportMode()                     { return (curQueryCtrlr != null) && curQueryCtrlr.inReportMode(); }

  public List<ResultRow> results()                   { return curQueryCtrlr == null ? List.of() : curQueryCtrlr.results(); }
  public void refreshTables()                        { queryCtrlrs.forEach(queryCtrlr -> queryCtrlr.getResultsTV().refresh()); }
  public void setCB(ComboBox<ResultRow> cb)          { this.cb = cb; updateCB(curQueryCtrlr); }
  public void btnExecuteClick()                      { curQueryCtrlr.btnExecuteClick(true); }
  public QueryCtrlr getCurQueryCtrlr()               { return curQueryCtrlr; }

  @Override protected RecordType type()              { return hdtNone; }
  @Override protected void setRecord(HDT_Record rec) { if (curQueryCtrlr != null) curQueryCtrlr.setRecord(rec); }
  @Override protected void updateFromRecord()        { curQueryCtrlr.refreshView(true); }

  @Override public int recordCount()                 { return results().size(); }
  @Override public void setDividerPositions()        { return; }
  @Override public void getDividerPositions()        { return; }
  @Override public boolean saveToRecord()            { return false; }
  @Override public HDT_Record activeRecord()         { return curQueryCtrlr == null ? null : curQueryCtrlr.getRecord(); }
  @Override public HDT_Record viewRecord()           { return activeRecord(); }
  @Override public String recordName()               { return nullSwitch(activeRecord(), "", HDT_Record::getCBText); }
  @Override public int recordNdx()                   { return recordCount() > 0 ? curQueryCtrlr.getResultsTV().getSelectionModel().getSelectedIndex() : -1; }
  @Override public void findWithinDesc()             { if ((activeRecord() != null) || inReportMode()) highlighter.hilite(); }
  @Override public void nextSearchResult()           { highlighter.nextSearchResult    (); }
  @Override public void previousSearchResult()       { highlighter.previousSearchResult(); }

  @FXML private void mnuCopyToFolderClick()          { copyFilesToFolder(true); }

  @Override public TextViewInfo mainTextInfo(HDT_Record record) { return new TextViewInfo(record, MainTextUtil.webEngineScrollPos(webView.getEngine())); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueriesTabCtrlr(Tab tab) throws IOException
  {
    super(TabEnum.queryTabEnum, tab, "query/QueriesTab");

    GeneralQueries .addQueries(allQueries);
    FolderQueries  .addQueries(allQueries);
    PersonQueries  .addQueries(allQueries);
    ArgumentQueries.addQueries(allQueries);
    WorkQueries    .addQueries(allQueries);

//---------------------------------------------------------------------------

    btnExecute.setOnAction(event -> btnExecuteClick());
    btnToggleFavorite.setOnAction(event -> curQueryCtrlr.btnFavoriteClick());

    tabPane.getTabs().addListener((Change<? extends Tab> c) -> Platform.runLater(tabPane::requestLayout));

    tabPane.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (clearingViews == false) tabPaneChange(newValue);
      Platform.runLater(tabPane::requestLayout);
    });

    highlighter = new Highlighter(webView);

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      if (curQueryCtrlr.inReportMode())
      {
        MainTextUtil.handleJSEvent("", webView.getEngine());
        return;
      }

      HDT_Record record = activeRecord();
      if (record == null) return;

      MainTextUtil.handleJSEvent(MainTextUtil.prepHtmlForDisplay(HDT_Record.getDescHtml(record)), webView.getEngine());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.getEngine().getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
    {
      if (newState == Worker.State.SUCCEEDED)
      {
        if ((curQueryCtrlr.getRecordToHilite() != null) && ui.currentFindInDescriptionText().isBlank())
          highlighter.hiliteAlreadyTagged();
        else
          highlighter.hilite(true);
      }
    });

    webView.setOnDragOver(Event::consume);
    webView.setOnDragDropped(Event::consume);

    MainTextUtil.webViewAddZoom(webView, PREF_KEY_QUERYTAB_ZOOM);

    tfFavName.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || newValue.equals(safeStr(oldValue)) || (curQueryCtrlr == null)) return;

      curQueryCtrlr.favNameChange();
    });

    initFileActions();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initFileActions()
  {
    mnuClear            .setOnAction(event -> mnuClearSearchFolderClick());
    mnuClearAndAdd      .setOnAction(event -> mnuCopyAllClick          ());
    mnuAddSelected      .setOnAction(event -> mnuCopyToFolderClick     ());

    mnuShowInSysExplorer.setOnAction(event -> mnuShowSearchFolderClick (false));
    mnuShowInFileMgr    .setOnAction(event -> mnuShowSearchFolderClick (true ));

    mnuIncludeEdited = new CheckBoxMenuItem("Include edited works"          , btnFileActions.showingProperty());
    mnuExcludeAnnots = new CheckBoxMenuItem("Copy files without annotations", btnFileActions.showingProperty());
    mnuEntirePDF     = new CheckBoxMenuItem("Always copy entire PDF file"   , btnFileActions.showingProperty());

    btnFileActions.getItems().addAll(List.of(mnuIncludeEdited, mnuExcludeAnnots, mnuEntirePDF));

    btnFileActions.setTooltip(new WebTooltip("""
      <html lang="en">
      <head>
        <style>
          .topic      { color: #4682B4; }
          .recname    { color: #FF6347; }
          .large-text { font-size: 1.3em; font-weight: normal; }
        </style>
      </head>
      <body>
        <p>The File Actions menu allows you to copy files associated with the results of your queries to the<br/>
           Search Results database folder. Then you can easily perform actions on the subset of files you are<br/>
           interested in, for example full-text search, combining PDFs into a single PDF or zip file, sending<br/>
           the files to colleagues, or making the files available to students as course readings.</p>
        <p>Below is an overview of each menu option:</p>
        <h4 class="topic">File Actions</h4>
        <ul>
          <li><strong>Clear Search Results Folder and Add All Results:</strong> This option clears the current contents<br/>
                      of the Search Results folder and then copies all files associated with the query results to the<br/>
                      folder.</li>
          <li><strong>Clear Search Results Folder:</strong> Use this option to clear all files currently in the Search<br/>
                      Results folder without adding any new files.</li>
          <li><strong>Copy Selected to Search Results Folder:</strong> This option copies the files associated with the<br/>
                      selected query results to the Search Results folder.</li>
          <li><strong>Show Search Results Folder in System Explorer:</strong> Opens the Search Results folder in your<br/>
                      system's file explorer (e.g., Finder on macOS).</li>
          <li><strong>Show Search Results Folder in File Manager:</strong> Opens the Search Results folder in the<br/>
                      Hypernomicon File Manager.</li>
        </ul>
        <h4 class="topic">Options</h4>
        <ul>
          <li><strong>Include Edited Works:</strong> By default, files associated with edited works are not copied. Check<br/>
                      this option to include them in the copying process.</li>
          <li><strong>Copy Files Without Annotations:</strong> Select this option if you want to copy PDF files without<br/>
                      any annotations or highlights, useful for sharing clean versions of the documents.</li>
          <li><strong>Always Copy Entire PDF File:</strong> Check this option to ensure the entire PDF file is copied. By<br/>
                      default, only the specific pages related to the query result are copied.</li>
        </ul>
      </body>
      </html>
      """));

    WebTooltip.setupClickHandler(btnFileActionsHelp, btnFileActions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuShowSearchFolderClick(boolean inFileMgr)
  {
    if (db.isLoaded() == false) return;

    if (inFileMgr)
      ui.goToFileInManager(db.resultsPath());
    else
      launchFile(db.resultsPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeRecord(HDT_Record record)
  {
    queryCtrlrs.forEach(queryCtrlr -> queryCtrlr.getResultsTV().getItems().removeIf(row -> row.getRecord() == record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void tabPaneChange(Tab newValue)
  {
    QueryCtrlr queryCtrlr;

    if (newValue == tabNew)
    {
      queryCtrlr = addQueryCtrlr();
      tabPane.getSelectionModel().select(queryCtrlr.getTab());
      queryCtrlr.focusOnFields();
    }
    else
    {
      queryCtrlr = findFirst(queryCtrlrs, view -> view.getTab() == newValue);
      if (queryCtrlr == null) return;
    }

    if (curQueryCtrlr != null)
      curQueryCtrlr.deactivate();

    queryCtrlr.activate();

    curQueryCtrlr = queryCtrlr;
    updateCB(curQueryCtrlr);

    queryCtrlr.refreshView(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void deleteView(Tab tab)
  {
    QueryCtrlr queryCtrlr = findFirst(queryCtrlrs, view -> view.getTab() == tab);
    if (queryCtrlr == null) return;

    queryCtrlr.saveColumnWidths();
    queryCtrlrs.remove(queryCtrlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private QueryCtrlr addQueryCtrlr()
  {
    QueryCtrlr queryCtrlr = new QueryCtrlr(this, webView, tabPane, tfFavName);

    queryCtrlrs.add(queryCtrlr);
    queryCtrlr.resetFields();

    return queryCtrlr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    clearingViews = true;

    if (curQueryCtrlr != null)
      curQueryCtrlr.deactivate();

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
    {
      webView.getEngine().loadContent("");

      QueryCtrlr newQueryCtrlr = addQueryCtrlr();
      tabPane.getSelectionModel().select(newQueryCtrlr.getTab());
    }
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

  /**
   * Executes a search based on the provided parameters.
   *
   * @param doSearch If true, performs the search; if false, skips the search, invokes the favorite if one was passed in, and returns false.
   * @param type The type of query to be executed. Either this parameter should be null or fav should be.
   * @param query The query ID to be executed. Pass in -1 if fav is non-null.
   * @param fav The favorite query to be invoked, if any. Should be left null if type and query parameters are used.
   * @param op1 The first operand for the query.
   * @param op2 The second operand for the query.
   * @param caption The caption to set for the query sub-tab within the Queries tab.
   * @return True if the query ran successfully, regardless of whether there were any results;
   * false if the query did not run, encountered an error during execution, or was cancelled by the user.
   */
  public boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite fav, HyperTableCell op1, HyperTableCell op2, String caption)
  {
    if ((type != qtReport) && (db.isLoaded() == false)) return false;

    QueryCtrlr queryCtrlr = addQueryCtrlr();
    tabPane.getSelectionModel().select(queryCtrlr.getTab());

    return queryCtrlr.showSearch(doSearch, type, query, fav, op1, op2, caption);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuCopyAllClick()
  {
    boolean startWatcher = folderTreeWatcher.stop();

    mnuClearSearchFolderClick();

    if (copyFilesToFolder(false))
      mnuShowSearchFolderClick(false);

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean copyFilesToFolder(boolean onlySelected)
  {
    SearchResultFileList fileList = new SearchResultFileList(mnuEntirePDF.isSelected(), mnuIncludeEdited.isSelected());

    if ((db.isLoaded() == false) || results().isEmpty()) return false;

    if (new HyperTask("BuildListOfFilesToCopy", "Building list...") { @Override protected void call() throws CancelledTaskException
    {
      List<ResultRow> resultRowList = onlySelected ? curQueryCtrlr.getResultsTV().getSelectionModel().getSelectedItems() : results();

      totalCount = resultRowList.size();

      for (ResultRow row : resultRowList)
      {
        HDT_Record record = row.getRecord();
        if (record instanceof HDT_RecordWithPath recordWithPath)
          fileList.addRecord(recordWithPath);

        incrementAndUpdateProgress();
      }

    }}.runWithProgressDialog() != State.SUCCEEDED) return false;

    boolean startWatcher = folderTreeWatcher.stop();

    fileList.newCopyAllTask(mnuExcludeAnnots.isSelected()).runWithProgressDialog();

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
      errorPopup("One or more file(s)/folder(s) in the search results folder are in use by the database.");
      return;
    }

    boolean startWatcher = folderTreeWatcher.stop();

    try
    {
      FileUtils.cleanDirectory(db.resultsPath().toFile());

      fileManagerDlg.pruneAndRefresh();
    }
    catch (IOException e) { errorPopup("One or more files were not deleted. Reason: " + getThrowableMessage(e)); }

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

    TableView<ResultRow> tvResults = queryCtrlr.getResultsTV();

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

      Platform.runLater(() ->
      {
        tvResults.getSelectionModel().clearSelection();
        tvResults.getSelectionModel().select(newValue);

        HyperTable.scrollToSelection(tvResults, false);

        alreadySettingSelection = false;
      });
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

  private ImageView ivStar, ivStarEmpty;

  void setFavNameToggle(boolean selected)
  {
    // Things are done in a certain way in this function to avoid problems with
    // the query control refreshing after running a query.

    if (ivStar == null)
    {
      ivStar      = imgViewFromRelPath("resources/images/star.png");
      ivStarEmpty = imgViewFromRelPath("resources/images/star-empty.png");
    }

    if (selected)
    {
      btnToggleFavorite.setText("Remove from favorites");

      if (btnToggleFavorite.getGraphic() != ivStar)
        btnToggleFavorite.setGraphic(ivStar);
    }
    else
    {
      btnToggleFavorite.setText("Add to favorites");

      if (btnToggleFavorite.getGraphic() != ivStarEmpty)
        btnToggleFavorite.setGraphic(ivStarEmpty);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
