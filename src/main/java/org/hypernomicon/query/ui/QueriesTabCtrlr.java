/*
 * Copyright 2015-2026 Jason Winning
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
import java.util.*;

import org.hypernomicon.util.file.deletion.FileDeletion;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult;

import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.property.Property;
import javafx.beans.value.ChangeListener;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.concurrent.Worker;
import javafx.concurrent.Worker.State;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.geometry.Bounds;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.VBox;
import javafx.scene.web.WebView;
import javafx.stage.Popup;
import javafx.util.Duration;

import org.hypernomicon.HyperTask;
import org.hypernomicon.fileManager.FileManager;
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
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.wrappers.*;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

//---------------------------------------------------------------------------

public class QueriesTabCtrlr extends HyperTab<HDT_Record, HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apOrigDescription;
  @FXML private Button btnToggleFavorite, btnFileActionsHelp, btnSearchWithinFiles, btnNewRecordsQuery, btnNewFTSSearch;
  @FXML private MenuButton btnFileActions;
  @FXML private MenuItem mnuClear, mnuClearAndAdd, mnuAddSelected, mnuShowInSysExplorer, mnuShowInFileMgr;
  @FXML private Tab tabNew;
  @FXML private TabPane tabPane;
  @FXML private WebView webView;

  @FXML Button btnExecute;

  private CheckBoxMenuItem mnuIncludeEdited, mnuExcludeAnnots, mnuEntirePDF;

  private Property<ObservableList<ResultRow>> propToUnbind = null;
  private ChangeListener<ResultRow> cbListenerToRemove = null, tvListenerToRemove = null;
  private ComboBox<ResultRow> cb;
  private boolean clearingViews = false;

  private final List<QuerySubCtrlr> subCtrlrs = new ArrayList<>();
  private final Highlighter highlighter;

  private QuerySubCtrlr curSubCtrlr;
  private WebView ftsWebView;
  private Popup newTabPopup;
  private Node tabNewHeader;
  private PauseTransition popupDismissTimer;

  private static final List<Query<?>> allQueries = new ArrayList<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean inReportMode()                     { return nullSwitch(curQueryCtrlr(), false, QueryCtrlr::inReportMode); }
  private WebView curWebView()                       { return isFTSTabActive() && (ftsWebView != null) ? ftsWebView : webView; }

  public QueryCtrlr curQueryCtrlr()                  { return curSubCtrlr instanceof QueryCtrlr qc ? qc : null; }
  public List<ResultRow> results()                   { return nullSwitch(curQueryCtrlr(), List.of(), QueryCtrlr::results); }
  public void refreshTables()                        { subCtrlrs.forEach(sc -> { if (sc instanceof QueryCtrlr qc) qc.getResultsTV().refresh(); }); }
  public void setCB(ComboBox<ResultRow> cb)          { this.cb = cb; updateCB(curQueryCtrlr()); }
  public void btnExecuteClick()                      { if (curSubCtrlr != null) curSubCtrlr.executeOrSearch(); }
  public boolean isFTSTabActive()                    { return curSubCtrlr instanceof FTSQueryCtrlr; }

  @Override protected RecordType type()              { return hdtNone; }
  @Override protected void setRecord(HDT_Record rec) { nullSwitch(curQueryCtrlr(), qc -> qc.setRecord(rec)); }
  @Override protected void updateFromRecord()        { nullSwitch(curQueryCtrlr(), qc -> qc.refreshView(true)); }

  @Override public int recordCount()                 { return results().size(); }
  @Override public void setDividerPositions()        { }
  @Override public void getDividerPositions()        { }
  @Override public HDT_Record activeRecord()         { return nullSwitch(curQueryCtrlr(), null, QueryCtrlr::getRecord); }
  @Override public HDT_Record viewRecord()           { return activeRecord(); }
  @Override public String recordName()               { return nullSwitch(activeRecord(), "", HDT_Record::defaultChoiceText); }
  @Override public int recordNdx()                   { QueryCtrlr qc = curQueryCtrlr(); return (qc != null) && (recordCount() > 0) ? qc.getResultsTV().getSelectionModel().getSelectedIndex() : -1; }
  @Override public void findWithinDesc()             { if ((isFTSTabActive() == false) && ((activeRecord() != null) || inReportMode())) highlighter.hilite(); }
  @Override public void nextSearchResult()           { if (isFTSTabActive() == false) highlighter.nextSearchResult    (); }
  @Override public void previousSearchResult()       { if (isFTSTabActive() == false) highlighter.previousSearchResult(); }

  @Override public boolean saveToRecord(boolean saveNameIfBlank) { return false; }
  @Override public TextViewInfo mainTextInfo(HDT_Record record)  { return new TextViewInfo(record, webEngineScrollPos(curWebView().getEngine())); }

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
    btnToggleFavorite.setOnAction(event -> nullSwitch(curQueryCtrlr(), QueryCtrlr::btnFavoriteClick));

    btnNewRecordsQuery.setOnAction(event -> openNewRecordQueryTab());

    setToolTip(btnNewRecordsQuery, "Open a new tab for a record query or report");

    btnNewFTSSearch.setOnAction(event -> goToNewFTSTab());
    setToolTip(btnNewFTSSearch, "Open a new tab for searching file contents (full-text search)");

    setToolTip(btnSearchWithinFiles, "Search file contents within the files associated with the current query results");

    tabPane.getTabs().addListener((Change<? extends Tab> c) -> Platform.runLater(tabPane::requestLayout));

    tabPane.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (clearingViews == false) tabPaneChange(newValue);
      Platform.runLater(tabPane::requestLayout);
    });

    highlighter = new Highlighter(webView);

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      QueryCtrlr qc = curQueryCtrlr();
      if (qc == null) return;

      if (qc.inReportMode())
      {
        handleJSEvent("", webView.getEngine());
        return;
      }

      HDT_Record record = activeRecord();
      if (record == null) return;

      handleJSEvent(prepHtmlForDisplay(HDT_Record.getDescHtml(record)), webView.getEngine());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.getEngine().getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
    {
      QueryCtrlr qc = curQueryCtrlr();

      if ((newState == Worker.State.SUCCEEDED) && (qc != null))
      {
        if ((collEmpty(qc.getRecordsToHilite()) == false) && ui.currentFindInDescriptionText().isBlank())
          highlighter.hiliteAlreadyTagged();
        else
          highlighter.hilite(true);
      }
    });

    webView.setOnDragOver   (Event::consume);
    webView.setOnDragDropped(Event::consume);

    webViewAddZoom(webView, ZoomPrefKey.QUERYTAB);

    webView.getEngine().setUserStyleSheetLocation(cssStrToDataURI(EMPTY_FONT_CSS));

    initFileActions();
    initNewTabPopup();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initFileActions()
  {
    mnuClear            .setOnAction(event -> mnuClearSearchFolderClick());
    mnuClearAndAdd      .setOnAction(event -> mnuCopyAllClick          ());

    mnuAddSelected      .setOnAction(event -> copyFilesToFolder        (true ));
    mnuShowInSysExplorer.setOnAction(event -> mnuShowSearchFolderClick (false));
    mnuShowInFileMgr    .setOnAction(event -> mnuShowSearchFolderClick (true ));

    mnuIncludeEdited = new CheckBoxMenuItem("Include edited works"               , btnFileActions.showingProperty());
    mnuExcludeAnnots = new CheckBoxMenuItem("Strip annotations from PDFs on copy", btnFileActions.showingProperty());
    mnuEntirePDF     = new CheckBoxMenuItem("Always copy entire PDF file"        , btnFileActions.showingProperty());

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
        <p>The <strong>File Actions</strong> menu allows you to copy files associated with the results of your queries to the<br/>
           Search Results database folder. Then you can easily perform actions on the subset of files you are<br/>
           interested in, for example full-text search, combining PDFs into a single PDF or zip file, sending<br/>
           the files to colleagues, or making the files available to students as course readings.</p>
        <p>Description of each menu option:</p>
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
          <li><strong>Strip Annotations from PDFs on Copy:</strong> Removes annotations and highlights from PDF copies in<br/>
                      the destination folder; non-PDF files are copied as-is. The original files are not modified.<br/>
                      Useful for sharing clean versions of documents.</li>
          <li><strong>Always Copy Entire PDF File:</strong> Check this option to ensure the entire PDF file is copied. By<br/>
                      default, only the specific pages related to the query result are copied.</li>
        </ul>
      </body>
      </html>
      """));

    WebTooltip.setupClickHandler(btnFileActionsHelp, btnFileActions);

    setToolTip(btnFileActionsHelp, "File Actions Help");

    btnSearchWithinFiles.setOnAction(event -> searchWithinFiles());
    setToolTip(btnSearchWithinFiles, "Search file contents of the current query results");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void searchWithinFiles()
  {
    if (inReportMode())
    {
      errorPopup("This cannot be used with reports.");
      return;
    }

    List<ResultRow> resultRows = results();

    if (resultRows.isEmpty())
    {
      errorPopup("No query results to search within.");
      return;
    }

    List<HDT_RecordWithPath> sourceRecords = new ArrayList<>();

    for (ResultRow row : resultRows)
    {
      HDT_Record record = row.getRecord();
      if (record instanceof HDT_RecordWithPath recordWithPath)
        sourceRecords.add(recordWithPath);
    }

    // Include edited works: the user explicitly selected these records

    SearchResultFileList scopeList = new SearchResultFileList(false, true);
    sourceRecords.forEach(scopeList::addRecord);

    if (scopeList.getPathScope().isEmpty())
    {
      errorPopup("No indexable files found in the query results.");
      return;
    }

    goToNewFTSTab().setRecordScope(scopeList, sourceRecords);

    scopeList.showErrors();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void mnuShowSearchFolderClick(boolean inFileMgr)
  {
    if (db.isOffline()) return;

    if (inFileMgr)
      FileManager.show(db.resultsPath());
    else
      launchFile(db.resultsPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeRecord(HDT_Record record)
  {
    subCtrlrs.forEach(sc -> sc.removeRecord(record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void tabPaneChange(Tab newValue)
  {
    if (newValue == tabNew)
    {
      // Revert selection to previously active tab; show popup

      if (curSubCtrlr != null)
        tabPane.getSelectionModel().select(curSubCtrlr.getTab());

      showNewTabPopup();
      return;
    }

    QuerySubCtrlr subCtrlr = findFirst(subCtrlrs, sc -> sc.getTab() == newValue);

    if (subCtrlr != null)
    {
      deactivateCurrent();
      curSubCtrlr = subCtrlr;
      subCtrlr.onTabSelected(this);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void deleteSubView(Tab tab)
  {
    QuerySubCtrlr sc = findFirst(subCtrlrs, s -> s.getTab() == tab);
    if (sc == null) return;

    sc.onTabClosing();
    subCtrlrs.remove(sc);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FTSQueryCtrlr addFTSQueryCtrlr()
  {
    if (ftsWebView == null)
      ftsWebView = new WebView();

    FTSQueryCtrlr ftsCtrlr = new FTSQueryCtrlr(this, ftsWebView, tabPane);

    subCtrlrs.add(ftsCtrlr);

    return ftsCtrlr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private QueryCtrlr addQueryCtrlr()
  {
    QueryCtrlr queryCtrlr = new QueryCtrlr(this, webView, tabPane);

    subCtrlrs.add(queryCtrlr);
    queryCtrlr.resetFields();

    return queryCtrlr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void openNewRecordQueryTab()
  {
    QueryCtrlr queryCtrlr = addQueryCtrlr();
    tabPane.getSelectionModel().select(queryCtrlr.getTab());
    queryCtrlr.focusOnFields();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    clearingViews = true;

    deactivateCurrent();

    removeFromParent(webView);
    addToParent(webView, apOrigDescription);

    if (ftsWebView != null)
      removeFromParent(ftsWebView);

    subCtrlrs.removeIf(sc ->
    {
      sc.onClear(tabPane);
      return true;
    });

    curSubCtrlr = null;

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
    if ((type != qtReport) && db.isOffline()) return false;

    QueryCtrlr queryCtrlr = addQueryCtrlr();
    tabPane.getSelectionModel().select(queryCtrlr.getTab());

    return queryCtrlr.showSearch(doSearch, type, query, fav, op1, op2, caption);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void mnuCopyAllClick()
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

    if (db.isOffline() || results().isEmpty()) return false;

    if (new HyperTask("BuildListOfFilesToCopy", "Building list...") { @Override protected void call() throws CancelledTaskException
    {
      QueryCtrlr qc = curQueryCtrlr();
      List<ResultRow> resultRowList = (onlySelected && (qc != null)) ? qc.getResultsTV().getSelectionModel().getSelectedItems() : results();

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

  private static void mnuClearSearchFolderClick()
  {
    if (db.isOffline()) return;

    HDT_Folder resultsFolder = db.getResultsFolder();

    if (resultsFolder.getPath().isInUseByRecords() ||
        resultsFolder.childFolders.stream().anyMatch(childFolder -> childFolder.isSpecial(true)))
    {
      errorPopup("One or more file(s)/folder(s) in the search results folder are in use by the database.");
      return;
    }

    if (FileDeletion.ofDirContentsOnly(db.resultsPath()).interactive().execute() == DeletionResult.ABORTED)
      return;

    FileManager.pruneAndRefresh(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void closeCurrentView()
  {
    Tab selectedTab = tabPane.getSelectionModel().getSelectedItem();
    int ndx = tabPane.getSelectionModel().getSelectedIndex(), nextNdx = ndx + 1;

    deleteSubView(selectedTab);

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
    if (cb == null) return;

    if (propToUnbind != null)
    {
      cb.itemsProperty().unbindBidirectional(propToUnbind);
      propToUnbind = null;
    }

    if (cbListenerToRemove != null)
    {
      cb.getSelectionModel().selectedItemProperty().removeListener(cbListenerToRemove);
      cbListenerToRemove = null;
    }

    if (queryCtrlr == null)
    {
      if (tvListenerToRemove != null)
        tvListenerToRemove = null;

      cb.setItems(null);
      return;
    }

    TableView<ResultRow> tvResults = queryCtrlr.getResultsTV();

    if (tvListenerToRemove != null)
    {
      tvResults.getSelectionModel().selectedItemProperty().removeListener(tvListenerToRemove);
      tvListenerToRemove = null;
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

  private void deactivateCurrent()
  {
    if (curSubCtrlr != null)
      curSubCtrlr.deactivate();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setQueryToolbarVisible(boolean visible)
  {
    // btnToggleFavorite is disabled (not hidden) on FTS sub-tabs so the
    // toolbar layout doesn't collapse and leave a gap next to Execute Query.
    // Reset the icon to star-empty so the disabled state doesn't display the
    // filled star left over from a previously-active favorited records tab.

    btnToggleFavorite.setDisable(visible == false);
    if (visible == false)
      setFavNameToggle(false);

    setAllVisible(visible, btnSearchWithinFiles);
    btnSearchWithinFiles.setManaged(visible);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initNewTabPopup()
  {
    popupDismissTimer = new PauseTransition(Duration.millis(200));
    popupDismissTimer.setOnFinished(event ->
    {
      if (newTabPopup == null) return;

      Node content = newTabPopup.getContent().getFirst();
      if (content.isHover() || ((tabNewHeader != null) && tabNewHeader.isHover()))
      {
        popupDismissTimer.playFromStart();
        return;
      }

      newTabPopup.hide();
    });

    Label lblNewQuery = new Label("New record search"),
          lblNewFTS   = new Label("New file content search");

    String itemStyle  = "-fx-padding: 6 12 6 12; -fx-cursor: hand; -fx-background-color: -fx-control-inner-background;",
           hoverStyle = "-fx-padding: 6 12 6 12; -fx-cursor: hand; -fx-background-color: -fx-accent;";

    for (Label lbl : List.of(lblNewQuery, lblNewFTS))
    {
      lbl.setStyle(itemStyle);
      lbl.setMaxWidth(Double.MAX_VALUE);
      lbl.setOnMouseEntered(e -> lbl.setStyle(hoverStyle));
      lbl.setOnMouseExited (e -> lbl.setStyle(itemStyle));
    }

    lblNewQuery.setOnMouseClicked(event ->
    {
      newTabPopup.hide();
      openNewRecordQueryTab();
    });

    lblNewFTS.setOnMouseClicked(event ->
    {
      newTabPopup.hide();
      goToNewFTSTab();
    });

    VBox innerBox = new VBox(lblNewQuery, lblNewFTS);
    innerBox.setStyle("-fx-background-color: -fx-background; -fx-border-color: -fx-box-border; -fx-border-width: 1; -fx-padding: 2;");

    // Wrap in a container with padding so the drop shadow isn't clipped by the Popup bounds

    VBox vbox = new VBox(innerBox);
    vbox.setStyle("-fx-padding: 0 8 8 0; -fx-effect: dropshadow(gaussian, rgba(0,0,0,0.3), 8, 0, 2, 2);");

    innerBox.setOnMouseEntered(e -> popupDismissTimer.stop());
    innerBox.setOnMouseExited (e -> popupDismissTimer.playFromStart());

    newTabPopup = new Popup();
    newTabPopup.setAutoHide(true);
    newTabPopup.getContent().add(vbox);

    newTabPopup.setOnShowing(event ->
    {
      scaleNodeForDPI(innerBox);
      setFontSize(innerBox);
    });

    Platform.runLater(this::initTabNewHeader);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  FTSQueryCtrlr goToNewFTSTab()
  {
    FTSQueryCtrlr ftsCtrlr = addFTSQueryCtrlr();
    tabPane.getSelectionModel().select(ftsCtrlr.getTab());
    return ftsCtrlr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initTabNewHeader()
  {
    tabNewHeader = findFirst(tabPane.lookupAll(".tab"), header ->
    {
      Label label = (Label) header.lookup(".tab-label");
      return (label != null) && "+".equals(label.getText());
    });

    if (tabNewHeader == null) return;

    tabNewHeader.addEventFilter(MouseEvent.MOUSE_CLICKED, event ->
    {
      event.consume();
      showNewTabPopup();
    });

    tabNewHeader.setOnMouseEntered(e ->
    {
      popupDismissTimer.stop();
      showNewTabPopup();
    });

    tabNewHeader.setOnMouseExited(e -> popupDismissTimer.playFromStart());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showNewTabPopup()
  {
    if ((tabNewHeader == null) || (db.isLoaded() == false) || newTabPopup.isShowing()) return;

    Bounds bounds = tabNewHeader.localToScreen(tabNewHeader.getBoundsInLocal());
    if (bounds == null) return;

    newTabPopup.show(tabPane.getScene().getWindow(), bounds.getMinX(), bounds.getMaxY());
  }

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
      if (btnToggleFavorite.getGraphic() != ivStar)
        btnToggleFavorite.setGraphic(ivStar);

      setToolTip(btnToggleFavorite, "Edit name or remove from favorites");
    }
    else
    {
      if (btnToggleFavorite.getGraphic() != ivStarEmpty)
        btnToggleFavorite.setGraphic(ivStarEmpty);

      setToolTip(btnToggleFavorite, "Add to favorites");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
