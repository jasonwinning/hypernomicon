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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.Function;
import java.util.stream.IntStream;

import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.search.*;

import org.hypernomicon.App;
import org.hypernomicon.Const.TablePrefKey;
import org.hypernomicon.HyperTask;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.SearchBatch;
import org.hypernomicon.fts.FullTextIndexer.SearchResult;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_WorkFile.WorkBoundary;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.util.Util;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.OneTouchExpandableWrapper;
import org.hypernomicon.view.wrappers.OneTouchExpandableWrapper.CollapsedState;

import javafx.application.Platform;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.concurrent.Worker.State;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.scene.web.WebView;
import javafx.stage.DirectoryChooser;

//---------------------------------------------------------------------------

public class FTSQueryCtrlr extends QuerySubCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  record FTSResultRow(SearchResult result, HDT_RecordWithPath resolvedRecord)
  {
    String path() { return result.path(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnSearch, btnSearchHelp, btnChooseFolder, btnShowMore, btnShowAll, btnViewScope;
  @FXML private CheckBox chkExactPhrase, chkIncludeUnassociated, chkIncludeEdited;
  @FXML private HBox hbFolderGroup;
  @FXML private Label lblStatus, lblRecordScope;
  @FXML private RadioButton rbFolderScope, rbRecordScope;
  @FXML private SplitPane spLower;
  @FXML private TableColumn<FTSResultRow, String> colFileName, colFilePath, colRecord, colMatches, colExcerpt;
  @FXML private TableColumn<FTSResultRow, BibliographicDate> colDate;
  @FXML private TableView<FTSResultRow> tvResults;
  @FXML private TextField tfQuery, tfFolder, tfFileMask;
  @FXML private VBox vbMain;

  private final ObservableList<FTSResultRow> allRows = FXCollections.observableArrayList();
  private final FilteredList<FTSResultRow> filteredRows = new FilteredList<>(allRows);
  private final Map<String, List<PageMatch>> highlightCache = new ConcurrentHashMap<>();
  private final Set<String> highlightRequested = ConcurrentHashMap.newKeySet();
  private final ExecutorService highlightExecutor = Executors.newSingleThreadExecutor(runnable ->
  {
    HyperThread hyperThread = new HyperThread("FTS-highlight", runnable);
    hyperThread.setDaemon(true);
    return hyperThread;
  });

  private static final int PAGE_SIZE = 200, MAX_PASSAGES_PER_FILE = 10_000;

  private final FTSContextPaneRenderer contextPaneRenderer = new FTSContextPaneRenderer();

  private SearchResultFileList recordScopeList, lastScopeList;
  private ScoreDoc lastScoreDoc;
  private List<HDT_RecordWithPath> recordScopeRecords;
  private String lastQueryStr, lastFileMask, lastFolderPrefix, cachedContextHtml;

  /** Owns the current file's highlight lifecycle (viewer load sequencing, hit
   *  application). Disposed and replaced on file switch; {@code null} when no
   *  file is being previewed. */
  private FileHighlightCoordinator currentCoordinator;

  private int currentPreviewPage = 1, totalMatchCount = -1;
  private boolean hasMore;

  // searchGeneration: incremented only on the JavaFX Application Thread (in executeSearch),
  // read on the FX thread and on highlightExecutor worker threads to detect and abort
  // stale per-row highlight work when a new query replaces the current results.
  // Single-writer model; volatile provides the required visibility.
  //
  // Per-file highlight cancellation (previously the job of a sibling highlightGeneration
  // counter) is now handled by FileHighlightCoordinator.dispose().

  private volatile int searchGeneration;

//---------------------------------------------------------------------------

  FTSQueryCtrlr(QueriesTabCtrlr queriesTabCtrlr, WebView ftsWebView, TabPane tabPane)
  {
    super(ftsWebView);

    FXMLLoader loader = new FXMLLoader(App.class.getResource("query/FTSQuery.fxml"), null, null, klass -> this);

    try { tab = new Tab("New file content search", loader.load()); }
    catch (IOException e)
    {
      internalErrorPopup(90204);
      return;
    }

    tabPane.getTabs().add(tabPane.getTabs().size() - 1, tab);
    tab.setOnCloseRequest(event -> queriesTabCtrlr.deleteSubView((Tab) event.getSource()));

    SortedList<FTSResultRow> sortedRows = new SortedList<>(filteredRows);
    tvResults.setItems(sortedRows);
    sortedRows.comparatorProperty().bind(tvResults.comparatorProperty());

    chkIncludeUnassociated.selectedProperty().addListener((ob, ov, nv) ->
    {
      filteredRows.setPredicate(nv ? null : row -> row.resolvedRecord() != null);
      updateStatusLabel();
    });

    setToolTip(chkIncludeUnassociated, "Include results from files that aren't associated with any database record");

    initColumns();
    initContextMenu();

    scaleNodeForDPI(vbMain);
    setFontSize(vbMain);

    HyperTable.registerTable(tvResults, TablePrefKey.FTS_RESULTS, null);

    Platform.runLater(() -> OneTouchExpandableWrapper.wrap(spLower, () -> "search results", () -> "file content preview", 0.6, CollapsedState.Expanded));

    tfFolder.setEditable(false);

    if (db.isLoaded())
      tfFolder.setText(db.getRootPath().toString());

    btnSearch.setOnAction (event -> executeSearch());
    tfQuery.setOnAction   (event -> executeSearch());
    tfFileMask.setOnAction(event -> executeSearch());

    setToolTip(tfFileMask, "Search only files whose names match these comma-separated patterns (e.g. *.pdf, *.docx); leave blank to search all files");

    btnShowMore.setOnAction (event -> showMore());
    btnShowAll .setOnAction (event -> showAll());
    btnViewScope.setOnAction(event -> showRecordScopePopup());

    // Radio button scope switching

    rbFolderScope.selectedProperty().addListener((ob, oldVal, newVal) -> disableAllIff(newVal == false, hbFolderGroup , btnChooseFolder));
    rbRecordScope.selectedProperty().addListener((ob, oldVal, newVal) -> disableAllIff(newVal == false, lblRecordScope, chkIncludeEdited, btnViewScope));

    chkIncludeEdited.selectedProperty().addListener((ob, oldVal, newVal) -> rebuildRecordScope());

    // Initial state: record scope radio and its controls disabled (no snapshot yet)

    disableAll(rbRecordScope, lblRecordScope, chkIncludeEdited, btnViewScope);

    initSearchHelp();

    btnChooseFolder.setOnAction(event ->
    {
      DirectoryChooser dirChooser = new DirectoryChooser();
      dirChooser.setTitle("Select folder to search");

      if (db.isLoaded())
        dirChooser.setInitialDirectory(db.getRootPath().toFile());

      FilePath filePath = showDirDialog(dirChooser);

      if (FilePath.isEmpty(filePath)) return;

      if (filePath.isUnderDbRoot() == false)
      {
        errorPopup("You must choose a subfolder of the main database folder.");
        return;
      }

      tfFolder.setText(filePath.toString());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());
    webView.setOnDragOver            (Event::consume);
    webView.setOnDragDropped         (Event::consume);

    Platform.runLater(tfQuery::requestFocus);
  }

//---------------------------------------------------------------------------

  @Override void executeOrSearch()        { executeSearch(); }
  @Override void onTabClosing()           { }
  @Override void onClear(TabPane tabPane) { tabPane.getTabs().remove(getTab()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initSearchHelp()
  {
    tfQuery.setTooltip(new WebTooltip("""
      <html lang="en">
      <head>
        <style>
          h3 { color: #4682B4; margin-bottom: 4px; }
          code { font-weight: bold; color: orangered; }
          td { padding: 2px 8px 2px 0; vertical-align: text-top; }
        </style>
      </head>
      <body>
        <h3>Basic Search</h3>
        <p>All words must appear in the document (AND is the default operator).<br/>
           Searches are case-insensitive and match whole words.</p>

        <h3>Phrases</h3>
        <table>
          <tr><td><code>"social epistemology"</code></td><td>Matches the exact phrase</td></tr>
          <tr><td><code>"knowledge virtue"~3</code></td><td>Words within 3 positions of each other (proximity search)</td></tr>
        </table>
        <p>Tip: use the <strong>Exact phrase</strong> checkbox to automatically quote your query.</p>

        <h3>Wildcards</h3>
        <table>
          <tr><td><code>cognit*</code></td><td>Matches <em>cognitive</em>, <em>cognition</em>, <em>cognitivism</em>, etc.</td></tr>
          <tr><td><code>wom?n</code></td><td>Matches <em>woman</em> or <em>women</em> (single character)</td></tr>
        </table>

        <h3>Boolean Operators</h3>
        <table>
          <tr><td><code>virtue AND epistemology</code></td><td>Both words must appear (default behavior)</td></tr>
          <tr><td><code>virtue OR knowledge</code></td><td>Either word may appear</td></tr>
          <tr><td><code>epistemology NOT reliabilism</code></td><td>Excludes documents containing <em>reliabilism</em></td></tr>
          <tr><td><code>+virtue -foundationalism</code></td><td>Must contain <em>virtue</em>, must not contain <em>foundationalism</em></td></tr>
        </table>

        <h3>Grouping</h3>
        <table>
          <tr><td><code>(virtue OR credit) AND epistemology</code></td><td>Parentheses control evaluation order</td></tr>
        </table>

        <h3>Regular Expressions</h3>
        <table>
          <tr><td><code>/cogniti.*/</code></td><td>Regex pattern matching against individual terms</td></tr>
        </table>

        <h3>Escaping Special Characters</h3>
        <p>To search for characters that have special meaning, prefix them with a backslash:<br/>
           <code>+ - &amp;&amp; || ! ( ) { } [ ] ^ " ~ * ? : \\ /</code></p>
      </body>
      </html>
      """));

    WebTooltip.setupClickHandler(btnSearchHelp, tfQuery);

    setToolTip(btnSearchHelp, "Search Syntax Help");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initColumns()
  {
    colFileName.setCellValueFactory(cellData ->
    {
      String path = cellData.getValue().path();
      int lastSlash = path.lastIndexOf('/');
      return new SimpleStringProperty(lastSlash >= 0 ? path.substring(lastSlash + 1) : path);
    });

    colFileName.setCellFactory(col -> new TableCell<>()
    {
      @Override protected void updateItem(String path, boolean empty)
      {
        super.updateItem(path, empty);

        if (empty || (path == null))
        {
          setGraphic(null);
          setText(null);
          return;
        }

        setGraphic(nullSwitch(nullSwitch(getTableRow(), null,
                                         TableRow::getItem), null, ftsResultRow ->
                                         imgViewFromFilePath(db.getRootPath(ftsResultRow.path()), null)));
        setText(path);
      }
    });

    colFilePath.setCellValueFactory(cellData ->
    {
      String path = cellData.getValue().path();
      int lastSlash = path.lastIndexOf('/');
      return new SimpleStringProperty(lastSlash >= 0 ? path.substring(0, lastSlash) : "");
    });

    colRecord.setCellValueFactory(cellData ->
    {
      HDT_RecordWithPath record = cellData.getValue().resolvedRecord();
      return new SimpleStringProperty(nullSwitch(record, "", HDT_RecordWithPath::defaultChoiceText));
    });

    colRecord.setCellFactory(col -> new TableCell<>()
    {
      @Override protected void updateItem(String value, boolean empty)
      {
        super.updateItem(value, empty);

        if (empty || (value == null))
        {
          setGraphic(null);
          setText(null);
          return;
        }

        HDT_RecordWithPath record = nullSwitch(nullSwitch(getTableRow(), null, TableRow::getItem), null, FTSResultRow::resolvedRecord);

        setGraphic(record != null ? imgViewForRecord(record, record.getType()) : null);
        setText(value);
      }
    });

    colDate.setCellValueFactory(cellData ->
    {
      HDT_RecordWithPath record = cellData.getValue().resolvedRecord();
      BibliographicDate date = (record == null) || (record.getType() != hdtWork)
        ? BibliographicDate.EMPTY_DATE
        : ((HDT_Work) record).getBibDate();
      return new SimpleObjectProperty<>(date);
    });

    colDate.setCellFactory(col -> new TableCell<>()
    {
      @Override protected void updateItem(BibliographicDate date, boolean empty)
      {
        super.updateItem(date, empty);
        setText((empty || (date == null) || (BibliographicDate.isEmpty(date))) ? null : date.displayToUser());
      }
    });

    colMatches.setCellValueFactory(cellData ->
    {
      String path = cellData.getValue().path();
      List<PageMatch> matches = highlightCache.get(path);

      if (matches == null)
      {
        requestHighlight(path);
        return new SimpleStringProperty("");
      }

      int count = matches.stream().mapToInt(pm -> nullSwitch(pm.hitRanges(), 0, List::size)).sum();
      return new SimpleStringProperty(count > 0 ? String.valueOf(count) : "");
    });

    colMatches.setStyle("-fx-alignment: CENTER-RIGHT;");

    colExcerpt.setCellValueFactory(cellData ->
    {
      String path = cellData.getValue().path();
      List<PageMatch> matches = highlightCache.get(path);

      if (matches == null)
      {
        requestHighlight(path);
        return new SimpleStringProperty(null);
      }

      if (matches.isEmpty())
        return new SimpleStringProperty("");

      return new SimpleStringProperty(matches.getFirst().snippet().replaceAll("\\s+", " ").trim());
    });

    colExcerpt.setCellFactory(col -> new TableCell<>()
    {
      private final HBox hbox = new HBox(0);

      private final ChangeListener<Boolean> excerptSelectionListener = (ob, wasSelected, isSelected) ->
        applyExcerptColors(hbox, isSelected);

      {
        hbox.setAlignment(Pos.BASELINE_LEFT);

        tableRowProperty().addListener((ob, oldRow, newRow) ->
        {
          if (oldRow != null) oldRow.selectedProperty().removeListener(excerptSelectionListener);
          if (newRow != null) newRow.selectedProperty().addListener(excerptSelectionListener);
        });
      }

      @Override protected void updateItem(String value, boolean empty)
      {
        super.updateItem(value, empty);

        if (empty || (value == null))
        {
          setGraphic(null);
          setText(null);
          return;
        }

        FTSResultRow item = nullSwitch(getTableRow(), null, TableRow::getItem);

        if (item == null)
        {
          setGraphic(null);
          setText(value);
          return;
        }

        List<PageMatch> matches = highlightCache.get(item.path());

        if (collEmpty(matches))
        {
          setGraphic(null);
          setText(value);
          return;
        }

        hbox.getChildren().clear();
        buildHighlightedExcerpt(hbox, matches.getFirst());
        applyExcerptColors(hbox, nullSwitch(getTableRow(), false, TableRow::isSelected));
        setGraphic(hbox);
        setText(null);
      }
    });

    colFileName.setComparator(FilePath::compareFileNames);
    colFilePath.setComparator(FilePath::comparePaths);
    colRecord  .setComparator(Comparator.comparing(str -> HDT_RecordBase.makeSortKeyByType(nullSwitch(str, "", Function.identity()), hdtWork)));
    colMatches .setComparator(Util::compareNumberStrings);
    colExcerpt .setSortable(false);

    tvResults.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      // The listener also fires when a row in allRows is replaced in place (e.g., when the Record column is updated after record resolution).
      // JavaFX re-fires with oldValue=null, newValue=the replacement row. That's not a real navigation event; the user is still on the same
      // file. Don't reset the page or re-trigger setPreview (which would scroll the preview back to page 1). Just refresh the context view
      // since the row's resolvedRecord may have changed.

      if ((currentCoordinator != null) && currentCoordinator.path().equals(newValue.path()))
      {
        updateContextView(newValue);
        return;
      }

      currentPreviewPage = 1;
      updateContextView(newValue);
      setPreview(newValue);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initContextMenu()
  {
    tvResults.setRowFactory(tv ->
    {
      TableRow<FTSResultRow> row = new TableRow<>();
      ContextMenu contextMenu = new ContextMenu();

      MenuItem launchItem   = new MenuItem("Launch file"),
               explorerItem = new MenuItem("Show in system explorer"),
               fmItem       = new MenuItem("Show in File Manager"),
               copyPathItem = new MenuItem("Copy path to clipboard"),
               previewItem  = new MenuItem("Show in Preview Window"),
               goToItem     = new MenuItem("Go to record");

      launchItem  .setOnAction(event -> nullSwitch(row.getItem(), item -> { FilePath fp = db.getRootPath(item.path()); if (fp.exists()) launchWorkFile(fp, currentPreviewPage); }));
      explorerItem.setOnAction(event -> nullSwitch(row.getItem(), item -> highlightFileInExplorer(db.getRootPath(item.path()))));
      fmItem      .setOnAction(event -> nullSwitch(row.getItem(), item -> FileManager.show(db.getRootPath(item.path()))));
      copyPathItem.setOnAction(event -> nullSwitch(row.getItem(), item -> copyToClipboard(db.getRootPath(item.path()).toString())));
      goToItem    .setOnAction(event -> nullSwitch(row.getItem(), item -> nullSwitch(item.resolvedRecord(), record -> ui.goToRecord(record, false))));
      previewItem .setOnAction(event -> nullSwitch(row.getItem(), item ->
      {
        FilePath fp = db.getRootPath(item.path());
        if (fp.exists() == false) return;

        // Open the window first so the WebView is initialized before the
        // coordinator pushes hits into it. Dispose the existing coordinator
        // so setPreview's same-file check fails and a fresh coordinator is
        // created against the now-visible viewer (otherwise direct-content
        // hits set while the window was closed would be silently dropped).

        PreviewWindow.show(pvsQueriesTab);
        disposeCurrentCoordinator();
        setPreview(item);
      }));

      contextMenu.getItems().addAll(launchItem, explorerItem, fmItem, copyPathItem, previewItem, goToItem);

      contextMenu.setOnShowing(event ->
      {
        FTSResultRow item = row.getItem();
        if (item == null) return;

        boolean fileExists = db.getRootPath(item.path()).exists();

        setAllVisible(fileExists, launchItem, explorerItem, fmItem, copyPathItem, previewItem);
        goToItem.setVisible(item.resolvedRecord() != null);
      });

      row.setOnMouseClicked(mouseEvent ->
      {
        if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2))
          nullSwitch(row.getItem(), item ->
          {
            FilePath filePath = db.getRootPath(item.path());

            if (filePath.exists())
              launchWorkFile(filePath, currentPreviewPage);
          });
      });

      row.contextMenuProperty().bind
      (
        javafx.beans.binding.Bindings.when(row.emptyProperty())
          .then((ContextMenu) null)
          .otherwise(contextMenu)
      );

      return row;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String HIT_TERM_MARKER = "hit";

  private static void buildHighlightedExcerpt(Pane container, PageMatch pm)
  {
    String snippet = pm.snippet();
    List<SearchResult.HitRange> ranges = pm.hitRanges();

    if (collEmpty(ranges))
    {
      container.getChildren().add(new Text(snippet.replaceAll("\\s+", " ").trim()));
      return;
    }

    int pos = 0;

    for (SearchResult.HitRange range : ranges)
    {
      int start = Math.max(range.start(), pos),
          end = Math.min(range.end(), snippet.length());

      if (start > pos)
      {
        String seg = snippet.substring(pos, start).replaceAll("\\s+", " ");
        if ((pos == 0) && (seg.isEmpty() == false) && (seg.charAt(0) == ' '))
          seg = seg.substring(1);

        container.getChildren().add(new Text(seg));
      }

      if (end > start)
      {
        Text highlighted = new Text(snippet.substring(start, end));
        highlighted.setUserData(HIT_TERM_MARKER);
        container.getChildren().add(highlighted);
      }

      pos = end;
    }

    if (pos < snippet.length())
    {
      String seg = snippet.substring(pos).replaceAll("\\s+", " ");
      if (seg.endsWith(" "))
        seg = seg.substring(0, seg.length() - 1);

      container.getChildren().add(new Text(seg));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void applyExcerptColors(Pane container, boolean selected)
  {
    for (javafx.scene.Node child : container.getChildren())
    {
      if (child instanceof Text text)
      {
        if (selected)
          text.setStyle(text.getUserData() == HIT_TERM_MARKER ? "-fx-font-weight: bold; -fx-fill: white;" : "-fx-fill: white;");
        else
          text.setStyle(text.getUserData() == HIT_TERM_MARKER ? "-fx-font-weight: bold; -fx-fill: black;" : "-fx-fill: #555555;");
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface
  private interface SearchCall<T>
  {
    T run() throws ParseException, IndexSearcher.TooManyNestedClauses;
  }

  /**
   * Runs a Lucene search inside a HyperTask configured for FTS:
   * interrupt-on-cancel, fire-and-forget cancel (Lucene isn't responsive to
   * Thread.interrupt mid-search), and a daemon thread (so an abandoned task
   * doesn't block JVM exit). Quick searches finish before the progress dialog
   * shows; slow ones get a cancellable dialog.
   * <p>
   * Returns the lambda's result, or null on cancel / parse error / too-many
   * clauses (an errorPopup is shown for the latter two).
   */
  private static <T> T runSearchTask(String dialogMessage, SearchCall<T> call)
  {
    final class SearchTask extends HyperTask
    {
      private volatile T result;
      private volatile ParseException parseError;
      private volatile IndexSearcher.TooManyNestedClauses tooManyError;

      private SearchTask() { super("FTSSearch", dialogMessage, false); }

      @Override protected void call()
      {
        try { result = call.run(); }
        catch (ParseException e)                     { parseError = e; }
        catch (IndexSearcher.TooManyNestedClauses e) { tooManyError = e; }
      }
    }

    SearchTask task = new SearchTask();
    task.setInterruptOnCancel(true);
    task.setWaitOnCancel(false);  // Lucene search isn't responsive to interrupt; abandon stale tasks
    task.setDaemonThread(true);   // ...and don't let an abandoned task block JVM exit

    if (task.runWithProgressDialog() != State.SUCCEEDED) return null;

    if (task.tooManyError != null)
    {
      errorPopup("Your search expanded to too many term variants. " +
                 "Try using a longer or more specific word, or avoid broad prefix or wildcard terms.");

      return null;
    }

    if (task.parseError != null)
    {
      errorPopup("Unable to parse search query: " + task.parseError.getMessage());
      return null;
    }

    return task.result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Carries a {@link SearchBatch} plus the total match count from
   *  {@code countMatches}; both are computed inside the same off-FX task in
   *  {@link #executeSearch}. */
  private record SearchWithTotal(SearchBatch batch, int totalCount) {}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void executeSearch()
  {
    String queryStr = tfQuery.getText();
    if (strNullOrBlank(queryStr)) return;

    FullTextIndexer indexer = db.getFullTextIndexer();
    if (indexer == null) return;

    if (chkExactPhrase.isSelected() && (queryStr.startsWith("\"") == false))
      queryStr = '"' + queryStr + '"';

    boolean useRecordScope = rbRecordScope.isSelected() && (recordScopeList != null);

    String fileMask = useRecordScope ? null : tfFileMask.getText();

    if ((fileMask != null) && fileMask.isBlank()) fileMask = null;

    Set<String> pathScope = useRecordScope ? recordScopeList.getPathScope() : null;

    String folderPrefix = useRecordScope ? null : computeFolderPrefix();

    // Clear previous state

    ++searchGeneration;
    highlightCache.clear();
    highlightRequested.clear();
    currentPreviewPage = 1;
    disposeCurrentCoordinator();

    // Fast light search: no highlighting, just paths and scores. Both the
    // searchLight and the countMatches run off the FX thread inside one
    // HyperTask so the UI doesn't freeze on long queries.

    String finalQueryStr = queryStr,
           finalFileMask = fileMask;

    SearchWithTotal result = runSearchTask("Searching...", () ->
    {
      SearchBatch b = indexer.searchLight(finalQueryStr, PAGE_SIZE, finalFileMask, pathScope, folderPrefix);

      int count = -1;

      if (b.hasMore())
      {
        try
        {
          count = indexer.countMatches(finalQueryStr);
        }
        catch (Exception e) { /* keep -1 */ }
      }

      return new SearchWithTotal(b, count);
    });

    if (result == null) return;

    SearchBatch batch = result.batch();

    lastQueryStr = queryStr;
    lastFileMask = fileMask;
    lastFolderPrefix = folderPrefix;
    lastScopeList = useRecordScope ? recordScopeList : null;
    hasMore = batch.hasMore();
    totalMatchCount = hasMore ? result.totalCount() : -1;

    List<SearchResult> lightResults = batch.results();

    lastScoreDoc = lightResults.isEmpty() ? null : lightResults.getLast().scoreDoc();

    tab.setText(queryStr.length() > 20 ? queryStr.substring(0, 20) + "..." : queryStr);

    if (useRecordScope)
    {
      // For record-scoped searches, highlight and filter up front so rows with
      // no in-range matches never appear in the table.

      int gen = searchGeneration;
      SearchResultFileList scopeList = lastScopeList;

      highlightExecutor.submit(() ->
      {
        if (searchGeneration != gen) return;

        FullTextIndexer idx = db.getFullTextIndexer();
        if (idx == null) return;

        try
        {
          List<SearchResult> highlighted = idx.highlightResults(finalQueryStr, lightResults, MAX_PASSAGES_PER_FILE);

          if (searchGeneration != gen) return;

          if (scopeList != null)
            highlighted = scopeList.filterResults(highlighted);

          List<SearchResult> finalHighlighted = highlighted;

          for (SearchResult sr : finalHighlighted)
          {
            List<PageMatch> matches = sr.pageMatches();
            if (matches != null)
            {
              highlightCache.put(sr.path(), matches);
              highlightRequested.add(sr.path());
            }
          }

          Platform.runLater(() ->
          {
            if (searchGeneration != gen) return;

            List<FTSResultRow> rows = new ArrayList<>();

            for (SearchResult sr : finalHighlighted)
            {
              FilePath filePath = db.getRootPath(sr.path());
              List<PageMatch> matches = sr.pageMatches();
              IntStream pages = (matches != null) ? matches.stream().mapToInt(PageMatch::pageNumber) : IntStream.empty();
              HDT_RecordWithPath record = HDT_WorkFile.resolveRecordForPages(filePath, pages, HyperPath.resolveRecord(filePath, 0));
              rows.add(new FTSResultRow(sr, record));
            }

            showResults(rows);
          });
        }
        catch (ParseException e) { /* query was valid when search ran */ }
      });
    }
    else
    {
      // For folder/file-mask searches, show rows immediately; highlighting is lazy.

      List<FTSResultRow> rows = new ArrayList<>();

      for (SearchResult sr : lightResults)
      {
        FilePath filePath = db.getRootPath(sr.path());
        rows.add(new FTSResultRow(sr, HyperPath.resolveRecord(filePath, 0)));
      }

      showResults(rows);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showResults(List<FTSResultRow> rows)
  {
    allRows.setAll(rows);

    if (allRows.isEmpty())
      loadContextHtml("");
    else
      tvResults.getSelectionModel().selectFirst();

    updateStatusLabel();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showMore()
  {
    if ((lastScoreDoc == null) || (hasMore == false)) return;

    FullTextIndexer indexer = db.getFullTextIndexer();
    if (indexer == null) return;

    Set<String> pathScope = nullSwitch(lastScopeList, null, SearchResultFileList::getPathScope);

    ScoreDoc finalAfter = lastScoreDoc;
    String finalQueryStr = lastQueryStr, finalFileMask = lastFileMask, finalFolderPrefix = lastFolderPrefix;

    SearchBatch batch = runSearchTask("Loading more results...", () ->
      indexer.searchLightAfter(finalAfter, finalQueryStr, PAGE_SIZE, finalFileMask, pathScope, finalFolderPrefix));

    if (batch == null) return;

    hasMore = batch.hasMore();

    List<SearchResult> lightResults = batch.results();

    if (lightResults.isEmpty() == false)
    {
      lastScoreDoc = lightResults.getLast().scoreDoc();

      for (SearchResult sr : lightResults)
      {
        FilePath filePath = db.getRootPath(sr.path());
        allRows.add(new FTSResultRow(sr, HyperPath.resolveRecord(filePath, 0)));
      }
    }

    updateStatusLabel();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showAll()
  {
    while (hasMore)
      showMore();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void requestHighlight(String path)
  {
    if (highlightRequested.add(path) == false) return;

    String queryStr = lastQueryStr;
    if (queryStr == null) return;

    int gen = searchGeneration;
    SearchResultFileList scopeList = lastScopeList;

    highlightExecutor.submit(() ->
    {
      if (searchGeneration != gen) return;

      FullTextIndexer indexer = db.getFullTextIndexer();
      if (indexer == null) return;

      try
      {
        SearchResult light = new SearchResult(path, 0f, null, null);
        List<SearchResult> results = indexer.highlightResults(queryStr, List.of(light), MAX_PASSAGES_PER_FILE);

        if (searchGeneration != gen) return;

        if (scopeList != null)
          results = scopeList.filterResults(results);

        List<PageMatch> matches = results.isEmpty()
          ? List.of()
          : nullSwitch(results.getFirst().pageMatches(), List.of());

        highlightCache.put(path, matches);

        Platform.runLater(() ->
        {
          if (searchGeneration != gen) return;

          // If page-range filtering removed all matches, remove the row entirely

          if ((scopeList != null) && matches.isEmpty())
          {
            allRows.removeIf(row -> row.path().equals(path));
          }
          else
          {
            // Update the Record column with the most specific work covering all match pages

            for (int ndx = 0; ndx < allRows.size(); ndx++)
            {
              FTSResultRow row = allRows.get(ndx);

              if (row.path().equals(path))
              {
                FilePath filePath = db.getRootPath(path);
                IntStream pages = matches.stream().mapToInt(PageMatch::pageNumber);
                HDT_RecordWithPath newRecord = HDT_WorkFile.resolveRecordForPages(filePath, pages, row.resolvedRecord());

                if (newRecord != row.resolvedRecord())
                  allRows.set(ndx, new FTSResultRow(row.result(), newRecord));

                break;
              }
            }
          }

          tvResults.refresh();
          updateStatusLabel();

          FTSResultRow selected = tvResults.getSelectionModel().getSelectedItem();
          if ((selected != null) && selected.path().equals(path))
          {
            updateContextView(selected);

            // Force re-send of hits now that we have match data

            disposeCurrentCoordinator();
            setPreview(selected);
          }
        });
      }
      catch (ParseException e) { /* query was valid when search ran */ }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateContextView(FTSResultRow selectedRow)
  {
    String path = selectedRow.path();
    List<PageMatch> matches = highlightCache.get(path);

    if (matches == null)
    {
      requestHighlight(path);
      loadContextHtml(FTSContextPaneRenderer.renderLoading(path));
      return;
    }

    List<WorkBoundary> boundaries = HDT_WorkFile.getBoundariesForFile(db.getRootPath(path));
    loadContextHtml(contextPaneRenderer.renderInitial(path, matches, boundaries));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadContextHtml(String html)
  {
    cachedContextHtml = html;
    webView.getEngine().loadContent(html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadMorePassages()
  {
    FTSResultRow selected = tvResults.getSelectionModel().getSelectedItem();
    if (selected == null) return;

    List<PageMatch> matches = highlightCache.get(selected.path());
    if (nullSwitch(matches, false, contextPaneRenderer::hasMore) == false) return;

    String escapedHtml = contextPaneRenderer.renderNextBatch(matches)
      .replace("\\", "\\\\")
      .replace("'", "\\'")
      .replace("\n", "\\n")
      .replace("\r", "");

    StringBuilder js = new StringBuilder();
    js.append("var s = document.getElementById('sentinel'); if (s) s.remove(); ");
    js.append("document.getElementById('passages').insertAdjacentHTML('beforeend', '").append(escapedHtml).append("'); ");

    if (contextPaneRenderer.hasMore(matches))
    {
      js.append("var sentinel = document.createElement('div'); sentinel.id = 'sentinel'; sentinel.style.height = '1px'; ");
      js.append("document.getElementById('passages').appendChild(sentinel); ");
      js.append("new IntersectionObserver(function(entries) { if (entries[0].isIntersecting) alert('loadmore'); }, ");
      js.append("{threshold: 0.1}).observe(sentinel); ");
    }

    webView.getEngine().executeScript(js.toString());

    // Update cached HTML to include the newly appended passages
    Object content = webView.getEngine().executeScript("document.documentElement.outerHTML");
    if (content instanceof String s)
      cachedContextHtml = s;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateStatusLabel()
  {
    int totalCount = allRows.size(),
        shownCount = filteredRows.size(),
        hiddenCount = totalCount - shownCount;

    String noun = shownCount == 1 ? "file" : "files",
           hiddenSuffix = hiddenCount > 0 ? " (" + hiddenCount + " unassociated hidden)" : "";

    if (totalCount == 0)
    {
      lblStatus.setText("No matches found");
      setAllVisible(false, btnShowMore, btnShowAll);
    }
    else if (shownCount == 0)
    {
      lblStatus.setText("No matches found" + hiddenSuffix);
      setAllVisible(false, btnShowMore, btnShowAll);
    }
    else if (hasMore)
    {
      String totalSuffix = (totalMatchCount > 0) ? " of " + totalMatchCount : "";
      lblStatus.setText("Showing first " + shownCount + ' ' + noun + totalSuffix + hiddenSuffix);
      setAllVisible(true, btnShowMore, btnShowAll);
    }
    else
    {
      lblStatus.setText(shownCount + " " + noun + hiddenSuffix);
      setAllVisible(false, btnShowMore, btnShowAll);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String computeFolderPrefix()
  {
    String folderStr = tfFolder.getText();
    if (strNullOrBlank(folderStr) || (db.isLoaded() == false)) return null;

    FilePath folderPath = FilePath.of(folderStr),
             relPath    = db.getRootPath().relativize(folderPath);

    if (relPath == null) return null;

    String prefix = relPath.toString().replace('\\', '/');
    if (prefix.isEmpty()) return null;  // folder is the DB root; no filtering needed

    return prefix.endsWith("/") ? prefix : (prefix + '/');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setFolderScope(FilePath folderPath)
  {
    tfFolder.setText(folderPath.toString());
    rbFolderScope.setSelected(true);
    Platform.runLater(tfQuery::requestFocus);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setRecordScope(SearchResultFileList scopeList, List<HDT_RecordWithPath> sourceRecords)
  {
    recordScopeList = scopeList;
    recordScopeRecords = sourceRecords;

    lblRecordScope.setText(scopeList.getSummary());

    disableAllIff(false, rbRecordScope, chkIncludeEdited);
    setAllVisible(true , rbRecordScope, btnViewScope);
    rbRecordScope.setSelected(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rebuildRecordScope()
  {
    if (recordScopeRecords == null) return;

    SearchResultFileList scopeList = new SearchResultFileList(false, chkIncludeEdited.isSelected());

    for (HDT_RecordWithPath record : recordScopeRecords)
      scopeList.addRecord(record);

    recordScopeList = scopeList;
    lblRecordScope.setText(scopeList.getSummary());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showRecordScopePopup()
  {
    if (recordScopeList == null) return;

    List<String> desc = recordScopeList.getScopeDescription();
    String headerText = "Searching " + desc.size() + " file" + (desc.size() == 1 ? "" : "s");

    longMessagePopup("Record Scope", AlertType.INFORMATION, headerText, String.join("\n", desc));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void activate()
  {
    addToParent(webView, apDescription);
    setAnchors(webView, 0.0, 0.0, 0.0, 0.0);

    webView.getEngine().setOnAlert(event ->
    {
      String data = event.getData();

      if (data.startsWith("page:"))
      {
        currentPreviewPage = Math.max(parseInt(data.substring(5), 1), 1);

        FTSResultRow selected = tvResults.getSelectionModel().getSelectedItem();

        if (selected != null)
          setPreview(selected);
      }
      else if (data.startsWith("work:"))
      {
        int id = parseInt(data.substring(5), -1);
        HDT_Work work = db.works.getByID(id);
        if (work != null)
          ui.goToRecord(work, false);
      }
      else if ("loadmore".equals(data))
        loadMorePassages();
    });

    if (cachedContextHtml != null)
    {
      String html = cachedContextHtml;
      int scrollPos = scrollPosPriorToBeingDeactivated;

      if (scrollPos > 0)
        html = html.replace("<body", "<body onload='setTimeout(function(){window.scrollTo(0," + scrollPos + ");},0);'");

      webView.getEngine().loadContent(html);
    }
    else
    {
      webView.getEngine().loadContent("");
    }

    setPreview(tvResults.getSelectionModel().getSelectedItem());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPreview(FTSResultRow row)
  {
    if ((row == null) || (db.getRootPath(row.path()).exists() == false))
    {
      disposeCurrentCoordinator();
      PreviewWindow.clearPreview(pvsQueriesTab);
      return;
    }

    FilePath filePath = db.getRootPath(row.path());

    // Same file: user is navigating within it (passage click, Record-column
    // re-resolve). Keep the active coordinator; just move the viewer to the
    // requested page.

    if ((currentCoordinator != null) && currentCoordinator.path().equals(row.path()))
    {
      PreviewWindow.setPreview(pvsQueriesTab, filePath, currentPreviewPage, -1, row.resolvedRecord());
      return;
    }

    List<PageMatch> matches = nullSwitch(row.result().pageMatches(), highlightCache.get(row.path()));

    // For new files, navigate to the first page with a match (if known)

    if ((currentPreviewPage <= 1) && (collEmpty(matches) == false))
    {
      int firstPage = matches.stream()
        .mapToInt(PageMatch::pageNumber)
        .filter(p -> p > 0)
        .min().orElse(1);

      if (firstPage > 1)
        currentPreviewPage = firstPage;
    }

    PreviewWindow.setPreview(pvsQueriesTab, filePath, currentPreviewPage, -1, row.resolvedRecord());

    disposeCurrentCoordinator();

    if (matches == null) return;

    FullTextIndexer indexer = db.getFullTextIndexer();
    if (indexer == null) return;

    int[] pageOffsets = indexer.getPageOffsets(row.path());
    if (pageOffsets == null) return;

    currentCoordinator = new PdfHitCoordinator(row, indexer, matches, currentPreviewPage);
    currentCoordinator.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Disposes the current coordinator (if any) and nulls the reference. Safe
   * to call when no coordinator is active.
   */
  private void disposeCurrentCoordinator()
  {
    if (currentCoordinator != null)
    {
      currentCoordinator.dispose();
      currentCoordinator = null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void removeRecord(HDT_Record record)
  {
    for (int ndx = 0; ndx < allRows.size(); ndx++)
    {
      FTSResultRow row = allRows.get(ndx);
      if (row.resolvedRecord() == record)
        allRows.set(ndx, new FTSResultRow(row.result(), null));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void onTabSelected(QueriesTabCtrlr queriesTabCtrlr)
  {
    activate();
    queriesTabCtrlr.updateCB(null);
    queriesTabCtrlr.setQueryToolbarVisible(false);
    ui.updateBottomPanel(false, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
