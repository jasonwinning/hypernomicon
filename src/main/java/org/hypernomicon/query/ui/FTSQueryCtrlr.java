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
import static org.hypernomicon.fts.FTSUtil.*;
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
import java.util.function.IntPredicate;
import java.util.stream.IntStream;

import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.search.*;

import org.hypernomicon.App;
import org.hypernomicon.Const.TablePrefKey;
import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.SearchKeySelectDlgCtrlr;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.fts.FileSearchability;
import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.*;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.fts.HitSetService;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_WorkFile.WorkBoundary;
import org.hypernomicon.model.searchKeys.Keyword;
import org.hypernomicon.model.searchKeys.SearchKeys;
import org.hypernomicon.previewWindow.ConversionSession;
import org.hypernomicon.previewWindow.ConversionSession.NoOfficeInstallationException;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.previewWindow.ScrollTarget;
import org.hypernomicon.query.ui.SearchResultFileList.ScopedFile;
import org.hypernomicon.util.SettleGate;
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

  /**
   * A row of the results table: a file a search found, or one a blank query listed. A listed
   * row carries its {@code searchability}, which says whether a search could have found it; a
   * search row has none, and has matches instead.
   */
  record FTSResultRow(SearchResult result, HDT_RecordWithFilePath resolvedRecord, FileSearchability searchability)
  {
    FTSResultRow(SearchResult result, HDT_RecordWithFilePath resolvedRecord) { this(result, resolvedRecord, null); }

    String path() { return result.path(); }

    /** Whether the row comes from a listing rather than a search, so it has no matches to show. */
    boolean isListed() { return searchability != null; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnSearch, btnSearchHelp, btnChooseFolder, btnShowMore, btnShowAll, btnViewScope, btnSelectRecord;
  @FXML private CheckBox chkExactPhrase, chkIncludeUnassociated, chkIncludeEdited, chkSearchKey;
  @FXML private HBox hbFolderGroup;
  @FXML private Label lblStatus, lblRecordScope;
  @FXML private RadioButton rbFolderScope, rbRecordScope;
  @FXML private SplitPane spLower;
  @FXML private TableColumn<FTSResultRow, String> colFileName, colFilePath, colRecord, colMatches, colExcerpt, colStatus;
  @FXML private TableColumn<FTSResultRow, BibliographicDate> colDate;
  @FXML private TableView<FTSResultRow> tvResults;
  @FXML private TextField tfQuery, tfFolder, tfFileMask;
  @FXML private VBox vbMain;

  private final ObservableList<FTSResultRow> allRows = FXCollections.observableArrayList();
  private final FilteredList<FTSResultRow> filteredRows = new FilteredList<>(allRows);
  private final SortedList<FTSResultRow> sortedRows = new SortedList<>(filteredRows);

  /** Owns match computation and caching for this controller's searches: the
   *  per-file match cache and match counts, request deduplication,
   *  search-generation staleness, and the "FTS-highlight" worker thread. */
  private final HitSetService hitSetService = new HitSetService();

  private static final int PAGE_SIZE = 200;

  private final FTSContextPaneRenderer contextPaneRenderer = new FTSContextPaneRenderer();

  private Query lastSearchKeyQuery;
  private SearchResultFileList recordScopeList, lastScopeList;
  private ScoreDoc lastScoreDoc;
  private List<HDT_RecordWithFilePath> recordScopeRecords;
  private String lastQueryStr, lastFileMask, lastFolderPrefix, cachedContextHtml;

  /** Path of the row whose preview intent is currently set on the queries
   *  pane (guards redundant selection-listener refires), or {@code null} when
   *  no FTS preview is active. */
  private String currentPreviewPath;

  /** Clicked-match target for the next preview intent, set by a passage click
   *  and consumed when {@link #setPreview} actually sets an intent. It stays
   *  set across the closed-window deferral, so the activation replay opens the
   *  preview scrolled to the clicked match; a selection change or new search
   *  abandons it. */
  private ScrollTarget pendingScrollTarget;

  /** Passage index of a converted-office passage click that could not be
   *  navigated at click time because the alignment did not exist yet (preview
   *  window closed, or pipeline still running); consumed by
   *  {@link #applyStashedConvertedPassage} when the pipeline publishes the
   *  alignment. Same lifecycle as {@link #pendingScrollTarget}. */
  private int pendingConvertedPassageNdx = -1;

  /** Tika-to-pdf.js coordinate alignment for passage-click navigation,
   *  published by the converted-office hit pipeline for the path in
   *  {@code convertedAlignmentPath}; never gated on whether highlights were
   *  delivered. {@code convertedLaunchPath} records which path's pipeline has
   *  been launched this generation, so same-file navigation does not re-run
   *  extraction. */
  private HitSetService.ConvertedPdfAlignment convertedAlignment;
  private String convertedAlignmentPath, convertedLaunchPath;

  private int currentPreviewPage = 1, totalMatchCount = -1;
  private boolean hasMore;

  /** {@code listingShown}: whether the table holds the files a blank query listed rather than
   *  the results of a search (see {@link #executeListing}). */
  private boolean listingShown;

  /**
   * Settle gate for result-row selection: rapid selection (key-repeat through
   * the results table) must not set a preview intent, launch a hit pipeline, or
   * enqueue a conversion for rows it merely passes over; only the row the
   * selection settles on previews.
   */
  private final SettleGate previewSettleGate = new SettleGate(150);

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

    tvResults.setItems(sortedRows);
    tvResults.setSortPolicy(this::sortRows);

    chkIncludeUnassociated.selectedProperty().addListener((ob, ov, nv) ->
    {
      filteredRows.setPredicate(nv ? null : row -> row.resolvedRecord() != null);
      updateStatusLabel();
    });

    setToolTip(chkIncludeUnassociated, "Include results from files that aren't associated with any database record");

    chkSearchKey.selectedProperty().addListener((ob, ov, nv) -> chkExactPhrase.setDisable(nv));

    setToolTip(chkSearchKey, "Interpret the query as a semicolon-delimited list of record search keys");

    initColumns();
    initContextMenu();

    scaleNodeForDPI(vbMain);
    setFontSize(vbMain);

    HyperTable.registerTable(tvResults, TablePrefKey.FTS_RESULTS, null);

    showListingColumns(false);  // The saved column settings may be a listing's; a new tab starts as a search

    Platform.runLater(() -> OneTouchExpandableWrapper.wrap(spLower, () -> "search results", () -> "file content preview", 0.6, CollapsedState.Expanded));

    tfFolder.setEditable(false);

    if (db.isLoaded())
      tfFolder.setText(db.getRootPath().toString());

    btnSearch .setOnAction(event -> executeSearch());
    tfQuery   .setOnAction(event -> executeSearch());
    tfFileMask.setOnAction(event -> executeSearch());

    setToolTip(tfFileMask, "Search only files whose names match these comma-separated patterns (e.g. *.pdf, *.docx); leave blank to search all files. " +
                           "With a blank query, the matching files are listed instead of searched.");

    // The Search button says what it would do with the query and scope as they stand

    tfQuery   .textProperty().addListener((ob, ov, nv) -> updateSearchButtonCaption());
    tfFolder  .textProperty().addListener((ob, ov, nv) -> updateSearchButtonCaption());
    tfFileMask.textProperty().addListener((ob, ov, nv) -> updateSearchButtonCaption());

    setToolTip(btnSearch, () -> listingRequested() ? "List the files in the scope, since the query is blank" : "Search the files in the scope for the query");

    btnShowMore .setOnAction(event -> showMore());
    btnShowAll  .setOnAction(event -> showAll());
    btnViewScope.setOnAction(event -> showRecordScopePopup());

    // Radio button scope switching

    rbFolderScope   .selectedProperty().addListener((ob, oldVal, newVal) -> disableAllIff(newVal == false, hbFolderGroup , btnChooseFolder));
    rbRecordScope   .selectedProperty().addListener((ob, oldVal, newVal) ->
    {
      disableAllIff(newVal == false, lblRecordScope, chkIncludeEdited, btnViewScope);
      updateSearchButtonCaption();
    });

    chkIncludeEdited.selectedProperty().addListener((ob, oldVal, newVal) -> rebuildRecordScope());

    // Initial state: record scope radio and its controls disabled (no snapshot yet);
    // search key scope controls start disabled

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

    btnSelectRecord.setOnAction(event ->
    {
      SearchKeySelectDlgCtrlr dlg = new SearchKeySelectDlgCtrlr(true);

      if (dlg.showModal())
      {
        tfQuery.setText(dlg.getKeyword());
        chkSearchKey.setSelected(true);
      }
    });

    setToolTip(btnSelectRecord, "Choose a record whose search key to search for");

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());
    webView.setOnDragOver            (Event::consume);
    webView.setOnDragDropped         (Event::consume);

    Platform.runLater(tfQuery::requestFocus);
  }

//---------------------------------------------------------------------------

  @Override void executeOrSearch()        { executeSearch(); }
  @Override void onTabClosing()           { }
  @Override void onClear(TabPane tabPane) { tabPane.getTabs().remove(getTab()); }

  boolean hasResults() { return allRows.isEmpty() == false; }

  private static List<String> pathsOf(List<SearchResult> results) { return results.stream().map(SearchResult::path).toList(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initSearchHelp()
  {
    tfQuery.setTooltip(new WebTooltip("""
      <html lang="en">
      <head>
        <style>
          h3 { color: #4682B4; margin-bottom: 4px; margin-top: 0; }
          h3.cont { margin-top: 12px; }
          code { font-weight: bold; color: orangered; }
          td { padding: 2px 8px 2px 0; vertical-align: text-top; }
          td.col { padding-right: 28px; border-right: 1px solid #ddd; }
          td.col + td.col { padding-left: 28px; border-right: none; }
        </style>
      </head>
      <body>
        <table>
          <tr>
            <td class="col">
              <h3>Basic Search</h3>
              <p>All words must appear in the document (AND is the default operator).<br/>
                 Searches are case-insensitive and match whole words.</p>

              <h3 class="cont">Phrases</h3>
              <table>
                <tr><td><code>"social epistemology"</code></td><td>Matches the exact phrase</td></tr>
                <tr><td><code>"knowledge virtue"~3</code></td><td>Words within 3 positions of each other (proximity search)</td></tr>
              </table>
              <p>Tip: use the <strong>Exact phrase</strong> checkbox to automatically quote your query.</p>

              <h3 class="cont">Wildcards</h3>
              <table>
                <tr><td><code>cognit*</code></td><td>Matches <em>cognitive</em>, <em>cognition</em>, <em>cognitivism</em>, etc.</td></tr>
                <tr><td><code>wom?n</code></td><td>Matches <em>woman</em> or <em>women</em> (single character)</td></tr>
              </table>

              <h3 class="cont">Boolean Operators</h3>
              <table>
                <tr><td><code>virtue AND epistemology</code></td><td>Both words must appear (default behavior)</td></tr>
                <tr><td><code>virtue OR knowledge</code></td><td>Either word may appear</td></tr>
                <tr><td><code>epistemology NOT reliabilism</code></td><td>Excludes documents containing <em>reliabilism</em></td></tr>
                <tr><td><code>+virtue -foundationalism</code></td><td>Must contain <em>virtue</em>, must not contain <em>foundationalism</em></td></tr>
              </table>

              <h3 class="cont">Grouping</h3>
              <table>
                <tr><td><code>(virtue OR credit) AND epistemology</code></td><td>Parentheses control evaluation order</td></tr>
              </table>
            </td>
            <td class="col">
              <h3>Regular Expressions</h3>
              <table>
                <tr><td><code>/cogniti.*/</code></td><td>Regex pattern matching against individual terms</td></tr>
              </table>

              <h3 class="cont">Escaping Special Characters</h3>
              <p>To search for characters that have special meaning, prefix them with a backslash:<br/>
                 <code>+ - &amp;&amp; || ! ( ) { } [ ] ^ " ~ * ? : \\ /</code></p>

              <h3 class="cont">Record Search Key Mode</h3>
              <p>Check the <strong>Record search key mode</strong> checkbox to interpret the query as a
                 semicolon-delimited list of<br>record search keys. Use the
                 <strong>Select record</strong> button to fill the field from an existing record's search keys.</p>
              <table>
                <tr><td><code>Parfit; Derek Parfit</code></td><td>Two keys, combined with OR</td></tr>
                <tr><td><code>determinis</code></td><td>Last word is prefix-matched: matches <em>determinism</em>, <em>determinist</em>, etc.</td></tr>
                <tr><td><code>isotropic</code></td><td>Without <code>^</code>, first word is suffix-matched: matches <em>anisotropic</em>, <em>isotropic</em>, etc.</td></tr>
                <tr><td><code>^Freud</code></td><td><code>^</code> anchors the first word to an exact match (no suffix matching)</td></tr>
                <tr><td><code>Kant$</code></td><td><code>$</code> anchors the last word to an exact match (no prefix matching)</td></tr>
                <tr><td><code>^Wittgenstein$</code></td><td>Both ends anchored: exact whole-word match</td></tr>
                <tr><td><code>social epistemology</code></td><td>Multi-word keys are matched as an exact phrase, in the order written</td></tr>
              </table>

              <h3 class="cont">Listing Files</h3>
              <p>Leave the query blank to list the files in the scope instead of searching them.</p>
            </td>
          </tr>
        </table>
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

    // Shown in the place of Matches and Excerpt while the table holds a listing (see executeListing)

    colStatus.setCellValueFactory(cellData -> new SimpleStringProperty(nullSwitch(cellData.getValue().searchability(), "", FileSearchability::displayName)));

    colStatus.setCellFactory(col -> new TableCell<>()
    {
      @Override protected void updateItem(String value, boolean empty)
      {
        super.updateItem(value, empty);

        FileSearchability searchability = empty ? null : nullSwitch(nullSwitch(getTableRow(), null, TableRow::getItem), null, FTSResultRow::searchability);

        setText(empty ? null : value);
        setTooltip(searchability == null ? null : makeTooltip(searchability.description()));
      }
    });

    // No highlighting is requested from here: sorting by this column reads every row's value,
    // so a request per row would queue the whole result list. Counts come from the index as
    // rows load; in a record-scoped search they come from highlighting, which the Excerpt
    // column requests for the rows on screen.

    colMatches.setCellValueFactory(cellData ->
    {
      Integer count = hitSetService.matchCount(cellData.getValue().path());
      return new SimpleStringProperty(((count == null) || (count == 0)) ? "" : String.valueOf(count));
    });

    colMatches.setStyle("-fx-alignment: CENTER-RIGHT;");

    colExcerpt.setCellValueFactory(cellData ->
    {
      String path = cellData.getValue().path();
      List<PageMatch> matches = hitSetService.cachedMatches(path);

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

        List<PageMatch> matches = hitSetService.cachedMatches(item.path());

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
      // file. Don't reset the page or re-trigger setPreview (which would scroll the preview back to page 1 and, for converted docs, risk
      // re-queueing work). Just refresh the context view since the row's resolvedRecord may have changed.

      if (newValue.path().equals(currentPreviewPath))
      {
        updateContextView(newValue);
        return;
      }

      currentPreviewPage = 1;
      pendingScrollTarget = null;
      pendingConvertedPassageNdx = -1;
      updateContextView(newValue);  // Context view is cheap; only the preview work is settle-gated
      requestSettledPreview(newValue);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Previews through the settle gate; a deferred preview re-checks at fire
   *  time that its row is still the selected one. */
  private void requestSettledPreview(FTSResultRow row)
  {
    previewSettleGate.request(() ->
    {
      if (row == tvResults.getSelectionModel().getSelectedItem())
        setPreview(row);
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

        // Open the window first so the viewer is initialized, then set a
        // fresh intent against the now-visible pane (hits computed while the
        // window was closed would otherwise never have been requested, since
        // setPreview defers all work while the source is not showing).

        PreviewWindow.show(pvsQueriesTab);
        currentPreviewPath = null;
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

  @FunctionalInterface
  private interface ProgressSearchCall<T>
  {
    T run(HyperTask task) throws ParseException, IndexSearcher.TooManyNestedClauses, CancelledTaskException;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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
    return runSearchTaskImpl(dialogMessage, false, task -> call.run());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The task behind {@link #runSearchTask}, for lambdas that take the {@link HyperTask}.
   *
   * @param withProgressUpdates whether the lambda drives a determinate progress bar through
   *                            {@link HyperTask#updateProgress(double, double)}; otherwise
   *                            the dialog shows an indeterminate spinner
   */
  private static <T> T runSearchTaskImpl(String dialogMessage, boolean withProgressUpdates, ProgressSearchCall<T> call)
  {
    final class SearchTask extends HyperTask
    {
      private volatile T result;
      private volatile ParseException parseError;
      private volatile IndexSearcher.TooManyNestedClauses tooManyError;

      private SearchTask() { super("FTSSearch", dialogMessage, withProgressUpdates); }

      @Override protected void call() throws CancelledTaskException
      {
        try { result = call.run(this); }
        catch (ParseException e)                     { parseError = e; }
        catch (IndexSearcher.TooManyNestedClauses e) { tooManyError = e; }
      }
    }

    SearchTask task = (SearchTask) new SearchTask()
      .setInterruptOnCancel(true)
      .setWaitOnCancel(false)   // Lucene search isn't responsive to interrupt; abandon stale tasks
      .setDaemonThread(true);   // ...and don't let an abandoned task block JVM exit

    if (task.runWithProgressDialog() != State.SUCCEEDED) return null;

    if (task.tooManyError != null)
    {
      errorPopup("A search term expanded to too many variants. " +
                 "Try adding ^ before the first word or $ after the last word (to match it exactly), " +
                 "or use a longer or more specific word.");

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

  /**
   * A progress listener for the indexer's loops: reports {@code offset} plus the number done
   * against {@code total} on the task's progress bar, and stops the work once the task has
   * been cancelled.
   */
  private static IntPredicate progressReporter(HyperTask task, int offset, int total)
  {
    return done ->
    {
      try
      {
        task.updateProgress(offset + done, total);
        return true;
      }
      catch (CancelledTaskException e)
      {
        return false;
      }
    };
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

    if (strNullOrBlank(queryStr))
    {
      executeListing();
      return;
    }

    FullTextIndexer indexer = db.getFullTextIndexer();
    if (indexer == null) return;

    boolean useSearchKey = chkSearchKey.isSelected();

    Query searchKeyQuery = null;
    Function<String, Iterable<Keyword>> searchKeyLookup = null;

    if (useSearchKey)
    {
      searchKeyQuery = FullTextIndexer.buildSearchKeyQuery(queryStr);
      if (searchKeyQuery == null)
      {
        errorPopup("No valid search keys found in the query text.");
        return;
      }

      searchKeyLookup = SearchKeys.buildAdHocLookup(queryStr);
    }
    else if (chkExactPhrase.isSelected() && (queryStr.startsWith("\"") == false))
    {
      queryStr = '"' + queryStr + '"';
    }

    boolean useRecordScope = rbRecordScope.isSelected() && (recordScopeList != null);

    String fileMask = useRecordScope ? null : tfFileMask.getText();

    if ((fileMask != null) && fileMask.isBlank()) fileMask = null;

    Set<String> pathScope = useRecordScope ? recordScopeList.getPathScope() : null;

    String folderPrefix = useRecordScope ? null : computeFolderPrefix();

    // Clear previous state and begin a new hit-set generation for this query

    resetForNewResults(new HitSetService.QueryDescriptor(queryStr, searchKeyQuery, searchKeyLookup,
      useRecordScope ? recordScopeList::filterResults : null));

    // Fast light search: no highlighting, just paths and scores. The searchLight,
    // the per-file match counts and the countMatches all run off the FX thread
    // inside one HyperTask so the UI doesn't freeze on long queries; the counts
    // are in place before the rows reach the table, so a sort by Matches is
    // right from the start.

    String finalQueryStr = queryStr,
           finalFileMask = fileMask;
    Query finalSearchKeyQuery = searchKeyQuery;

    SearchWithTotal result = runSearchTask("Searching...", () ->
    {
      SearchBatch b = useSearchKey
        ? indexer.searchLight(finalSearchKeyQuery, PAGE_SIZE, finalFileMask, pathScope, folderPrefix)
        : indexer.searchLight(finalQueryStr,       PAGE_SIZE, finalFileMask, pathScope, folderPrefix);

      hitSetService.countMatchesFromIndex(pathsOf(b.results()), null);

      int count = -1;

      if (b.hasMore())
      {
        try
        {
          count = useSearchKey
            ? indexer.countMatches(finalSearchKeyQuery, finalFileMask, pathScope, folderPrefix)
            : indexer.countMatches(finalQueryStr,       finalFileMask, pathScope, folderPrefix);
        }
        catch (Exception e) { /* keep -1 */ }
      }

      return new SearchWithTotal(b, count);
    });

    if (result == null) return;

    SearchBatch batch = result.batch();

    lastQueryStr = queryStr;
    lastSearchKeyQuery = searchKeyQuery;
    lastFileMask = fileMask;
    lastFolderPrefix = folderPrefix;
    lastScopeList = useRecordScope ? recordScopeList : null;
    hasMore = batch.hasMore();
    totalMatchCount = hasMore ? result.totalCount() : -1;

    List<SearchResult> lightResults = batch.results();

    lastScoreDoc = lightResults.isEmpty() ? null : lightResults.getLast().scoreDoc();

    tab.setText(queryStr.length() > 20 ? queryStr.substring(0, 20) + "..." : queryStr);

    listingShown = false;
    showListingColumns(false);

    if (useRecordScope)
    {
      // For record-scoped searches, highlight and filter up front so rows with
      // no in-range matches never appear in the table.

      hitSetService.computeMatchesForBatch(lightResults, highlighted -> showResults(rowsForHighlighted(highlighted)));
    }
    else
    {
      // For folder/file-mask searches, show rows immediately; highlighting is lazy.

      showResults(rowsFor(lightResults));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Clears what the previous search or listing left behind, so that the next one's rows meet a
   * clean preview and hit state, and begins a new hit-set generation for {@code query}, which is
   * null for a listing, since a listing has no hits.
   */
  private void resetForNewResults(HitSetService.QueryDescriptor query)
  {
    hitSetService.beginGeneration(query);

    currentPreviewPage = 1;
    pendingScrollTarget = null;
    pendingConvertedPassageNdx = -1;
    previewSettleGate.cancel();
    PreviewWindow.runWhenSourceActivates(pvsQueriesTab, null);
    currentPreviewPath = null;
    convertedAlignment = null;
    convertedAlignmentPath = null;
    convertedLaunchPath = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * What a blank query does: lists the files in the scope instead of searching them. These are
   * every file under the folder whose name the file mask lets through, or every file the record
   * scope holds, from the file registry rather than the index, so the files a search could never
   * find are listed too, and the Index Status column says of each whether a search could find it.
   * Nothing is listed for the whole database with no file mask, since that is every file in it;
   * the status label says so.
   */
  private void executeListing()
  {
    FullTextIndexer indexer = db.getFullTextIndexer();
    if (indexer == null) return;

    if (listingScopeChosen() == false)
    {
      lblStatus.setText("Enter a query, or choose a folder or file mask to list files");
      setAllVisible(false, btnShowMore, btnShowAll);
      return;
    }

    boolean useRecordScope = rbRecordScope.isSelected();

    String maskText     = tfFileMask.getText(),
           fileMask     = (useRecordScope || strNullOrBlank(maskText)) ? null : maskText,
           folderPrefix = useRecordScope ? null : computeFolderPrefix();

    SearchResultFileList scopeList = useRecordScope ? recordScopeList : null;

    List<FTSResultRow> rows;

    if (scopeList != null)
      rows = listedRowsForScope(indexer, scopeList);
    else
    {
      // The registry scan reads the disk once per file to drop the entries no file is behind, so
      // it runs off the FX thread as a search does; the rows' records are resolved on it, as a
      // search's are. Unlike a search, a listing has no hit counts to record during the task, so
      // the previous results' state is cleared only once the listing is in hand.

      List<ListedFile> listed = runSearchTask("Listing files...", () -> indexer.listFiles(folderPrefix, fileMask));
      if (listed == null) return;

      rows = listedRowsForFolder(listed);
    }

    resetForNewResults(null);

    lastQueryStr = null;
    lastSearchKeyQuery = null;
    lastFileMask = fileMask;
    lastFolderPrefix = folderPrefix;
    lastScopeList = scopeList;
    lastScoreDoc = null;
    hasMore = false;
    totalMatchCount = -1;

    tab.setText(listingTitle(scopeList != null, folderPrefix, fileMask));

    listingShown = true;
    showListingColumns(true);
    showResults(rows);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Whether the scope controls describe something a blank query can list: a record scope, a
   *  folder other than the database root, or a file mask. */
  private boolean listingScopeChosen()
  {
    if (rbRecordScope.isSelected()) return recordScopeList != null;

    return (computeFolderPrefix() != null) || strNotNullOrBlank(tfFileMask.getText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Whether the Search button would list files rather than search them. */
  private boolean listingRequested()
  {
    return strNullOrBlank(tfQuery.getText()) && listingScopeChosen();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Captions the Search button with what it would do. */
  private void updateSearchButtonCaption()
  {
    btnSearch.setText(listingRequested() ? "List files" : "Search");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Shows a listing's Index Status column in the place of a search's Matches and Excerpt
   *  columns, or the reverse. */
  private void showListingColumns(boolean listing)
  {
    colStatus .setVisible(listing);
    colMatches.setVisible(listing == false);
    colExcerpt.setVisible(listing == false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The tab title of a listing, in the place of a search's query text: the folder, or the file
   *  mask and the folder, or the record scope. */
  private String listingTitle(boolean recordScope, String folderPrefix, String fileMask)
  {
    if (recordScope) return "Scoped files";

    String folderName = (folderPrefix == null) ? "database" : db.getRootPath(folderPrefix.substring(0, folderPrefix.length() - 1)).getNameOnly().toString(),
           title      = (fileMask == null) ? ("Files in " + folderName) : (fileMask + " in " + folderName);

    return title.length() > 20 ? title.substring(0, 20) + "..." : title;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A row for a listed file: a result with an empty match list rather than none, since a
   *  listing has no hits to compute. */
  private static FTSResultRow listedRow(ListedFile listed, HDT_RecordWithFilePath record)
  {
    return new FTSResultRow(new SearchResult(listed.path(), 0f, List.of(), null), record, listed.searchability());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A row for each listed file, with the record resolved from its path as a search's rows are. */
  private static List<FTSResultRow> listedRowsForFolder(List<ListedFile> listed)
  {
    List<FTSResultRow> rows = new ArrayList<>(listed.size());

    for (ListedFile file : listed)
      rows.add(listedRow(file, HyperPath.resolveRecordForPage(db.getRootPath(file.path()), 0)));

    return rows;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A row for each file of a record scope, in each page range the scope holds it in. The record
   * is the most specific work covering the range, as a search's row would resolve it from its
   * matches' pages, so an anthology in scope for one of its chapters shows that chapter's work.
   */
  private static List<FTSResultRow> listedRowsForScope(FullTextIndexer indexer, SearchResultFileList scopeList)
  {
    List<FTSResultRow> rows = new ArrayList<>();

    for (ScopedFile scoped : scopeList.getScopedFiles())
    {
      ListedFile listed = indexer.listFile(scoped.filePath());
      if (listed == null) continue;

      HDT_RecordWithFilePath fallback = HyperPath.resolveRecordForPage(scoped.filePath(), 0),
                             record   = scoped.hasPageRestriction()
                                          ? HDT_WorkFile.resolveRecordForPages(scoped.filePath(), IntStream.of(scoped.startPage(), scoped.endPage()), fallback)
                                          : fallback;

      rows.add(listedRow(listed, record));
    }

    return rows;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showResults(List<FTSResultRow> rows)
  {
    List<FTSResultRow> shown = rows;

    // A table still sorted by match count from an earlier search ranks these results the
    // same way, over all of them, so the rest are loaded before any are shown; if that is
    // cancelled, the sort is dropped rather than shown over the first page alone

    if (isSortedByMatches() && hasMore)
    {
      List<FTSResultRow> rest = loadMoreResults(true, rows.size());

      if (rest == null)
        tvResults.getSortOrder().clear();
      else
      {
        shown = new ArrayList<>(rows);
        shown.addAll(rest);
      }
    }

    allRows.setAll(shown);

    if (allRows.isEmpty())
      loadContextHtml("");
    else
      tvResults.getSelectionModel().selectFirst();

    updateStatusLabel();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Captures the query parameters and indexer reference needed to fetch
   *  successive {@link SearchBatch}es after the initial search. Built once at
   *  the start of {@link #loadMoreResults} and reused for every batch in the
   *  load. */
  private record FetchContext(FullTextIndexer indexer, Query searchKeyQuery, String queryStr,
                              String fileMask, String folderPrefix, Set<String> pathScope)
  {
    private SearchBatch fetch(ScoreDoc cursor, int maxResults, IntPredicate onProgress) throws ParseException, IndexSearcher.TooManyNestedClauses
    {
      return (searchKeyQuery != null)
        ? indexer.searchLightAfter(cursor, searchKeyQuery, maxResults, fileMask, pathScope, folderPrefix, onProgress)
        : indexer.searchLightAfter(cursor, queryStr,       maxResults, fileMask, pathScope, folderPrefix, onProgress);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Builds a {@link FetchContext} from the controller's current last-search
   *  state. Returns {@code null} if the indexer is unavailable. */
  private FetchContext buildFetchContext()
  {
    FullTextIndexer indexer = db.getFullTextIndexer();
    if (indexer == null) return null;

    return new FetchContext(indexer, lastSearchKeyQuery, lastQueryStr,
                            lastFileMask, lastFolderPrefix,
                            (lastScopeList != null) ? lastScopeList.getPathScope() : null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A row for each light {@link SearchResult}, with the record resolved from its file path. */
  private static List<FTSResultRow> rowsFor(List<SearchResult> results)
  {
    List<FTSResultRow> rows = new ArrayList<>(results.size());

    for (SearchResult sr : results)
      rows.add(new FTSResultRow(sr, HyperPath.resolveRecordForPage(db.getRootPath(sr.path()), 0)));

    return rows;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A row for each highlighted {@link SearchResult} of a record-scoped search, with the
   *  record narrowed to the most specific work covering all of its match pages. */
  private static List<FTSResultRow> rowsForHighlighted(List<SearchResult> highlighted)
  {
    List<FTSResultRow> rows = new ArrayList<>(highlighted.size());

    for (SearchResult sr : highlighted)
    {
      FilePath filePath = db.getRootPath(sr.path());
      List<PageMatch> matches = sr.pageMatches();
      IntStream pages = (matches != null) ? matches.stream().mapToInt(PageMatch::pageNumber) : IntStream.empty();

      rows.add(new FTSResultRow(sr, HDT_WorkFile.resolveRecordForPages(filePath, pages, HyperPath.resolveRecordForPage(filePath, 0))));
    }

    return rows;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showMore() { appendLoadedResults(loadMoreResults(false, allRows.size())); }
  private void showAll () { appendLoadedResults(loadMoreResults(true , allRows.size())); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Adds the rows {@link #loadMoreResults} loaded, if it was not cancelled. */
  private void appendLoadedResults(List<FTSResultRow> rows)
  {
    if (rows == null) return;

    allRows.addAll(rows);
    updateStatusLabel();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Carries the results a load fetched plus the cursor and hasMore state it ended with. */
  private record LoadedResults(List<SearchResult> results, ScoreDoc lastScoreDoc, boolean hasMore) {}

  /**
   * Loads the next page of results, or every result not loaded yet, as rows ready for the
   * table: counted, and in a record-scoped search highlighted and filtered as its first page
   * was, so that every row has its count. Advances the result cursor.
   *
   * @param all         whether to load every remaining result instead of one page
   * @param loadedCount how many results are loaded already; not always the size of
   *                    {@link #allRows}, since a new search's rows may not be in it yet
   * @return the rows, or null if there was nothing to load, or loading was cancelled or failed
   */
  private List<FTSResultRow> loadMoreResults(boolean all, int loadedCount)
  {
    if ((lastScoreDoc == null) || (hasMore == false)) return null;

    FetchContext ctx = buildFetchContext();
    if (ctx == null) return null;

    ScoreDoc startAfter = lastScoreDoc;
    boolean recordScope = lastScopeList != null;

    // Every search looks up the terms the query's wildcards match, which takes a while in
    // a large index, so when every result is wanted and the total is known, they come in
    // one search instead of a batch per page. The total runs short only when rows have
    // been removed (record scope); the loop then fetches what is left.

    int batchSize = all ? Math.max(PAGE_SIZE, totalMatchCount - loadedCount) : PAGE_SIZE,
        expected  = all ? Math.max(1        , totalMatchCount - loadedCount) : PAGE_SIZE,
        totalWork = 2 * expected;  // Each result is read from the index, then counted or highlighted

    LoadedResults outcome = runSearchTaskImpl(all ? "Loading all results..." : "Loading more results...", true, task ->
    {
      List<SearchResult> accumulated = new ArrayList<>();
      ScoreDoc cursor = startAfter;
      boolean moreFlag;

      do
      {
        SearchBatch batch = ctx.fetch(cursor, batchSize, progressReporter(task, accumulated.size(), totalWork));
        List<SearchResult> batchResults = batch.results();

        moreFlag = batch.hasMore();

        if (batchResults.isEmpty()) break;

        accumulated.addAll(batchResults);
        cursor = batchResults.getLast().scoreDoc();
      }
      while (all && moreFlag);

      HyperTask.throwExceptionIfCancelled(task);  // A cancelled search returns what it had read

      if (recordScope == false)
      {
        hitSetService.countMatchesFromIndex(pathsOf(accumulated), progressReporter(task, expected, totalWork));
        return new LoadedResults(accumulated, cursor, moreFlag);
      }

      // Record scope: highlight and filter now, as the first page was, so that rows with no
      // in-range matches never appear and every row has its count

      List<SearchResult> highlighted = new ArrayList<>();

      for (int ndx = 0; ndx < accumulated.size(); ndx++)
      {
        SearchResult sr = accumulated.get(ndx);
        List<PageMatch> matches = hitSetService.computeMatchesNow(sr.path());

        if ((matches != null) && (matches.isEmpty() == false))
          highlighted.add(new SearchResult(sr.path(), sr.score(), matches, sr.scoreDoc()));

        task.updateProgress(expected + ndx + 1, totalWork);
      }

      return new LoadedResults(highlighted, cursor, moreFlag);
    });

    if (outcome == null) return null;  // cancelled / parse error / too-many-clauses

    lastScoreDoc = outcome.lastScoreDoc();
    hasMore = outcome.hasMore();

    return recordScope ? rowsForHighlighted(outcome.results()) : rowsFor(outcome.results());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Whether the table is sorted by match count (first, if it is sorted by several columns). */
  private boolean isSortedByMatches()
  {
    List<TableColumn<FTSResultRow, ?>> sortOrder = tvResults.getSortOrder();

    return (sortOrder.isEmpty() == false) && (sortOrder.getFirst() == colMatches);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The table's sort policy, which sets the rows' comparator. The rows are not simply bound
   * to the table's comparator because JavaFX sets that before it runs the policy, so bound
   * rows would re-sort the instant a header is clicked. A sort by match count has to cover
   * every result, so a click on the Matches header while results are still unloaded is
   * refused here, which makes JavaFX take the column back out of the sort order with
   * nothing re-sorted; the sort is then made once the rest are loaded.
   */
  private boolean sortRows(TableView<FTSResultRow> table)
  {
    if (isSortedByMatches() && hasMore)
    {
      Platform.runLater(this::sortByMatchesAfterLoadingAll);
      return false;
    }

    sortedRows.setComparator(table.getComparator());
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Loads every result not loaded yet, then sorts by match count with the most matches
   * first, which is what a sort by match count is for (a column's first click would sort
   * it ascending). If loading is cancelled, the table stays as it was.
   */
  private void sortByMatchesAfterLoadingAll()
  {
    if (hasMore)
    {
      List<FTSResultRow> rows = loadMoreResults(true, allRows.size());
      if (rows == null) return;

      appendLoadedResults(rows);
    }

    colMatches.setSortType(TableColumn.SortType.DESCENDING);
    tvResults.getSortOrder().setAll(List.of(colMatches));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Adds the current FTS row(s) to the given file list. With
   * {@code onlySelected=true} this contributes the single selected row
   * (FTS uses single-select); with {@code onlySelected=false} it contributes
   * every row in {@link #allRows}. Files are added without page bounds:
   * each FTS row already represents a specific file the user picked.
   */
  void populateFileList(SearchResultFileList fileList, boolean onlySelected)
  {
    List<FTSResultRow> rows;

    if (onlySelected)
    {
      FTSResultRow selected = tvResults.getSelectionModel().getSelectedItem();
      rows = (selected == null) ? List.of() : List.of(selected);
    }
    else
    {
      rows = List.copyOf(allRows);  // snapshot to avoid concurrent mutation by the highlight executor
    }

    for (FTSResultRow row : rows)
    {
      FilePath filePath = db.getRootPath(row.path());
      if (FilePath.isEmpty(filePath) == false)
        fileList.addFilePath(filePath);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void requestHighlight(String path)
  {
    hitSetService.requestMatches(path, matches ->
    {
      applyMatchesToRow(path, matches);
      showNewMatches(Set.of(path));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Updates the row of a file whose matches were just computed: drops it from a
   * record-scoped search if none of its matches are within the page ranges, and otherwise
   * narrows its Record column to the most specific work covering all the match pages.
   */
  private void applyMatchesToRow(String path, List<PageMatch> matches)
  {
    if ((lastScopeList != null) && matches.isEmpty())
    {
      allRows.removeIf(row -> row.path().equals(path));
      return;
    }

    for (int ndx = 0; ndx < allRows.size(); ndx++)
    {
      FTSResultRow row = allRows.get(ndx);

      if (row.path().equals(path))
      {
        FilePath filePath = db.getRootPath(path);
        IntStream pages = matches.stream().mapToInt(PageMatch::pageNumber);
        HDT_RecordWithFilePath newRecord = HDT_WorkFile.resolveRecordForPages(filePath, pages, row.resolvedRecord());

        if (newRecord != row.resolvedRecord())
          allRows.set(ndx, new FTSResultRow(row.result(), newRecord, row.searchability()));

        return;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Shows matches just computed for these files: refreshes the table and status, and if one
   * of the files is selected, its context view and preview.
   */
  private void showNewMatches(Collection<String> paths)
  {
    tvResults.refresh();
    updateStatusLabel();

    FTSResultRow selected = tvResults.getSelectionModel().getSelectedItem();
    if ((selected != null) && paths.contains(selected.path()))
    {
      updateContextView(selected);

      // Re-derive the preview now that match data exists: the intent is
      // refreshed and the newly-computed hits ship to the pane

      setPreview(selected);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateContextView(FTSResultRow selectedRow)
  {
    String path = selectedRow.path();

    if (selectedRow.isListed())
    {
      // A listed file has no passages; the pane says what a search would make of the file instead

      loadContextHtml(FTSContextPaneRenderer.renderListedFile(path, selectedRow.searchability(), scopedPageRanges(path),
                                                              HDT_WorkFile.getBoundariesForFile(db.getRootPath(path)), selectedRow.resolvedRecord()));
      return;
    }

    List<PageMatch> matches = hitSetService.cachedMatches(path);

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

  /** The page ranges of the file that a record-scoped listing holds it in, formatted; empty when
   *  the whole file is in scope or the listing is not record-scoped. */
  private List<String> scopedPageRanges(String path)
  {
    if (lastScopeList == null) return List.of();

    FilePath filePath = db.getRootPath(path);

    return lastScopeList.getScopedFiles().stream()
      .filter(scoped -> scoped.hasPageRestriction() && scoped.filePath().equals(filePath))
      .map(scoped -> formatPageRange(scoped.startPage(), scoped.endPage()))
      .toList();
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

    List<PageMatch> matches = hitSetService.cachedMatches(selected.path());
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
           hiddenSuffix = hiddenCount > 0 ? " (" + hiddenCount + " unassociated hidden)" : "",
           noneFound = listingShown ? "No files found" : "No matches found";

    if (totalCount == 0)
    {
      lblStatus.setText(noneFound);
      setAllVisible(false, btnShowMore, btnShowAll);
    }
    else if (shownCount == 0)
    {
      lblStatus.setText(noneFound + hiddenSuffix);
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

  public void setQueryTextAndSearch(String text, boolean asSearchKey)
  {
    tfQuery.setText(text);
    chkSearchKey.setSelected(asSearchKey);
    rbFolderScope.setSelected(true);
    executeSearch();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * @param radioCaption replacement caption for the scope radio button, describing where
   * the scope came from; null keeps the default ("By record query results")
   * @param includeEdited initial state of the "Include edited works" checkbox; must match
   * the includeEdited value the caller built {@code scopeList} with so the checkbox and the
   * displayed file set agree
   */
  void setRecordScope(SearchResultFileList scopeList, List<HDT_RecordWithFilePath> sourceRecords, String radioCaption, boolean includeEdited)
  {
    recordScopeRecords = sourceRecords;

    // Reflect how the caller built the scope list so the checkbox and the displayed file set
    // agree. Setting this may fire the listener (rebuildRecordScope); assigning recordScopeList
    // afterward keeps the caller's list authoritative.

    chkIncludeEdited.setSelected(includeEdited);

    recordScopeList = scopeList;

    lblRecordScope.setText(scopeList.getSummary());

    if (radioCaption != null)
      rbRecordScope.setText(radioCaption);

    disableAllIff(false, rbRecordScope, chkIncludeEdited);
    setAllVisible(true , rbRecordScope, btnViewScope);
    rbRecordScope.setSelected(true);
    updateSearchButtonCaption();  // The radio may have been selected already, with no scope list yet

    Platform.runLater(tfQuery::requestFocus);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Scopes the search to a single file. Reuses the record-scope path filtering
   * mechanism with a one-file scope list; "Include edited works" is hidden because
   * it only applies when the scope came from record query results.
   */
  void setFileScope(FilePath filePath)
  {
    SearchResultFileList scopeList = new SearchResultFileList(false, true);
    scopeList.addFilePath(filePath);

    recordScopeList = scopeList;
    recordScopeRecords = null;

    lblRecordScope.setText(filePath.toDbRelativeStr());

    rbRecordScope.setText("Single file");
    rbRecordScope.setDisable(false);
    setAllVisible(false, chkIncludeEdited);
    rbRecordScope.setSelected(true);
    updateSearchButtonCaption();

    Platform.runLater(tfQuery::requestFocus);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rebuildRecordScope()
  {
    if (recordScopeRecords == null) return;

    SearchResultFileList scopeList = new SearchResultFileList(false, chkIncludeEdited.isSelected());

    for (HDT_RecordWithFilePath record : recordScopeRecords)
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
        String[] parts = data.substring(5).split(":");
        currentPreviewPage = Math.max(parseInt(parts[0], 1), 1);

        int passageNdx = (parts.length > 1) ? parseInt(parts[1], -1) : -1;

        FTSResultRow selected = tvResults.getSelectionModel().getSelectedItem();

        if (selected != null)
        {
          // For converted PDFs, map the passage index to a viewer page via the
          // Tika/pdf.js normalized-text alignment the hit pipeline computed.

          if ((passageNdx >= 0) && (convertedAlignment != null) && selected.path().equals(convertedAlignmentPath))
          {
            List<PageMatch> tikaMatches = hitSetService.cachedMatches(selected.path());
            if (tikaMatches == null) tikaMatches = selected.result().pageMatches();

            int targetPage = ((tikaMatches == null) || (passageNdx >= tikaMatches.size()))
              ? -1
              : convertedAlignment.pageForPassage(tikaMatches.get(passageNdx));

            if (targetPage > 0)
            {
              currentPreviewPage = targetPage;
              setPreview(selected);
              return;
            }
          }

          // Scroll to the highlight for this passage: the target is computed
          // here but rides the preview intent, so it is delivered once the
          // document and its highlights are in place; a click made while the
          // preview window is closed survives the deferral and the window
          // opens scrolled to the clicked match. Direct content uses the global
          // match index (each highlight span carries a data-match-ndx attribute
          // in matches-list order); the PDF viewer is addressed by (page, index
          // within that page), since the global list order is not
          // reconstructible there.

          if (passageNdx >= 0)
          {
            if (isOfficeDocConvertedToPdf(db.getRootPath(selected.path())))
            {
              // No match target from here: these Tika-side numbers cannot
              // address the converted artifact's viewer coordinates. Stash the
              // passage instead; when the hit pipeline publishes the
              // alignment, applyStashedConvertedPassage derives the clicked
              // passage's viewer page and re-sets the intent to it (the
              // open-from-closed and mid-pipeline cases; aligned clicks were
              // already navigated by page above).

              pendingConvertedPassageNdx = passageNdx;
            }
            else
            {
              List<PageMatch> matches = nullSwitch(hitSetService.cachedMatches(selected.path()), selected.result().pageMatches());

              int matchNdx = 0, pageNum = -1, ndxOnPage = 0;

              if ((matches != null) && (passageNdx < matches.size()))
              {
                pageNum = matches.get(passageNdx).pageNumber();

                for (int ndx = 0; ndx < passageNdx; ndx++)
                {
                  PageMatch match = matches.get(ndx);
                  int rangeCount = (match.hitRanges() != null) ? match.hitRanges().size() : 0;

                  matchNdx += rangeCount;

                  if (match.pageNumber() == pageNum)
                    ndxOnPage += rangeCount;
                }
              }

              pendingScrollTarget = ScrollTarget.of(matchNdx, pageNum, ndxOnPage);
            }
          }

          setPreview(selected);
        }
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

  /** Hands off the pending clicked-match target to the intent that carries it;
   *  the deferral and early-return paths of {@link #setPreview} deliberately do
   *  not consume, so a later re-invocation for the same row still carries it. */
  private ScrollTarget consumeScrollTarget()
  {
    ScrollTarget target = pendingScrollTarget;
    pendingScrollTarget = null;
    return target;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPreview(FTSResultRow row)
  {
    if ((row == null) || (db.getRootPath(row.path()).exists() == false))
    {
      PreviewWindow.runWhenSourceActivates(pvsQueriesTab, null);
      currentPreviewPath = null;
      pendingScrollTarget = null;
      pendingConvertedPassageNdx = -1;

      if (PreviewWindow.isPreviewLocked(pvsQueriesTab))
      {
        // Locked: keep the current preview; the clear replays on unlock (unless
        // a later selection replaces it)

        PreviewWindow.runWhenUnlocked(pvsQueriesTab, PreviewWindow::clearQueriesFtsPreview);
        return;
      }

      PreviewWindow.clearQueriesFtsPreview();
      return;
    }

    // The lock defers previews of OTHER files; re-previews of the locked file
    // itself (passage clicks navigating within it) proceed as page navigation

    if (PreviewWindow.isPreviewLocked(pvsQueriesTab) && (row.path().equals(currentPreviewPath) == false))
    {
      // Keep the current preview and replay this row's preview on unlock, with
      // the same context re-checks as the activation replay below

      PreviewWindow.runWhenUnlocked(pvsQueriesTab, () ->
      {
        if (tab.isSelected() && (tvResults.getSelectionModel().getSelectedItem() == row))
          setPreview(row);
      });

      return;
    }

    if (PreviewWindow.isSourceActiveAndShowing(pvsQueriesTab) == false)
    {
      currentPreviewPath = null;
      PreviewWindow.clearQueriesFtsPreview();

      // The replay verifies its context is still current before firing: pvsQueriesTab is shared with
      // the non-FTS QueryCtrlr (which knows nothing about this deferral), so a stale replay would
      // otherwise load this row over a preview the user requested from a different queries sub-tab.
      // The tab.isSelected() check also covers this sub-tab having been closed in the meantime.

      PreviewWindow.runWhenSourceActivates(pvsQueriesTab, () ->
      {
        if (tab.isSelected() && (tvResults.getSelectionModel().getSelectedItem() == row))
          setPreview(row);
      });

      return;
    }

    FilePath filePath = db.getRootPath(row.path());

    if (row.isListed())
    {
      // A listed file has no hits, so it shows at once at the current page with none, a converted
      // office document included: with no hit set to wait for, the pane does not withhold it

      boolean paged = isOfficeDocConvertedToPdf(filePath) || getMediaType(filePath).toString().contains("pdf");

      currentPreviewPath = row.path();

      PreviewWindow.setQueriesFtsPreview(filePath, row.resolvedRecord(), paged, paged ? Math.max(currentPreviewPage, 1) : 1, false, consumeScrollTarget());
      return;
    }

    List<PageMatch> matches = nullSwitch(row.result().pageMatches(), hitSetService.cachedMatches(row.path()));

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

    // Converted office: the pane withholds the display until the hit pipeline
    // determines the first-match page (intent page -1 = derive from hit set).
    // Skip entirely until matches are available; requestHighlight's callback
    // will re-invoke setPreview once the cache is populated.

    if (isOfficeDocConvertedToPdf(filePath))
    {
      if (matches == null) return;

      FullTextIndexer indexer = db.getFullTextIndexer();
      if (indexer == null) return;

      // "Already launched" must also mean the preview never left this file: on
      // any navigation away, the pane host discards the delivered hit status
      // (hitsStatus resets to Pending for the new file), so a revisit needs the
      // pipeline relaunched even though the conversion itself is cached; without
      // this, the revisited document displays but its hits never arrive.

      boolean alreadyLaunched = row.path().equals(convertedLaunchPath) && row.path().equals(currentPreviewPath);

      currentPreviewPath = row.path();

      // The first display derives its page from the hit set; subsequent
      // same-file navigation (passage clicks) honors the explicit page

      PreviewWindow.setQueriesFtsPreview(filePath, row.resolvedRecord(), true,
        alreadyLaunched ? currentPreviewPage : -1, true, consumeScrollTarget());

      if (alreadyLaunched == false)
      {
        convertedLaunchPath = row.path();
        launchConvertedHitPipeline(row.path(), filePath, indexer);
      }

      return;
    }

    // Native PDF / direct content: the intent shows the file immediately at
    // an explicit page; hits attach when computed

    FullTextIndexer indexer = db.getFullTextIndexer();
    int[] pageOffsets = (indexer == null) ? null : indexer.getPageOffsets(row.path());

    // Kind comes from the mimetype, not from whether the index has page
    // offsets: a PDF must go to the paged viewer even if offsets are missing
    // (it then simply displays without page-addressed hits)

    boolean paged = (pageOffsets != null) || getMediaType(filePath).toString().contains("pdf");

    currentPreviewPath = row.path();

    PreviewWindow.setQueriesFtsPreview(filePath, row.resolvedRecord(), paged, paged ? Math.max(currentPreviewPage, 1) : 1, matches != null, consumeScrollTarget());

    if ((matches == null) || (indexer == null)) return;

    HitSetService.TextSource source = HitSetService.TextSource.of(indexer);

    if (paged)
    {
      HitSetService.PagedHits hits = HitSetService.pdfHits(source, row.path(), matches);
      PreviewWindow.updateQueriesFtsHitsPaged(filePath, hits == null ? null : hits.hitsJson(), -1);
    }
    else
    {
      HitSetService.DirectHits hits = HitSetService.directContentHits(source, row.path(), matches);
      PreviewWindow.updateQueriesFtsHitsDirect(filePath, hits == null ? null : hits.hitsJson());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs the converted-office hit pipeline for the current query on the hit
   * service's worker thread: joins the content-keyed conversion, extracts the
   * converted PDF's text, computes hits and the passage-click alignment, and
   * pushes the results to the queries pane. A hit-pipeline failure pushes a
   * failed hit status, which the pane degrades to an unhighlighted page-1
   * display; conversion failures themselves surface through the pane's
   * artifact side.
   */
  private void launchConvertedHitPipeline(String indexPath, FilePath filePath, FullTextIndexer indexer)
  {
    String mimetypeStr = getMediaType(filePath).toString();

    ConversionSession session = PreviewWindow.getOrCreateSession(pvsQueriesTab, mimetypeStr, filePath);

    if (session == null)
    {
      PreviewWindow.updateQueriesFtsHitsFailed(filePath);
      return;
    }

    CompletableFuture<FilePath> extractionFuture = session.subscribeExtraction();
    PreviewWindow.enqueueForConversion(session);

    HitSetService.QueryDescriptor query = hitSetService.query();

    hitSetService.execute(() ->
    {
      FilePath convertedPath;

      try { convertedPath = extractionFuture.get(60, TimeUnit.SECONDS); }
      catch (Exception e)
      {
        // Cancellation means this request was superseded, interruption means the
        // executor is shutting down, and the no-office failure is a settings
        // condition the artifact side already reports specifically; anything
        // else is a real conversion failure worth recording.

        if   (((e instanceof CancellationException)
           ||  (e instanceof InterruptedException)
           ||  (e.getCause() instanceof CancellationException)
           ||  (e.getCause() instanceof NoOfficeInstallationException)) == false)
          logThrowable(e);

        Platform.runLater(() -> PreviewWindow.updateQueriesFtsHitsFailed(filePath));
        return;
      }

      HitSetService.PagedHits hits;

      try
      {
        String dbRootPathStr = db.isLoaded() ? db.getRootPath().toString().replace('/', '\\') : null;

        hits = HitSetService.computeConvertedPdfHits(HitSetService.TextSource.of(indexer), query, indexPath, convertedPath, dbRootPathStr);
      }
      catch (Throwable e)
      {
        logThrowable(e);
        Platform.runLater(() -> PreviewWindow.updateQueriesFtsHitsFailed(filePath));
        return;
      }

      Platform.runLater(() ->
      {
        if (hits == null)
        {
          PreviewWindow.updateQueriesFtsHitsFailed(filePath);
          return;
        }

        // Publish the alignment unconditionally; passage-click navigation must
        // never depend on whether highlights were applied

        convertedAlignment = hits.alignment();
        convertedAlignmentPath = indexPath;

        // Before the hits: the display is withheld until they arrive, so an
        // intent re-set here paints directly at the clicked passage's page
        // instead of flashing the first-match page first

        applyStashedConvertedPassage(indexPath, filePath);

        PreviewWindow.updateQueriesFtsHitsPaged(filePath, hits.hitsJson(), hits.firstMatchPage());
      });
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Applies a converted-office passage click that could not be navigated at
   * click time because the alignment did not exist yet (the preview window
   * was closed, or the pipeline was still running): now that the pipeline has
   * published the alignment, derive the clicked passage's viewer page and
   * re-set the intent to it, so the document displays at the clicked
   * passage's page instead of the derived first-match page. The scroll target
   * centers the first highlight on that page (best-effort; the exact match is
   * not addressable in viewer coordinates).
   */
  private void applyStashedConvertedPassage(String indexPath, FilePath filePath)
  {
    int passageNdx = pendingConvertedPassageNdx;
    pendingConvertedPassageNdx = -1;

    if (passageNdx < 0) return;

    // Same context checks as the deferred-preview replay: the click must still
    // describe the current sub-tab, the selected row, and a showing pane

    if ((tab.isSelected() == false) || (PreviewWindow.isSourceActiveAndShowing(pvsQueriesTab) == false)) return;

    FTSResultRow row = tvResults.getSelectionModel().getSelectedItem();
    if ((row == null) || (row.path().equals(indexPath) == false)) return;

    List<PageMatch> tikaMatches = hitSetService.cachedMatches(indexPath);
    if (tikaMatches == null) tikaMatches = row.result().pageMatches();

    if ((tikaMatches == null) || (passageNdx >= tikaMatches.size())) return;

    int targetPage = convertedAlignment.pageForPassage(tikaMatches.get(passageNdx));
    if (targetPage < 1) return;

    currentPreviewPage = targetPage;

    PreviewWindow.setQueriesFtsPreview(filePath, row.resolvedRecord(), true, targetPage, true, ScrollTarget.of(0, targetPage, 0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void removeRecord(HDT_Record record)
  {
    for (int ndx = 0; ndx < allRows.size(); ndx++)
    {
      FTSResultRow row = allRows.get(ndx);
      if (row.resolvedRecord() == record)
        allRows.set(ndx, new FTSResultRow(row.result(), null, row.searchability()));
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
