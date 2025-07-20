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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.query.GeneralQueries.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;
import java.text.ParseException;
import java.util.*;
import java.util.function.Supplier;

import java.util.Map.Entry;

import org.hypernomicon.App;
import org.hypernomicon.HyperTask;
import org.hypernomicon.Const.TablePrefKey;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.query.Query;
import org.hypernomicon.query.QueryType;
import org.hypernomicon.query.reports.ReportEngine;
import org.hypernomicon.query.reports.ReportTable;
import org.hypernomicon.query.sources.*;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.HyperFavorites.QueryRow;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.populators.QueryPopulator.QueryCell;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.OneTouchExpandableWrapper.CollapsedState;
import org.hypernomicon.util.boolEvaluator.BoolEvaluator;
import org.hypernomicon.util.boolEvaluator.BoolExpression;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.concurrent.Worker.State;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public final class QueryCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apDescription, apResults;
  @FXML private Button btnResetFields;
  @FXML private SplitPane spMain, spLower;
  @FXML private TableView<HyperTableRow> tvFields;
  @FXML private TableView<ResultRow> tvResults;
  @FXML private TextField tfCustomLogic;
  @FXML private ToggleButton btnCustom, btnAnd, btnOr;
  @FXML private ToggleGroup tgLogic;

  private final List<ResultRow> resultsBackingList = new ArrayList<>();
  private final Map<HDT_Record, ResultRow> recordToRow = new HashMap<>();

  private final QueriesTabCtrlr queriesTabCtrlr;
  private final WebView webView;
  private final TabPane tabPane;
  private final TextField tfFavName;

  private HyperTable htFields;
  private ReportTable reportTable;
  private Tab tab;
  private QueryFavorite fav = null;
  private OneTouchExpandableWrapper lowerOneTouchExpandableWrapper;
  private HDT_Record curResult = null;
  private int scrollPosPriorToBeingDeactivated = 0;

  private ResultsTable resultsTable;

  private boolean programmaticFavNameChange = false,
                  programmaticCustomLogicChange = false,
                  disableAutoShowDropdownList = false,
                  inRecordMode = true,
                  searchLinkedRecords;

  public static final int ROW_NUMBER_COL_NDX = 0,
                          QUERY_TYPE_COL_NDX = 1,
                          QUERY_COL_NDX      = 2,
                          OPERAND_1_COL_NDX  = 3,
                          OPERAND_2_COL_NDX  = 4,
                          OPERAND_3_COL_NDX  = 5;

  static final int[] OPERAND_COL_INDICES = {OPERAND_1_COL_NDX, OPERAND_2_COL_NDX, OPERAND_3_COL_NDX};

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<ResultRow> results()        { return inReportMode() ? List.of() : Collections.unmodifiableList(resultsBackingList); }
  void saveColumnWidths()                 { HyperTable.saveColWidthsForTable(tvFields, tvFields.getColumns(), TablePrefKey.QUERY_FIELDS); }
  void focusOnFields()                    { safeFocus(tvFields); }
  public boolean inReportMode()           { return inRecordMode == false; }
  Tab getTab()                            { return tab; }
  HDT_Record getRecord()                  { return curResult; }
  public boolean getSearchLinkedRecords() { return searchLinkedRecords; }
  TableView<ResultRow> getResultsTV()     { return tvResults; }

  private static Query<?> getQuery(HyperTableRow row)      { return row.getCell(QUERY_COL_NDX) instanceof QueryCell queryCell ? queryCell.getQuery() : null; }
  private static QueryType getQueryType(HyperTableRow row) { return QueryType.codeToVal(row.getID(QUERY_TYPE_COL_NDX)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  QueryCtrlr(QueriesTabCtrlr queriesTabCtrlr, WebView webView, TabPane tabPane, TextField tfFavName)
  {
    this.queriesTabCtrlr = queriesTabCtrlr;
    this.webView = webView;
    this.tabPane = tabPane;
    this.tfFavName = tfFavName;

    EventHandler<ActionEvent> onAction = event ->
    {
      queriesTabCtrlr.btnExecute.requestFocus();
      btnExecuteClick(true);
    };

    FXMLLoader loader = new FXMLLoader(App.class.getResource("query/Query.fxml"), null, null, klass -> this);

    try { tab = new Tab("New query", loader.load()); }
    catch (IOException e)
    {
      internalErrorPopup(90203);
      return;
    }

    tabPane.getTabs().add(tabPane.getTabs().size() - 1, tab);
    tab.setOnCloseRequest(event -> queriesTabCtrlr.deleteView((Tab) event.getSource()));

    forceToggleSelection(tgLogic);

    tfCustomLogic.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (programmaticCustomLogicChange || (newValue == null) || newValue.strip().equals(stripSafe(oldValue))) return;

      tgLogic.selectToggle(btnCustom);
    });

    htFields = new HyperTable(tvFields, QUERY_COL_NDX, true, "");

    HyperTable.loadColWidthsForTable(tvFields, tvFields.getColumns(), TablePrefKey.QUERY_FIELDS);

    htFields.autoCommitListSelections = true;

    htFields.addLabelColWithAlignment(hdtNone, Pos.CENTER);

    VariablePopulator queryTypePopulator = new VariablePopulator(() -> createQueryTypePopulator(true, true));

//---------------------------------------------------------------------------

    // Query type column with change handler

    htFields.addColAltPopulatorWithUpdateHandler(hdtNone, ctEditableLimitedDropDown, queryTypePopulator, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      int rowNdx = tvFields.getItems().indexOf(row);

      row.setCellValue(ROW_NUMBER_COL_NDX, String.valueOf(rowNdx + 1), hdtNone);

      boolean tempDASD = disableAutoShowDropdownList;
      disableAutoShowDropdownList = true;

      int queryID = row.getID(QUERY_COL_NDX);
      QueryType queryType = QueryType.codeToVal(getCellID(cellVal));

      if (queryType != null)
      {
        if (queryType == qtReport)
        {
          queryTypePopulator.setPopulator(tvFields.getItems().get(rowNdx + 1), createQueryTypePopulator(false, false));
        }
        else
        {
          if (rowNdx > 0)
          {
            queryTypePopulator.setPopulator(tvFields.getItems().get(rowNdx - 1), createQueryTypePopulator(false, true));
            queryTypePopulator.setPopulator(tvFields.getItems().get(rowNdx), createQueryTypePopulator(false, true));
          }

          queryTypePopulator.setPopulator(tvFields.getItems().get(rowNdx + 1), createQueryTypePopulator(false, true));
        }
      }

      boolean clearQueryAndOperands = (queryType == qtReport) ||
                                      (queryType == null) ||
                                      ((queryID != QUERY_ANY_FIELD_CONTAINS) &&
                                       (queryID != QUERY_WITH_NAME_CONTAINING) &&
                                       (queryID != QUERY_LIST_ALL) &&
                                       (queryID != QUERY_MATCHING_RECORD) &&
                                       (queryID != QUERY_MATCHING_STRING));

      if (clearQueryAndOperands)
        row.setCellValue(nextColNdx, "", nextPopulator.getRecordType(row)); // Blank out the query

      ((QueryPopulator)nextPopulator).setQueryType(row, queryType);

      if (clearQueryAndOperands)
        clearOperands(row, 1); // Blank out the operands

      disableAutoShowDropdownList = tempDASD;

      if ((clearQueryAndOperands || (queryID == QUERY_ANY_FIELD_CONTAINS)) && (disableAutoShowDropdownList == false) && (queryType != null))
        htFields.edit(row, QUERY_COL_NDX);
    });

//---------------------------------------------------------------------------

    // Query select column with change handler

    htFields.addColAltPopulatorWithUpdateHandler(hdtNone, ctEditableLimitedDropDown, new QueryPopulator(), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      boolean tempDASD = disableAutoShowDropdownList;
      disableAutoShowDropdownList = true;

      Query<?> query = getQuery(row);

      if (queryChange(query, row))
        row.setCellValue(nextColNdx, "", nextPopulator.getRecordType(row));

      disableAutoShowDropdownList = tempDASD;

      if (disableAutoShowDropdownList || (query == null)) return;

      if (queryHasOperand(query, getQueryType(row), 1))
        htFields.edit(row, OPERAND_1_COL_NDX);
    });

//---------------------------------------------------------------------------

    // Operand 1 column with change handler

    htFields.addColAltPopulatorWithBothHandlers(hdtNone, ctEditableUnlimitedDropDown, new VariablePopulator(), onAction, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      Query<?> query = getQuery(row);

      if (op1Change(query, cellVal, row))
      {
        boolean tempDASD = disableAutoShowDropdownList;
        disableAutoShowDropdownList = true;

        row.setCellValue(nextColNdx, "", nextPopulator.getRecordType(row));
        Populator nextPop = ((VariablePopulator) nextPopulator).getPopulator(row);

        disableAutoShowDropdownList = tempDASD;

        int op1ID = getCellID(cellVal); // This is the number corresponding to the Tag entered if this is a Tag column, or the number corresponding
                                        // to the relation if this is a relation column

        if ((op1ID >= 0) && (nextPop != null) && (nextPop.getValueType() == cvtOperand))
        {
          HyperTableCell operandCell = row.getPopulator(OPERAND_1_COL_NDX).getValueType(row) == cvtBibField ?
            nextPop.getChoiceByID(Query.CONTAINS_OPERAND_ID)
          :
            nextPop.getChoiceByID(Query.EQUAL_TO_OPERAND_ID);

          row.setCellValue(nextColNdx, operandCell);
          if ((tempDASD == false) && queryHasOperand(query, getQueryType(row), 3, cellVal, operandCell))
            htFields.edit(row, OPERAND_3_COL_NDX);
        }
        else
        {
          if ((tempDASD == false) && queryHasOperand(query, getQueryType(row), 2, cellVal, GenericNonRecordHTC.blankCell))
            htFields.edit(row, OPERAND_2_COL_NDX);
        }
      }
    });

//---------------------------------------------------------------------------

    // Operand 2 column with change handler

    htFields.addColAltPopulatorWithBothHandlers(hdtNone, ctEditableUnlimitedDropDown, new VariablePopulator(), onAction, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      boolean tempDASD = disableAutoShowDropdownList;
      disableAutoShowDropdownList = true;

      Query<?> query = getQuery(row);

      if (op2Change(query, cellVal, row))
        row.setCellValue(nextColNdx, "", nextPopulator.getRecordType(row));

      disableAutoShowDropdownList = tempDASD;

      if (disableAutoShowDropdownList) return;

      if (queryHasOperand(query, getQueryType(row), 3, row.getCell(OPERAND_1_COL_NDX), cellVal))
        htFields.edit(row, OPERAND_3_COL_NDX);
    });

//---------------------------------------------------------------------------

    htFields.addColAltPopulatorWithActionHandler(hdtNone, ctEditableUnlimitedDropDown, new VariablePopulator(), onAction);

    htFields.getColumns().forEach(col -> col.setDontCreateNewRecord(true));

    htFields.addRefreshHandler(tabPane::requestLayout);

    htFields.addRemoveMenuItem(row -> row != htFields.getRows().get(htFields.getRows().size() - 1));

    resultsTable = new ResultsTable(tvResults, this);
    tvResults.setItems(FXCollections.observableList(resultsBackingList));

    reportTable = new ReportTable(this);

    tvResults.getSelectionModel().selectedIndexProperty().addListener((ob, oldValue, newValue) ->
    {
      refreshView(newValue.intValue(), false);
      tabPane.requestLayout();
    });

    btnResetFields.setOnAction(event -> resetFields());

    switchToRecordMode();

    scaleNodeForDPI(spMain);
    setFontSize(spMain);

    Platform.runLater(() ->
    {
      Supplier<String> fieldsDescSupplier  = () -> inRecordMode ? "query fields"  : "report fields",
                       resultsDescSupplier = () -> inRecordMode ? "query results" : "report results",
                       detailsDescSupplier = () -> inRecordMode ? "description of currently selected record" : "report result details";

      OneTouchExpandableWrapper.wrap(spMain, fieldsDescSupplier, resultsDescSupplier, 0.35, CollapsedState.Expanded);
      lowerOneTouchExpandableWrapper = OneTouchExpandableWrapper.wrap(spLower, resultsDescSupplier, detailsDescSupplier, 0.55, CollapsedState.ShowingOnlyFirstRegion);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Populator createQueryTypePopulator(boolean includeReport, boolean includeRecordTypes)
  {
    EnumSet<QueryType> queryTypes = includeRecordTypes ? EnumSet.allOf(QueryType.class) : EnumSet.of(qtReport);

    if (includeReport == false) queryTypes.remove(qtReport);

    return Populator.create(cvtQueryType, queryTypes.stream()
      .map(queryType -> new GenericNonRecordHTC(queryType.getCode(), queryType.getCaption(), queryType.getRecordType()))
      .toList());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refreshView(boolean activatingTab)
  {
    if (inRecordMode)
      refreshView(tvResults.getSelectionModel().getSelectedIndex(), activatingTab);
    else
    {
      ui.updateBottomPanel(false, false);

      webView.getEngine().loadContent(reportTable.getHtmlForCurrentRow());

      setPreview();
    }

    if (activatingTab) tvResults.refresh();
    tabPane.requestLayout();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshView(int selRowNdx, boolean activatingTab)
  {
    curResult = (resultsBackingList.isEmpty() || selRowNdx < 0) ? null : resultsBackingList.get(selRowNdx).getRecord();

    ui.updateBottomPanel(false, false);

    if (curResult == null)
    {
      webView.getEngine().loadContent("");
    }
    else
    {
      int scrollPos = queriesTabCtrlr.getUseTextViewInfo() ? queriesTabCtrlr.getView().getTextInfo().scrollPos : (activatingTab ? scrollPosPriorToBeingDeactivated : 0);

      MainTextWrapper.setReadOnlyHTML(HDT_Record.getDescHtml(curResult), webView.getEngine(), scrollPos, ui.currentFindInDescriptionText().isBlank() ? getRecordsToHilite() : null);
    }

    setPreview();

    queriesTabCtrlr.setFavNameToggle(fav != null);

    programmaticFavNameChange = true;
    tfFavName.setText(fav == null ? "" : fav.name);
    programmaticFavNameChange = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPreview()
  {
    if (inReportMode() || (curResult == null))
    {
      PreviewWindow.clearPreview(pvsQueriesTab);
      return;
    }

    switch (curResult.getType())
    {
      case hdtWork : case hdtMiscFile : case hdtWorkFile : case hdtPerson :

        PreviewWindow.setPreview(pvsQueriesTab, (HDT_RecordWithPath) curResult);
        break;

      default :

        PreviewWindow.clearPreview(pvsQueriesTab);
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void favNameChange()
  {
    if (programmaticFavNameChange) return;
    fav = null;
    queriesTabCtrlr.setFavNameToggle(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setRecord(HDT_Record record)
  {
    ResultRow targetRow = record == null ? null : recordToRow.get(record);
    if (targetRow == null) return;

    int ndx = resultsBackingList.indexOf(targetRow);
    if (ndx == -1) return;

    tvResults.getSelectionModel().clearAndSelect(ndx);
    HyperTable.scrollToSelection(tvResults, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Executes a search based on the provided parameters.
   *
   * @param doSearch If true, performs the search; if false, skips the search, invokes the favorite if one was passed in, and returns false.
   * @param type The type of query to be executed. Either this parameter should be null or newFav should be.
   * @param query The query ID to be executed. Pass in -1 if newFav is non-null.
   * @param newFav The favorite query to be invoked, if any. Should be left null if type and query parameters are used.
   * @param op1 The first operand for the query.
   * @param op2 The second operand for the query.
   * @param caption The caption to set for the query sub-tab within the Queries tab.
   * @return True if the query ran successfully, regardless of whether there were any results;
   * false if the query did not run, encountered an error during execution, or was cancelled by the user.
   */
  boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite newFav, HyperTableCell op1, HyperTableCell op2, String caption)
  {
    if ((type != qtReport) && (db.isLoaded() == false)) return false;

    if (newFav != null)
    {
      if ((type != null) || (query != -1))
        return falseWithInternalErrorPopup(82467);

      fav = newFav;
      invokeFavorite(newFav);
    }
    else if ((type == null) || (query < 0))
    {
      return falseWithInternalErrorPopup(82468);
    }

    fav = newFav;

    if (doSearch == false)
    {
      Platform.runLater(tabPane::requestLayout);
      return false;
    }

    if (type != null)
    {
      disableAutoShowDropdownList = true;

      htFields.clear();
      HyperTableRow row = htFields.newDataRow();

      htFields.selectID(QUERY_TYPE_COL_NDX, row, type.getCode());
      htFields.selectID(QUERY_COL_NDX     , row, query);

      if (op1 != null)
      {
        if (getCellID(op1) > 0)
          htFields.selectID(OPERAND_1_COL_NDX, row, getCellID(op1));
        else if (getCellText(op1).isEmpty())
          htFields.selectType(OPERAND_1_COL_NDX, row, getCellType(op1));
        else
          row.setCellValue(OPERAND_1_COL_NDX, op1);
      }

      if (op2 != null)
      {
        if (getCellID(op2) > 0)
          htFields.selectID(OPERAND_2_COL_NDX, row, getCellID(op2));
        else if (getCellText(op2).isEmpty())
          htFields.selectType(OPERAND_2_COL_NDX, row, getCellType(op2));
        else
          row.setCellValue(OPERAND_2_COL_NDX, op2);
      }

      disableAutoShowDropdownList = false;

      htFields.selectRow(0);
    }

    if (strNotNullOrBlank(caption))
      tab.setText(caption);

    return btnExecuteClick(caption.isEmpty());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void btnFavoriteClick()
  {
    if (db.isLoaded() == false) return;

    if (fav == null)
    {
      NewQueryFavDlgCtrlr ctrlr = new NewQueryFavDlgCtrlr(tfFavName.getText());

      if (ctrlr.showModal() == false) return;

      fav = tgLogic.getSelectedToggle() == btnCustom ?
        new QueryFavorite(ctrlr.getNewName(), ctrlr.getAutoExec(), tfCustomLogic.getText().strip(), false)
      :
        new QueryFavorite(ctrlr.getNewName(), ctrlr.getAutoExec(), "", tgLogic.getSelectedToggle() == btnOr);

      htFields.dataRows().forEach(row ->
      {
        QueryRow queryRow = new QueryRow();

        for (int colNdx = QUERY_TYPE_COL_NDX; colNdx <= OPERAND_3_COL_NDX; colNdx++)
          queryRow.cells[colNdx - QUERY_TYPE_COL_NDX] = HyperTableCell.clone(row.getCell(colNdx));

        fav.rows.add(queryRow);
      });

      ui.mnuQueries.getItems().add(fav);

      programmaticFavNameChange = true;
      tfFavName.setText(fav.name);
      programmaticFavNameChange = false;

      queriesTabCtrlr.setFavNameToggle(true);
    }

    else
    {
      fav.removeFromList(ui.mnuQueries.getItems());
      fav = null;

      programmaticFavNameChange = true;
      tfFavName.setText("");
      programmaticFavNameChange = false;

      queriesTabCtrlr.setFavNameToggle(false);
    }

    ui.updateFavorites();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void invokeFavorite(QueryFavorite fav)
  {
    this.fav = fav;

    if (db.isLoaded() == false) return;

    disableAutoShowDropdownList = true;

    htFields.clear();

    htFields.buildRows(fav.rows, (row, queryRow) ->
    {
      for (int colNdx = QUERY_TYPE_COL_NDX; colNdx <= OPERAND_3_COL_NDX; colNdx++)
        row.setCellValue(colNdx, queryRow.cells[colNdx - QUERY_TYPE_COL_NDX]);
    });

    programmaticCustomLogicChange = true;
    tfCustomLogic.setText(fav.customLogic);
    programmaticCustomLogicChange = false;

    tgLogic.selectToggle(fav.customLogic.isBlank() ? (fav.orLogic ? btnOr : btnAnd) : btnCustom);

    refreshView(false);
    htFields.selectRow(0);

    disableAutoShowDropdownList = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setCaption()
  {
    htFields.dataRows().forEach(row ->
    {
      for (int colNdx = QUERY_COL_NDX; colNdx <= OPERAND_3_COL_NDX; colNdx++)
      {
        String text = row.getText(colNdx);
        if (strNotNullOrBlank(text))
          tab.setText(text);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void switchToReportMode()
  {
    if (inRecordMode == false) return;

    removeFromParent(tvResults);
    reportTable.setParent(apResults);

    inRecordMode = false;
    curResult = null;

    webView.getEngine().loadContent("");
    ui.updateBottomPanel(false, true);
    queriesTabCtrlr.updateCB(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void switchToRecordMode()
  {
    if (inRecordMode) return;

    reportTable.removeFromParent();
    addToParent(tvResults, apResults);

    inRecordMode = true;

    webView.getEngine().loadContent("");
    ui.updateBottomPanel(false, false);
    queriesTabCtrlr.updateCB(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void executeReport(HyperTableRow row, boolean setCaption)
  {
    switchToReportMode();

    if (setCaption)
      setCaption();

    ReportEngine reportEngine = ReportEngine.createEngine(row.getID(QUERY_COL_NDX));

    if (reportEngine == null)
    {
      reportTable.clear();
      return;
    }

    reportTable.format(reportEngine);

    if (new HyperTask("GenerateReport", "Generating report...") { @Override protected void call() throws HyperDataException, CancelledTaskException
    {
      reportEngine.generate(this, row.getCell(OPERAND_1_COL_NDX), row.getCell(OPERAND_2_COL_NDX), row.getCell(OPERAND_3_COL_NDX));

    }}.runWithProgressDialog() != State.SUCCEEDED) return;

    reportTable.inject(reportEngine);

    if (reportEngine.autoShowDescription() && (reportEngine.getRows().size() > 0))
      lowerOneTouchExpandableWrapper.setCollapsedState(CollapsedState.Expanded);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // sources parameter is assumed empty and needs to be populated by this function so it can be used by the caller

  private static QuerySource getCombinedRecordSource(Map<HyperTableRow, QuerySource> sources)
  {
    // Build list of sources and list of unfiltered types

    boolean hasFiltered = false, hasUnfiltered = false;
    EnumSet<RecordType> unfilteredTypes = EnumSet.noneOf(RecordType.class);

    for (Entry<HyperTableRow, QuerySource> entry : sources.entrySet())
    {
      QuerySource source = entry.getValue();

      unfilteredTypes.add(source.recordType());

      switch (source.sourceType())
      {
        case QST_filteredRecords :

          hasFiltered   = true; break;

        case QST_recordsByType : case QST_allRecords :

          hasUnfiltered = true; break;

        default : break;
      }
    }

    // Generate combined record source

    if (hasUnfiltered)
      return new CombinedUnfilteredQuerySource(unfilteredTypes);

    if (hasFiltered)
      return new CombinedFilteredQuerySource(sources.values());

    return new CombinedUnfilteredQuerySource(EnumSet.noneOf(RecordType.class));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

 // If any of the queries are unfiltered, they will all be treated as unfiltered

  /**
   * Execute a query.
   * @param setCaption Whether to set the caption of this query sub-tab within the Queries tab
   * @return True if the query ran successfully, regardless of whether there were any results;
   * false if the query did not run, encountered an error during execution, or was cancelled by the user.
   */
  boolean btnExecuteClick(boolean setCaption)
  {
    for (HyperTableRow row : htFields.dataRows())
    {
      if (QueryType.codeToVal(row.getID(QUERY_TYPE_COL_NDX)) == qtReport)
      {
        executeReport(row, setCaption);
        return true;
      }
    }

    if (db.isLoaded() == false) return false;

    switchToRecordMode();

    boolean showDesc    = false,
            customLogic = (tgLogic.getSelectedToggle() == btnCustom),
            orLogic     = (tgLogic.getSelectedToggle() == btnOr    );

    if (customLogic)
    {
      if (tfCustomLogic.getText().isBlank())
        return falseWithErrorPopup("No custom evaluation logic expression has been entered.", tfCustomLogic);

      try
      {
        BoolExpression.create(tfCustomLogic.getText());
      }
      catch (ParseException e)
      {
        return falseWithErrorPopup("Error while parsing custom logic expression: " + getThrowableMessage(e), tfCustomLogic);
      }
    }

    Map<HyperTableRow, Query<?>> queries = new LinkedHashMap<>();
    Map<HyperTableRow, QuerySource> sources = new LinkedHashMap<>();

    for (HyperTableRow row : htFields.dataRows())
    {
      if (row.getID(QUERY_COL_NDX) < 0) continue;

      QueryType type = getQueryType(row);
      Query<?> query = getQuery(row);

      if (query.needsMentionsIndex() && (db.waitUntilRebuildIsDone() == false))
        return false;

      queries.put(row, query);
      sources.put(row, query.getSource(type, row));

      if (query.autoShowDescription())
        showDesc = true;
    }

    resultsTable.reset(this);
    recordToRow.clear();
    webView.getEngine().loadContent("");

    if (setCaption)
      setCaption();

    QuerySource combinedSource = getCombinedRecordSource(sources);

    searchLinkedRecords = combinedSource.recordType() != hdtNone;

    tvResults.setItems(FXCollections.emptyObservableList());

    // Evaluate record queries

    HyperTask task = new HyperTask("Query", "Running query...", combinedSource.size())
    {

//---------------------------------------------------------------------------

      @Override protected void call() throws CancelledTaskException, HyperDataException
      {
        resultsBackingList.clear();

        Map<HyperTableRow, Integer> rowNumbers = new HashMap<>(sources.size());
        Map<Integer, Boolean> results = new HashMap<>(sources.size());

        for (HyperTableRow row : sources.keySet())
        {
          rowNumbers.put(row, tvFields.getItems().indexOf(row) + 1);
          queries.get(row).init(row.getCell(OPERAND_1_COL_NDX), row.getCell(OPERAND_2_COL_NDX), row.getCell(OPERAND_3_COL_NDX));
        }

        Iterator<HDT_Record> recordIterator = combinedSource.iterator();

        try
        {
          BoolExpression expr = BoolExpression.create(tfCustomLogic.getText());

          while (recordIterator.hasNext())
          {
            incrementAndUpdateProgress(50);

            HDT_Record record = recordIterator.next();

            boolean firstRow = true, add = false;

            for (Entry<HyperTableRow, QuerySource> entry : sources.entrySet())
            {
              HyperTableRow row = entry.getKey();
              QuerySource source = entry.getValue();
              Query<?> query = queries.get(row);

              @SuppressWarnings("unchecked")
              boolean result = source.contains(record) && ((Query<HDT_Record>)query).evaluate(record, row, row.getCell(OPERAND_1_COL_NDX),
                                                                                                           row.getCell(OPERAND_2_COL_NDX),
                                                                                                           row.getCell(OPERAND_3_COL_NDX));
              if (customLogic)    results.put(rowNumbers.get(row), result);
              else if (firstRow)  add = result;
              else if (orLogic)   add = add || result;
              else                add = add && result;

              firstRow = false;
            }

            if (customLogic)
            {
              if (BoolEvaluator.evaluate(expr, results))
                addRecord(record, false);
            }
            else if (add)
              addRecord(record, false);
          }
        }
        catch (ParseException e)
        {
          throw new HyperDataException("Error while evaluating custom logic expression: " + getThrowableMessage(e), e);
        }
      }

//---------------------------------------------------------------------------

    };

    task.addDoneHandler(state -> queries.values().forEach(query -> query.cleanup(state)));

    boolean succeeded = task.runWithProgressDialog() == State.SUCCEEDED;

    if (succeeded == false)
      resultsBackingList.clear();

    Platform.runLater(() -> tvResults.setItems(FXCollections.observableList(resultsBackingList)));

    if (succeeded == false) return false;

    resultsTable.addExtraColumnsToTable(queries);

    if (showDesc)
      lowerOneTouchExpandableWrapper.setCollapsedState(CollapsedState.Expanded);

    boolean focusOnFirstResult = false;

    if (resultsBackingList.isEmpty() == false)
    {
      if (collEmpty(getRecordsToHilite()) == false)
      {
        ui.switchToRecordSearch();

        if (showDesc)
          focusOnFirstResult = true;

        refreshView(false);
      }
      else
      {
        refreshView(false);

        String textToHilite = getTextToHilite();
        if (textToHilite.isBlank() == false)
        {
          ui.findInDescription(textToHilite);

          if (showDesc)
            focusOnFirstResult = true;
        }
      }
    }
    else
      refreshView(false);

    if (focusOnFirstResult) Platform.runLater(() ->
    {
      safeFocus(tvResults);
      tvResults.getSelectionModel().selectFirst();
    });

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addRecord(HDT_Record record, boolean addToObsList)
  {
    resultsTable.addColumnsForRecord(record, addToObsList);

    ResultRow row = new ResultRow(record);
    recordToRow.put(record, row);

    if (addToObsList)
    {
      tvResults.getItems().add(row);
      refreshView(false);
    }
    else
      resultsBackingList.add(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean queryHasOperand(Query<?> query, QueryType queryType, int opNum)
  {
    return queryHasOperand(query, queryType, opNum, GenericNonRecordHTC.blankCell, GenericNonRecordHTC.blankCell);
  }

  private static boolean queryHasOperand(Query<?> query, QueryType queryType, int opNum, HyperTableCell op1, HyperTableCell op2)
  {
    return (queryType != qtReport) && query.hasOperand(opNum, op1, op2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determines the record(s) to highlight based on the query rows in the table.
   * <p>
   * This method iterates through all data rows in the table and examines their query type.
   * It prioritizes finding records associated with the <code>qtAllRecords</code> query type, and if no such
   * records are found, it returns the first matching set of records for a non-report query type.
   *
   * @return The record(s) to highlight. Returns the first matching set of records for an all-records query
   *         if available. Otherwise, returns the first matching set of records.
   */
  Set<HDT_Record> getRecordsToHilite()
  {
    Set<HDT_Record> altSet = null;

    for (HyperTableRow row : htFields.dataRows())
    {
      int queryTypeID = row.getID(QUERY_TYPE_COL_NDX);
      Set<HDT_Record> set = getRecordsToHiliteForQueryRow(row);

      if ((queryTypeID == qtAllRecords.getCode()) && (collEmpty(set) == false))
        return set;

      if ((altSet == null) && (queryTypeID != qtReport.getCode()))
        altSet = set;
    }

    return altSet;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Set<HDT_Record> getRecordsToHiliteForQueryRow(HyperTableRow row)
  {
    switch (row.getID(QUERY_COL_NDX))
    {
      case QUERY_LINKING_TO_RECORD : case QUERY_MATCHING_RECORD :
      {
        HDT_Record record = HyperTableCell.getRecord(row.getCell(OPERAND_2_COL_NDX));
        if (record instanceof HDT_Concept concept) return Set.of(concept.term.get());
        return Set.of(record);
      }
      case QUERY_MATCHING_WORK_OR_SUBWORKS :
      {
        HDT_Work rootWork = HyperTableCell.getRecord(row.getCell(OPERAND_1_COL_NDX));
        Set<HDT_Record> set = new HashSet<>();
        Deque<HDT_Work> queue = new ArrayDeque<>();
        queue.add(rootWork);

        while (queue.isEmpty() == false)
        {
          HDT_Work curWork = queue.poll();

          if (set.add(curWork))
            queue.addAll(curWork.subWorks);
        }

        return set;
      }
      default :
        break;
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getTextToHilite()
  {
    for (HyperTableRow row : htFields.dataRows())
    {
      if (row.getID(QUERY_COL_NDX) > -1)
      {
        for (int colNdx : OPERAND_COL_INDICES)
        {
          if (((VariablePopulator) htFields.getPopulator(colNdx)).getRestricted(row) == false)
          {
            String cellText = getCellText(row.getCell(colNdx));
            if (strNotNullOrBlank(cellText))
              return cellText;
          }
        }
      }
    }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void clearOperands(HyperTableRow row, int startOpNum)
  {
    if (startOpNum > 3)
      return;

    if (startOpNum < 1)
    {
      internalErrorPopup(90087);
      return;
    }

    if (startOpNum == 1) clearOperand(row, 1);
    if (startOpNum <= 2) clearOperand(row, 2);
    clearOperand(row, 3);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean clearingOperand = false;

  private static void clearOperand(HyperTableRow row, int opNum)
  {
    boolean wasClearingOperand = clearingOperand;
    clearingOperand = true;

    VariablePopulator vp = row.getPopulator(opNum + QUERY_COL_NDX);
    vp.setPopulator(row, null);
    vp.setRestricted(row, true);
    row.setCellValue(opNum + QUERY_COL_NDX, "", hdtNone);

    clearingOperand = wasClearingOperand;
  }
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  //returns true if subsequent cells need to be updated

  private boolean queryChange(Query<?> query, HyperTableRow row)
  {
    if (db.isLoaded() == false) return false;

    if (getQueryType(row) == qtReport) return true;

    clearOperands(row, 1);

    return (query == null) || query.initRow(row, htFields.getPopulator(OPERAND_1_COL_NDX), htFields.getPopulator(OPERAND_2_COL_NDX), htFields.getPopulator(OPERAND_3_COL_NDX));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  //returns true if subsequent cells need to be updated

  private boolean op1Change(Query<?> query, HyperTableCell op1, HyperTableRow row)
  {
    if (clearingOperand || (db.isLoaded() == false)) return false;

    if ((query == null) || (getQueryType(row) == qtReport)) return true;

    return query.op1Change(op1, row, htFields.getPopulator(OPERAND_1_COL_NDX), htFields.getPopulator(OPERAND_2_COL_NDX), htFields.getPopulator(OPERAND_3_COL_NDX));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // returns true if subsequent cells need to be updated

  private boolean op2Change(Query<?> query, HyperTableCell op2, HyperTableRow row)
  {
    if (clearingOperand || (db.isLoaded() == false)) return false;

    if ((query == null) || (getQueryType(row) == qtReport)) return true;

    HyperTableCell op1 = row.getCell(OPERAND_1_COL_NDX);

    return query.op2Change(op1, op2, row, htFields.getPopulator(OPERAND_1_COL_NDX), htFields.getPopulator(OPERAND_2_COL_NDX), htFields.getPopulator(OPERAND_3_COL_NDX));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void resetFields()
  {
    disableAutoShowDropdownList = true;

    htFields.clear();

    HyperTableRow row = htFields.newDataRow();
    htFields.selectID(QUERY_TYPE_COL_NDX, row, qtAllRecords.getCode());
    htFields.selectID(QUERY_COL_NDX, row, QUERY_ANY_FIELD_CONTAINS);
    htFields.selectRow(row);

    disableAutoShowDropdownList = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void deactivate()
  {
    scrollPosPriorToBeingDeactivated = MainTextUtil.webEngineScrollPos(webView.getEngine());

    removeFromParent(webView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void activate()
  {
    apDescription.getChildren().setAll(webView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
