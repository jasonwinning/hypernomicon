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

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.query.GeneralQueries.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.query.ui.ResultsTable.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.controlsfx.control.MasterDetailPane;
import org.hypernomicon.App;
import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.query.Query;
import org.hypernomicon.query.QueryType;
import org.hypernomicon.query.reports.ReportEngine;
import org.hypernomicon.query.reports.ReportTable;
import org.hypernomicon.query.sources.CombinedFilteredQuerySource;
import org.hypernomicon.query.sources.CombinedUnfilteredQuerySource;
import org.hypernomicon.query.sources.QuerySource;
import org.hypernomicon.view.HyperFavorites.FavMenuItem;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.HyperFavorites.QueryRow;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.populators.QueryPopulator.QueryCell;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.util.boolEvaluator.BoolEvaluator;
import org.hypernomicon.util.boolEvaluator.BoolExpression;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Multimap;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.concurrent.Worker.State;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Pos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;

public final class QueryCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private MasterDetailPane spMain, spLower;
  @FXML private TableView<HyperTableRow> tvFields;
  @FXML private TableView<ResultsRow> tvResults;
  @FXML private AnchorPane apDescription, apResults;
  @FXML private ToggleGroup tgLogic;
  @FXML private TextField tfCustomLogic;
  @FXML private ToggleButton btnCustom, btnAnd, btnOr;

  private final QueriesTabCtrlr queriesTabCtrlr;
  private final WebView webView;
  private final TabPane tabPane;
  private final TextField tfFavName;
  private HyperTable htFields;
  private ReportTable reportTable;
  private Tab tab;
  private QueryFavorite fav = null;
  private HDT_Record curResult = null;

  ResultsTable resultsTable;

  private final List<ResultsRow> resultsBackingList = new ArrayList<>();
  private final Multimap<RecordType, ColumnGroup> recordTypeToColumnGroup = LinkedHashMultimap.create();
  private final Map<HDT_Record, ResultsRow> recordToRow = new HashMap<>();

  private boolean programmaticFavNameChange = false,
                  disableAutoShowDropdownList = false,
                  inRecordMode = true,
                  searchLinkedRecords;

  public static final int ROW_NUMBER_COL_NDX = 0,
                          QUERY_TYPE_COL_NDX = 1,
                          QUERY_COL_NDX      = 2,
                          OPERAND_1_COL_NDX  = 3,
                          OPERAND_2_COL_NDX  = 4,
                          OPERAND_3_COL_NDX  = 5;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<ResultsRow> results()       { return inReportMode() ? List.of() : Collections.unmodifiableList(resultsBackingList); }
  void saveColumnWidths()                 { HyperTable.saveColWidthsForTable(tvFields.getColumns(), PREF_KEY_HT_QUERY_FIELDS, false); }
  void focusOnFields()                    { safeFocus(tvFields); }
  public boolean inReportMode()           { return inRecordMode == false; }
  Tab getTab()                            { return tab; }
  HDT_Record getRecord()                  { return curResult; }
  public boolean getSearchLinkedRecords() { return searchLinkedRecords; }

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
      messageDialog("Internal error #90203", mtError);
      return;
    }

    tabPane.getTabs().add(tabPane.getTabs().size() - 1, tab);
    tab.setOnCloseRequest(event -> queriesTabCtrlr.deleteView((Tab) event.getSource()));

    tgLogic.selectedToggleProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null)
        oldValue.setSelected(true);
    });

    htFields = new HyperTable(tvFields, QUERY_COL_NDX, true, "");

    HyperTable.loadColWidthsForTable(tvFields.getColumns(), PREF_KEY_HT_QUERY_FIELDS);

    htFields.autoCommitListSelections = true;

    htFields.addLabelCol(hdtNone, Pos.CENTER);

    Populator queryTypePopulator = Populator.create(cvtQueryType, EnumSet.allOf(QueryType.class).stream()
      .map(queryType -> new HyperTableCell(queryType.getCode(), queryType.getCaption(), queryType.getRecordType()))
      .collect(Collectors.toList()));

//---------------------------------------------------------------------------

    // Query type column with change handler

    htFields.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, queryTypePopulator, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      row.setCellValue(ROW_NUMBER_COL_NDX, String.valueOf(tvFields.getItems().indexOf(row) + 1), hdtNone);

      boolean tempDASD = disableAutoShowDropdownList;
      disableAutoShowDropdownList = true;

      int queryID = row.getID(QUERY_COL_NDX);
      QueryType queryType = QueryType.codeToVal(getCellID(cellVal));

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

    htFields.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, new QueryPopulator(), (row, cellVal, nextColNdx, nextPopulator) ->
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

    htFields.addColAltPopulatorWithBothHandlers(hdtNone, ctDropDown, new VariablePopulator(), onAction, (row, cellVal, nextColNdx, nextPopulator) ->
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
            nextPop.getChoiceByID(null, Query.CONTAINS_OPERAND_ID)
          :
            nextPop.getChoiceByID(null, Query.EQUAL_TO_OPERAND_ID);

          row.setCellValue(nextColNdx, operandCell);
          if ((tempDASD == false) && queryHasOperand(query, getQueryType(row), 3, cellVal, operandCell))
            htFields.edit(row, OPERAND_3_COL_NDX);
        }
        else
        {
          if ((tempDASD == false) && queryHasOperand(query, getQueryType(row), 2, cellVal, blankCell))
            htFields.edit(row, OPERAND_2_COL_NDX);
        }
      }
    });

//---------------------------------------------------------------------------

    // Operand 2 column with change handler

    htFields.addColAltPopulatorWithBothHandlers(hdtNone, ctDropDown, new VariablePopulator(), onAction, (row, cellVal, nextColNdx, nextPopulator) ->
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

    htFields.addColAltPopulatorWithActionHandler(hdtNone, ctDropDown, new VariablePopulator(), onAction);

    htFields.getColumns().forEach(col -> col.setDontCreateNewRecord(true));

    htFields.addRefreshHandler(tabPane::requestLayout);

    resultsTable = new ResultsTable(tvResults);
    resultsTable.getTV().setItems(FXCollections.observableList(resultsBackingList));

    reportTable = new ReportTable(this);

    tvResults.getSelectionModel().selectedIndexProperty().addListener((ob, oldValue, newValue) ->
    {
      refreshView(newValue.intValue());
      tabPane.requestLayout();
    });

    recordTypeToColumnGroup.clear();
    resultsBackingList.clear();

    switchToRecordMode();

    scaleNodeForDPI(spMain);
    setFontSize(spMain);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Query<?> getQuery(HyperTableRow row)
  {
    HyperTableCell cell = row.getCell(QUERY_COL_NDX);
    return cell instanceof QueryCell ? ((QueryCell)cell).getQuery() : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refreshView(boolean refreshTable)
  {
    if (inRecordMode)
      refreshView(tvResults.getSelectionModel().getSelectedIndex());
    else
    {
      ui.updateBottomPanel(false);

      webView.getEngine().loadContent(reportTable.getHtmlForCurrentRow());

      setPreview();
    }

    if (refreshTable) tvResults.refresh();
    tabPane.requestLayout();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshView(int selRowNdx)
  {
    curResult = (resultsBackingList.isEmpty() || selRowNdx < 0) ? null : resultsBackingList.get(selRowNdx).getRecord();

    ui.updateBottomPanel(false);

    if (curResult == null)
    {
      webView.getEngine().loadContent("");
    }
    else
    {
      queriesTabCtrlr.setTextToHilite(getTextToHilite());

      String mainText = curResult.hasDesc() ? ((HDT_RecordWithDescription) curResult).getDesc().getHtml() : "";

      MainTextWrapper.setReadOnlyHTML(mainText, webView.getEngine(), new TextViewInfo(), getRecordToHilite());
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
      previewWindow.clearPreview(pvsQueriesTab);
      return;
    }

    switch (curResult.getType())
    {
      case hdtWork :

        HDT_Work work = (HDT_Work) curResult;
        previewWindow.setPreview(pvsQueriesTab, work.filePathIncludeExt(), work.getStartPageNum(), work.getEndPageNum(), work);
        break;

      case hdtMiscFile : case hdtWorkFile : case hdtPerson :

        previewWindow.setPreview(pvsQueriesTab, ((HDT_RecordWithPath) curResult).filePath(), curResult);
        break;

      default :

        previewWindow.clearPreview(pvsQueriesTab);
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
    ResultsRow targetRow = record == null ? null : recordToRow.get(record);
    if (targetRow == null) return;

    for (int ndx = 0, max = resultsBackingList.size(); ndx < max; ndx++)
    {
      if (resultsBackingList.get(ndx) == targetRow)
      {
        tvResults.getSelectionModel().clearAndSelect(ndx);
        HyperTable.scrollToSelection(tvResults, false);
        return;
      }
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  // Returns true if search was completed

  boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite newFav, HyperTableCell op1, HyperTableCell op2, String caption)
  {
    if ((type != qtReport) && (db.isLoaded() == false)) return false;

    fav = newFav;

    if (newFav != null) invokeFavorite(newFav);

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

      if (query > -1)
      {
        htFields.selectID(QUERY_COL_NDX, row, query);

        if (op1 != null)
        {
          if (getCellID(op1) > 0)
            htFields.selectID(OPERAND_1_COL_NDX, row, getCellID(op1));
          else if (getCellText(op1).isEmpty())
            htFields.selectType(OPERAND_1_COL_NDX, row, getCellType(op1));
          else
            row.setCellValue(OPERAND_1_COL_NDX, op1.clone());
        }

        if (op2 != null)
        {
          if (getCellID(op2) > 0)
            htFields.selectID(OPERAND_2_COL_NDX, row, getCellID(op2));
          else if (getCellText(op2).isEmpty())
            htFields.selectType(OPERAND_2_COL_NDX, row, getCellType(op2));
          else
            row.setCellValue(OPERAND_2_COL_NDX, op2.clone());
        }
      }

      disableAutoShowDropdownList = false;

      htFields.selectRow(0);
    }

    if (caption.length() > 0)
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
      if (Boolean.TRUE.booleanValue()) return; // disable saving favorites for now

      NewQueryFavDlgCtrlr ctrlr = NewQueryFavDlgCtrlr.build(tfFavName.getText());

      if (ctrlr.showModal() == false) return;

      fav = new QueryFavorite();

      fav.name = ctrlr.getNewName();
      fav.autoexec = ctrlr.getAutoExec();

      htFields.dataRows().forEach(row ->
      {
        QueryRow queryRow = new QueryRow();

        for (int colNdx = QUERY_TYPE_COL_NDX; colNdx <= OPERAND_3_COL_NDX; colNdx++)
          queryRow.cells[colNdx] = row.getCell(colNdx).clone();

        fav.rows.add(queryRow);
      });

      ui.mnuQueries.getItems().add(new FavMenuItem(fav));

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
        row.setCellValue(colNdx, queryRow.cells[colNdx - QUERY_TYPE_COL_NDX].clone());
    });

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
        if (text.length() > 0)
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
    ui.updateBottomPanel(false);
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
    ui.updateBottomPanel(false);
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

    HyperTask task = new HyperTask("GenerateReport") { @Override protected void call() throws HyperDataException, CancelledTaskException
    {
      updateMessage("Generating report...");
      updateProgress(0, 1);

      reportEngine.generate(this, row.getCell(OPERAND_1_COL_NDX), row.getCell(OPERAND_2_COL_NDX), row.getCell(OPERAND_3_COL_NDX));
    }};

    if (task.runWithProgressDialog() != State.SUCCEEDED) return;

    reportTable.inject(reportEngine);

    if (reportEngine.autoShowDescription() && (reportEngine.getRows().size() > 0))
      queriesTabCtrlr.chkShowDesc.setSelected(true);
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

  boolean btnExecuteClick(boolean setCaption)
  {
    for (HyperTableRow row : htFields.dataRows())
    {
      if (QueryType.codeToVal(row.getID(QUERY_TYPE_COL_NDX)) == qtReport)
      {
        htFields.setDataRows(List.of(row));

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
      try
      {
        BoolExpression.create(tfCustomLogic.getText());
      }
      catch (ParseException e)
      {
        messageDialog("Error while parsing custom logic expression: " + e.getMessage(), mtError);
        return false;
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

    resultsTable.reset();
    recordToRow.clear();
    webView.getEngine().loadContent("");

    if (setCaption)
      setCaption();

    QuerySource combinedSource = getCombinedRecordSource(sources);

    searchLinkedRecords = combinedSource.recordType() != hdtNone;
    int total = combinedSource.size();

    resultsTable.getTV().setItems(FXCollections.emptyObservableList());

    // Evaluate record queries

    HyperTask task = new HyperTask("Query")
    {
      //---------------------------------------------------------------------------

      @Override protected void call() throws CancelledTaskException, HyperDataException
      {
        recordTypeToColumnGroup.clear();
        resultsBackingList.clear();

        updateMessage("Running query...");
        updateProgress(0, 1);

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

          for (int recordNdx = 0; recordIterator.hasNext(); recordNdx++)
          {
            if (isCancelled())
              throw new CancelledTaskException();

            if ((recordNdx % 50) == 0)
              updateProgress(recordNdx, total);

            HDT_Record record = recordIterator.next();

            boolean firstRow = true, add = false;

            for (Entry<HyperTableRow, QuerySource> entry : sources.entrySet())
            {
              HyperTableRow row = entry.getKey();
              QuerySource source = entry.getValue();
              Query<?> query = queries.get(row);

              boolean result = source.contains(record) ?
                evaluate(query, record, row, row.getCell(OPERAND_1_COL_NDX), row.getCell(OPERAND_2_COL_NDX), row.getCell(OPERAND_3_COL_NDX))
              :
                false;

              if (customLogic)              results.put(rowNumbers.get(row), result);
              else if (firstRow)            add = result;
              else if (orLogic)             add = add || result;
              else                          add = add && result;

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
          throw new HyperDataException("Error while evaluating custom logic expression: " + e.getMessage(), e);
        }
      }

      //---------------------------------------------------------------------------

      @SuppressWarnings("unchecked")
      private <HDT_T extends HDT_Record> boolean evaluate(Query<?> query, HDT_T record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException
      {
        return ((Query<HDT_T>)query).evaluate(record, row, op1, op2, op3);
      }

      //---------------------------------------------------------------------------
    };

    task.runWhenFinalStateSet(state -> queries.values().forEach(query -> query.cleanup(state)));

    boolean succeeded = task.runWithProgressDialog() == State.SUCCEEDED;

    if (succeeded == false)
      resultsBackingList.clear();

    Platform.runLater(() -> resultsTable.getTV().setItems(FXCollections.observableList(resultsBackingList)));

    if (succeeded == false) return false;

    recordTypeToColumnGroup.forEach((recordType, colGroup) -> colGroup.addColumnsToTable(resultsTable));

    if (showDesc)
      queriesTabCtrlr.chkShowDesc.setSelected(true);

    curQC.refreshView(false);
    return true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void addRecord(HDT_Record record, boolean addToObsList)
  {
    RecordType recordType = record.getType();

    if (recordTypeToColumnGroup.containsKey(recordType) == false)
    {
      if (recordType.getDisregardDates() == false)
        resultsTable.addDateColumns();

      Set<Tag> tags = record.getAllTags();
      removeAll(tags, tagHub, tagPictureCrop, tagMainText);

      ColumnGroup colGroup = new ColumnGroup(recordType, tags);
      recordTypeToColumnGroup.put(recordType, colGroup);

      if (addToObsList)
        colGroup.addColumnsToTable(resultsTable);

      colGroups.add(colGroup);

      if ((recordType == hdtWork) && db.bibLibraryIsLinked())
      {
        colGroup = ColumnGroup.newBibFieldsColumnGroup();
        recordTypeToColumnGroup.put(recordType, colGroup);

        if (addToObsList)
          colGroup.addColumnsToTable(resultsTable);

        colGroups.add(colGroup);
      }
    }

    ResultsRow row = new ResultsRow(record);
    recordToRow.put(record, row);

    if (addToObsList)
    {
      resultsTable.getTV().getItems().add(row);
      refreshView(false);
    }
    else
      resultsBackingList.add(row);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private static boolean queryHasOperand(Query<?> query, QueryType queryType, int opNum)
  {
    return queryHasOperand(query, queryType, opNum, blankCell, blankCell);
  }

  private static boolean queryHasOperand(Query<?> query, QueryType queryType, int opNum, HyperTableCell op1, HyperTableCell op2)
  {
    return (queryType != qtReport) && query.hasOperand(opNum, op1, op2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_Record getRecordToHilite()
  {
    for (HyperTableRow row : htFields.dataRows())
    {
      if (row.getID(QUERY_TYPE_COL_NDX) == qtAllRecords.getCode())
      {
        switch (row.getID(QUERY_COL_NDX))
        {
          case QUERY_LINKING_TO_RECORD : case QUERY_MATCHING_RECORD :

            HDT_Record record = HyperTableCell.getRecord(row.getCell(OPERAND_2_COL_NDX));
            if (record != null) return record;
            break;

          default :
            break;
        }
      }
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getTextToHilite()
  {
    for (HyperTableRow row : htFields.dataRows())
    {
      if (row.getID(QUERY_COL_NDX) > -1)
      {
        for (int colNdx = OPERAND_1_COL_NDX; colNdx <= OPERAND_3_COL_NDX; colNdx++)
        {
          if (((VariablePopulator) htFields.getPopulator(colNdx)).getRestricted(row) == false)
          {
            String cellText = getCellText(row.getCell(colNdx));
            if (cellText.length() > 0)
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
      messageDialog("Internal error 90087", mtError);
      return;
    }

    if (startOpNum <= 1) clearOperand(row, 1);
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

  public void deactivate()
  {
    spMain .showDetailNodeProperty().unbind();
    spLower.showDetailNodeProperty().unbind();

    removeFromParent(webView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void activate(CheckBox chkShowFields, CheckBox chkShowDesc)
  {
    chkShowFields.setSelected(spMain .isShowDetailNode());
    chkShowDesc  .setSelected(spLower.isShowDetailNode());

    spMain .showDetailNodeProperty().bind(chkShowFields.selectedProperty());
    spLower.showDetailNodeProperty().bind(chkShowDesc  .selectedProperty());

    apDescription.getChildren().setAll(webView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
