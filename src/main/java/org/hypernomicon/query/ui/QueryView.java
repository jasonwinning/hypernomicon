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
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
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
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.query.GeneralQueries;
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
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;

public final class QueryView
{

//---------------------------------------------------------------------------

  private final QueryTabCtrlr queryTabCtrlr;
  private final WebView webView;
  private final TabPane tabPane;
  private final TextField tfFavName;
  private MasterDetailPane spMain, spLower;
  private AnchorPane apDescription, apResults;
  private HyperTable htFields;
  private ReportTable reportTable;
  private Tab tab;
  private QueryFavorite fav = null;
  private HDT_Record curResult = null;
  private TableView<ResultsRow> tvResults;
  private TableView<HyperTableRow> tvFields;

  ResultsTable resultsTable;

  private final List<ResultsRow> resultsBackingList = new ArrayList<>();
  private final Map<RecordType, ColumnGroup> recordTypeToColumnGroup = new LinkedHashMap<>();
  private final Map<HDT_Record, ResultsRow> recordToRow = new HashMap<>();

  private boolean programmaticFavNameChange = false,
                  disableAutoShowDropdownList = false,
                  inRecordMode = true,
                  searchLinkedRecords;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<ResultsRow> results()       { return inReportMode() ? List.of() : Collections.unmodifiableList(resultsBackingList); }
  void saveColumnWidths()                 { HyperTable.saveColWidthsForTable(tvFields.getColumns(), PREF_KEY_HT_QUERY_FIELDS, false); }
  void focusOnFields()                    { safeFocus(tvFields); }
  boolean inReportMode()                  { return inRecordMode == false; }
  Tab getTab()                            { return tab; }
  HDT_Record getRecord()                  { return curResult; }
  public boolean getSearchLinkedRecords() { return searchLinkedRecords; }

  private static QueryType getQueryType(HyperTableRow row) { return QueryType.codeToVal(row.getID(0)); }
  private static Query<?> getQuery(HyperTableRow row)      { return QueryTabCtrlr.queryTable.get(QueryType.codeToVal(row.getID(0)), row.getID(1)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  QueryView(QueryTabCtrlr queryTabCtrlr, WebView webView, TabPane tabPane, TextField tfFavName)
  {
    this.queryTabCtrlr = queryTabCtrlr;
    this.webView = webView;
    this.tabPane = tabPane;
    this.tfFavName = tfFavName;

    EventHandler<ActionEvent> onAction = event ->
    {
      queryTabCtrlr.btnExecute.requestFocus();
      btnExecuteClick(true);
    };

    FXMLLoader loader = new FXMLLoader(App.class.getResource("query/QueryView.fxml"));

    try { tab = new Tab("New query", loader.load()); }
    catch (IOException e)
    {
      messageDialog("Internal error #90203", mtError);
      return;
    }

    tabPane.getTabs().add(tabPane.getTabs().size() - 1, tab);
    tab.setOnCloseRequest(event -> queryTabCtrlr.deleteView((Tab) event.getSource()));

    QueryViewCtrlr ctrlr = loader.getController();

    spMain        = ctrlr.spMain;
    spLower       = ctrlr.spLower;
    tvFields      = ctrlr.tvFields;
    tvResults     = ctrlr.tvResults;
    apDescription = ctrlr.apDescription;
    apResults     = ctrlr.apResults;

    htFields = new HyperTable(tvFields, 1, true, "");

    HyperTable.loadColWidthsForTable(tvFields.getColumns(), PREF_KEY_HT_QUERY_FIELDS);

    htFields.autoCommitListSelections = true;

    Populator queryTypePopulator = Populator.create(cvtQueryType, EnumSet.allOf(QueryType.class).stream()
      .map(queryType -> new HyperTableCell(queryType.getCode(), queryType.getCaption(), queryType.getRecordType()))
      .collect(Collectors.toList()));

//---------------------------------------------------------------------------

    htFields.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, queryTypePopulator, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      int queryID = row.getID(1);
      QueryType qt = QueryType.codeToVal(getCellID(cellVal));

      boolean tempDASD = disableAutoShowDropdownList;
      disableAutoShowDropdownList = true;

      if ((qt == qtReport) ||
          ((queryID != QUERY_ANY_FIELD_CONTAINS) &&
           (queryID != QUERY_WITH_NAME_CONTAINING) &&
           (queryID != QUERY_LIST_ALL)))
        row.setCellValue(nextColNdx, new HyperTableCell("", nextPopulator.getRecordType(row)));

      ((QueryPopulator)nextPopulator).setQueryType(row, qt);

      switch (queryID)
      {
        case QUERY_WITH_NAME_CONTAINING : case QUERY_ANY_FIELD_CONTAINS :
        case QUERY_LIST_ALL             : case QUERY_WHERE_FIELD        :
          break;

        default :
          clearOperands(row, 1);
      }

      disableAutoShowDropdownList = tempDASD;

      if (disableAutoShowDropdownList == false)
        htFields.edit(row, 1);
    });

//---------------------------------------------------------------------------

    htFields.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, new QueryPopulator(), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      boolean tempDASD = disableAutoShowDropdownList;
      disableAutoShowDropdownList = true;

      Query<?> query = getQuery(row);

      if (queryChange(query, row))
        row.setCellValue(nextColNdx, new HyperTableCell("", nextPopulator.getRecordType(row)));

      disableAutoShowDropdownList = tempDASD;

      if (disableAutoShowDropdownList) return;

      if (queryHasOperand(query, getQueryType(row), 1))
        htFields.edit(row, 2);
    });

//---------------------------------------------------------------------------

    htFields.addColAltPopulatorWithBothHandlers(hdtNone, ctDropDown, new VariablePopulator(), onAction, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      Query<?> query = getQuery(row);

      if (op1Change(query, cellVal, row))
      {
        boolean tempDASD = disableAutoShowDropdownList;
        disableAutoShowDropdownList = true;

        row.setCellValue(nextColNdx, new HyperTableCell("", nextPopulator.getRecordType(row)));
        Populator pop = ((VariablePopulator) nextPopulator).getPopulator(row);

        disableAutoShowDropdownList = tempDASD;

        if ((getCellID(cellVal) >= 0) && (pop.getValueType() == cvtOperand))
        {
          row.setCellValue(nextColNdx, pop.getChoiceByID(null, QueryTabCtrlr.EQUAL_TO_OPERAND_ID));
          if ((tempDASD == false) && queryHasOperand(query, getQueryType(row), 3, cellVal))
            htFields.edit(row, 4);
        }
        else
        {
          if ((tempDASD == false) && queryHasOperand(query, getQueryType(row), 2, cellVal))
            htFields.edit(row, 3);
        }
      }
    });

//---------------------------------------------------------------------------

    htFields.addColAltPopulatorWithBothHandlers(hdtNone, ctDropDown, new VariablePopulator(), onAction, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      boolean tempDASD = disableAutoShowDropdownList;
      disableAutoShowDropdownList = true;

      Query<?> query = getQuery(row);

      if (op2Change(query, cellVal, row))
        row.setCellValue(nextColNdx, new HyperTableCell("", nextPopulator.getRecordType(row)));

      disableAutoShowDropdownList = tempDASD;

      if (disableAutoShowDropdownList) return;

      if (queryHasOperand(query, getQueryType(row), 3, cellVal))
        htFields.edit(row, 4);
    });

//---------------------------------------------------------------------------

    htFields.addColAltPopulatorWithActionHandler(hdtNone, ctDropDown, new VariablePopulator(), onAction);
    htFields.addColAltPopulator(hdtNone, ctDropDownList, Populator.create(cvtConnective, QueryTabCtrlr.andCell, QueryTabCtrlr.orCell));

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

  public void refreshView(boolean refreshTable)
  {
    if (inRecordMode)
      refreshView(tvResults.getSelectionModel().getSelectedIndex());
    else
    {
      webView.getEngine().loadContent(reportTable.getHtmlForCurrentRow());

      ui.updateBottomPanel(false);
    }

    if (refreshTable) tvResults.refresh();
    tabPane.requestLayout();
  }

  private void refreshView(int selRowNdx)
  {
    if (resultsBackingList.size() > 0)
    {
      ui.updateBottomPanel(false);
      curResult = selRowNdx > -1 ? resultsBackingList.get(selRowNdx).getRecord() : null;

      if (curResult != null)
      {
        queryTabCtrlr.setTextToHilite(getTextToHilite());

        String mainText = curResult.hasDesc() ? ((HDT_RecordWithDescription) curResult).getDesc().getHtml() : "";

        MainTextWrapper.setReadOnlyHTML(mainText, webView.getEngine(), new TextViewInfo(), getRecordToHilite());

        if (curResult.getType() == hdtWork)
        {
          HDT_Work work = (HDT_Work) curResult;
          previewWindow.setPreview(pvsQueryTab, work.previewFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
        }
        else if (curResult.getType() == hdtMiscFile)
        {
          HDT_MiscFile miscFile = (HDT_MiscFile) curResult;
          previewWindow.setPreview(pvsQueryTab, miscFile.filePath(), miscFile);
        }
        else if ((curResult.getType() == hdtWorkFile) || (curResult.getType() == hdtPerson))
          previewWindow.setPreview(pvsQueryTab, ((HDT_RecordWithPath) curResult).filePath(), curResult);
        else
          previewWindow.clearPreview(pvsQueryTab);
      }
      else
      {
        webView.getEngine().loadContent("");
        previewWindow.clearPreview(pvsQueryTab);
      }
    }
    else
    {
      webView.getEngine().loadContent("");

      if (curResult == null)
        ui.updateBottomPanel(false);
    }

    queryTabCtrlr.setFavNameToggle(fav != null);

    programmaticFavNameChange = true;
    tfFavName.setText(fav == null ? "" : fav.name);
    programmaticFavNameChange = false;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void favNameChange()
  {
    if (programmaticFavNameChange) return;
    fav = null;
    queryTabCtrlr.setFavNameToggle(false);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void setRecord(HDT_Record record)
  {
    ResultsRow targetRow = record == null ? null :recordToRow.get(record);
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

      htFields.selectID(0, row, type.getCode());

      if (query > -1)
      {
        htFields.selectID(1, row, query);

        if (op1 != null)
        {
          if (getCellID(op1) > 0)
            htFields.selectID(2, row, getCellID(op1));
          else if (getCellText(op1).isEmpty())
            htFields.selectType(2, row, getCellType(op1));
          else
            row.setCellValue(2, op1.clone());
        }

        if (op2 != null)
        {
          if (getCellID(op2) > 0)
            htFields.selectID(3, row, getCellID(op2));
          else if (getCellText(op2).isEmpty())
            htFields.selectType(3, row, getCellType(op2));
          else
            row.setCellValue(3, op2.clone());
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
      NewQueryFavDlgCtrlr ctrlr = NewQueryFavDlgCtrlr.build(tfFavName.getText());

      if (ctrlr.showModal() == false) return;

      fav = new QueryFavorite();

      fav.name = ctrlr.getNewName();
      fav.autoexec = ctrlr.getAutoExec();

      htFields.dataRows().forEach(row ->
      {
        QueryRow queryRow = new QueryRow();

        for (int colNdx = 0; colNdx < 6; colNdx++)
          queryRow.cells[colNdx] = row.getCell(colNdx).clone();

        fav.rows.add(queryRow);
      });

      ui.mnuQueries.getItems().add(new FavMenuItem(fav));

      programmaticFavNameChange = true;
      tfFavName.setText(fav.name);
      programmaticFavNameChange = false;

      queryTabCtrlr.setFavNameToggle(true);
    }

    else
    {
      fav.removeFromList(ui.mnuQueries.getItems());
      fav = null;

      programmaticFavNameChange = true;
      tfFavName.setText("");
      programmaticFavNameChange = false;

      queryTabCtrlr.setFavNameToggle(false);
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
      for (int colNdx = 0; colNdx < 6; colNdx++)
        row.setCellValue(colNdx, queryRow.cells[colNdx].clone());
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
      for (int colNdx = 1; colNdx <= 4; colNdx++)
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

    queryTabCtrlr.updateCB(this);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void switchToRecordMode()
  {
    if (inRecordMode) return;

    reportTable.removeFromParent();
    addToParent(tvResults, apResults);

    inRecordMode = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void executeReport(HyperTableRow row, boolean setCaption)
  {
    switchToReportMode();

    if (setCaption)
      setCaption();

    ReportEngine reportEngine = ReportEngine.createEngine(row.getID(1));

    if (reportEngine == null)
    {
      reportTable.clear();
      return;
    }

    reportTable.format(reportEngine);

    HyperTask task = new HyperTask("GenerateReport") { @Override protected Boolean call() throws Exception
    {
      updateMessage("Generating report...");
      updateProgress(0, 1);

      reportEngine.generate(this, row.getCell(2), row.getCell(3), row.getCell(4));

      return true;
    }};

    if (!HyperTask.performTaskWithProgressDialog(task)) return;

    reportTable.inject(reportEngine);

    if (reportEngine.alwaysShowDescription() && (reportEngine.getRows().size() > 0))
      queryTabCtrlr.chkShowDesc.setSelected(true);
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

 // if any of the queries are unfiltered, they will all be treated as unfiltered

  boolean btnExecuteClick(boolean setCaption)
  {
    for (HyperTableRow row : htFields.dataRows())
    {
      if (QueryType.codeToVal(row.getID(0)) == qtReport)
      {
        htFields.setDataRows(List.of(row));

        executeReport(row, setCaption);
        return true;
      }
    }

    if (db.isLoaded() == false) return false;

    switchToRecordMode();

    boolean needMentionsIndex = false, showDesc = false;

    Map<HyperTableRow, Query<?>> queries = new LinkedHashMap<>();
    Map<HyperTableRow, QuerySource> sources = new LinkedHashMap<>();

    for (HyperTableRow row : htFields.dataRows())
    {
      int queryID = row.getID(1);
      if (queryID < 0) continue;

      if (queryID == QUERY_ANY_FIELD_CONTAINS)
        showDesc = true;

      QueryType type = getQueryType(row);
      Query<?> query = QueryTabCtrlr.queryTable.get(type, queryID);
      queries.put(row, query);
      sources.put(row, query.getSource(type, row));

      if ((type == qtAllRecords) && ((queryID == GeneralQueries.QUERY_LINKING_TO_RECORD)    ||
                                     (queryID == GeneralQueries.QUERY_MATCHING_RECORD  )    ||
                                     (queryID == GeneralQueries.QUERY_MATCHING_STRING  )))
        showDesc = true;

      if (query.needsMentionsIndex())
        needMentionsIndex = true;
    }

    if (needMentionsIndex && (db.waitUntilRebuildIsDone() == false))
      return false;

    resultsTable.reset();
    recordToRow.clear();
    webView.getEngine().loadContent("");

    if (setCaption)
      setCaption();

    QuerySource combinedSource = getCombinedRecordSource(sources);

    searchLinkedRecords = combinedSource.recordType() != hdtNone;
    int total = combinedSource.size();

    // Evaluate record queries

    HyperTask task = new HyperTask("Query")
    {
      //---------------------------------------------------------------------------

      @Override protected Boolean call() throws HyperDataException, TerminateTaskException
      {
        boolean firstCall = true;

        recordTypeToColumnGroup.clear();
        resultsBackingList.clear();

        updateMessage("Running query...");
        updateProgress(0, 1);

        Iterator<HDT_Record> recordIterator = combinedSource.iterator();

        for (int recordNdx = 0; recordIterator.hasNext(); recordNdx++)
        {
          if (isCancelled())
          {
            cleanup();
            throw new TerminateTaskException();
          }

          if ((recordNdx % 50) == 0)
            updateProgress(recordNdx, total);

          HDT_Record record = recordIterator.next();

          boolean lastConnectiveWasOr = false, firstRow = true, add = false;

          try
          {
            for (Entry<HyperTableRow, QuerySource> entry : sources.entrySet())
            {
              HyperTableRow row = entry.getKey();
              QuerySource source = entry.getValue();
              Query<?> query = queries.get(row);

              if (source.contains(record))
              {
                boolean result = evaluate(query, record, row, row.getCell(2), row.getCell(3), row.getCell(4), firstCall, recordNdx == (total - 1));
                firstCall = false;

                if      (firstRow)            add = result;
                else if (lastConnectiveWasOr) add = add || result;
                else                          add = add && result;
              }

              lastConnectiveWasOr = row.getID(5) == QueryTabCtrlr.OR_CONNECTIVE_ID;
              firstRow = false;
            }
          }
          catch (HyperDataException e)
          {
            cleanup();
            throw e;
          }

          if (add)
            addRecord(record, false);
        }

        cleanup();

        return true;
      }

      //---------------------------------------------------------------------------

      @SuppressWarnings("unchecked")
      private <HDT_T extends HDT_Record> boolean evaluate(Query<?> query, HDT_T record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall) throws HyperDataException
      {
        return ((Query<HDT_T>)query).evaluate(record, row, op1, op2, op3, firstCall, lastCall);
      }

      private void cleanup()
      {
        queries.values().forEach(Query::cleanup);
      }

      //---------------------------------------------------------------------------
    };

    if (!HyperTask.performTaskWithProgressDialog(task)) return false;

    Platform.runLater(() -> resultsTable.getTV().setItems(FXCollections.observableList(resultsBackingList)));

    recordTypeToColumnGroup.forEach(this::addColumns);

    if (showDesc)
      queryTabCtrlr.chkShowDesc.setSelected(true);

    curQV.refreshView(false);
    return true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void addColumns(RecordType recordType, ColumnGroup group)
  {
    for (ColumnGroupItem item : group)
    {
      if (item.tag == tagName) continue;

      ResultColumn<? extends Comparable<?>> col = null;
      EnumMap<RecordType, ColumnGroupItem> map = new EnumMap<>(RecordType.class);
      map.put(recordType, item);

      for (ColumnGroup grp : colGroups) for (ColumnGroupItem otherItem : grp)
        if ((item.tag != tagNone) && (item.tag == otherItem.tag))
        {
          map.put(grp.recordType, otherItem);

          if (otherItem.col != null)
          {
            col = otherItem.col;

            if (item.relType == rtNone)
              col.setVisible(true);

            col.map.putAll(map);
            map = col.map;
          }
        }

      if (col == null)
        col = resultsTable.addNonGeneralColumn(map);

      for (ColumnGroupItem otherItem : map.values())
        otherItem.col = col;
    }
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
        addColumns(recordType, colGroup);

      colGroups.add(colGroup);
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
    return queryHasOperand(query, queryType, opNum, null);
  }

  private static boolean queryHasOperand(Query<?> query, QueryType queryType, int opNum, HyperTableCell prevOp)
  {
    return (queryType != qtReport) && query.hasOperand(opNum, prevOp);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_Record getRecordToHilite()
  {
    for (HyperTableRow row : htFields.dataRows())
    {
      if (row.getID(0) == qtAllRecords.getCode())
      {
        switch (row.getID(1))
        {
          case GeneralQueries.QUERY_LINKING_TO_RECORD : case GeneralQueries.QUERY_MATCHING_RECORD :

            HDT_Record record = HyperTableCell.getRecord(row.getCell(3));
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
      if (row.getID(1) > -1)
      {
        for (int colNdx = 2; colNdx <= 4; colNdx++)
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

    VariablePopulator vp = row.getPopulator(opNum + 1);
    vp.setPopulator(row, null);
    vp.setRestricted(row, true);
    row.setCellValue(opNum + 1, "", hdtNone);

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

    return (query == null) || query.initRow(row, htFields.getPopulator(2), htFields.getPopulator(3), htFields.getPopulator(4));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  //returns true if subsequent cells need to be updated
  private boolean op1Change(Query<?> query, HyperTableCell op1, HyperTableRow row)
  {
    if (clearingOperand || (db.isLoaded() == false)) return false;

    return query.op1Change(op1, row, htFields.getPopulator(2), htFields.getPopulator(3), htFields.getPopulator(4));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // returns true if subsequent cells need to be updated
  private boolean op2Change(Query<?> query, HyperTableCell op2, HyperTableRow row)
  {
    if (clearingOperand || (db.isLoaded() == false)) return false;

    HyperTableCell op1 = row.getCell(2);

    return query.op2Change(op1, op2, row, htFields.getPopulator(2), htFields.getPopulator(3), htFields.getPopulator(4));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void resetFields()
  {
    disableAutoShowDropdownList = true;

    htFields.clear();

    HyperTableRow row = htFields.newDataRow();
    htFields.selectID(0, row, qtAllRecords.getCode());
    htFields.selectID(1, row, QUERY_ANY_FIELD_CONTAINS);
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
