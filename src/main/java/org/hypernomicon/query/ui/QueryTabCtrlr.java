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
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.Set;

import com.google.common.collect.ListMultimap;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.HashBasedTable;

import org.apache.commons.io.FileUtils;
import org.controlsfx.control.MasterDetailPane;

import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ChangeListener;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.concurrent.Worker;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
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
import org.hypernomicon.App;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.query.*;
import org.hypernomicon.query.reports.ReportEngine;
import org.hypernomicon.query.reports.ReportTable;
import org.hypernomicon.query.sources.CombinedFilteredQuerySource;
import org.hypernomicon.query.sources.CombinedUnfilteredQuerySource;
import org.hypernomicon.query.sources.QuerySource;
import org.hypernomicon.view.HyperFavorites.FavMenuItem;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.HyperFavorites.QueryRow;
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
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.query.ui.ResultsTable.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

//---------------------------------------------------------------------------

public class QueryTabCtrlr extends HyperTab<HDT_Record, HDT_Record>
{
  public final class QueryView
  {
    private MasterDetailPane spMain, spLower;
    private TableView<HyperTableRow> tvFields;
    private TableView<ResultsRow> tvResults;
    private AnchorPane apDescription, apResults;

    private HyperTable htFields;
    ResultsTable resultsTable;
    private ReportTable reportTable;

    public final List<ResultsRow> resultsBackingList = new ArrayList<>();
    private final Map<RecordType, ColumnGroup> recordTypeToColumnGroup = new LinkedHashMap<>();

    private Tab tab;
    private QueryFavorite fav = null;
    private HDT_Record curResult = null;

    private boolean programmaticFavNameChange = false,
                    disableAutoShowDropdownList = false,
                    inRecordMode = true,
                    searchLinkedRecords;

    private void setRecord(HDT_Record record)         { curResult = record; }
    private QueryType getQueryType(HyperTableRow row) { return QueryType.codeToVal(row.getID(0)); }
    private Query<?> getQuery(HyperTableRow row)      { return queryTable.get(QueryType.codeToVal(row.getID(0)), row.getID(1)); }
    public boolean getSearchLinkedRecords()           { return searchLinkedRecords; }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private QueryView()
    {
      EventHandler<ActionEvent> onAction = event ->
      {
        btnExecute.requestFocus();
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
      tab.setOnCloseRequest(event -> deleteView((Tab) event.getSource()));

      QueryViewCtrlr ctrlr = loader.getController();

      spMain = ctrlr.spMain;
      spLower = ctrlr.spLower;
      tvFields = ctrlr.tvFields;
      tvResults = ctrlr.tvResults;
      apDescription = ctrlr.apDescription;
      apResults = ctrlr.apResults;

      htFields = new HyperTable(tvFields, 1, true, "");

      HyperTable.loadColWidthsForTable(tvFields.getColumns(), PREF_KEY_HT_QUERY_FIELDS);

      htFields.autoCommitListSelections = true;

      Populator queryTypePopulator = Populator.create(cvtQueryType, EnumSet.allOf(QueryType.class).stream()
        .map(queryType -> new HyperTableCell(queryType.getCode(), queryType.getCaption(), queryType.getRecordType()))
        .collect(Collectors.toList()));

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
            row.setCellValue(nextColNdx, pop.getChoiceByID(null, EQUAL_TO_OPERAND_ID));
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

      htFields.addColAltPopulatorWithActionHandler(hdtNone, ctDropDown, new VariablePopulator(), onAction);
      htFields.addColAltPopulator(hdtNone, ctDropDownList, Populator.create(cvtConnective, andCell, orCell));

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

      resultsTable.reset();
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
      if (results().size() > 0)
      {
        ui.updateBottomPanel(false);
        curResult = selRowNdx > -1 ? results().get(selRowNdx).getRecord() : null;

        if (curResult != null)
        {
          textToHilite = getTextToHilite();

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

      setFavNameToggle(fav != null);

      programmaticFavNameChange = true;
      tfName.setText(fav == null ? "" : fav.name);
      programmaticFavNameChange = false;
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void setFavNameToggle(boolean selected)
    {
      btnToggleFavorite.setText(selected ? "Remove from favorites" : "Add to favorites");
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void favNameChange()
    {
      if (programmaticFavNameChange) return;
      fav = null;
      setFavNameToggle(false);
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    // Returns true if search was completed

    private boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite newFav, HyperTableCell op1, HyperTableCell op2, String caption)
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

    private void btnFavoriteClick()
    {
      if (db.isLoaded() == false) return;

      if (fav == null)
      {
        NewQueryFavDlgCtrlr ctrlr = NewQueryFavDlgCtrlr.build(tfName.getText());

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
        tfName.setText(fav.name);
        programmaticFavNameChange = false;

        setFavNameToggle(true);
      }

      else
      {
        fav.removeFromList(ui.mnuQueries.getItems());
        fav = null;

        programmaticFavNameChange = true;
        tfName.setText("");
        programmaticFavNameChange = false;

        setFavNameToggle(false);
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

      updateCB();
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
        chkShowDesc.setSelected(true);
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    // sources parameter is assumed empty and needs to be populated by this function so it can be used by the caller

    private QuerySource getCombinedRecordSource(Map<HyperTableRow, QuerySource> sources)
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

    private boolean btnExecuteClick(boolean setCaption)
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
        Query<?> query = queryTable.get(type, queryID);
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

                lastConnectiveWasOr = row.getID(5) == OR_CONNECTIVE_ID;
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
        chkShowDesc.setSelected(true);

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

      if (addToObsList)
      {
        resultsTable.getTV().getItems().add(new ResultsRow(record));
        refreshView(false);
      }
      else
        resultsBackingList.add(new ResultsRow(record));
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private boolean queryHasOperand(Query<?> query, QueryType queryType, int opNum)
    {
      return queryHasOperand(query, queryType, opNum, null);
    }

    private boolean queryHasOperand(Query<?> query, QueryType queryType, int opNum, HyperTableCell prevOp)
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

              HDT_Record record = getRecord(row.getCell(3));
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

    private String getTextToHilite()
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

    //returns true if subsequent cells need to be updated
    private boolean queryChange(Query<?> query, HyperTableRow row)
    {
      if (db.isLoaded() == false) return false;

      QueryType queryType = getQueryType(row);

      if (queryType == qtReport)
        return true;

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

    private void resetFields()
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

    private void updateCB()
    {
      if (cb == null) return;

      if (propToUnbind != null)
        cb.itemsProperty().unbindBidirectional(propToUnbind);

      if (inRecordMode == false)
      {
        cb.setItems(null);
        return;
      }

      cb.itemsProperty().bindBidirectional(tvResults.itemsProperty());

      propToUnbind = tvResults.itemsProperty();

      if (cbListenerToRemove != null)
        cb.getSelectionModel().selectedItemProperty().removeListener(cbListenerToRemove);

      cb.getSelectionModel().select(tvResults.getSelectionModel().getSelectedItem());

      cbListenerToRemove = (ob, oldValue, newValue) ->
      {
        if ((newValue != null) && (newValue.getRecord() != null))
        {
          tvResults.getSelectionModel().select(newValue);
          if (noScroll == false) HyperTable.scrollToSelection(tvResults, false);
        }
      };

      cb.getSelectionModel().selectedItemProperty().addListener(cbListenerToRemove);

      if (tvListenerToRemove != null)
        tvResults.getSelectionModel().selectedItemProperty().removeListener(tvListenerToRemove);

      tvListenerToRemove = (ob, oldValue, newValue) ->
      {
        noScroll = true;
        cb.getSelectionModel().select(newValue);
        noScroll = false;
      };

      tvResults.getSelectionModel().selectedItemProperty().addListener(tvListenerToRemove);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------



//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final int  QUERY_WITH_NAME_CONTAINING  = 1,
                           QUERY_ANY_FIELD_CONTAINS    = 2,
                           QUERY_LIST_ALL              = 3,
                           QUERY_WHERE_FIELD           = 4,
                           QUERY_WHERE_RELATIVE        = 5,
                           QUERY_FIRST_NDX             = 6,

                           EQUAL_TO_OPERAND_ID         = 1,           AND_CONNECITVE_ID = 1,
                           NOT_EQUAL_TO_OPERAND_ID     = 2,           OR_CONNECTIVE_ID  = 2,
                           CONTAINS_OPERAND_ID         = 3,           TRUE_ID  = 1,
                           DOES_NOT_CONTAIN_OPERAND_ID = 4,           FALSE_ID = 2,
                           IS_EMPTY_OPERAND_ID         = 5,           UNSET_ID = 3,
                           IS_NOT_EMPTY_OPERAND_ID     = 6;

  public static final HyperTableCell andCell = new HyperTableCell(AND_CONNECITVE_ID, "and", hdtNone),
                                     orCell  = new HyperTableCell(OR_CONNECTIVE_ID , "or" , hdtNone),
                                     trueCell  = new HyperTableCell(TRUE_ID , "True" , hdtNone),
                                     falseCell = new HyperTableCell(FALSE_ID, "False", hdtNone),
                                     unsetCell = new HyperTableCell(UNSET_ID, "Unset", hdtNone);

  @FXML private CheckBox chkShowFields, chkShowDesc;
  @FXML private Button btnToggleFavorite, btnClear, btnExecute;
  @FXML private TextField tfName;
  @FXML private TabPane tabPane;
  @FXML private Tab tabNew;
  @FXML private AnchorPane apOrigDescription;
  @FXML private WebView webView;

  private ComboBox<CheckBoxOrCommand> fileBtn = null;

  private static boolean noScroll = false;
  private boolean clearingViews = false, clearingOperand = false;
  private String textToHilite = "";
  private ObjectProperty<ObservableList<ResultsRow>> propToUnbind = null;
  private ChangeListener<ResultsRow> cbListenerToRemove = null, tvListenerToRemove = null;
  private ComboBox<ResultsRow> cb;
  private final BooleanProperty includeEdited = new SimpleBooleanProperty(),
                                excludeAnnots = new SimpleBooleanProperty(),
                                entirePDF     = new SimpleBooleanProperty();

  private final List<QueryView> queryViews = new ArrayList<>();

  private static final ListMultimap<QueryType, Query<?>> queryTypeToQueries = ArrayListMultimap.create();
  private static final HashBasedTable<QueryType, Integer, Query<?>> queryTable = HashBasedTable.create();

  public void refreshTables()                       { queryViews.forEach(qv -> qv.resultsTable.getTV().refresh()); }
  public void setCB(ComboBox<ResultsRow> cb)        { this.cb = cb; updateCB(); }
  private static void updateCB()                    { if (curQV != null) curQV.updateCB(); }
  public static void btnExecuteClick()              { curQV.btnExecuteClick(true); }   // if any of the queries are unfiltered, they
                                                                                       // will all be treated as unfiltered
  @Override protected RecordType type()             { return hdtNone; }
  @Override public void update()                    { curQV.refreshView(true); }
  @Override public void setRecord(HDT_Record rec)   { if (curQV != null) curQV.setRecord(rec); }
  @Override public int recordCount()                { return results().size(); }
  @Override public TextViewInfo mainTextInfo()      { return new TextViewInfo(MainTextUtil.webEngineScrollPos(webView.getEngine())); }
  @Override public void setDividerPositions()       { return; }
  @Override public void getDividerPositions()       { return; }
  @Override public boolean saveToRecord()           { return false; }
  @Override public HDT_Record activeRecord()        { return curQV == null ? null : curQV.curResult; }
  @Override public String recordName()              { return nullSwitch(activeRecord(), "", HDT_Record::getCBText); }
  @Override public int recordNdx()                  { return recordCount() > 0 ? curQV.tvResults.getSelectionModel().getSelectedIndex() : -1; }
  @Override public void findWithinDesc(String text) { if (activeRecord() != null) MainTextWrapper.hiliteText(text, webView.getEngine()); }

  @FXML private void mnuCopyToFolderClick()         { copyFilesToFolder(true); }
  @FXML private void mnuShowSearchFolderClick()     { if (db.isLoaded()) launchFile(db.resultsPath()); }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void clearOperands(HyperTableRow row, int startOpNum)
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

  private void clearOperand(HyperTableRow row, int opNum)
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

  public void removeRecord(HDT_Record record)
  {
    queryViews.forEach(qv -> qv.resultsTable.getTV().getItems().removeIf(row -> row.getRecord() == record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<ResultsRow> results()
  {
    if ((curQV == null) || (curQV.inRecordMode == false)) return FXCollections.observableArrayList();

    return curQV.resultsTable.getTV().getItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Populator operandPopulator()
  {
    return Populator.create(cvtOperand,

        new HyperTableCell(EQUAL_TO_OPERAND_ID        , "Is or includes record", hdtNone),
        new HyperTableCell(NOT_EQUAL_TO_OPERAND_ID    , "Excludes record"      , hdtNone),
        new HyperTableCell(CONTAINS_OPERAND_ID        , "Contains text"        , hdtNone),
        new HyperTableCell(DOES_NOT_CONTAIN_OPERAND_ID, "Doesn't contain text" , hdtNone),
        new HyperTableCell(IS_EMPTY_OPERAND_ID        , "Is empty"             , hdtNone),
        new HyperTableCell(IS_NOT_EMPTY_OPERAND_ID    , "Is not empty"         , hdtNone));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void init()
  {
    GeneralQueries.addQueries(queryTypeToQueries);
    FolderQueries .addQueries(queryTypeToQueries);
    PersonQueries .addQueries(queryTypeToQueries);
    WorkQueries   .addQueries(queryTypeToQueries);

    queryTypeToQueries.forEach((queryType, query) -> queryTable.put(queryType, query.getID(), query));

  //---------------------------------------------------------------------------

    btnExecute.setOnAction(event -> btnExecuteClick());
    btnClear.setOnAction(event -> curQV.resetFields());
    btnToggleFavorite.setOnAction(event -> curQV.btnFavoriteClick());

    tabPane.getTabs().addListener((Change<? extends Tab> c) -> Platform.runLater(tabPane::requestLayout));

    tabPane.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (clearingViews == false) tabPaneChange(newValue);
      Platform.runLater(tabPane::requestLayout);
    });

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      if (curQV.inRecordMode == false)
      {
        MainTextUtil.handleJSEvent("", webView.getEngine(), new TextViewInfo());
        return;
      }

      HDT_Record record = curQV.resultsTable.selectedRecord();
      if (record == null) return;

      textToHilite = curQV.getTextToHilite();
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

    tfName.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || newValue.equals(safeStr(oldValue)) || (curQV == null)) return;

      curQV.favNameChange();
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
    }

    fileBtn = CheckBoxOrCommand.createComboBox(items, "Files");

    fileBtn.setMaxSize (64.0, 24.0);
    fileBtn.setMinSize (64.0, 24.0);
    fileBtn.setPrefSize(64.0, 24.0);

    AnchorPane.setTopAnchor  (fileBtn, 2.0);
    AnchorPane.setRightAnchor(fileBtn, 0.0);

    children.add(children.indexOf(tabPane), fileBtn);

    fileBtn.setOnHidden(event -> addFilesButton()); // This is necessary to deselect list item without making the button caption disappear
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void tabPaneChange(Tab newValue)
  {
    QueryView qV;

    if (newValue == tabNew)
    {
      qV = addQueryView();
      tabPane.getSelectionModel().select(qV.tab);
      safeFocus(qV.tvFields);
    }
    else
    {
      qV = findFirst(queryViews, view -> view.tab == newValue);
      if (qV == null) return;
    }

    if (curQV != null)
    {
      curQV.spMain .showDetailNodeProperty().unbind();
      curQV.spLower.showDetailNodeProperty().unbind();
    }

    chkShowFields.setSelected(qV.spMain .isShowDetailNode());
    chkShowDesc  .setSelected(qV.spLower.isShowDetailNode());

    qV.spMain .showDetailNodeProperty().bind(chkShowFields.selectedProperty());
    qV.spLower.showDetailNodeProperty().bind(chkShowDesc  .selectedProperty());

    curQV = qV;
    updateCB();

    removeFromParent(webView);
    curQV.apDescription.getChildren().setAll(webView);

    qV.refreshView(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void deleteView(Tab tab)
  {
    QueryView qV = findFirst(queryViews, view -> view.tab == tab);
    if (qV == null) return;

    HyperTable.saveColWidthsForTable(qV.tvFields.getColumns(), PREF_KEY_HT_QUERY_FIELDS, false);
    queryViews.remove(qV);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private QueryView addQueryView()
  {
    QueryView newQV = new QueryView();

    queryViews.add(newQV);
    newQV.resetFields();

    return newQV;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    clearingViews = true;

    removeFromParent(webView);
    addToParent(webView, apOrigDescription);

    queryViews.removeIf(queryView ->
    {
      HyperTable.saveColWidthsForTable(queryView.tvFields.getColumns(), PREF_KEY_HT_QUERY_FIELDS, false);
      tabPane.getTabs().remove(queryView.tab);
      return true;
    });

    clearingViews = false;

    if (ui.isShuttingDown() == false)
      webView.getEngine().loadContent("");

    QueryView newQV = addQueryView();
    tabPane.getSelectionModel().select(newQV.tab);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueries(QueryPopulator pop, HyperTableRow row, QueryType newType)
  {
    if (newType == qtReport)
    {
      ReportEngine.addQueries(pop, row);
      return;
    }

    queryTypeToQueries.get(newType).forEach(query -> pop.addEntry(row, query.getID(), query.getDescription()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite fav, HyperTableCell op1, HyperTableCell op2, String caption)
  {
    if ((type != qtReport) && (db.isLoaded() == false)) return false;

    QueryView qV = addQueryView();
    tabPane.getSelectionModel().select(qV.tab);

    return qV.showSearch(doSearch, type, query, fav, op1, op2, caption);
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
    SearchResultFileList fileList = new SearchResultFileList(entirePDF.get());

    if ((db.isLoaded() == false) || (results().size() < 1)) return false;

    HyperTask task = new HyperTask("BuildListOfFilesToCopy") { @Override protected Boolean call() throws Exception
    {
      updateMessage("Building list...");

      updateProgress(0, 1);

      List<ResultsRow> resultRowList = onlySelected ? curQV.resultsTable.getTV().getSelectionModel().getSelectedItems() : results();

      int ndx = 0; for (ResultsRow row : resultRowList)
      {
        HDT_Record record = row.getRecord();
        if (record instanceof HDT_RecordWithPath)
          fileList.addRecord((HDT_RecordWithPath)record, includeEdited.getValue());

        if (isCancelled())
          throw new TerminateTaskException();

        updateProgress(ndx++, resultRowList.size());
      }

      return true;
    }};

    if (!HyperTask.performTaskWithProgressDialog(task))
      return false;

    boolean startWatcher = folderTreeWatcher.stop();

    task = new HyperTask("CopyingFiles") { @Override protected Boolean call() throws Exception
    {
      updateMessage("Copying files...");

      updateProgress(0, 1);

      fileList.copyAll(excludeAnnots.getValue(), this);
      return true;
    }};

    HyperTask.performTaskWithProgressDialog(task);

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

}
