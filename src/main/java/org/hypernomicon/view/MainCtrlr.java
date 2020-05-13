/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.view;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.queryEngines.AllQueryEngine.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.queryEngines.QueryEngine.QueryType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.tabs.QueryTabCtrlr.*;
import static org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource.*;

import org.hypernomicon.App;
import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibTexBibData;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.data.GUIBibData;
import org.hypernomicon.bib.data.RISBibData;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.queryEngines.QueryEngine.QueryType;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.WebButton;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.dialogs.*;
import org.hypernomicon.view.fileManager.FileRow;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.view.settings.SettingsDlgCtrlr;
import org.hypernomicon.view.settings.SettingsDlgCtrlr.SettingsPage;
import org.hypernomicon.view.settings.WebButtonSettingsCtrlr;
import org.hypernomicon.view.tabs.*;
import org.hypernomicon.view.tabs.HyperTab.TabEnum;
import org.hypernomicon.view.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.view.wrappers.*;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.lang3.SystemUtils;
import org.jbibtex.ParseException;
import org.jbibtex.TokenMgrException;
import com.google.common.collect.EnumHashBiMap;
import com.melloware.jintellitype.JIntellitype;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.application.Platform;
import javafx.beans.binding.BooleanExpression;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.geometry.Point2D;
import javafx.geometry.Side;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

public final class MainCtrlr
{
  @FXML Tab tabOmniSelector;
  @FXML TableView<HyperTableRow> tvFind;
  @FXML private AnchorPane apFindBackground, apGoTo, apListGoTo, apStatus, midAnchorPane;
  @FXML private Button btnAdvancedSearch, btnBibMgr, btnDecrement, btnFileMgr, btnIncrement, btnMentions, btnPreviewWindow,
                       btnSave, btnDelete, btnRevert, btnCreateNew, btnBack, btnForward, btnSaveAll, btnTextSearch;
  @FXML private CheckMenuItem mnuAutoImport;
  @FXML private ComboBox<HyperTableCell> cbGoTo;
  @FXML private GridPane gpBottom;
  @FXML private HBox topHBox;
  @FXML private ImageView ivDates;
  @FXML private Label lblProgress;
  @FXML private Menu mnuFolders;
  @FXML private MenuBar menuBar;
  @FXML private MenuItem mnuAddToQueryResults, mnuChangeID, mnuCloseDatabase, mnuExitNoSave, mnuFindNextAll, mnuFindNextInName,
                         mnuFindPreviousAll, mnuFindPreviousInName, mnuFindWithinAnyField, mnuFindWithinName, mnuImportBibClipboard,
                         mnuImportBibFile, mnuNewCountry, mnuNewDatabase, mnuNewField, mnuNewPersonStatus, mnuNewRank, mnuVideos,
                         mnuRecordSelect, mnuRevertToDiskCopy, mnuSaveReloadAll, mnuToggleFavorite, mnuImportWork, mnuImportFile;
  @FXML private ProgressBar progressBar;
  @FXML private SeparatorMenuItem mnuBibImportSeparator;
  @FXML private SplitMenuButton btnGoTo;
  @FXML private Tab tabViewSelector;
  @FXML private TabPane selectorTabPane, tabPane;
  @FXML private TextField tfID, tfOmniGoTo, tfRecord;
  @FXML private ToggleButton btnPointerPreview;
  @FXML private ToolBar topToolBar;
  @FXML public Label lblStatus;
  @FXML public Menu mnuFavorites, mnuQueries;
  @FXML public Tab tabArguments, tabDebates, tabFiles, tabInst, tabNotes, tabPersons, tabPositions, tabQueries, tabTerms, tabTree, tabWorks;
  @FXML public ToggleButton btnPointerLaunch;

  public final WindowStack windows = new WindowStack();
  public final Map<String, WebButton> webButtonMap = new HashMap<>();
  public HyperViewSequence viewSequence;
  private final EnumHashBiMap<TabEnum, Tab> selectorTabs = EnumHashBiMap.create(TabEnum.class);
  private HyperFavorites favorites;
  private OmniFinder omniFinder;
  private HyperCB hcbGoTo;
  private HyperTable htFind;
  public final ComboBox<TreeRow> cbTreeGoTo = new ComboBox<>();
  private ComboBox<ResultsRow> cbResultGoTo = null;
  private ClickHoldButton chbBack, chbForward;
  private TextField tfSelector = null;
  public final TreeSelector treeSelector = new TreeSelector();

  public Tooltip ttDates;
  private boolean selectorTabChangeIsProgrammatic = false, maximized = false, internetNotCheckedYet = true, shuttingDown = false;
  private double toolBarWidth = 0.0, maxWidth = 0.0, maxHeight = 0.0;
  private long lastImportTime = 0;
  private FilePath lastImportFilePath = null;

  private static final String TREE_SELECT_BTN_CAPTION = "Select";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<ResultsRow> results()          { return curQV.resultsTable.getTV().getItems(); }
  MenuBar getMenuBar()                        { return menuBar; }
  public TreeWrapper getTree()                { return treeHyperTab().getTree(); }
  private Stage primaryStage()                { return app.getPrimaryStage(); }
  public boolean isShuttingDown()             { return shuttingDown; }

  @FXML private void mnuExitClick()           { shutDown(true, true, true); }
  @FXML private void mnuExitNoSaveClick()     { if (confirmDialog("Abandon changes and quit?")) shutDown(false, true, false); }
  @FXML private void mnuOpenClick()           { openDB(null); }
  @FXML private void mnuAboutClick()          { AboutDlgCtrlr.build().showModal(); }
  @FXML private void mnuChangeFavOrderClick() { FavOrderDlgCtrlr.build().showModal(); }
  @FXML private void mnuSettingsClick()       { if (!cantSaveRecord()) SettingsDlgCtrlr.build().showModal(); }
  @FXML private void mnuFindMentionsClick()   { if (!cantSaveRecord()) searchForMentions(activeRecord(), false); }

  public PersonTabCtrlr personHyperTab  () { return getHyperTab(personTabEnum  ); }
  public InstTabCtrlr   instHyperTab    () { return getHyperTab(instTabEnum    ); }
  public WorkTabCtrlr   workHyperTab    () { return getHyperTab(workTabEnum    ); }
  public FileTabCtrlr   fileHyperTab    () { return getHyperTab(fileTabEnum    ); }
  public DebateTab      debateHyperTab  () { return getHyperTab(debateTabEnum  ); }
  public PositionTab    positionHyperTab() { return getHyperTab(positionTabEnum); }
  public ArgumentTab    argumentHyperTab() { return getHyperTab(argumentTabEnum); }
  public NoteTab        noteHyperTab    () { return getHyperTab(noteTabEnum    ); }
  public TermTab        termHyperTab    () { return getHyperTab(termTabEnum    ); }
  public QueryTabCtrlr  queryHyperTab   () { return getHyperTab(queryTabEnum   ); }
  public TreeTabCtrlr   treeHyperTab    () { return getHyperTab(treeTabEnum    ); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void init() throws IOException
  {
    menuBar.setUseSystemMenuBar(true);

    updateProgress("", -1);

    Stage stage = primaryStage();

    windows.push(stage);

    stage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (windows.getCyclingFocus()) return;

      if (Boolean.TRUE.equals(newValue) == false) return;

      windows.push(stage);
    });

    stage.widthProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue.doubleValue() < oldValue.doubleValue())
        maximized = false;
      else if ((newValue.doubleValue() >= maxWidth) && (stage.getHeight() >= maxHeight) && (maxWidth > 0.0))
      {
        maximized = true;
        maxHeight = stage.getHeight();
        maxWidth = newValue.doubleValue();
      }
    });

    stage.heightProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue.doubleValue() < oldValue.doubleValue())
        maximized = false;
      else if ((newValue.doubleValue() >= maxHeight) && (stage.getWidth() >= maxWidth) && (maxHeight > 0.0))
      {
        maximized = true;
        maxHeight = newValue.doubleValue();
        maxWidth = stage.getWidth();
      }
    });

    stage.maximizedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue) && Boolean.FALSE.equals(oldValue))
      {
        maximized = true;
        maxWidth = stage.getWidth();
        maxHeight = stage.getHeight();
      }
    });

    MainTextWrapper.init();

    ttDates = new Tooltip("No dates to show.");
    ttDates.setStyle("-fx-font-size: 14px;");

    PersonTabCtrlr   .addHyperTab(personTabEnum   , tabPersons     , "PersonTab.fxml");
    InstTabCtrlr     .addHyperTab(instTabEnum     , tabInst        , "InstTab.fxml");
    WorkTabCtrlr     .addHyperTab(workTabEnum     , tabWorks       , "WorkTab.fxml");
    FileTabCtrlr     .addHyperTab(fileTabEnum     , tabFiles       , "FileTab.fxml");

    new DebateTab()  .baseInit   (debateTabEnum   , tabDebates);
    new PositionTab().baseInit   (positionTabEnum , tabPositions);
    new ArgumentTab().baseInit   (argumentTabEnum , tabArguments);
    new NoteTab()    .baseInit   (noteTabEnum     , tabNotes);
    new TermTab()    .baseInit   (termTabEnum     , tabTerms);

    QueryTabCtrlr    .addHyperTab(queryTabEnum    , tabQueries     , "QueryTab.fxml");
    TreeTabCtrlr     .addHyperTab(treeTabEnum     , tabTree        , "TreeTab.fxml");

    addSelectorTab(omniTabEnum);
    addSelectorTab(listTabEnum);

    chbBack = new ClickHoldButton(btnBack, Side.TOP);
    chbForward = new ClickHoldButton(btnForward, Side.TOP);

    setToolTip(btnBack   , "Click to go back, hold to see history"   );
    setToolTip(btnForward, "Click to go forward, hold to see history");

    chbBack   .setOnAction(event -> btnBackClick());
    chbForward.setOnAction(event -> btnForwardClick());

    viewSequence = new HyperViewSequence(tabPane, chbForward, chbBack);

    tabViewSelector.disableProperty().bind(BooleanExpression.booleanExpression(tabQueries.selectedProperty().or(tabTree.selectedProperty())).not());

    setSelectorTab(selectorTabPane.getTabs().get(selectorTabPane.getTabs().size() - 1));

    hcbGoTo = new HyperCB(cbGoTo, ctDropDown, new RecordByTypePopulator());

    htFind = new HyperTable(tvFind, 1, false, PREF_KEY_HT_FIND); htFind.disableRefreshAfterCellUpdate = true;

    htFind.addIconCol();
    htFind.addCol(hdtNone, ctIncremental);
    htFind.addCol(hdtNone, ctNone);
    htFind.addCol(hdtNone, ctNone);

    htFind.setOnShowMore(() -> tfOmniGoToChange(tfOmniGoTo.getText(), true));

    htFind.addDefaultMenuItems();

    omniFinder = new OmniFinder(htFind);

    btnFileMgr.setOnAction(event -> runFileMgr());
    btnBibMgr .setOnAction(event -> runBibMgr());

    btnGoTo.setOnAction        (event -> btnGoToClick(false));
    mnuRecordSelect.setOnAction(event -> btnGoToClick(true));

    hcbGoTo.setInnerOnAction(event -> recordLookup());
    hcbGoTo.dontCreateNewRecord = true;

    mnuImportWork        .setOnAction(event -> importWorkFile(null, null, false));
    mnuImportFile        .setOnAction(event -> importMiscFile(null, null));
    mnuImportBibFile     .setOnAction(event -> importBibFile(null, null));
    mnuImportBibClipboard.setOnAction(event -> importBibFile(convertMultiLineStrToStrList(getClipboardText(false), false), null));

    mnuVideos            .setOnAction(event -> openWebLink("http://hypernomicon.org/support.html"));

    mnuFindNextAll       .setOnAction(event -> getTree().find(cbTreeGoTo.getEditor().getText(), true,  false));
    mnuFindPreviousAll   .setOnAction(event -> getTree().find(cbTreeGoTo.getEditor().getText(), false, false));
    mnuFindNextInName    .setOnAction(event -> getTree().find(cbTreeGoTo.getEditor().getText(), true,  true ));
    mnuFindPreviousInName.setOnAction(event -> getTree().find(cbTreeGoTo.getEditor().getText(), false, true ));

    btnSaveAll.       setOnAction(event -> saveAllToDisk(true, true, true));
    btnDelete.        setOnAction(event -> deleteCurrentRecord(true));
    btnRevert.        setOnAction(event -> update());
    btnAdvancedSearch.setOnAction(event -> showSearch(false, null, -1, null, null, null, ""));

    if (appPrefs.getBoolean(PREF_KEY_RIGHT_CLICK_TO_LAUNCH, true))
      btnPointerLaunch.setSelected(true);
    else
      btnPointerPreview.setSelected(true);

    btnPointerLaunch.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        appPrefs.putBoolean(PREF_KEY_RIGHT_CLICK_TO_LAUNCH, true);
    });

    btnPointerPreview.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        appPrefs.putBoolean(PREF_KEY_RIGHT_CLICK_TO_LAUNCH, false);
    });

    btnPointerLaunch.getToggleGroup().selectedToggleProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null)
        oldValue.setSelected(true);
    });

    setToolTip(btnPointerLaunch , "On right/secondary click on link to work record, launch work file");
    setToolTip(btnPointerPreview, "On right/secondary click on link to work record, show in preview window");

    setToolTip(btnMentions, "Show records whose description mentions this record");

    btnIncrement.setOnAction(event -> incDecClick(true));
    btnDecrement.setOnAction(event -> incDecClick(false));

    setToolTip(btnTextSearch    , "Search within description");
    setToolTip(btnAdvancedSearch, "Start a new search in Queries tab");
    setToolTip(btnPreviewWindow , "Open Preview Window");
    setToolTip(btnBibMgr        , "Open Bibliography Manager Window");
    setToolTip(btnFileMgr       , "Open File Manager Window");
    setToolTip(btnSaveAll       , "Save all records to XML files");

    apFindBackground.setOnMousePressed(event -> hideFindTable());

    tfOmniGoTo.setOnAction(event -> htFind.doRowAction());

    ttDates.setAutoHide(true);

    tabTree        .setGraphic(imgViewFromRelPath("resources/images/treeview-small.png"));
    tabQueries     .setGraphic(imgViewFromRelPath("resources/images/glasses-db.png"));
    tabOmniSelector.setGraphic(imgViewFromRelPath("resources/images/globe.png"));
    tabViewSelector.setGraphic(imgViewFromRelPath("resources/images/details.png"));

    favorites = new HyperFavorites(mnuFavorites, mnuQueries);

    forEachHyperTab(hyperTab ->
    {
      TabEnum hyperTabEnum = hyperTab.getTabEnum();
      HDT_RecordType recordType = getRecordTypeByTabEnum(hyperTabEnum);

      nullSwitch(imgViewForRecordType(recordType), graphic ->
      {
        hyperTab.getTab().setGraphic(graphic);

        nullSwitch(selectorTabs.get(hyperTabEnum), selectorTab -> selectorTab.setGraphic(imgViewForRecordType(recordType)));
      });
    });

    WebButtonSettingsCtrlr.loadPrefs();

    if (SystemUtils.IS_OS_MAC)
    {
      topHBox.getChildren().remove(topToolBar);
      topHBox.setMinHeight(0.0);
      topHBox.setPrefHeight(0.0);
      topHBox.setMaxHeight(0.0);
      midAnchorPane.getChildren().add(topToolBar);
      AnchorPane.setTopAnchor(topToolBar, 0.0);
      AnchorPane.setRightAnchor(topToolBar, 0.0);

      midAnchorPane.widthProperty().addListener((ob, oldValue, newValue) ->
      {
        if ((newValue != null) && (newValue.doubleValue() > 1))
          adjustToolBar(newValue.doubleValue());
      });
    }

    if (JIntellitype.isJIntellitypeSupported())  // In JavaFX 12, support will exist for forward and back mouse buttons so that
    {                                            // JIntellitype can be removed. See https://bugs.openjdk.java.net/browse/JDK-8090930
      JIntellitype.getInstance().addIntellitypeListener(code ->
      {
        switch (code)
        {
          case JIntellitype.APPCOMMAND_BROWSER_BACKWARD :

            if (primaryStage().isFocused())
              Platform.runLater(this::btnBackClick);
            else if (fileManagerDlg != null)
              if (fileManagerDlg.getStage().isShowing() && fileManagerDlg.getStage().isFocused())
                Platform.runLater(fileManagerDlg::btnBackClick);
            return;

          case JIntellitype.APPCOMMAND_BROWSER_FORWARD :

            if (primaryStage().isFocused())
              Platform.runLater(this::btnForwardClick);
            else if (fileManagerDlg != null)
              if (fileManagerDlg.getStage().isShowing() && fileManagerDlg.getStage().isFocused())
                Platform.runLater(fileManagerDlg::btnForwardClick);
            return;
        }
      });
    }

//---------------------------------------------------------------------------

    db.addDeleteHandler(record ->
    {
      if ((record.getType() == hdtPerson) && (personHyperTab().activeRecord() == record))
        personHyperTab().curPicture = null;  // User has already been asked if they want to delete the picture; don't ask again

      queryHyperTab().queryViews.forEach(qv -> qv.resultsTable.getTV().getItems().removeIf(row -> row.getRecord() == record));

      int ndx = favorites.indexOfRecord(record);

      if (ndx > -1)
      {
        mnuFavorites.getItems().remove(ndx);
        updateFavorites();
      }

      viewSequence.removeRecord(record);
    });

//---------------------------------------------------------------------------

    selectorTabPane.getSelectionModel().selectedItemProperty().addListener((ob, oldTab, newTab) ->
    {
      if ((newTab == null) || (oldTab == newTab)) return;

      if ((oldTab != null) && (oldTab != tabOmniSelector) && (oldTab != tabViewSelector))
        oldTab.setContent(null);

      if ((newTab != tabOmniSelector) && (newTab != tabViewSelector))
        newTab.setContent(apGoTo);

      if (selectorTabChangeIsProgrammatic) return;

      updateSelectorTab(true);
    });

//---------------------------------------------------------------------------

    tfOmniGoTo.setOnMouseClicked(event ->
    {
      if (htFind.getDataRowCount() > 0)
        showFindTable();
    });

//---------------------------------------------------------------------------

    tfOmniGoTo.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue != null) && (newValue.equals(oldValue) == false))
        tfOmniGoToChange(newValue, false);
    });

//---------------------------------------------------------------------------

    tfOmniGoTo.setOnKeyPressed(event ->
    {
      switch (event.getCode())
      {
        case DOWN : case UP : case PAGE_DOWN : case PAGE_UP :

          showFindTable();
          tvFind.fireEvent(event.copyFor(tvFind, tvFind));
          event.consume();
          break;

        default : break;
      }
    });

//---------------------------------------------------------------------------

    ivDates.setOnMouseEntered(event ->
    {
      if (ttDates.isShowing() == false)
        Platform.runLater(() -> ttDates.show(ivDates, event.getScreenX(), event.getScreenY()));
    });

//---------------------------------------------------------------------------

    tfRecord.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        tfRecord.setText("");
      else
        updateBottomPanel(true);
    });

//---------------------------------------------------------------------------

    tfID.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((Boolean.TRUE.equals(newValue) == false) && (activeRecord() != null))
        tfID.setText(String.valueOf(activeRecord().getID()));
      else
        tfID.setText("");
    });

//---------------------------------------------------------------------------

    tfRecord.setOnAction(event ->
    {
      if ((activeTabEnum() == treeTabEnum) || (activeTabEnum() == queryTabEnum)) return;
      if (activeRecord() == null)
      {
        tfRecord.setText("");
        return;
      }

      HDT_RecordType type = activeType();

      int curRecordNdx = db.records(type).getKeyNdxByID(activeRecord().getID()),
          newRecordNdx = parseInt(tfRecord.getText(), 0) - 1;

      if ((newRecordNdx != curRecordNdx) && (newRecordNdx >= 0) && (newRecordNdx < db.records(type).size()))
        goToRecord(db.records(type).getByKeyNdx(newRecordNdx), true);
      else
        tfRecord.setText("");
    });

//---------------------------------------------------------------------------

    btnPreviewWindow.setOnAction(event ->
    {
      PreviewSource src = determinePreviewContext();

      if (activeTabEnum() == fileTabEnum)
      {
        HDT_MiscFile miscFile = (HDT_MiscFile) activeRecord();

        if (miscFile == null)
          previewWindow.clearPreview(src);
        else
          previewWindow.setPreview(src, miscFile.filePath(), -1, -1, miscFile);
      }

      openPreviewWindow(src);
    });

//---------------------------------------------------------------------------

    tfID.setOnAction(event ->
    {
      if ((activeTabEnum() == treeTabEnum) || (activeTabEnum() == queryTabEnum)) return;

      HDT_Record record = activeRecord();
      if (record == null)
      {
        tfID.setText("");
        return;
      }

      int newRecordID = parseInt(tfID.getText(), -1);

      if ((record.getID() != newRecordID) && (newRecordID > 0) && (db.records(activeType()).getIDNdxByID(newRecordID) > -1))
        goToRecord(db.records(activeType()).getByID(newRecordID), true);
      else
        tfID.setText(String.valueOf(activeRecord().getID()));
    });

//---------------------------------------------------------------------------

    mnuFindWithinName.setOnAction(event ->
    {
      if (selectorTabEnum() == omniTabEnum)
        showSearch(true, QueryType.qtAllRecords, QUERY_WITH_NAME_CONTAINING, null, new HyperTableCell(-1, tfSelector.getText(), hdtNone), null, tfSelector.getText());
      else
        mnuFindWithinNameClick();
    });

//---------------------------------------------------------------------------

    mnuFindWithinAnyField.setOnAction(event ->
    {
      if (selectorTabEnum() == omniTabEnum)
        showSearch(true, QueryType.qtAllRecords, QUERY_ANY_FIELD_CONTAINS, null, new HyperTableCell(-1, tfSelector.getText(), hdtNone), null, tfSelector.getText());
      else
        showSearch(true, QueryType.fromRecordType(selectorType()), QUERY_ANY_FIELD_CONTAINS, null, new HyperTableCell(-1, tfSelector.getText(), hdtNone), null, tfSelector.getText());
    });

//---------------------------------------------------------------------------

    mnuAutoImport.setSelected(appPrefs.getBoolean(PREF_KEY_AUTO_IMPORT, true));
    mnuAutoImport.setOnAction(event -> appPrefs.putBoolean(PREF_KEY_AUTO_IMPORT, mnuAutoImport.isSelected()));

//---------------------------------------------------------------------------

    primaryStage().setOnCloseRequest(event ->
    {
      shutDown(true, true, true);
      event.consume();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addSelectorTab(TabEnum tabEnum)
  {
    int ndx = selectorTabPane.getTabs().size() - (selectorTabs.size() + 1);

    selectorTabs.put(tabEnum, selectorTabPane.getTabs().get(ndx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PreviewSource determinePreviewContext()
  {
    if (primaryStage().isFocused())
    {
      switch (activeTabEnum())
      {
        case personTabEnum : return pvsPersonTab;
        case workTabEnum   : return pvsWorkTab;
        case queryTabEnum  : return pvsQueryTab;
        case treeTabEnum   : return pvsTreeTab;
        default            : return pvsOther;
      }
    }

    if ((fileManagerDlg != null) && fileManagerDlg.getStage().isShowing() && fileManagerDlg.getStage().isFocused())
      return pvsManager;

    return pvsOther;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void btnForwardClick()
  {
    if (btnForward.isDisabled() || cantSaveRecord()) return;

    viewSequence.stepForward();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void btnBackClick()
  {
    if (btnBack.isDisabled() || cantSaveRecord()) return;

    viewSequence.stepBack();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void openPreviewWindow(PreviewSource src)
  {
    if (jxBrowserDisabled) return;

    if (src != null)
      previewWindow.switchTo(src);

    if (previewWindow.getStage().isShowing())
      windows.focusStage(previewWindow.getStage());
    else
      previewWindow.showNonmodal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void runFileMgr()
  {
    if (fileManagerDlg.getStage().isShowing())
    {
      windows.focusStage(fileManagerDlg.getStage());
      return;
    }

    if (cantSaveRecord()) return;

    fileManagerDlg.showNonmodal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void runBibMgr()
  {
    if (bibManagerDlg.getStage().isShowing())
    {
      bibManagerDlg.refresh();
      windows.focusStage(bibManagerDlg.getStage());
      return;
    }

    bibManagerDlg.showNonmodal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void adjustToolBar(double anchorWidth)
  {
    if (anchorWidth == 0)
      anchorWidth = midAnchorPane.getWidth();

    Point2D p2 = midAnchorPane.localToScreen(anchorWidth, 0),
            p1 = tabTree.getGraphic().localToScreen(16, 0);

    if ((p1.getX() > 1) && (p2.getX() > 1))
    {
      if (toolBarWidth == 0)
        toolBarWidth = topToolBar.getWidth();

      if (toolBarWidth > 1)
      {
        double diff = ((p2.getX() - toolBarWidth) - p1.getX()) - 36;
        topToolBar.setMaxWidth(toolBarWidth + diff);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnTextSearchClick()
  {
    if (db.isLoaded() == false) return;

    hideFindTable();

    activeTab().findWithinDesc(tfSelector.getText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnGoToClick(boolean fromMenu)
  {
    if (selectorTabEnum() != listTabEnum)
    {
      hcbGoTo.triggerOnAction();

      if (hcbGoTo.selectedID() > 0)
        recordLookup();

      return;
    }

    TabEnum tabEnum = activeTabEnum();

    if (tabEnum == queryTabEnum)
      goToRecord(queryHyperTab().activeRecord(), false);

    if (tabEnum != treeTabEnum) return;

    if (fromMenu)
    {
      goToRecord(getTree().selectedRecord(), false);
      return;
    }

    String text = cbTreeGoTo.getEditor().getText();
    if (text.length() > 0)
      getTree().findAgain(text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void mnuFindWithinNameClick()
  {
    HDT_RecordType type = selectorType();
    String query = tfSelector.getText();
    boolean backClick = activeTabEnum() != queryTabEnum;

    lblStatus.setText("");

    if (!showSearch(true, QueryType.fromRecordType(type), QUERY_WITH_NAME_CONTAINING, null, new HyperTableCell(-1, query, hdtNone), null, query))
    {
      discardLastQuery(backClick);
      return;
    }

    // The following is done inside a runLater command because the results table gets populated inside a runLater; this runLater needs
    // to run after that one

    Platform.runLater(() ->
    {
      int num = results().size();

      if (num == 1)
        goToRecord(results().get(0).getRecord(), false);
      else if (num == 0)
      {
        discardLastQuery(backClick);
        lblStatus.setText("No results: searched " + db.getTypeName(type) + " records for \"" + abbreviate(query) + "\"");
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // This assumes that the Queries tab is currently selected
  //
  private void discardLastQuery(boolean backClick)
  {
    queryHyperTab().closeCurrentView();

    if (backClick) btnBackClick();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void shutDown(boolean save, boolean savePrefs, boolean prompt)
  {
    DialogResult result = mrRetry;

    if (db.isLoaded())
    {
      if (save)
      {
        if (cantSaveRecord() && prompt)
          if (!confirmDialog("Unable to accept most recent changes to this record; however, all other data will be saved. Continue exiting?"))
            return;

        if (appPrefs.getBoolean(PREF_KEY_CHECK_INTERNET, true))
        {
          while ((result != mrIgnore) && (checkInternetConnection() == false))
          {
            result = abortRetryIgnoreDialog("Warning: Internet connection check failed.");

            if (result == mrAbort)
              return;
          }
        }

        saveAllToDisk(false, false, false);
      }

      shuttingDown = true;
      forEachHyperTab(HyperTab::clear);

      folderTreeWatcher.stop();

      try { db.close(null); }
      catch (HDB_InternalError e)
      {
        messageDialog(e.getMessage(), mtError);
      }
    }

    closeWindows(true);

    Stage stage = primaryStage();

    if (savePrefs)
    {
      forEachHyperTab(HyperTab::getDividerPositions);
      fileManagerDlg.getDividerPositions();
      bibManagerDlg.getDividerPositions();

      boolean iconified = stage.isIconified(), fullScreen = stage.isFullScreen(), maximized;

      if (Environment.isMac())
        maximized = this.maximized;
      else
        maximized = stage.isMaximized(); // stage.maximized is never changed from true to false on Mac OS X. JDK-8087618

      if (fullScreen || maximized) iconified = false; // This has to be done due to bug JDK-8087997

      appPrefs.putDouble(PREF_KEY_WINDOW_X, stage.getX());
      appPrefs.putDouble(PREF_KEY_WINDOW_Y, stage.getY());
      appPrefs.putDouble(PREF_KEY_WINDOW_WIDTH, stage.getWidth());
      appPrefs.putDouble(PREF_KEY_WINDOW_HEIGHT, stage.getHeight());
      appPrefs.putBoolean(PREF_KEY_WINDOW_ICONIFIED, iconified);
      appPrefs.putBoolean(PREF_KEY_WINDOW_FULLSCREEN, fullScreen);
      appPrefs.putBoolean(PREF_KEY_WINDOW_MAXIMIZED, maximized);

      if (fileManagerDlg.shownAlready())
      {
        appPrefs.putDouble(PREF_KEY_FM_WINDOW_X, fileManagerDlg.getStage().getX());
        appPrefs.putDouble(PREF_KEY_FM_WINDOW_Y, fileManagerDlg.getStage().getY());
        appPrefs.putDouble(PREF_KEY_FM_WINDOW_WIDTH, fileManagerDlg.getStage().getWidth());
        appPrefs.putDouble(PREF_KEY_FM_WINDOW_HEIGHT, fileManagerDlg.getStage().getHeight());
      }

      if (bibManagerDlg.shownAlready())
      {
        appPrefs.putDouble(PREF_KEY_BM_WINDOW_X, bibManagerDlg.getStage().getX());
        appPrefs.putDouble(PREF_KEY_BM_WINDOW_Y, bibManagerDlg.getStage().getY());
        appPrefs.putDouble(PREF_KEY_BM_WINDOW_WIDTH, bibManagerDlg.getStage().getWidth());
        appPrefs.putDouble(PREF_KEY_BM_WINDOW_HEIGHT, bibManagerDlg.getStage().getHeight());
      }

      if (previewWindow.shownAlready())
      {
        appPrefs.putDouble(PREF_KEY_PREV_WINDOW_X, previewWindow.getStage().getX());
        appPrefs.putDouble(PREF_KEY_PREV_WINDOW_Y, previewWindow.getStage().getY());
        appPrefs.putDouble(PREF_KEY_PREV_WINDOW_WIDTH, previewWindow.getStage().getWidth());
        appPrefs.putDouble(PREF_KEY_PREV_WINDOW_HEIGHT, previewWindow.getStage().getHeight());
      }

      if (contentsWindow.shownAlready())
      {
        appPrefs.putDouble(PREF_KEY_CONTENTS_WINDOW_X, contentsWindow.getStage().getX());
        appPrefs.putDouble(PREF_KEY_CONTENTS_WINDOW_Y, contentsWindow.getStage().getY());
        appPrefs.putDouble(PREF_KEY_CONTENTS_WINDOW_WIDTH, contentsWindow.getStage().getWidth());
        appPrefs.putDouble(PREF_KEY_CONTENTS_WINDOW_HEIGHT, contentsWindow.getStage().getHeight());
      }

      HyperTable.saveColWidthsToPrefs();
    }

    if (jxBrowserInitialized)
      Platform.runLater(previewWindow::cleanup); // This eventually closes the application main window
    else
    {
      stage.close();

      if (JIntellitype.isJIntellitypeSupported())
        //JIntellitype.getInstance().cleanUp();   // This causes the VM to crash in Java 11
        System.exit(0);

      if (Environment.isMac())
        Platform.exit();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void closeWindows(boolean exitingApp)
  {
    if ((fileManagerDlg != null) && fileManagerDlg.getStage().isShowing())
      fileManagerDlg.getStage().close();

    if ((bibManagerDlg != null) && bibManagerDlg.getStage().isShowing())
      bibManagerDlg.getStage().close();

    if ((exitingApp == false) || (Environment.isMac() == false))
      if ((previewWindow != null) && previewWindow.getStage().isShowing())
        previewWindow.getStage().close();

    if ((contentsWindow != null) && contentsWindow.getStage().isShowing())
      contentsWindow.getStage().close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void enableControls(boolean enabled)
  {
    boolean disabled = !enabled;

    gpBottom.getChildren().forEach(node -> node.setDisable(disabled));

    apStatus.setDisable(false);

    forEachHyperTab(hyperTab -> hyperTab.enable(enabled));

    enableAllIff(enabled, mnuCloseDatabase,    mnuImportWork,        mnuImportFile,         mnuExitNoSave, mnuChangeID,        mnuNewField,
                          mnuNewCountry,       mnuImportBibFile,     mnuImportBibClipboard, mnuNewRank,    mnuNewPersonStatus, mnuSaveReloadAll,
                          mnuRevertToDiskCopy, mnuAddToQueryResults, btnFileMgr,            btnBibMgr,     btnPreviewWindow,   btnMentions,
                          btnAdvancedSearch,   btnSaveAll);
    if (disabled)
      getTree().clear();

    hideFindTable();

    updateBibImportMenus();
    updateFavorites();
    updateTopicalFolders();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateBibImportMenus()
  {
    if (db.bibLibraryIsLinked())
    {
      if (topToolBar.getItems().contains(btnBibMgr) == false)
        topToolBar.getItems().add(2, btnBibMgr);
    }
    else
      topToolBar.getItems().remove(btnBibMgr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initResultCB()
  {
    cbResultGoTo = new ComboBox<>();
    cbResultGoTo.setEditable(true);

    copyRegionLayout(cbGoTo, cbResultGoTo);

    queryHyperTab().setCB(cbResultGoTo);

    cbResultGoTo.setConverter(new StringConverter<>()
    {
      @Override public String toString(ResultsRow row)
      {
        return nullSwitch(row, "", ResultsRow::getCBText);
      }

      @Override public ResultsRow fromString(String string)
      {
        if (cbResultGoTo.getItems() == null)
          return new ResultsRow("");

        return nullSwitch(findFirst(cbResultGoTo.getItems(), row -> string.equals(row.getCBText())), new ResultsRow(string));
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateDatesTooltip(HDT_Record record)
  {
    if (record == null)
    {
      ttDates.setText("No dates to show.");
      return;
    }

    try
    {
      ttDates.setText("Created: "    + dateTimeToUserReadableStr(record.getCreationDate()) +
                      "\nModified: " + dateTimeToUserReadableStr(record.getModifiedDate()) +
                      "\nAccessed: " + dateTimeToUserReadableStr(record.getViewDate()));
    }
    catch(Exception e)
    {
      ttDates.setText("No dates to show.");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean saveAllToDisk(boolean saveRecord, boolean restartWatcher, boolean updateUI)
  {
    try
    {
      if (db.isLoaded() == false)
        return falseWithErrorMessage("No database is currently loaded.");

      if (saveRecord && cantSaveRecord()) return false;

      db.prefs.putInt(PREF_KEY_PERSON_ID     , personHyperTab  ().getActiveID());
      db.prefs.putInt(PREF_KEY_INSTITUTION_ID, instHyperTab    ().getActiveID());
      db.prefs.putInt(PREF_KEY_DEBATE_ID     , debateHyperTab  ().getActiveID());
      db.prefs.putInt(PREF_KEY_POSITION_ID   , positionHyperTab().getActiveID());
      db.prefs.putInt(PREF_KEY_ARGUMENT_ID   , argumentHyperTab().getActiveID());
      db.prefs.putInt(PREF_KEY_WORK_ID       , workHyperTab    ().getActiveID());
      db.prefs.putInt(PREF_KEY_TERM_ID       , termHyperTab    ().getActiveID());
      db.prefs.putInt(PREF_KEY_FILE_ID       , fileHyperTab    ().getActiveID());
      db.prefs.putInt(PREF_KEY_NOTE_ID       , noteHyperTab    ().getActiveID());

      db.prefs.put(PREF_KEY_RECORD_TYPE, db.getTypeTagStr(activeType() == hdtNone ? hdtPerson : activeType()));

      boolean watcherWasRunning = folderTreeWatcher.stop();

      db.saveAllToDisk(favorites);

      if (restartWatcher && watcherWasRunning)
        folderTreeWatcher.createNewWatcherAndStart();

      if (updateUI) update();

      lblStatus.setText("Database last saved to XML files: " + timeToUserReadableStr(LocalDateTime.now()));

      return true;
    }
    catch (Throwable e)
    {
      showStackTrace(e);
      return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuSaveReloadAllClick()
  {
    if (saveAllToDisk(true, false, false) == false) return;

    if (loadDataFromDisk())
    {
      viewSequence.refreshAll();

      forEachHyperTab(HyperTab::refresh);

      if (activeTabEnum() == queryTabEnum)
        activeTab().clear();

      update();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void openDB(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath))
    {
      FileChooser fileChooser = new FileChooser();

      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter(appTitle + " files (*.hdb)", "*.hdb"));

      File dir = new File(appPrefs.get(PREF_KEY_SOURCE_PATH, System.getProperty("user.dir")));

      if (dir.exists() == false)
        dir = new File(System.getProperty("user.dir"));

      fileChooser.setInitialDirectory(dir);

      filePath = windows.showOpenDialog(fileChooser, primaryStage());
    }

    if (FilePath.isEmpty(filePath)) return;

    if (db.isLoaded())
    {
      DialogResult result = yesNoCancelDialog("Save data to XML files?");

      if (result == mrCancel) return;

      if (result == mrYes)
      {
        if (cantSaveRecord()) return;
        saveAllToDisk(false, false, false);
      }

      closeWindows(false);
      clearAllTabsAndViews();
    }

    appPrefs.put(PREF_KEY_SOURCE_FILENAME, filePath.getNameOnly().toString());
    appPrefs.put(PREF_KEY_SOURCE_PATH, filePath.getDirOnly().toString());

    loadDB();
  }

  //---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuNewDatabaseClick()
  {
    if (SystemUtils.IS_OS_WINDOWS == false)
      messageDialog("Select an empty folder in which to create the new database.", mtInformation);

    DirectoryChooser dirChooser = new DirectoryChooser();

    File file = new File(appPrefs.get(PREF_KEY_SOURCE_PATH, ""));

    dirChooser.setTitle("Select an empty folder in which to create database");

    if (file.exists() && file.isDirectory())
      dirChooser.setInitialDirectory(file);
    else
      dirChooser.setInitialDirectory(new File(System.getProperty("user.dir")));

    FilePath rootPath = ui.windows.showDirDialog(dirChooser, primaryStage());
    if (FilePath.isEmpty(rootPath)) return;

    String[] list = rootPath.toFile().list();
    if (list == null)
    {
      messageDialog("Selected item is not a folder.", mtError);
      return;
    }

    if (list.length != 0)
    {
      messageDialog("The selected folder is not empty.", mtError);
      return;
    }

    if (cantSaveRecord()) return;

    if (db.isLoaded())
    {
      DialogResult result = yesNoCancelDialog("Save data to XML files?");

      if (result == mrCancel) return;

      if (result == mrYes)
        saveAllToDisk(false, false, false);

      NewDatabaseDlgCtrlr dlg = NewDatabaseDlgCtrlr.build(rootPath.toString());

      if (dlg.showModal() == false)
        return;

      closeWindows(false);

      try { db.newDB(rootPath, dlg.getChoices(), dlg.getFolders()); }
      catch (HDB_InternalError e)
      {
        messageDialog("Unable to create new database: " + e.getMessage(), mtError);
        shutDown(false, true, false); // An error in db.close is unrecoverable.
        return;
      }

      personHyperTab().curPicture = null;
      clearAllTabsAndViews();

      saveAllToDisk(false, false, false);
    }
    else
    {
      FilePath srcFilePath = null;

      try (ZipInputStream zis = new ZipInputStream(App.class.getResourceAsStream("resources/blank_db.zip")))
      {
        ZipEntry entry;

        while ((entry = zis.getNextEntry()) != null)
        {
          FilePath filePath = rootPath.resolve(new FilePath(entry.getName()));

          if (entry.isDirectory())
          {
            filePath.toFile().mkdirs();
          }
          else
          {
            int size;
            byte[] buffer = new byte[2048];

            if (filePath.getExtensionOnly().equals("hdb"))
              srcFilePath = filePath;

            try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(filePath.toFile()), buffer.length))
            {
              while ((size = zis.read(buffer, 0, buffer.length)) != -1)
                bos.write(buffer, 0, size);

              bos.flush();
            }
          }
        }
      }
      catch (IOException e)
      {
        messageDialog("Unable to create new database: " + e.getMessage(), mtError);
        return;
      }

      appPrefs.put(PREF_KEY_SOURCE_FILENAME, srcFilePath.getNameOnly().toString());
      appPrefs.put(PREF_KEY_SOURCE_PATH, srcFilePath.getDirOnly().toString());
    }

    loadDB();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML public void btnSaveClick()
  {
    if (btnSave.getText().equals(TREE_SELECT_BTN_CAPTION))
      treeSelect();
    else if (!cantSaveRecord())
      update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnCreateClick()
  {
    if (cantSaveRecord()) return;

    HDT_RecordType type = selectorType();
    String name = hcbGoTo.selectedID() == -1 ? cbGoTo.getEditor().getText() : "";

    HDT_Record record = db.createNewBlankRecord(type);
    if (name.length() > 0)
    {
      if (type == hdtPerson)
      {
        HDT_Person person = (HDT_Person)record;

        StringBuilder searchKey = new StringBuilder();
        PersonName personName = new PersonName(name);
        person.setName(personName);
        HDT_Person.makeSearchKey(personName, person, searchKey);

        try { person.setSearchKey(searchKey.toString()); } catch (SearchKeyException e) { noOp(); }
      }
      else if (type == hdtWork)
        record.setName(HDT_Work.fixCase(name.trim()));
      else
        record.setName(titleCase(name.trim()));
    }

    if (type == hdtTerm)
    {
      HDT_Concept concept = db.createNewBlankRecord(hdtConcept);
      HDT_Term.class.cast(record).concepts.add(concept);
      concept.glossary.set(db.glossaries.getByID(1));
    }

    goToRecord(record, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean deleteCurrentRecord(boolean confirm)
  {
    HDT_Record record = activeRecord();

    if (record == null) return false;

    HDT_RecordType type = record.getType();

    switch (type)
    {
      case hdtGlossary :

        if (activeTabEnum() != treeTabEnum)
          return falseWithErrorMessage("Glossary records can only be deleted from the tree tab.");

        HDT_Glossary glossary = (HDT_Glossary) record;

        if (glossary.concepts.isEmpty() == false)
          return falseWithErrorMessage("A glossary record can only be deleted if it does not contain any terms.");

        break;

      case hdtNone : case hdtConcept : case hdtFolder : case hdtWorkFile : case hdtHub :

        return falseWithErrorMessage("Records of that type cannot be deleted by this method.");

      default :
        break;
    }

    if (confirm)
    {
      String msg;

      if (type == hdtTerm)
        msg = "Are you sure you want to delete this record and all associated concepts?";
      else
        msg = "Are you sure you want to delete this record?";

      String name = record.getCBText();
      if (ultraTrim(name).isEmpty())
        name = activeTab().recordName();

      if (confirmDialog("Type: " + db.getTypeName(type) + "\n" +
                        "Name: " + name + "\n" +
                        "ID: " + record.getID() + "\n\n" + msg) == false) return false;
    }

    db.deleteRecord(type, record.getID());

    viewSequence.activateCurrentView();
    fileManagerDlg.refresh();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuCloseClick()
  {
    if (db.isLoaded())
    {
      DialogResult result = yesNoCancelDialog("Save data to XML files before closing?");

      if (result == mrCancel) return;

      if (result == mrYes)
      {
        if (cantSaveRecord()) return;
        saveAllToDisk(false, false, false);
      }
    }

    clearAllTabsAndViews();
    lblStatus.setText("");

    treeSelector.reset();
    closeWindows(false);

    try { db.close(null); }
    catch (HDB_InternalError e)
    {
      messageDialog(e.getMessage(), mtError);
      shutDown(false, true, false); // An error in db.close is unrecoverable.
      return;
    }

    enableControls(false);

    updateBottomPanel(true);
    tfRecord.setText("");
    tfID.setText("");
    hcbGoTo.clear();

    primaryStage().setTitle(appTitle);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateTopicalFolders()
  {
    while (mnuFolders.getItems().size() > 10) // Clear the topical folder items that currently exist
      mnuFolders.getItems().remove(10);

    if (db.isLoaded() == false)
    {
      mnuFolders.setDisable(true);
      return;
    }

    FilePath topicalPath = db.topicalPath();
    mnuFolders.setDisable(false);

    try (DirectoryStream<Path> stream = Files.newDirectoryStream(topicalPath.toPath(), "**"))
    {
      stream.forEach(entry ->
      {
        FilePath entryFilePath = new FilePath(entry);
        if (entryFilePath.isDirectory() == false) return;

        FilePath relFilePath = topicalPath.relativize(entryFilePath);

        if (FilePath.isEmpty(relFilePath) == false)
        {
          MenuItem item = new MenuItem();
          item.setText(relFilePath.toString());
          item.setOnAction(event -> launchFile(entryFilePath));
          mnuFolders.getItems().add(item);
        }
      });
    }
    catch (DirectoryIteratorException | IOException ex) { noOp(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuChangeIDClick()
  {
    if (db.isLoaded() == false)
    {
      messageDialog("No database is currently loaded.", mtError);
      return;
    }

    if (cantSaveRecord()) return;

    ChangeIDDlgCtrlr ctrlr = ChangeIDDlgCtrlr.build();

    if (ctrlr.showModal())
    {
      HDT_RecordType changedType = ctrlr.hcbRecord.selectedType();
      int oldID = parseInt(ctrlr.tfOldID.getText(), -100),
          newID = parseInt(ctrlr.tfNewID.getText(), -1);

      db.rebuildMentions();

      favorites.changeRecordID(changedType, oldID, newID);

      update();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuNewFieldClick       () { mnuNewCategoryClick(hdtField       ); }
  @FXML private void mnuNewRankClick        () { mnuNewCategoryClick(hdtRank        ); }
  @FXML private void mnuNewCountryClick     () { mnuNewCategoryClick(hdtCountry     ); }
  @FXML private void mnuNewPersonStatusClick() { mnuNewCategoryClick(hdtPersonStatus); }

  private void mnuNewCategoryClick(HDT_RecordType type)
  {
    if (db.isLoaded() == false)
    {
      messageDialog("No database is currently loaded.", mtError);
      return;
    }

    if (cantSaveRecord()) return;

    NewCategoryDlgCtrlr ctrlr = NewCategoryDlgCtrlr.build(type);

    if (ctrlr.showModal() == false) return;

    int id = parseInt(ctrlr.tfNewID.getText(), -1);
    type = ctrlr.hcbRecordType.selectedType();

    HDT_RecordState recordState = new HDT_RecordState(type, id, ctrlr.tfNewKey.getText(), "", "", "");

    try { db.createNewRecordFromState(recordState, true); } catch (Exception e) { noOp(); }

    db.records(type).getByID(id).setName(ctrlr.tfNewName.getText());

    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuFolderClick(Event event)
  {
    MenuItem item = (MenuItem)event.getSource();
    int code = parseInt(item.getId(), 0);
    boolean clipboard = (code % 10) == 1;
    FilePath filePath = null;

    code = code / 10;

    switch (code)
    {
      case 1 : filePath = db.papersPath   (); break;
      case 2 : filePath = db.booksPath    (); break;
      case 3 : filePath = db.unenteredPath(); break;
      case 4 : filePath = db.topicalPath  (); break;
      case 5 : filePath = db.picturesPath (); break;
      case 6 : filePath = db.miscFilesPath(); break;
      case 7 : filePath = db.getRootPath  (); break;
      case 8 : filePath = db.resultsPath  (); break;
    }

    if (FilePath.isEmpty(filePath)) return;

    if (clipboard)
      copyToClipboard(filePath.toString());
    else
      launchFile(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public class FavMenuItem extends MenuItem
  {
    public FavMenuItem(HDT_Record record)
    {
      super(db.getTypeName(record.getType()) + ": " + record.getCBText());
      isQuery = false;
      favRecord = new HyperTableCell(record.getID(), record.getCBText(), record.getType());
      query = null;
      setOnAction(event -> goToRecord(record, true));
    }

    public FavMenuItem(QueryFavorite query)
    {
      super("Query: " + query.name);
      isQuery = true;
      this.query = query;
      favRecord = null;
      setOnAction(event -> showSearch(query.autoexec, null, -1, query, null, null, query.name));
    }

    final public boolean isQuery;
    final public QueryFavorite query;
    public HyperTableCell favRecord;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateFavorites()
  {
    mnuToggleFavorite.setText("Add to favorites...");

    if (db.isLoaded() == false)
    {
      favorites.clear();
      mnuFavorites.setDisable(true);
      return;
    }

    mnuFavorites.setDisable(false);

    if ((activeTabEnum() != treeTabEnum) && (activeTabEnum() != queryTabEnum) && (viewRecord() != null))
    {
      mnuToggleFavorite.setDisable(false);

      if (favorites.indexOfRecord(viewRecord()) > -1)
        mnuToggleFavorite.setText("Remove from favorites...");
    }
    else
      mnuToggleFavorite.setDisable(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuToggleFavoriteClick()
  {
    if ((activeTabEnum() == treeTabEnum) || (activeTabEnum() == queryTabEnum)) return;
    if (cantSaveRecord()) return;

    HDT_Record record = viewRecord();
    if (record == null) return;

    int ndx = favorites.indexOfRecord(record);

    if (ndx > -1)
      mnuFavorites.getItems().remove(ndx);
    else
      mnuFavorites.getItems().add(new FavMenuItem(record));

    updateFavorites();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void initPositionContextMenu(HyperTable ht)
  {
    ht.addContextMenuItem("Launch work", HDT_Position.class,
      pos -> pos.getLaunchableWork() != null,
      pos -> pos.getLaunchableWork().work.launch(-1));

    ht.addContextMenuItem("Go to work record", HDT_Position.class,
      pos -> pos.getWork() != null,
      pos -> goToRecord(nullSwitch(pos.getLaunchableWork(), pos.getWork()).work, true));

    ht.addContextMenuItem("Go to person record", HDT_Position.class,
      pos -> pos.getWorkWithAuthor() != null,
      pos -> goToRecord(pos.getWorkWithAuthor().author, true));

    ht.addContextMenuItem("Go to argument record", HDT_Position.class,
      pos -> pos.arguments.size() > 0,
      pos -> goToRecord(nullSwitch(pos.getLaunchableWork(), nullSwitch(pos.getWork(), pos.getArgument())).argument, true));

    ht.addContextMenuItem("Go to position record", HDT_Position.class,
      pos -> goToRecord(pos, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateProgress(String task, double amount)
  {
    if (amount < 0)
    {
      progressBar.setVisible(false);
      lblProgress.setVisible(false);
      return;
    }

    if (progressBar.isVisible() == false)
      progressBar.setVisible(true);

    if (task.length() > 0)
    {
      if (lblProgress.isVisible() == false)
        lblProgress.setVisible(true);

      if (lblProgress.getText().equals(task) == false)
        lblProgress.setText(task);
    }

    progressBar.setProgress(amount);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void searchForMentions(HDT_Record record, boolean descOnly)
  {
    if (record == null) return;

    HDT_RecordType type = record.getType();
    boolean backClick = activeTabEnum() != queryTabEnum;

    lblStatus.setText("");

    if (showSearch(true, qtAllRecords, descOnly ? QUERY_LINKING_TO_RECORD : QUERY_MATCHING_RECORD, null,
                   new HyperTableCell(-1, "", type), new HyperTableCell(record, ""), "Mentions: " + record.listName()))
    {
      if ((curQV.resultsBackingList.size() > 0) && (curQV.resultsBackingList.equals(List.of(record)) == false))
        return;

      lblStatus.setText("No mentioners: " + db.getTypeName(type).toLowerCase() + " \"" + abbreviate(record.listName()) + "\"");
    }

    discardLastQuery(backClick);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuAddToQueryResultsClick()
  {
    if (db.isLoaded() == false)
    {
      messageDialog("No database is currently loaded.", mtError);
      return;
    }

    HDT_Record record = null;

    if (activeTabEnum() == termTabEnum)
    {
      HDT_Term term = (HDT_Term) activeRecord();
      record = viewRecord();

      if (record == null)
        record = term;
      else
      {
        DialogResult result = new PopupDialog("Which record?")

          .addButton("Term", mrYes)
          .addButton("Concept", mrNo)

          .showModal();

        if (result == mrYes)
          record = term;
      }
    }
    else
      record = activeRecord();

    if (record == null)
    {
      messageDialog("No record is currently selected.", mtError);
      return;
    }

    for (ResultsRow row : curQV.resultsBackingList)
      if (row.getRecord() == record) return;

    curQV.addRecord(record, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private boolean mnuRevertToDiskCopyClick()
  {
    if (db.isLoaded() == false)
      return falseWithErrorMessage("No database is currently loaded.");

    HDT_Record record = activeRecord();

    if (record == null)
      return falseWithErrorMessage("No record is currently selected.");

    if (record.hasStoredState() == false)
      return falseWithErrorMessage("Unable to revert: the record may not have been previously saved to disk.");

    if (confirmDialog("Are you sure you want to revert this record to the last version saved to disk?") == false) return false;

    HDT_Record viewRecord = viewRecord();

    if (revertToDiskCopy(record) && (viewRecord != null) && (viewRecord != record))
      if ((activeTabEnum() != treeTabEnum) && (activeTabEnum() != queryTabEnum))
        revertToDiskCopy(viewRecord);

    update();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean revertToDiskCopy(HDT_Record record)
  {
    boolean success = true;
    String recordStr = db.getTypeName(record.getType()) + " \"" + record.getCBText() + "\"";

    HDT_Hub hub = record.isUnitable() ? HDT_RecordWithConnector.class.cast(record).getHub() : null;
    HDT_RecordState backupState = record.getRecordStateBackup(),
                    hubState = hub == null ? null : hub.getRecordStateBackup();

    try
    {
      if (hub != null)
        hub.bringStoredCopyOnline(false);

      record.bringStoredCopyOnline(false);
    }
    catch (RelationCycleException e)
    {
      messageDialog("Unable to revert " + recordStr + ": Records would be organized in a cycle as a result.", mtError);
      success = false;
    }
    catch (HubChangedException | SearchKeyException e)
    {
      messageDialog("Unable to revert " + recordStr + ": " + e.getMessage(), mtError);
      success = false;
    }

    if (success) return true;

    try
    {
      if (hub != null)
        hub.restoreTo(hubState, false);

      record.restoreTo(backupState, false);
    }
    catch (RelationCycleException | SearchKeyException | HubChangedException e) { noOp(); }
    catch (HDB_InternalError e)
    {
      messageDialog("Unable to restore " + recordStr + " to pre-reverting state: " + e.getMessage(), mtError);
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void startEmpty()
  {
    clearAllTabsAndViews();
    enableControls(db.isLoaded());
    showWelcomeWindow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void loadDB()
  {
    if (loadDataFromDisk() == false)
    {
      if (db.isLoaded() == false)
        clearAllTabsAndViews();

      enableControls(db.isLoaded());
      return;
    }

    setTabView(new HyperView<>(personTabEnum  , db.persons     .getByID(db.prefs.getInt(PREF_KEY_PERSON_ID     , -1))));
    setTabView(new HyperView<>(instTabEnum    , db.institutions.getByID(db.prefs.getInt(PREF_KEY_INSTITUTION_ID, -1))));
    setTabView(new HyperView<>(debateTabEnum  , db.debates     .getByID(db.prefs.getInt(PREF_KEY_DEBATE_ID     , -1))));
    setTabView(new HyperView<>(positionTabEnum, db.positions   .getByID(db.prefs.getInt(PREF_KEY_POSITION_ID   , -1))));
    setTabView(new HyperView<>(argumentTabEnum, db.arguments   .getByID(db.prefs.getInt(PREF_KEY_ARGUMENT_ID   , -1))));
    setTabView(new HyperView<>(workTabEnum    , db.works       .getByID(db.prefs.getInt(PREF_KEY_WORK_ID       , -1))));

    HDT_Concept concept = nullSwitch(db.terms.getByID(db.prefs.getInt(PREF_KEY_TERM_ID, -1)), null, term -> term.concepts.get(0));

    setTabView(new HyperView<>(termTabEnum,     concept));

    setTabView(new HyperView<>(fileTabEnum    , db.miscFiles   .getByID(db.prefs.getInt(PREF_KEY_FILE_ID       , -1))));
    setTabView(new HyperView<>(noteTabEnum    , db.notes       .getByID(db.prefs.getInt(PREF_KEY_NOTE_ID       , -1))));
    setTabView(new HyperView<>(queryTabEnum   , null));
    setTabView(new HyperView<>(treeTabEnum    , null));

    enableControls(db.isLoaded());

    viewSequence.init(getTabEnumByRecordType(db.parseTypeTagStr(db.prefs.get(PREF_KEY_RECORD_TYPE, ""))));

    if (db.bibLibraryIsLinked() || db.prefs.getBoolean(PREF_KEY_NOTIFY_USER_NOT_LINKED, true) == false)
      return;

    DialogResult result = new PopupDialog("This database is not currently integrated with a reference manager account (like Mendeley or Zotero). Add one now?")

      .addButton("Yes", mrYes)
      .addButton("Remind me later" , mrNo)
      .addButton("Do not ask again for this database", mrIgnore)

      .showModal();

    if (result == mrNo)
      db.prefs.putBoolean(PREF_KEY_NOTIFY_USER_NOT_LINKED, true);

    else if (result == mrIgnore)
      db.prefs.putBoolean(PREF_KEY_NOTIFY_USER_NOT_LINKED, false);

    if (result == mrYes)
      SettingsDlgCtrlr.build(SettingsPage.BibMgr).showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean loadDataFromDisk()
  {
    if (SystemUtils.IS_OS_MAC)
      Platform.runLater(() -> adjustToolBar(0));

    FilePath hdbPath = null;
    boolean hdbExists = false;
    String srcName = appPrefs.get(PREF_KEY_SOURCE_FILENAME, "");
    if (srcName.isBlank() == false)
    {
      String srcPath = appPrefs.get(PREF_KEY_SOURCE_PATH, "");
      if (srcPath.isBlank() == false)
      {
        hdbPath = new FilePath(srcPath).resolve(srcName);
        if (hdbPath.exists())
          hdbExists = true;
      }
    }

    if (hdbExists == false)
    {
      if (hdbPath == null)
        return false;

      return falseWithErrorMessage("Unable to load database. The file does not exist: " + hdbPath.toString());
    }

    if (internetNotCheckedYet && appPrefs.getBoolean(PREF_KEY_CHECK_INTERNET, true))
    {
      internetNotCheckedYet = false;

      DialogResult result = mrRetry;
      while ((result != mrIgnore) && (checkInternetConnection() == false))
      {
        result = abortRetryIgnoreDialog("Warning: Internet connection check failed.");

        if (result == mrAbort)
          return false;
      }
    }

    String otherCompName = db.getLockOwner();
    if (otherCompName != null)
    {
      if (LockedDlgCtrlr.build(otherCompName).showModal() == false)
        return false;

      if (db.getLockOwner() != null)
        return false;
    }

    boolean success = false;

    try { success = db.loadAllFromDisk(favorites); }
    catch (HDB_InternalError e)
    {
      messageDialog("Unable to load database. Reason: " + e.getMessage(), mtError);
      shutDown(false, true, false); // An error in db.close is unrecoverable.
      return false;
    }

    if (success)
    {
      List<String> mruList = getHdbMRUs();
      mruList.add(0, hdbPath.toString());
      saveHdbMRUs(mruList);

      success = folderTreeWatcher.createNewWatcherAndStart();
    }

    if (success)
    {
      lblStatus.setText("");
      updateTopicalFolders();
      queryHyperTab().clear();

      gpBottom.setDisable(false);
      queryHyperTab().enable(true);
      treeHyperTab().enable(true);

      getTree().expandMainBranches();
      fileManagerDlg.folderTree.expandMainBranches();

      primaryStage().setTitle(appTitle + " - " + db.getRootPath(appPrefs.get(PREF_KEY_SOURCE_FILENAME, "")));
    }
    else
      mnuCloseClick();

    return success;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveHdbMRUs(List<String> mruList)
  {
    mruList.removeIf(String::isBlank);
    removeDupsInStrList(mruList);

    for (int ndx = 0; ndx < HDB_MRU_SIZE; ndx++)
      appPrefs.put(PREF_KEY_HDB_MRU + String.valueOf(ndx + 1), mruList.size() > ndx ? mruList.get(ndx) : "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<String> getHdbMRUs()
  {
    List<String> mruList = new ArrayList<>();

    for (int ndx = 0; ndx < HDB_MRU_SIZE; ndx++)
      mruList.add(appPrefs.get(PREF_KEY_HDB_MRU + String.valueOf(ndx + 1), ""));

    mruList.removeIf(String::isBlank);
    return mruList;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean cantSaveRecord()
  {
    if ((db.isLoaded() == false) || (activeTabEnum() == queryTabEnum) || (activeTabEnum() == treeTabEnum) || (activeRecord() == null) || shuttingDown)
      return false;

    CommitableWrapper.commitWrapper(primaryStage().getScene().getFocusOwner());

    return activeTab().saveToRecord() == false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToTreeRecord(HDT_Record record)
  {
    if ((db.isLoaded() == false) || shuttingDown) return;

    if (windows.getOutermostStage() != primaryStage())
      windows.focusStage(primaryStage());

    if (cantSaveRecord())
    {
      treeSelector.reset();
      return;
    }

    viewSequence.forwardToNewSlotAndView(new HyperView<>(treeTabEnum, record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TabEnum activeTabEnum()                    { return viewSequence.isEmpty() ? personTabEnum : viewSequence.curTabEnum(); }
  public HyperTab<? extends HDT_Record,
                  ? extends HDT_Record> activeTab() { return viewSequence.isEmpty() ? null : viewSequence.curHyperTab(); }
  public HDT_RecordType activeType()                { return viewSequence.isEmpty() ? hdtPerson : viewSequence.curHyperView().getTabRecordType(); }
  public HDT_Record activeRecord()                  { return viewSequence.isEmpty() ? null : activeTab().activeRecord(); }
  public HDT_Record viewRecord()                    { return viewSequence.isEmpty() ? null : activeTab().viewRecord(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToFileInManager(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    runFileMgr();
    fileManagerDlg.goToFilePath(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToWorkInBibManager(HDT_Work work)
  {
    runBibMgr();
    bibManagerDlg.goToWork(work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_RecordWithConnector getSpokeToGoTo(StrongLink link)
  {
    if (link == null) return null;

    HDT_RecordWithConnector spoke = link.getConcept();
    if (spoke == null)      spoke = link.getDebate();
    if (spoke == null)      spoke = link.getPosition();
    if (spoke == null)      spoke = link.getNote();
    if (spoke == null)      spoke = link.getLabel();

    return spoke;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToRecord(HDT_Record record, boolean save)
  {
    if ((record == null) || (db.isLoaded() == false) || shuttingDown) return;

    treeSelector.reset();
    HDT_Investigation inv = null;
    HDT_WorkFile workFile = null;

    switch (record.getType())
    {
      case hdtHub :

        record = getSpokeToGoTo(((HDT_Hub)record).getLink());
        if (record == null) return;
        break;

      case hdtGlossary :

        goToTreeRecord(record);
        return;

      case hdtWorkLabel :

        StrongLink link = ((HDT_WorkLabel)record).getLink();

        if (link == null)
        {
          goToTreeRecord(record);
          return;
        }

        record = getSpokeToGoTo(link);
        break;

      case hdtFolder :

        goToFileInManager(((HDT_Folder)record).filePath());
        return;

      case hdtInvestigation :

        inv = (HDT_Investigation)record;
        record = inv.person.get();
        break;

      case hdtWorkFile :

        workFile = (HDT_WorkFile)record;
        if (workFile.works.size() > 0)
          record = workFile.works.get(0);
        else
        {
          goToFileInManager(workFile.filePath());
          return;
        }

        break;

      case hdtTerm :

        record = ((HDT_Term)record).concepts.get(0);
        break;

      default : break;
    }

    if (getTabEnumByRecordType(record.getType()) == personTabEnum)
      if (record.getType() != hdtPerson) return;

    if (windows.getOutermostStage() != primaryStage())
      windows.focusStage(primaryStage());

    if (save && cantSaveRecord()) return;

    viewSequence.forwardToNewSlotAndView(new HyperView<>(record));

    if (inv != null)
      personHyperTab().showInvestigation(inv.getID());
    else if (workFile != null)
      workHyperTab().showWorkFile(workFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void update()
  {
    updateTopicalFolders();

    if (db.isLoaded() == false)
    {
      getTree().clear();
      return;
    }

    TabEnum tabEnum = activeTabEnum();
    HyperTab<? extends HDT_Record, ? extends HDT_Record> tab = activeTab();
    HDT_Record record = activeRecord();

    switch (tabEnum)
    {
      case queryTabEnum : case treeTabEnum :
        tab.update();
        updateBottomPanel(true);
        return;

      default :
        break;
    }

    int count = tab.getRecordCount();

    treeSelector.reset();

    if (count > 0)
    {
      if (HDT_Record.isEmpty(record))
      {
        int ndx = tab.getView().getTabRecordKeyNdx();

        if (ndx >= count)
          ndx = count - 1;

        if (ndx < 0)
          ndx = 0;

        record = db.records(activeType()).getByKeyNdx(ndx);

        if (tabEnum == termTabEnum)
          viewSequence.updateCurrentView(new HyperView<>(termTabEnum, ((HDT_Term)record).concepts.get(0)));
        else
          viewSequence.updateCurrentView(new HyperView<>(tabEnum, record));
      }
    }
    else
      viewSequence.updateCurrentView(new HyperView<>(tabEnum, null));

    updateBottomPanel(true);
    tab.clear();

    if (HDT_Record.isEmpty(record))
    {
      tab.enable(false);
      return;
    }

    tab.enable(true);
    if (tab.update())
      record.viewNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TabEnum selectorTabEnum()
  {
    return selectorTabs.inverse().get(selectorTabPane.getSelectionModel().getSelectedItem());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_RecordType selectorType()
  {
    TabEnum tabEnum = selectorTabEnum();

    return (tabEnum == listTabEnum) || (tabEnum == omniTabEnum) ? activeType() : getRecordTypeByTabEnum(tabEnum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateSelectorTab(boolean setFocus)
  {
    TabEnum selectorTabEnum = selectorTabEnum(), activeTabEnum = activeTabEnum();
    HyperTab<? extends HDT_Record, ? extends HDT_Record> hyperTab = nullSwitch(getHyperTab(selectorTabEnum), activeTab());
    tfSelector = null;

    int count = hyperTab == null ? 0 : hyperTab.getRecordCount();

    mnuRecordSelect.setVisible(true);

    setAllVisible(false, mnuFindNextInName, mnuFindNextAll, mnuFindPreviousAll, mnuFindWithinName, mnuFindWithinAnyField, mnuFindPreviousInName);

    switch (selectorTabEnum)
    {
      case listTabEnum :

        if (activeTabEnum == queryTabEnum)
        {
          if (cbResultGoTo == null) initResultCB();

          apListGoTo.getChildren().setAll(cbResultGoTo);
          tfSelector = cbResultGoTo.getEditor();
        }

        if (activeTabEnum == treeTabEnum)
        {
          setAllVisible(true, mnuFindNextAll, mnuFindPreviousAll, mnuFindPreviousInName, mnuFindNextInName);

          copyRegionLayout(cbGoTo, cbTreeGoTo);
          apListGoTo.getChildren().setAll(cbTreeGoTo);
          tfSelector = cbTreeGoTo.getEditor();
        }

        btnCreateNew.setDisable(true);

        break;

      case omniTabEnum :

        mnuRecordSelect.setVisible(false);
        setAllVisible(true, mnuFindWithinAnyField, mnuFindWithinName);

        tfSelector = tfOmniGoTo;

        btnCreateNew.setDisable((activeTabEnum == queryTabEnum) || (activeTabEnum == treeTabEnum));

        break;

      default :

        setAllVisible(true, mnuFindWithinAnyField, mnuFindWithinName);

        tfSelector = cbGoTo.getEditor();
        hcbGoTo.clear();
        RecordByTypePopulator.class.cast(hcbGoTo.getPopulator()).setRecordType(Populator.dummyRow, selectorType());
        if (cbGoTo.isEditable() == false) cbGoTo.setEditable(true);
        btnCreateNew.setDisable(false);

        if (count > 0)
        {
          HDT_Record record = nullSwitch(hyperTab, null, HyperTab::activeRecord);  // Save to variable to avoid Maven compile errors
          hcbGoTo.addAndSelectEntryOrBlank(record, HDT_Record::listName);
        }

        break;
    }

    hideFindTable();

    if (setFocus && (tfSelector != null)) Platform.runLater(() ->
    {
      tfSelector.requestFocus();
      tfSelector.selectAll();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void attachOrphansToRoots()
  {
    Set<HDT_Position> orphans = db.getOrphans(rtParentPosOfPos, HDT_Position.class);

    db.getOrphans(rtDebateOfPosition, HDT_Position.class).forEach(position ->
    {
      if (orphans.contains(position))
        position.debates.add(db.debates.getByID(1));
    });

    db.getOrphans(rtParentDebateOfDebate, HDT_Debate     .class).forEach(debate -> debate.largerDebates.add(db.debates     .getByID(1)));
    db.getOrphans(rtParentNoteOfNote    , HDT_Note       .class).forEach(note   -> note  .parentNotes  .add(db.notes       .getByID(1)));
    db.getOrphans(rtParentLabelOfLabel  , HDT_WorkLabel  .class).forEach(label  -> label .parentLabels .add(db.workLabels  .getByID(1)));
    db.getOrphans(rtParentGroupOfGroup  , HDT_PersonGroup.class).forEach(group  -> group .parentGroups .add(db.personGroups.getByID(1)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateBottomPanel(boolean refreshDropDown)
  {
    ttDates.setText("No dates to show.");
    if (db.isLoaded() == false) return;

    HyperTab<? extends HDT_Record, ? extends HDT_Record> curTab = activeTab();
    if (curTab == null) return;

    int count = curTab.getRecordCount(), ndx = curTab.getRecordNdx();
    HDT_Record activeRec = activeRecord();
    TabEnum activeTabEnum = activeTabEnum();

    attachOrphansToRoots();

    btnTextSearch.setDisable(false);

  //---------------------------------------------------------------------------
  // Query-specific stuff
  //---------------------------------------------------------------------------

    if (activeTabEnum == queryTabEnum)
    {
      btnSave.setText("Accept Edits");
      btnRevert.setText("Revert");
      btnDelete.setDisable(activeRec == null);

      disableAll(btnSave, btnRevert, btnIncrement, btnDecrement);
    }

  //---------------------------------------------------------------------------
  // Tree-specific stuff
  //---------------------------------------------------------------------------

    else if (activeTabEnum == treeTabEnum)
    {
      if (treeSelector.getBase() == null)
      {
        btnSave.setDisable(true);
        btnSave.setText("Accept Edits");
      }
      else
      {
        btnSave.setDisable(false);
        btnSave.setText(TREE_SELECT_BTN_CAPTION);
      }

      btnRevert.setDisable(false);
      btnRevert.setText("Refresh");

      disableAllIff(count < 1, btnIncrement, btnDecrement);

      btnDelete.setDisable(activeRec == null);
    }

  //---------------------------------------------------------------------------
  // Single-record-tab-specific stuff
  //---------------------------------------------------------------------------

    else
    {
      btnTextSearch.setDisable(EnumSet.of(hdtArgument, hdtDebate,   hdtMiscFile, hdtNote,
                                          hdtPerson,   hdtPosition, hdtTerm,     hdtWork).contains(activeType()) == false);

      btnSave.setText("Accept Edits");

      disableAllIff(activeRec == null, btnDelete, btnSave, btnRevert);
      btnRevert.setText("Revert");

//      btnRevert.setDisable(changed == false);

      btnDecrement.setDisable((count == 0) || (ndx == 0));
      btnIncrement.setDisable((count == 0) || (ndx == (count - 1)));
    }

  //---------------------------------------------------------------------------
  // General stuff
  //---------------------------------------------------------------------------

    tfRecord.setText(count < 1 ? "" : ((ndx + 1) + " of " + count));
    tfID.setText(activeRec == null ? "" : String.valueOf(activeRec.getID()));

    updateDatesTooltip(activeRec);
    updateFavorites();

    if (refreshDropDown)
      updateSelectorTab(false);
    else
      hideFindTable();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite fav, HyperTableCell op1, HyperTableCell op2, String caption)
  {
    if (cantSaveRecord()) return false;

    viewSequence.forwardToNewSlotAndView(new HyperView<>(queryTabEnum, queryHyperTab().activeRecord(), queryHyperTab().mainTextInfo()));

    boolean result = queryHyperTab().showSearch(doSearch, type, query, fav, op1, op2, caption);
    updateFavorites();

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void recordLookup()
  {
    int nextID = hcbGoTo.somethingWasTyped ? HyperTableCell.getCellID(hcbGoTo.typedMatch) : -1;

    if (nextID < 1)
      nextID = hcbGoTo.selectedID();

    if (nextID < 1)
    {
      String text = HyperTableCell.getCellText(hcbGoTo.selectedHTC()).trim();
      if (text.length() > 0)
        lblStatus.setText("No results: searched " + db.getTypeName(selectorType()) + " records for \"" + abbreviate(text) + "\"");

      return;
    }

    goToRecord(db.records(selectorType()).getByID(nextID), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void incDecClick(boolean increment)
  {
    if (activeTabEnum() == treeTabEnum)
    {
      getTree().selectNextInstance(increment);
      return;
    }

    HyperDataset<? extends HDT_Record>.CoreAccessor records = db.records(activeType());

    int ndx = records.getKeyNdxByID(activeRecord().getID()) + (increment ? 1 : -1);
    if ((ndx >= 0) && (ndx < records.size()))
      goToRecord(records.getByKeyNdx(ndx), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void treeSelect()
  {
    if (treeSelector.getBase() == null)
    {
      messageDialog("Internal error #91827", mtError);
      return;
    }

    HDT_Record record = nullSwitch(getTree().selectedItem(), null, treeItem ->
                        nullSwitch(treeItem.getValue(), null, TreeRow::getRecord));

    if (record == null) return;

    RelationType relType = treeSelector.getRelTypeForTargetType(record.getType());

    if (relType == rtNone)
    {
      messageDialog("You must select a record of type: " + treeSelector.getTypesStr() + ".", mtError);
      return;
    }

    if (relType == rtUnited)
    {
      treeSelector.selectToUnite((HDT_RecordWithConnector) record, true);
      return;
    }

    if (treeSelector.select(record, true))
      goToRecord(treeSelector.getSubj(), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void uniteRecords(HDT_RecordWithConnector record1, HDT_RecordWithConnector record2, boolean goToRecord2)
  {
    String desc;

    if      ((record2.getType() == hdtWorkLabel) && (record2.isLinked() == false))       desc = record1.getMainText().getHtml();
    else if (ultraTrim(convertToSingleLine(record1.getMainText().getPlain())).isEmpty()) desc = record2.getMainText().getHtml();
    else if (ultraTrim(convertToSingleLine(record2.getMainText().getPlain())).isEmpty()) desc = record1.getMainText().getHtml();
    else if (record1.getMainText().getHtml().equals(record2.getMainText().getHtml()))    desc = record1.getMainText().getHtml();
    else
    {
      MergeSpokeDlgCtrlr frmMerge = MergeSpokeDlgCtrlr.build(record1, record2);

      if (frmMerge.showModal() == false)
        return;

      desc = frmMerge.getDesc();
    }

    if (StrongLink.connectRecords(record1.getConnector(), record2.getConnector(), desc))
      goToRecord(goToRecord2 ? record2 : record1, false);
    else
      update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setSelectorTab(Tab selectorTab)
  {
    selectorTabChangeIsProgrammatic = true;
    selectorTabPane.getSelectionModel().select(selectorTab);
    selectorTabChangeIsProgrammatic = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void tfOmniGoToChange(String newValue, boolean showingMore)
  {
    if (newValue.length() > 0)
      showFindTable();

    if (newValue.isEmpty())
    {
      tvFind.setPlaceholder(new Text(""));
      omniFinder.stop();
      return;
    }

    tvFind.setPlaceholder(new Text("Searching..."));
    omniFinder.setQueryAndStart(newValue, showingMore);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void omniFocus()
  {
    setSelectorTab(tabOmniSelector);
    safeFocus(tfOmniGoTo);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showFindTable()
  {
    tvFind.setVisible(true);
    apFindBackground.setMouseTransparent(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void hideFindTable()
  {
    apFindBackground.setMouseTransparent(true);
    tvFind.setVisible(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void importMiscFile(FileRow fileRow, FilePath filePath)
  {
    if (ui.cantSaveRecord()) return;

    HDT_MiscFile miscFile = db.createNewBlankRecord(hdtMiscFile);

    if (fileRow != null)
      miscFile.getPath().assign(fileRow.getFolder(), fileRow.getFilePath().getNameOnly());

    ui.goToRecord(miscFile, false);

    if (fileHyperTab().showFileDialog(filePath) == false)
    {
      if (fileRow != null)
        miscFile.getPath().clear(false);

      ui.deleteCurrentRecord(false);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean importWorkFile(HDT_Person person, FilePath filePathToUse, boolean promptForExistingRecord)
  {
    if (filePathToUse != null)
    {
      if (filePathToUse.equals(lastImportFilePath) && ((Instant.now().toEpochMilli() - lastImportTime) < 10000))
        return false;

      if (filePathToUse.exists() == false) return false;
    }

    if (cantSaveRecord()) return false;

    HDT_Work work = null;
    BibData bdToUse = null;
    BibEntry bibEntry = null;
    Ternary newEntryChoice = Ternary.Unset;
    EntryType newEntryType = EntryType.etUnentered;

    if (promptForExistingRecord)
    {
      SelectWorkDlgCtrlr swdc = SelectWorkDlgCtrlr.build(person, filePathToUse);
      if (swdc.showModal() == false) return false;

      work = swdc.getWork();
      person = swdc.getAuthor();
      bdToUse = swdc.getBibData();
      bibEntry = swdc.getBibEntry();
      MergeWorksDlgCtrlr mwd = null;

      if (work != null)
      {
        if (bibEntry != null)
        {
          if (work.getBibEntryKey().isBlank())
          {
            if (bibEntry.linkedToWork())
            {
              messageDialog("Internal error #62883", mtError);
              return false;
            }

            try
            {
              mwd = MergeWorksDlgCtrlr.build("Merge Fields", work.getBibData(), bibEntry, bdToUse, null, work, false, false, newEntryChoice,
                                             nullSwitch(filePathToUse, work.filePath()));
            }
            catch (IOException e)
            {
              messageDialog("Unable to initialize merge dialog window.", mtError);
              return false;
            }

            if (mwd.showModal() == false) return false;

            work.setBibEntryKey(bibEntry.getKey());
            mwd.mergeInto(bibEntry);
            bdToUse = bibEntry;
          }
          else if (bibEntry.getKey().equals(work.getBibEntryKey()) == false)
          {
            messageDialog("Internal error #62884", mtError);
            return false;
          }
        }

        if (mwd == null)
        {
          BibData workBD = work.getBibData();

          if ((bdToUse != null) && (bdToUse != GUIBibData.NoneFoundBD))
          {
            try
            {
              mwd = MergeWorksDlgCtrlr.build("Merge Fields", workBD, bdToUse, null, null, work, false, true, newEntryChoice,
                                             nullSwitch(filePathToUse, work.filePath()));
            }
            catch (IOException e)
            {
              messageDialog("Unable to initialize merge dialog window.", mtError);
              return false;
            }

            if (mwd.showModal() == false) return false;

            mwd.mergeInto(workBD);
          }

          bdToUse = workBD;
        }
      }

      if (mwd != null)
      {
        newEntryChoice = mwd.creatingNewEntry();
        newEntryType = mwd.getEntryType();
      }
    }

    boolean deleteRecord = work == null;

    if (work == null)
    {
      if ((bibEntry != null) && bibEntry.linkedToWork())
      {
        messageDialog("Internal error #62885", mtError);
        return false;
      }

      work = db.createNewBlankRecord(hdtWork);
      Authors authors = work.getAuthors();

      if (bibEntry != null)
      {
        work.getBibData().copyAllFieldsFrom(bibEntry, false, false);
        authors.setAll(bibEntry.getAuthors());
        work.setBibEntryKey(bibEntry.getKey());
        bdToUse = bibEntry;
      }

      if ((HDT_Work.sourceUnenteredWork != null) && authors.isEmpty())
        HDT_Work.sourceUnenteredWork.getAuthors().forEach(authors::add);
    }

    if ((bdToUse == null) || BibAuthors.isEmpty(bdToUse.getAuthors()))
    {
      Authors authors = work.getAuthors();

      if ((person != null) && (authors.containsPerson(person) == false))
        authors.add(person);
    }

    goToRecord(work, false);

    if (bdToUse == GUIBibData.NoneFoundBD)
      bdToUse = work.getBibData();

    if (workHyperTab().showWorkDialog(null, filePathToUse, bdToUse, newEntryChoice, newEntryType))
      return true;

    if (deleteRecord)
      deleteCurrentRecord(false);

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void handleArgs(List<String> args)
  {
    if ((db.isLoaded() == false) || collEmpty(args) || (windows.getOutermostModality() != Modality.NONE)) return;

    FilePath filePath = new FilePath(args.get(0));

    String mediaTypeStr = getMediaType(filePath).toString();

    if (mediaTypeStr.contains("pdf"))
    {
      importWorkFile(null, filePath, true);
      return;
    }

    if (mediaTypeStr.contains("text"))
    {
      importBibFile(null, filePath);
      return;
    }

    DialogResult result = new PopupDialog("What should the file be imported as?")

      .addButton("Work", mrYes)
      .addButton("Misc. file", mrNo)
      .addButton("Bibliographic details", mrContinue)
      .addButton("Cancel", mrCancel)

      .showModal();

    switch (result)
    {
      case mrYes      : importWorkFile(null, filePath, true); return;
      case mrNo       : importMiscFile(null, filePath      ); return;
      case mrContinue : importBibFile (null, filePath      ); return;

      default         : return;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void importBibFile(List<String> lines, FilePath filePath)
  {
    if (cantSaveRecord()) return;

    ImportBibEntryDlgCtrlr ibed = ImportBibEntryDlgCtrlr.build(lines, filePath);

    if (ibed.getFailedToLoad() || !ibed.showModal()) return;

    lines = ibed.getLines();
    filePath = ibed.getFilePath();

    String pathStr = FilePath.isEmpty(filePath) ? "" : (" " + filePath.toString());

    BibData bd = null;
    Exception ex = null;

    try
    {
      bd = BibTexBibData.create(lines);
    }
    catch (TokenMgrException | ParseException e)
    {
      ex = e;
    }

    if (bd == null)
      bd = RISBibData.create(lines);

    if (bd == null)
    {
      if (ex == null)
        messageDialog("Unable to parse bibliographic information.", mtError);
      else
        messageDialog("An error occurred while trying to parse BibTex file" + pathStr + ": " + ex.getMessage(), mtError);

      return;
    }

    boolean creatingNewWork = ibed.getCreateNewWork(),
            newEntryChecked = ibed.getCreateNewBibEntry(),
            showNewEntry = true;

    HDT_Work work = creatingNewWork ? db.createNewBlankRecord(hdtWork) : ibed.getRecord();

    BibData workBibData = work.getBibData();

    if (work.getBibEntryKey().length() > 0)
      showNewEntry = false;

    MergeWorksDlgCtrlr mwd = null;

    try
    {
      mwd = MergeWorksDlgCtrlr.build("Import Into " + (creatingNewWork ? "New" : "Existing") + " Work Record",
                                     workBibData, bd, null, null, work, creatingNewWork, showNewEntry, newEntryChecked ? Ternary.True : Ternary.Unset);
    }
    catch (IOException e)
    {
      messageDialog("Unable to initialize merge dialog window.", mtError);
      return;
    }

    if (mwd.showModal() == false)
    {
      if (creatingNewWork) db.deleteRecord(hdtWork, work.getID());
      return;
    }

    if (mwd.creatingNewEntry().isTrue())
    {
      BibEntry entry = db.getBibLibrary().addEntry(mwd.getEntryType());
      work.setBibEntryKey(entry.getKey());
      workBibData = entry;
    }

    mwd.mergeInto(workBibData);
    bibManagerDlg.refresh();

    goToRecord(work, false);
    update();

    if ((FilePath.isEmpty(filePath) == false) && ibed.getDeleteFile())
      filePath.deletePromptOnFail(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void showWelcomeWindow()
  {
    WelcomeDlgCtrlr wdc = WelcomeDlgCtrlr.build();
    if (wdc.showModal() == false) return;

    if (wdc.newClicked())
      mnuNewDatabaseClick();
    else if (wdc.openClicked())
      openDB(wdc.getOpenPath());

    if (db.isLoaded() == false)
      Platform.runLater(this::showWelcomeWindow);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void notifyOfImport(FilePath filePath)
  {
    lastImportTime = Instant.now().toEpochMilli();
    lastImportFilePath = filePath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static WebTooltip searchKeyToolTip = null;

  public void setSearchKeyToolTip(TextField tf)
  {
    if (searchKeyToolTip == null) searchKeyToolTip = new WebTooltip(

      "Multiple search keys should be separated by semicolon (<code>;</code>) character.<br>" +
      "<br>" +
      "Example:<blockquote>" +
      "<table><tr><td>Search keys:</td><td><code>Parfit; Derek Parfit</code></td></tr>" +
      "<tr><td>Description text:</td><td>This conclusion was rejected by <a href=\"\">Derek Parfit</a>. Instead, <a href=\"\">Parfit</a> argues that&hellip;</td></tr></table></blockquote>" +
      "Use up-caret (<code>^</code>) character to indicate that search key should match beginning of word.<br>" +
      "Use dollar sign (<code>$</code>) character to indicate that search key should match end of word.<br><br>" +
      "Example:" +
      "<blockquote>" +
      "<table><tr><td>Search keys:</td><td><code>^thing; object$; objects$; individual</code></td></tr>" +
      "<tr><td>Description text:</td><td>Anything that is an instance of an ontological category is an entity, but objectively<br>" +
      "speaking, only instances of certain categories count as <a href=\"\">things</a>, <a href=\"\">objects</a>, or <a href=\"\">individuals</a>.</td></tr></table>" +
      "</blockquote>" +
      "Notice that the <code>^</code> at the beginning of the <code>thing</code> search key prevents the word &lsquo;Anything&rsquo; from being matched. Similarly,<br>"+
      "the <code>$</code> at the end of the <code>object</code> search key prevents the word &lsquo;objectively&rsquo; from being matched.");

    tf.setTooltip(searchKeyToolTip);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}