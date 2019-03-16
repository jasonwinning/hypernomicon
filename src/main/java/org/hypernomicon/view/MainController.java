/*
 * Copyright 2015-2019 Jason Winning
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
import static org.hypernomicon.model.relations.RelationSet.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.queryEngines.AllQueryEngine.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.queryEngines.QueryEngine.QueryType.*;
import static org.hypernomicon.view.tabs.HyperTab.getHyperTab;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.tabs.QueriesTabController.*;
import static org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource.*;

import org.hypernomicon.App;
import org.hypernomicon.bib.BibData;
import org.hypernomicon.bib.lib.BibEntry;
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.queryEngines.QueryEngine.QueryType;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.dialogs.*;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.view.tabs.*;
import org.hypernomicon.view.tabs.HyperTab.TabEnum;
import org.hypernomicon.view.workMerge.MergeWorksDialogController;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.TreeWrapper.TreeTargetType;

import java.io.File;
import java.io.IOException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.SystemUtils;
import org.jbibtex.ParseException;
import org.jbibtex.TokenMgrException;
import com.google.common.collect.EnumHashBiMap;
import com.melloware.jintellitype.JIntellitype;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.application.Platform;
import javafx.beans.binding.BooleanExpression;
import javafx.collections.ObservableList;
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

public final class MainController
{
  @FXML Tab tabOmniSelector, tabViewSelector;
  @FXML TableView<HyperTableRow> tvFind;
  @FXML private AnchorPane apFindBackground, apGoTo, apListGoTo, apOmniGoTo, apStatus, midAnchorPane;
  @FXML private BorderPane mainPane;
  @FXML private Button btnAdvancedSearch, btnBibMgr, btnDecrement, btnFileMgr, btnIncrement, btnMentions, btnPreviewWindow,
                       btnSave, btnDelete, btnRevert, btnCreateNew, btnSearch, btnBack, btnForward, btnSaveAll, btnTextSearch;
  @FXML private ComboBox<HyperTableCell> cbGoTo;
  @FXML private GridPane gpBottom;
  @FXML private HBox topHBox;
  @FXML private ImageView ivDates, ivLeft, ivRight;
  @FXML private Label lblProgress;
  @FXML private Menu mnuFolders;
  @FXML private MenuBar menuBar;
  @FXML private MenuItem mnuAddToQueryResults, mnuChangeID, mnuCloseDatabase, mnuExitNoSave, mnuFindNextAll, mnuFindNextInName,
                         mnuFindPreviousAll, mnuFindPreviousInName, mnuFindWithinAnyField, mnuFindWithinName, mnuImportBibClipboard,
                         mnuImportBibFile, mnuNewCountry, mnuNewDatabase, mnuNewField, mnuNewPersonStatus, mnuNewRank,
                         mnuRecordSelect, mnuRevertToDiskCopy, mnuSaveReloadAll, mnuToggleFavorite;
  @FXML private ProgressBar progressBar;
  @FXML private SeparatorMenuItem mnuBibImportSeparator;
  @FXML private SplitMenuButton btnGoTo;
  @FXML private StackPane stackPane;
  @FXML private TabPane selectorTabPane, tabPane;
  @FXML private TextField tfID, tfOmniGoTo, tfRecord;
  @FXML private ToggleButton btnPointerPreview;
  @FXML private ToolBar tbGoTo, topToolBar;
  @FXML public Label lblStatus;
  @FXML public Menu mnuFavorites, mnuQueries;
  @FXML public Tab tabArguments, tabDebates, tabFiles, tabInstitutions, tabNotes, tabPersons, tabPositions, tabQueries, tabTerms, tabTree, tabWorks;
  @FXML public ToggleButton btnPointerLaunch;

  public final WindowStack windows = new WindowStack();
  public HyperViewSequence viewSequence;
  private final EnumHashBiMap<TabEnum, Tab> selectorTabs = EnumHashBiMap.create(TabEnum.class);
  private HyperFavorites favorites;
  private OmniFinder omniFinder;
  private HyperCB hcbGoTo;
  private HyperTable htFind;
  public final ComboBox<TreeRow> cbTreeGoTo = new ComboBox<>();
  private ComboBox<ResultsRow> cbResultGoTo = null;
  private ClickHoldButton chbBack, chbForward;
  private TextField selectorTF = null;

  public HDT_Base treeSubjRecord = null, treeObjRecord = null;
  public final List<TreeTargetType> treeTargetTypes = new ArrayList<>();

  public Tooltip ttDates;
  private boolean selectorTabChangeIsProgrammatic = false, maximized = false, internetNotCheckedYet = true;
  private double toolBarWidth = 0.0, maxWidth = 0.0, maxHeight = 0.0;

  private static final String TREE_SELECT_BTN_CAPTION = "Select";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ObservableList<ResultsRow> results() { return curQV.resultsTable.getTV().getItems(); }
  MenuBar getMenuBar()                         { return menuBar; }
  public TreeWrapper getTree()                 { return TreeTabController.class.cast(getHyperTab(treeTab)).getTree(); }
  private Stage primaryStage()                 { return app.getPrimaryStage(); }

  @FXML private void mnuExitClick()           { shutDown(true, true, true); }
  @FXML private void mnuExitNoSaveClick()     { if (confirmDialog("Abandon changes and quit?")) shutDown(false, true, false); }
  @FXML private void mnuAboutClick()          { AboutDialogController.create("About " + appTitle).showModal(); }
  @FXML private void mnuChangeFavOrderClick() { FavOrderDialogController.create("Change Order of Favorites").showModal(); }
  @FXML private void mnuSettingsClick()       { if (!cantSaveRecord(true)) OptionsDialogController.create(appTitle + " Settings").showModal(); }
  @FXML private void mnuFindMentionsClick()   { if (!cantSaveRecord(true)) searchForMentions(activeRecord(), false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void init() throws IOException
  {
    menuBar.setUseSystemMenuBar(true);

    updateProgress("", -1);

    Stage stage = primaryStage();

    windows.push(stage);

    stage.focusedProperty().addListener((Observable, oldValue, newValue) ->
    {
      if (windows.getCyclingFocus()) return;

      if ((newValue == null) || (newValue.booleanValue() == false)) return;

      windows.push(stage);
    });

    stage.widthProperty().addListener((Observable, oldValue, newValue) ->
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

    stage.heightProperty().addListener((observable, oldValue, newValue) ->
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

    stage.maximizedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue && !oldValue)
      {
        maximized = true;
        maxWidth = stage.getWidth();
        maxHeight = stage.getHeight();
      }
    });

    MainTextWrapper.init();

    ttDates = new Tooltip("No dates to show.");
    ttDates.setStyle("-fx-font-size: 14px;");

    PersonTabController     .addHyperTab(personTab     , tabPersons     , "PersonTab.fxml");
    InstitutionTabController.addHyperTab(institutionTab, tabInstitutions, "InstitutionTab.fxml");
    WorkTabController       .addHyperTab(workTab       , tabWorks       , "WorkTab.fxml");
    FileTabController       .addHyperTab(miscFileTab   , tabFiles       , "FileTab.fxml");
    DebateTab               .addHyperTab(debateTab     , tabDebates     , new DebateTab());
    PositionTab             .addHyperTab(positionTab   , tabPositions   , new PositionTab());
    ArgumentTab             .addHyperTab(argumentTab   , tabArguments   , new ArgumentTab());
    NoteTab                 .addHyperTab(noteTab       , tabNotes       , new NoteTab());
    TermTab                 .addHyperTab(termTab       , tabTerms       , new TermTab());
    QueriesTabController    .addHyperTab(queryTab      , tabQueries     , "QueriesTab.fxml");
    TreeTabController       .addHyperTab(treeTab       , tabTree        , "TreeTab.fxml");

    addSelectorTab(omniTab);
    addSelectorTab(listTab);

    chbBack = new ClickHoldButton(btnBack, Side.TOP);
    chbForward = new ClickHoldButton(btnForward, Side.TOP);

    btnBack.setTooltip(new Tooltip("Click to go back, hold to see history"));
    btnForward.setTooltip(new Tooltip("Click to go forward, hold to see history"));

    chbBack   .setOnAction(event -> btnBackClick());
    chbForward.setOnAction(event -> btnForwardClick());

    viewSequence = new HyperViewSequence(tabPane, chbForward, chbBack);

    tabViewSelector.disableProperty().bind(BooleanExpression.booleanExpression(tabQueries.selectedProperty().or(tabTree.selectedProperty())).not());

    setSelectorTab(selectorTabPane.getTabs().get(selectorTabPane.getTabs().size() - 1));

    hcbGoTo = new HyperCB(cbGoTo, ctDropDown, new RecordByTypePopulator(), null);

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

    btnGoTo.setOnAction         (event -> btnGoToClick(false));
    mnuRecordSelect.setOnAction (event -> btnGoToClick(true));

    hcbGoTo.setInnerOnAction(event -> recordLookup());
    hcbGoTo.dontCreateNewRecord = true;

    mnuImportBibFile     .setOnAction(event -> importBibFile(null, null));
    mnuImportBibClipboard.setOnAction(event -> importBibFile(convertMultiLineStrToStrList(getClipboardText(false), false), null));

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

    btnPointerLaunch.selectedProperty().addListener((observable, oldValue, newValue) ->
    {
      if ((newValue == null) || (newValue.booleanValue() == false) || (newValue.equals(oldValue))) return;
      appPrefs.putBoolean(PREF_KEY_RIGHT_CLICK_TO_LAUNCH, true);
    });

    btnPointerPreview.selectedProperty().addListener((observable, oldValue, newValue) ->
    {
      if ((newValue == null) || (newValue.booleanValue() == false) || (newValue.equals(oldValue))) return;
      appPrefs.putBoolean(PREF_KEY_RIGHT_CLICK_TO_LAUNCH, false);
    });

    btnPointerLaunch.setOnAction(event ->
    {
      if (appPrefs.getBoolean(PREF_KEY_RIGHT_CLICK_TO_LAUNCH, true))
        btnPointerLaunch.setSelected(true);
    });

    btnPointerPreview.setOnAction(event ->
    {
      if (appPrefs.getBoolean(PREF_KEY_RIGHT_CLICK_TO_LAUNCH, true) == false)
        btnPointerPreview.setSelected(true);
    });

    btnIncrement.setOnAction(event -> incDecClick(true));
    btnDecrement.setOnAction(event -> incDecClick(false));

    btnTextSearch.setTooltip(new Tooltip("Search within description"));

    apFindBackground.setOnMousePressed(event -> hideFindTable());

    tfOmniGoTo.setOnAction(event -> htFind.doRowAction());

    ttDates.setAutoHide(true);

    tabTree        .setGraphic(getImageViewForRelativePath("resources/images/treeview-small.png"));
    tabQueries     .setGraphic(getImageViewForRelativePath("resources/images/glasses-db.png"));
    tabOmniSelector.setGraphic(getImageViewForRelativePath("resources/images/globe.png"));
    tabViewSelector.setGraphic(getImageViewForRelativePath("resources/images/details.png"));

    favorites = new HyperFavorites(mnuFavorites, mnuQueries);

    HyperTab.getHyperTabs().forEach(hyperTab ->
    {
      TabEnum hyperTabEnum = hyperTab.getTabEnum();
      String path = getGraphicRelativePathByType(HyperTab.getRecordTypeByTabEnum(hyperTabEnum));

      ImageView graphic = getImageViewForRelativePath(path);
      if (graphic == null) return;

      hyperTab.getTab().setGraphic(graphic);

      nullSwitch(selectorTabs.get(hyperTabEnum), selectorTab -> selectorTab.setGraphic(getImageViewForRelativePath(path)));
    });

    if (SystemUtils.IS_OS_MAC)
    {
      topHBox.getChildren().remove(topToolBar);
      topHBox.setMinHeight(0.0);
      topHBox.setPrefHeight(0.0);
      topHBox.setMaxHeight(0.0);
      midAnchorPane.getChildren().add(topToolBar);
      AnchorPane.setTopAnchor(topToolBar, 0.0);
      AnchorPane.setRightAnchor(topToolBar, 0.0);

      midAnchorPane.widthProperty().addListener((observable, oldValue, newValue) ->
      {
        if ((newValue != null) && (newValue.doubleValue() > 1))
          adjustToolBar(newValue.doubleValue());
      });
    }

    if (JIntellitype.isJIntellitypeSupported())
    {
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
      if (record.getType() == hdtPerson)
      {
        PersonTabController personHyperTab = HyperTab.getHyperTab(personTab);
        if (personHyperTab.activeRecord() == record)
          personHyperTab.curPicture = null;  // User has already been asked if they want to delete the picture; don't ask again
      }

      QueriesTabController.class.cast(HyperTab.getHyperTab(queryTab)).queryViews.forEach(qv ->
        qv.resultsTable.getTV().getItems().removeIf(row -> row.getRecord() == record));

      int ndx = favorites.indexOfRecord(record);

      if (ndx > -1)
      {
        mnuFavorites.getItems().remove(ndx);
        updateFavorites();
      }

      viewSequence.removeRecord(record);
    });

//---------------------------------------------------------------------------

    selectorTabPane.getSelectionModel().selectedItemProperty().addListener((observable, oldTab, newTab) ->
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

    tfOmniGoTo.textProperty().addListener((observable, oldValue, newValue) ->
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

    tfRecord.focusedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue)
        tfRecord.setText("");
      else
        updateBottomPanel(true);
    });

//---------------------------------------------------------------------------

    tfID.focusedProperty().addListener((observable, oldValue, newValue) ->
    {
      if ((newValue.booleanValue() == false) && (activeRecord() != null))
        tfID.setText("" + activeRecord().getID());
      else
        tfID.setText("");
    });

//---------------------------------------------------------------------------

    tfRecord.setOnAction(event ->
    {
      if ((activeTab() == treeTab) || (activeTab() == queryTab)) return;
      if (activeRecord() == null)
      {
        tfRecord.setText("");
        return;
      }

      int curRecordNdx = db.records(activeType()).getKeyNdxByID(activeRecord().getID()),
          newRecordNdx = parseInt(tfRecord.getText(), 0) - 1;

      if ((newRecordNdx != curRecordNdx) && (newRecordNdx >= 0) && (newRecordNdx < db.records(activeType()).size()))
        goToRecord(db.records(activeType()).getByKeyNdx(newRecordNdx), true);
      else
        tfRecord.setText("");
    });

//---------------------------------------------------------------------------

    btnPreviewWindow.setOnAction(event ->
    {
      PreviewSource src = determinePreviewContext();

      if (activeTab() == TabEnum.miscFileTab)
      {
        HDT_MiscFile miscFile = (HDT_MiscFile) activeRecord();

        if (miscFile == null)
          previewWindow.clearPreview(src);
        else
          previewWindow.setPreview(src, miscFile.getPath().getFilePath(), -1, -1, miscFile);
      }

      openPreviewWindow(src);
    });

//---------------------------------------------------------------------------

    tfID.setOnAction(event ->
    {
      if ((activeTab() == treeTab) || (activeTab() == queryTab)) return;

      HDT_Base record = activeRecord();
      if (record == null)
      {
        tfID.setText("");
        return;
      }

      int newRecordID = parseInt(tfID.getText(), -1);

      if ((record.getID() != newRecordID) && (newRecordID > 0) && (db.records(activeType()).getIDNdxByID(newRecordID) > -1))
        goToRecord(db.records(activeType()).getByID(newRecordID), true);
      else
        tfID.setText("" + activeRecord().getID());
    });

//---------------------------------------------------------------------------

    mnuFindWithinName.setOnAction(event ->
    {
      if (selectorTabEnum() == omniTab)
        showSearch(true, QueryType.qtAllRecords, QUERY_WITH_NAME_CONTAINING, null, new HyperTableCell(-1, selectorTF.getText(), hdtNone), null, selectorTF.getText());
      else
        mnuFindWithinNameClick();
    });

//---------------------------------------------------------------------------

    mnuFindWithinAnyField.setOnAction(event ->
    {
      if (selectorTabEnum() == omniTab)
        showSearch(true, QueryType.qtAllRecords, QUERY_ANY_FIELD_CONTAINS, null, new HyperTableCell(-1, selectorTF.getText(), hdtNone), null, selectorTF.getText());
      else
        showSearch(true, QueryType.fromRecordType(selectorType()), QUERY_ANY_FIELD_CONTAINS, null, new HyperTableCell(-1, selectorTF.getText(), hdtNone), null, selectorTF.getText());
    });

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
      switch (activeTab())
      {
        case personTab : return pvsPersonTab;
        case workTab   : return pvsWorkTab;
        case queryTab  : return pvsQueryTab;
        case treeTab   : return pvsTreeTab;
        default        : return pvsOther;
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
    if (btnForward.isDisabled() || cantSaveRecord(true)) return;

    viewSequence.stepForward();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void btnBackClick()
  {
    if (btnBack.isDisabled() || cantSaveRecord(true)) return;

    viewSequence.stepBack();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void openPreviewWindow(PreviewSource src)
  {
    if (App.jxBrowserDisabled) return;

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

    if (cantSaveRecord(true)) return;

    fileManagerDlg.showNonmodal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void runBibMgr()
  {
    if (bibManagerDlg.getStage().isShowing())
    {
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

    currentTab().findWithinDesc(selectorTF.getText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnGoToClick(boolean fromMenu)
  {
    if (selectorTabEnum() != listTab)
    {
      hcbGoTo.triggerOnAction();

      if (hcbGoTo.selectedID() > 0)
        recordLookup();

      return;
    }

    TabEnum curTab = activeTab();

    if (curTab == queryTab)
    {
      curQV.resultsTable.dblClick(curQV.resultsTable.getTV().getSelectionModel().getSelectedItem());
      return;
    }
    else if (curTab != treeTab) return;

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

  public String getGraphicRelativePath(HDT_Base record)
  {
    switch (record.getType())
    {
      case hdtWork :

        WorkTypeEnum workType = HDT_Work.class.cast(record).getWorkTypeValue();

        switch (workType)
        {
          case wtBook         : return "resources/images/book.png";
          case wtChapter      : return "resources/images/chapter.png";
          case wtNone         : return "resources/images/unknown.png";
          case wtPaper        : return "resources/images/paper.png";
          case wtRecording    : return "resources/images/recording.png";
          case wtWebPage      : return "resources/images/text-html.png";
          case wtUnenteredSet : return "resources/images/inbox-document-text.png";
          default             : return "resources/images/unknown.png";
        }

      case hdtMiscFile :

        HDT_MiscFile miscFile = (HDT_MiscFile) record;

        if (miscFile.getPath().isEmpty() == false)
          return getImageRelPathForFilePath(miscFile.getPath().getFilePath(), null);

      default :

        return getGraphicRelativePathByType(record.getType());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getGraphicRelativePathByType(HDT_RecordType type)
  {
    switch (type)
    {
      case hdtWorkLabel     : return "resources/images/tag.png";
      case hdtMiscFile      : return "resources/images/file.png";
      case hdtConcept       : return "resources/images/term.png";
      case hdtGlossary      : return "resources/images/bookshelf.png";
      case hdtTerm          : return "resources/images/term.png";
      case hdtNote          : return "resources/images/notebook-pencil.png";
      case hdtWork          : return "resources/images/paper.png";
      case hdtPerson        : return "resources/images/people.png";
      case hdtInstitution   : return "resources/images/building-hedge.png";
      case hdtDebate        : return "resources/images/debate.png";
      case hdtPosition      : return "resources/images/position.png";
      case hdtArgument      : return "resources/images/argument.png";
      case hdtInvestigation : return "resources/images/documents-stack.png";
      case hdtFolder        : return "resources/images/folder.png";
      default               : return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void mnuFindWithinNameClick()
  {
    HDT_RecordType type = selectorType();
    String query = selectorTF.getText();
    boolean backClick = activeTab() != queryTab;

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

  // This assumes that the Queries tab is currently selected
  //
  private void discardLastQuery(boolean backClick)
  {
    QueriesTabController.class.cast(HyperTab.getHyperTab(queryTab)).closeCurrentView();

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
        if (cantSaveRecord(false) && prompt)
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

      HyperTab.getHyperTabs().forEach(HyperTab::clear);

      folderTreeWatcher.stop();

      try { db.close(null); }
      catch (HDB_InternalError e)
      {
        messageDialog(e.getMessage(), mtError);
      }
    }

    closeWindows(true);

    if (JIntellitype.isJIntellitypeSupported())
      JIntellitype.getInstance().cleanUp();

    Stage stage = primaryStage();

    if (savePrefs)
    {
      getHyperTabs().forEach(HyperTab::getDividerPositions);
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

    if (browserCoreInitialized)
      Platform.runLater(previewWindow::cleanup); // This eventually closes the application main window
    else
      stage.close();
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

  private void enableAll(boolean enabled)
  {
    boolean disabled = !enabled;

    gpBottom.getChildren().forEach(node -> node.setDisable(disabled));

    apStatus.setDisable(false);

    getHyperTabs().forEach(hyperTab -> hyperTab.enable(enabled));

    mnuNewDatabase      .setDisable(disabled);
    mnuCloseDatabase    .setDisable(disabled);
    mnuExitNoSave       .setDisable(disabled);
    mnuChangeID         .setDisable(disabled);
    mnuNewField         .setDisable(disabled);
    mnuNewCountry       .setDisable(disabled);
    mnuNewRank          .setDisable(disabled);
    mnuNewPersonStatus  .setDisable(disabled);
    mnuSaveReloadAll    .setDisable(disabled);
    mnuRevertToDiskCopy .setDisable(disabled);
    mnuAddToQueryResults.setDisable(disabled);
    btnFileMgr          .setDisable(disabled);
    btnBibMgr           .setDisable(disabled);
    btnPreviewWindow    .setDisable(disabled);
    btnMentions         .setDisable(disabled);
    btnAdvancedSearch   .setDisable(disabled);
    btnSaveAll          .setDisable(disabled);

    if (disabled)
      getTree().clear();

    hideFindTable();

    updateBibImportMenus();
    updateFavorites();
    updateTopicFolders();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateBibImportMenus()
  {
    mnuImportBibFile     .setVisible(db.bibLibraryIsLinked());
    mnuImportBibClipboard.setVisible(db.bibLibraryIsLinked());
    mnuBibImportSeparator.setVisible(db.bibLibraryIsLinked());

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

    QueriesTabController queriesTab = HyperTab.getHyperTab(queryTab);
    queriesTab.setCB(cbResultGoTo);

    cbResultGoTo.setConverter(new StringConverter<ResultsRow>()
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

  private void updateDatesTooltip(HDT_Base record)
  {
    if (record == null)
      ttDates.setText("No dates to show.");
    else
    {
      try
      {
        ttDates.setText("Created: " + dateTimeToUserReadableStr(record.getCreationDate()) +
                        "\nModified: " + dateTimeToUserReadableStr(record.getModifiedDate()) +
                        "\nAccessed: " + dateTimeToUserReadableStr(record.getViewDate()));
      }
      catch(Exception e)
      {
        ttDates.setText("No dates to show.");
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean saveAllToDisk(boolean saveRecord, boolean restartWatcher, boolean updateUI)
  {
    if (db.isLoaded() == false)
      return falseWithErrorMessage("No database is currently loaded.");

    if (saveRecord)
      if (cantSaveRecord(false)) return false;

    db.prefs.putInt(PREF_KEY_PERSON_ID     , getHyperTab(personTab     ).getActiveID());
    db.prefs.putInt(PREF_KEY_INSTITUTION_ID, getHyperTab(institutionTab).getActiveID());
    db.prefs.putInt(PREF_KEY_DEBATE_ID     , getHyperTab(debateTab     ).getActiveID());
    db.prefs.putInt(PREF_KEY_POSITION_ID   , getHyperTab(positionTab   ).getActiveID());
    db.prefs.putInt(PREF_KEY_ARGUMENT_ID   , getHyperTab(argumentTab   ).getActiveID());
    db.prefs.putInt(PREF_KEY_WORK_ID       , getHyperTab(workTab       ).getActiveID());
    db.prefs.putInt(PREF_KEY_TERM_ID       , getHyperTab(termTab       ).getActiveID());
    db.prefs.putInt(PREF_KEY_FILE_ID       , getHyperTab(miscFileTab   ).getActiveID());
    db.prefs.putInt(PREF_KEY_NOTE_ID       , getHyperTab(noteTab       ).getActiveID());

    db.prefs.put(PREF_KEY_RECORD_TYPE, db.getTypeTagStr(activeType() == hdtNone ? hdtPerson : activeType()));

    boolean watcherWasRunning = folderTreeWatcher.stop();

    if (db.bibLibraryIsLinked())
      bibManagerDlg.saveToDisk();

    db.saveAllToDisk(favorites);

    if (restartWatcher && watcherWasRunning)
      folderTreeWatcher.createNewWatcherAndStart();

    if (updateUI) update();

    lblStatus.setText("Database last saved to XML files: " + timeToUserReadableStr(LocalDateTime.now()));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuSaveReloadAllClick()
  {
    if (saveAllToDisk(true, false, false) == false) return;

    if (loadDataFromDisk())
    {
      viewSequence.refresh();

      HyperTab.getHyperTabs().forEach(this::refreshTab);

      if (activeTab() == queryTab)
        HyperTab.getHyperTab(queryTab).clear();

      update();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> void refreshTab(HyperTab<HDT_RT, HDT_CT> hyperTab)
  {
    nullSwitch(hyperTab.getView(), view ->
    {
      view.refresh();
      nullSwitch(view.getViewRecord(), hyperTab::setRecord);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuOpenClick()
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter(appTitle + " files (*.hdb)", "*.hdb"));

    File dir = new File(appPrefs.get(PREF_KEY_SOURCE_PATH, System.getProperty("user.dir")));

    if (dir.exists() == false)
      dir = new File(System.getProperty("user.dir"));

    fileChooser.setInitialDirectory(dir);

    FilePath filePath = new FilePath(fileChooser.showOpenDialog(primaryStage()));

    if (FilePath.isEmpty(filePath)) return;

    if (db.isLoaded())
    {
      if (confirmDialog("Save data to XML files?"))
      {
        if (cantSaveRecord(true)) return;
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
    DirectoryChooser dirChooser = new DirectoryChooser();

    File file = new File(appPrefs.get(PREF_KEY_SOURCE_PATH, ""));

    dirChooser.setTitle("Select an empty folder in which to create database");

    if (file.exists() && file.isDirectory())
      dirChooser.setInitialDirectory(file);
    else
      dirChooser.setInitialDirectory(new File(System.getProperty("user.dir")));

    file = dirChooser.showDialog(primaryStage());

    if (file == null) return;

    String[] list = file.list();
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

    if (cantSaveRecord(true)) return;

    if (db.isLoaded())
    {
      if (confirmDialog("Save data to XML files?"))
        saveAllToDisk(false, false, false);
    }

    NewDatabaseDialogController dlg = NewDatabaseDialogController.create("Customize How Database Will Be Created", file.getPath());

    if (dlg.showModal() == false)
      return;

    closeWindows(false);

    try { db.newDB(file.getPath(), dlg.getChoices(), dlg.getFolders()); }
    catch (HDB_InternalError e)
    {
      messageDialog("Unable to create new database: " + e.getMessage(), mtError);
      shutDown(false, true, false); // An error in db.close is unrecoverable.
      return;
    }

    clearAllTabsAndViews();

    saveAllToDisk(false, false, false);

    loadDB();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearAllTabsAndViews()
  {
    HyperTab.getHyperTabs().forEach(this::clearTab);

    previewWindow.clearAll();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> void clearTab(HyperTab<HDT_RT, HDT_CT> hyperTab)
  {
    HyperTab.setTabView(new HyperView<HDT_CT>(hyperTab.getTabEnum(), null));
    hyperTab.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML public void btnSaveClick()
  {
    if (btnSave.getText().equals(TREE_SELECT_BTN_CAPTION))
      treeSelect();
    else if (!cantSaveRecord(true))
      update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnCreateClick()
  {
    if (cantSaveRecord(true)) return;

    HDT_RecordType type = selectorType();
    String name = hcbGoTo.selectedID() == -1 ? cbGoTo.getEditor().getText() : "";

    HDT_Base record = db.createNewBlankRecord(type);
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
      else
        record.setName(titleCase(name).trim());
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
    HDT_Base record = activeRecord();

    if (record == null) return false;

    HDT_RecordType type = record.getType();

    switch (type)
    {
      case hdtGlossary :

        if (activeTab() != treeTab)
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

      if (confirmDialog("Type: " + db.getTypeName(type) + "\n" +
                        "Name: " + record.getCBText() + "\n" +
                        "ID: " + record.getID() + "\n\n" + msg) == false) return false;
    }

    db.deleteRecord(type, record.getID());

    viewSequence.activateCurrentSlot();
    fileManagerDlg.setNeedRefresh();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuCloseClick()
  {
    if (db.isLoaded())
    {
      if (confirmDialog("Save data to XML files before closing?"))
      {
        if (cantSaveRecord(true)) return;
        saveAllToDisk(false, false, false);
      }
    }

    clearAllTabsAndViews();

    treeSubjRecord = null;
    closeWindows(false);

    try { db.close(null); }
    catch (HDB_InternalError e)
    {
      messageDialog(e.getMessage(), mtError);
      shutDown(false, true, false); // An error in db.close is unrecoverable.
      return;
    }

    enableAll(false);

    updateBottomPanel(true);
    tfRecord.setText("");
    tfID.setText("");
    hcbGoTo.clear();

    primaryStage().setTitle(appTitle);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateTopicFolders()
  {
    while (mnuFolders.getItems().size() > 10) // Clear the topical folder items that currently exist
      mnuFolders.getItems().remove(10);

    if (db.isLoaded() == false)
    {
      mnuFolders.setDisable(true);
      return;
    }

    FilePath topicsPath = db.getPath(PREF_KEY_TOPICAL_PATH);
    mnuFolders.setDisable(false);

    try (DirectoryStream<Path> stream = Files.newDirectoryStream(topicsPath.toPath(), "**"))
    {
      stream.forEach(entry ->
      {
        FilePath entryFilePath = new FilePath(entry);

        if (entryFilePath.isDirectory())
        {
          FilePath relFilePath = topicsPath.relativize(entryFilePath);

          if (FilePath.isEmpty(relFilePath) == false)
          {
            MenuItem item = new MenuItem();
            item.setText(relFilePath.toString());
            item.setOnAction(event -> launchFile(entryFilePath));
            mnuFolders.getItems().add(item);
          }
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

    if (cantSaveRecord(true)) return;

    ChangeIDDialogController ctrlr = ChangeIDDialogController.create("Change Record ID");

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

    if (cantSaveRecord(true)) return;

    NewCategoryDialogController ctrlr = NewCategoryDialogController.create("New Category", type);

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
      case 1 : filePath = db.getPath(PREF_KEY_PAPERS_PATH    ); break;
      case 2 : filePath = db.getPath(PREF_KEY_BOOKS_PATH     ); break;
      case 3 : filePath = db.getPath(PREF_KEY_UNENTERED_PATH ); break;
      case 4 : filePath = db.getPath(PREF_KEY_TOPICAL_PATH   ); break;
      case 5 : filePath = db.getPath(PREF_KEY_PICTURES_PATH  ); break;
      case 6 : filePath = db.getPath(PREF_KEY_MISC_FILES_PATH); break;
      case 7 : filePath = new FilePath(appPrefs.get(PREF_KEY_SOURCE_PATH, "")); break;
      case 8 : filePath = db.getPath(PREF_KEY_RESULTS_PATH   ); break;
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
    public FavMenuItem(HDT_Base record)
    {
      super(db.getTypeName(record.getType()) + ": " + record.getCBText());
      isQuery = false;
      favRecord = new HyperTableCell(record.getID(), record.getCBText(), record.getType());
      setOnAction(event -> goToRecord(record, true));
    }

    public FavMenuItem(QueryFavorite query)
    {
      super("Query: " + query.name);
      isQuery = true;
      this.query = query;
      setOnAction(event -> showSearch(query.autoexec, null, -1, query, null, null, query.name));
    }

    public boolean isQuery;
    public QueryFavorite query;
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

    if ((activeTab() != treeTab) && (activeTab() != queryTab) && viewRecord() != null)
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
    if ((activeTab() == treeTab) || (activeTab() == queryTab)) return;
    if (cantSaveRecord(true)) return;

    HDT_Base record = viewRecord();
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
    ht.addContextMenuItem("Launch work file", HDT_Position.class,
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

  private void searchForMentions(HDT_Base record, boolean descOnly)
  {
    boolean noneFound = false, didSearch = false, backClick = activeTab() != queryTab;

    if (record == null) return;

    HDT_RecordType type = record.getType();
    int id = record.getID();

    lblStatus.setText("");

    if (descOnly)
      didSearch = showSearch(true, qtAllRecords, QUERY_LINKING_TO_RECORD, null, new HyperTableCell(-1, "", type), new HyperTableCell(id, "", type), "Mentions: " + record.listName());
    else
      didSearch = showSearch(true, qtAllRecords, QUERY_MATCHING_RECORD, null, new HyperTableCell(-1, "", type), new HyperTableCell(id, "", type), "Mentions: " + record.listName());

    if (!didSearch)
    {
      discardLastQuery(backClick);
      return;
    }

    if (curQV.resultsBackingList.size() == 1)
    {
      if (curQV.resultsBackingList.get(0).getRecord() == record)
        noneFound = true;
    }
    else if (curQV.resultsBackingList.size() == 0)
      noneFound = true;

    if (!noneFound) return;

    discardLastQuery(backClick);
    lblStatus.setText("No mentioners: " + db.getTypeName(type).toLowerCase() + " \"" + abbreviate(record.listName()) + "\"");
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

    HDT_Base record = null;

    if (activeTab() == termTab)
    {
      HDT_Term term = (HDT_Term) activeRecord();
      record = viewRecord();

      if (record == null)
        record = term;
      else
      {
        PopupDialog dlg = new PopupDialog("Which record?");

        dlg.addButton("Term", mrYes);
        dlg.addButton("Concept", mrNo);

        if (dlg.showModal() == mrYes)
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

    HDT_Base record = activeRecord();

    if (record == null)
      return falseWithErrorMessage("No record is currently selected.");

    if (record.hasStoredState() == false)
      return falseWithErrorMessage("Unable to revert: the record may not have been previously saved to disk.");

    if (confirmDialog("Are you sure you want to revert this record to the last version saved to disk?") == false) return false;

    HDT_Base viewRecord = viewRecord();

    if (revertToDiskCopy(record) && (viewRecord != null) && (viewRecord != record))
      if ((activeTab() != treeTab) && (activeTab() != queryTab))
        revertToDiskCopy(viewRecord);

    update();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean revertToDiskCopy(HDT_Base record)
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

  public void loadDB()
  {
    if (loadDataFromDisk() == false)
    {
      if (db.isLoaded() == false)
        clearAllTabsAndViews();

      enableAll(db.isLoaded());
      return;
    }

    HyperTab.setTabView(new HyperView<>(personTab     , db.persons     .getByID(db.prefs.getInt(PREF_KEY_PERSON_ID     , -1))));
    HyperTab.setTabView(new HyperView<>(institutionTab, db.institutions.getByID(db.prefs.getInt(PREF_KEY_INSTITUTION_ID, -1))));
    HyperTab.setTabView(new HyperView<>(debateTab     , db.debates     .getByID(db.prefs.getInt(PREF_KEY_DEBATE_ID     , -1))));
    HyperTab.setTabView(new HyperView<>(positionTab   , db.positions   .getByID(db.prefs.getInt(PREF_KEY_POSITION_ID   , -1))));
    HyperTab.setTabView(new HyperView<>(argumentTab   , db.arguments   .getByID(db.prefs.getInt(PREF_KEY_ARGUMENT_ID   , -1))));
    HyperTab.setTabView(new HyperView<>(workTab       , db.works       .getByID(db.prefs.getInt(PREF_KEY_WORK_ID       , -1))));

    HDT_Term term = db.terms.getByID(db.prefs.getInt(PREF_KEY_TERM_ID, -1));
    HDT_Concept concept = term != null ? term.concepts.get(0) : null;
    HyperTab.setTabView(new HyperView<>(termTab,        concept));

    HyperTab.setTabView(new HyperView<>(miscFileTab   , db.miscFiles   .getByID(db.prefs.getInt(PREF_KEY_FILE_ID       , -1))));
    HyperTab.setTabView(new HyperView<>(noteTab       , db.notes       .getByID(db.prefs.getInt(PREF_KEY_NOTE_ID       , -1))));
    HyperTab.setTabView(new HyperView<>(queryTab      , null));
    HyperTab.setTabView(new HyperView<>(treeTab       , null));

    enableAll(db.isLoaded());

    TabEnum tabEnum = HyperTab.getTabEnumByRecordType(db.parseTypeTagStr(db.prefs.get(PREF_KEY_RECORD_TYPE, "")));

    viewSequence.init();
    viewSequence.forwardToNewSlot(tabEnum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean loadDataFromDisk()
  {
    if (SystemUtils.IS_OS_MAC)
      Platform.runLater(() -> adjustToolBar(0));

    if (appPrefs.get(PREF_KEY_SOURCE_FILENAME, "").length() == 0) return false;
    if (appPrefs.get(PREF_KEY_SOURCE_PATH, "").length() == 0) return false;

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
      if (LockedDialogController.create("Database is Currently Locked", otherCompName).showModal() == false)
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
      success = folderTreeWatcher.createNewWatcherAndStart();

    if (success)
    {
      lblStatus.setText("");
      updateTopicFolders();
      getHyperTab(queryTab).clear();

      gpBottom.setDisable(false);
      getHyperTab(queryTab).enable(true);
      getHyperTab(treeTab).enable(true);

      getTree().expandMainBranches();
      fileManagerDlg.folderTree.expandMainBranches();

      primaryStage().setTitle(appTitle + " - " + db.getPath("", appPrefs.get(PREF_KEY_SOURCE_FILENAME, "")));
    }
    else
      mnuCloseClick();

    return success;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean cantSaveRecord(boolean showMessage)
  {
    if ((db.isLoaded() == false) || (activeTab() == queryTab) || (activeTab() == treeTab) || (activeRecord() == null))
      return false;

    CommitableWrapper.commitWrapper(primaryStage().getScene().getFocusOwner());

    return currentTab().saveToRecord(showMessage) == false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToTreeRecord(HDT_Base record)
  {
    if (db.isLoaded() == false) return;

    if (cantSaveRecord(true))
    {
      treeSubjRecord = null;
      treeObjRecord = null;
      treeTargetTypes.clear();

      return;
    }

    viewSequence.forwardToNewSlotAndView(new HyperView<>(treeTab, record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TabEnum activeTab()                       { return viewSequence.isEmpty() ? personTab : viewSequence.curTabEnum(); }
  public HyperTab<? extends HDT_Base,
                  ? extends HDT_Base> currentTab() { return viewSequence.isEmpty() ? null : viewSequence.curHyperTab(); }
  public HDT_RecordType activeType()               { return viewSequence.isEmpty() ? hdtPerson : viewSequence.curHyperView().getTabRecordType(); }
  public HDT_Base activeRecord()                   { return viewSequence.isEmpty() ? null : currentTab().activeRecord(); }
  public HDT_Base viewRecord()                     { return viewSequence.isEmpty() ? null : currentTab().viewRecord(); }

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

  public void goToRecord(HDT_Base record, boolean save)
  {
    if ((record == null) || (db.isLoaded() == false)) return;

    treeSubjRecord = null;
    HDT_Investigation inv = null;

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

        goToFileInManager(HDT_Folder.class.cast(record).getPath().getFilePath());
        return;

      case hdtInvestigation :

        inv = (HDT_Investigation)record;
        record = inv.person.get();
        break;

      case hdtWorkFile :

        HDT_WorkFile workFile = (HDT_WorkFile)record;
        if (workFile.works.size() > 0)
          record = workFile.works.get(0);
        break;

      case hdtTerm :

        record = HDT_Term.class.cast(record).concepts.get(0);
        break;

      default : break;
    }

    if (HyperTab.getTabEnumByRecordType(record.getType()) == personTab)
      if (record.getType() != hdtPerson) return;

    if (windows.getOutermostStage() != primaryStage())
      windows.focusStage(primaryStage());

    if (save && cantSaveRecord(true)) return;

    viewSequence.forwardToNewSlotAndView(HyperViewSequence.createViewForRecord(record));

    if (inv != null)
      PersonTabController.class.cast(currentTab()).showInvestigation(inv.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void update()
  {
    updateTopicFolders();

    if (db.isLoaded() == false)
    {
      getTree().clear();
      return;
    }

    switch (activeTab())
    {
      case queryTab : case treeTab :
        HyperTab.getHyperTab(activeTab()).update();
        updateBottomPanel(true);
        return;

      default :
        break;
    }

    int count = currentTab().getRecordCount();

    treeSubjRecord = null;

    if (count > 0)
    {
      if (HDT_Record.isEmpty(activeRecord()))
      {
        int ndx = HyperTab.getHyperTab(activeTab()).getView().getTabRecordKeyNdx();

        if (ndx >= count)
          ndx = count - 1;

        if (ndx < 0)
          ndx = 0;

        if (activeTab() == termTab)
          viewSequence.updateCurrentView(HyperViewSequence.createViewForRecord(termTab, db.terms.getByKeyNdx(ndx).concepts.get(0)));
        else
        {
          HDT_Base record = db.records(activeType()).getByKeyNdx(ndx);
          viewSequence.updateCurrentView(HyperViewSequence.createViewForRecord(activeTab(), record));
        }
      }
    }
    else
      viewSequence.updateCurrentView(HyperViewSequence.createViewForRecord(activeTab(), null));

    if (currentTab() == null)
      messageDialog("Internal error #38273", mtError);
    else
    {
      updateBottomPanel(true);
      currentTab().clear();

      if (activeRecord() == null)
      {
        currentTab().enable(false);
        return;
      }

      currentTab().enable(true);
      if (currentTab().update())
        activeRecord().viewNow();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TabEnum selectorTabEnum()
  {
    Tab tab = selectorTabPane.getSelectionModel().getSelectedItem();
    return selectorTabs.inverse().get(tab);
  }

  private HDT_RecordType selectorType()
  {
    TabEnum tabEnum = selectorTabEnum();

    if (EnumSet.of(listTab, omniTab).contains(selectorTabEnum()))
      return activeType();

    return HyperTab.getRecordTypeByTabEnum(tabEnum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateSelectorTab(boolean setFocus)
  {
    TabEnum tabEnum = selectorTabEnum();
    HyperTab<? extends HDT_Base, ? extends HDT_Base> hyperTab = nullSwitch(HyperTab.getHyperTab(tabEnum), currentTab());
    selectorTF = null;

    int count = hyperTab == null ? 0 : hyperTab.getRecordCount();

    mnuRecordSelect.setVisible(true);
    mnuFindNextInName.setVisible(false);
    mnuFindNextAll.setVisible(false);
    mnuFindPreviousAll.setVisible(false);
    mnuFindWithinName.setVisible(false);
    mnuFindWithinAnyField.setVisible(false);
    mnuFindPreviousInName.setVisible(false);

    switch (tabEnum)
    {
      case listTab :

        if (activeTab() == queryTab)
        {
          if (cbResultGoTo == null) initResultCB();

          apListGoTo.getChildren().setAll(cbResultGoTo);
          selectorTF = cbResultGoTo.getEditor();
        }

        if (activeTab() == treeTab)
        {
          mnuFindNextAll.setVisible(true);
          mnuFindPreviousAll.setVisible(true);
          mnuFindPreviousInName.setVisible(true);
          mnuFindNextInName.setVisible(true);

          copyRegionLayout(cbGoTo, cbTreeGoTo);
          apListGoTo.getChildren().setAll(cbTreeGoTo);
          selectorTF = cbTreeGoTo.getEditor();
        }

        btnCreateNew.setDisable(true);

        break;

      case omniTab :

        mnuRecordSelect.setVisible(false);
        mnuFindWithinAnyField.setVisible(true);
        mnuFindWithinName.setVisible(true);

        selectorTF = tfOmniGoTo;

        btnCreateNew.setDisable((activeTab() == queryTab) || (activeTab() == treeTab));

        break;

      default :

        mnuFindWithinAnyField.setVisible(true);
        mnuFindWithinName.setVisible(true);

        selectorTF = cbGoTo.getEditor();
        hcbGoTo.clear();
        RecordByTypePopulator.class.cast(hcbGoTo.getPopulator()).setRecordType(Populator.dummyRow, selectorType());
        if (cbGoTo.isEditable() == false) cbGoTo.setEditable(true);
        btnCreateNew.setDisable(false);

        if (count > 0)
        {
          HDT_Base record = nullSwitch(hyperTab, null, HyperTab::activeRecord);  // Save to variable to avoid Maven compile errors
          hcbGoTo.addAndSelectEntryOrBlank(record, HDT_Base::listName);
        }

        break;
    }

    hideFindTable();

    if (setFocus && (selectorTF != null)) Platform.runLater(() ->
    {
      selectorTF.requestFocus();
      selectorTF.selectAll();
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

    HyperTab<? extends HDT_Base, ? extends HDT_Base> curTab = currentTab();
    if (curTab == null) return;

    int count = curTab.getRecordCount(), ndx = curTab.getRecordNdx();
    HDT_Base activeRec = activeRecord();
    TabEnum activeTabEnum = activeTab();

    attachOrphansToRoots();

    btnTextSearch.setDisable(false);

  //---------------------------------------------------------------------------
  // Query-specific stuff
  //---------------------------------------------------------------------------

    if (activeTabEnum == queryTab)
    {
      btnSave.setDisable(true);
      btnSave.setText("Accept Edits");
      btnDelete.setDisable(activeRec == null);
      btnRevert.setDisable(true);
      btnRevert.setText("Revert");
      btnIncrement.setDisable(true);
      btnDecrement.setDisable(true);
    }

  //---------------------------------------------------------------------------
  // Tree-specific stuff
  //---------------------------------------------------------------------------

    else if (activeTabEnum == treeTab)
    {
      if (treeSubjRecord == null)
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

      btnIncrement.setDisable(count < 1);
      btnDecrement.setDisable(count < 1);

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

      btnDelete.setDisable(activeRec == null);
      btnSave  .setDisable(activeRec == null);

      btnRevert.setText("Revert");
//      if (changed)
      btnRevert.setDisable(false);
//      else
//        btnRevert->Enabled = false;

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
    if (cantSaveRecord(true)) return false;

    QueriesTabController queriesTab = (QueriesTabController) HyperTab.getHyperTab(queryTab);

    viewSequence.forwardToNewSlotAndView(new HyperView<>(queryTab, queriesTab.activeRecord(), queriesTab.getMainTextInfo()));

    boolean result = queriesTab.showSearch(doSearch, type, query, fav, op1, op2, caption);
    updateFavorites();

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void recordLookup()
  {
    int nextID = -1;

    if (hcbGoTo.somethingWasTyped)
      nextID = HyperTableCell.getCellID(hcbGoTo.typedMatch);

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
    if (activeTab() == treeTab)
    {
      getTree().selectNextInstance(increment);
      return;
    }

    HyperDataset<? extends HDT_Base>.CoreAccessor records = db.records(activeType());

    int ndx = records.getKeyNdxByID(activeRecord().getID());

    if (increment)
    {
      ndx++;
      if (ndx >= records.size()) return;
    }
    else
    {
      if (ndx <= 0) return;
      ndx--;
    }

    goToRecord(records.getByKeyNdx(ndx), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void treeSelect()
  {
    if (treeSubjRecord == null)
    {
      messageDialog("Internal error #91827", mtError);
      return;
    }

    HDT_Base record = nullSwitch(getTree().selectedItem(), (HDT_Base)null, treeItem ->
                      nullSwitch(treeItem.getValue(), null, TreeRow::getRecord));

    if (record == null) return;

    RelationType relType = findFirst(treeTargetTypes, ttType -> ttType.objType == record.getType(), rtNone, ttType -> ttType.relType);

    if (relType == rtNone)
    {
      String msg = "You must select a record of type: ";
      int lastNdx = treeTargetTypes.size() - 1;

      for (int ndx = 0; ndx <= lastNdx; ndx++)
      {
        msg += db.getTypeName(treeTargetTypes.get(ndx).objType);

        if       ((ndx == 0) && (lastNdx == 1))   msg += " or ";
        else if  (ndx == (lastNdx - 1))           msg += ", or ";
        else if  (ndx < lastNdx)                  msg += ", ";
      }

      messageDialog(msg + ".", mtError);
      return;
    }

    if (relType == rtUnited)
    {
      treeSelectToUnite((HDT_RecordWithConnector) record);
      return;
    }

    if (treeObjRecord != null)
      db.getObjectList(getRelation(treeSubjRecord.getType(), treeObjRecord.getType()), treeSubjRecord, true).remove(treeObjRecord);

    HyperObjList<HDT_Base, HDT_Base> objList = db.getObjectList(relType, treeSubjRecord, true);

    objList.add(record);
    try { objList.throwLastException(); }
    catch (RelationCycleException e)
    {
      messageDialog("Cannot use selected record: Records would be organized in a cycle as a result.", mtError);

      if (treeObjRecord != null)
        db.getObjectList(getRelation(treeSubjRecord.getType(), treeObjRecord.getType()), treeSubjRecord, true).add(treeObjRecord);
    }

    goToRecord(treeSubjRecord, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean treeSelectToUnite(HDT_RecordWithConnector record2)
  {
    HDT_RecordWithConnector record1 = (HDT_RecordWithConnector) treeSubjRecord;

    if ((record2.getType() == record1.getType()))
      return falseWithErrorMessage("You cannot connect records of the same type.");

    if (record2.isLinked())
    {
      if ((record2.getLink().getSpoke(record1.getType()) != null))
        return falseWithErrorMessage("The selected " + db.getTypeName(record2.getType()) + " is already connected to a " + db.getTypeName(record1.getType()) + ".");

      if (record1.getType() == hdtDebate)
        if ((record2.getLink().getSpoke(hdtPosition) != null))
          return falseWithErrorMessage("The selected " + db.getTypeName(record2.getType()) + " is already connected to a " + db.getTypeName(hdtPosition) + ".");

      if (record1.getType() == hdtPosition)
        if ((record2.getLink().getSpoke(hdtDebate) != null))
          return falseWithErrorMessage("The selected " + db.getTypeName(record2.getType()) + " is already connected to a " + db.getTypeName(hdtDebate) + ".");

      if (record1.isLinked())
        return falseWithErrorMessage("Both records are already linked to other records.");
    }

    if (record1.isLinked())
    {
      if ((record1.getLink().getSpoke(record2.getType()) != null))
        return falseWithErrorMessage("The selected " + db.getTypeName(record1.getType()) + " is already connected to a " + db.getTypeName(record2.getType()) + ".");

      if (record2.getType() == hdtDebate)
        if ((record1.getLink().getSpoke(hdtPosition) != null))
          return falseWithErrorMessage("The selected " + db.getTypeName(record1.getType()) + " is already connected to a " + db.getTypeName(hdtPosition) + ".");

      if (record2.getType() == hdtPosition)
        if ((record1.getLink().getSpoke(hdtDebate) != null))
          return falseWithErrorMessage("The selected " + db.getTypeName(record1.getType()) + " is already connected to a " + db.getTypeName(hdtDebate) + ".");
    }

    uniteRecords(record1, record2);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void uniteRecords(HDT_RecordWithConnector record1, HDT_RecordWithConnector record2)
  {
    String desc;

    if      ((record2.getType() == hdtWorkLabel) && (record2.isLinked() == false))           desc = record1.getMainText().getHtml();
    else if (ultraTrim(convertToSingleLine(record1.getMainText().getPlain())).length() == 0) desc = record2.getMainText().getHtml();
    else if (ultraTrim(convertToSingleLine(record2.getMainText().getPlain())).length() == 0) desc = record1.getMainText().getHtml();
    else if (record1.getMainText().getHtml().equals(record2.getMainText().getHtml()))        desc = record1.getMainText().getHtml();
    else
    {
      MergeSpokeDialogController frmMerge = MergeSpokeDialogController.create("Select how to merge fields", record1, record2);

      if (frmMerge.showModal())
        desc = frmMerge.getDesc();
      else
        return;
    }

    if (StrongLink.connectRecords(record1.getConnector(), record2.getConnector(), desc))
      goToRecord(record1, false);
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

    if (newValue.length() == 0)
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

  public void handleArgs(List<String> args)
  {
    if ((db.isLoaded() == false) || collEmpty(args) || (windows.getOutermostModality() != Modality.NONE)) return;

    FilePath filePath = new FilePath(args.get(0));

    String mediaTypeStr = getMediaType(filePath).toString();

    if (mediaTypeStr.contains("pdf"))
    {
      if (cantSaveRecord(true)) return;

      HDT_Work work = db.createNewBlankRecord(hdtWork);

      goToRecord(work, false);

      WorkTabController workCtrlr = HyperTab.getHyperTab(workTab);

      if (workCtrlr.showWorkDialog(null, filePath) == false)
        deleteCurrentRecord(false);

      return;
    }

    if (mediaTypeStr.contains("text"))
      importBibFile(null, filePath);
    else
      messageDialog("Unable to import file: " + filePath.toString(), mtError);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void importBibFile(List<String> lines, FilePath filePath)
  {
    if (cantSaveRecord(true)) return;

    ImportBibEntryDialogController ibed = ImportBibEntryDialogController.create("Import Bibliography File", lines, filePath);

    if (ibed.getFailedToLoad() || !ibed.showModal()) return;

    lines = ibed.getLines();
    filePath = ibed.getFilePath();

    String pathStr = (filePath == null ? "" : " " + filePath.toString());

    BibData bd = null;
    Exception ex = null;

    try
    {
      bd = BibData.createFromBibTex(lines);
    }
    catch (TokenMgrException | ParseException e)
    {
      ex = e;
    }

    if (bd == null)
      bd = BibData.createFromRIS(lines);

    if (bd == null)
    {
      if (ex == null)
        messageDialog("Unable to parse bibliographic information.", mtError);
      else
        messageDialog("An error occurred while trying to parse BibTex file" + pathStr + ": " + ex.getMessage(), mtError);

      return;
    }

    boolean creatingNewWork = ibed.getCreateNewWork(),
            creatingNewEntry = ibed.getCreateNewBibEntry();

    HDT_Work work = creatingNewWork ? db.createNewBlankRecord(hdtWork) : ibed.getRecord();

    BibData workBibData = work.getBibData();

    if (work.getBibEntryKey().length() > 0)
      creatingNewEntry = false;

    MergeWorksDialogController mwd = null;

    try
    {
      mwd = MergeWorksDialogController.create("Import Into Existing Work Record", workBibData, bd, null, null, work, creatingNewWork, creatingNewEntry);
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

    if (creatingNewEntry)
    {
      BibEntry entry = db.getBibLibrary().addEntry(mwd.getEntryType());
      work.setBibEntryKey(entry.getEntryKey());
      workBibData = entry;
    }

    mwd.mergeInto(workBibData);
    bibManagerDlg.refresh();

    goToRecord(work, false);
    update();

    if ((filePath != null) && ibed.getDeleteFile())
      filePath.deletePromptOnFail(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}