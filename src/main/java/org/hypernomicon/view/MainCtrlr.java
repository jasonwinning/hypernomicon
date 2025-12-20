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

package org.hypernomicon.view;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.query.GeneralQueries.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.query.ui.QueriesTabCtrlr.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import org.hypernomicon.App;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.BibManager;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.*;
import org.hypernomicon.dialogs.*;
import org.hypernomicon.dialogs.base.NonmodalWindow;
import org.hypernomicon.dialogs.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.fileManager.FileRow;
import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.authors.WorkAuthors;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.previewWindow.ContentsWindow;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.query.QueryType;
import org.hypernomicon.query.ui.*;
import org.hypernomicon.settings.SettingsDlgCtrlr;
import org.hypernomicon.settings.WebButtonSettingsCtrlr;
import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutAction;
import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutContext;
import org.hypernomicon.testTools.TestConsoleDlgCtrlr;
import org.hypernomicon.settings.SettingsDlgCtrlr.SettingsPage;
import org.hypernomicon.tree.*;
import org.hypernomicon.util.*;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;
import org.hypernomicon.view.HyperFavorites.RecordFavorite;
import org.hypernomicon.view.cellValues.*;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.mainText.SymbolPickerDlgCtrlr;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.tabs.*;
import org.hypernomicon.view.wrappers.*;

import java.io.*;
import java.nio.file.*;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.control.textfield.TextFields;

import org.jbibtex.ParseException;
import org.jbibtex.TokenMgrException;

import com.google.common.collect.EnumHashBiMap;

import javafx.scene.input.*;

import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.animation.FadeTransition;
import javafx.animation.PauseTransition;
import javafx.application.Platform;
import javafx.beans.binding.BooleanExpression;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Point2D;
import javafx.geometry.Side;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.stage.*;
import javafx.util.Duration;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

public final class MainCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML Tab tabOmniSelector;
  @FXML private TableView<HyperTableRow> tvFind;
  @FXML private AnchorPane apFindBackground, apGoTo, apListGoTo, midAnchorPane;
  @FXML private Button btnBibMgr, btnDecrement, btnFileMgr, btnIncrement, btnMentions, btnPreviewWindow, btnSave,
                       btnDelete, btnRevert, btnBack, btnForward, btnSaveAll, btnPrevResult, btnNextResult;
  @FXML private CheckMenuItem mnuAutoImport;
  @FXML private ComboBox<HyperTableCell> cbGoTo;
  @FXML private GridPane gpFindTable;
  @FXML private HBox topHBox, bottomToolBar, hbRecord;
  @FXML private ImageView ivDates;
  @FXML private Label lblProgress, lblFindToast;
  @FXML private Menu mnuFolders;
  @FXML private MenuBar menuBar;
  @FXML private MenuItem mnuAddToQueryResults, mnuChangeID, mnuCloseDatabase, mnuExitNoSave, mnuFindNextAll, mnuFindNextInName,
                         mnuFindPreviousAll, mnuFindPreviousInName, mnuFindWithinAnyField, mnuFindWithinName, mnuImportBibClipboard,
                         mnuImportBibFile, mnuNewCountry, mnuNewDatabase, mnuNewField, mnuNewPersonStatus, mnuNewRank, mnuVideos,
                         mnuRecordSelect, mnuRevertToXmlVersion, mnuSaveReloadAll, mnuToggleFavorite, mnuImportWork, mnuImportFile,
                         mnuShortcuts, mnuChangeFieldOrder, mnuChangeRankOrder, mnuChangeCountryOrder, mnuChangePersonStatusOrder,
                         mnuChangeFileTypeOrder, mnuChangeWorkTypeOrder, mnuChangeArgVerdictOrder, mnuChangePosVerdictOrder,
                         mnuChangeInstitutionTypeOrder, mnuTestConsole;

  @FXML private MenuButton mbCreateNew;
  @FXML private ProgressBar progressBar;
  @FXML private SeparatorMenuItem mnuBibImportSeparator;
  @FXML private SplitMenuButton btnGoTo, btnCreateNew;
  @FXML private StackPane lowerStackPane;
  @FXML private Tab tabViewSelector, tabArguments, tabDebates, tabFiles, tabInst, tabNotes, tabPersons, tabPositions, tabQueries, tabTerms, tabTree, tabWorks;
  @FXML private TabPane selectorTabPane, tabPane;
  @FXML private TextField tfID, tfOmniGoTo, tfRecord;
  @FXML private ToggleButton btnPointerPreview, btnTextSearch;
  @FXML private ToolBar topToolBar;

  @FXML public Label lblStatus;
  @FXML public Menu mnuFavorites, mnuQueries;
  @FXML public ToggleButton btnPointerLaunch;

  public final WindowStack windows = new WindowStack();
  public final Map<String, WebButton> webButtonMap = new HashMap<>();
  public final HyperViewSequence viewSequence;
  public final ComboBox<TreeRow> cbTreeGoTo = new ComboBox<>();
  public final TreeSelector treeSelector = new TreeSelector();
  public final Tooltip ttDates;
  public final HyperFavorites favorites;

  private final EnumHashBiMap<TabEnum, Tab> selectorTabs = EnumHashBiMap.create(TabEnum.class);
  private final Stage stage;
  private final OmniFinder omniFinder;
  private final CustomTextField ctfOmniGoTo;
  private final ClickHoldButton chbBack, chbForward;
  private final HyperCB hcbGoTo;
  private final HyperTable htFind;
  private final CreateMenuItems createMenuItems;

  private ComboBox<ResultRow> cbResultGoTo = null;
  private TextField tfSelector = null;

  private boolean selectorTabChangeIsProgrammatic   = false, dontShowOmniTable     = false, maximized    = false,
                  btnTextSearchToggleIsProgrammatic = false, internetNotCheckedYet = true , shuttingDown = false;
  private double toolBarWidth = 0.0, maxWidth = 0.0, maxHeight = 0.0;
  private Instant lastImportTime = Instant.EPOCH;
  private FilePath lastImportFilePath = null;

  private static final String TREE_SELECT_BTN_CAPTION = "Select";

  public static final String AUTOFILL_TOOLTIP = "Try to automatically fill in missing bibliographic information",
                             NO_DATES_TOOLTIP = "No dates to show.";

//---------------------------------------------------------------------------

  public enum OmniSearchMode
  {
    asYouType,
    allFields,
    currentDesc
  }

  private final Property<OmniSearchMode> omniSearchMode = new SimpleObjectProperty<>(OmniSearchMode.asYouType);

//---------------------------------------------------------------------------

  MenuBar getMenuBar()                        { return menuBar; }
  public TreeWrapper tree()                   { return treeHyperTab().getTree(); }
  public Stage getStage()                     { return stage; }
  public boolean isShuttingDown()             { return shuttingDown; }

  @FXML private void mnuExitClick()           { shutDown(ShutDownMode.Normal      ); }
  @FXML private void mnuExitNoSaveClick()     { shutDown(ShutDownMode.NormalNoSave); }
  @FXML private void mnuOpenClick()           { openDB(null); }
  @FXML private void mnuAboutClick()          { new AboutDlgCtrlr().showModal(); }
  @FXML private void mnuChangeFavOrderClick() { new FavOrderDlgCtrlr().showModal(); }
  @FXML private void mnuSettingsClick()       { if (cantSaveRecord() == false) new SettingsDlgCtrlr().showModal(); }
  @FXML private void mnuTestConsoleClick()    { if (cantSaveRecord() == false) new TestConsoleDlgCtrlr().showModal(); }
  @FXML private void btnMentionsClick()       { if (cantSaveRecord() == false) searchForMentions(false); }

  public PersonTabCtrlr   personHyperTab   () { return getHyperTab(personTabEnum  ); }
  public InstTabCtrlr     instHyperTab     () { return getHyperTab(instTabEnum    ); }
  public WorkTabCtrlr     workHyperTab     () { return getHyperTab(workTabEnum    ); }
  public FileTabCtrlr     fileHyperTab     () { return getHyperTab(fileTabEnum    ); }
  public DebateTabCtrlr   debateHyperTab   () { return getHyperTab(debateTabEnum  ); }
  public PositionTabCtrlr positionHyperTab () { return getHyperTab(positionTabEnum); }
  public ArgumentTabCtrlr argumentHyperTab () { return getHyperTab(argumentTabEnum); }
  public NoteTabCtrlr     noteHyperTab     () { return getHyperTab(noteTabEnum    ); }
  public TermTabCtrlr     termHyperTab     () { return getHyperTab(termTabEnum    ); }
  public QueriesTabCtrlr  queryHyperTab    () { return getHyperTab(queryTabEnum   ); }
  public TreeTabCtrlr     treeHyperTab     () { return getHyperTab(treeTabEnum    ); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class CreateMenuItems
  {
    private final List<MenuItem> list1, list2;

    private CreateMenuItems(MenuButton mb, SplitMenuButton smb)
    {
      list1 = mb.getItems();
      list2 = smb.getItems();

      list1.clear();
      list2.clear();
    }

    private void add(MenuItem menuItem)
    {
      list1.add(menuItem);
      list2.add(copyOf(menuItem));
    }

    private void addSeparator()
    {
      list1.add(new SeparatorMenuItem());
      list2.add(new SeparatorMenuItem());
    }

    private void clear()
    {
      SeparatorMenuItem sepItem = (SeparatorMenuItem) findFirst(list1, item -> item instanceof SeparatorMenuItem);

      if (sepItem == null) return;

      int ndx = list1.indexOf(sepItem);

      while (list1.size() > ndx)
      {
        list1.remove(ndx);
        list2.remove(ndx);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void create(Stage stage) throws IOException
  {
    noOp(new MainCtrlr(stage));
  }

  private MainCtrlr(Stage stage) throws IOException
  {
    synchronized(MainCtrlr.class)
    {
      if (ui != null)
        throw new UnsupportedOperationException("MainCtrlr can only be instantiated once.");

      ui = this;
    }

    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/Main.fxml"), null, null, klass -> this);
    Region rootNode = loader.load();

    this.stage = stage;

    if (IS_OS_MAC)
      menuBar.getMenus().forEach(UIUtil::stripMnemonics);

    menuBar.setUseSystemMenuBar(true);

    updateProgress("", -1);

    windows.push(stage);

    stage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((windows.getCyclingFocus() == false) && Boolean.TRUE.equals(newValue))
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

    ttDates = makeTooltip(NO_DATES_TOOLTIP);

    initHyperTabs();

    addSelectorTab(omniTabEnum);
    addSelectorTab(listTabEnum);

    chbBack    = new ClickHoldButton(btnBack   , Side.TOP);
    chbForward = new ClickHoldButton(btnForward, Side.TOP);

    setToolTip(btnBack   , "Click to go back, hold to see history"   );
    setToolTip(btnForward, "Click to go forward, hold to see history");

    chbBack   .setOnAction(event -> btnBackClick   ());
    chbForward.setOnAction(event -> btnForwardClick());

    viewSequence = new HyperViewSequence(tabPane, chbForward, chbBack);

    tabViewSelector.disableProperty().bind(BooleanExpression.booleanExpression(tabQueries.selectedProperty().or(tabTree.selectedProperty())).not());

    tabViewSelector.tooltipProperty().bind(tabPane.getSelectionModel().selectedItemProperty().map(tab -> switch (getHyperTabByTab(tab).getTabEnum())
    {
      case queryTabEnum -> makeTooltip("Populate dropdown with query results");
      case treeTabEnum  -> makeTooltip("Search within Tree (additional actions become available in magnifying glass button dropdown menu)");
      default           -> null;
    }));

    setSelectorTab(selectorTabPane.getTabs().getLast());

    ctfOmniGoTo = (CustomTextField) TextFields.createClearableTextField();
    copyRegionLayout(tfOmniGoTo, ctfOmniGoTo);
    addToParent(ctfOmniGoTo, removeFromParent(tfOmniGoTo));

    hcbGoTo = new HyperCB(cbGoTo, ctEditableUnlimitedDropDown, new RecordByTypePopulator());

    htFind = new HyperTable(tvFind, 1, false, TablePrefKey.FIND); htFind.disableRefreshAfterCellUpdate = true;

    htFind.addIconCol ();
    htFind.addCol     (hdtNone, ctIncremental);
    htFind.addLabelCol(hdtNone, smStandard);
    htFind.addLabelCol(hdtNone, smTextSimple);

    htFind.setOnShowMore(() -> tfOmniGoToChange(ctfOmniGoTo.getText(), true));

    htFind.addDefaultMenuItems();

    omniFinder = new OmniFinder(htFind);

    btnFileMgr.setOnAction(event -> FileManager.show(    ));
    btnBibMgr .setOnAction(event -> BibManager .show(true));

    btnGoTo.setOnAction        (event -> btnGoToClick(false));
    mnuRecordSelect.setOnAction(event -> btnGoToClick(true));

    setToolTip(btnGoTo, this::btnGoToTooltip);

    hcbGoTo.setEnterKeyHandler(this::recordLookup);
    hcbGoTo.dontCreateNewRecord = true;

    mnuImportWork        .setOnAction(event -> importWorkFile(null, null, true));
    mnuImportFile        .setOnAction(event -> importMiscFile(null, null));
    mnuImportBibFile     .setOnAction(event -> importBibFile(null, null));
    mnuImportBibClipboard.setOnAction(event -> importBibFile(convertMultiLineStrToStrList(getClipboardText(false), false), null));

    mnuVideos            .setOnAction(event -> openWebLink("http://hypernomicon.org/support.html"));

    mnuFindNextAll       .setOnAction(event -> tree().find(true,  false));
    mnuFindPreviousAll   .setOnAction(event -> tree().find(false, false));
    mnuFindNextInName    .setOnAction(event -> tree().find(true,  true ));
    mnuFindPreviousInName.setOnAction(event -> tree().find(false, true ));

    btnSaveAll           .setOnAction(event -> saveAllToXML(true, true, false, false));
    btnDelete            .setOnAction(event -> deleteCurrentRecord(true));
    btnRevert            .setOnAction(event -> update());

    btnPrevResult        .setOnAction(event -> previousSearchResult());
    btnNextResult        .setOnAction(event -> nextSearchResult    ());

    btnGoTo      .visibleProperty().bind(btnTextSearch.selectedProperty().not());
    btnPrevResult.visibleProperty().bind(btnTextSearch.selectedProperty()      );
    btnNextResult.visibleProperty().bind(btnTextSearch.selectedProperty()      );

    mnuChangeFieldOrder          .setOnAction(event -> mnuChangeSortOrder(hdtField          ));
    mnuChangeRankOrder           .setOnAction(event -> mnuChangeSortOrder(hdtRank           ));
    mnuChangeCountryOrder        .setOnAction(event -> mnuChangeSortOrder(hdtCountry        ));
    mnuChangePersonStatusOrder   .setOnAction(event -> mnuChangeSortOrder(hdtPersonStatus   ));
    mnuChangeFileTypeOrder       .setOnAction(event -> mnuChangeSortOrder(hdtFileType       ));
    mnuChangeWorkTypeOrder       .setOnAction(event -> mnuChangeSortOrder(hdtWorkType       ));
    mnuChangeArgVerdictOrder     .setOnAction(event -> mnuChangeSortOrder(hdtArgumentVerdict));
    mnuChangePosVerdictOrder     .setOnAction(event -> mnuChangeSortOrder(hdtPositionVerdict));
    mnuChangeInstitutionTypeOrder.setOnAction(event -> mnuChangeSortOrder(hdtInstitutionType));

    if (app.prefs.getBoolean(PrefKey.RIGHT_CLICK_TO_LAUNCH, true))
      btnPointerLaunch.setSelected(true);
    else
      btnPointerPreview.setSelected(true);

    btnPointerLaunch.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        app.prefs.putBoolean(PrefKey.RIGHT_CLICK_TO_LAUNCH, true);
    });

    btnPointerPreview.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        app.prefs.putBoolean(PrefKey.RIGHT_CLICK_TO_LAUNCH, false);
    });

    forceToggleSelection(btnPointerLaunch.getToggleGroup());

    setToolTip(btnPointerLaunch , "On right/secondary click on link to work record, launch work file");
    setToolTip(btnPointerPreview, "On right/secondary click on link to work record, show in preview window");

    setToolTip(btnMentions, "Show records whose description mentions this record");

    btnIncrement.setOnAction(event -> incDecClick(true));
    btnDecrement.setOnAction(event -> incDecClick(false));

    setToolTip(btnPrevResult    , "Previous match");
    setToolTip(btnNextResult    , "Next match");
    setToolTip(btnTextSearch    , "Search within description of record currently showing (" + (IS_OS_MAC ? "Cmd" : "Ctrl") + "-Shift-F)");
    setToolTip(btnPreviewWindow , "View selected record/file in Preview Window");
    setToolTip(btnBibMgr        , "Open Bibliography Manager Window");
    setToolTip(btnFileMgr       , "Open File Manager Window");
    setToolTip(btnSaveAll       , "Save all records to XML files (" + (IS_OS_MAC ? "Cmd" : "Ctrl") + "-S)");

    btnSaveAll.setText(underlinedChar('S') + "ave to XML");

    apFindBackground.setOnMousePressed(event -> hideFindTable());

    ttDates.setAutoHide(true);

    tabTree        .setGraphic(imgViewFromRelPath("resources/images/treeview-small.png"));
    tabQueries     .setGraphic(imgViewFromRelPath("resources/images/glasses-db.png"));
    tabOmniSelector.setGraphic(imgViewFromRelPath("resources/images/globe.png"));
    tabViewSelector.setGraphic(imgViewFromRelPath("resources/images/details.png"));

    favorites = new HyperFavorites(mnuFavorites, mnuQueries);

    setToolTip(tabOmniSelector, "Search all records");

    forEachHyperTab(hyperTab ->
    {
      TabEnum hyperTabEnum = hyperTab.getTabEnum();
      RecordType recordType = getRecordTypeByTabEnum(hyperTabEnum);

      nullSwitch(imgViewForRecord(null, recordType), graphic ->
      {
        hyperTab.getTab().setGraphic(graphic);

        nullSwitch(selectorTabs.get(hyperTabEnum), selectorTab ->
        {
          selectorTab.setGraphic(imgViewForRecord(null, recordType));
          setToolTip(selectorTab, "Search " + getTypeName(recordType) + " records");
        });
      });
    });

    WebButtonSettingsCtrlr.loadPrefs();

    if (IS_OS_MAC)
    {
      topHBox.getChildren().remove(topToolBar);
      setHeights(topHBox, 0.0);
      midAnchorPane.getChildren().add(topToolBar);
      AnchorPane.setTopAnchor(topToolBar, 0.0);
      AnchorPane.setRightAnchor(topToolBar, 0.0);

      midAnchorPane.widthProperty().addListener((ob, oldValue, newValue) ->
      {
        if ((newValue != null) && (newValue.doubleValue() > 1))
          adjustToolBar(newValue.doubleValue());
      });
    }

    createMenuItems = new CreateMenuItems(mbCreateNew, btnCreateNew);

    repositionPopupListWorkaround(btnCreateNew);
    repositionPopupListWorkaround(mbCreateNew);

    mbCreateNew.disableProperty().bind(btnCreateNew.disabledProperty());
    mbCreateNew.visibleProperty().bind(btnCreateNew.visibleProperty().not());

    setToolTip(mbCreateNew, "Create a new record");

    tabPane.getTabs().forEach(tab ->
    {
      RecordType type = getRecordTypeByTabEnum(getHyperTabByTab(tab).getTabEnum());
      if (type == hdtNone) return;

      MenuItem menuItem = new MenuItem(getTypeName(type));
      menuItem.setOnAction(event -> createNew(type));
      createMenuItems.add(menuItem);
    });

    Scene scene = new Scene(rootNode);

    scene.getStylesheets().add(App.class.getResource("resources/css.css").toExternalForm());

    stage.setScene(scene);

    initInputHandlers();

    stage.getIcons().addAll(Stream.of("16x16", "32x32", "48x48", "64x64", "128x128", "256x256")
                                  .map(str -> new Image(App.class.getResourceAsStream("resources/images/logo-" + str + ".png")))
                                  .toList());
    hideFindTable();

    tfRecord.setText("");
    tfID.setText("");

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
      omniSearchMode.setValue(OmniSearchMode.asYouType);
    });

//---------------------------------------------------------------------------

    addOmniSearchModeListener();

    btnTextSearch.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (btnTextSearchToggleIsProgrammatic || (newValue == null) || (newValue.equals(oldValue))) return;

      if (Boolean.TRUE.equals(newValue))
        omniSearchMode.setValue(OmniSearchMode.currentDesc);
      else
        omniSearchMode.setValue(OmniSearchMode.asYouType);
    });

//---------------------------------------------------------------------------

    ctfOmniGoTo.setOnMouseClicked(event ->
    {
      if ((omniSearchMode.getValue() == OmniSearchMode.asYouType) && (htFind.dataRowCount() > 0))
        showFindTable();
    });

//---------------------------------------------------------------------------

    ctfOmniGoTo.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue != null) && (newValue.equals(oldValue) == false))
        tfOmniGoToChange(newValue, false);
    });

//---------------------------------------------------------------------------

    ctfOmniGoTo.setOnAction(event ->
    {
      if (omniSearchMode.getValue() == OmniSearchMode.currentDesc)
        findWithinDesc();
      else
        recordLookup();
    });

//---------------------------------------------------------------------------

    ctfOmniGoTo.addEventHandler(KeyEvent.KEY_PRESSED, event ->
    {
      switch (event.getCode())
      {
        case DOWN : case UP : case PAGE_DOWN : case PAGE_UP :

          if (omniSearchMode.getValue() == OmniSearchMode.asYouType)
          {
            showFindTable();
            tvFind.fireEvent(event.copyFor(tvFind, tvFind));
            event.consume();
          }

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
        updateBottomPanel(true, false);
    });

//---------------------------------------------------------------------------

    tfID.focusedProperty().addListener((ob, oldValue, newValue) ->
      tfID.setText((Boolean.TRUE.equals(newValue) == false) && (activeRecord() != null) ?
        String.valueOf(activeRecord().getID())
      :
        ""));

//---------------------------------------------------------------------------

    tfRecord.setOnAction(event ->
    {
      if ((activeTabEnum() == treeTabEnum) || (activeTabEnum() == queryTabEnum)) return;

      HDT_Record record = activeRecord();
      if (record == null)
      {
        tfRecord.setText("");
        return;
      }

      RecordType type = activeType();
      int newRecordNdx = parseInt(tfRecord.getText(), 0) - 1;

      if ((newRecordNdx != record.keyNdx()) && (newRecordNdx >= 0) && (newRecordNdx < db.records(type).size()))
        goToRecord(db.records(type).getByKeyNdx(newRecordNdx), true);
      else
        tfRecord.setText("");
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

    btnPreviewWindow.setOnAction(event ->
    {
      PreviewSource src = determinePreviewContext();

      if (activeTabEnum() == fileTabEnum)
      {
        HDT_MiscFile miscFile = (HDT_MiscFile) activeRecord();

        if (miscFile == null)
          PreviewWindow.clearPreview(src);
        else
          PreviewWindow.setPreview(src, miscFile);
      }

      PreviewWindow.show(src);
    });

//---------------------------------------------------------------------------

    mnuFindWithinName.setOnAction(event ->
    {
      if (selectorTabEnum() == omniTabEnum)
        showSearch(true, qtAllRecords, QUERY_WITH_NAME_CONTAINING, null, new GenericNonRecordHTC(tfSelector.getText(), hdtNone), null, tfSelector.getText());
      else
        mnuFindWithinNameClick();
    });

//---------------------------------------------------------------------------

    mnuFindWithinAnyField.setOnAction(event ->
    {
      if (selectorTabEnum() == omniTabEnum)
        showSearch(true, qtAllRecords, QUERY_ANY_FIELD_CONTAINS, null, new GenericNonRecordHTC(tfSelector.getText(), hdtNone), null, tfSelector.getText());
      else
        showSearch(true, fromRecordType(selectorType()), QUERY_ANY_FIELD_CONTAINS, null, new GenericNonRecordHTC(tfSelector.getText(), hdtNone), null, tfSelector.getText());
    });

//---------------------------------------------------------------------------

    mnuAutoImport.setSelected(app.prefs.getBoolean(PrefKey.AUTO_IMPORT, true));
    mnuAutoImport.setOnAction(event -> app.prefs.putBoolean(PrefKey.AUTO_IMPORT, mnuAutoImport.isSelected()));

    setAllVisible(app.debugging, mnuChangeID, mnuSaveReloadAll, mnuTestConsole);

//---------------------------------------------------------------------------

    stage.setOnCloseRequest(event ->
    {
      shutDown(ShutDownMode.Normal);
      event.consume();
    });

//---------------------------------------------------------------------------

    db.addDeleteHandler(record ->
    {
      // When changing this handler, changes may need to be made to
      // mnuChangeIDClick() as well

      if ((record.getType() == hdtPerson) && (personHyperTab().activeRecord() == record))
        personHyperTab().assignPicture(null, false);  // User has already been asked if they want to delete the picture; don't ask again

      queryHyperTab().removeRecord(record);

      int ndx = favorites.indexOfRecord(record);

      if (ndx > -1)
      {
        mnuFavorites.getItems().remove(ndx);
        updateFavorites();
      }

      htFind.removeRowsIf(row -> row.getRecord() == record);
      viewSequence.removeRecord(record);
    });

//---------------------------------------------------------------------------

    double  x             = app.prefs.getDouble (PrefKey.WINDOW_X,          stage.getX()),
            y             = app.prefs.getDouble (PrefKey.WINDOW_Y,          stage.getY()),
            width         = app.prefs.getDouble (PrefKey.WINDOW_WIDTH,      rootNode.getPrefWidth()),  // stage.getWidth and stage.getHeight are not the
            height        = app.prefs.getDouble (PrefKey.WINDOW_HEIGHT,     rootNode.getPrefHeight()); // correct values in some Linux environments
    boolean fullScreen    = app.prefs.getBoolean(PrefKey.WINDOW_FULLSCREEN, stage.isFullScreen()),
            maximizedPref = app.prefs.getBoolean(PrefKey.WINDOW_MAXIMIZED,  stage.isMaximized());

    stage.setX(x); // set X and Y first so that window gets full-screened or
    stage.setY(y); // maximized onto the correct monitor if there are more than one

    if      (fullScreen)    stage.setFullScreen(true);
    else if (maximizedPref) stage.setMaximized(true);
    else
    {
      stage.setWidth(width);
      stage.setHeight(height);

      ensureVisible(stage, rootNode.getPrefWidth(), rootNode.getPrefHeight());
    }

    setFontSize(rootNode);

    stage.show();

    scaleNodeForDPI(rootNode);
    MainTextWrapper.rescale();
    forEachHyperTab(HyperTab::rescale);

    forEachHyperTab(HyperTab::setDividerPositions);

    lockHeightTo(tfRecord, btnBack, btnForward, btnIncrement, btnDecrement, btnTextSearch, btnGoTo, btnPrevResult, btnNextResult,
                 cbGoTo, ctfOmniGoTo, btnSave, btnRevert, btnDelete, btnCreateNew, mbCreateNew, apGoTo, apListGoTo, (Region)ctfOmniGoTo.getParent());

    ctfOmniGoTo.widthProperty().addListener((obs, ov, nv) ->
      AnchorPane.setRightAnchor(gpFindTable, apFindBackground.getWidth() - (selectorTabPane.getLayoutX() + nv.doubleValue())));

    noOp(SequentialLayoutWrapper.forPane(bottomToolBar));

    selectorTabPane.layoutXProperty().addListener((obs, ov, nv) -> AnchorPane.setLeftAnchor(lblFindToast, nv.doubleValue()));
    btnTextSearch  .layoutXProperty().addListener((obs, ov, nv) -> AnchorPane.setLeftAnchor(gpFindTable , nv.doubleValue()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initInputHandlers()
  {
    registerShortcuts();

    app.shortcuts.addListener((obs, ov, nv) -> registerShortcuts());

//---------------------------------------------------------------------------

    // Override CTRL-H for textfields and text areas, which for some reason is mapped to act like Backspace

    if (IS_OS_MAC == false) stage.addEventFilter(KeyEvent.KEY_PRESSED, event ->
    {
      if ((event.getCode() == KeyCode.H) && event.isShortcutDown())
      {
        if (db.isOnline()) chbBack.showMenu();
        event.consume();
      }
    });

//---------------------------------------------------------------------------

    stage.addEventFilter(MouseEvent.MOUSE_CLICKED, event ->
    {
      if      (event.getButton() == MouseButton.BACK   ) Platform.runLater(this::btnBackClick   );
      else if (event.getButton() == MouseButton.FORWARD) Platform.runLater(this::btnForwardClick);
      else                                               return;

      event.consume();
    });

//---------------------------------------------------------------------------

    stage.addEventFilter(DragEvent.DRAG_OVER, event ->
    {
      if (event.getDragboard().hasContent(HYPERNOMICON_DATA_FORMAT))
        return;

      if (event.getDragboard().hasFiles())
        event.acceptTransferModes(TransferMode.COPY, TransferMode.MOVE);

      event.consume();
    });

//---------------------------------------------------------------------------

    stage.addEventFilter(DragEvent.DRAG_DROPPED, event ->
    {
      Dragboard board = event.getDragboard();

      if (board.hasContent(HYPERNOMICON_DATA_FORMAT))
        return;

      if (board.hasImage() && app.debugging)
        System.out.println("has image");

      if (board.hasFiles())
      {
        if (workHyperTab().processDragEvent(event))
        {
          event.setDropCompleted(true);
          return;
        }

        List<String> args = board.getFiles().stream().map(File::getAbsolutePath).toList();
        Platform.runLater(() -> handleArgs(args));
        event.setDropCompleted(true);
      }

      event.consume();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void registerShortcuts()
  {
    Scene scene = stage.getScene();

    // Clear old accelerators first

    scene.getAccelerators().clear();

  //---------------------------------------------------------------------------

    // Hard-coded shortcuts

    scene.getAccelerators().putAll(Map.of
    (
      new KeyCodeCombination(KeyCode.S, KeyCombination.SHORTCUT_DOWN), () ->
      {
        if (db.isOnline())
          saveAllToXML(true, true, false, false);
      },

      new KeyCodeCombination(KeyCode.S, KeyCombination.SHORTCUT_DOWN, KeyCombination.SHIFT_DOWN), () ->
      {
        if (db.isOnline() && db.bibLibraryIsLinked())
          BibManager.syncAndSaveDB();
      },

      new KeyCodeCombination(KeyCode.ESCAPE), this::hideFindTable,

      new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN, KeyCombination.SHIFT_DOWN), () ->
      {
        if (db.isOnline())
          omniFocus(OmniSearchMode.currentDesc, true);
      },

      new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN), () ->
      {
        if (db.isOnline())
        {
          if ((omniSearchMode.getValue() == OmniSearchMode.asYouType) && ctfOmniGoTo.isFocused())
            omniFocus(OmniSearchMode.allFields, true);
          else
            omniFocus(OmniSearchMode.asYouType, true);
        }
      }
    ));

    scene.getAccelerators().putAll(IS_OS_MAC ? Map.of
    (
      new KeyCodeCombination(KeyCode.Y            , KeyCombination.SHORTCUT_DOWN                           ), () -> { if (db.isOnline()) chbBack.showMenu(); },
      new KeyCodeCombination(KeyCode.G            , KeyCombination.SHORTCUT_DOWN                           ), this::nextSearchResult,
      new KeyCodeCombination(KeyCode.G            , KeyCombination.SHORTCUT_DOWN, KeyCombination.SHIFT_DOWN), this::previousSearchResult,
      new KeyCodeCombination(KeyCode.OPEN_BRACKET , KeyCombination.SHORTCUT_DOWN                           ), () -> Platform.runLater(this::btnBackClick),
      new KeyCodeCombination(KeyCode.CLOSE_BRACKET, KeyCombination.SHORTCUT_DOWN                           ), () -> Platform.runLater(this::btnForwardClick)
    )
    : Map.of
    (
      new KeyCodeCombination(KeyCode.F3                              ), this::nextSearchResult,
      new KeyCodeCombination(KeyCode.F3   , KeyCombination.SHIFT_DOWN), this::previousSearchResult,
      new KeyCodeCombination(KeyCode.LEFT , KeyCombination.ALT_DOWN  ), () -> Platform.runLater(this::btnBackClick),
      new KeyCodeCombination(KeyCode.RIGHT, KeyCombination.ALT_DOWN  ), () -> Platform.runLater(this::btnForwardClick)
    ));

  //---------------------------------------------------------------------------

    // User-defined shortcuts

    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.PreviewRecord, () -> { if (btnPreviewWindow.isDisabled() == false) btnPreviewWindow.fire(); });

    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.ShowMentions, () -> { if (btnMentions.isDisabled() == false) btnMentions.fire(); });

    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewRecord, () ->
    {
      if (db.isOnline() && (activeTabEnum() != treeTabEnum) && (activeTabEnum() != queryTabEnum))
        createNew(activeType());
    });

    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewPerson     , () -> { if (db.isOnline()) createNew(hdtPerson     ); });
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewInstitution, () -> { if (db.isOnline()) createNew(hdtInstitution); });
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewWork       , () -> { if (db.isOnline()) createNew(hdtWork       ); });
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewMiscFile   , () -> { if (db.isOnline()) createNew(hdtMiscFile   ); });
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewDebate     , () -> { if (db.isOnline()) createNew(hdtDebate     ); });
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewPosition   , () -> { if (db.isOnline()) createNew(hdtPosition   ); });
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewArgument   , () -> { if (db.isOnline()) createNew(hdtArgument   ); });
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewNote       , () -> { if (db.isOnline()) createNew(hdtNote       ); });
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.CreateNewTerm       , () -> { if (db.isOnline()) createNew(hdtTerm       ); });

    assignShortcut(ShortcutContext.PersonsTab, ShortcutAction.CreateNewInvestigation, () ->
    {
      if (db.isOnline() && (activeTabEnum() == personTabEnum) && (activeRecord() != null))
        personHyperTab().newInvestigation();
    });

    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToFileManager  , () -> { if (db.isOnline()) FileManager.show(); });
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToPreviewWindow, () -> { if (db.isOnline()) PreviewWindow.show(); });
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToBibManager   , () -> { if (db.isOnline() && db.bibLibraryIsLinked()) BibManager.show(true); });

    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToPersonsTab     , () -> tabPane.getSelectionModel().select(tabPersons  ));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToInstitutionsTab, () -> tabPane.getSelectionModel().select(tabInst     ));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToWorksTab       , () -> tabPane.getSelectionModel().select(tabWorks    ));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToMiscFilesTab   , () -> tabPane.getSelectionModel().select(tabFiles    ));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToDebatesTab     , () -> tabPane.getSelectionModel().select(tabDebates  ));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToPositionsTab   , () -> tabPane.getSelectionModel().select(tabPositions));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToArgumentsTab   , () -> tabPane.getSelectionModel().select(tabArguments));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToNotesTab       , () -> tabPane.getSelectionModel().select(tabNotes    ));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToTermsTab       , () -> tabPane.getSelectionModel().select(tabTerms    ));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToQueriesTab     , () -> tabPane.getSelectionModel().select(tabQueries  ));
    assignShortcut(ShortcutContext.MainWindow, ShortcutAction.GoToTreeTab        , () -> tabPane.getSelectionModel().select(tabTree     ));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void assignShortcut(ShortcutContext context, ShortcutAction action, Runnable handler)
  {
    NonmodalWindow.assignShortcut(stage, context, action, handler);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void findWithinDesc()
  {
    HyperTab<? extends HDT_Record, ? extends HDT_Record> hyperTab = activeTab();
    if (hyperTab != null) hyperTab.findWithinDesc();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void previousSearchResult()
  {
    if (db.isOffline()) return;

    HyperTab<? extends HDT_Record, ? extends HDT_Record> hyperTab = activeTab();

    if (hyperTab != null)
    {
      hyperTab.previousSearchResult();

      if (ctfOmniGoTo.isFocused() == false)
        safeFocus(btnPrevResult);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void nextSearchResult()
  {
    if (db.isOffline()) return;

    HyperTab<? extends HDT_Record, ? extends HDT_Record> hyperTab = activeTab();

    if (hyperTab != null)
    {
      hyperTab.nextSearchResult();

      if (ctfOmniGoTo.isFocused() == false)
        safeFocus(btnNextResult);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  private void initHyperTabs() throws IOException
  {
    new PersonTabCtrlr  (tabPersons  );
    new InstTabCtrlr    (tabInst     );
    new WorkTabCtrlr    (tabWorks    );
    new FileTabCtrlr    (tabFiles    );
    new DebateTabCtrlr  (tabDebates  );
    new PositionTabCtrlr(tabPositions);
    new ArgumentTabCtrlr(tabArguments);
    new NoteTabCtrlr    (tabNotes    );
    new TermTabCtrlr    (tabTerms    );
    new QueriesTabCtrlr (tabQueries  );
    new TreeTabCtrlr    (tabTree     );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void findInDescription(String text)
  {
    omniSearchMode.setValue(OmniSearchMode.currentDesc);
    ctfOmniGoTo.setText(text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String currentFindInDescriptionText()
  {
    return omniSearchMode.getValue() == OmniSearchMode.currentDesc ? ctfOmniGoTo.getText() : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML public void mnuShortcutsClick()
  {
    new HelpDlgCtrlr().showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuChangeIDClick()
  {
    // When changing this function, changes may need to be made to
    // the delete handler as well

    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    if (cantSaveRecord()) return;

    ChangeIDDlgCtrlr ctrlr = new ChangeIDDlgCtrlr();

    if (ctrlr.showModal() == false) return;

    RecordType changedType = ctrlr.hcbRecord.selectedType();
    int oldID = parseInt(ctrlr.tfOldID.getText(), -100),
        newID = parseInt(ctrlr.tfNewID.getText(), -1);

    queryHyperTab().refreshTables();
    htFind.changeIDs(changedType, oldID, newID);

    db.rebuildMentions();

    favorites.changeRecordID(changedType, oldID, newID);

    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void mnuChangeSortOrder(RecordType recordType)
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    if (cantSaveRecord()) return;

    SortOrderDlgCtrlr ctrlr = new SortOrderDlgCtrlr(recordType);

    if (ctrlr.showModal() == false) return;

    update();
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
    if (stage.isFocused())
    {
      return switch (activeTabEnum())
      {
        case personTabEnum -> pvsPersonTab;
        case workTabEnum   -> pvsWorkTab;
        case queryTabEnum  -> pvsQueriesTab;
        case treeTabEnum   -> pvsTreeTab;
        default            -> pvsOther;
      };
    }

    return FileManager.isFocused() ? pvsManager : pvsOther;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnForwardClick()
  {
    if (btnForward.isDisabled() || cantSaveRecord()) return;

    viewSequence.stepForward();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnBackClick()
  {
    if (btnBack.isDisabled() || cantSaveRecord()) return;

    viewSequence.stepBack();
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

  private String btnGoToTooltip()
  {
    if (selectorTabEnum() != listTabEnum)
    {
      return "Go to selected record or find " + (selectorTabEnum() == omniTabEnum ?
        "records with matching text in any field"
      :
        getTypeName(selectorType()) + " records with matching text");
    }

    return switch (activeTabEnum())
    {
      case queryTabEnum -> "Go record selected in query results";
      case treeTabEnum  -> "Go to next/previous match, or go to selected record";
      default           -> "In this context, this button has no effect.";
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnGoToClick(boolean fromMenu)
  {
    if (selectorTabEnum() != listTabEnum)
    {
      recordLookup();
      return;
    }

    TabEnum tabEnum = activeTabEnum();

    if (tabEnum == queryTabEnum)
      goToRecord(queryHyperTab().activeRecord(), false);

    if (tabEnum != treeTabEnum) return;

    if (fromMenu)
    {
      goToRecord(tree().selectedRecord(), false);
      return;
    }

    tree().findAgain();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void mnuFindWithinNameClick()
  {
    RecordType type = selectorType();
    String query = tfSelector.getText();
    boolean backClick = activeTabEnum() != queryTabEnum;

    lblStatus.setText("");

    if (!showSearch(true, fromRecordType(type), QUERY_WITH_NAME_CONTAINING, null, new GenericNonRecordHTC(query, hdtNone), null, query))
    {
      discardLastQuery(backClick);
      return;
    }

    // The following is done in a runLater because the results table gets populated in a runLater; this runLater needs
    // to run after that one

    Platform.runLater(() ->
    {
      List<ResultRow> resultList = queryHyperTab().results();
      int num = resultList.size();

      if (num == 1)
        goToRecord(resultList.getFirst().getRecord(), false);
      else if (num == 0)
      {
        discardLastQuery(backClick);
        lblStatus.setText("No results: searched " + getTypeName(type) + " records for \"" + query + '"');
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

  public enum ShutDownMode
  {
    Normal,                    // User interactively selects Save to XML menu option
    NormalNoSave,              // User interactively selects option to exit without saving
    FromOtherComputer,         // Request to shut down received from a different computer
    InitializationFailure,     // Shutting down because an error occurred during application initialization
    UnrecoverableInternalError // Shutting down because of an unrecoverable internal error
  }

  public void shutDown(ShutDownMode shutDownMode)
  {
    if (db.getState() == DBState.UNRECOVERABLE_ERROR)
      shutDownMode = ShutDownMode.UnrecoverableInternalError;

    boolean popupRobotWasActive = PopupRobot.isActive();

    if (db.getState() != DBState.CLOSED)
    {
      if (shutDownMode == ShutDownMode.NormalNoSave)
      {
        if (confirmDialog("Abandon changes and quit?", false) == false)
          return;
      }
      else if ((shutDownMode == ShutDownMode.Normal) || (shutDownMode == ShutDownMode.FromOtherComputer))
      {
        if (shutDownMode == ShutDownMode.FromOtherComputer)
        {
          shuttingDown = true;
          PopupRobot.setActive(true); // Don't show popup messages while saving current record; just decline confirmations and fail
        }

        if (cantSaveRecord(shutDownMode == ShutDownMode.Normal) && (shutDownMode == ShutDownMode.Normal))
          if (!confirmDialog("Unable to accept most recent changes to this record; however, all other data will be saved. Continue exiting?", false))
            return;

        PopupRobot.setActive(popupRobotWasActive);

        if ((shuttingDown == false) && app.prefs.getBoolean(PrefKey.CHECK_INTERNET, true) && (checkInternet() == false))
          return;

        if (saveAllToXML(false, false, false, true) == false)
        {
          shuttingDown = false;
          return;
        }
      }
      else if (shutDownMode == ShutDownMode.UnrecoverableInternalError)
        PopupRobot.setActive(true);

      /* ********************************************** */
      /*                                                */
      /*               Point of no return               */
      /*                                                */
      /* ********************************************** */

      shuttingDown = true;
      forEachHyperTab(hyperTab -> hyperTab.clear(true));

      folderTreeWatcher.stop();

      if ((shutDownMode != ShutDownMode.UnrecoverableInternalError) && (shutDownMode != ShutDownMode.InitializationFailure))
      {
        try { db.close(null); }
        catch (HDB_UnrecoverableInternalError e)
        {
          errorPopup(e);
        }
      }
    }

    closeWindows(true);

    if (shutDownMode != ShutDownMode.InitializationFailure)
    {
      forEachHyperTab(HyperTab::getDividerPositions);

      boolean iconified = stage.isIconified(), fullScreen = stage.isFullScreen(),
              maximizedPrefVal = IS_OS_MAC ? this.maximized : stage.isMaximized(); // stage.maximized is never changed from true to false on Mac OS. JDK-8087618

      if (fullScreen || maximizedPrefVal) iconified = false; // This has to be done due to bug JDK-8087997

      app.prefs.putDouble (PrefKey.WINDOW_X         , stage.getX());
      app.prefs.putDouble (PrefKey.WINDOW_Y         , stage.getY());
      app.prefs.putDouble (PrefKey.WINDOW_WIDTH     , stage.getWidth());
      app.prefs.putDouble (PrefKey.WINDOW_HEIGHT    , stage.getHeight());
      app.prefs.putBoolean(PrefKey.WINDOW_ICONIFIED , iconified);
      app.prefs.putBoolean(PrefKey.WINDOW_FULLSCREEN, fullScreen);
      app.prefs.putBoolean(PrefKey.WINDOW_MAXIMIZED , maximizedPrefVal);

      HyperTable.saveColWidthsToPrefs();
    }

    InterProcClient.removeThisInstanceFromInstancesTempFile();

    if (jxBrowserInitialized)
      Platform.runLater(PreviewWindow::cleanup); // This eventually closes the application main window
    else
    {
      stage.close();

      if (Environment.isMac())
        Platform.exit();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Similar to PDFJSWrapper.disable

  private void closeWindows(boolean exitingApp)
  {
    if (ctfOmniGoTo != null)
      ctfOmniGoTo.clear();

    clearOmniFinder();

    FileManager         .close(exitingApp);
    BibManager          .close(exitingApp);
    ContentsWindow      .close(exitingApp);
    SymbolPickerDlgCtrlr.close(exitingApp);

    if ((exitingApp == false) || (Environment.isMac() == false))
      PreviewWindow.close(exitingApp);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void enableControls(boolean enabled)
  {
    boolean disabled = !enabled;

    bottomToolBar.getChildren().stream().filter(node -> node.disableProperty().isBound() == false).forEach(node -> node.setDisable(disabled));

    forEachHyperTab(hyperTab -> hyperTab.enable(enabled));

    enableAllIff(enabled, mnuCloseDatabase,      mnuImportWork,     mnuImportFile,         mnuExitNoSave, mnuChangeID,      mnuChangeFieldOrder.getParentMenu(),
                          mnuImportBibClipboard, mnuImportBibFile,  mnuRevertToXmlVersion, btnFileMgr,    btnBibMgr,        mnuNewRank.getParentMenu(),
                          btnPreviewWindow,      btnMentions,       mnuAddToQueryResults,  btnSaveAll,    mnuSaveReloadAll);
    if (disabled)
      tree().clear();

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

    limitRowsToMeasureWidthInCB(cbResultGoTo);
    copyRegionLayout(cbGoTo, cbResultGoTo);

    noOp(new AutoCompleteCBHelper(cbResultGoTo, true, ResultRow::new));

    queryHyperTab().setCB(cbResultGoTo);

    cbResultGoTo.setConverter(new StringConverter<>()
    {
      @Override public String toString(ResultRow row)
      {
        return nullSwitch(row, "", ResultRow::getCBText);
      }

      @Override public ResultRow fromString(String string)
      {
        return cbResultGoTo.getItems() == null ?
          new ResultRow("")
        :
          nullSwitch(findFirst(cbResultGoTo.getItems(), row -> string.equals(row.getCBText())), new ResultRow(string));
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateDatesTooltip(HDT_Record record)
  {
    if (record == null)
    {
      ttDates.setText(NO_DATES_TOOLTIP);
      return;
    }

    try
    {
      ttDates.setText("Created: "    + dateTimeToUserReadableStr(record.getCreationDate()) +
                      "\nModified: " + dateTimeToUserReadableStr(record.getModifiedDate()) +
                      "\nAccessed: " + dateTimeToUserReadableStr(record.getViewDate()));
    }
    catch (Exception e)
    {
      ttDates.setText(NO_DATES_TOOLTIP);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   *
   * @param saveRecord Whether to try to save the current record first
   * @param restartWatcher Whether to restart the folder tree watcher if it was running
   * @param didSyncBib Whether bib mgr was just synced; will update what is showing in the current tab
   * @param confirmRefMgrSecretsSaved Whether to confirm that reference manager secrets have been saved; should be done when closing the database
   * @return True if the data was actually saved to XML; false otherwise
   */
  public boolean saveAllToXML(boolean saveRecord, boolean restartWatcher, boolean didSyncBib, boolean confirmRefMgrSecretsSaved)
  {
    try
    {
      if (db.isLoaded() == false)
        return falseWithErrorPopup("No database is currently loaded.");

      if (saveRecord && cantSaveRecord()) return false;

      if (confirmRefMgrSecretsSaved && db.bibLibraryIsLinked() && db.getBibLibrary().secretsStillNeedToBeSavedToKeyring())
      {
        // This will probably only be relevant for Linux because the Windows and Mac KeyringProviders
        // don't have a programmatic way of signaling whether a failure occurred besides logging a
        // warning message.

        DialogResult result = mrRetry;

        while ((result == mrRetry) && (db.getBibLibrary().saveSecretsToKeyringIfUnsaved() == false))
        {
          String msg = "Warning: Previous attempt(s) to save " + db.bibLibraryUserFriendlyName() + " configuration failed.\n" +
                       "You may need to re-establish account access next time if it is not saved.";

          result = abortRetryIgnoreDialog(msg);

          if (result == mrAbort) return false;
        }
      }

      db.prefs.putInt(RecordIDPrefKey.PERSON       , HDT_Record.getIDSafe(personHyperTab  ().activeRecord       ()));
      db.prefs.putInt(RecordIDPrefKey.INVESTIGATION, HDT_Record.getIDSafe(personHyperTab  ().getCurInvestigation()));
      db.prefs.putInt(RecordIDPrefKey.INSTITUTION  , HDT_Record.getIDSafe(instHyperTab    ().activeRecord       ()));
      db.prefs.putInt(RecordIDPrefKey.DEBATE       , HDT_Record.getIDSafe(debateHyperTab  ().activeRecord       ()));
      db.prefs.putInt(RecordIDPrefKey.POSITION     , HDT_Record.getIDSafe(positionHyperTab().activeRecord       ()));
      db.prefs.putInt(RecordIDPrefKey.ARGUMENT     , HDT_Record.getIDSafe(argumentHyperTab().activeRecord       ()));
      db.prefs.putInt(RecordIDPrefKey.WORK         , HDT_Record.getIDSafe(workHyperTab    ().activeRecord       ()));
      db.prefs.putInt(RecordIDPrefKey.CONCEPT      , HDT_Record.getIDSafe(termHyperTab    ().viewRecord         ()));
      db.prefs.putInt(RecordIDPrefKey.FILE         , HDT_Record.getIDSafe(fileHyperTab    ().activeRecord       ()));
      db.prefs.putInt(RecordIDPrefKey.NOTE         , HDT_Record.getIDSafe(noteHyperTab    ().activeRecord       ()));

      RecordType startingTypeForNextTime = viewSequence.lastActiveRecordType();

      if (startingTypeForNextTime == hdtNone)
        startingTypeForNextTime = hdtPerson;

      db.prefs.put(PrefKey.RECORD_TYPE, Tag.getTypeTagStr(startingTypeForNextTime));

      boolean watcherWasRunning = folderTreeWatcher.stop();

      boolean rv = db.saveAllToPersistentStorage(favorites);

      if (restartWatcher && watcherWasRunning)
        folderTreeWatcher.createNewWatcherAndStart();

      if (didSyncBib)
      {
        update();

        updateSavedStatus(rv, true);
      }
      else if (rv)
        updateSavedStatus(true, false);

      return rv;
    }
    catch (Throwable e)
    {
      logThrowable(e);
      errorPopup("An error occurred while saving to XML files: " + getThrowableMessage(e));
      return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateSavedStatus(boolean savedToXML, boolean syncedToRefMgr)
  {
    String timeStr = timeToUserReadableStr(LocalDateTime.now());

    if (savedToXML)
    {
      if (syncedToRefMgr)
        lblStatus.setText("Last saved to XML files and synced to " + db.bibLibraryUserFriendlyName() + ": " + timeStr);
      else
        lblStatus.setText("Last saved to XML files: " + timeStr);

      return;
    }

    if (syncedToRefMgr)
      lblStatus.setText("Last synced to " + db.bibLibraryUserFriendlyName() + ": " + timeStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuSaveReloadAllClick()
  {
    if (saveAllToXML(true, false, false, true) == false) return;

    app.prefs.put(PrefKey.SOURCE_FILENAME, db.getHdbPath().getNameOnly().toString());
    app.prefs.put(PrefKey.SOURCE_PATH, db.getRootPath().toString());

    if (loadAllFromXML(false))
    {
      PreviewWindow.clearAll();
      viewSequence.refreshRecordPtrs();
      FileManager.clearHistory();
      FileManager.pruneAndRefresh(true);

      forEachHyperTab(HyperTab::refreshRecordPtr);

      if (activeTabEnum() == queryTabEnum)
        activeTab().clear(true);

      viewSequence.loadViewFromCurrentSlotToUI();

      return;
    }

    if (db.isOffline())
      clearAllTabsAndViews();

    enableControls(db.isOnline());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void openDB(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath))
    {
      FileChooser fileChooser = new FileChooser();

      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter(appTitle + " files (*.hdb)", "*.hdb"));

      File dir = new File(app.prefs.get(PrefKey.SOURCE_PATH, userWorkingDir()));

      if (dir.exists() == false)
        dir = new File(userWorkingDir());

      fileChooser.setInitialDirectory(dir);

      filePath = showOpenDialog(fileChooser);
    }

    if (FilePath.isEmpty(filePath)        ||
        (close(true) == false)            ||
        (db.getState() != DBState.CLOSED) ||
        ui.isShuttingDown())
      return;

    app.prefs.put(PrefKey.SOURCE_FILENAME, filePath.getNameOnly().toString());
    app.prefs.put(PrefKey.SOURCE_PATH    , filePath.getDirOnly ().toString());

    loadAllFromXmlAndResetUI(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuNewDatabaseClick()
  {
    if (IS_OS_WINDOWS == false)
      infoPopup("Select an empty folder in which to create the new database.");

    DirectoryChooser dirChooser = new DirectoryChooser();

    File file = new File(app.prefs.get(PrefKey.SOURCE_PATH, ""));

    dirChooser.setTitle("Select an empty folder in which to create database");

    dirChooser.setInitialDirectory(file.exists() && file.isDirectory() ? file : new File(userWorkingDir()));

    FilePath rootPath = showDirDialog(dirChooser);
    if (FilePath.isEmpty(rootPath)) return;

    String[] list = rootPath.toFile().list();
    if (list == null)
    {
      errorPopup("Selected item is not a folder.");
      return;
    }

    if (list.length != 0)
    {
      errorPopup("The selected folder is not empty.");
      return;
    }

    if (createNewDB(rootPath))
      loadAllFromXmlAndResetUI(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean createNewDB(FilePath rootPath)
  {
    if (db.isOnline())
    {
      DialogResult result = yesNoCancelDialog("Save data to XML files?");

      if (result == mrCancel)
        return false;

      if ((result == mrYes) && (saveAllToXML(true, false, false, true) == false))
        return false;

      NewDatabaseDlgCtrlr dlg = new NewDatabaseDlgCtrlr(rootPath.toString());

      if (dlg.showModal() == false)
        return false;

      resetUIPreClose();
      viewSequence.clear();
      boolean success;

      String hdbFileName = app.prefs.get(PrefKey.SOURCE_FILENAME, HDB_DEFAULT_FILENAME);

      try
      {
        success = db.newDB(rootPath, hdbFileName, dlg.getChoices(), dlg.getFolders());
      }
      catch (HDB_UnrecoverableInternalError e)
      {
        errorPopup("Unable to create new database: " + getThrowableMessage(e));

        shutDown(ShutDownMode.UnrecoverableInternalError);
        return false;
      }
      catch (HDB_InternalError e)
      {
        errorPopup("Unable to create new database: " + getThrowableMessage(e));

        success = false;
      }

      if (success == false)
      {
        close(false);
        return false;
      }

      clearAllTabsAndViews();

      if (saveAllToXML(false, false, false, false) == false)
      {
        close(false);
        return false;
      }
    }
    else
    {
      FilePath srcFilePath = null;

      try (ZipInputStream zis = new ZipInputStream(App.class.getResourceAsStream(BLANK_DB_RESOURCE_NAME)))
      {
        ZipEntry entry;

        while ((entry = zis.getNextEntry()) != null)
        {
          FilePath filePath = rootPath.resolve(new FilePath(entry.getName()));

          if (entry.isDirectory())
          {
            filePath.createDirectories();
          }
          else
          {
            if ("hdb".equals(filePath.getExtensionOnly()))
              srcFilePath = filePath;

            byte[] buffer = new byte[2048];

            try (BufferedOutputStream bos = new BufferedOutputStream(Files.newOutputStream(filePath.toPath()), buffer.length))
            {
              int size;

              while ((size = zis.read(buffer, 0, buffer.length)) != -1)
                bos.write(buffer, 0, size);

              bos.flush();
            }
          }
        }
      }
      catch (IOException e)
      {
        errorPopup("Unable to create new database: " + getThrowableMessage(e));
        close(false);
        return false;
      }

      app.prefs.put(PrefKey.SOURCE_FILENAME, srcFilePath.getNameOnly().toString());
      app.prefs.put(PrefKey.SOURCE_PATH    , srcFilePath.getDirOnly ().toString());
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML public void btnSaveClick()
  {
    if (btnSave.getText().equals(TREE_SELECT_BTN_CAPTION))
      treeSelector.select(tree().selectedRecord());
    else if (cantSaveRecord() == false)
      update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnCreateClick()
  {
    if (activeTabEnum() == treeTabEnum)
      treeHyperTab().getCreateMenuItems().getFirst().fire();
    else
      createNew(activeType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createNew(RecordType type)
  {
    if (cantSaveRecord()) return;

    goToRecord(type == hdtTerm ?
      HDT_Term.create(db.glossaries.getByID(1))
    :
      db.createNewBlankRecord(type), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void deleteCurrentRecord(boolean confirm)
  {
    HDT_Record record = activeRecord();

    if (record == null) return;

    RecordType type = record.getType();

    switch (type)
    {
      case hdtGlossary :

        if (activeTabEnum() != treeTabEnum)
        {
          infoPopup("Glossary records can only be deleted from the tree tab.");
          return;
        }

        HDT_Glossary glossary = (HDT_Glossary) record;

        if (glossary.concepts.isEmpty() == false)
        {
          infoPopup("A glossary record can only be deleted if it does not contain any terms.");
          return;
        }

        break;

      case hdtNone : case hdtAuxiliary : case hdtHub : case hdtConcept : case hdtFolder : case hdtWorkFile :

        infoPopup("Records of that type cannot be deleted by this method.");
        return;

      default :
        break;
    }

    if (confirm)
    {
      String msg = type == hdtTerm ?
        "Are you sure you want to delete this record and all associated concepts?"
      :
        "Are you sure you want to delete this record?";

      String name = record.defaultChoiceText();
      if (name.isBlank())
        name = activeTab().recordName();

      if (confirmDialog("Type: " + getTypeName(type) + '\n' +
                        "Name: " + name + '\n' +
                        "ID: " + record.getID() + "\n\n" + msg, false) == false) return;
    }

    db.deleteRecord(record);

    viewSequence.loadViewFromCurrentSlotToUI();
    FileManager.refresh();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuCloseClick()
  {
    close(true);
  }

  public boolean close(boolean needToSave)
  {
    if (needToSave && db.isOnline())
    {
      DialogResult result = yesNoCancelDialog("Save data to XML files before closing?");

      if (result == mrCancel)
        return false;

      if ((result == mrYes) && (saveAllToXML(true, false, false, true) == false))
        return false;
    }

    resetUIPreClose();

    try
    {
      db.close(null);
    }
    catch (HDB_UnrecoverableInternalError e)
    {
      errorPopup(e);
      shutDown(ShutDownMode.UnrecoverableInternalError);
      return false;
    }

    enableControls(false);

    updateBottomPanel(true, true);
    tfRecord.setText("");
    tfID.setText("");
    hcbGoTo.clear();
    viewSequence.clear();

    stage.setTitle(appTitle);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void resetUIPreClose()
  {
    omniSearchMode.setValue(OmniSearchMode.asYouType);

    clearAllTabsAndViews();
    lblStatus.setText("");

    treeSelector.clear();
    closeWindows(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Menu makeTopicalMenu(String text, FilePath filePath)
  {
    Menu menu = new Menu(text);

    MenuItem item = new MenuItem("Show in system explorer");
    item.setOnAction(event -> launchFile(filePath));
    menu.getItems().add(item);

    item = new MenuItem("Show in File Manager");
    item.setOnAction(event -> FileManager.show(filePath));
    menu.getItems().add(item);

    item = new MenuItem("Copy path to clipboard");
    item.setOnAction(event -> copyToClipboard(filePath.toString()));
    menu.getItems().add(item);

    return menu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateTopicalFolders()
  {
    ObservableList<MenuItem> items = mnuFolders.getItems();
    items.clear();

    if (db.isOffline())
    {
      mnuFolders.setDisable(true);
      return;
    }

    mnuFolders.setDisable(false);

    // Each of these needs to be added at index=0 to prevent IndexOutOfBoundsException on Mac

    items.addFirst(makeTopicalMenu("Database root folder" , db.getRootPath  ()));
    items.addFirst(makeTopicalMenu("Search results"       , db.resultsPath  ()));
    items.addFirst(makeTopicalMenu("Misc. files"          , db.miscFilesPath()));
    items.addFirst(makeTopicalMenu("Pictures"             , db.picturesPath ()));
    items.addFirst(makeTopicalMenu("Topical"              , db.topicalPath  ()));
    items.addFirst(makeTopicalMenu("Works not entered yet", db.unenteredPath()));
    items.addFirst(makeTopicalMenu("Books"                , db.booksPath    ()));
    items.addFirst(makeTopicalMenu("Papers"               , db.papersPath   ()));

    int separatorPos = items.size();
    FilePath topicalPath = db.topicalPath();

    try (DirectoryStream<Path> stream = Files.newDirectoryStream(topicalPath.toPath()))
    {
      stream.forEach(entry ->
      {
        FilePath entryFilePath = new FilePath(entry);
        if (entryFilePath.isDirectory() == false) return;

        FilePath relFilePath = topicalPath.relativize(entryFilePath);

        if (FilePath.isEmpty(relFilePath) == false)
          items.add(makeTopicalMenu(relFilePath.toString(), entryFilePath));
      });
    }
    catch (DirectoryIteratorException | IOException e) { logThrowable(e); }

    if (items.size() > separatorPos)
      items.add(separatorPos, new SeparatorMenuItem());

    // This next part has to be done because otherwise the menu will sometimes appear empty on Mac

    mnuFolders.setVisible(false);
    mnuFolders.setVisible(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuNewFieldClick       () { mnuNewCategoryClick(hdtField       , true, true); }
  @FXML private void mnuNewRankClick        () { mnuNewCategoryClick(hdtRank        , true, true); }
  @FXML private void mnuNewCountryClick     () { mnuNewCategoryClick(hdtCountry     , true, true); }
  @FXML private void mnuNewPersonStatusClick() { mnuNewCategoryClick(hdtPersonStatus, true, true); }

  public <T extends HDT_RecordBase> T mnuNewCategoryClick(RecordType type, boolean canChangeType, boolean save)
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return null;
    }

    if (save && cantSaveRecord()) return null;

    NewCategoryDlgCtrlr ctrlr = new NewCategoryDlgCtrlr(type, canChangeType);

    if (ctrlr.showModal() == false) return null;

    int id = parseInt(ctrlr.tfNewID.getText(), -1);
    type = ctrlr.hcbRecordType.selectedType();

    RecordState recordState = new RecordState(type, id, ctrlr.tfNewKey.getText(), "", "");
    T newRecord;

    try
    {
      newRecord = db.createNewRecordFromState(recordState, true);
    }
    catch (HyperDataException e)
    {
      errorPopup("An error occurred while creating the record: " + getThrowableMessage(e));
      return null;
    }

    newRecord.setName(ctrlr.tfNewName.getText());

    if (save) update();

    return newRecord;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateFavorites()
  {
    mnuToggleFavorite.setText("Add to favorites");

    if (db.isOffline())
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
        mnuToggleFavorite.setText("Remove from favorites");
    }
    else
      mnuToggleFavorite.setDisable(true);

    favorites.updateItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuToggleFavoriteClick()
  {
    if ((activeTabEnum() == treeTabEnum) || (activeTabEnum() == queryTabEnum) || cantSaveRecord()) return;

    HDT_Record record = viewRecord();
    if (record == null) return;

    int ndx = favorites.indexOfRecord(record);

    if (ndx > -1)
      mnuFavorites.getItems().remove(ndx);
    else
      mnuFavorites.getItems().add(new RecordFavorite(record));

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

    ht.addContextMenuItem("Go to argument/stance record", HDT_Position.class,
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
      setAllVisible(false, progressBar, lblProgress);
      return;
    }

    if (progressBar.isVisible() == false)
      progressBar.setVisible(true);

    if (strNotNullOrBlank(task))
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

  private void searchForMentions(boolean descOnly)
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    HDT_Record activeRecord  = activeRecord(),
               focusedRecord = getFocusedRecord(),
               record;

    if ((activeRecord == focusedRecord) || strNullOrEmpty(focusedRecord.getSearchKey()))
      record = activeRecord;
    else if (strNullOrEmpty(activeRecord.getSearchKey()))
      record = focusedRecord;
    else
      record = getSelectedRecordAskIfNeeded();

    if (record == null)
      return;

    RecordType type = record.getType();
    boolean backClick = activeTabEnum() != queryTabEnum;

    lblStatus.setText("");

    if (showSearch(true, qtAllRecords, descOnly ? QUERY_LINKING_TO_RECORD : QUERY_MATCHING_RECORD, null,
                   new GenericNonRecordHTC("", type), new RecordHTC(record, ""), "Mentions: " + record.defaultCellText()))
    {
      List<ResultRow> resultRows = queryHyperTab().results();

      if ((resultRows.size() > 0) && ((resultRows.size() != 1) || (resultRows.getFirst().getRecord() != record)))
        return;

      lblStatus.setText("No mentioners: " + getTypeName(type).toLowerCase() + " \"" + record.defaultCellText() + '"');
    }

    discardLastQuery(backClick);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_Record getSelectedRecordAskIfNeeded()
  {
    HDT_Record activeRecord  = activeRecord(),
               focusedRecord = getFocusedRecord();

    if ((activeRecord == null) || (focusedRecord == null) || (activeRecord == focusedRecord))
    {
      if (activeRecord == null)
        errorPopup("No record is currently selected.");

      return activeRecord;
    }

    DialogResult result = new PopupDialog("Which record?\n\n" + getTypeName(activeRecord .getType()) + ": " + activeRecord .defaultChoiceText() +
                                                         '\n' + getTypeName(focusedRecord.getType()) + ": " + focusedRecord.defaultChoiceText())

      .addDefaultButton(getTypeName(activeRecord .getType()), mrYes   )
      .addButton       (getTypeName(focusedRecord.getType()), mrNo    )
      .addButton       ("Cancel"                            , mrCancel)

      .showModal();

    return switch (result)
    {
      case mrYes -> activeRecord;
      case mrNo  -> focusedRecord;
      default    -> null;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Get the "innermost" record the user is currently focused on, which is usually
   * the same record returned by activeRecord(), but would be the current HDT_Concept
   * if the Terms tab is active, or the current HDT_Investigation if the Persons tab
   * is active.
   * @return The focused record
   */
  private HDT_Record getFocusedRecord()
  {
    return switch (activeTabEnum())
    {
      case termTabEnum   -> viewRecord();
      case personTabEnum -> personHyperTab().getCurInvestigation();
      default            -> activeRecord();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuAddToQueryResultsClick()
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    if (queryHyperTab().getCurQueryCtrlr().inReportMode())
    {
      infoPopup("That menu option cannot be used to add a record to a report.");
      return;
    }

    HDT_Record record = getSelectedRecordAskIfNeeded();

    if (record == null)
      return;

    QueryCtrlr curQueryCtrlr = queryHyperTab().getCurQueryCtrlr();

    for (ResultRow row : curQueryCtrlr.results())
      if (row.getRecord() == record) return;

    curQueryCtrlr.addRecord(record, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void mnuRevertToXmlVersionClick()
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    if ((activeTabEnum() == termTabEnum) || (activeTabEnum() == personTabEnum))
      if (cantSaveRecord())
        return;  // Need to save if it might only be a partial reversion

    HDT_Record record = getSelectedRecordAskIfNeeded();

    if (record == null)
      return;

    if (record.hasStoredState() == false)
    {
      errorPopup("Unable to revert: the record may not have been previously saved to XML.");
      return;
    }

    String msg = "Are you sure you want to revert this record to the last version saved to XML?",
           additionalMsg = "";

    RecordType type = record.getType();

    switch (type)
    {
      case hdtTerm   : additionalMsg = "\n\nNote: Reverting a Term record does not automatically revert the associated Concept records.";         break;
      case hdtPerson : if (((HDT_Person)record).investigations.size() > 0) additionalMsg = "\n\nNote: Reverting a Person record does not automatically revert associated Investigation records."; break;

      default : break;
    }

    String name = record.defaultChoiceText();
    if (name.isBlank())
      name = activeTab().recordName();

    if (confirmDialog("Type: " + getTypeName(type) + '\n' +
                      "Name: " + name + '\n' +
                      "ID: " + record.getID() + "\n\n" + msg + additionalMsg, false) == false)
      return;

    String recordStr = getTypeName(record.getType()) + " \"" + name + '"';

    try
    {
      record.revertToXmlVersion();
    }
    catch (RelationCycleException e)
    {
      errorPopup("Unable to revert " + recordStr + ": Records would be organized in a cycle as a result.");
    }
    catch (HyperDataException e)
    {
      errorPopup("Unable to revert " + recordStr + ": " + getThrowableMessage(e));
    }

    update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void startEmpty()
  {
    clearAllTabsAndViews();
    enableControls(false);
    showWelcomeWindow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Record getInitialTabRecord(Map<RecordType, HDT_Record> typeToMostRecentlyViewedRecord, RecordType type, String prefID)
  {
    HDT_Record record = db.records(type).getByID(db.prefs.getInt(prefID, -1));
    if (record != null) return record;

    record = typeToMostRecentlyViewedRecord.get(type);
    if (record != null) return record;

    return db.records(type).isEmpty() ? null : db.records(type).getByKeyNdx(0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void loadAllFromXmlAndResetUI(boolean creatingNew)
  {
    loadAllFromXmlAndResetUI(creatingNew, false);
  }

  private void loadAllFromXmlAndResetUI(boolean creatingNew, boolean dontAskAboutRefMgr)
  {
    if (loadAllFromXML(creatingNew) == false)
    {
      if (db.isOffline())
        clearAllTabsAndViews();

      enableControls(db.isOnline());
      return;
    }

    Map<RecordType, HDT_Record> typeToMostRecentlyViewedRecord = new EnumMap<>(RecordType.class);
    ListIterator<HDT_Record> iterator = db.initialNavHistory().listIterator(db.initialNavHistory().size());

    while (iterator.hasPrevious())
    {
      HDT_Record record = iterator.previous();
      typeToMostRecentlyViewedRecord.putIfAbsent(record.getType(), record);
    }

    HDT_Record invRecord = db.investigations.getByID(db.prefs.getInt(RecordIDPrefKey.INVESTIGATION, -1));

    if (invRecord != null)
      saveViewToViewsTab(new HyperView<>(personTabEnum, invRecord));
    else
    {
      invRecord               = getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtInvestigation, RecordIDPrefKey.INVESTIGATION);
      HDT_Record personRecord = getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtPerson       , RecordIDPrefKey.PERSON       );

      if ((invRecord != null) && ((personRecord == null) || invRecord.getViewDate().isAfter(personRecord.getViewDate())))
        saveViewToViewsTab(new HyperView<>(personTabEnum, invRecord));
      else
        saveViewToViewsTab(new HyperView<>(personTabEnum, personRecord));
    }

    saveViewToViewsTab(new HyperView<>(instTabEnum    , getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtInstitution, RecordIDPrefKey.INSTITUTION)));
    saveViewToViewsTab(new HyperView<>(debateTabEnum  , getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtDebate     , RecordIDPrefKey.DEBATE     )));
    saveViewToViewsTab(new HyperView<>(positionTabEnum, getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtPosition   , RecordIDPrefKey.POSITION   )));
    saveViewToViewsTab(new HyperView<>(argumentTabEnum, getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtArgument   , RecordIDPrefKey.ARGUMENT   )));
    saveViewToViewsTab(new HyperView<>(workTabEnum    , getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtWork       , RecordIDPrefKey.WORK       )));
    saveViewToViewsTab(new HyperView<>(termTabEnum    , getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtConcept    , RecordIDPrefKey.CONCEPT    )));
    saveViewToViewsTab(new HyperView<>(fileTabEnum    , getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtMiscFile   , RecordIDPrefKey.FILE       )));
    saveViewToViewsTab(new HyperView<>(noteTabEnum    , getInitialTabRecord(typeToMostRecentlyViewedRecord, hdtNote       , RecordIDPrefKey.NOTE       )));
    saveViewToViewsTab(new HyperView<>(queryTabEnum   , null));
    saveViewToViewsTab(new HyperView<>(treeTabEnum    , null));

    enableControls(db.isOnline());

    viewSequence.init(getTabEnumByRecordType(Tag.parseTypeTagStr(db.prefs.get(PrefKey.RECORD_TYPE, ""))));

    if (dontAskAboutRefMgr || db.bibLibraryIsLinked() || (db.prefs.getBoolean(PrefKey.NOTIFY_USER_NOT_LINKED, true) == false))
      return;

    switch (new PopupDialog("This database is not currently integrated with a reference manager account (like Mendeley or Zotero). Add one now?")

      .addButton       ("Yes"                               , mrYes   )
      .addDefaultButton("Remind me later"                   , mrNo    )
      .addButton       ("Do not ask again for this database", mrIgnore)

      .showModal())
    {
      case mrYes    : new SettingsDlgCtrlr(SettingsPage.BibMgr).showModal();       break;

      case mrNo     : db.prefs.putBoolean(PrefKey.NOTIFY_USER_NOT_LINKED, true ); break;
      case mrIgnore : db.prefs.putBoolean(PrefKey.NOTIFY_USER_NOT_LINKED, false); break;

      default       : break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean loadAllFromXML(boolean creatingNew)
  {
    if (IS_OS_MAC)
      Platform.runLater(() -> adjustToolBar(0));

    FilePath hdbPath = null;
    boolean hdbExists = false;
    String srcName = app.prefs.get(PrefKey.SOURCE_FILENAME, "");
    if (srcName.isBlank() == false)
    {
      String srcPath = app.prefs.get(PrefKey.SOURCE_PATH, "");
      if (srcPath.isBlank() == false)
      {
        hdbPath = new FilePath(srcPath).resolve(srcName);
        if (hdbPath.exists())
          hdbExists = true;
      }
    }

    if (hdbExists == false)
      return (hdbPath != null) && falseWithErrorPopup("Unable to load database. The file does not exist: " + hdbPath);

    if (InterProcClient.folderInUseByAnotherInstance(hdbPath))
      return falseWithErrorPopup("Unable to load database: Database folder(s) are already in use by another instance of " + appTitle);

    if (internetNotCheckedYet && app.prefs.getBoolean(PrefKey.CHECK_INTERNET, true))
    {
      if (checkInternet() == false)
        return false;

      internetNotCheckedYet = false;
    }

    String otherCompName = db.getOtherLockOwner();
    if (otherCompName != null)
    {
      if (new LockedDlgCtrlr(otherCompName).showModal() == false)
        return false;

      if (db.getOtherLockOwner() != null)
        return false;

      // Need to check for another instance on this computer because another instance might have
      // already overridden the remote computer's lock while this instance's LockedDlg popup was open

      if (InterProcClient.folderInUseByAnotherInstance(hdbPath))
        return falseWithErrorPopup("Unable to load database: Database folder(s) are already in use by another instance of " + appTitle);
    }

    boolean success;

    FilePath newRootFilePath = new FilePath(app.prefs.get(PrefKey.SOURCE_PATH, userWorkingDir()));
    String hdbFileName = app.prefs.get(PrefKey.SOURCE_FILENAME, HDB_DEFAULT_FILENAME);

    try
    {
      success = db.loadAllFromPersistentStorage(creatingNew, favorites, newRootFilePath, hdbFileName);
    }
    catch (HDB_UnrecoverableInternalError e)
    {
      errorPopup("Unable to load database. Reason: " + getThrowableMessage(e));
      shutDown(ShutDownMode.UnrecoverableInternalError);
      return false;
    }

    if (success)
    {
      List<String> mruList = getHdbMRUs();
      mruList.addFirst(hdbPath.toString());
      saveHdbMRUs(mruList);

      success = folderTreeWatcher.createNewWatcherAndStart();
    }

    if (success)
    {
      lblStatus.setText("");
      updateTopicalFolders();
      queryHyperTab().clear(true);

      queryHyperTab().enable(true);
      treeHyperTab().enable(true);

      tree().expandMainBranches();
      FileManager.instance().folderTree.expandMainBranches();

      stage.setTitle(appTitle + " - " + db.getHdbPath());
    }
    else
      close(false);

    return success;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void saveHdbMRUs(List<String> mruList)
  {
    mruList.removeIf(String::isBlank);
    removeDuplicatesInPlace(mruList);

    for (int ndx = 0; ndx < HDB_MRU_SIZE; ndx++)
      app.prefs.put(PrefKey.HDB_MRU + (ndx + 1), mruList.size() > ndx ? mruList.get(ndx) : "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> getHdbMRUs()
  {
    List<String> mruList = new ArrayList<>();

    for (int ndx = 0; ndx < HDB_MRU_SIZE; ndx++)
      mruList.add(app.prefs.get(PrefKey.HDB_MRU + (ndx + 1), ""));

    mruList.removeIf(String::isBlank);
    return mruList;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Checks whether there is a currently active record that can be saved, and if so, saves it.
   * @return True if the application is <i>not</i> in the process of shutting down and there was an active record that could not be saved;
   * false indicates that the record was actually saved or no record was active or the application is shutting down.
   */
  public boolean cantSaveRecord()
  {
    return cantSaveRecord(true);
  }

  public boolean cantSaveRecord(boolean saveNameIfBlank)
  {
    if (db.isOffline() || (activeTabEnum() == queryTabEnum) || (activeTabEnum() == treeTabEnum) || (activeRecord() == null))
      return false;

    if (shuttingDown == false)
      CommitableWrapper.commitWrapper(stage.getScene().getFocusOwner());

    if ((activeTab().saveToRecord(saveNameIfBlank) == false) && (shuttingDown == false))
    {
      ui.windows.focusStage(ui.getStage());
      return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToTreeRecord(HDT_Record record)
  {
    if ((record == null) || db.isOffline() || shuttingDown) return;

    if (windows.getOutermostStage() != stage)
      windows.focusStage(stage);

    if (cantSaveRecord())
    {
      treeSelector.clear();
      return;
    }

    viewSequence.saveViewFromUItoSlotAdvanceCursorAndLoadNewViewToUI(new HyperView<>(treeTabEnum, record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TabEnum activeTabEnum()                    { return viewSequence.isEmpty() ? personTabEnum : viewSequence.tabEnumOfViewInCurrentSlot(); }
  public HyperTab<? extends HDT_Record,
                  ? extends HDT_Record> activeTab() { return viewSequence.isEmpty() ? null : viewSequence.tabOfViewInCurrentSlot(); }
  private RecordType activeType()                   { return viewSequence.isEmpty() ? hdtPerson : viewSequence.getViewInCurrentSlot().getTabRecordType(); }
  public HDT_Record activeRecord()                  { return viewSequence.isEmpty() ? null : activeTab().activeRecord(); }
  public HDT_Record viewRecord()                    { return viewSequence.isEmpty() ? null : activeTab().viewRecord(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToRecord(HDT_Record record, boolean save)
  {
    if ((record == null) || db.isOffline() || shuttingDown) return;

    treeSelector.clear();
    HDT_WorkFile workFile = null;

    switch (record.getType())
    {
      case hdtHub :

        record = ((HDT_Hub) record).mainSpoke();
        if (record == null) return;
        break;

      case hdtGlossary :

        goToTreeRecord(record);
        return;

      case hdtWorkLabel :

        HDT_Hub hub = ((HDT_WorkLabel) record).getHub();

        if (hub == null)
        {
          goToTreeRecord(record);
          return;
        }

        record = hub.mainSpoke();
        break;

      case hdtFolder :

        FileManager.show(((HDT_Folder) record).filePath());
        return;

      case hdtWorkFile :

        workFile = (HDT_WorkFile) record;
        if (workFile.works.size() > 0)
          record = workFile.works.getFirst();
        else
        {
          FileManager.show(workFile.filePath());
          return;
        }

        break;

      case hdtTerm :

        record = ((HDT_Term) record).concepts.getFirst();
        break;

      default : break;
    }

    if (getTabEnumByRecordType(record.getType()) == personTabEnum)
      if ((record.getType() != hdtPerson) && (record.getType() != hdtInvestigation)) return;

    if (windows.getOutermostStage() != stage)
      windows.focusStage(stage);

    if (save && cantSaveRecord()) return;

    viewSequence.saveViewFromUItoSlotAdvanceCursorAndLoadNewViewToUI(new HyperView<>(record));

    if (workFile != null)
      workHyperTab().showWorkFile(workFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Update the main window to refresh the display with the currently selected record in the UI.<br>
   * This also updates the last viewed instant for that record.
   */
  public void update()
  {
    update(null);
  }

  /**
   * Update the main window to refresh the display with the currently selected record.<br>
   * This also updates the last viewed instant for that record.
   *
   * @param record If a record is passed in, use that as the active record; it will likely
   * be the record set in the active HyperView, rather than the one selected in the UI.
   * <br>
   * <p>If the parameter is null, the currently selected record in the UI is used.
   * <br>
   * <p>The parameter is ignored if the active tab is not Tree or Queries.
   */
  public void update(HDT_Record record)
  {
    update(record, true);
  }

  /**
   * Update the main window to refresh the display with the currently selected record
   *
   * @param record If a record is passed in, use that as the active record; it will likely
   * be the record set in the active HyperView, rather than the one selected in the UI.
   * <br>
   * <p>If the parameter is null, the currently selected record in the UI is used.
   * <br>
   * <p>The parameter is ignored if the active tab is not Tree or Queries.
   * @param updateLastViewedInstant Whether to update the last viewed instant for the record.<br>
   * The usual reason for passing false to this parameter is that if the last view instant is
   * updated after a record is first created and the name is blank, it is assumed that the popup
   * to confirm whether the name should be blank has already been shown to the user.
   */
  public void update(HDT_Record record, boolean updateLastViewedInstant)
  {
    updateTopicalFolders();

    if (db.isOffline())
    {
      tree().clear();
      return;
    }

    TabEnum tabEnum = activeTabEnum();
    HyperTab<? extends HDT_Record, ? extends HDT_Record> tab = activeTab();
    boolean updateRecord = record != null;

    switch (tabEnum)
    {
      case queryTabEnum : case treeTabEnum :
        tab.update(updateRecord);

        updateBottomPanel(true, true);
        return;

      default :
        break;
    }

    if (record == null) record = activeRecord();

    int count = tab.recordCount();

    treeSelector.clear();

    if (count > 0)
    {
      if (HDT_Record.isEmpty(record, true))
      {
        int ndx = tab.getView().getTabRecordKeyNdx();

        if (ndx >= count)
          ndx = count - 1;

        if (ndx < 0)
          ndx = 0;

        record = db.records(activeType()).getByKeyNdx(ndx);

        viewSequence.saveViewToCurrentSlotAndTab(tabEnum == termTabEnum ?
          new HyperView<>(termTabEnum, ((HDT_Term) record).concepts.getFirst())
        :
          new HyperView<>(tabEnum, record));
      }
    }
    else
      viewSequence.saveViewToCurrentSlotAndTab(new HyperView<>(tabEnum, null));

    updateBottomPanel(true, true);

    if (HDT_Record.isEmpty(record, true))
    {
      tab.clear(true);
      tab.enable(false);
      return;
    }

    tab.clear(false);
    tab.enable(true);
    tab.update(true);

    if (updateLastViewedInstant)
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

  private RecordType selectorType()
  {
    TabEnum tabEnum = selectorTabEnum();

    return (tabEnum == listTabEnum) || (tabEnum == omniTabEnum) ? activeType() : getRecordTypeByTabEnum(tabEnum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateSelectorTab(boolean setFocus)
  {
    TabEnum selectorTabEnum = selectorTabEnum(), activeTabEnum = activeTabEnum();
    HyperTab<? extends HDT_Record, ? extends HDT_Record> hyperTab = getHyperTab(selectorTabEnum);
    if (hyperTab == null) hyperTab = activeTab();  // This is done on a separate line instead of using nullSwitch to avoid false-positive build error

    tfSelector = null;

    int count = hyperTab == null ? 0 : hyperTab.recordCount();

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

          limitRowsToMeasureWidthInCB(cbTreeGoTo);
          copyRegionLayout(cbGoTo, cbTreeGoTo);

          apListGoTo.getChildren().setAll(cbTreeGoTo);
          tfSelector = cbTreeGoTo.getEditor();
        }

        break;

      case omniTabEnum :

        mnuRecordSelect.setVisible(false);
        setAllVisible(true, mnuFindWithinAnyField, mnuFindWithinName);

        tfSelector = ctfOmniGoTo;

        break;

      default :

        setAllVisible(true, mnuFindWithinAnyField, mnuFindWithinName);

        tfSelector = cbGoTo.getEditor();
        hcbGoTo.clear();
        ((RecordByTypePopulator) hcbGoTo.getPopulator()).setRecordType(selectorType());
        if (cbGoTo.isEditable() == false) cbGoTo.setEditable(true);

        if (count > 0)
          hcbGoTo.selectIDofRecord(hyperTab.activeRecord());

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

  public void updateBottomPanel(boolean refreshDropDown, boolean switchToRecordSearch)
  {
    ttDates.setText(NO_DATES_TOOLTIP);

    if (db.isOffline())
    {
      treeHyperTab().hideSelectingMessage();
      return;
    }

    HyperTab<? extends HDT_Record, ? extends HDT_Record> curTab = activeTab();
    if (curTab == null) return;

    int count = curTab.recordCount(), ndx = curTab.recordNdx();
    HDT_Record activeRec = activeRecord();
    TabEnum activeTabEnum = activeTabEnum();

    db.attachOrphansToRoots();

    btnTextSearch.setDisable(false);

    createMenuItems.clear();

  //---------------------------------------------------------------------------
  // Query-specific stuff
  //---------------------------------------------------------------------------

    if (activeTabEnum == queryTabEnum)
    {
      btnSave.setText("Accept Edits");

      btnRevert.setDisable(false);
      btnRevert.setText("Refresh");
      setToolTip(btnRevert, "Refresh data in table. This does not re-run the query.");

      btnDelete.setDisable(activeRec == null);
      setToolTip(btnDelete, "Delete selected record");

      btnCreateNew.setVisible(false);

      disableAll(btnSave, btnIncrement, btnDecrement);
    }

  //---------------------------------------------------------------------------
  // Tree-specific stuff
  //---------------------------------------------------------------------------

    else if (activeTabEnum == treeTabEnum)
    {
      if (treeSelector.getBase() == null)
      {
        treeHyperTab().hideSelectingMessage();

        btnSave.setDisable(true);
        btnSave.setText("Accept Edits");
      }
      else
      {
        treeHyperTab().showSelectingMessage();

        btnSave.setDisable(false);
        btnSave.setText(TREE_SELECT_BTN_CAPTION);
      }

      setToolTip(btnSave, "");

      btnRevert.setDisable(false);
      btnRevert.setText("Refresh");
      setToolTip(btnRevert, "Refresh data in tree");

      disableAllIff(count < 1, btnIncrement, btnDecrement);

      setToolTip(btnIncrement, "Go to next instance of this record in tree");
      setToolTip(btnDecrement, "Go to previous instance of this record in tree");

      btnDelete.setDisable(activeRec == null);
      setToolTip(btnDelete, "Delete selected record");

      List<MenuItem> menuItems = treeHyperTab().getCreateMenuItems();
      btnCreateNew.setVisible(menuItems.size() > 0);

      if (menuItems.size() > 0)
      {
        setToolTip(btnCreateNew, menuItems.getFirst().getText());

        createMenuItems.addSeparator();
        menuItems.forEach(createMenuItems::add);
      }
    }

  //---------------------------------------------------------------------------
  // Single-record-tab-specific stuff
  //---------------------------------------------------------------------------

    else
    {
      btnTextSearch.setDisable(EnumSet.of(hdtArgument, hdtDebate,   hdtMiscFile, hdtNote,
                                          hdtPerson,   hdtPosition, hdtTerm,     hdtWork).contains(activeType()) == false);

      btnSave.setText("Accept Edits");
      setToolTip(btnSave, "Commit changes made while this record has been showing. Does not save changes to database XML files.");

      disableAllIff(activeRec == null, btnDelete, btnSave, btnRevert);
      setToolTip(btnDelete, "Delete this record");

      btnRevert.setText("Revert");
      setToolTip(btnRevert, "Revert changes made while this record has been showing and since Accept Edits was last pressed");

//      btnRevert.setDisable(changed == false);

      btnCreateNew.setVisible(true);
      setToolTip(btnCreateNew, "Create a new " + getTypeName(getRecordTypeByTabEnum(activeTabEnum)) + " record");

      btnDecrement.setDisable((count == 0) || (ndx == 0));
      btnIncrement.setDisable((count == 0) || (ndx == (count - 1)));

      setToolTip(btnIncrement, "Go to next record in sort order");
      setToolTip(btnDecrement, "Go to previous record in sort order");
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

    if (switchToRecordSearch)
      switchToRecordSearch();
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
   * false if the query did not run (which would be because the current record couldn't be saved first),
   * encountered an error during execution, or was cancelled by the user.
   */
  public boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite fav, HyperTableCell op1, HyperTableCell op2, String caption)
  {
    if (cantSaveRecord()) return false;

    viewSequence.saveViewFromUItoSlotAdvanceCursorAndLoadNewViewToUI(queryHyperTab().newView(queryHyperTab().activeRecord()));

    boolean result = queryHyperTab().showSearch(doSearch, type, query, fav, op1, op2, caption);
    updateFavorites();

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void incDecClick(boolean increment)
  {
    if (activeTabEnum() == treeTabEnum)
    {
      tree().selectNextInstance(increment);
      return;
    }

    DatasetAccessor<? extends HDT_Record> records = db.records(activeType());

    int ndx = activeRecord().keyNdx() + (increment ? 1 : -1);
    if ((ndx >= 0) && (ndx < records.size()))
      goToRecord(records.getByKeyNdx(ndx), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Unites record1 and record2, ensuring that they have the same description. Will prompt user to
   * decide how to merge descriptions if necessary.
   * @param record1 First record to be united
   * @param record2 Second record to be united
   * @return False if user was prompted for how to merge descriptions and clicked Cancel; true otherwise
   * @throws HyperDataException if record1 and record2 cannot be united for some reason
   */
  public static boolean uniteRecords(HDT_RecordWithMainText record1, HDT_RecordWithMainText record2) throws HyperDataException
  {
    String desc;

    if      ((record2.getType() == hdtWorkLabel) && (record2.hasHub() == false))      desc = record1.getMainText().getHtml();
    else if (convertToSingleLine(record1.getMainText().getPlain()).isBlank())         desc = record2.getMainText().getHtml();
    else if (convertToSingleLine(record2.getMainText().getPlain()).isBlank())         desc = record1.getMainText().getHtml();
    else if (record1.getMainText().getHtml().equals(record2.getMainText().getHtml())) desc = record1.getMainText().getHtml();
    else
    {
      MergeSpokeDlgCtrlr msdc = new MergeSpokeDlgCtrlr(record1, record2);

      if (msdc.showModal() == false)
        return false;

      desc = msdc.getDesc();
    }

    HDT_Hub.uniteRecords(record1, record2, desc);
    return true;
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

  private void showFindTable()
  {
    tvFind.setVisible(true);
    apFindBackground.setMouseTransparent(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void hideFindTable()
  {
    apFindBackground.setMouseTransparent(true);
    tvFind.setVisible(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void importMiscFile(FileRow fileRow, FilePath filePath)
  {
    importMiscFile(fileRow, filePath, true);
  }

  private void importMiscFile(FileRow fileRow, FilePath filePath, boolean save)
  {
    if (save && cantSaveRecord()) return;

    HDT_MiscFile miscFile = db.createNewBlankRecord(hdtMiscFile);

    if (fileRow != null)
      miscFile.getPath().assign(fileRow.getFolder(), fileRow.getFilePath().getNameOnly());

    goToRecord(miscFile, false);

    if (fileHyperTab().showFileDialog(filePath, false) == false)
    {
      if (fileRow != null)
        miscFile.getPath().clear(false);

      deleteCurrentRecord(false);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean importWorkFile(HDT_Person person, FilePath filePathToUse, boolean promptForExistingRecord)
  {
    return importWorkFile(person, filePathToUse, promptForExistingRecord, false);
  }

  private boolean importWorkFile(HDT_Person person, FilePath filePathToUse, boolean promptForExistingRecord, boolean includeMiscFileOption)
  {
    if (filePathToUse != null)
    {
      if (filePathToUse.equals(lastImportFilePath) && (milliDiff(Instant.now(), lastImportTime) < 10000L))
        return false;

      if (filePathToUse.exists() == false) return false;
    }

    if (cantSaveRecord()) return false;

    HDT_Work work = null;
    BibData bdToUse = null;
    BibEntry<?, ?> bibEntry = null;
    Ternary newEntryChoice = Ternary.Unset;
    EntryType newEntryType = EntryType.etUnentered;

    if (promptForExistingRecord)
    {
      SelectWorkDlgCtrlr swdc = new SelectWorkDlgCtrlr(person, filePathToUse, includeMiscFileOption);
      if (swdc.showModal() == false) return false;

      if (swdc.getButtonClicked() == SelectWorkDlgCtrlr.ButtonClicked.NewMisc)
      {
        importMiscFile(null, swdc.getFilePath(), false);
        return false;
      }

      filePathToUse = swdc.getFilePath();
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
              internalErrorPopup(62883);
              return false;
            }

            try
            {
              mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", Stream.of(work.getBibData(), bibEntry, bdToUse), work, false, false, newEntryChoice, nullSwitch(filePathToUse, work.filePath()));
            }
            catch (IOException e)
            {
              errorPopup("Unable to initialize merge dialog window.");
              return false;
            }

            if (mwd.showModal() == false) return false;

            work.setBibEntryKey(bibEntry.getKey());
            mwd.mergeInto(bibEntry);
            bdToUse = bibEntry;
          }
          else if (bibEntry.getKey().equals(work.getBibEntryKey()) == false)
          {
            internalErrorPopup(62884);
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
              mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", Stream.of(workBD, bdToUse), work, false, true, newEntryChoice, nullSwitch(filePathToUse, work.filePath()));
            }
            catch (IOException e)
            {
              errorPopup("Unable to initialize merge dialog window.");
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
        internalErrorPopup(62885);
        return false;
      }

      work = db.createNewBlankRecord(hdtWork);
      WorkAuthors authors = work.getAuthors();

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
      WorkAuthors authors = work.getAuthors();

      if ((person != null) && (authors.containsPerson(person) == false))
        authors.add(person);
    }

    goToRecord(work, false);

    if (bdToUse == GUIBibData.NoneFoundBD)
      bdToUse = work.getBibData();

    if (workHyperTab().showWorkDialog(null, false, filePathToUse, bdToUse, newEntryChoice, newEntryType))
      return true;

    if (deleteRecord)
      deleteCurrentRecord(false);

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void handleArgs(List<String> args)
  {
    if (db.isOffline() || collEmpty(args) || (windows.getOutermostModality() != Modality.NONE)) return;

    FilePath filePath = new FilePath(args.getFirst());

    String mediaTypeStr = getMediaType(filePath).toString();

    if (mediaTypeStr.contains("pdf"))
    {
      importWorkFile(null, filePath, true, true);
      return;
    }

    if (mediaTypeStr.contains("text"))
    {
      BibDataStandalone fileBibData = null;

      try
      {
        if (filePath.size() < 50000)
          fileBibData = BibDataStandalone.detectWithinTextFile(null, filePath);
      }
      catch (TokenMgrException | IOException | ParseException e)
      {
        noOp();
      }

      if (fileBibData != null)
      {
        importBibFile(null, filePath);
        return;
      }
    }

    switch (new PopupDialog("What should the file be imported as?")

      .addDefaultButton("Work"                 , mrYes   )
      .addButton       ("Misc. file"           , mrNo    )
      .addButton       ("Bibliographic details", mrOk    )
      .addButton       ("Cancel"               , mrCancel)

      .showModal())
    {
      case mrYes : importWorkFile(null, filePath, true); return;
      case mrNo  : importMiscFile(null, filePath      ); return;
      case mrOk  : importBibFile (null, filePath      ); return;

      default    : break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void importBibFile(List<String> lines, FilePath filePath)
  {
    if (cantSaveRecord()) return;

    ImportBibEntryDlgCtrlr ibed = new ImportBibEntryDlgCtrlr(lines, filePath);

    if (ibed.getFailedToLoad() || !ibed.showModal()) return;

    lines = ibed.getLines();
    filePath = ibed.getFilePath();

    String pathStr = FilePath.isEmpty(filePath) ? "" : (" " + filePath);

    BibDataStandalone fileBibData = null;
    Exception ex = null;

    try
    {
      fileBibData = BibDataStandalone.detectWithinTextFile(lines, null);
    }
    catch (TokenMgrException | IOException | ParseException e)
    {
      ex = e;
    }

    if (fileBibData == null)
    {
      errorPopup(ex == null ?
        "Unable to parse bibliographic information."
      :
        "An error occurred while trying to parse BibTex file" + pathStr + ": " + getThrowableMessage(ex));

      return;
    }

    boolean creatingNewWork = ibed.getCreateNewWork(),
            newEntryChecked = ibed.getCreateNewBibEntry(),
            showNewEntry = true;

    HDT_Work work = creatingNewWork ? db.createNewBlankRecord(hdtWork) : ibed.getRecord();

    BibData workBibData = work.getBibData();

    if (strNotNullOrBlank(work.getBibEntryKey()))
      showNewEntry = false;

    MergeWorksDlgCtrlr mwd;

    try
    {
      mwd = new MergeWorksDlgCtrlr("Import Into " + (creatingNewWork ? "New" : "Existing") + " Work Record",
                                   Stream.of(workBibData, fileBibData), work, creatingNewWork, showNewEntry, newEntryChecked ? Ternary.True : Ternary.Unset);
    }
    catch (IOException e)
    {
      errorPopup("Unable to initialize merge dialog window.");
      if (creatingNewWork) db.deleteRecord(work);
      return;
    }

    if (mwd.showModal() == false)
    {
      if (creatingNewWork) db.deleteRecord(work);
      return;
    }

    if (mwd.creatingNewEntry().isTrue())
    {
      BibEntry<?, ?> entry = db.getBibLibrary().addEntry(mwd.getEntryType());
      work.setBibEntryKey(entry.getKey());
      workBibData = entry;
    }

    mwd.mergeInto(workBibData);
    BibManager.refresh();

    goToRecord(work, false);
    update();

    if ((FilePath.isEmpty(filePath) == false) && ibed.getDeleteFile())
      filePath.deletePromptOnFail(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML public void showWelcomeWindow()
  {
    WelcomeDlgCtrlr wdc = new WelcomeDlgCtrlr();
    if (wdc.showModal() == false) return;

    if (wdc.newClicked())
      mnuNewDatabaseClick();
    else if (wdc.openClicked())
      openDB(wdc.getOpenPath());

    if (db.isOffline())
      windows.runInFXThreadAfterModalPopups(250, this::showWelcomeWindow);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void notifyOfImport(FilePath filePath)
  {
    lastImportTime = Instant.now();
    lastImportFilePath = filePath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static WebTooltip searchKeyToolTip = null;

  synchronized public static Tooltip getSearchKeyToolTip()
  {
    if (searchKeyToolTip == null) searchKeyToolTip = new WebTooltip("""
      Multiple search keys should be separated by semicolon (<code>;</code>) character.<br><br>

      Example:<blockquote>

      <table><tr><td>Search keys:</td><td><code>Parfit; Derek Parfit</code></td></tr>
      <tr><td>Description text:</td><td>This conclusion was rejected by <a href="">Derek Parfit</a>. Instead, <a href="">Parfit</a> argues that&hellip;</td></tr></table></blockquote>

      Use up-caret (<code>^</code>) character to indicate that search key should match beginning of word.<br>
      Use dollar sign (<code>$</code>) character to indicate that search key should match end of word.<br><br>

      Example:<blockquote>

      <table><tr><td>Search keys:</td><td><code>^thing; object$; objects$; individual</code></td></tr>
      <tr><td>Description text:</td><td>Anything that is an instance of an ontological category is an entity, but objectively<br>
      speaking, only instances of certain categories count as <a href="">things</a>, <a href="">objects</a>, or <a href="">individuals</a>.</td></tr></table>
      </blockquote>

      Notice that the <code>^</code> at the beginning of the <code>thing</code> search key prevents the word &lsquo;Anything&rsquo; from being matched. Similarly,<br>
      the <code>$</code> at the end of the <code>object</code> search key prevents the word &lsquo;objectively&rsquo; from being matched.""");

    return searchKeyToolTip;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setSearchKeyToolTip(TextField tf)
  {
    tf.setTooltip(getSearchKeyToolTip());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void createTransientTestDB(FilePath transientDBFilePath, LibraryType libraryType)
  {
    if (createNewDB(transientDBFilePath) == false)
      return;

    loadAllFromXmlAndResetUI(true, true);

    if (db.isOffline()) return;

    if (libraryType == null)
    {
      db.prefs.putBoolean(PrefKey.NOTIFY_USER_NOT_LINKED, false);
      return;
    }

    SettingsDlgCtrlr settingsDlgCtrlr = new SettingsDlgCtrlr(SettingsPage.BibMgr);
    settingsDlgCtrlr.selectLibraryType(libraryType);
    settingsDlgCtrlr.showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void switchToRecordSearch()
  {
    dontShowOmniTable = true;
    omniSearchMode.setValue(OmniSearchMode.asYouType);
    dontShowOmniTable = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearOmniFinder()
  {
    tvFind.setPlaceholder(new Text(""));

    if (omniFinder != null)
      omniFinder.stop();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void tfOmniGoToChange(String newValue, boolean showingMore)
  {
    if (omniSearchMode.getValue() == OmniSearchMode.currentDesc)
    {
      findWithinDesc();
      return;
    }

    if (omniSearchMode.getValue() != OmniSearchMode.asYouType)
      return;

    if ((dontShowOmniTable == false) && strNotNullOrBlank(newValue) && (selectorTabEnum() == omniTabEnum))
      showFindTable();

    if (newValue.isEmpty())
    {
      clearOmniFinder();
      return;
    }

    tvFind.setPlaceholder(new Text("Searching..."));
    omniFinder.setQueryAndStart(newValue, showingMore);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void recordLookup()
  {
    if (selectorTabEnum() == omniTabEnum)
    {
      if (omniSearchMode.getValue() == OmniSearchMode.allFields)
        mnuFindWithinAnyField.fire();
      else
      {
        HDT_Record selectedRecord = tvFind.isVisible() ? htFind.selectedRecord() : null;

        if (selectedRecord != null)
          htFind.doRowAction();
        else if (ctfOmniGoTo.getText().isBlank() == false)
          mnuFindWithinAnyField.fire();
      }

      return;
    }

    int nextID = HyperTableCell.getCellID(hcbGoTo.getTypedMatch());

    if (nextID < 1)
      nextID = hcbGoTo.selectedID();

    if (nextID < 1)
    {
      String text = HyperTableCell.getCellText(hcbGoTo.selectedHTC()).strip();
      if (strNotNullOrBlank(text))
        lblStatus.setText("No results: searched " + getTypeName(selectorType()) + " records for \"" + text + '"');

      return;
    }

    goToRecord(db.records(selectorType()).getByID(nextID), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This is called when the user hits ctrl-F or ctrl-shift-F
   * Also when focus should go to the omni search box after loading tab
   * @param newOmniSearchMode The search mode to activate
   * @param userInitiated Whether the mode is being changed as a result of a keyboard shortcut
   */
  void omniFocus(OmniSearchMode newOmniSearchMode, boolean userInitiated)
  {
    if ((newOmniSearchMode == OmniSearchMode.currentDesc) && (activeTabEnum() == instTabEnum))
      return;

    OmniSearchMode prevOmniSearchMode = omniSearchMode.getValue();

    setSelectorTab(tabOmniSelector);

    omniSearchMode.setValue(newOmniSearchMode);

    if (newOmniSearchMode == OmniSearchMode.currentDesc)
    {
      ctfOmniGoTo.selectAll();
    }
    else
    {
      if (userInitiated && (prevOmniSearchMode != newOmniSearchMode))
      {
        if (prevOmniSearchMode == OmniSearchMode.asYouType)
        {
          hideFindTable();
          clearOmniFinder();
          showToast("Find within all fields");
        }
        else if (prevOmniSearchMode != OmniSearchMode.currentDesc)
        {
          showToast("Find-as-you-type");
          tfOmniGoToChange(ctfOmniGoTo.getText(), false);
        }
      }
    }

    safeFocus(ctfOmniGoTo);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addOmniSearchModeListener()
  {
    omniSearchMode.addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || newValue.equals(oldValue)) return;

      btnTextSearchToggleIsProgrammatic = true;

      switch (newValue)
      {
        case asYouType:

          btnTextSearch.setSelected(false);
          clearOmniFinder();
          tfOmniGoToChange(ctfOmniGoTo.getText(), false);

          break;

        case currentDesc:

          btnTextSearch.setSelected(true);
          String searchText = tfSelector.getText();
          setSelectorTab(tabOmniSelector);
          updateSelectorTab(false);
          hideFindTable();
          ctfOmniGoTo.setText(searchText);
          findWithinDesc();

          break;

        default: break;
      }

      btnTextSearchToggleIsProgrammatic = false;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FadeTransition findToastFadeIn, findToastFadeOut;
  private PauseTransition findToastPause;

  public void showToast(String message)
  {
    lblFindToast.setText(message);
    lblFindToast.setVisible(true);
    lblFindToast.setOpacity(1.0);

    hideFindTable();

    if (findToastFadeIn  != null) findToastFadeIn .stop();
    if (findToastPause   != null) findToastPause  .stop();
    if (findToastFadeOut != null) findToastFadeOut.stop();

    findToastFadeIn = new FadeTransition(Duration.seconds(0.12), lblFindToast);
    findToastFadeIn.setFromValue(0.0);
    findToastFadeIn.setToValue(1.0);

    findToastPause = new PauseTransition(Duration.seconds(0.7));
    findToastPause.setOnFinished(event ->
    {
      findToastFadeOut = new FadeTransition(Duration.seconds(1), lblFindToast);
      findToastFadeOut.setFromValue(1.0);
      findToastFadeOut.setToValue(0.0);
      findToastFadeOut.setOnFinished(evt -> lblFindToast.setVisible(false));
      findToastFadeOut.play();
    });

    findToastFadeIn.setOnFinished(event -> findToastPause.play());
    findToastFadeIn.play();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
