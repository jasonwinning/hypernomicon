/*
 * Copyright 2015-2018 Jason Winning
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

import static java.util.Objects.nonNull;
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
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.queryEngines.QueryEngine.QueryType;
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
import org.hypernomicon.view.tabs.QueriesTabController.QueryView;
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
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.SystemUtils;
import org.apache.tika.mime.MediaType;
import org.jbibtex.ParseException;
import org.jbibtex.TokenMgrException;

import com.google.common.collect.EnumHashBiMap;
import com.melloware.jintellitype.JIntellitype;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.application.Platform;
import javafx.beans.binding.BooleanExpression;
import javafx.collections.FXCollections;
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
  @FXML private BorderPane mainPane;
  @FXML private StackPane stackPane;
  @FXML private ToolBar tbGoTo;
  @FXML private ImageView ivDates;
  @FXML private Label lblProgress;
  @FXML private ProgressBar progressBar;
  @FXML private TabPane tabPane;
  @FXML private TabPane selectorTabPane;
  @FXML private AnchorPane midAnchorPane;
  @FXML private HBox topHBox;
  @FXML private ToolBar topToolBar;
  @FXML private AnchorPane apGoTo;
  @FXML private AnchorPane apOmniGoTo;
  @FXML private AnchorPane apListGoTo;
  @FXML private AnchorPane apStatus;
  @FXML public Tab tabPersons;
  @FXML public Tab tabInstitutions;
  @FXML public Tab tabTree;
  @FXML public Tab tabQueries;
  @FXML public Tab tabPositions;
  @FXML public Tab tabArguments;
  @FXML public Tab tabDebates;
  @FXML public Tab tabTerms;
  @FXML public Tab tabFiles;
  @FXML public Tab tabNotes;
  @FXML public Tab tabWorks;
  @FXML Tab tabOmniSelector;
  @FXML Tab tabViewSelector;
  @FXML private Button btnDecrement;
  @FXML private Button btnIncrement;
  @FXML private ImageView ivLeft;
  @FXML private ImageView ivRight;
  @FXML private SeparatorMenuItem mnuBibImportSeparator;
  @FXML private MenuItem mnuImportBibFile;
  @FXML private MenuItem mnuImportBibClipboard;
  @FXML private MenuItem mnuNewDatabase;
  @FXML private MenuItem mnuCloseDatabase;
  @FXML private MenuItem mnuExitNoSave;
  @FXML private MenuItem mnuChangeID;
  @FXML private MenuItem mnuNewField;
  @FXML private MenuItem mnuNewCountry;
  @FXML private MenuItem mnuNewRank;
  @FXML private MenuItem mnuNewPersonStatus;
  @FXML private MenuItem mnuSaveReloadAll;
  @FXML public Label lblStatus;
  @FXML private Button btnTextSearch;
  @FXML private SplitMenuButton btnGoTo;
  @FXML private Button btnSave;
  @FXML private Button btnDelete;
  @FXML private Button btnRevert;
  @FXML private Button btnCreateNew;
  @FXML private Button btnSearch;
  @FXML private Button btnBack;
  @FXML private Button btnForward;
  @FXML private TextField tfRecord;
  @FXML private TextField tfID;
  @FXML private ComboBox<HyperTableCell> cbGoTo;
  @FXML private TextField tfOmniGoTo;
  @FXML public GridPane gpBottom;
  @FXML private Button btnSaveAll;
  @FXML private Button btnFileMgr;
  @FXML private Button btnBibMgr;
  @FXML private Button btnPreviewWindow;
  @FXML private Button btnMentions;
  @FXML private Button btnAdvancedSearch;
  @FXML public ToggleButton btnPointerLaunch;
  @FXML private ToggleButton btnPointerPreview;
  @FXML private MenuBar menuBar;
  @FXML private Menu mnuFolders;
  @FXML public Menu mnuFavorites;
  @FXML public Menu mnuQueries;
  @FXML private MenuItem mnuToggleFavorite;
  @FXML private MenuItem mnuRecordSelect;
  @FXML private MenuItem mnuFindWithinName;
  @FXML private MenuItem mnuFindWithinAnyField;
  @FXML private MenuItem mnuFindPreviousAll;
  @FXML private MenuItem mnuFindNextAll;
  @FXML private MenuItem mnuFindPreviousInName;
  @FXML private MenuItem mnuFindNextInName;
  @FXML private MenuItem mnuRevertToDiskCopy;
  @FXML private MenuItem mnuAddToQueryResults;
  @FXML TableView<HyperTableRow> tvFind;
  @FXML private AnchorPane apFindBackground;

  public final WindowStack windows = new WindowStack();
  public HyperViewSequence viewSequence;
  public final EnumHashBiMap<TabEnum, Tab> selectorTabs = EnumHashBiMap.create(TabEnum.class);
  public HyperFavorites favorites;
  public OmniFinder omniFinder;  
  public HyperCB hcbGoTo;
  public HyperTable htFind;
  public final ComboBox<TreeRow> cbTreeGoTo = new ComboBox<>();
  private ComboBox<ResultsRow> cbResultGoTo = null;
  private ClickHoldButton chbBack, chbForward;
  public TextField selectorTF = null;
  
  public HDT_Base treeSubjRecord = null, treeObjRecord = null;
  public final List<TreeTargetType> treeTargetTypes = new ArrayList<>();
  
  private double toolBarWidth = 0;
  public Tooltip ttDates;
  private boolean selectorTabChangeIsProgrammatic = false, maximized = false, internetNotCheckedYet = true;
  private double maxWidth = 0.0, maxHeight = 0.0;
  
  public static final String TREE_SELECT_BTN_CAPTION = "Select";
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   
  
  public final ObservableList<ResultsRow> results() { return curQV.resultsTable.getTV().getItems(); }
  public final MenuBar getMenuBar()                 { return menuBar; }
  public final TreeWrapper getTree()                { return TreeTabController.class.cast(getHyperTab(treeTab)).tree; }
  public final Stage primaryStage()                 { return app.getPrimaryStage(); }
  
  @FXML private final void mnuExitClick()           { shutDown(true, true, true); }
  @FXML private final void mnuExitNoSaveClick()     { if (confirmDialog("Abandon changes and quit?")) shutDown(false, true, false); }
  @FXML final void mnuAboutClick()                  { AboutDialogController.create("About " + appTitle).showModal(); }
  @FXML private final void mnuChangeFavOrderClick() { FavOrderDialogController.create("Change Order of Favorites").showModal(); }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void init() throws IOException
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
    
    RecordListView.addDefaultMenuItems(htFind);
    
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
    btnAdvancedSearch.setOnAction(event -> showSearch(false));
    
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
      ImageView graphic = getImageViewForRelativePath(getGraphicRelativePathByType(HyperTab.getRecordTypeByTabEnum(hyperTab.getTabEnum())));
      if (graphic != null) 
      {
        hyperTab.getTab().setGraphic(graphic);
        
        Tab selectorTab = selectorTabs.get(hyperTab.getTabEnum());
        if (selectorTab != null)
          selectorTab.setGraphic(getImageViewForRelativePath(getGraphicRelativePathByType(HyperTab.getRecordTypeByTabEnum(hyperTab.getTabEnum()))));
      }
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

    db.addDeleteHandler((record) -> 
    {
      if (record.getType() == hdtPerson)
      {
        PersonTabController personHyperTab = HyperTab.getHyperTab(personTab);
        if (personHyperTab.activeRecord() == record)
          personHyperTab.curPicture = null;  // User has already been asked if they want to delete the picture; don't ask again
      }
      
      for (QueryView qv : QueriesTabController.class.cast(HyperTab.getHyperTab(queryTab)).queryViews)
        qv.resultsTable.getTV().getItems().removeIf(row -> row.getRecord() == record);

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

      int curRecordNdx = db.records(activeType()).getKeyNdxByID(activeRecord().getID());
      int newRecordNdx = parseInt(tfRecord.getText(), 0) - 1;

      if (newRecordNdx != curRecordNdx)
        if ((newRecordNdx >= 0) && (newRecordNdx < db.records(activeType()).size()))
        {
          goToRecord(db.records(activeType()).getByKeyNdx(newRecordNdx), true);
          return;
        }
      
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
      if (record.getID() == newRecordID) 
        return;
     
      if ((newRecordID > 0) && db.records(activeType()).getIDNdxByID(newRecordID) > -1)
      {
        goToRecord(db.records(activeType()).getByID(newRecordID), true);
        return;
      }

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

  public final void addSelectorTab(TabEnum tabEnum)
  {
    int ndx = selectorTabPane.getTabs().size() - (selectorTabs.size() + 1);
    
    selectorTabs.put(tabEnum, selectorTabPane.getTabs().get(ndx));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final PreviewSource determinePreviewContext()
  {
    if (primaryStage().isFocused())
    {
      switch (activeTab())
      {
        case personTab : return pvsPersonTab;          
        case workTab :   return pvsWorkTab;          
        case queryTab :  return pvsQueryTab;        
        case treeTab :   return pvsTreeTab;
        default :        return pvsOther;                 
      }
    }
    
    if ((fileManagerDlg != null) && fileManagerDlg.getStage().isShowing() && fileManagerDlg.getStage().isFocused())
      return pvsManager;
    
    return pvsOther;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void btnForwardClick()
  {
    if (btnForward.isDisabled()) return;
    if (cantSaveRecord(true)) return;
    
    viewSequence.stepForward();
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void btnBackClick()
  {
    if (btnBack.isDisabled()) return;
    if (cantSaveRecord(true)) return;
    
    viewSequence.stepBack();
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void openPreviewWindow(PreviewSource src)
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

  public final void runFileMgr()
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

  public final void runBibMgr()
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

  public final void adjustToolBar(double anchorWidth)
  {
    if (anchorWidth == 0)
      anchorWidth = midAnchorPane.getWidth();
    
    Point2D p2 = midAnchorPane.localToScreen(anchorWidth, 0);
    Point2D p1 = tabTree.getGraphic().localToScreen(16, 0);
    
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

  @FXML public final void btnTextSearchClick()
  {   
    if (db.isLoaded() == false) return;

    hideFindTable();
    
    currentTab().findWithinDesc(selectorTF.getText());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void btnGoToClick(boolean fromMenu)
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

  public final String getGraphicRelativePath(HDT_Base record)
  {
    switch (record.getType())
    {
      case hdtWork :
                
        WorkTypeEnum workType = HDT_Work.class.cast(record).getWorkTypeValue();
        
        switch (workType)
        {
          case wtBook:            return "resources/images/book.png";
          case wtChapter:         return "resources/images/chapter.png";
          case wtNone:            return "resources/images/unknown.png";
          case wtPaper:           return "resources/images/paper.png";
          case wtRecording:       return "resources/images/recording.png";
          case wtWebPage:         return "resources/images/text-html.png";
          case wtUnenteredSet:    return "resources/images/inbox-document-text.png";
          default:                return "resources/images/unknown.png";     
        }
        
      case hdtMiscFile :
        
        HDT_MiscFile miscFile = (HDT_MiscFile) record;
        
        if (miscFile.getPath().isEmpty() == false)
          return getImageRelPathForFilePath(miscFile.getPath().getFilePath(), null);

        break;
        
      default :
        
        break;
    }
    
    return getGraphicRelativePathByType(record.getType());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final String getGraphicRelativePathByType(HDT_RecordType type)
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

  public final void mnuFindWithinNameClick()
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
  public final void discardLastQuery(boolean backClick)
  {
    QueriesTabController.class.cast(HyperTab.getHyperTab(queryTab)).closeCurrentView();
       
    if (backClick) btnBackClick();
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void shutDown(boolean save, boolean savePrefs, boolean prompt)
  {
    DialogResult result = mrRetry;

    if (db.isLoaded())
    {
      if (save)
      {
        if (cantSaveRecord(false))
          if (prompt)
            if (confirmDialog("Unable to accept most recent changes to this record; however, all or data will be saved. Continue exiting?") == false)             
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
      
      boolean iconified = stage.isIconified();
      boolean fullScreen = stage.isFullScreen();
      boolean maximized;
      
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
    
    return;
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void closeWindows(boolean exitingApp)
  {
    if (fileManagerDlg != null)
      if (fileManagerDlg.getStage().isShowing())
        fileManagerDlg.getStage().close();

    if (bibManagerDlg != null)
      if (bibManagerDlg.getStage().isShowing())
        bibManagerDlg.getStage().close();

    if ((exitingApp == false) || (Environment.isMac() == false))
      if (previewWindow != null)
        if (previewWindow.getStage().isShowing())
          previewWindow.getStage().close();
    
    if (contentsWindow != null)
      if (contentsWindow.getStage().isShowing())
        contentsWindow.getStage().close();
  } 

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void enableAll(boolean enabled)
  {
    boolean disabled = (enabled == false);
    
    gpBottom.getChildren().forEach(node -> node.setDisable(disabled));
    
    apStatus.setDisable(false);
    
    getHyperTabs().forEach(hyperTab -> hyperTab.enable(enabled));
    
    mnuNewDatabase.setDisable(disabled);
    mnuCloseDatabase.setDisable(disabled);
    mnuExitNoSave.setDisable(disabled);
    mnuChangeID.setDisable(disabled);
    mnuNewField.setDisable(disabled);
    mnuNewCountry.setDisable(disabled);
    mnuNewRank.setDisable(disabled);
    mnuNewPersonStatus.setDisable(disabled);    
    mnuSaveReloadAll.setDisable(disabled);
    mnuRevertToDiskCopy.setDisable(disabled);
    mnuAddToQueryResults.setDisable(disabled);
    btnFileMgr.setDisable(disabled);
    btnBibMgr.setDisable(disabled);
    btnPreviewWindow.setDisable(disabled);
    btnMentions.setDisable(disabled);
    btnAdvancedSearch.setDisable(disabled);
    btnSaveAll.setDisable(disabled);
    
    if (disabled) 
      getTree().clear();
      
    hideFindTable();
    
    updateBibImportMenus();
    updateFavorites();
    updateTopicFolders();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public final void updateBibImportMenus()
  {
    mnuImportBibFile.setVisible(db.bibLibraryIsLinked());    
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
  
  public final void initResultCB()
  {
    cbResultGoTo = new ComboBox<ResultsRow>();
    cbResultGoTo.setEditable(true);
    
    copyRegionLayout(cbGoTo, cbResultGoTo);
    
    QueriesTabController queriesTab = HyperTab.getHyperTab(queryTab);
    queriesTab.setCB(cbResultGoTo);
    
    cbResultGoTo.setConverter(new StringConverter<ResultsRow>() 
    {
      @Override public final String toString(ResultsRow row) 
      {
        return nullSwitch(row, "", ResultsRow::getCBText);
      }
  
      @Override public final ResultsRow fromString(String string) 
      {
        if (cbResultGoTo.getItems() == null)
          return new ResultsRow("");
        
        for (ResultsRow row : cbResultGoTo.getItems()) 
          if (string.equals(row.getCBText())) 
            return row;
        
        return new ResultsRow(string);
      }
    });
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public final void updateDatesTooltip(HDT_Base record)
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
      
  public final boolean saveAllToDisk(boolean saveRecord, boolean restartWatcher, boolean updateUI)
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

  @FXML final void mnuSaveReloadAllClick()
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

  public final <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> void refreshTab(HyperTab<HDT_RT, HDT_CT> hyperTab)
  {
    HyperView<HDT_CT> view = hyperTab.getView(); 
    if (view == null) return;

    view.refresh();
    nullSwitch(view.getViewRecord(), record -> hyperTab.setRecord(record));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void mnuOpenClick()
  {
    FileChooser fileChooser = new FileChooser();

    FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter(appTitle + " files (*.hdb)", "*.hdb");
    fileChooser.getExtensionFilters().add(extFilter);
    
    File dir = new File(appPrefs.get(PREF_KEY_SOURCE_PATH, System.getProperty("user.dir")));
    
    if (dir.exists() == false)
      dir = new File(System.getProperty("user.dir"));
    
    fileChooser.setInitialDirectory(dir);

    FilePath filePath = new FilePath(fileChooser.showOpenDialog(primaryStage()));

    if (FilePath.isEmpty(filePath)) return;
    
    if (cantSaveRecord(true)) return;
    
    if (db.isLoaded())
    {
      if (confirmDialog("Save data to XML files?"))
        saveAllToDisk(false, false, false);
      
      closeWindows(false);
      clearAllTabsAndViews();
    }

    appPrefs.put(PREF_KEY_SOURCE_FILENAME, filePath.getNameOnly().toString());
    appPrefs.put(PREF_KEY_SOURCE_PATH, filePath.getDirOnly().toString());

    loadDB();
  }

  //---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public final void mnuNewDatabaseClick()
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

  public final void clearAllTabsAndViews()
  {
    HyperTab.getHyperTabs().forEach(this::clearTab);
    
    previewWindow.clearAll();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final <HDT_RT extends HDT_Base, HDT_CT extends HDT_Base> void clearTab(HyperTab<HDT_RT, HDT_CT> hyperTab)
  {
    HyperTab.setTabView(new HyperView<HDT_CT>(hyperTab.getTabEnum(), null));
    hyperTab.clear();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
   
  @FXML public final void btnSaveClick()
  {
    if (btnSave.getText().equals(TREE_SELECT_BTN_CAPTION))
    {
      treeSelect();
      return;
    }

    if (cantSaveRecord(true)) return;
    update();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @FXML final void btnCreateClick()
  {
    if (cantSaveRecord(true)) return;
    
    HDT_RecordType type = selectorType();
    String name = "";

    if (hcbGoTo.selectedID() == -1)
      name = cbGoTo.getEditor().getText();

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

  public final void deleteCurrentRecord(boolean confirm)
  {    
    HDT_Base record = activeRecord();
    
    if (record == null) return;
        
    switch (record.getType())
    {
      case hdtGlossary :
        
        if (activeTab() != treeTab)
        {
          messageDialog("Glossary records can only be deleted from the tree tab.", mtError);
          return;
        }
        
        HDT_Glossary glossary = (HDT_Glossary) record;
        
        if (glossary.concepts.isEmpty() == false)
        {
          messageDialog("A glossary record can only be deleted if it does not contain any terms.", mtError);
          return;
        }
        
        break;
      
      case hdtNone : case hdtConcept : case hdtFolder : case hdtWorkFile : case hdtHub :
        
        messageDialog("Records of that type cannot be deleted by this method.", mtError);
        return;
        
      default :
        break;
    }
    
    if (confirm) if (confirmDialog("Are you sure you want to delete this record?") == false) return;  
    
    db.deleteRecord(record.getType(), record.getID());

    viewSequence.activateCurrentSlot();
    fileManagerDlg.setNeedRefresh();
  }
   
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @FXML public final void mnuCloseClick()
  {
    if (cantSaveRecord(true)) return;

    if (db.isLoaded())
    {
      if (confirmDialog("Save data to XML files before closing?"))
        saveAllToDisk(false, false, false);
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
  
  public final void updateTopicFolders()
  {
    while (mnuFolders.getItems().size() > 10) // Clear the topical folder items that currently exist
      mnuFolders.getItems().remove(10);

    if (db.isLoaded() == false)
    {
      mnuFolders.setDisable(true);
      return;
    }

    FilePath topicsPath = db.getPath(PREF_KEY_TOPICAL_PATH, null);
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
  
  @FXML public final void mnuChangeIDClick()
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
  
  @FXML public final void mnuNewFieldClick()        { mnuNewCategoryClick(hdtField); }
  @FXML public final void mnuNewRankClick()         { mnuNewCategoryClick(hdtRank); }
  @FXML public final void mnuNewCountryClick()      { mnuNewCategoryClick(hdtCountry); }
  @FXML public final void mnuNewPersonStatusClick() { mnuNewCategoryClick(hdtPersonStatus); }
  
  public final void mnuNewCategoryClick(HDT_RecordType type)
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

  @FXML public final void mnuFolderClick(Event event)
  {
    MenuItem item = (MenuItem)event.getSource();
    int code = parseInt(item.getId(), 0);
    boolean clipboard = ((code % 10) == 1);
    FilePath filePath = null;
    
    code = code / 10;
    
    switch (code)
    {
      case 1 : filePath = db.getPath(PREF_KEY_PAPERS_PATH, null);     break;        
      case 2 : filePath = db.getPath(PREF_KEY_BOOKS_PATH, null);      break;        
      case 3 : filePath = db.getPath(PREF_KEY_UNENTERED_PATH, null);  break;        
      case 4 : filePath = db.getPath(PREF_KEY_TOPICAL_PATH, null);    break;        
      case 5 : filePath = db.getPath(PREF_KEY_PICTURES_PATH, null);   break;        
      case 6 : filePath = db.getPath(PREF_KEY_MISC_FILES_PATH, null); break;        
      case 7 : filePath = new FilePath(appPrefs.get(PREF_KEY_SOURCE_PATH, "")); break;
      case 8 : filePath = db.getPath(PREF_KEY_RESULTS_PATH, null);    break;
    }
    
    if (FilePath.isEmpty(filePath)) return;
    
    if (clipboard)
      copyToClipboard(filePath.toString());
    else
      launchFile(filePath);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final class FavMenuItem extends MenuItem
  {
    public FavMenuItem(HDT_Base record)
    {
      super(db.getTypeName(record.getType()) + ": " + record.listName());
      isQuery = false;
      favRecord = new HyperTableCell(record.getID(), record.listName(), record.getType());
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

  public final void updateFavorites()
  { 
    mnuToggleFavorite.setText("Add to favorites...");
   
    if (db.isLoaded() == false) 
    {
      favorites.clear();
      mnuFavorites.setDisable(true);
      return;
    }
    
    mnuFavorites.setDisable(false);
  
    if (viewRecord() != null)
    {
      mnuToggleFavorite.setDisable(false);
      
      if (favorites.indexOfRecord(activeRecord()) > -1)
        mnuToggleFavorite.setText("Remove from favorites...");
    }
    else
      mnuToggleFavorite.setDisable(true);
  }  
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML final void mnuToggleFavoriteClick()
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

  public final void initPositionContextMenu(HyperTable ht)
  {
    ht.addCondContextMenuItem("Launch work file", HDT_Position.class, 
      pos -> pos.getLaunchableWork() != null,      
      pos -> pos.getLaunchableWork().work.launch(-1));
    
    ht.addCondContextMenuItem("Go to work record", HDT_Position.class, 
      pos -> pos.getWork() != null,      
      pos -> goToRecord(nullSwitch(pos.getLaunchableWork(), pos.getWork()).work, true));
    
    ht.addCondContextMenuItem("Go to person record", HDT_Position.class,
      pos -> pos.getWorkWithAuthor() != null,    
      pos -> goToRecord(pos.getWorkWithAuthor().author, true));
        
    ht.addCondContextMenuItem("Go to argument record", HDT_Position.class, 
      pos -> pos.arguments.size() > 0,
      pos -> goToRecord(nullSwitch(pos.getLaunchableWork(), nullSwitch(pos.getWork(), pos.getArgument())).argument, true));
    
    ht.addContextMenuItem("Go to position record", HDT_Position.class,
      pos -> goToRecord(pos, true));
  }
   
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void updateProgress(String task, double amount)
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

  @FXML public final void mnuFindMentionsClick()
  {
    if (db.isLoaded() == false) return;
        
    searchForMentions(activeRecord(), false);     
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public final void searchForMentions(HDT_Base record, boolean descOnly)
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

  @FXML public final void mnuAddToQueryResultsClick()
  {
    if (db.isLoaded() == false)
    {
      messageDialog("No database is currently loaded.", mtError);
      return;
    }

    HDT_Base record = activeRecord();
       
    if (record == null)
    {
      messageDialog("No record is currently selected.", mtError);
      return;
    }

    if (curQV.resultsBackingList == null)
    {
      curQV.clear();
      curQV.resultsTable.getTV().setItems(FXCollections.observableList(curQV.resultsBackingList));
    }
    
    for (ResultsRow row : curQV.resultsBackingList)
      if (row.getRecord() == record) return;
    
    curQV.addRecord(record, true);      
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  @FXML public final void mnuRevertToDiskCopyClick()
  {    
    if (db.isLoaded() == false)
    {
      messageDialog("No database is currently loaded.", mtError);
      return;
    }

    HDT_Base record = activeRecord();
       
    if (record == null)
    {
      messageDialog("No record is currently selected.", mtError);
      return;
    }

    if (record.hasStoredState() == false)
    {
      messageDialog("Unable to revert: the record may not have been previously saved to disk.", mtError);
      return;
    }
    
    if (confirmDialog("Are you sure you want to revert this record to the last version saved to disk?") == false) return;
    
    HDT_Base viewRecord = viewRecord();
    
    if (revertToDiskCopy(record))
      if (viewRecord != null)
        if (viewRecord != record)
          revertToDiskCopy(viewRecord);
    
    update();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public final boolean revertToDiskCopy(HDT_Base record)
  {
    boolean success = true;
    String recordStr = db.getTypeName(record.getType()) + " \"" + record.getCBText() + "\"";
    
    HDT_RecordState backupState = record.getRecordStateBackup();
    
    try
    {
      record.bringStoredCopyOnline();
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
      record.restoreTo(backupState);
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

  public final void loadDB()
  {
    if (loadDataFromDisk() == false)
    {
      if (db.isLoaded() == false)
        clearAllTabsAndViews();
      
      enableAll(db.isLoaded());
      return;
    }
       
    HyperTab.setTabView(new HyperView<HDT_Person>     (personTab,      db.persons     .getByID(db.prefs.getInt(PREF_KEY_PERSON_ID     , -1))));
    HyperTab.setTabView(new HyperView<HDT_Institution>(institutionTab, db.institutions.getByID(db.prefs.getInt(PREF_KEY_INSTITUTION_ID, -1))));
    HyperTab.setTabView(new HyperView<HDT_Debate>     (debateTab,      db.debates     .getByID(db.prefs.getInt(PREF_KEY_DEBATE_ID     , -1))));
    HyperTab.setTabView(new HyperView<HDT_Position>   (positionTab,    db.positions   .getByID(db.prefs.getInt(PREF_KEY_POSITION_ID   , -1))));
    HyperTab.setTabView(new HyperView<HDT_Argument>   (argumentTab,    db.arguments   .getByID(db.prefs.getInt(PREF_KEY_ARGUMENT_ID   , -1))));
    HyperTab.setTabView(new HyperView<HDT_Work>       (workTab,        db.works       .getByID(db.prefs.getInt(PREF_KEY_WORK_ID       , -1))));
    
    HDT_Term term = db.terms.getByID(db.prefs.getInt(PREF_KEY_TERM_ID, -1));
    HDT_Concept concept = (term != null ? term.concepts.get(0) : null);
    HyperTab.setTabView(new HyperView<HDT_Concept>    (termTab,        concept));
    
    HyperTab.setTabView(new HyperView<HDT_MiscFile>   (miscFileTab,    db.miscFiles   .getByID(db.prefs.getInt(PREF_KEY_FILE_ID       , -1))));
    HyperTab.setTabView(new HyperView<HDT_Note>       (noteTab,        db.notes       .getByID(db.prefs.getInt(PREF_KEY_NOTE_ID       , -1))));      
    HyperTab.setTabView(new HyperView<HDT_Base>       (queryTab,       null));
    HyperTab.setTabView(new HyperView<HDT_Base>       (treeTab,        null));

    enableAll(db.isLoaded());
    
    TabEnum tabEnum = HyperTab.getTabEnumByRecordType(db.parseTypeTagStr(db.prefs.get(PREF_KEY_RECORD_TYPE, "")));

    viewSequence.init();     
    viewSequence.forwardToNewSlot(tabEnum); 
  }
  
//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
  
  public final boolean loadDataFromDisk()
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
    if (nonNull(otherCompName))
    {
      if (LockedDialogController.create("Database is Currently Locked", otherCompName).showModal() == false)
        return false;
      
      if (nonNull(db.getLockOwner()))
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
      
      primaryStage().setTitle(appTitle + " - " + db.getPath("", new FilePath(appPrefs.get(PREF_KEY_SOURCE_FILENAME, ""))));      
    }
    else
      mnuCloseClick();
        
    return success;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final boolean cantSaveRecord(boolean showMessage)
  {    
    if (db.isLoaded() == false) 
      return false;
    
    if ((activeTab() == queryTab) || (activeTab() == treeTab))
      return false;
    
    if (activeRecord() == null) 
      return false;

    CommitableWrapper.commitWrapper(primaryStage().getScene().getFocusOwner());
    
    return currentTab().saveToRecord(showMessage) == false;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public final void goToTreeRecord(HDT_Base record)
  { 
    if (db.isLoaded() == false) return;
    
    if (cantSaveRecord(true)) 
    {
      treeSubjRecord = null;
      treeObjRecord = null;
      treeTargetTypes.clear();
      
      return;
    }
      
    viewSequence.forwardToNewSlotAndView(new HyperView<HDT_Base>(treeTab, record));
  }  
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final TabEnum activeTab()                       { return viewSequence.isEmpty() ? personTab : viewSequence.curTabEnum(); }  
  public final HyperTab<? extends HDT_Base, 
                        ? extends HDT_Base> currentTab() { return viewSequence.isEmpty() ? null : viewSequence.curHyperTab(); }
  public final HDT_RecordType activeType()               { return viewSequence.isEmpty() ? hdtPerson : viewSequence.curHyperView().getTabRecordType(); }  
  public final HDT_Base activeRecord()                   { return viewSequence.isEmpty() ? null : currentTab().activeRecord(); }
  public final HDT_Base viewRecord()                     { return viewSequence.isEmpty() ? null : currentTab().viewRecord(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void goToFileInManager(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;
    
    runFileMgr();
    fileManagerDlg.goToFilePath(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void goToWorkInBibManager(HDT_Work work)
  {
    runBibMgr();
    bibManagerDlg.goToWork(work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final HDT_RecordWithConnector getSpokeToGoTo(StrongLink link)
  {
    if (link == null) return null;
    
    HDT_RecordWithConnector spoke = link.getConcept();
    if (spoke == null) spoke = link.getDebate();
    if (spoke == null) spoke = link.getPosition();
    if (spoke == null) spoke = link.getNote();
    if (spoke == null) spoke = link.getLabel();
    
    return spoke;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void goToRecord(HDT_Base record, boolean save)
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

  public final void update()
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

  public final TabEnum selectorTabEnum()
  {
    Tab tab = selectorTabPane.getSelectionModel().getSelectedItem();    
    return selectorTabs.inverse().get(tab);
  }
  
  public final HDT_RecordType selectorType()
  {
    TabEnum tabEnum = selectorTabEnum();
    
    switch (tabEnum)
    {
      case listTab: case omniTab :
        return activeType();
        
      default :
        break;
    }
    
    return HyperTab.getRecordTypeByTabEnum(tabEnum);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @SuppressWarnings("cast")
  public final void updateSelectorTab(boolean setFocus)
  {    
    TabEnum tabEnum = selectorTabEnum();
    HyperTab<? extends HDT_Base, ? extends HDT_Base> hyperTab = HyperTab.getHyperTab(tabEnum);
    if (hyperTab == null) hyperTab = currentTab();
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
        
        if (count > 0)         // HDT_Base cast added to avoid Maven compile errors
          hcbGoTo.addAndSelectEntryOrBlank((HDT_Base)nullSwitch(hyperTab, null, HyperTab::activeRecord), HDT_Base::listName);
        
        break;
    }
    
    hideFindTable();
    
    if (setFocus && (selectorTF != null))
    {
      Platform.runLater(() -> 
      {        
        selectorTF.requestFocus();
        selectorTF.selectAll();                
      });
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void attachOrphansToRoots()
  {
    Set<HDT_Position> orphans2 = db.getOrphans(rtParentPosOfPos, HDT_Position.class);
    
    db.getOrphans(rtDebateOfPosition, HDT_Position.class).forEach(position ->
    {
      if (orphans2.contains(position))
        position.debates.add(db.debates.getByID(1));
    });

    db.getOrphans(rtParentDebateOfDebate, HDT_Debate     .class).forEach(debate -> debate.largerDebates.add(db.debates     .getByID(1)));
    db.getOrphans(rtParentNoteOfNote    , HDT_Note       .class).forEach(note   -> note  .parentNotes  .add(db.notes       .getByID(1)));    
    db.getOrphans(rtParentLabelOfLabel  , HDT_WorkLabel  .class).forEach(label  -> label .parentLabels .add(db.workLabels  .getByID(1)));    
    db.getOrphans(rtParentGroupOfGroup  , HDT_PersonGroup.class).forEach(group  -> group .parentGroups .add(db.personGroups.getByID(1)));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void updateBottomPanel(boolean refreshDropDown)
  {
    ttDates.setText("No dates to show.");
    if (db.isLoaded() == false) return;

    HyperTab<? extends HDT_Base, ? extends HDT_Base> curTab = currentTab();
    if (curTab == null) return;
    
    int count = curTab.getRecordCount();
    int ndx = curTab.getRecordNdx();

    attachOrphansToRoots();
    
    btnTextSearch.setDisable(false);
    
  //---------------------------------------------------------------------------
  // Query-specific stuff
  //---------------------------------------------------------------------------
    
    if (activeTab() == queryTab)
    {
      btnSave.setDisable(true);
      btnSave.setText("Accept Edits");
      btnDelete.setDisable(activeRecord() == null);
      btnRevert.setDisable(true);
      btnRevert.setText("Revert");
      btnIncrement.setDisable(true);
      btnDecrement.setDisable(true);
    }
    
  //---------------------------------------------------------------------------
  // Tree-specific stuff  
  //---------------------------------------------------------------------------
    
    else if (activeTab() == treeTab)
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
      btnIncrement.setDisable(true);
      btnDecrement.setDisable(true);

      if (count > 0)
      {
        btnDecrement.setDisable(false);
        btnIncrement.setDisable(false);        
      }
      
      btnDelete.setDisable(activeRecord() == null);
    }
    
  //---------------------------------------------------------------------------
  // Single-record-tab-specific stuff  
  //---------------------------------------------------------------------------
    
    else
    {         
      switch (activeType())
      {
        case hdtArgument: case hdtDebate:   case hdtMiscFile: case hdtNote:
        case hdtPerson:   case hdtPosition: case hdtTerm:     case hdtWork:
          
          btnTextSearch.setDisable(false); break;
          
        default:
          
          btnTextSearch.setDisable(true);
      }
           
      btnSave.setText("Accept Edits");

      if (activeRecord() != null)
      {
        btnDelete.setDisable(false);
        btnSave.setDisable(false);
      }
      else
      {
        btnDelete.setDisable(true);
        btnSave.setDisable(true);
      }

      btnRevert.setText("Revert");
//      if (changed)
      btnRevert.setDisable(false);
//      else
//        btnRevert->Enabled = false;

      if ((count == 0) || (ndx == 0))
        btnDecrement.setDisable(true);
      else
        btnDecrement.setDisable(false);

      if ((count == 0) || (ndx == (count - 1)))
        btnIncrement.setDisable(true);
      else
        btnIncrement.setDisable(false);
    }

  //---------------------------------------------------------------------------
  // General stuff  
  //---------------------------------------------------------------------------
    
    if (count > 0)
      tfRecord.setText((ndx + 1) + " of " + count);
    else
      tfRecord.setText("");
    
    if (activeRecord() != null)
      tfID.setText(String.valueOf(activeRecord().getID()));
    else
      tfID.setText("");

    updateDatesTooltip(activeRecord());
    updateFavorites();

    if (refreshDropDown)
      updateSelectorTab(false);
    else
      hideFindTable();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final boolean showSearch(boolean doSearch)
  {
    return showSearch(doSearch, null, -1, null, null, null, "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final boolean showSearch(boolean doSearch, QueryType type, int query, QueryFavorite fav, HyperTableCell op1, HyperTableCell op2, String caption)
  { 
    if (cantSaveRecord(true)) return false;
  
    QueriesTabController queriesTab = (QueriesTabController) HyperTab.getHyperTab(queryTab);
         
    viewSequence.forwardToNewSlotAndView(new HyperView<HDT_Base>(queryTab, queriesTab.activeRecord(), queriesTab.getMainTextInfo()));
    
    boolean result = queriesTab.showSearch(doSearch, type, query, fav, op1, op2, caption);
    updateFavorites();
    
    return result;
  }  
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void recordLookup()
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

  public final void incDecClick(boolean increment)
  {
    if (activeTab() == treeTab)
    {
      getTree().selectNextInstance(increment);
      return;
    }
  
    int ndx = db.records(activeType()).getKeyNdxByID(activeRecord().getID());
  
    if (increment)
    {
      ndx++;
      if (ndx >= db.records(activeType()).size()) return;
    }
    else
    {
      if (ndx <= 0) return;
      ndx--;
    }
      
    goToRecord(db.records(activeType()).getByKeyNdx(ndx), true);
  }
   
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void treeSelect()
  {
    if (treeSubjRecord == null)
    {
      messageDialog("Internal error #91827", mtError);
      return;
    }

    HDT_Base record = nullSwitch(getTree().selectedItem(), null, treeItem -> 
                      nullSwitch(treeItem.getValue(), null, row -> 
                      row.getRecord()));
    
    if (record == null) return;

    RelationType relType = rtNone;
    
    for (TreeTargetType ttType : treeTargetTypes)
      if (ttType.objType == record.getType())
        relType = ttType.relType;
    
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
  
  public final boolean treeSelectToUnite(HDT_RecordWithConnector record2)
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

  public final void uniteRecords(HDT_RecordWithConnector record1, HDT_RecordWithConnector record2)
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
    {
      goToRecord(record1, false);
      return;      
    }

    update();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public final void setSelectorTab(Tab selectorTab)
  {
    selectorTabChangeIsProgrammatic = true;
    selectorTabPane.getSelectionModel().select(selectorTab);
    selectorTabChangeIsProgrammatic = false;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public final void tfOmniGoToChange(String newValue, boolean showingMore)
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

  public final void omniFocus()
  {
    setSelectorTab(tabOmniSelector);    
    safeFocus(tfOmniGoTo);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
 
  public final void showFindTable()
  {
    tvFind.setVisible(true);
    apFindBackground.setMouseTransparent(false);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public final void hideFindTable()
  {    
    apFindBackground.setMouseTransparent(true);
    tvFind.setVisible(false);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void handleArgs(List<String> args)
  {
    if (db.isLoaded() == false) return;
    if (args == null) return;
    if (args.size() < 1) return;    
    if (windows.getOutermostModality() != Modality.NONE) return;
    
    FilePath filePath = new FilePath(args.get(0));
    
    MediaType mediaType = getMediaType(filePath);
    
    if (mediaType.toString().contains("pdf"))
    {
      if (cantSaveRecord(true)) return;
      
      HDT_Work work = db.createNewBlankRecord(hdtWork);

      goToRecord(work, false);
      
      WorkTabController workCtrlr = HyperTab.getHyperTab(workTab);
      
      if (workCtrlr.showWorkDialog(null, filePath) == false)
        deleteCurrentRecord(false);

      return;
    }
       
    if (mediaType.toString().contains("text") == false)
    {
      messageDialog("Unable to import file: " + filePath.toString(), mtError);
      return;
    }
    
    importBibFile(null, filePath);
  }
    
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
    
  public final void importBibFile(List<String> lines, FilePath filePath)
  {
    if (cantSaveRecord(true)) return;
    
    ImportBibEntryDialogController ibed = ImportBibEntryDialogController.create("Import Bibliography File", lines, filePath);
    
    if (ibed.getFailedToLoad()) return;
    
    if (!ibed.showModal()) return;
    
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
      mwd = MergeWorksDialogController.create("Import Into Existing Work Record", workBibData, bd, null, null, creatingNewWork, creatingNewEntry);
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
    
    if ((filePath == null) || (ibed.getDeleteFile() == false))
      return;
    
    filePath.deletePromptOnFail(true);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  @FXML public final void mnuSettingsClick()        
  { 
    if (db.isLoaded())
      if (cantSaveRecord(true)) return;
    
    OptionsDialogController.create(appTitle + " Settings").showModal();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

}