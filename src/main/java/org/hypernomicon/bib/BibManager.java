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

package org.hypernomicon.bib;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import static java.util.Objects.*;

import java.io.IOException;
import java.util.*;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

import com.google.common.collect.EnumHashBiMap;
import com.google.common.collect.Ordering;

import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.control.textfield.TextFields;
import org.hypernomicon.bib.CollectionTree.BibCollectionType;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.LibraryWrapper.SyncTask;
import org.hypernomicon.bib.data.BibDataRetriever;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.dialogs.SelectWorkDlgCtrlr;
import org.hypernomicon.dialogs.base.NonmodalWindow;
import org.hypernomicon.dialogs.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.wrappers.*;

import javafx.animation.*;
import javafx.application.Platform;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.concurrent.Worker.State;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.input.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebView;
import javafx.util.Duration;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

public final class BibManager extends NonmodalWindow
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apRelated, apSelecting;
  @FXML private BorderPane borderPane;
  @FXML private Button btnCreateNew, btnAutofill, btnViewInRefMgr, btnAssign, btnUnassign, btnDelete, btnMainWindow, btnPreviewWindow, btnStop, btnSync, btnUpdateRelatives;
  @FXML private CheckBox chkRequireByDefault;
  @FXML private ComboBox<EntryType> cbNewType;
  @FXML private Label lblSelect, lblSelecting;
  @FXML private ProgressBar progressBar;
  @FXML private SplitPane spMain;
  @FXML private TableView<BibEntryRow> tableView;
  @FXML private TableView<HyperTableRow> tvRelatives;
  @FXML private TextField tfSearch;
  @FXML private TitledPane tpRelated;
  @FXML private ToolBar toolBar, toolBar2;
  @FXML private TreeView<BibCollectionRow> treeView;
  @FXML private WebView webView;

  private static final String dialogTitle = "Bibliographic Entry Manager";
  private static final AsyncHttpClient httpClient = new AsyncHttpClient();

  private static BibManager instance;

  private final HyperTable htRelatives;
  private final BibEntryTable entryTable;
  private final CollectionTree collTree;
  private final CustomTextField searchField;
  private final ToolBarWrapper toolBarWrapper;

  public static final Property<HDT_Work> workRecordToAssign = new SimpleObjectProperty<>();

  private LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> libraryWrapper = null;
  private BibDataRetriever bibDataRetriever = null;
  private SyncTask syncTask = null;

//---------------------------------------------------------------------------

  private void hideBottomControls()                      { setAllVisible(false, lblSelect, btnCreateNew, cbNewType); borderPane.setTop(null); }
  private void viewInRefMgr()                            { viewInRefMgr(tableView.getSelectionModel().getSelectedItem().getEntry()); }
  private void rebuildCollectionTree()                   { collTree.rebuild(libraryWrapper.getKeyToColl()); }
  private void clearCollectionTree()                     { collTree.clear(); }

  private static void viewInRefMgr(BibEntry<?, ?> entry) { DesktopUtil.openWebLink(entry.getURLtoViewEntryInRefMgr()); }
  public  static void close(boolean exitingApp)          { close(instance, exitingApp); }

//---------------------------------------------------------------------------

  public static BibManager instance()
  {
    if (instance == null) instance = new BibManager();

    return instance;
  }

//---------------------------------------------------------------------------

  private BibManager()
  {
    super("bib/BibManager", dialogTitle, PrefKey.BM_WINDOW_X, PrefKey.BM_WINDOW_Y, PrefKey.BM_WINDOW_WIDTH, PrefKey.BM_WINDOW_HEIGHT);

    entryTable = new BibEntryTable(tableView, this);
    collTree = new CollectionTree(treeView);

    searchField = setupSearchField();

    toolBarWrapper = new ToolBarWrapper(toolBar);
    toolBarWrapper.setVisible(false, btnUnassign, progressBar, btnDelete, btnPreviewWindow); // Latter 2 are not yet supported. (Not sure if the preview button is needed?)

    btnAssign      .setOnAction(event -> assign  (tableView.getSelectionModel().getSelectedItem()));
    btnUnassign    .setOnAction(event -> unassign(tableView.getSelectionModel().getSelectedItem()));

    btnMainWindow  .setOnAction(event -> ui.windows.focusStage(ui.getStage()));
    btnSync        .setOnAction(event -> sync());
    btnStop        .setOnAction(event -> stop());
    btnAutofill    .setOnAction(event -> autofill());
    btnViewInRefMgr.setOnAction(event -> viewInRefMgr());

    Supplier<String> viewEntryInRefMgrCaptionSupplier = () -> "View this entry in " + libraryWrapper.getUserFriendlyName();

    setToolTip(btnMainWindow, "Return to main application window");
    setToolTip(btnAutofill, MainCtrlr.AUTOFILL_TOOLTIP);
    setToolTip(btnViewInRefMgr, viewEntryInRefMgrCaptionSupplier);

    btnUpdateRelatives.disableProperty().bind(btnStop.disabledProperty().not());

    workRecordToAssign.addListener((ob, oldValue, newValue) ->
    {
      if (newValue != oldValue)
      {
        cbNewType.getSelectionModel().clearSelection();

        if (newValue != null)
        {
          WorkTypeEnum workTypeEnum = newValue.getWorkTypeEnum();

          if (EnumSet.of(wtBook, wtChapter, wtThesis, wtWebPage, wtPaper).contains(workTypeEnum))
          {
            EntryType entryType = fromWorkType(workTypeEnum);
            if (libraryWrapper.getEntryTypeMap().containsKey(entryType))
              cbNewType.getSelectionModel().select(fromWorkType(workTypeEnum));
          }
        }
      }

      if ((newValue != null) && newValue.getBibEntryKey().isEmpty())
      {
        borderPane.setTop(apSelecting);

        String workStr = newValue.getCBText();

        lblSelecting.setText("Select an entry that is not already assigned to a work and click the Assign to Work Record button to assign to " + workStr + ". Or, select an entry type below and click the Create New button.");

        lblSelect.setText("Assigning to work record: " + newValue.getCBText());
        setAllVisible(true, lblSelect, btnCreateNew, cbNewType);
      }
      else
      {
        hideBottomControls();
      }
    });

    btnCreateNew.setOnAction(event -> btnCreateNewClick());
    btnUpdateRelatives.setOnAction(event -> btnUpdateRelativesClick());

    // In the next line, the refresh is done in a runLater because we might already be in a refresh when this is happening

    tableView.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) -> Platform.runLater(this::doRefresh));

    entryTable.addContextMenuItem(viewEntryInRefMgrCaptionSupplier, row -> strNotNullOrBlank(row.getURLtoViewEntryInRefMgr()), row -> viewInRefMgr(row.getEntry()));

    entryTable.addContextMenuItem("Go to work record", HDT_Work.class, work -> ui.goToRecord(work, true));

    entryTable.addContextMenuItem("Unassign work record" , row -> nonNull(row.getWork()), BibManager::unassign);
    entryTable.addContextMenuItem("Assign to work record", row -> isNull (row.getWork()), BibManager::assign);

    entryTable.addContextMenuItem("Launch work", HDT_Work.class, HDT_Work::canLaunch, work -> work.launch(-1));

    entryTable.addContextMenuItem("Show in Preview Window", HDT_Work.class, HDT_Work::canPreview, work -> PreviewWindow.show(ui.determinePreviewContext(), work));

    tpRelated.setExpanded(false);
    tpRelated.heightProperty().addListener((ob, ov, nv) -> AnchorPane.setTopAnchor(webView, nv.doubleValue()));

    htRelatives = new HyperTable(tvRelatives, 2, false, "");
    htRelatives.addLabelCol(hdtWork, smTextSimple);
    htRelatives.addIconCol();
    htRelatives.addLabelCol(hdtWork, smTextSimple);
    htRelatives.setDblClickHandler(HDT_Work.class, work -> goToWork(work, false));

    htRelatives.addDefaultMenuItems();

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) -> handleJSEvent("", webView.getEngine()));

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.setOnDragOver   (Event::consume);
    webView.setOnDragDropped(Event::consume);

    webViewAddZoom(webView, ZoomPrefKey.BIBMGR);

//---------------------------------------------------------------------------

    onShown = () ->
    {
      if (shownAlready() == false)
      {
        hideBottomControls();

        entryTable.clear();

        collTree.selectAllEntries();
      }

      refresh();

      safeFocus(searchField);
    };

//---------------------------------------------------------------------------

    treeView.getSelectionModel().selectedItemProperty().addListener((ob, oldTreeItem, newTreeItem) ->
    {
      if (newTreeItem == null)
      {
        entryTable.clear();
        return;
      }

      if (newTreeItem != oldTreeItem)
      {
        searchField.clear();
        entryTable.update(getViewForTreeItem(newTreeItem));
      }
    });

//---------------------------------------------------------------------------

    stage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus() || (Boolean.TRUE.equals(newValue) == false))
        return;

      refresh();
    });

//---------------------------------------------------------------------------

    db.addBibChangedHandler(() ->
    {
      setLibrary(db.getBibLibrary());

      if ((db.bibLibraryIsLinked() == false) && stage.isShowing())
        stage.close();

      ui.updateBibImportMenus();

      if (db.isLoaded())
        ui.update();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private CustomTextField setupSearchField()
  {
    CustomTextField ctf = (CustomTextField) TextFields.createClearableTextField();
    ctf.setPromptText("Title, Author, Year, Published In, Publisher");
    ImageView imageView = imgViewFromRelPath("resources/images/magnifier.png");
    imageView.setFitHeight(16);
    imageView.setPreserveRatio(true);
    ctf.setLeft(imageView);

    copyRegionLayout(tfSearch, ctf);

    toolBar2.getItems().remove(tfSearch);
    toolBar2.getItems().add(ctf);

    ctf.textProperty().addListener((obs, ov, nv) -> entryTable.filter(nv, chkRequireByDefault.isSelected()));

    chkRequireByDefault.setSelected(app.prefs.getBoolean(PrefKey.BIB_SRCH_REQUIRE_BY_DEFAULT, false));

    chkRequireByDefault.setOnAction(event ->
    {
      app.prefs.putBoolean(PrefKey.BIB_SRCH_REQUIRE_BY_DEFAULT, chkRequireByDefault.isSelected());
      entryTable.filter(ctf.getText(), chkRequireByDefault.isSelected());
    });

    KeyCombination keyComb = new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN);
    stage.addEventHandler(KeyEvent.KEY_PRESSED, event ->
    {
      if (keyComb.match(event))
        safeFocus(ctf);
    });

    Tooltip tooltip = new WebTooltip("""
      Use this search field to search entries in your reference manager.<br><br>
      In order to match a phrase, put it in double quotes.<br><br>
      If a search term should be required, prefix it with + (plus sign).<br><br>
      If a search term should be excluded, prefix it with - (minus sign).<br><br>
      For example, if you want to find all entries that include Smith and the phrase Journal of,<br>
      and that don't include Philosophy, you can enter:<br>
      <blockquote><code>+Smith +&quot;Journal of&quot; -Philosophy</code></blockquote>
      Select [Implicit +] to treat all terms as required unless prefixed by minus sign.<br><br>
      The search is not case-sensitive.""");

    ctf.setTooltip(tooltip);
    chkRequireByDefault.setTooltip(tooltip);

    return ctf;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setLibrary(LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> libraryWrapper)
  {
    this.libraryWrapper = libraryWrapper;

    if (libraryWrapper == null)
    {
      clearCollectionTree();
    }
    else
    {
      setToolTip(btnSync, "Synchronize with " + libraryWrapper.getUserFriendlyName());
      initEntryTypeCB(cbNewType);
      rebuildCollectionTree();
    }

    if (shownAlready() == false) return;

    stop();

    entryTable.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void sync()
  {
    if (ui.cantSaveRecord()) return;

    if (btnStop.isDisabled() == false) return;

    stop();

    try
    {
      syncTask = libraryWrapper.createNewSyncTask("Syncing...");
    }
    catch (HyperDataException e)
    {
      errorPopup(LibraryWrapper.syncErrorMessage(e));
      return;
    }

    RotateTransition rt1 = new RotateTransition(Duration.millis(1000), btnSync.getGraphic());
    rt1.setByAngle(360);
    rt1.setCycleCount(1);
    rt1.setInterpolator(Interpolator.EASE_IN);

    RotateTransition rt2 = new RotateTransition(Duration.millis(1000), btnSync.getGraphic());
    rt2.setByAngle(360);
    rt2.setCycleCount(Animation.INDEFINITE);
    rt2.setInterpolator(Interpolator.LINEAR);

    SequentialTransition seqT = new SequentialTransition(rt1, rt2);
    seqT.play();

    btnStop.setDisable(false);

    libraryWrapper.setKeyChangeHandler(entryTable::updateKey);

    syncTask.addDoneHandler(state ->
    {
      stop();
      seqT.stop();

      ImageView iv1 = imgViewFromRelPath("resources/images/refresh.png");
      iv1.setFitWidth(16);
      iv1.setFitHeight(16);
      btnSync.setGraphic(iv1);

      if (state == State.CANCELLED)
      {
        // If the task failed, and it was a HyperDataException, a popup is displayed by HyperTask.InnerTask.failed()

        Throwable e = syncTask.getException();

        if (e instanceof HyperDataException)
          errorPopup(e);
      }

      boolean changed = syncTask.getChanged();
      syncTask = null;

      if (changed == false) return;

      collTree.refresh(libraryWrapper.getKeyToColl());
      refresh();

      ui.update();
      ui.saveAllToXML(true, true, true, false);
    });

    syncTask.startWithNewThreadAsDaemon();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stop()
  {
    if (bibDataRetriever != null)
      bibDataRetriever.stop();

    toolBarWrapper.setVisible(false, progressBar);

    btnStop.setDisable(true);
    btnSync.setDisable(false);

    if (libraryWrapper != null)
      libraryWrapper.stop();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void autofill()
  {
    if (ui.cantSaveRecord(false)) return;

    stop();

    btnStop.setDisable(false);
    btnSync.setDisable(true);
    toolBarWrapper.setVisible(true, progressBar);

    BibEntry<?, ?> entry = tableView.getSelectionModel().getSelectedItem().getEntry();

    List<FilePath> pdfFilePaths;

    if (entry.linkedToWork())
    {
      pdfFilePaths = entry.getWork().workFiles.stream().filter(HDT_WorkFile::pathNotEmpty)
                                                       .map(HDT_WorkFile::filePath)
                                                       .toList();
      if (collEmpty(pdfFilePaths))
        pdfFilePaths = safeListOf(db.resolveExtFilePath(entry.getWork().getURL()));
    }
    else
      pdfFilePaths = Collections.emptyList();

    bibDataRetriever = new BibDataRetriever(httpClient, entry, pdfFilePaths, (pdfBD, queryBD, messageShown) ->
    {
      stop();

      if ((pdfBD == null) && (queryBD == null))
      {
        if (messageShown == false)
          infoPopup("Unable to find bibliographic information.");

        return;
      }

      MergeWorksDlgCtrlr mwd;

      try
      {
        mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", Stream.of(entry, pdfBD, queryBD), entry.getWork(), false, false, Ternary.False);
      }
      catch (IOException e)
      {
        errorPopup("Unable to initialize merge dialog window.");
        return;
      }

      if (mwd.showModal() == false) return;

      mwd.mergeInto(entry);

      refresh();
      ui.update();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void unassign(BibEntryRow row)
  {
    if (confirmDialog("Are you sure you want to unassign the work record?", false) == false) return;
    if (ui.cantSaveRecord()) return;

    HDT_Work work = row.getWork();

    row.getEntry().unassignWork();

    if (workRecordToAssign.getValue() == work)
    {
      workRecordToAssign.setValue(null);
      workRecordToAssign.setValue(work);
    }

    refresh();
    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum BibEntryRelation { Parent, Sibling, Child }

  public record RelatedBibEntry(BibEntryRelation relation, BibEntry<?, ?> entry) { }

//---------------------------------------------------------------------------

  private void updateRightPane()
  {
    htRelatives.clear();

    BibEntryRow curRow = null;

    if (libraryWrapper == null)
      webView.getEngine().loadContent("");
    else
    {
      curRow = tableView.getSelectionModel().getSelectedItem();
      webView.getEngine().loadContent(nullSwitch(nullSwitch(curRow, null, BibEntryRow::getEntry), "", entry -> entry.createReport(true)));
    }

    List<RelatedBibEntry> list = getRelativesForRow(curRow);

    if (list.isEmpty())
    {
      tpRelated.setExpanded(false);
      tpRelated.setDisable(true);
      return;
    }

    tpRelated.setDisable(false);

    htRelatives.buildRows(list, (row, relative) ->
    {
      row.setCellValue(0, relative.entry.getWork(), relative.relation.name());
      row.setCellValue(1, relative.entry.getWork(), "");
      row.setCellValue(2, relative.entry.getWork(), relative.entry.getWork().getCBText());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnUpdateRelativesClick()
  {
    if (ui.cantSaveRecord()) return;

    BibEntryRow curRow = tableView.getSelectionModel().getSelectedItem();

    List<RelatedBibEntry> list = getRelativesForRow(curRow);

    if (list.isEmpty()) return;

    BibEntry<?, ?> entry = curRow.getEntry();

    list.forEach(relative -> updateRelative(relative, entry));

    refresh();
    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void updateRelative(RelatedBibEntry relative, BibEntry<?, ?> entry)
  {
    relative.entry.setStr(bfVolume   , entry.getStr(bfVolume   ));
    relative.entry.setStr(bfPublisher, entry.getStr(bfPublisher));
    relative.entry.setStr(bfPubLoc   , entry.getStr(bfPubLoc   ));
    relative.entry.setStr(bfEdition  , entry.getStr(bfEdition  ));

    switch (relative.relation)
    {
      case Child:

        relative.entry.setMultiStr(bfContainerTitle, entry.getMultiStr(bfTitle));
        break;

      case Parent:

        relative.entry.setMultiStr(bfTitle, entry.getMultiStr(bfContainerTitle));
        break;

      case Sibling:

        relative.entry.setMultiStr(bfContainerTitle, entry.getMultiStr(bfContainerTitle));
        break;
    }

    entry.syncBookAuthorsTo(relative);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<RelatedBibEntry> getRelativesForRow(BibEntryRow row)
  {
    List<RelatedBibEntry> list = new ArrayList<>();

    HDT_Work work = nullSwitch(row, null, BibEntryRow::getWork);

    if ((libraryWrapper == null) || (work == null))
      return list;

    BibEntry<?, ?> entry = row.getEntry();

    if (entry.getEntryType().isChild() && work.largerWork.isNotNull())
    {
      HDT_Work parentWork = work.largerWork.get();
      if (parentWork.getBibEntryKey().isBlank() == false)
      {
        BibEntry<?, ?> parentEntry = libraryWrapper.getEntryByKey(parentWork.getBibEntryKey());
        if (parentEntry.getEntryType().isParent())
        {
          list.add(new RelatedBibEntry(BibEntryRelation.Parent, parentEntry));

          addChildren(work, parentWork.subWorks, BibEntryRelation.Sibling, list);
        }
      }
    }

    if (entry.getEntryType().isParent())
      addChildren(work, work.subWorks, BibEntryRelation.Child, list);

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addChildren(HDT_Work self, Iterable<HDT_Work> subWorks, BibEntryRelation relation, Collection<RelatedBibEntry> list)
  {
    subWorks.forEach(childWork ->
    {
      if ((childWork == self) || childWork.getBibEntryKey().isBlank()) return;

      BibEntry<?, ?> childEntry = libraryWrapper.getEntryByKey(childWork.getBibEntryKey());
      if (childEntry.getEntryType().isChild())
        list.add(new RelatedBibEntry(relation, childEntry));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnCreateNewClick()
  {
    EntryType et = cbNewType.getValue();
    if (et == null)
    {
      falseWithWarningPopup("You must select an entry type.", cbNewType);
      return;
    }

    if (ui.cantSaveRecord()) return;

    HDT_Work work = workRecordToAssign.getValue();
    BibEntry<?, ?> entry = libraryWrapper.addEntry(et);

    work.setBibEntryKey(entry.getKey());

    if (et.isChild() && work.largerWork.isNotNull() && (work.largerWork.get().getBibEntryKey().isBlank() == false))
    {
      BibEntry<?, ?> parentEntry = libraryWrapper.getEntryByKey(work.largerWork.get().getBibEntryKey());

      if (parentEntry.getEntryType().isParent())
        updateRelative(new RelatedBibEntry(BibEntryRelation.Child, entry), parentEntry);
    }

    workRecordToAssign.setValue(null);

    refresh();

    ui.update();

    goToWork(work, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assign(BibEntryRow row)
  {
    if (ui.cantSaveRecord(false)) return;

    SelectWorkDlgCtrlr dlg = new SelectWorkDlgCtrlr(workRecordToAssign.getValue(), row.getEntry());

    if (dlg.showModal() == false) return;

    HDT_Work work = dlg.getWork();

    if (work == null)
    {
      work = db.createNewBlankRecord(hdtWork);

      work.getBibData().copyAllFieldsFrom(row.getEntry(), false, false);
      work.getAuthors().setAll(row.getEntry().getAuthors());
      work.setBibEntryKey(row.getEntry().getKey());

      ui.goToRecord(work, false);
      return;
    }

    assignToExisting(work, row.getEntry());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assignToExisting(HDT_Work work, BibEntry<?, ?> entry)
  {
    if ((work == null) || (entry == null)) return;

    MergeWorksDlgCtrlr mwd;

    try
    {
      mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", Stream.of(work.getBibData(), entry), work, false, false, Ternary.False);
    }
    catch (IOException e)
    {
      errorPopup("Unable to initialize merge dialog window.");
      return;
    }

    if (mwd.showModal() == false) return;

    work.setBibEntryKey(entry.getKey());
    mwd.mergeInto(entry);

    workRecordToAssign.setValue(null);
    refresh();

    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Stream<? extends BibEntry<?, ?>> getViewForTreeItem(TreeItem<BibCollectionRow> item)
  {
    BibCollectionRow row = nullSwitch(item, null, TreeItem::getValue);
    BibCollectionType type = nullSwitch(row, null, BibCollectionRow::getType);

    return type == null ? Stream.empty() : switch (type)
    {
      case bctAll           -> libraryWrapper.getNonTrashEntries();
      case bctAllAssigned   -> libraryWrapper.getNonTrashEntries().filter(BibEntry::linkedToWork);
      case bctAllUnassigned -> libraryWrapper.getNonTrashEntries().filter(Predicate.not(BibEntry::linkedToWork));
      case bctTrash         -> libraryWrapper.getTrash().stream();
      case bctUser          -> libraryWrapper.getCollectionEntries(row.getKey());
      default               -> libraryWrapper.getUnsorted();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void goToWork(HDT_Work work, boolean setAssigning)
  {
    if (HDT_Work.isUnenteredSet(work))
    {
      workRecordToAssign.setValue(null);
      return;
    }

    String key = work.getBibEntryKey();

    workRecordToAssign.setValue(setAssigning && key.isEmpty() ? work : null);

    if (key.isEmpty()) return;

    if (entryTable.containsKey(key))
    {
      entryTable.selectKey(key);
      return;
    }

    if (libraryWrapper.getTrash().contains(libraryWrapper.getEntryByKey(key)))
      collTree.selectTrash();
    else
      collTree.selectAllEntries();

    entryTable.selectKey(key);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void refresh()
  {
    instance.doRefresh();
  }

  private void doRefresh()
  {
    entryTable.refresh(getViewForTreeItem(treeView.getSelectionModel().getSelectedItem()));

    tableView.refresh();
    updateRightPane();

    BibEntryRow row = tableView.getSelectionModel().getSelectedItem();

    btnAssign      .setDisable (row == null);
    btnAutofill    .setDisable (row == null);
    btnViewInRefMgr.setDisable((row == null) || row.getURLtoViewEntryInRefMgr().isBlank());

    toolBarWrapper.setVisibleNoUpdate((libraryWrapper != null) && (libraryWrapper.type() != LibraryType.ltMendeley), btnViewInRefMgr);
    toolBarWrapper.setVisibleNoUpdate((row == null) || (row.getWork() == null), btnAssign  );
    toolBarWrapper.setVisibleNoUpdate((row != null) && (row.getWork() != null), btnUnassign);

    toolBarWrapper.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void initEntryTypeCB(ComboBox<EntryType> cb)
  {
    EnumHashBiMap<EntryType, String> map = libraryWrapper.getEntryTypeMap();

    cb.setConverter(new StringConverter<>()
    {
      @Override public String toString(EntryType et)
      {
        return map.containsKey(et) ? et.getUserFriendlyName() : "";
      }

      @Override public EntryType fromString(String string)
      {
        EntryType et = parse(string);
        return map.containsKey(et) ? et : null;
      }
    });

    List<EntryType> choices = Ordering.from(Comparator.comparing(EntryType::getUserFriendlyName)).sortedCopy(map.keySet());

    cb.setItems(null);
    cb.setItems(FXCollections.observableList(choices));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setDividerPositions()
  {
    setDividerPosition(spMain, DividerPositionPrefKey.BIB_LEFT_HORIZ , 0);
    setDividerPosition(spMain, DividerPositionPrefKey.BIB_RIGHT_HORIZ, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void getDividerPositions()
  {
    getDividerPosition(spMain, DividerPositionPrefKey.BIB_LEFT_HORIZ , 0);
    getDividerPosition(spMain, DividerPositionPrefKey.BIB_RIGHT_HORIZ, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void show(HDT_Work work, boolean setAssigning)
  {
    if (instance == null) return;

    show(false);
    instance.goToWork(work, setAssigning);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void show(boolean focusOnSearchField)
  {
    if (instance == null) return;

    if (instance.stage.isShowing())
      instance.doRefresh();

    show(instance);

    if (focusOnSearchField) Platform.runLater(() -> safeFocus(instance.searchField));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void syncWithModalPopup()
  {
    try
    {
      syncTask = libraryWrapper.createNewSyncTask("Downloading library data...");
    }
    catch (HyperDataException e)
    {
      errorPopup("Unable to download library data: " + getThrowableMessage(e));
      return;
    }

    syncTask.startWithNewThread();  // Start before calling runWithProgressDialog because we want to cancel it by calling LibraryWrapper.stop()

    syncTask.runWithProgressDialog();

    libraryWrapper.stop();

    syncTask.cancelAndWait();

    rebuildCollectionTree();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
