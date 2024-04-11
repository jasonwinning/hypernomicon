/*
 * Copyright 2015-2024 Jason Winning
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
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;
import static java.util.Objects.*;

import java.io.IOException;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Stream;

import com.google.common.collect.EnumHashBiMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Ordering;
import com.google.common.collect.Sets;

import org.controlsfx.control.textfield.CustomTextField;
import org.controlsfx.control.textfield.TextFields;

import org.hypernomicon.bib.CollectionTree.BibCollectionType;
import org.hypernomicon.bib.LibraryWrapper.SyncTask;
import org.hypernomicon.bib.data.BibDataRetriever;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.dialogs.SelectWorkDlgCtrlr;
import org.hypernomicon.dialogs.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.animation.Animation;
import javafx.animation.Interpolator;
import javafx.animation.RotateTransition;
import javafx.animation.SequentialTransition;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.concurrent.Worker.State;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.TitledPane;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;
import javafx.stage.Modality;
import javafx.stage.StageStyle;
import javafx.util.Duration;
import javafx.util.StringConverter;

public class BibManager extends HyperDlg
{
  @FXML private Button btnCreateNew, btnAutofill, btnViewOnWeb, btnAssign, btnUnassign, btnDelete, btnMainWindow, btnPreviewWindow, btnStop, btnSync, btnUpdateRelatives;
  @FXML private CheckBox chkRequireByDefault;
  @FXML private ComboBox<EntryType> cbNewType;
  @FXML private Label lblSelect;
  @FXML private SplitPane spMain;
  @FXML private TableView<BibEntryRow> tableView;
  @FXML private TableView<HyperTableRow> tvRelatives;
  @FXML private TextField tfSearch;
  @FXML private TitledPane tpRelated;
  @FXML private AnchorPane apRelated;
  @FXML private ToolBar toolBar, toolBar2;
  @FXML private TreeView<BibCollectionRow> treeView;
  @FXML private WebView webView;
  @FXML private ProgressBar progressBar;

  private static final String dialogTitle = "Bibliographic Entry Manager";
  private static final AsyncHttpClient httpClient = new AsyncHttpClient();

  private final HyperTable htRelatives;
  private final BibEntryTable entryTable;
  private final CollectionTree collTree;
  private final String assignCaption, unassignCaption;
  private final ImageView assignImg, unassignImg;
  private final CustomTextField searchField;

  public final ObjectProperty<HDT_Work> workRecordToAssign = new SimpleObjectProperty<>();

  private LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> libraryWrapper = null;
  private BibDataRetriever bibDataRetriever = null;
  private SyncTask syncTask = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()               { return true; }

  private void hideBottomControls()                   { setAllVisible(false, lblSelect, btnCreateNew, cbNewType); }
  private void viewOnWeb()                            { viewOnWeb(tableView.getSelectionModel().getSelectedItem().getEntry()); }
  private static void viewOnWeb(BibEntry<?, ?> entry) { DesktopUtil.openWebLink(entry.getEntryURL()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibManager()
  {
    super("bib/BibManager", dialogTitle, true, StageStyle.DECORATED, Modality.NONE);

    entryTable = new BibEntryTable(tableView, this);
    collTree = new CollectionTree(treeView);

    searchField = setupSearchField();

    assignCaption = btnAssign.getText();
    unassignCaption = btnUnassign.getText();
    assignImg = (ImageView) btnAssign.getGraphic();
    unassignImg = (ImageView) btnUnassign.getGraphic();
    btnUnassign.setGraphic(null);

    toolBar.getItems().removeAll(btnUnassign, progressBar, btnDelete, btnPreviewWindow); // Latter 2 are not yet supported. (Not sure if the preview button is needed?)

    btnMainWindow.setOnAction(event -> ui.windows.focusStage(ui.getStage()));
    btnSync.setOnAction(event -> sync());
    btnStop.setOnAction(event -> stop());
    btnAutofill.setOnAction(event -> autofill());
    btnViewOnWeb.setOnAction(event -> viewOnWeb());

    setToolTip(btnMainWindow, "Return to main application window");
    setToolTip(btnAutofill, MainCtrlr.AUTOFILL_TOOLTIP);
    setToolTip(btnViewOnWeb, "View this entry on the web");

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
        lblSelect.setText("Assigning to work record: " + newValue.getCBText());
        setAllVisible(true, lblSelect, btnCreateNew, cbNewType);
        return;
      }

      hideBottomControls();
    });

    btnCreateNew.setOnAction(event -> btnCreateNewClick());
    btnUpdateRelatives.setOnAction(event -> btnUpdateRelativesClick());

    tableView.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) -> refresh());

    entryTable.addContextMenuItem("View this entry on the web", row -> row.getURL().length() > 0, row -> viewOnWeb(row.getEntry()));

    entryTable.addContextMenuItem("Go to work record", HDT_Work.class, work -> ui.goToRecord(work, true));

    entryTable.addContextMenuItem("Unassign work record" , row -> nonNull(row.getWork()), this::unassign);
    entryTable.addContextMenuItem("Assign to work record", row -> isNull (row.getWork()), this::assign  );

    entryTable.addContextMenuItem("Launch work", HDT_Work.class, HDT_Work::canLaunch, work -> work.launch(-1));

    entryTable.addContextMenuItem("Show in Preview Window", HDT_Work.class, HDT_Work::canPreview, work ->
    {
      PreviewSource src = ui.determinePreviewContext();
      previewWindow.setPreview(src, work);
      ui.openPreviewWindow(src);
    });

    tpRelated.setExpanded(false);
    tpRelated.heightProperty().addListener((ob, ov, nv) -> AnchorPane.setTopAnchor(webView, nv.doubleValue()));

    htRelatives = new HyperTable(tvRelatives, 2, false, "");
    htRelatives.addLabelCol(hdtWork, smTextSimple);
    htRelatives.addIconCol();
    htRelatives.addLabelCol(hdtWork, smTextSimple);
    htRelatives.setDblClickHandler(HDT_Work.class, this::goToWork);

    htRelatives.addDefaultMenuItems();

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) -> MainTextUtil.handleJSEvent("", webView.getEngine()));

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.setOnDragOver(Event::consume);
    webView.setOnDragDropped(Event::consume);

    MainTextUtil.webViewAddZoom(webView, PREF_KEY_BIBMGR_ZOOM);

//---------------------------------------------------------------------------

    onShown = () ->
    {
      if (shownAlready() == false)
      {
        hideBottomControls();

        collTree.clear();
        entryTable.clear();

        collTree.rebuild(libraryWrapper.getKeyToColl());

        treeView.getSelectionModel().clearAndSelect(0);

        setDividerPositions();
      }
      else
        refresh();

      ui.windows.push(dialogStage);

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

    dialogStage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus() || (Boolean.TRUE.equals(newValue) == false))
        return;

      ui.windows.push(dialogStage);

      refresh();
    });

//---------------------------------------------------------------------------

    dialogStage.setOnHidden(event -> ui.windows.focusStage(ui.getStage()));
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

    chkRequireByDefault.setSelected(app.prefs.getBoolean(PREF_KEY_BIB_SRCH_REQUIRE_BY_DEFAULT, false));

    chkRequireByDefault.setOnAction(event ->
    {
      app.prefs.putBoolean(PREF_KEY_BIB_SRCH_REQUIRE_BY_DEFAULT, chkRequireByDefault.isSelected());
      entryTable.filter(ctf.getText(), chkRequireByDefault.isSelected());
    });

    KeyCombination keyComb = new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN);
    dialogStage.addEventHandler(KeyEvent.KEY_PRESSED, event ->
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

  public void setLibrary(LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> libraryWrapper)
  {
    this.libraryWrapper = libraryWrapper;

    if (libraryWrapper != null)
    {
      setToolTip(btnSync, "Synchronize with " + libraryWrapper.type().getUserFriendlyName());
      initCB(cbNewType);
    }

    if (shownAlready() == false) return;

    stop();

    collTree.clear();
    entryTable.clear();

    if (libraryWrapper == null) return;

    collTree.rebuild(libraryWrapper.getKeyToColl());
    treeView.getSelectionModel().clearAndSelect(0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void sync()
  {
    if (ui.cantSaveRecord()) return;

    if (btnStop.isDisabled() == false) return;

    stop();

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

    syncTask = libraryWrapper.createNewSyncTask();

    syncTask.runningProperty().addListener((ob, wasRunning, isRunning) ->
    {
      if (Boolean.FALSE.equals(wasRunning) || Boolean.TRUE.equals(isRunning)) return;

      stop();
      seqT.stop();

      ImageView iv1 = imgViewFromRelPath("resources/images/refresh.png");
      iv1.setFitWidth(16);
      iv1.setFitHeight(16);
      btnSync.setGraphic(iv1);

      if ((syncTask.getState() == State.FAILED) || (syncTask.getState() == State.CANCELLED))
      {
        Throwable ex = syncTask.getException();

        if (ex instanceof HyperDataException)
          messageDialog(getThrowableMessage(ex), mtError);
      }

      boolean changed = syncTask.getChanged();
      syncTask = null;

      if (changed == false) return;

      collTree.refresh(libraryWrapper.getKeyToColl());
      refresh();

      ui.update();
      ui.saveAllToDisk(true, true, true);
    });

    syncTask.startWithNewThreadAsDaemon();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stop()
  {
    if (bibDataRetriever != null)
      bibDataRetriever.stop();

    toolBar.getItems().remove(progressBar);
    btnStop.setDisable(true);
    btnSync.setDisable(false);

    if (libraryWrapper != null)
      libraryWrapper.stop();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void autofill()
  {
    if (ui.cantSaveRecord()) return;

    stop();

    btnStop.setDisable(false);
    toolBar.getItems().add(progressBar);
    btnSync.setDisable(true);

    BibEntry<?, ?> entry = tableView.getSelectionModel().getSelectedItem().getEntry();

    List<FilePath> pdfFilePaths;

    if (entry.linkedToWork())
    {
      pdfFilePaths = entry.getWork().workFiles.stream().filter(HDT_WorkFile::pathNotEmpty)
                                                       .map(HDT_WorkFile::filePath)
                                                       .toList();
      if (collEmpty(pdfFilePaths))
        pdfFilePaths = safeListOf(resolveExtFilePath(entry.getWork().getURL()));
    }
    else
      pdfFilePaths = Collections.emptyList();

    bibDataRetriever = new BibDataRetriever(httpClient, entry, pdfFilePaths, (pdfBD, queryBD, messageShown) ->
    {
      stop();

      if ((pdfBD == null) && (queryBD == null))
      {
        if (messageShown == false)
          messageDialog("Unable to find bibliographic information.", mtInformation);

        return;
      }

      MergeWorksDlgCtrlr mwd;

      try
      {
        mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", entry, pdfBD, queryBD, null, entry.getWork(), false, false, Ternary.False);
      }
      catch (IOException e)
      {
        messageDialog("Unable to initialize merge dialog window.", mtError);
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

  public enum BibEntryRelation { Parent, Sibling, Child }

  private static final ImmutableSet<EntryType> childTypes  = Sets.immutableEnumSet(etBookChapter, etEncyclopediaArticle, etConferencePaper, etDictionaryEntry),
                                               parentTypes = Sets.immutableEnumSet(etBook, etConferenceProceedings);

//---------------------------------------------------------------------------

  private void unassign(BibEntryRow row)
  {
    if (confirmDialog("Are you sure you want to unassign the work record?") == false) return;
    if (ui.cantSaveRecord()) return;

    HDT_Work work = row.getWork();

    row.getEntry().unassignWork();

    if (workRecordToAssign.get() == work)
    {
      workRecordToAssign.set(null);
      workRecordToAssign.set(work);
    }

    refresh();
    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class RelatedBibEntry
  {
    public final BibEntryRelation relation;
    public final BibEntry<?, ?> entry;

    private RelatedBibEntry(BibEntryRelation relation, BibEntry<?, ?> entry)
    {
      this.relation = relation;
      this.entry = entry;
    }
  }

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

    if (childTypes.contains(entry.getEntryType()) && work.largerWork.isNotNull())
    {
      HDT_Work parentWork = work.largerWork.get();
      if (parentWork.getBibEntryKey().isBlank() == false)
      {
        BibEntry<?, ?> parentEntry = libraryWrapper.getEntryByKey(parentWork.getBibEntryKey());
        if (parentTypes.contains(parentEntry.getEntryType()))
        {
          list.add(new RelatedBibEntry(BibEntryRelation.Parent, parentEntry));

          addChildren(work, parentWork.subWorks, BibEntryRelation.Sibling, list);
        }
      }
    }

    if (parentTypes.contains(entry.getEntryType()))
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
      if (childTypes.contains(childEntry.getEntryType()))
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
      falseWithWarningMessage("You must select an entry type.", cbNewType);
      return;
    }

    if (ui.cantSaveRecord()) return;

    HDT_Work work = workRecordToAssign.get();
    BibEntry<?, ?> entry = libraryWrapper.addEntry(et);

    work.setBibEntryKey(entry.getKey());

    if (childTypes.contains(et) && work.largerWork.isNotNull() && (work.largerWork.get().getBibEntryKey().isBlank() == false))
    {
      BibEntry<?, ?> parentEntry = libraryWrapper.getEntryByKey(work.largerWork.get().getBibEntryKey());

      if (parentTypes.contains(parentEntry.getEntryType()))
        updateRelative(new RelatedBibEntry(BibEntryRelation.Child, entry), parentEntry);
    }

    workRecordToAssign.set(null);

    refresh();

    ui.update();

    goToWork(work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void assign(BibEntryRow row)
  {
    if (ui.cantSaveRecord()) return;

    SelectWorkDlgCtrlr dlg = new SelectWorkDlgCtrlr(workRecordToAssign.get(), row.getEntry());

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

  private void assignToExisting(HDT_Work work, BibEntry<?, ?> entry)
  {
    if ((work == null) || (entry == null)) return;

    MergeWorksDlgCtrlr mwd;

    try
    {
      mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", work.getBibData(), entry, null, null, work, false, false, Ternary.False);
    }
    catch (IOException e)
    {
      messageDialog("Unable to initialize merge dialog window.", mtError);
      return;
    }

    if (mwd.showModal() == false) return;

    work.setBibEntryKey(entry.getKey());
    mwd.mergeInto(entry);

    workRecordToAssign.set(null);
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

  public void goToWork(HDT_Work work)
  {
    if (HDT_Work.isUnenteredSet(work))
    {
      workRecordToAssign.set(null);
      return;
    }

    workRecordToAssign.set(work);

    String key = work.getBibEntryKey();

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

  public void refresh()
  {
    entryTable.refresh(getViewForTreeItem(treeView.getSelectionModel().getSelectedItem()));

    tableView.refresh();
    updateRightPane();

    BibEntryRow row = tableView.getSelectionModel().getSelectedItem();

    btnAssign   .setDisable(row == null);
    btnAutofill .setDisable(row == null);
    btnViewOnWeb.setDisable((row == null) || row.getURL().isBlank());

    if ((row == null) || (row.getWork() == null))
    {
      btnAssign.setOnAction(event -> assign(tableView.getSelectionModel().getSelectedItem()));
      btnAssign.setGraphic(assignImg);
      btnAssign.setText(assignCaption);
    }
    else
    {
      btnAssign.setOnAction(event -> unassign(tableView.getSelectionModel().getSelectedItem()));
      btnAssign.setGraphic(unassignImg);
      btnAssign.setText(unassignCaption);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void initCB(ComboBox<EntryType> cb)
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

  private void setDividerPositions()
  {
    setDividerPosition(spMain, PREF_KEY_BIB_LEFT_HORIZ, 0);
    setDividerPosition(spMain, PREF_KEY_BIB_RIGHT_HORIZ, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getDividerPositions()
  {
    if (shownAlready() == false) return;

    getDividerPosition(spMain, PREF_KEY_BIB_LEFT_HORIZ, 0);
    getDividerPosition(spMain, PREF_KEY_BIB_RIGHT_HORIZ, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void focusOnSearchField()
  {
    safeFocus(searchField);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
