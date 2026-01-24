/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.view.tabs;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.BibManager;
import org.hypernomicon.bib.data.*;
import org.hypernomicon.dialogs.*;
import org.hypernomicon.dialogs.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.authors.*;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;
import org.hypernomicon.util.http.AsyncHttpClient;
import org.hypernomicon.view.cellValues.*;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.tableCells.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.*;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.MainCtrlr.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.io.File;
import java.io.IOException;
import java.time.Month;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.prefs.Preferences;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;

import javafx.application.Platform;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.control.SplitPane.Divider;
import javafx.scene.image.ImageView;
import javafx.scene.input.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------

public class WorkTabCtrlr extends HyperTab<HDT_Work, HDT_Work>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apDescription, apLowerMid, apLowerRight;
  @FXML private Button btnBibManager, btnLargerWork, btnLaunch, btnMergeBib, btnNewChapter, btnURL,
                       btnStop, btnTree, btnUseDOI, btnUseISBN, btnWebSrch1, btnWebSrch2, btnAutofill, btnTopAutofill;
  @FXML private ComboBox<HyperTableCell> cbLargerWork, cbType;
  @FXML private ComboBox<Month> cbMonth;
  @FXML private Label lblSearchKey, lblTitle;
  @FXML private MenuItem mnuCrossref, mnuFindDOIonCrossref, mnuFindISBNonGoogleBooks, mnuGoogle, mnuShowMetadata, mnuStoreMetadata;
  @FXML private ProgressBar progressBar;
  @FXML private SplitMenuButton btnDOI, smbWebSrch1, btnFolder;
  @FXML private SplitPane spHoriz1, spHoriz2, spVert, spMentioners;
  @FXML private Tab tabBibDetails, tabCrossref, tabGoogleBooks, tabEntry, tabMiscBib, tabMiscFiles, tabPdfMetadata, tabSubworks, tabWorkFiles;
  @FXML private TabPane tabPane, tpBib;
  @FXML private TableView<HyperTableRow> tvArguments, tvAuthors, tvISBN, tvKeyMentions,
                                         tvLabels, tvMiscFiles, tvSubworks, tvWorkFiles;
  @FXML private TextArea taEntry, taCrossref, taGoogleBooks, taMiscBib, taPdfMetadata;
  @FXML private TextField tfDOI, tfURL, tfSearchKey, tfTitle, tfYear, tfDay;

  private final HyperTable htLabels, htSubworks, htArguments, htMiscFiles, htWorkFiles, htKeyMentioners, htISBN;
  private final HyperCB hcbLargerWork;
  private final MainTextWrapper mainText;
  private final DateControlsWrapper dateCtrls;
  private final Map<Tab, String> tabCaptions = new HashMap<>();
  private final MutableBoolean alreadyChangingTitle = new MutableBoolean(false);
  private final Property<CrossrefBibData> crossrefBDprop = new SimpleObjectProperty<>();
  private final Property<PDFBibData>      pdfBDprop      = new SimpleObjectProperty<>();
  private final Property<GoogleBibData>   googleBDprop   = new SimpleObjectProperty<>();

  private static final AsyncHttpClient httpClient = new AsyncHttpClient();
  private static final String TOOLTIP_PREFIX = "Search for this work in ";

  public FileDlgCtrlr fdc = null;
  public WorkDlgCtrlr wdc = null;
  public HyperTable htAuthors;
  public HyperCB hcbType;

  private BibDataRetriever bibDataRetriever = null;
  private HDT_Work curWork, lastWork = null;
  private double btnURLLeftAnchor, tfURLLeftAnchor, tfURLRightAnchor;
  private boolean inNormalMode = true, programmaticTypeChange = false;

//---------------------------------------------------------------------------

  public WorkTabCtrlr(Tab tab) throws IOException
  {
    super(workTabEnum, tab, "view/tabs/WorkTab");

    mainText = new MainTextWrapper(apDescription);

    tabPane.setStyle("-fx-open-tab-animation: NONE; -fx-close-tab-animation: NONE;");
    tpBib  .setStyle("-fx-open-tab-animation: NONE; -fx-close-tab-animation: NONE;");

    tabPane.getTabs().forEach(subTab -> tabCaptions.put(subTab, subTab.getText()));

    setToolTip(btnWebSrch1, () -> TOOLTIP_PREFIX + btnWebSrch1.getText());
    setToolTip(smbWebSrch1, () -> TOOLTIP_PREFIX + smbWebSrch1.getText());
    setToolTip(btnWebSrch2, () -> TOOLTIP_PREFIX + btnWebSrch2.getText());
    setToolTip(btnAutofill, AUTOFILL_TOOLTIP);
    setToolTip(btnTopAutofill, AUTOFILL_TOOLTIP);

    setSearchKeyToolTip(tfSearchKey);

    htAuthors = new HyperTable(tvAuthors, 1, true, TablePrefKey.WORK_AUTHORS);

    htAuthors.addActionCol(ctGoBtn, 1);
    htAuthors.addAuthorEditCol(() -> curWork, null);
    htAuthors.addCheckboxCol();
    htAuthors.addCheckboxCol();

    htAuthors.addContextMenuItem("Create person record",
      row -> strNotNullOrEmpty(row.getText(1)) && (row.getID(1) < 1),
      row ->
      {
        if (ui.cantSaveRecord()) return;

        String text = row.getText(1);

        Ternary isInFileName = Ternary.Unset;
        RecordAuthor author = curWork.getAuthors().getAuthor(new PersonName(text));
        if (author != null)
          isInFileName = author.getInFileName();

        NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(true, text, author);

        if (npdc.showModal())
        {
          htAuthors.getPopulator(1).setChanged(row);               // A new record has been created so force it to repopulate
          htAuthors.selectID(1, row, npdc.getPerson().getID());
          saveToRecord();
          curWork.setPersonIsInFileName(npdc.getPerson(), isInFileName);
          ui.update();
        }
      });

    htAuthors.addChangeOrderMenuItem();
    htAuthors.addRemoveMenuItem();

    htAuthors.addContextMenuItem("Remove this row",
      row -> strNotNullOrEmpty(row.getText(1)) && (row.getID(1) < 1),
      htAuthors::removeRow);

    htLabels = new HyperTable(tvLabels, 2, true, TablePrefKey.WORK_LABELS);

    htLabels.addActionCol(ctGoBtn    , 2).setButtonTooltip(ButtonAction.baGo    , "Go to this record");
    htLabels.addActionCol(ctBrowseBtn, 2).setButtonTooltip(ButtonAction.baBrowse, "Select a Label from the Tree");
    htLabels.addCol(hdtWorkLabel, ctEditableLimitedDropDown)
      .setCellToolTipHndlr(row -> makeTooltip(row.getText(2)))
      .setTextOverrunStyle(OverrunStyle.LEADING_ELLIPSIS);

    htLabels.addRemoveMenuItem();

    htSubworks = new HyperTable(tvSubworks, 1, false, TablePrefKey.WORK_SUB);

    htSubworks.addLabelCol(hdtPerson);
    htSubworks.addLabelCol(hdtWork, smStandard);  // Title
    htSubworks.addLabelCol(hdtWork, smStandard);  // Date

    htSubworks.addContextMenuItem("Go to person record", HDT_Person.class,
      person -> ui.goToRecord(person, true));

    htSubworks.addContextMenuItem("Go to work record", HDT_Work.class,
      work -> ui.goToRecord(work, true));

    htSubworks.addDefaultMenuItems();

    htSubworks.addChangeOrderMenuItem(false, () -> curWork.subWorks.reorder(htSubworks.saveToList(1, hdtWork), true));

    htKeyMentioners = new HyperTable(tvKeyMentions, 1, false, TablePrefKey.WORK_MENTIONERS);

    htKeyMentioners.addIconCol();
    htKeyMentioners.addLabelCol(hdtNone);
    htKeyMentioners.addLabelCol(hdtNone);

    htKeyMentioners.addDefaultMenuItems();

    htArguments = new HyperTable(tvArguments, 3, false, TablePrefKey.WORK_ARG);

    htArguments.addIconCol();                       // Icon to indicate the type of record this argument/stance targets
    htArguments.addLabelCol(hdtNone);               // Record name of the target of the argument/stance
    htArguments.addLabelCol(hdtNone, smTextSimple); // Verdict of the argument/stance
    htArguments.addLabelCol(hdtArgument);           // Argument/stance name
    htArguments.addLabelCol(hdtWork)                // Pages
               .setValueType(cvtPageRange);

    htWorkFiles = new HyperTable(tvWorkFiles, 2, true, TablePrefKey.WORK_FILES);

    htWorkFiles.addRefreshHandler(tabPane::requestLayout);

    htWorkFiles.addActionColWithButtonHandler(ctEditNewBtn, 2, (row, colNdx) -> showWorkDialog(row.getRecord(colNdx), true))
      .setButtonTooltip(ButtonAction.baEdit, "Update or rename this work file")
      .setButtonTooltip(ButtonAction.baNew, "Add a new Work file");

    htWorkFiles.addCheckboxCol();
    htWorkFiles.addLabelCol(hdtWorkFile);

    htWorkFiles.addTextEditColWithUpdateHandler(hdtWorkFile, false, smNumeric, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      int startPageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);
      if (startPageNum < 0) return;

      HDT_WorkFile workFile = row.getRecord();
      if (workFile == null) return;

      int endPageNum = parseInt(row.getText(nextColNdx), -1);

      PreviewWindow.setPreview(pvsWorkTab, workFile.filePath(), startPageNum, endPageNum, curWork);

    }).setHeaderTooltip("Start page in PDF (actual PDF page, not page label)");

    htWorkFiles.addTextEditColWithUpdateHandler(hdtWorkFile, false, smNumeric, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      int endPageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);
      if (endPageNum < 0) return;

      HDT_WorkFile workFile = row.getRecord();
      if (workFile == null) return;

      int startPageNum = parseInt(row.getText(nextColNdx - 2), -1);

      PreviewWindow.setPreview(pvsWorkTab, workFile.filePath(), startPageNum, endPageNum, curWork);

    }).setHeaderTooltip("End page in PDF (actual PDF page, not page label)");

    htWorkFiles.addTextEditCol(hdtWorkFile, false);

    htWorkFiles.addContextMenuItem("Launch file", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> launchWorkFile(workFile.filePath(), getCurPageNum(curWork, workFile, true)));

    htWorkFiles.addContextMenuItem("Show in system explorer", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> highlightFileInExplorer(workFile.filePath()));

    htWorkFiles.addContextMenuItem("Show in File Manager", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> FileManager.show(workFile.filePath()));

    htWorkFiles.addContextMenuItem("Copy path to clipboard", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> copyToClipboard(workFile.getPath().toString()));

    htWorkFiles.addContextMenuItem("Update or rename this work file", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> showWorkDialog(workFile, true));

    htWorkFiles.addContextMenuItem("Select parent work file",
      row ->
      {
        if ((curWork == null) || curWork.largerWork.isNull()) return false;
        return curWork.largerWork.get().workFiles.stream().anyMatch(workFile -> curWork.workFiles.contains(workFile) == false);
      },
      row ->
      {
        ChooseParentWorkFileDlgCtrlr ctrlr = new ChooseParentWorkFileDlgCtrlr(curWork);

        if (ctrlr.showModal() == false) return;

        HDT_WorkFile workFile = ctrlr.getWorkFile();
        if (workFile == null) return;

        HDT_WorkFile oldWorkFile = htWorkFiles.selectedRecord();

        if (oldWorkFile == null)
          curWork.addWorkFile(workFile.getID());
        else
          curWork.replaceWorkFile(oldWorkFile, workFile);

        refreshFiles();
      });

    Predicate<HDT_WorkFile> condHandler = workFile ->
      inNormalMode || workFile.getPath().isEmpty() || workFile.works.stream().anyMatch(work -> work.getWorkTypeEnum() != wtUnenteredSet) ?
        false
      :
        curWork.getWorkTypeEnum() == wtUnenteredSet;

    htWorkFiles.addContextMenuItem("Move to a dedicated work record", HDT_WorkFile.class, condHandler, this::moveUnenteredWorkFile);

    htWorkFiles.addContextMenuItem("Remove file", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile ->
      {
        if (ui.cantSaveRecord()) return;

        if (confirmDialog("Are you sure you want to remove this file from the work record?", false) == false) return;

        db.getObjectList(rtWorkFileOfWork, curWork, true).remove(workFile);
        FileManager.setNeedRefresh();
        ui.update();
      });

    htWorkFiles.addChangeOrderMenuItem(true, () -> db.<HDT_WorkFile, HDT_Work>getObjectList(rtWorkFileOfWork, curWork, true).reorder(htWorkFiles.saveToList(2, hdtWorkFile)));

    htWorkFiles.setDblClickHandler(HDT_WorkFile.class, workFile -> launchWorkFile(workFile.filePath(), getCurPageNum(curWork, workFile, true)));

    tvWorkFiles.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (oldValue == newValue)) return;

      HDT_WorkFile workFile = newValue.getRecord();
      if (workFile == null)
        PreviewWindow.setPreview(pvsWorkTab, curWork.filePathIncludeExt(), getCurPageNum(curWork, null, true), getCurPageNum(curWork, null, false), curWork);
      else
        PreviewWindow.setPreview(pvsWorkTab, workFile.filePath(), getCurPageNum(curWork, workFile, true), getCurPageNum(curWork, workFile, false), curWork);
    });

    tvSubworks.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (oldValue == newValue)) return;

      PreviewWindow.setPreview(pvsWorkTab, (HDT_Work) newValue.getRecord());
    });

    htMiscFiles = new HyperTable(tvMiscFiles, 2, true, TablePrefKey.WORK_MISC);

    htMiscFiles.addGoNewCol(hdtMiscFile, 2);

    htMiscFiles.addCustomActionCol(2, "Preview", (row, colNdx) ->
    {
      HDT_MiscFile miscFile = row.getRecord();
      if (miscFile.pathNotEmpty() == false) return;
      PreviewWindow.show(pvsWorkTab, miscFile);

    }).setButtonTooltip(ButtonAction.baCustom, "Show in Preview Window");

    htMiscFiles.addLabelCol(hdtMiscFile);

    htMiscFiles.addDefaultMenuItems();

    htISBN = new HyperTable(tvISBN, 0, true, "");

    htISBN.addTextEditCol(hdtWork, true, smTextSimple);

    htISBN.addContextMenuItem(() -> ui.webButtonMap.get(WebButtonContextPrefKey.ISBN).getCaption(),
      row ->
      {
        List<String> list = matchISBN(row.getText(0));
        return collEmpty(list) == false;
      },
      row -> ui.webButtonMap.get(WebButtonContextPrefKey.ISBN).first(WebButtonField.ISBN, row.getText(0)).go());

    htISBN.addContextMenuItem("Google Books query",
      row -> strNotNullOrEmpty(row.getText(0)),
      row ->
      {
        List<String> list = matchISBN(row.getText(0));
        if (collEmpty(list) == false)
          retrieveBibData(false, "", list);
      });

    htISBN.addRefreshHandler(tabPane::requestLayout);

    hcbType       = new HyperCB(cbType      , ctEditableLimitedDropDown, new StandardPopulator(hdtWorkType), true);
    hcbLargerWork = new HyperCB(cbLargerWork, ctEditableLimitedDropDown, new StandardPopulator(hdtWork    ), true);

    btnWebSrch1.setOnAction(searchBtnEvent(WebButtonContextPrefKey.WORK + '1'));
    smbWebSrch1.setOnAction(searchBtnEvent(WebButtonContextPrefKey.WORK + '1'));
    btnWebSrch2.setOnAction(searchBtnEvent(WebButtonContextPrefKey.WORK + '2'));

    btnDOI.setOnAction(event -> searchDOI(tfDOI.getText()));
    setToolTip(btnDOI, "Use this DOI to locate the document online");

    btnBibManager.setOnAction(event -> BibManager.show(curWork, true));

    btnStop.setOnAction(event -> stopRetrieving());

    mnuFindDOIonCrossref    .setOnAction(event -> retrieveBibData(true , "", null));
    mnuFindISBNonGoogleBooks.setOnAction(event -> retrieveBibData(false, "", null));

    mnuGoogle.setOnAction(event ->
    {
      if (tfDOI.getText().isBlank()) return;

      ui.webButtonMap.get(WebButtonContextPrefKey.DOI).first(WebButtonField.doi, tfDOI.getText()).go();
    });

    mnuCrossref.setOnAction(event ->
    {
      if (strNotNullOrEmpty(tfDOI.getText()))
        retrieveBibData(true, tfDOI.getText(), null);
    });

    mnuShowMetadata.setOnAction(event -> mnuShowMetadataClick());

    mnuStoreMetadata.setVisible(false); // Not implemented yet

    btnUseDOI     .setOnAction(event -> useDOIClick     ());
    btnUseISBN    .setOnAction(event -> useISBNClick    ());
    btnAutofill   .setOnAction(event -> btnAutofillClick());
    btnTopAutofill.setOnAction(event -> btnAutofillClick());

    btnLaunch.setOnAction(event ->
    {
      if (curWork.workFiles.isEmpty())
        urlClick();
      else
        curWork.launch(getCurPageNum(curWork, null, true));
    });

    btnURL.setOnAction(event -> urlClick());
    setToolTip(btnURL, "Navigate to this URL in browser");

    btnLargerWork.setOnAction(event ->
    {
      if (inNormalMode)
        ui.goToRecord(HyperTableCell.getRecord(hcbLargerWork.selectedHTC()), true);
      else
        moveAllFiles();
    });

    initArgContextMenu();
    btnTree.setOnAction(event -> ui.goToTreeRecord(curWork));

    setToolTip(btnTree, "Go to this record in Tree tab");

    hcbType.addListener((oldValue, newValue) ->
    {
      if (programmaticTypeChange || (newValue == null)) return;

      WorkTypeEnum workTypeEnumVal = HDT_WorkType.workTypeIDToEnumVal(HyperTableCell.getCellID(newValue)),
                   oldEnumVal = curWork.getWorkTypeEnum();

      if (workTypeEnumVal != wtUnenteredSet)
      {
        if (oldEnumVal == wtUnenteredSet)
        {
          programmaticTypeChange = true;
          errorPopup("You cannot change the work type after it has been set to Unentered Set of Work Files.");
          programmaticTypeChange = false;

          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }

        changeToNormalMode();

        return;
      }

      if (oldEnumVal != wtUnenteredSet)
      {
        if (oldEnumVal != wtNone)
        {
          programmaticTypeChange = true;
          errorPopup("You cannot change a work with an existing work type into an unentered set of work files.");
          programmaticTypeChange = false;

          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }

        if (strNotNullOrEmpty(curWork.getBibEntryKey()))
        {
          programmaticTypeChange = true;
          errorPopup("You cannot change a work that is assigned to a " + db.bibLibraryUserFriendlyName() + " entry into an unentered set of work files.");
          programmaticTypeChange = false;

          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }
      }

      changeToUnenteredSetMode();
    });

    Divider div1 = spHoriz1.getDividers().getFirst(),
            div2 = spHoriz2.getDividers().getFirst();

    div1.positionProperty().bindBidirectional(div2.positionProperty());

    EventHandler<ActionEvent> handler = event ->
    {
      if (strNotNullOrEmpty(tfURL.getText()) && (tfURL.getText().charAt(0) != '('))
        launchFile(new FilePath(tfURL.getText()));
    };

    btnFolder.setOnAction(handler);
    addFolderMenuItem("Show in system explorer", handler);

    addFolderMenuItem("Show in file manager", event ->
    {
      if (strNotNullOrEmpty(tfURL.getText()) && (tfURL.getText().charAt(0) != '('))
        FileManager.show(new FilePath(tfURL.getText()));
    });

    setToolTip(lblSearchKey, "Regenerate search key");

    lblSearchKey.setOnMouseClicked(event -> lblSearchKeyClick());

    setToolTip(lblTitle, "Reformat title");

    lblTitle.setOnMouseClicked(event ->
    {
      String title = HDT_Work.fixCase(convertToSingleLine(tfTitle.getText().strip()));

      alreadyChangingTitle.setTrue();
      tfTitle.setText(title);
      alreadyChangingTitle.setFalse();

      safeFocus(tfTitle);
    });

    tfTitle.setTextFormatter(WorkDlgCtrlr.titleFormatter(alreadyChangingTitle));

    crossrefBDprop.addListener((ob, oldBD, newBD) -> updateMergeButton());
    pdfBDprop     .addListener((ob, oldBD, newBD) -> updateMergeButton());
    googleBDprop  .addListener((ob, oldBD, newBD) -> updateMergeButton());

    taMiscBib    .textProperty().addListener((ob, ov, nv) -> updateBibButtons());
    taPdfMetadata.textProperty().addListener((ob, ov, nv) -> updateBibButtons());
    taCrossref   .textProperty().addListener((ob, ov, nv) -> updateBibButtons());
    taGoogleBooks.textProperty().addListener((ob, ov, nv) -> updateBibButtons());

    tpBib.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) -> updateBibButtons());

    tabPdfMetadata.setOnClosed(event -> { taPdfMetadata.clear(); pdfBDprop     .setValue(null); });
    tabCrossref   .setOnClosed(event -> { taCrossref   .clear(); crossrefBDprop.setValue(null); });
    tabGoogleBooks.setOnClosed(event -> { taGoogleBooks.clear(); googleBDprop  .setValue(null); });

    btnMergeBib.setOnAction(event -> btnMergeBibClick());

    dateCtrls = new DateControlsWrapper(tfYear, cbMonth, tfDay);

    tabPane.addEventFilter(InputEvent.ANY, event -> tabPane.requestLayout()); // Fix for https://sourceforge.net/p/hypernomicon/tickets/18/
  }

//---------------------------------------------------------------------------

  @Override protected RecordType type()              { return hdtWork; }
  @Override protected void setRecord(HDT_Work work)  { curWork = work; }

  @Override public String recordName()               { return tfTitle.getText(); }
  @Override public MainTextWrapper mainTextWrapper() { return mainText; }

  private Stream<Author> getAuthorsFromUI()    { return AuthorStandalone.getAuthorsFromObjectGroups(getAuthorGroups()); }
  public String getShortAuthorsStr()           { return Author.getShortAuthorsStr(getAuthorsFromUI(), false, true); }
  private List<ObjectGroup> getAuthorGroups()  { return htAuthors.getAuthorGroups(curWork, 1, -1, 2, 3); }
  private void lblSearchKeyClick()             { tfSearchKey.setText(curWork.makeWorkSearchKey(getAuthorsFromUI(), tfYear.getText(), true, false)); }
  public String getTitle()                     { return tfTitle.getText(); }
  public BibliographicDate getDateFromUI()     { return dateCtrls.getDate(); }
  private void setTabCaption(Tab tab, int cnt) { tab.setText(tabCaptions.get(tab) + " (" + cnt + ')'); }
  private void saveISBNs()                     { curWork.setISBNs(htISBN.dataRowStream().map(row -> row.getText(0)).toList()); }
  private void useDOIClick()                   { tfDOI.setText(getDoiFromBibTab()); }
  private void useISBNClick()                  { htISBN.buildRows(getIsbnsFromBibTab(), (row, isbn) -> row.setCellValue(0, isbn, hdtNone)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void urlClick()
  {
    String url = tfURL.getText();

    if (url.startsWith(EXT_1) && (db.extPath() == null))
    {
      warningPopup(WorkTabCtrlr.NO_EXT_PATH_MESSAGE);
      return;
    }

    FilePath filePath = db.resolveExtFilePath(url);

    if (FilePath.isEmpty(filePath))
      openWebLink(url);
    else
      launchWorkFile(filePath, curWork.getStartPageNum());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateMergeButton()
  {
    btnMergeBib.setDisable((crossrefBDprop.getValue() == null) && (pdfBDprop.getValue() == null) && (googleBDprop.getValue() == null));

    tabPane.requestLayout();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getFirstAuthorSingleName()
  {
    return getAuthorsFromUI().findFirst().map(Author::singleName).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getCurPageNum(HDT_Work work, HDT_WorkFile workFile, boolean isStart)
  {
    if ((curWork == null) || (curWork != work) || curWork.workFiles.isEmpty()) return -1;

    if (workFile == null)
      workFile = curWork.workFiles.getFirst();

    for (HyperTableRow row : htWorkFiles.dataRows())
      if (workFile == row.getRecord())
        return Math.max(-1, parseInt(row.getText(isStart ? 3 : 4), -1));

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setPageNum(HDT_WorkFile workFile, int val, boolean isStart)
  {
    if ((curWork == null) || curWork.workFiles.isEmpty() || (workFile == null)) return;

    String str = val < 0 ? "" : String.valueOf(val);

    htWorkFiles.dataRows().forEach(row ->
    {
      if (workFile == row.getRecord())
        row.setCellValue(isStart ? 3 : 4, workFile, str);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateFromRecord()
  {
    btnTree.setDisable(ui.tree().getRowsForRecord(curWork).isEmpty());

    WorkTypeEnum workTypeEnumVal = curWork.getWorkTypeEnum();

    if (workTypeEnumVal == wtUnenteredSet)
      changeToUnenteredSetMode();
    else
    {
      changeToNormalMode();

      dateCtrls.setDate(curWork.getBibDate());
    }

    alreadyChangingTitle.setTrue();
    tfTitle.setText(curWork.name());
    alreadyChangingTitle.setFalse();

    if (curWork.getBibEntryKey().isBlank() == false)
    {
      tabEntry.setText(db.bibLibraryUserFriendlyName() + " entry");
      tpBib.getTabs().addFirst(tabEntry);
      tpBib.getSelectionModel().select(tabEntry);
      taEntry.appendText(curWork.getBibData().createReport());
    }

    taMiscBib.setText(curWork.getMiscBib());

    tfDOI.setText(curWork.getDOI());
    tfSearchKey.setText(curWork.getSearchKey());
    tfURL.setText(curWork.getURL());

    htISBN.buildRows(curWork.getISBNs(), (row, isbn) -> row.setCellValue(0, isbn, hdtNone));

    mainText.loadFromRecord(curWork, true, getView().getTextInfo());

    if (curWork.workType.isNotNull())
    {
      hcbType.selectIDofRecord(curWork.workType);
      getTab().setGraphic(imgViewForRecord(curWork, hdtWork));
    }

  // Populate authors
  // ----------------

    htAuthors.buildRows(curWork.getAuthors(), (row, author) ->
    {
      HDT_Person authorRecord = author.getPerson();

      if (authorRecord == null)
      {
        Populator pop = htAuthors.getPopulator(1);
        pop.populate(false);
        pop.addEntry(author.nameLastFirst());
        row.setCellValue(1, author.nameLastFirst(), hdtPerson);
      }
      else
        row.setCellValue(1, authorRecord);

      row.setCheckboxValue(2, author.getIsEditor());
      row.setCheckboxValue(3, author.getIsTrans());
    });

  // Populate Labels
  // ---------------

    htLabels.buildRows(curWork.labelStream(), (row, label) -> row.setCellValue(2, label, label.extendedText()));

  // Populate parent and child works
  // -------------------------------

    hcbLargerWork.selectIDofRecord(curWork.largerWork);

    htSubworks.buildRows(curWork.subWorks, (row, subWork) ->
    {
      if (subWork.authorRecords.size() > 0)
        row.setCellValue(0, subWork.authorRecords.getFirst(), subWork.getLongAuthorsStr());
      else
        row.setCellValue(0, subWork, subWork.getLongAuthorsStr());

      row.setCellValue(1, subWork);
      row.setCellValue(2, new BibDateHTC(subWork, subWork.getBibDate()));
    });

  // Populate arguments/stances
  // --------------------------

    htArguments.buildRows(curWork.arguments, (row, arg) ->
    {
      if (arg.positions.size() > 0)
      {
        HDT_Position target = arg.positions.getFirst();
        row.setIconCellValue(0, target);
        row.setCellValue(1, target);

        nullSwitch(arg.getPosVerdict(target), verdict -> row.setCellValue(2, verdict));
      }
      else if (arg.targetArgs.size() > 0)
      {
        HDT_Argument target = arg.targetArgs.getFirst();
        row.setIconCellValue(0, target);
        row.setCellValue(1, target);

        nullSwitch(arg.getArgVerdict(target), verdict -> row.setCellValue(2, verdict));
      }

      row.setCellValue(3, arg);
      row.setCellValue(4, new PageRangeHTC(arg, arg.pagesInWork(curWork)));
    });

  // Populate work files
  // -------------------

    refreshFiles();

  // Populate miscellaneous files
  // ----------------------------

    htMiscFiles.buildRows(curWork.miscFiles, (row, miscFile) -> row.setCellValue(2, miscFile));

  // Populate displayers, key mentioners, and investigations
  // -------------------------------------------------------

    populateDisplayersAndKeyMentioners();

  // Other stuff
  // -----------

    int subworkCnt  = curWork.subWorks .size(),
        miscFileCnt = curWork.miscFiles.size(),
        workFileCnt = curWork.workFiles.size();

    setTabCaption(tabWorkFiles, workFileCnt);
    setTabCaption(tabSubworks , subworkCnt);
    setTabCaption(tabMiscFiles, miscFileCnt);

    tfSearchKey.setText(curWork.getSearchKey());
    if (tfSearchKey.getText().isEmpty())
      if (strNotNullOrEmpty(curWork.getYearStr()))
        if (curWork.authorRecords.size() > 0)
          if (curWork.authorRecords.getFirst().getName().getSingle().isBlank() == false)
            tfSearchKey.setText(curWork.makeWorkSearchKey(true, false));

    FilePath filePath = curWork.filePathIncludeExt();
    boolean updatePreview = true;

    if (curWork == lastWork)
    {
      htWorkFiles.getTV().getSortOrder().setAll(List.copyOf(htWorkFiles.getTV().getSortOrder()));

      updatePreview = FilePath.isEmpty(filePath) || (filePath.equals(PreviewWindow.instance().getFilePath(pvsWorkTab)) == false);
    }
    else
    {
      BibManager.workRecordToAssign.setValue(null);

      SingleSelectionModel<Tab> tpsm = tabPane.getSelectionModel();

      if      (subworkCnt  > 0)                         tpsm.select(tabSubworks );
      else if (workFileCnt > 1)                         tpsm.select(tabWorkFiles);
      else if (miscFileCnt > 0)                         tpsm.select(tabMiscFiles);
      else if (tpsm.getSelectedItem() != tabBibDetails) tpsm.select(tabWorkFiles);

      if (FilePath.isEmpty(filePath) == false)
        if (filePath.equals(PreviewWindow.instance().getFilePath(pvsWorkTab)))
          if (curWork.getStartPageNum() < 2)
            updatePreview = false;
    }

    if (strNotNullOrEmpty(curWork.getBibEntryKey()))
    {
      ImageView iv = imgViewFromRelPath("resources/images/card-catalog.png");
      iv.setFitWidth(16);
      iv.setFitHeight(16);
      btnBibManager.setGraphic(iv);
      setToolTip(btnBibManager, "Go to " + db.bibLibraryUserFriendlyName() + " entry for this work");
    }

    if (updatePreview)
      PreviewWindow.setPreview(pvsWorkTab, filePath, curWork);
    else
      PreviewWindow.instance().refreshControls(pvsWorkTab);

    lastWork = curWork;

    safeFocus(tfTitle);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refreshFiles()
  {
    HDT_Folder folder = null;
    boolean notInSame = false;

    htWorkFiles.clearKeepSortOrder();

    for (HDT_WorkFile workFile : curWork.workFiles)
    {
      if (workFile.parentFolder() != null)
      {
        if (folder == null)
          folder = workFile.parentFolder();
        else if (folder != workFile.parentFolder())
          notInSame = true;
      }

      HyperTableRow row = htWorkFiles.newDataRow();
      row.setCheckboxValue(1, workFile.getAnnotated());
      row.setCellValue(2, workFile, workFile.getPath().getNameStr());

      int pageNum = curWork.getStartPageNum(workFile);
      if (pageNum > -1)
        row.setCellValue(3, workFile, String.valueOf(pageNum));

      pageNum = curWork.getEndPageNum(workFile);
      if (pageNum > -1)
        row.setCellValue(4, workFile, String.valueOf(pageNum));

      row.setCellValue(5, workFile, workFile.name());

      workFile.viewNow();
    }

    if (inNormalMode == false)
    {
      if (notInSame)
        tfURL.setText("(The files are located in multiple folders.)");
      else if (folder != null)
        tfURL.setText(folder.filePath().toString());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateDisplayersAndKeyMentioners()
  {
    Set<HDT_RecordWithMainText> set = new LinkedHashSet<>(), invSet = new LinkedHashSet<>();

    Stream<HDT_RecordWithMainText> stream = db.keyWorkMentionerStream(curWork, true).filter(recordWMT -> recordWMT.getType() != hdtWorkLabel);

    if (curWork.hasMainText())
      stream = Stream.concat(stream, db.displayerStream(curWork));

    stream.forEachOrdered(mentioner -> (mentioner.getType() == hdtInvestigation ? invSet : set).add(mentioner));

    htKeyMentioners.buildRows(Stream.concat(invSet.stream(), set.stream()), (row, mentioner) ->
    {
      row.setIconCellValue(0, mentioner);
      row.setCellValue(1, mentioner, mentioner.defaultChoiceText());
      row.setCellValue(2, mentioner, mentioner.getMainText().getPlainForDisplay());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void changeToNormalMode()
  {
    btnTopAutofill.setVisible(true);

    btnNewChapter.setText("New Chapter");
    setToolTip(btnNewChapter, "Create new work record having this work record as parent");

    dateCtrls   .setDisable(false);
    cbLargerWork.setDisable(false);

    btnLargerWork.setText("Larger Work:");
    setToolTip(btnLargerWork, "Go to parent record");

    btnFolder.setVisible(false);

    setAllVisible(true, cbLargerWork, apLowerRight, btnURL);

    tfURL.setEditable(true);

    if (inNormalMode) return;

    GridPane.setColumnSpan(apLowerMid, 1);

    AnchorPane.setLeftAnchor (btnURL, btnURLLeftAnchor);
    AnchorPane.setLeftAnchor (tfURL , tfURLLeftAnchor);
    AnchorPane.setRightAnchor(tfURL , tfURLRightAnchor);

    apLowerMid  .getChildren().remove(tfURL);
    apLowerRight.getChildren().add   (tfURL);

    inNormalMode = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addFolderMenuItem(String text, EventHandler<ActionEvent> handler)
  {
    MenuItem menuItem = new MenuItem(text);
    menuItem.setOnAction(handler);
    btnFolder.getItems().add(menuItem);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void changeToUnenteredSetMode()
  {
    if (inNormalMode == false) return;

    btnTopAutofill.setVisible(false);

    btnURLLeftAnchor = AnchorPane.getLeftAnchor (btnURL);
    tfURLLeftAnchor  = AnchorPane.getLeftAnchor (tfURL );
    tfURLRightAnchor = AnchorPane.getRightAnchor(tfURL );

    cbLargerWork.setDisable(true);
    dateCtrls   .setDisable(true);

    btnNewChapter.setText("Add Multiple Files");
    setToolTip(btnNewChapter, "Choose multiple files to add to this work record");

    GridPane.setColumnSpan(apLowerMid, GridPane.REMAINING);

    btnLargerWork.setText("Move All Files");
    setToolTip(btnLargerWork, "Select location where to move all files linked to this work record");

    tfURL.setEditable(false);

    setAllVisible(false, cbLargerWork, apLowerRight, btnURL);

    apLowerRight.getChildren().remove(tfURL);
    apLowerMid  .getChildren().add   (tfURL);

    btnFolder.setVisible(true);

    AnchorPane.setLeftAnchor(tfURL, btnFolder.getBoundsInParent().getMaxX() + 2.0);
    AnchorPane.setRightAnchor(tfURL, 2.0);

    inNormalMode = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initArgContextMenu()
  {
    htArguments.addContextMenuItem("Argument/Stance Record...", HDT_Argument.class,
      arg -> ui.goToRecord(arg, true));

    htArguments.addContextMenuItem("Position Record...", HDT_Argument.class,
      arg -> arg.positions.size() > 0,
      arg -> ui.goToRecord(arg.positions.getFirst(), true));

    htArguments.addContextMenuItem("Problem/Debate Record...", HDT_Argument.class,
      arg -> (arg.positions.isEmpty() == false) && (arg.positions.getFirst().getLargerDebate() != null),
      arg -> ui.goToRecord(arg.positions.getFirst().getLargerDebate(), true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void moveUnenteredWorkFile(HDT_WorkFile workFile)
  {
    int startPage = getCurPageNum(curWork, workFile, true ),
        endPage   = getCurPageNum(curWork, workFile, false);

    HDT_Work.sourceUnenteredWork = curWork;

    if (ui.importWorkFile(curWork.authorRecords.isEmpty() ? null : curWork.authorRecords.getFirst(), workFile.filePath(), true))
    {
      curWork.setStartPageNum(workFile, startPage);
      curWork.setEndPageNum(workFile, endPage);

      ui.update();
    }

    HDT_Work.sourceUnenteredWork = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void moveAllFiles()
  {
    if (ui.cantSaveRecord()) return;

    if (curWork.workFiles.isEmpty())
    {
      warningPopup("There are no files to move.");
      return;
    }

    FilePathSet files = new FilePathSet();

    curWork.workFiles.stream().map(HDT_RecordWithPath::filePath).forEach(files::add);

    MutableBoolean allSame = new MutableBoolean();
    FilePath folder = pickDirectory(true, files, allSame);

    if (folder == null) return;

    if (allSame.isTrue())
    {
      warningPopup("All of the files are already located in the destination folder.");
      return;
    }

    boolean startWatcher = folderTreeWatcher.stop();

    try
    {
      HDT_Folder folderRecord = HyperPath.getFolderFromFilePath(folder, true);

      for (HDT_WorkFile workFile : curWork.workFiles)
        if (workFile.getPath().moveToFolder(folderRecord.getID(), true, false, "") == false) break;
    }
    catch (IOException | HDB_InternalError e)
    {
      errorPopup("An error occurred while moving the files: " + getThrowableMessage(e));
    }

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();

    ui.update();
    FileManager.setNeedRefresh();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnNewChapterClick()
  {
    if (ui.cantSaveRecord()) return;

    if (HDT_Work.isUnenteredSet(curWork))
    {
      addMultipleFiles();
      return;
    }

    HDT_Work newWork = db.createNewBlankRecord(hdtWork);

    if (curWork.workFiles.isEmpty() == false)
      db.getObjectList(rtWorkFileOfWork, newWork, true).addAll(curWork.workFiles);
    else
    {
      String url = safeStr(curWork.getURL());

      if (url.startsWith(EXT_1))
        newWork.setURL(url);
    }

    newWork.setLargerWork(curWork.getID(), false);
    newWork.setWorkType(wtChapter);
    newWork.setBibDate(getDateFromUI());

    ui.goToRecord(newWork, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addMultipleFiles()
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().addAll(new FileChooser.ExtensionFilter("Adobe PDF file (*.pdf)", "*.pdf"),
                                             new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    fileChooser.setInitialDirectory(db.unenteredPath().toFile());

    List<File> files = showOpenMultipleDialog(fileChooser);
    if (collEmpty(files)) return;

    FilePathSet filePaths = files.stream().map(FilePath::new).collect(Collectors.toCollection(FilePathSet::new));

    for (FilePath filePath : filePaths)
    {
      if (filePath.isDirectory())
      {
        errorPopup("One of the selected files is a directory.");
        return;
      }

      HDT_RecordWithPath fileRecord = HyperPath.getRecordFromFilePath(filePath);

      if (fileRecord != null)
      {
        errorPopup(HyperPath.alreadyInUseMessage(filePath, fileRecord));
        return;
      }
    }

    MutableBoolean allSame = new MutableBoolean();

    FilePath folder = pickDirectory(false, filePaths, allSame);

    if (folder == null) return;

    DialogResult moveOrCopy = mrCopy;

    if (allSame.booleanValue() == false)
    {
      moveOrCopy = new PopupDialog("Should the files be moved or copied from their present location?")

        .addDefaultButton("Move", mrMove)
        .addButton       ("Copy", mrCopy)

        .showModal();
    }

    try
    {
      for (FilePath srcFilePath : filePaths)
      {
        if ((moveOrCopy == mrMove) && (srcFilePath.canObtainLock() == false))
        {
          errorPopup("Unable to obtain lock on path: \"" + srcFilePath + '"');
          return;
        }

        FilePath destFilePath = folder.getDirOnly().resolve(srcFilePath.getNameOnly());

        if (destFilePath.canObtainLock() == false)
        {
          errorPopup("Unable to obtain lock on path: \"" + destFilePath + '"');
          return;
        }
      }
    }
    catch (IOException e)
    {
      errorPopup("An error occurred: " + getThrowableMessage(e));
      return;
    }

    boolean startWatcher = folderTreeWatcher.stop();

    for (FilePath srcFilePath : filePaths)
    {
      FilePath destFilePath = folder.getDirOnly().resolve(srcFilePath.getNameOnly());

      if (srcFilePath.equals(destFilePath) == false)
      {
        try
        {
          if (moveOrCopy == mrMove)
          {
            srcFilePath.moveTo(destFilePath, true);
            db.unmapFilePath(srcFilePath);
          }
          else
            srcFilePath.copyTo(destFilePath, true);
        }
        catch (IOException e)
        {
          errorPopup("Unable to " + (moveOrCopy == mrCopy ? "copy" : "move") + " the file: \"" + srcFilePath.getNameOnly() + "\". Reason: " + getThrowableMessage(e));
          ui.update();
          FileManager.setNeedRefresh();

          if (startWatcher)
            folderTreeWatcher.createNewWatcherAndStart();

          return;
        }
      }

      HDT_WorkFile workFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, destFilePath);
      if (workFile == null)
      {
        internalErrorPopup(67830);
        ui.update();
        FileManager.setNeedRefresh();

        if (startWatcher)
          folderTreeWatcher.createNewWatcherAndStart();

        return;
      }

      curWork.addWorkFile(workFile.getID());
    }

    ui.update();
    FileManager.setNeedRefresh();

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePath pickDirectory(boolean moveOnly, FilePathSet files, MutableBoolean allSame)
  {
    DirectoryChooser dirChooser = new DirectoryChooser();
    FilePath folder = null,
             destPath = curWork.workFiles.size() > 0 ? curWork.filePath().getDirOnly() : db.unenteredPath();
    HDT_Folder folderRecord = null;

    while (folderRecord == null)
    {
      dirChooser.setTitle(moveOnly ? "Select destination folder files will be moved into" : "Select destination folder files will be moved or copied into");

      if (destPath.isDirectory())
        dirChooser.setInitialDirectory(destPath.toFile());
      else
      {
        folder = db.unenteredPath();
        if (folder.isDirectory())
          dirChooser.setInitialDirectory(folder.toFile());
      }

      folder = showDirDialog(dirChooser);

      if (FilePath.isEmpty(folder)) return null;

      folderRecord = HyperPath.getFolderFromFilePath(folder, true);

      if (folderRecord == null)
        errorPopup("You must choose a subfolder of the main database folder.");
    }

    allSame.setTrue();

    for (FilePath file : files)
    {
      FilePath path = folder.getDirOnly().resolve(file.getNameOnly());

      if (path.exists())
      {
        if (file.equals(path) == false)
        {
          errorPopup("A file with the name \"" + file.getNameOnly() + "\" already exists in the destination folder.");
          return null;
        }
      }
      else
        allSame.setFalse();
    }

    return folder;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    disableAll(btnUseDOI, btnUseISBN, btnMergeBib);

    taMiscBib    .clear();
    taEntry      .clear();
    taCrossref   .clear();
    taPdfMetadata.clear();
    taGoogleBooks.clear();

    disableCache(taMiscBib    );
    disableCache(taEntry      );
    disableCache(taCrossref   );
    disableCache(taPdfMetadata);
    disableCache(taGoogleBooks);

    pdfBDprop     .setValue(null);
    crossrefBDprop.setValue(null);
    googleBDprop  .setValue(null);

    tpBib.getTabs().removeAll(tabEntry, tabCrossref, tabPdfMetadata, tabGoogleBooks);

    stopRetrieving();

    alreadyChangingTitle.setTrue();
    tfTitle.setText("");
    alreadyChangingTitle.setFalse();

    tfDOI      .setText("");
    tfSearchKey.setText("");
    tfURL      .setText("");

    dateCtrls.clear();

    htISBN          .clear();
    htAuthors       .clear();
    htLabels        .clear();
    htSubworks      .clear();
    htArguments     .clear();
    htMiscFiles     .clear();
    htWorkFiles     .clearKeepSortOrder();
    htKeyMentioners .clear();

    tabPane.getTabs().forEach(tab -> tab.setText(tabCaptions.get(tab)));

    hcbType      .clear();
    hcbLargerWork.clear();

    mainText.clear();

    changeToNormalMode();

    getTab().setGraphic(imgViewForRecord(null, hdtWork));

    if (db.bibLibraryIsLinked())
    {
      ImageView iv = imgViewFromRelPath("resources/images/card-catalog_tr.png");
      iv.setFitWidth(16);
      iv.setFitHeight(16);

      btnBibManager.setVisible(true);
      btnBibManager.setGraphic(iv);
      setToolTip(btnBibManager, "Assign to " + db.bibLibraryUserFriendlyName() + " entry");
    }
    else
      btnBibManager.setVisible(false);

    curWork  = resetRecord ? null : HDT_Record.getCurrentInstance(curWork);
    lastWork = resetRecord ? null : HDT_Record.getCurrentInstance(lastWork);

    if ((curWork != lastWork) || (curWork == null))
    {
      htWorkFiles.getTV().getSortOrder().clear();

      if (curWork == null)
      {
        PreviewWindow.clearPreview(pvsWorkTab);
        BibManager.workRecordToAssign.setValue(null);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord(boolean saveNameIfBlank)
  {
    if (saveNameIfBlank && nameCheck(tfTitle, "title") == false)
      return false;

    WorkTypeEnum workTypeEnumVal = HDT_WorkType.workTypeIDToEnumVal(hcbType.selectedID());

    if (tfSearchKey.getText().isEmpty())
      lblSearchKeyClick();
    else if (strNotNullOrEmpty(curWork.getYearStr()))
    {
      if (tfSearchKey.getText().contains(curWork.getYearStr()))
        if (workTypeEnumVal != wtUnenteredSet)
          if (tfYear.getText().equals(curWork.getYearStr()) == false)
            if (confirmDialog("Year has been modified. Update search key?", true))
              if (tfSearchKey.getText().matches(".*" + Pattern.quote(curWork.getYearStr()) + "[a-z].*"))
                lblSearchKeyClick();
              else
              {
                String key = tfSearchKey.getText().replace(curWork.getYearStr(), tfYear.getText());
                if (key.equals(curWork.makeWorkSearchKey(getAuthorsFromUI(), tfYear.getText(), false, false)))
                  lblSearchKeyClick();  // Search key is same as what would be automatically generated minus letter(s) so use automatically generated search key with letter(s) added
                else
                  tfSearchKey.setText(key);  // User has customized search key so it is different from what would be automatically generated, so let them deal with duplicates manually
              }
    }

    if (saveSearchKey(curWork, tfSearchKey) == false) return false;

    if (saveNameIfBlank || (tfTitle.getText().isBlank() == false))
      curWork.setName(tfTitle.getText());

    curWork.workType.setID(hcbType.selectedID());

    htWorkFiles.dataRows().forEach(row -> nullSwitch((HDT_WorkFile)row.getRecord(), file ->
    {
      file.setAnnotated(row.getCheckboxValue(1));
      curWork.setStartPageNum(file, parseInt(row.getText(3), -1));
      curWork.setEndPageNum(file, parseInt(row.getText(4), -1));
      file.setName(row.getText(5));
    }));

    boolean needToSaveISBNs = true, noIsbnUpdate = false;

    if (curWork.largerWork.isNull())
    {
      saveISBNs();
      needToSaveISBNs = false;
    }
    else if (hcbLargerWork.selectedID() > 0)
    {
      if (curWork.largerWork.getID() != hcbLargerWork.selectedID())
        noIsbnUpdate = true;
    }

    if (tfURL.getText().startsWith(EXT_1) == false)
      curWork.clearExtFilePageNums();

    if (workTypeEnumVal == wtUnenteredSet)
    {
      curWork.setBibDate(BibliographicDate.EMPTY_DATE);
      curWork.setLargerWork(-1, true);
      curWork.setURL("");
    }
    else
    {
      curWork.setBibDate(getDateFromUI());
      curWork.setLargerWork(hcbLargerWork.selectedID(), noIsbnUpdate);
      curWork.setURL(tfURL.getText());
    }

    if (needToSaveISBNs)
      saveISBNs();

    curWork.setMiscBib(taMiscBib.getText());
    curWork.setDOI(tfDOI.getText());

    mainText.save();

    curWork.setAuthors(getAuthorGroups());
    MainText.setKeyWorkMentioners(curWork, htLabels.saveToList(2, hdtWorkLabel), HDT_WorkLabel.class);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void newClick(RecordType objType, HyperTableRow row)
  {
    switch (objType)
    {
      case hdtMiscFile :

        if (ui.cantSaveRecord()) return;

        HDT_MiscFile file = db.createNewBlankRecord(hdtMiscFile);
        file.work.set(curWork);
        ui.goToRecord(file, false);

        break;

      default:
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showWorkDialog(HDT_WorkFile workFile, boolean saveFirst) { return showWorkDialog(workFile, saveFirst, null, null, Ternary.Unset, EntryType.etUnentered); }

  public boolean showWorkDialog(HDT_WorkFile workFile, boolean saveFirst, FilePath filePathToUse, BibData bdToUse, Ternary newEntryChoice, EntryType newEntryType)
  {
    if (saveFirst && ui.cantSaveRecord(false)) return false;

    if (HDT_Work.isUnenteredSet(curWork))
    {
      fdc = new FileDlgCtrlr("Unentered Work File", workFile, curWork);

      fdc.setSrcFilePath(filePathToUse, true);

      boolean result = fdc.showModal();
      fdc = null;

      if (result == false) return false;
    }
    else
    {
      wdc = (workFile == null) && (filePathToUse != null) ?
        new WorkDlgCtrlr(filePathToUse, bdToUse, newEntryChoice, newEntryType)
      :
        new WorkDlgCtrlr(workFile);

      boolean result = wdc.showModal();

      if (result == false)
      {
        wdc = null;
        return false;
      }

      if (wdc.getCreateEntry())
        curWork.setBibEntryKey(db.getBibLibrary().addEntry(wdc.getEntryType()).getKey());

      curWork.getBibData().copyAllFieldsFrom(wdc.getBibDataFromGUI(), false, false);

      curWork.setAuthors(wdc.getAuthorGroups());

      wdc = null;
    }

    ui.update();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getDoiFromBibTab()
  {
    return tpBib.getSelectionModel().getSelectedItem() == tabMiscBib ?
      matchDOI(taMiscBib.getText())
    :
      nullSwitch(getBibDataFromBibTab(), "", bd -> bd.getStr(bfDOI));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateBibButtons()
  {
    btnUseDOI .setDisable(getDoiFromBibTab  ().isEmpty());
    btnUseISBN.setDisable(getIsbnsFromBibTab().isEmpty());

    setToolTip(btnUseDOI , "Set current work's DOI to "     + getDoiFromBibTab());
    setToolTip(btnUseISBN, "Add ISBN(s) " + String.join("; ", getIsbnsFromBibTab()) + " to current work's ISBN(s)");

    tabPane.requestLayout();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<String> getIsbnsFromBibTab()
  {
    return tpBib.getSelectionModel().getSelectedItem() == tabMiscBib ?
      matchISBN(taMiscBib.getText())
    :
      nullSwitch(getBibDataFromBibTab(), new ArrayList<>(), bd -> bd.getMultiStr(bfISBNs));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BibDataStandalone getBibDataFromBibTab()
  {
    Tab curTab = tpBib.getSelectionModel().getSelectedItem();

    if (curTab == tabPdfMetadata) return pdfBDprop     .getValue();
    if (curTab == tabCrossref   ) return crossrefBDprop.getValue();
    if (curTab == tabGoogleBooks) return googleBDprop  .getValue();

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void mnuShowMetadataClick()
  {
    stopRetrieving();

    taPdfMetadata.clear();
    pdfBDprop.setValue(null);

    if (tpBib.getTabs().contains(tabPdfMetadata) == false)
      tpBib.getTabs().add(tabPdfMetadata);

    tpBib.getSelectionModel().select(tabPdfMetadata);

    List<FilePath> pdfFilePaths = curWork.workFiles.stream().filter(HDT_WorkFile::pathNotEmpty)
                                                            .map(HDT_WorkFile::filePath)
                                                            .toList();
    try
    {
      pdfBDprop.setValue(PDFBibData.createFromFiles(pdfFilePaths));

      if (pdfBDprop.getValue() == null)
        pdfBDprop.setValue(PDFBibData.createFromFiles(safeListOf(db.resolveExtFilePath(tfURL.getText()))));

      if (pdfBDprop.getValue() == null)
        taPdfMetadata.setText("[No PDF file.]");
      else
        taPdfMetadata.appendText(pdfBDprop.getValue().createReport());
    }
    catch (IOException e)
    {
      taPdfMetadata.setText("[Error: " + getThrowableMessage(e) + ']');
    }

    updateBibButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void retrieveBibData(boolean crossref, String doi, List<String> isbns)
  {
    stopRetrieving();

    if (db.isOffline() || (curWork == null)) return;

    Tab      tab = crossref ? tabCrossref : tabGoogleBooks;
    TextArea ta  = crossref ? taCrossref  : taGoogleBooks;

    if (tpBib.getTabs().contains(tab) == false)
      tpBib.getTabs().add(tab);

    tpBib.getSelectionModel().select(tab);

    ta.clear();
    setAllVisible(true, btnStop, progressBar);

    tabPane.requestLayout();
    httpClient.clearLastUrl();

    Consumer<BibDataStandalone> doneHndlr = queryBD ->
    {
      setAllVisible(false, btnStop, progressBar);

      if (crossref) crossrefBDprop.setValue((CrossrefBibData) queryBD);
      else          googleBDprop  .setValue((GoogleBibData  ) queryBD);

      ta.setText("Query URL: " + httpClient.lastUrl() + System.lineSeparator());

      if (queryBD == null)
      {
        ta.appendText("[No results.]");
        return;
      }

      ta.appendText(queryBD.createReport());
    };

    GUIBibData bd = new GUIBibData();
    bd.setAllAuthorsFromTable(getAuthorGroups());

    if (crossref)
    {
      if (strNullOrBlank(doi))
      {
        bd.setTitle(tfTitle.getText());
        bd.setDate(BibliographicDate.fromYearStr(tfYear.getText(), false));
      }
      else
        bd.setStr(bfDOI, doi);

      bibDataRetriever = BibDataRetriever.forCrossref(httpClient, bd, doneHndlr);
    }
    else
    {
      if (collEmpty(isbns))
      {
        bd.setTitle(tfTitle.getText());
        bd.setDate(BibliographicDate.fromYearStr(tfYear.getText(), false));
      }
      else
        bd.setMultiStr(bfISBNs, isbns);

      bibDataRetriever = BibDataRetriever.forGoogleBooks(httpClient, bd, doneHndlr);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stopRetrieving()
  {
    httpClient.stop();
    if (bibDataRetriever != null)
      bibDataRetriever.stop();

    setAllVisible(false, btnStop, progressBar);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnAutofillClick()
  {
    stopRetrieving();

    if (db.isOffline() || (curWork == null) || ui.cantSaveRecord(false)) return;

    List<FilePath> pdfFilePaths = curWork.workFiles.stream().filter(HDT_WorkFile::pathNotEmpty)
                                                            .map(HDT_WorkFile::filePath)
                                                            .toList();
    if (collEmpty(pdfFilePaths))
      pdfFilePaths = safeListOf(db.resolveExtFilePath(tfURL.getText()));

    BibData workBD = curWork.getBibData();

    tabPane.getSelectionModel().select(tabBibDetails);
    setAllVisible(true, btnStop, progressBar);

    bibDataRetriever = new BibDataRetriever(httpClient, workBD, pdfFilePaths, (pdfBD, queryBD, messageShown) ->
    {
      setAllVisible(false, btnStop, progressBar);
      if ((pdfBD == null) && (queryBD == null))
      {
        if (messageShown == false)
          infoPopup("Unable to find bibliographic information.");

        return;
      }

      MergeWorksDlgCtrlr mwd;

      try
      {
        mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", Stream.of(workBD, pdfBD, queryBD), curWork, false, true, Ternary.Unset);
      }
      catch (IOException e)
      {
        errorPopup("Unable to initialize merge dialog window.");
        return;
      }

      if (mwd.showModal() == false) return;

      BibData destBD = workBD;

      if (mwd.creatingNewEntry().isTrue())
      {
        BibEntry<?, ?> entry = db.getBibLibrary().addEntry(mwd.getEntryType());
        curWork.setBibEntryKey(entry.getKey());
        destBD = entry;
      }

      mwd.mergeInto(destBD);
      BibManager.refresh();
      ui.update();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnMergeBibClick()
  {
    if (ui.cantSaveRecord(false)) return;

    MergeWorksDlgCtrlr mwd;
    BibData workBibData = curWork.getBibData();

    try
    {
      mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", Stream.of(workBibData, pdfBDprop.getValue(), crossrefBDprop.getValue(), googleBDprop.getValue()), curWork, false, true, Ternary.Unset);
    }
    catch (IOException e)
    {
      errorPopup("Unable to initialize merge dialog window.");
      return;
    }

    if (mwd.showModal() == false) return;

    if (mwd.creatingNewEntry().isTrue())
    {
      BibEntry<?, ?> entry = db.getBibLibrary().addEntry(mwd.getEntryType());
      curWork.setBibEntryKey(entry.getKey());
      workBibData = entry;
    }

    mwd.mergeInto(workBibData);
    BibManager.refresh();
    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spVert      , DividerPositionPrefKey.WORK_MID_VERT    , 0);
    setDividerPosition(spVert      , DividerPositionPrefKey.WORK_BOTTOM_VERT , 1);
    setDividerPosition(spHoriz1    , DividerPositionPrefKey.WORK_RIGHT_HORIZ , 0);
    setDividerPosition(spMentioners, DividerPositionPrefKey.WORK_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spVert      , DividerPositionPrefKey.WORK_MID_VERT    , 0);
    getDividerPosition(spVert      , DividerPositionPrefKey.WORK_BOTTOM_VERT , 1);
    getDividerPosition(spHoriz1    , DividerPositionPrefKey.WORK_RIGHT_HORIZ , 0);
    getDividerPosition(spMentioners, DividerPositionPrefKey.WORK_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getBibDataFromGUI(GUIBibData bd)
  {
    List<String> isbns = htISBN.dataRowStream().map(row -> row.getText(0)).toList();

    bd.setMultiStr(bfISBNs, isbns);

    bd.setTitle(tfTitle.getText());
    bd.setDate(getDateFromUI());
    bd.setStr(bfURL, tfURL.getText());
    bd.setStr(bfDOI, tfDOI.getText());
    bd.setWorkType(hcbType.selectedRecord());

    bd.setMultiStr(bfMisc, convertMultiLineStrToStrList(taMiscBib.getText(), true));

    bd.setAllAuthorsFromTable(getAuthorGroups());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void updateWebButtons(Preferences node)
  {
    updateWebButtons(node, WebButtonContextPrefKey.WORK, 2, btnWebSrch1, smbWebSrch1, this::searchBtnEvent);

    btnWebSrch2.setText(ui.webButtonMap.get(WebButtonContextPrefKey.WORK + '2').getCaption());
    mnuGoogle  .setText("Search this DOI using " + ui.webButtonMap.get(WebButtonContextPrefKey.DOI).getCaption());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EventHandler<ActionEvent> searchBtnEvent(String prefKey)
  {
    return event -> ui.webButtonMap.get(prefKey).first(WebButtonField.SingleName, getFirstAuthorSingleName())
                                                .next (WebButtonField.Title, tfTitle.getText())
                                                .next (WebButtonField.QueryTitle, tfTitle.getText())
                                                .next (WebButtonField.NumericYear, tfYear.getText())
                                                .next (WebButtonField.doi, tfDOI.getText())
                                                .next (WebButtonField.ISBN, htISBN.dataRowStream().map(row -> row.getText(0)).findFirst().orElse(""))
                                                .go();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void showWorkFile(HDT_WorkFile workFile)
  {
    tabPane.getSelectionModel().select(tabWorkFiles);

    htWorkFiles.selectRowByRecord(workFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final String NO_EXT_PATH_MESSAGE = "No external file path has been set in Settings.";

  public boolean processDragEvent(DragEvent event)
  {
    if (tfURL.localToScene(tfURL.getBoundsInLocal()).contains(event.getSceneX(), event.getSceneY()) == false)
      return false;

    Dragboard board = event.getDragboard();
    List<File> files = board.getFiles();
    if (collEmpty(files))
      return false;

    FilePath filePath = new FilePath(files.getFirst());
    if (db.getRootPath().isSubpath(filePath))
      return false;

    FilePath extPath = db.extPath();
    if (FilePath.isEmpty(extPath))
    {
      errorPopup(NO_EXT_PATH_MESSAGE);
      return true;
    }

    if (extPath.isSubpath(filePath) == false)
      return false;

    tfURL.setText(EXT_1 + FilenameUtils.separatorsToUnix(extPath.relativize(filePath).toString()));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
