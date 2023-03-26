/*
 * Copyright 2015-2023 Jason Winning
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
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibDataRetriever;
import org.hypernomicon.bib.data.BibDataStandalone;
import org.hypernomicon.bib.data.CrossrefBibData;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.data.GUIBibData;
import org.hypernomicon.bib.data.GoogleBibData;
import org.hypernomicon.bib.data.PDFBibData;
import org.hypernomicon.dialogs.ChooseParentWorkFileDlgCtrlr;
import org.hypernomicon.dialogs.FileDlgCtrlr;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.dialogs.WorkDlgCtrlr;
import org.hypernomicon.dialogs.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.SearchKeys;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.WorkAuthors;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTableCell.CellSortMethod;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.view.MainCtrlr.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.workTabEnum;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.prefs.Preferences;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;

import javafx.application.Platform;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.SplitMenuButton;
import javafx.scene.control.SplitPane;
import javafx.scene.control.SplitPane.Divider;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.image.ImageView;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.InputEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------

public class WorkTabCtrlr extends HyperTab<HDT_Work, HDT_Work>
{
  @FXML private AnchorPane apDescription, apLowerMid, apLowerRight;
  @FXML private Button btnBibManager, btnLargerWork, btnLaunch, btnMergeBib, btnNewChapter, btnURL,
                       btnStop, btnTree, btnUseDOI, btnUseISBN, btnWebSrch1, btnWebSrch2, btnAutofill, btnTopAutofill;
  @FXML private ComboBox<HyperTableCell> cbLargerWork, cbType;
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
  @FXML private TextField tfDOI, tfURL, tfSearchKey, tfTitle;

  @FXML public TextField tfYear;

  private final HyperTable htLabels, htSubworks, htArguments, htMiscFiles, htWorkFiles, htKeyMentioners, htISBN;
  private final HyperCB hcbLargerWork;
  private final MainTextWrapper mainText;
  private final Map<Tab, String> tabCaptions = new HashMap<>();
  private final MenuItemSchema<HDT_Record, HyperTableRow> isbnSrchMenuItemSchema;
  private final MutableBoolean alreadyChangingTitle = new MutableBoolean(false);
  private final ObjectProperty<CrossrefBibData> crossrefBDprop = new SimpleObjectProperty<>();
  private final ObjectProperty<PDFBibData>      pdfBDprop      = new SimpleObjectProperty<>();
  private final ObjectProperty<GoogleBibData>   googleBDprop   = new SimpleObjectProperty<>();

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
//---------------------------------------------------------------------------

  public WorkTabCtrlr(Tab tab) throws IOException
  {
    super(workTabEnum, tab, "view/tabs/WorkTab");

    mainText = new MainTextWrapper(apDescription);

    tabPane.setStyle("-fx-open-tab-animation: NONE; -fx-close-tab-animation: NONE;");
    tpBib  .setStyle("-fx-open-tab-animation: NONE; -fx-close-tab-animation: NONE;");

    tabPane.getTabs().forEach(subTab -> tabCaptions.put(subTab, subTab.getText()));

    setToolTip(btnWebSrch1, TOOLTIP_PREFIX + "WorldCat");
    setToolTip(btnWebSrch2, TOOLTIP_PREFIX + "Google Scholar");
    setToolTip(btnAutofill, AUTOFILL_TOOLTIP);
    setToolTip(btnTopAutofill, AUTOFILL_TOOLTIP);

    setSearchKeyToolTip(tfSearchKey);

    htAuthors = new HyperTable(tvAuthors, 1, true, PREF_KEY_HT_WORK_AUTHORS);

    htAuthors.addActionCol(ctGoBtn, 1);
    htAuthors.addCol(hdtPerson, ctDropDownList);
    htAuthors.addCheckboxCol();
    htAuthors.addCheckboxCol();

    htAuthors.addRemoveMenuItem();
    htAuthors.addChangeOrderMenuItem(true);

    htAuthors.addContextMenuItem("Remove this row",
      row -> (row.getText(1).length() > 0) && (row.getID(1) < 1),
      htAuthors::removeRow);

    htAuthors.addContextMenuItem("Create person record",
      row -> (row.getText(1).length() > 0) && (row.getID(1) < 1),
      row ->
      {
        if (ui.cantSaveRecord()) return;

        String text = row.getText(1);

        Ternary isInFileName = Ternary.Unset;
        Author author = curWork.getAuthors().getAuthor(new PersonName(text));
        if (author != null)
          isInFileName = author.getInFileName();

        HDT_Person otherPerson = otherPersonToUse(text);

        if (otherPerson != null)
        {
          htAuthors.selectID(1, row, otherPerson.getID());
          saveToRecord();
          curWork.setPersonIsInFileName(otherPerson, isInFileName);
          ui.update();
          return;
        }

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

    htLabels = new HyperTable(tvLabels, 2, true, PREF_KEY_HT_WORK_LABELS);

    htLabels.addActionCol(ctGoBtn, 2);
    htLabels.addActionCol(ctBrowseBtn, 2);
    htLabels.addCol(hdtWorkLabel, ctDropDownList);

    htLabels.addRemoveMenuItem();

    htSubworks = new HyperTable(tvSubworks, 1, false, PREF_KEY_HT_WORK_SUB);

    htSubworks.addLabelCol(hdtPerson);
    htSubworks.addLabelCol(hdtWork);
    htSubworks.addLabelCol(hdtWork);

    htSubworks.addContextMenuItem("Go to person record", HDT_Person.class,
      person -> ui.goToRecord(person, true));

    htSubworks.addContextMenuItem("Go to work record", HDT_Work.class,
      work -> ui.goToRecord(work, true));

    htSubworks.addDefaultMenuItems();

    htSubworks.addChangeOrderMenuItem(false, () -> curWork.subWorks.reorder(htSubworks.saveToList(1, hdtWork), true));

    htKeyMentioners = new HyperTable(tvKeyMentions, 1, false, PREF_KEY_HT_WORK_MENTIONERS);

    htKeyMentioners.addIconCol();
    htKeyMentioners.addLabelCol(hdtNone);
    htKeyMentioners.addLabelCol(hdtNone);

    htKeyMentioners.addDefaultMenuItems();

    htArguments = new HyperTable(tvArguments, 3, false, PREF_KEY_HT_WORK_ARG);

    htArguments.addIconCol();
    htArguments.addLabelCol(hdtNone);
    htArguments.addLabelCol(hdtNone);      // record type = hdtNone so that the column will sort purely based on displayed text
    htArguments.addLabelCol(hdtArgument);

    htWorkFiles = new HyperTable(tvWorkFiles, 2, true, PREF_KEY_HT_WORK_FILES);

    htWorkFiles.addRefreshHandler(tabPane::requestLayout);

    htWorkFiles.addActionColWithButtonHandler(ctEditNewBtn, 2, (row, colNdx) -> showWorkDialog(row.getRecord(colNdx)));

    htWorkFiles.addCheckboxCol();
    htWorkFiles.addLabelCol(hdtWorkFile);
    htWorkFiles.addTextEditColWithUpdateHandler(hdtWorkFile, false, true, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      int startPageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);
      if (startPageNum < 0) return;

      HDT_WorkFile workFile = row.getRecord();
      if (workFile == null) return;

      int endPageNum = parseInt(row.getText(nextColNdx), -1);

      previewWindow.setPreview(pvsWorkTab, workFile.filePath(), startPageNum, endPageNum, curWork);
    });

    htWorkFiles.addTextEditColWithUpdateHandler(hdtWorkFile, false, true, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      int endPageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);
      if (endPageNum < 0) return;

      HDT_WorkFile workFile = row.getRecord();
      if (workFile == null) return;

      int startPageNum = parseInt(row.getText(nextColNdx - 2), -1);

      previewWindow.setPreview(pvsWorkTab, workFile.filePath(), startPageNum, endPageNum, curWork);
    });

    htWorkFiles.addTextEditCol(hdtWorkFile, false, false);

    htWorkFiles.setTooltip(0, ButtonAction.baEdit, "Update or rename this work file");
    htWorkFiles.setTooltip(0, ButtonAction.baNew, "Add a new work file");

    htWorkFiles.addContextMenuItem("Launch file", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> launchWorkFile(workFile.filePath(), getCurPageNum(curWork, workFile, true)));

    htWorkFiles.addContextMenuItem("Show in system explorer", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> highlightFileInExplorer(workFile.filePath()));

    htWorkFiles.addContextMenuItem("Show in File Manager", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> ui.goToFileInManager(workFile.filePath()));

    htWorkFiles.addContextMenuItem("Copy path to clipboard", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      workFile -> copyToClipboard(workFile.getPath().toString()));

    htWorkFiles.addContextMenuItem("Update or rename this work file", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
      this::showWorkDialog);

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

        if (confirmDialog("Are you sure you want to remove this file from the work record?") == false) return;

        db.getObjectList(rtWorkFileOfWork, curWork, true).remove(workFile);
        fileManagerDlg.setNeedRefresh();
        ui.update();
      });

    htWorkFiles.addChangeOrderMenuItem(true, () -> db.getObjectList(rtWorkFileOfWork, curWork, true).reorder(htWorkFiles.saveToList(2, hdtWorkFile)));

    htWorkFiles.setDblClickHandler(HDT_WorkFile.class, workFile -> launchWorkFile(workFile.filePath(), getCurPageNum(curWork, workFile, true)));

    tvWorkFiles.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (oldValue == newValue)) return;

      HDT_WorkFile workFile = newValue.getRecord();
      if (workFile == null)
        previewWindow.setPreview(pvsWorkTab, curWork.filePathIncludeExt(), getCurPageNum(curWork, null, true), getCurPageNum(curWork, null, false), curWork);
      else
        previewWindow.setPreview(pvsWorkTab, workFile.filePath(), getCurPageNum(curWork, workFile, true), getCurPageNum(curWork, workFile, false), curWork);
    });

    tvSubworks.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (oldValue == newValue)) return;

      HDT_Work subWork = newValue.getRecord();
      previewWindow.setPreview(pvsWorkTab, subWork.filePathIncludeExt(), subWork.getStartPageNum(), subWork.getEndPageNum(), subWork);
    });

    htMiscFiles = new HyperTable(tvMiscFiles, 1, true, PREF_KEY_HT_WORK_MISC);

    htMiscFiles.addActionCol(ctGoNewBtn, 1);
    htMiscFiles.addLabelCol(hdtMiscFile);

    htMiscFiles.addDefaultMenuItems();

    htISBN = new HyperTable(tvISBN, 0, true, "");

    htISBN.addTextEditCol(hdtWork, true, false);

    isbnSrchMenuItemSchema = htISBN.addContextMenuItem("WorldCat",
      row ->
      {
        List<String> list = matchISBN(row.getText(0));
        return collEmpty(list) == false;
      },
      row -> ui.webButtonMap.get(PREF_KEY_ISBN_SRCH).first(WebButtonField.ISBN, row.getText(0)).go());

    htISBN.addContextMenuItem("Google Books query",
      row -> row.getText(0).length() > 0,
      row ->
      {
        List<String> list = matchISBN(row.getText(0));
        if (collEmpty(list) == false)
          retrieveBibData(false, "", list);
      });

    htISBN.addRefreshHandler(tabPane::requestLayout);

    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtWorkType), true);
    hcbLargerWork = new HyperCB(cbLargerWork, ctDropDownList, new StandardPopulator(hdtWork), true);

    btnWebSrch1.setOnAction(searchBtnEvent(PREF_KEY_WORK_SRCH + '1'));
    smbWebSrch1.setOnAction(searchBtnEvent(PREF_KEY_WORK_SRCH + '1'));
    btnWebSrch2.setOnAction(searchBtnEvent(PREF_KEY_WORK_SRCH + '2'));

    btnDOI.setOnAction(event -> searchDOI(tfDOI.getText()));

    btnBibManager.setOnAction(event -> ui.goToWorkInBibManager(curWork));

    btnStop.setOnAction(event -> stopRetrieving());

    mnuFindDOIonCrossref.setOnAction(event -> retrieveBibData(true, "", null));
    mnuFindISBNonGoogleBooks.setOnAction(event -> retrieveBibData(false, "", null));

    mnuGoogle.setOnAction(event ->
    {
      if (tfDOI.getText().isBlank()) return;

      ui.webButtonMap.get(PREF_KEY_DOI_SRCH).first(WebButtonField.doi, tfDOI.getText()).go();
    });

    mnuCrossref.setOnAction(event ->
    {
      if (tfDOI.getText().length() > 0)
        retrieveBibData(true, tfDOI.getText(), null);
    });

    mnuShowMetadata.setOnAction(event -> mnuShowMetadataClick());

    mnuStoreMetadata.setVisible(false); // Not implemented yet

    btnUseDOI.setOnAction(event -> useDOIClick());
    btnUseISBN.setOnAction(event -> useISBNClick());
    btnAutofill.setOnAction(event -> btnAutofillClick());
    btnTopAutofill.setOnAction(event -> btnAutofillClick());

    btnLaunch.setOnAction(event ->
    {
      if (curWork.workFiles.isEmpty())
        urlClick();
      else
        curWork.launch(getCurPageNum(curWork, null, true));
    });

    btnURL.setOnAction(event -> urlClick());

    btnLargerWork.setOnAction(event ->
    {
      if (inNormalMode)
        ui.goToRecord(HyperTableCell.getRecord(hcbLargerWork.selectedHTC()), true);
      else
        moveAllFiles();
    });

    initArgContextMenu();
    btnTree.setOnAction(event -> ui.goToTreeRecord(curWork));

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
          messageDialog("You cannot change the work type after it has been set to Unentered Set of Work Files.", mtError);
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
          messageDialog("You cannot change a work with an existing work type into an unentered set of work files.", mtError);
          programmaticTypeChange = false;

          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }

        if (curWork.getBibEntryKey().length() > 0)
        {
          programmaticTypeChange = true;
          messageDialog("You cannot change a work that is assigned to a " + db.getBibLibrary().type().getUserFriendlyName() + " entry into an unentered set of work files.", mtError);
          programmaticTypeChange = false;

          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }
      }

      changeToUnenteredSetMode();
    });

    Divider div1 = spHoriz1.getDividers().get(0),
            div2 = spHoriz2.getDividers().get(0);

    div1.positionProperty().bindBidirectional(div2.positionProperty());

    EventHandler<ActionEvent> handler = event ->
    {
      if ((tfURL.getText().length() > 0) && (tfURL.getText().charAt(0) != '('))
        launchFile(new FilePath(tfURL.getText()));
    };

    btnFolder.setOnAction(handler);
    addFolderMenuItem("Show in system explorer", handler);

    addFolderMenuItem("Show in file manager", event ->
    {
      if ((tfURL.getText().length() > 0) && (tfURL.getText().charAt(0) != '('))
        ui.goToFileInManager(new FilePath(tfURL.getText()));
    });

    setToolTip(lblSearchKey, "Regenerate search key");

    lblSearchKey.setOnMouseClicked(event -> lblSearchKeyClick());

    setToolTip(lblTitle, "Reformat title");

    lblTitle.setOnMouseClicked(event ->
    {
      String title = HDT_Work.fixCase(convertToSingleLine(ultraTrim(tfTitle.getText())));

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

    tabPdfMetadata.setOnClosed(event -> { taPdfMetadata.clear(); pdfBDprop     .set(null); });
    tabCrossref   .setOnClosed(event -> { taCrossref   .clear(); crossrefBDprop.set(null); });
    tabGoogleBooks.setOnClosed(event -> { taGoogleBooks.clear(); googleBDprop  .set(null); });

    btnMergeBib.setOnAction(event -> btnMergeBibClick());

    tabPane.addEventFilter(InputEvent.ANY, event -> tabPane.requestLayout()); // Fix for https://sourceforge.net/p/hypernomicon/tickets/18/
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String recordName()               { return tfTitle.getText(); }
  @Override protected RecordType type()              { return hdtWork; }
  @Override public void setRecord(HDT_Work work)     { curWork = work; }
  @Override public MainTextWrapper mainTextWrapper() { return mainText; }

  private List<Author> getAuthorsFromUI()      { return WorkAuthors.getListFromObjectGroups(getAuthorGroups(), curWork); }
  public String getShortAuthorsStr()           { return Authors.getShortAuthorsStr(getAuthorsFromUI().stream(), false, true, true); }
  private List<ObjectGroup> getAuthorGroups()  { return htAuthors.getAuthorGroups(curWork, 1, -1, 2, 3); }
  private void lblSearchKeyClick()             { tfSearchKey.setText(makeWorkSearchKey(getAuthorsFromUI(), tfYear.getText(), curWork)); }
  public String getTitle()                     { return tfTitle.getText(); }
  private void setTabCaption(Tab tab, int cnt) { tab.setText(tabCaptions.get(tab) + " (" + cnt + ')'); }
  private void saveISBNs()                     { curWork.setISBNs(htISBN.dataRowStream().map(row -> row.getText(0)).collect(Collectors.toList())); }
  private void useDOIClick()                   { tfDOI.setText(getDoiFromBibTab()); }
  private void useISBNClick()                  { htISBN.buildRows(getIsbnsFromBibTab(), (row, isbn) -> row.setCellValue(0, isbn, hdtNone)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void urlClick()
  {
    String url = tfURL.getText();

    if (url.startsWith(EXT_1) && (extPath() == null))
    {
      messageDialog(WorkTabCtrlr.NO_EXT_PATH_MESSAGE, mtWarning);
      return;
    }

    FilePath filePath = resolveExtFilePath(url);

    if (FilePath.isEmpty(filePath))
      openWebLink(url);
    else
      launchWorkFile(filePath, curWork.getStartPageNum());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateMergeButton()
  {
    btnMergeBib.setDisable((crossrefBDprop.get() == null) && (pdfBDprop.get() == null) && (googleBDprop.get() == null));

    tabPane.requestLayout();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_Person otherPersonToUse(String text)
  {
    return nullSwitch(HDT_Person.lookUpByName(new PersonName(text)), null, otherPerson ->
                      confirmDialog(otherPerson.getNameLastFirst(false) +
                                    " is an existing person record in the database. Use existing record?") ? otherPerson : null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getFirstAuthorSingleName()
  {
    List<Author> authList = getAuthorsFromUI();
    return collEmpty(authList) ? "" : authList.get(0).singleName();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getCurPageNum(HDT_Work work, HDT_WorkFile workFile, boolean isStart)
  {
    if ((curWork == null) || (curWork != work) || curWork.workFiles.isEmpty()) return -1;

    if (workFile == null)
      workFile = curWork.workFiles.get(0);

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

  @Override public void updateFromRecord()
  {
    btnTree.setDisable(ui.tree().getRowsForRecord(curWork).isEmpty());

    WorkTypeEnum workTypeEnumVal = curWork.getWorkTypeEnum();

    if (workTypeEnumVal == wtUnenteredSet)
      changeToUnenteredSetMode();
    else
    {
      changeToNormalMode();
      tfYear.setText(curWork.getYear());
    }

    alreadyChangingTitle.setTrue();
    tfTitle.setText(curWork.name());
    alreadyChangingTitle.setFalse();

    if (curWork.getBibEntryKey().isBlank() == false)
    {
      tabEntry.setText(db.getBibLibrary().type().getUserFriendlyName() + " entry");
      tpBib.getTabs().add(0, tabEntry);
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
      hcbType.addAndSelectEntry(curWork.workType, HDT_Record::name);
      getTab().setGraphic(imgViewForRecord(curWork));
    }

  // Populate authors and investigations
  // -----------------------------------

    htAuthors.buildRows(curWork.getAuthors(), (row, author) ->
    {
      HDT_Person authorRecord = author.getPerson();

      if (authorRecord == null)
      {
        Populator pop = htAuthors.getPopulator(1);
        pop.populate(null, false);
        pop.addEntry(null, author.getNameLastFirst());
        row.setCellValue(1, author.getNameLastFirst(), hdtPerson);
      }
      else
        row.setCellValue(1, authorRecord, authorRecord.listName());

      row.setCheckboxValue(2, author.getIsEditor());
      row.setCheckboxValue(3, author.getIsTrans());
    });

  // Populate Labels
  // ------------------

    htLabels.buildRows(curWork.labelStream(), (row, label) -> row.setCellValue(2, label, label.extendedText()));

  // Populate works
  // ----------------------

    hcbLargerWork.addAndSelectEntry(curWork.largerWork, HDT_Record::getCBText);

    htSubworks.buildRows(curWork.subWorks, (row, subWork) ->
    {
      if (subWork.authorRecords.size() > 0)
        row.setCellValue(0, subWork.authorRecords.get(0), subWork.getLongAuthorsStr(true));
      else
        row.setCellValue(0, subWork, subWork.getLongAuthorsStr(true));

      row.setCellValue(1, subWork, subWork.name());
      row.setCellValue(2, subWork, subWork.getYear(), CellSortMethod.smNumeric);
    });

  // Populate arguments
  // ------------------

    htArguments.buildRows(curWork.arguments, (row, arg) ->
    {
      if (arg.positions.size() > 0)
      {
        HDT_Position position = arg.positions.get(0);
        row.setCellValue(0, position, "");
        row.setCellValue(1, position, position.listName());

        nullSwitch(arg.getPosVerdict(position), verdict -> row.setCellValue(2, verdict, verdict.listName()));
      }
      else if (arg.counteredArgs.size() > 0)
      {
        HDT_Argument countered = arg.counteredArgs.get(0);
        row.setCellValue(0, countered, "");
        row.setCellValue(1, countered, countered.listName());

        nullSwitch(arg.getArgVerdict(countered), verdict -> row.setCellValue(2, verdict, verdict.listName()));
      }

      row.setCellValue(3, arg, arg.listName());
    });

  // Populate work files
  // -------------------

    refreshFiles();

  // Populate miscellaneous files
  // ----------------------------

    htMiscFiles.buildRows(curWork.miscFiles, (row, miscFile) -> row.setCellValue(1, miscFile, miscFile.name()));

  // Populate key mentioners
  // -----------------------

    populateDisplayersAndKeyMentioners(curWork, htKeyMentioners);

  // Other stuff
  // -----------

    int subworkCnt  = curWork.subWorks .size(),
        miscFileCnt = curWork.miscFiles.size(),
        workFileCnt = curWork.workFiles.size();

    setTabCaption(tabWorkFiles     , workFileCnt);
    setTabCaption(tabSubworks      , subworkCnt);
    setTabCaption(tabMiscFiles     , miscFileCnt);

    tfSearchKey.setText(curWork.getSearchKey());
    if (tfSearchKey.getText().isEmpty())
      if (curWork.getYear().length() > 0)
        if (curWork.authorRecords.size() > 0)
          if (curWork.authorRecords.get(0).getLastName().length() > 0)
            tfSearchKey.setText(makeWorkSearchKey(curWork.getAuthors(), curWork.getYear(), curWork));

    FilePath filePath = curWork.filePathIncludeExt();
    boolean updatePreview = true;

    if (curWork == lastWork)
    {
      List<TableColumn<HyperTableRow, ?>> list = new ArrayList<>(htWorkFiles.getTV().getSortOrder());

      htWorkFiles.getTV().getSortOrder().setAll(list);

      updatePreview = FilePath.isEmpty(filePath) || (filePath.equals(previewWindow.getFilePath(pvsWorkTab)) == false);
    }
    else
    {
      bibManagerDlg.workRecordToAssign.set(null);

      if (subworkCnt > 0)
        tabPane.getSelectionModel().select(tabSubworks);
      else if (workFileCnt > 1)
        tabPane.getSelectionModel().select(tabWorkFiles);
      else if (miscFileCnt > 0)
        tabPane.getSelectionModel().select(tabMiscFiles);
      else if (tabPane.getSelectionModel().getSelectedItem() != tabBibDetails)
        tabPane.getSelectionModel().select(tabWorkFiles);

      if (FilePath.isEmpty(filePath) == false)
        if (filePath.equals(previewWindow.getFilePath(pvsWorkTab)))
          if (curWork.getStartPageNum() < 2)
            updatePreview = false;
    }

    if (curWork.getBibEntryKey().length() > 0)
    {
      ImageView iv = imgViewFromRelPath("resources/images/card-catalog.png");
      iv.setFitWidth(16);
      iv.setFitHeight(16);
      btnBibManager.setGraphic(iv);
      setToolTip(btnBibManager, "Go to " + db.getBibLibrary().type().getUserFriendlyName() + " entry for this work");
    }

    if (updatePreview)
      previewWindow.setPreview(pvsWorkTab, filePath, curWork.getStartPageNum(), curWork.getEndPageNum(), curWork);
    else
      previewWindow.refreshControls(pvsWorkTab);

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

    for (HDT_WorkFile file : curWork.workFiles)
    {
      if (file.parentFolder() != null)
      {
        if (folder == null)
          folder = file.parentFolder();
        else if (folder != file.parentFolder())
          notInSame = true;
      }

      HyperTableRow row = htWorkFiles.newDataRow();
      row.setCheckboxValue(1, file.getAnnotated());
      row.setCellValue(2, file, file.getPath().getNameStr());

      int pageNum = curWork.getStartPageNum(file);
      if (pageNum > -1)
        row.setCellValue(3, file, String.valueOf(pageNum));

      pageNum = curWork.getEndPageNum(file);
      if (pageNum > -1)
        row.setCellValue(4, file, String.valueOf(pageNum));

      row.setCellValue(5, file, file.name());

      file.viewNow();
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

  static void populateDisplayersAndKeyMentioners(HDT_RecordWithPath record, HyperTable table)
  {
    Set<HDT_RecordWithMainText> set = new LinkedHashSet<>(), invSet = new LinkedHashSet<>();

    Stream<HDT_RecordWithMainText> stream = db.keyWorkMentionerStream(record).filter(recordWMT -> recordWMT.getType() != hdtWorkLabel);

    if (record.hasMainText())
      stream = Stream.concat(stream, db.displayerStream((HDT_RecordWithMainText)record));

    stream.forEachOrdered(mentioner -> (mentioner.getType() == hdtInvestigation ? invSet : set).add(mentioner));

    table.buildRows(Stream.concat(invSet.stream(), set.stream()), (row, mentioner) ->
    {
      row.setCellValue(0, mentioner, "");
      row.setCellValue(1, mentioner, mentioner.getCBText());
      row.setCellValue(2, mentioner, mentioner.getMainText().getPlainForDisplay());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void changeToNormalMode()
  {
    if (inNormalMode) return;

    btnTopAutofill.setVisible(true);

    tfYear.setDisable(false);
    btnNewChapter.setText("New Chapter");
    cbLargerWork.setDisable(false);
    GridPane.setColumnSpan(apLowerMid, 1);
    btnLargerWork.setText("Larger Work:");

    btnFolder.setVisible(false);

    setAllVisible(true, cbLargerWork, apLowerRight, btnURL);

    tfURL.setEditable(true);
    AnchorPane.setLeftAnchor(btnURL, btnURLLeftAnchor);
    AnchorPane.setLeftAnchor(tfURL, tfURLLeftAnchor);
    AnchorPane.setRightAnchor(tfURL, tfURLRightAnchor);

    apLowerMid.getChildren().remove(tfURL);
    apLowerRight.getChildren().add(tfURL);

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

    btnURLLeftAnchor = AnchorPane.getLeftAnchor(btnURL);
    tfURLLeftAnchor = AnchorPane.getLeftAnchor(tfURL);
    tfURLRightAnchor = AnchorPane.getRightAnchor(tfURL);

    tfYear.setDisable(true);
    cbLargerWork.setDisable(true);

    btnNewChapter.setText("Add Multiple Files");
    GridPane.setColumnSpan(apLowerMid, GridPane.REMAINING);
    btnLargerWork.setText("Move All Files");

    tfURL.setEditable(false);

    setAllVisible(false, cbLargerWork, apLowerRight, btnURL);

    apLowerRight.getChildren().remove(tfURL);
    apLowerMid.getChildren().add(tfURL);

    btnFolder.setVisible(true);

    AnchorPane.setLeftAnchor(tfURL, btnFolder.getBoundsInParent().getMaxX() + 2.0);
    AnchorPane.setRightAnchor(tfURL, 2.0);

    inNormalMode = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initArgContextMenu()
  {
    htArguments.addContextMenuItem("Argument Record...", HDT_Argument.class,
      arg -> ui.goToRecord(arg, true));

    htArguments.addContextMenuItem("Position Record...", HDT_Argument.class,
      arg -> arg.positions.size() > 0,
      arg -> ui.goToRecord(arg.positions.get(0), true));

    htArguments.addContextMenuItem("Debate Record...", HDT_Argument.class,
      arg -> (arg.positions.isEmpty() == false) && (arg.positions.get(0).getLargerDebate() != null),
      arg -> ui.goToRecord(arg.positions.get(0).getLargerDebate(), true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void moveUnenteredWorkFile(HDT_WorkFile workFile)
  {
    int startPage = getCurPageNum(curWork, workFile, true ),
        endPage   = getCurPageNum(curWork, workFile, false);

    HDT_Work.sourceUnenteredWork = curWork;

    if (ui.importWorkFile(curWork.authorRecords.isEmpty() ? null : curWork.authorRecords.get(0), workFile.filePath(), true))
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
      messageDialog("There are no files to move.", mtWarning);
      return;
    }

    FilePathSet files = new FilePathSet();

    curWork.workFiles.forEach(workFile -> files.add(workFile.filePath()));

    MutableBoolean allSame = new MutableBoolean();
    FilePath folder = pickDirectory(true, files, allSame);

    if (folder == null) return;

    if (allSame.isTrue())
    {
      messageDialog("All of the files are already located in the destination folder.", mtWarning);
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
      messageDialog("An error occurred while moving the files: " + e.getMessage(), mtError);
    }

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();

    ui.update();
    fileManagerDlg.setNeedRefresh();
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
    newWork.setYear(curWork.getYear());

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

    List<File> files = ui.windows.showOpenMultipleDialog(fileChooser, ui.getStage());
    if (collEmpty(files)) return;

    FilePathSet filePaths = files.stream().map(FilePath::new).collect(Collectors.toCollection(FilePathSet::new));

    for (FilePath filePath : filePaths)
    {
      if (filePath.isDirectory())
      {
        messageDialog("One of the selected files is a directory.", mtError);
        return;
      }

      HDT_RecordWithPath fileRecord = HyperPath.getRecordFromFilePath(filePath);

      if (fileRecord != null)
      {
        messageDialog(HyperPath.alreadyInUseMessage(filePath, fileRecord), mtError);
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

        .addButton("Move", mrMove)
        .addButton("Copy", mrCopy)

        .showModal();
    }

    try
    {
      for (FilePath srcFilePath : filePaths)
      {
        if ((moveOrCopy == mrMove) && (srcFilePath.canObtainLock() == false))
        {
          messageDialog("Unable to obtain lock on path: \"" + srcFilePath + '"', mtError);
          return;
        }

        FilePath destFilePath = folder.getDirOnly().resolve(srcFilePath.getNameOnly());

        if (destFilePath.canObtainLock() == false)
        {
          messageDialog("Unable to obtain lock on path: \"" + destFilePath + '"', mtError);
          return;
        }
      }
    }
    catch (IOException e)
    {
      messageDialog("An error occurred: " + e.getMessage(), mtError);
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
          messageDialog("Unable to " + (moveOrCopy == mrCopy ? "copy" : "move") + " the file: \"" + srcFilePath.getNameOnly() + "\". Reason: " + e.getMessage(), mtError);
          ui.update();
          fileManagerDlg.setNeedRefresh();

          if (startWatcher)
            folderTreeWatcher.createNewWatcherAndStart();

          return;
        }
      }

      HDT_WorkFile workFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, destFilePath);
      if (workFile == null)
      {
        messageDialog("Internal error #67830", mtError);
        ui.update();
        fileManagerDlg.setNeedRefresh();

        if (startWatcher)
          folderTreeWatcher.createNewWatcherAndStart();

        return;
      }

      curWork.addWorkFile(workFile.getID());
    }

    ui.update();
    fileManagerDlg.setNeedRefresh();

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

      folder = ui.windows.showDirDialog(dirChooser, ui.getStage());

      if (FilePath.isEmpty(folder)) return null;

      folderRecord = HyperPath.getFolderFromFilePath(folder, true);

      if (folderRecord == null)
        messageDialog("You must choose a subfolder of the main database folder.", mtError);
    }

    allSame.setTrue();

    for (FilePath file : files)
    {
      FilePath path = folder.getDirOnly().resolve(file.getNameOnly());

      if (path.exists())
      {
        if (file.equals(path) == false)
        {
          messageDialog("A file with the name \"" + file.getNameOnly() + "\" already exists in the destination folder.", mtError);
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

  @Override public void clear()
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

    pdfBDprop     .set(null);
    crossrefBDprop.set(null);
    googleBDprop  .set(null);

    tpBib.getTabs().removeAll(tabEntry, tabCrossref, tabPdfMetadata, tabGoogleBooks);

    stopRetrieving();

    alreadyChangingTitle.setTrue();
    tfTitle.setText("");
    alreadyChangingTitle.setFalse();

    tfDOI      .setText("");
    tfYear     .setText("");
    tfSearchKey.setText("");
    tfURL      .setText("");

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

    getTab().setGraphic(imgViewForRecordType(hdtWork));

    if (db.bibLibraryIsLinked())
    {
      ImageView iv = imgViewFromRelPath("resources/images/card-catalog_tr.png");
      iv.setFitWidth(16);
      iv.setFitHeight(16);

      btnBibManager.setVisible(true);
      btnBibManager.setGraphic(iv);
      setToolTip(btnBibManager, "Assign to " + db.getBibLibrary().type().getUserFriendlyName() + " entry");
    }
    else
      btnBibManager.setVisible(false);

    if ((curWork != lastWork) || (curWork == null))
    {
      htWorkFiles.getTV().getSortOrder().clear();

      if (curWork == null)
      {
        previewWindow.clearPreview(pvsWorkTab);
        bibManagerDlg.workRecordToAssign.set(null);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String makeWorkSearchKey(Iterable<Author> authors, String year, HDT_Work work)
  {
    for (Author author : authors)
    {
      if ((author.getIsEditor() == false) && (author.getIsTrans() == false) && (author.getPerson() != null))
      {
        String key = makeWorkSearchKey(author, year, work);
        if (key.length() > 0) return key;
      }
    }

    for (Author author : authors)
    {
      if ((author.getIsTrans() == false) && (author.getPerson() != null))
      {
        String key = makeWorkSearchKey(author, year, work);
        if (key.length() > 0) return key;
      }
    }

    for (Author author : authors)
    {
      if ((author.getIsEditor() == false) && (author.getIsTrans() == false) && (author.getPerson() == null))
      {
        String key = makeWorkSearchKey(author, year, work);
        if (key.length() > 0) return key;
      }
    }

    for (Author author : authors)
    {
      if ((author.getIsTrans() == false) && (author.getPerson() == null))
      {
        String key = makeWorkSearchKey(author, year, work);
        if (key.length() > 0) return key;
      }
    }

    for (Author author : authors)
    {
      if (author.getPerson() != null)
      {
        String key = makeWorkSearchKey(author, year, work);
        if (key.length() > 0) return key;
      }
    }

    for (Author author : authors)
    {
      if (author.getPerson() == null)
      {
        String key = makeWorkSearchKey(author, year, work);
        if (key.length() > 0) return key;
      }
    }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeWorkSearchKey(Author author, String year, HDT_Work work)
  {
    return makeWorkSearchKey(author.getName().getSingle(), year, work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeWorkSearchKey(String name, String year, HDT_Work work)
  {
    return name.isEmpty() || year.isEmpty() ? "" : makeWorkSearchKey(name + ' ' + year, work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeWorkSearchKey(String searchKey, HDT_Work work)
  {
    char keyLetter = ' ';
    boolean keyTaken;

    do
    {
      SearchKeyword hyperKey = db.getKeyByKeyword((searchKey + keyLetter).trim());
      keyTaken = false;

      if ((hyperKey != null) && (hyperKey.record != work))
      {
        keyTaken = true;

        if (keyLetter == 'z') return "";

        keyLetter = keyLetter == ' ' ? 'a' : (char)(keyLetter + 1);
      }

    } while (keyTaken);

    return SearchKeys.prepSearchKey(searchKey + keyLetter);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord()
  {
    WorkTypeEnum workTypeEnumVal = HDT_WorkType.workTypeIDToEnumVal(hcbType.selectedID());

    if (tfSearchKey.getText().isEmpty())
      lblSearchKeyClick();
    else if (curWork.getYear().length() > 0)
    {
      if (tfSearchKey.getText().contains(curWork.getYear()))
        if (workTypeEnumVal != wtUnenteredSet)
          if (tfYear.getText().equals(curWork.getYear()) == false)
            if (confirmDialog("Year has been modified. Update search key?"))
              tfSearchKey.setText(tfSearchKey.getText().endsWith(curWork.getYear()) ?
                makeWorkSearchKey(tfSearchKey.getText().replace(curWork.getYear(), tfYear.getText()), curWork)
              :
                makeWorkSearchKey(getFirstAuthorSingleName(), tfYear.getText(), curWork));
    }

    if (saveSearchKey(curWork, tfSearchKey) == false) return false;

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
      curWork.setYear("");
      curWork.setLargerWork(-1, true);
      curWork.setURL("");
    }
    else
    {
      curWork.setYear(tfYear.getText());
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

  public boolean showWorkDialog(HDT_WorkFile workFile) { return showWorkDialog(workFile, null, null, Ternary.Unset, EntryType.etUnentered); }

  public boolean showWorkDialog(HDT_WorkFile workFile, FilePath filePathToUse, BibData bdToUse, Ternary newEntryChoice, EntryType newEntryType)
  {
    if (ui.cantSaveRecord()) return false;

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

    if (curTab == tabPdfMetadata) return pdfBDprop     .get();
    if (curTab == tabCrossref   ) return crossrefBDprop.get();
    if (curTab == tabGoogleBooks) return googleBDprop  .get();

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void mnuShowMetadataClick()
  {
    stopRetrieving();

    taPdfMetadata.clear();
    pdfBDprop.set(null);

    if (tpBib.getTabs().contains(tabPdfMetadata) == false)
      tpBib.getTabs().add(tabPdfMetadata);

    tpBib.getSelectionModel().select(tabPdfMetadata);

    List<FilePath> pdfFilePaths = curWork.workFiles.stream().filter(HDT_WorkFile::pathNotEmpty)
                                                            .map(HDT_WorkFile::filePath)
                                                            .collect(Collectors.toList());
    try
    {
      pdfBDprop.set(PDFBibData.createFromFiles(pdfFilePaths));

      if (pdfBDprop.get() == null)
        pdfBDprop.set(PDFBibData.createFromFiles(safeListOf(resolveExtFilePath(tfURL.getText()))));

      if (pdfBDprop.get() == null)
        taPdfMetadata.setText("[No PDF file.]");
      else
        taPdfMetadata.appendText(pdfBDprop.get().createReport());
    }
    catch (IOException e)
    {
      taPdfMetadata.setText("[Error: " + e.getMessage() + ']');
    }

    updateBibButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void retrieveBibData(boolean crossref, String doi, List<String> isbns)
  {
    stopRetrieving();

    if ((db.isLoaded() == false) || (curWork == null)) return;

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

      if (crossref) crossrefBDprop.set((CrossrefBibData) queryBD);
      else          googleBDprop  .set((GoogleBibData  ) queryBD);

      ta.setText("Query URL: " + httpClient.lastUrl() + System.lineSeparator());

      if (queryBD == null)
      {
        ta.appendText("[No results.]");
        return;
      }

      ta.appendText(queryBD.createReport());
    };

    GUIBibData bd = new GUIBibData();
    bd.getAuthors().setAllFromTable(getAuthorGroups());

    if (crossref)
    {
      if (safeStr(doi).isBlank())
      {
        bd.setTitle(tfTitle.getText());
        bd.setStr(bfYear, tfYear.getText());
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
        bd.setStr(bfYear, tfYear.getText());
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

    if ((db.isLoaded() == false) || (curWork == null) || ui.cantSaveRecord()) return;

    List<FilePath> pdfFilePaths = curWork.workFiles.stream().filter(HDT_WorkFile::pathNotEmpty)
                                                            .map(HDT_WorkFile::filePath)
                                                            .collect(Collectors.toList());
    if (collEmpty(pdfFilePaths))
      pdfFilePaths = safeListOf(resolveExtFilePath(tfURL.getText()));

    BibData workBD = curWork.getBibData();

    tabPane.getSelectionModel().select(tabBibDetails);
    setAllVisible(true, btnStop, progressBar);

    bibDataRetriever = new BibDataRetriever(httpClient, workBD, pdfFilePaths, (pdfBD, queryBD, messageShown) ->
    {
      setAllVisible(false, btnStop, progressBar);
      if ((pdfBD == null) && (queryBD == null))
      {
        if (messageShown == false)
          messageDialog("Unable to find bibliographic information.", mtInformation);

        return;
      }

      MergeWorksDlgCtrlr mwd;

      try
      {
        mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", workBD, pdfBD, queryBD, null, curWork, false, true, Ternary.Unset);
      }
      catch (IOException e)
      {
        messageDialog("Unable to initialize merge dialog window.", mtError);
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
      bibManagerDlg.refresh();
      ui.update();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnMergeBibClick()
  {
    if (ui.cantSaveRecord()) return;

    MergeWorksDlgCtrlr mwd;
    BibData workBibData = curWork.getBibData();

    try
    {
      mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", workBibData, pdfBDprop.get(), crossrefBDprop.get(), googleBDprop.get(), curWork, false, true, Ternary.Unset);
    }
    catch (IOException e)
    {
      messageDialog("Unable to initialize merge dialog window.", mtError);
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
    bibManagerDlg.refresh();
    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spVert, PREF_KEY_WORK_MID_VERT, 0);
    setDividerPosition(spVert, PREF_KEY_WORK_BOTTOM_VERT, 1);
    setDividerPosition(spHoriz1, PREF_KEY_WORK_RIGHT_HORIZ, 0);
    setDividerPosition(spMentioners, PREF_KEY_WORK_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spVert, PREF_KEY_WORK_MID_VERT, 0);
    getDividerPosition(spVert, PREF_KEY_WORK_BOTTOM_VERT, 1);
    getDividerPosition(spHoriz1, PREF_KEY_WORK_RIGHT_HORIZ, 0);
    getDividerPosition(spMentioners, PREF_KEY_WORK_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getBibDataFromGUI(GUIBibData bd)
  {
    List<String> isbns = htISBN.dataRowStream().map(row -> row.getText(0)).collect(Collectors.toList());

    bd.setMultiStr(bfISBNs, isbns);

    bd.setTitle(tfTitle.getText());
    bd.setStr(bfYear, tfYear.getText());
    bd.setStr(bfURL, tfURL.getText());
    bd.setStr(bfDOI, tfDOI.getText());
    bd.setWorkType(hcbType.selectedRecord());

    bd.setMultiStr(bfMisc, convertMultiLineStrToStrList(taMiscBib.getText(), true));

    bd.getAuthors().setAllFromTable(getAuthorGroups());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void updateWebButtons(Preferences node)
  {
    updateWebButtons(node, PREF_KEY_WORK_SRCH, 2, btnWebSrch1, smbWebSrch1, TOOLTIP_PREFIX, this::searchBtnEvent);

    btnWebSrch2.setText(ui.webButtonMap.get(PREF_KEY_WORK_SRCH + '2').getCaption());
    mnuGoogle  .setText("Search this DOI using " + ui.webButtonMap.get(PREF_KEY_DOI_SRCH).getCaption());

    isbnSrchMenuItemSchema.setCaption(ui.webButtonMap.get(PREF_KEY_ISBN_SRCH).getCaption());

    setToolTip(btnWebSrch2, TOOLTIP_PREFIX + btnWebSrch2.getText());
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

    FilePath filePath = new FilePath(files.get(0));
    if (db.getRootPath().isSubpath(filePath))
      return false;

    FilePath extPath = extPath();
    if (FilePath.isEmpty(extPath))
    {
      messageDialog(NO_EXT_PATH_MESSAGE, mtError);
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
