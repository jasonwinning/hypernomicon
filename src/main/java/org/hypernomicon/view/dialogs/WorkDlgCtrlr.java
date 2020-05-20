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

package org.hypernomicon.view.dialogs;

import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;

import org.apache.commons.io.FilenameUtils;
import org.controlsfx.control.MasterDetailPane;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibDataRetriever;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.CrossrefBibData;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.data.GUIBibData;
import org.hypernomicon.bib.data.GoogleBibData;
import org.hypernomicon.bib.data.PDFBibData;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.model.records.HDT_WorkFile.FileNameAuthor;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.ObjectGroup;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.previewWindow.PDFJSWrapper;
import org.hypernomicon.view.previewWindow.PreviewWrapper;
import org.hypernomicon.view.tabs.WorkTabCtrlr;
import org.hypernomicon.view.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTable.CellUpdateHandler;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import javafx.application.Platform;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.fxml.FXML;
import javafx.geometry.Side;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.RadioButton;
import javafx.scene.control.SplitMenuButton;
import javafx.scene.control.TableView;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.AnchorPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Screen;
import javafx.stage.Stage;

//---------------------------------------------------------------------------

public class WorkDlgCtrlr extends HyperDlg
{
  @FXML private AnchorPane apMain;
  @FXML private Button btnRegenerateFilename, btnStop, btnDest, btnPaste;
  @FXML private CheckBox chkSetDefault, chkCreateBibEntry, chkKeepFilenameUnchanged;
  @FXML private ComboBox<EntryType> cbEntryType;
  @FXML private ComboBox<HyperTableCell> cbType;
  @FXML private Hyperlink hlCase;
  @FXML private Label lblAutoPopulated;
  @FXML private MenuItem mnuPopulateUsingDOI, mnuPopulateFromPDF;
  @FXML private ProgressBar progressBar;
  @FXML private RadioButton rbCopy, rbCurrent, rbMove;
  @FXML private SplitMenuButton btnAutoFill;
  @FXML private TableView<HyperTableRow> tvAuthors, tvISBN;
  @FXML private TextArea taMisc;
  @FXML private TextField tfDest, tfDOI, tfFileTitle, tfNewFile, tfOrigFile, tfTitle, tfYear;
  @FXML private ToggleButton btnPreview;
  @FXML private ToggleGroup tgSelect;
  @FXML public Button btnCancel;

  private AnchorPane apPreview;
  private MasterDetailPane mdp;
  private HyperCB hcbType;
  private HyperTable htAuthors, htISBN;
  private HDT_WorkFile oldWorkFile = null, newWorkFile = null;
  private PDFJSWrapper jsWrapper = null;
  private FilePath previewFilePath = null, origFilePath = null;
  private BibData curBD = null, origBDtoUse = null;
  private HDT_Work curWork;
  private ObjectProperty<HDT_Folder> destFolder = new SimpleObjectProperty<>();
  private BibDataRetriever bibDataRetriever = null;
  private Ternary newEntryChoice;
  private boolean userOverrideDest = false, dontRegenerateFilename = false, alreadyChangingTitle = false, previewInitialized = false;

  private static final AsyncHttpClient httpClient = new AsyncHttpClient();

  public List<ObjectGroup> getAuthorGroups() { return htAuthors.getAuthorGroups(curWork, 0, 2, 3, 4); }
  public boolean getCreateEntry()            { return chkCreateBibEntry.isVisible() && chkCreateBibEntry.isSelected(); }

  @FXML private void btnLaunchClick()        { launchFile(origFilePath); }

//---------------------------------------------------------------------------

  public static WorkDlgCtrlr build(FilePath filePathToUse, BibData bdToUse, Ternary newEntryChoice, EntryType newEntryType)
  {
    return ((WorkDlgCtrlr) create("WorkDlg.fxml", "Import New Work File", true)).init(null, filePathToUse, bdToUse, newEntryChoice, newEntryType);
  }

  public static WorkDlgCtrlr build(HDT_WorkFile workFileToUse)
  {
    return ((WorkDlgCtrlr) create("WorkDlg.fxml", "Work File", true)).init(workFileToUse, null, null, Ternary.Unset, EntryType.etUnentered);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private WorkDlgCtrlr init(HDT_WorkFile workFileToUse, FilePath filePathToUse, BibData bdToUse, Ternary newEntryChoice, EntryType newEntryType)
  {
    apPreview = new AnchorPane();
    mdp = addPreview(stagePane, apMain, apPreview, btnPreview);

    mdp.showDetailNodeProperty().addListener((ob, ov, nv) ->
    {
      if (Boolean.TRUE.equals(nv) == false) return;

      if ((previewInitialized == false) && (jxBrowserDisabled == false))
        accommodatePreview(dialogStage, apMain, mdp);

      updatePreview();
    });

    initControls();

    onShown = () ->
    {
      disableCache(taMisc);

      if (oldWorkFile == null)
      {
        if (filePathToUse == null)
          btnSrcBrowseClick();
        else
          useChosenFile(filePathToUse, bdToUse);
      }

      if (newEntryType != EntryType.etUnentered)
        cbEntryType.getSelectionModel().select(newEntryType);
    };

    curWork = ui.workHyperTab().activeRecord();
    curBD = new GUIBibData(curWork.getBibData());

    this.newEntryChoice = newEntryChoice;
    origBDtoUse = bdToUse == null ? null : new GUIBibData(bdToUse);

    chkCreateBibEntry.setSelected(newEntryChoice.isTrue());

    if ((db.bibLibraryIsLinked() == false) || (curWork.getBibEntryKey().length() > 0))
      setAllVisible(false, chkCreateBibEntry, cbEntryType);

    if (workFileToUse == null)
    {
      switch (db.prefs.get(PREF_KEY_IMPORT_ACTION_DEFAULT, PREF_KEY_IMPORT_ACTION_MOVE))
      {
        case PREF_KEY_IMPORT_ACTION_MOVE : rbMove   .setSelected(true); break;
        case PREF_KEY_IMPORT_ACTION_COPY : rbCopy   .setSelected(true); break;
        case PREF_KEY_IMPORT_ACTION_NONE : rbCurrent.setSelected(true); break;
      }
    }
    else
    {
      oldWorkFile = workFileToUse;
      origFilePath = oldWorkFile.filePath();
      newWorkFile = oldWorkFile;

      rbCurrent.setSelected(true);
      rbCopy.setDisable(true);
    }

    ui.workHyperTab().getBibDataFromGUI(curBD);
    populateFieldsFromBibData(curBD, false);

    htAuthors.clear();
    htAuthors.getPopulator(0).populate(null, false);

    boolean atLeastOneInFilename = false;

    for (HyperTableRow origRow : ui.workHyperTab().htAuthors.getDataRows())
    {
      int authID = origRow.getID(1);
      String authName = origRow.getText(1);
      Ternary isInFileName = Ternary.Unset;

      if (authID > 0)
        isInFileName = curWork.personIsInFileName(origRow.getRecord());
      else
      {
        htAuthors.getPopulator(0).addEntry(null, -1, authName);
        Author auth = curWork.getAuthors().getAuthor(new PersonName(authName));
        if (auth != null)
          isInFileName = auth.getInFileName();
      }

      HyperTableRow newRow = htAuthors.newDataRow();
      newRow.setCellValue(0, authID, authName, hdtPerson);
      newRow.setCheckboxValue(1, authID > 0);

      boolean boolVal;

      switch (isInFileName)
      {
        case True  : boolVal = true; break;
        case False : boolVal = false; break;
        default    : boolVal = !atLeastOneInFilename; atLeastOneInFilename = true; break;
      }

      newRow.setCheckboxValue(2, boolVal);
      newRow.setCheckboxValue(3, origRow.getCheckboxValue(2));
      newRow.setCheckboxValue(4, origRow.getCheckboxValue(3));
    }

    if (oldWorkFile != null)
    {
      updatePreview();
      tfOrigFile.setText(origFilePath.toString());
    }

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initControls()
  {
    lblAutoPopulated.setText("");
    tfOrigFile.setEditable(false);

    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtWorkType));
    hcbType.getPopulator().setFilter(id -> HDT_WorkType.workTypeIDToEnumVal(id) != WorkTypeEnum.wtUnenteredSet);

    destFolder.addListener((obs, ov, nv) -> tfDest.setText(nv == null ? "" : (nv.pathNotEmpty() ? db.getRootPath().relativize(nv.filePath()).toString() : "")));

    if (db.bibLibraryIsLinked())
      bibManagerDlg.initCB(cbEntryType);

    tfNewFile.disableProperty().bind(chkKeepFilenameUnchanged.selectedProperty());

    btnPaste.setOnAction(event ->
    {
      FilePath filePath = new FilePath(getClipboardText(true));
      if (filePath.exists())
        useChosenFile(filePath, null);
    });

    setToolTip(btnPaste, "Paste file path from clipboard");

    mnuPopulateUsingDOI.setOnAction(event ->
    {
      String doi = matchDOI(tfDOI.getText());
      if (doi.length() > 0)
        industryIdClick(true, doi, null);
    });

    btnAutoFill       .setOnAction(event -> extractDataFromPdf(true , true, false));
    mnuPopulateFromPDF.setOnAction(event -> extractDataFromPdf(false, true, false));

    setToolTip(btnAutoFill,   MainCtrlr.AUTOFILL_TOOLTIP);

    htISBN = new HyperTable(tvISBN, 0, true, "");

    htISBN.addTextEditCol(hdtWork, true, false);

    htISBN.addContextMenuItem("Use this ISBN to fill in fields",
      row -> row.getText(0).length() > 0,
      row ->
      {
        List<String> list = matchISBN(row.getText(0));
        if (collEmpty(list) == false)
          industryIdClick(false, null, list.get(0));
      });

    htAuthors = new HyperTable(tvAuthors, 0, true, PREF_KEY_HT_WORK_DLG);

    htAuthors.addColWithUpdateHandler(hdtPerson, ctDropDownList, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      dontRegenerateFilename = true;

      if (HyperTableCell.getCellID(cellVal) > 0)
        row.setCheckboxValue(1, true);

      if (htAuthors.getDataRowCount() == 1)
        row.setCheckboxValue(2, true);
      else if (HyperTableCell.getCellID(cellVal) > 0)
      {
        boolean useInFilename = true, keepGoing = true;
        Iterator<HyperTableRow> it = htAuthors.getDataRows().iterator();

        while (it.hasNext() && keepGoing && useInFilename)
        {
          HyperTableRow loopRow = it.next();
          if (loopRow == row)
            keepGoing = false;
          else if (loopRow.getID(0) > 0)
            useInFilename = false;
        }

        if (useInFilename)
        {
          htAuthors.getDataRows().forEach(loopRow -> loopRow.setCheckboxValue(2, false));
          row.setCheckboxValue(2, true);
        }
      }

      dontRegenerateFilename = false;

      btnRegenerateFilenameClick();
    });

    htAuthors.addCheckboxColWithUpdateHandler(createAuthorRecordHandler(htAuthors, () -> curWork));

    CellUpdateHandler handler = (row, cellVal, nextColNdx, nextPopulator) -> btnRegenerateFilenameClick();

    htAuthors.addCheckboxColWithUpdateHandler(handler);
    htAuthors.addCheckboxColWithUpdateHandler(handler);
    htAuthors.addCheckboxColWithUpdateHandler(handler);

    Runnable removeHandler = () ->
    {
      if (htAuthors.getDataRowCount() == 0) return;

      HyperTableRow firstRecordRow = null;

      for (HyperTableRow row : htAuthors.getDataRows())
      {
        if (row.getCheckboxValue(2))
          return;

        if ((firstRecordRow == null) && (row.getID(0) > 0))
          firstRecordRow = row;
      }

      if (firstRecordRow == null)
        htAuthors.dataRowStream().findFirst().orElseThrow().setCheckboxValue(2, true);
      else
        firstRecordRow.setCheckboxValue(2, true);
    };

    htAuthors.addRemoveMenuItem(removeHandler);

    htAuthors.addContextMenuItem("Remove this row", row -> (row.getText(0).length() > 0) && (row.getID(0) < 1), row ->
    {
      htAuthors.removeRow(row);
      removeHandler.run();
    });


    htAuthors.addChangeOrderMenuItem(true);

    tfTitle.textProperty().addListener((ob, oldValue, newValue) ->
    {
      int pos;
      String fileTitle = newValue;

      fileTitle = fileTitle.replace('?', ':')
                           .replace('/', '-');

      pos = fileTitle.indexOf(':');
      if (pos >= 0) fileTitle = fileTitle.substring(0, pos);

      fileTitle = FilePath.removeInvalidFileNameChars(fileTitle);

      tfFileTitle.setText(fileTitle.trim());
    });

    tfFileTitle.textProperty().addListener((ob, oldValue, newValue) -> btnRegenerateFilenameClick());

    cbType.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      WorkTypeEnum workTypeEnumVal = HDT_WorkType.workTypeIDToEnumVal(HyperTableCell.getCellID(newValue)),
                   oldEnumVal = curWork.getWorkTypeEnum();

      if ((oldEnumVal == wtUnenteredSet) && (workTypeEnumVal != wtUnenteredSet))
      {
        messageDialog("You cannot change the work type after it has been set to Unentered Set of Work Files.", mtError);
        Platform.runLater(() -> cbType.setValue(oldValue));
        return;
      }

      if (workTypeEnumVal == wtUnenteredSet)
      {
        if ((oldEnumVal != wtUnenteredSet) && (oldEnumVal != wtNone))
        {
          messageDialog("You cannot change a work with an existing work type into an unentered set of work files.", mtError);
          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }

        tfNewFile.disableProperty().unbind();

        disableAll(tfFileTitle, tfNewFile, tfDest, btnDest, chkSetDefault, tfYear, chkKeepFilenameUnchanged, btnRegenerateFilename, rbMove, rbCopy, rbCurrent);
      }
      else if (workTypeEnumVal != wtNone)
      {
        enableAll(tfFileTitle, tfDest, btnDest, chkSetDefault, tfYear, chkKeepFilenameUnchanged, btnRegenerateFilename, rbMove, rbCopy);

        tfNewFile.disableProperty().bind(chkKeepFilenameUnchanged.selectedProperty());

        updateDest();
      }
    });

    tfYear.textProperty().addListener((ob, oldValue, newValue) -> btnRegenerateFilenameClick());

    tfOrigFile.textProperty().addListener((ob, oldValue, newValue) -> btnRegenerateFilenameClick());

    hlCase.setOnMouseClicked(event ->
    {
      alreadyChangingTitle = true;
      tfTitle.setText(HDT_Work.fixCase(tfTitle.getText()));
      alreadyChangingTitle = false;
    });

    tfTitle.setTextFormatter(new TextFormatter<>(change ->
    {
      if (alreadyChangingTitle) return change;

      if (change.getText().length() > 1)
      {
        alreadyChangingTitle = true;

        String title = convertToSingleLine(change.getControlNewText());
        while (title.contains("  "))
          title = title.replaceAll("  ", " ");

        if (title.equals(title.toUpperCase()) || title.equals(title.toLowerCase()))
          title = HDT_Work.fixCase(title);

        change.setRange(0, change.getControlText().length());
        change.setText(ultraTrim(title));
        alreadyChangingTitle = false;
      }

      return change;
    }));

    btnStop.setOnAction(event -> stopRetrieving());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateDest()
  {
    if (userOverrideDest) return;

    destFolder.set(db.getImportFolderForWorkType(HDT_WorkType.getEnumVal(hcbType.selectedRecord())));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static CellUpdateHandler createAuthorRecordHandler(HyperTable htAuthors, Supplier<HDT_Work> workSource)
  {
    return (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (row.getID(0) > 0)
      {
        Platform.runLater(() -> row.setCheckboxValue(1, true));
        return;
      }
      else if (cellVal.equals(HyperTableCell.falseCheckboxCell))
        return;

      String text = row.getText(0);

      HDT_Person otherPerson = WorkTabCtrlr.otherPersonToUse(text);

      if (otherPerson != null)
      {
        htAuthors.selectID(0, row, otherPerson.getID());
        return;
      }

      Author author = null;
      HDT_Work work = workSource.get();

      if (work != null)
      {
        author = work.getAuthors().getAuthor(new PersonName(text));

        if (author == null)
          author = new Author(work, new PersonName(text), false, false, Ternary.Unset);
      }

      NewPersonDlgCtrlr npdc = NewPersonDlgCtrlr.build(true, text, author);

      if (npdc.showModal())
      {
        htAuthors.getPopulator(0).setChanged(row);              // A new record has been created so force it to repopulate
        htAuthors.selectID(0, row, npdc.getPerson().getID());
      }
      else
        Platform.runLater(() -> row.setCheckboxValue(1, false));
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MasterDetailPane addPreview(AnchorPane stagePane, AnchorPane apMain, AnchorPane apPreview, ToggleButton btnPreview)
  {
    stagePane.getChildren().remove(apMain);

    MasterDetailPane mdp = new MasterDetailPane(Side.RIGHT, apMain, apPreview, false);
    setAnchors(mdp, 0.0, 0.0, 0.0, 0.0);
    stagePane.getChildren().add(mdp);

    btnPreview.selectedProperty().bindBidirectional(mdp.showDetailNodeProperty());

    return mdp;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void accommodatePreview(Stage dialogStage, AnchorPane apMain, MasterDetailPane mdp)
  {
    List<Screen> screens = Screen.getScreensForRectangle(dialogStage.getX(), dialogStage.getY(), dialogStage.getWidth(), dialogStage.getHeight());
    double minWidth = screens.size() == 1 ? screens.get(0).getVisualBounds().getWidth() - 60.0 : 1600.0;

    if (dialogStage.getWidth() < minWidth)
    {
      double diff = minWidth - dialogStage.getWidth();
      dialogStage.setX(dialogStage.getX() - (diff / 2.0));
      dialogStage.setWidth(minWidth);
      ensureVisible(dialogStage, apMain.getPrefWidth(), apMain.getPrefHeight());
    }

    mdp.setDividerPosition(0.55);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updatePreview()
  {
    if ((mdp.isShowDetailNode() == false) || jxBrowserDisabled) return;

    if (previewInitialized == false) jsWrapper = new PDFJSWrapper(apPreview, null, null, null);

    if (jxBrowserDisabled) return;

    previewInitialized = true;

    if (FilePath.isEmpty(origFilePath) && (FilePath.isEmpty(previewFilePath) == false))
      jsWrapper.close();

    if (FilePath.isEmpty(origFilePath) || origFilePath.equals(previewFilePath))
      return;

    previewFilePath = origFilePath;

    PreviewWrapper.showFile(previewFilePath, 1, jsWrapper);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnRegenerateFilenameClick()
  {
    if (dontRegenerateFilename) return;

    String ext, year, fileName, newFileName = "";

    ext = FilenameUtils.getExtension(tfOrigFile.getText());
    if (ext.isEmpty())
      ext = FilenameUtils.getExtension(tfNewFile.getText());

    year = tfYear.getText();

    List<FileNameAuthor> authors = new ArrayList<>();

    htAuthors.getDataRows().forEach(row ->
    {
      if ((row.getRecord() != null) || (row.getText(0).length() > 0))
        if (row.getCheckboxValue(2))
          authors.add(new FileNameAuthor(row.getText(0), row.getCheckboxValue(3), row.getCheckboxValue(4)));
    });

    fileName = HDT_WorkFile.makeFileName(authors, year, tfFileTitle.getText(), ext);

    if (fileName.isEmpty())
    {
      tfNewFile.setText("");
      return;
    }

    boolean nameTaken = true;

    for (int ctr = 1; nameTaken; ctr++)
    {
      if (ctr >= 1000)
      {
        newFileName = fileName;
        break;
      }

      newFileName = FilenameUtils.getBaseName(fileName) + (ctr == 1 ? "" : "_" + String.valueOf(1000 + (ctr % 1000)).substring(1, 4)) +
                    FilenameUtils.EXTENSION_SEPARATOR_STR + FilenameUtils.getExtension(fileName);
      nameTaken = false;

      for (HDT_WorkFile file : curWork.workFiles)
      {
        if (file != oldWorkFile)
          if (FilenameUtils.equalsNormalizedOnSystem(file.getPath().getNameStr(), newFileName))
            nameTaken = true;
      }
    }

    tfNewFile.setText(newFileName.trim());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnDestBrowseClick()
  {
    DirectoryChooser dirChooser = new DirectoryChooser();

    FilePath folderPath = nullSwitch(destFolder.getValue(), null, HDT_Folder::filePath);

    if (FilePath.isEmpty(folderPath) || (folderPath.exists() == false))
      dirChooser.setInitialDirectory(db.topicalPath().toFile());
    else
      dirChooser.setInitialDirectory(folderPath.toFile());

    dirChooser.setTitle("Select Destination Folder");

    FilePath filePath = ui.windows.showDirDialog(dirChooser, dialogStage);

    if (FilePath.isEmpty(filePath)) return;

    HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath, true);

    if ((folder == null) || (folder.getID() == HyperDB.ROOT_FOLDER_ID))
    {
      messageDialog("You must choose a subfolder of the main database folder.", mtError);
      return;
    }

    userOverrideDest = true;
    destFolder.set(folder);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnSrcBrowseClick()
  {
    FileChooser fileChooser = new FileChooser();

    if (EnumSet.of(wtBook, wtChapter, wtNone, wtPaper).contains(curWork.getWorkTypeEnum()))
      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Adobe PDF file (*.pdf)", "*.pdf"));

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    fileChooser.setInitialDirectory(db.unenteredPath().toFile());

    useChosenFile(ui.windows.showOpenDialog(fileChooser, getStage()), null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void useChosenFile(FilePath chosenFile, BibData bdToUse)
  {
    if (FilePath.isEmpty(chosenFile)) return;

    if (db.isProtectedFile(chosenFile, false))
    {
      messageDialog("That file cannot be assigned to a work record.", mtError);
      return;
    }

    // See if the chosen file is currently assigned to a file record

    HDT_RecordWithPath file = HyperPath.getRecordFromFilePath(chosenFile);

    if (file != null)
    {
      if ((file instanceof HDT_MiscFile) || (file instanceof HDT_Person))
      {
        messageDialog(HyperPath.alreadyInUseMessage(chosenFile, file), mtError);
        return;
      }

      // Set variable to the currently assigned file record of the chosen file

      newWorkFile = (HDT_WorkFile) file;
    }
    else
      newWorkFile = null; // no file record was already assigned to the chosen file

    rbMove.setDisable(false);

    rbCurrent.setDisable(db.getRootPath().isSubpath(chosenFile) == false);

    if ((newWorkFile != null) && (newWorkFile != oldWorkFile))
    {
      rbCurrent.setSelected(true);
      chkKeepFilenameUnchanged.setSelected(true);
    }
    else if (rbCurrent.isSelected())
    {
      if (rbCurrent.isDisabled() || db.unenteredPath().isSubpath(chosenFile))
        rbMove.setSelected(true);
    }

    // check if there will be any change in which file record will be assigned to the work.
    // if not, disable the option to "copy" ("copy" creates a new work file record)

    if ((newWorkFile == null) || (oldWorkFile != newWorkFile))
    {
      rbCopy.setDisable(false);
    }
    else
    {
      rbCopy.setDisable(true);

      if (rbCopy.isSelected())
        rbMove.setSelected(true);
    }

    origFilePath = chosenFile;
    updatePreview();
    tfOrigFile.setText(origFilePath.toString());

    if (bdToUse != null)
      populateFieldsFromWebBD(bdToUse);
    else if (tfTitle.getText().isEmpty() && tfYear.getText().isEmpty())
      extractDataFromPdf(appPrefs.getBoolean(PREF_KEY_AUTO_RETRIEVE_BIB, true), false, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean showModal()
  {
    boolean rv = super.showModal();

    stopRetrieving();

    if (previewInitialized)
      jsWrapper.cleanup();

    return rv;
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

  private void extractDataFromPdf(boolean doWebQuery, boolean doMerge, boolean launchIfNoData)
  {
    stopRetrieving();

    lblAutoPopulated.setText("");

    getBibDataFromGUI();

    if (doWebQuery)
    {
      setAllVisible(true, btnStop, progressBar);

      bibDataRetriever = new BibDataRetriever(httpClient, curBD, safeListOf(origFilePath), (pdfBD, queryBD, messageShown) ->
      {
        setAllVisible(false, btnStop, progressBar);

        if ((pdfBD == null) && (queryBD == null))
        {
          if (launchIfNoData && appPrefs.getBoolean(PREF_KEY_AUTO_OPEN_PDF, true))
            mdp.setShowDetailNode(true);

          return;
        }

        if (doMerge)
          doMerge(pdfBD, queryBD);
        else if ((queryBD instanceof CrossrefBibData) || (queryBD instanceof GoogleBibData))
          populateFieldsFromWebBD(queryBD);
        else
        {
          lblAutoPopulated.setText("Fields auto-populated with information extracted from PDF file");
          populateFieldsFromBibData(pdfBD, true);

          if (launchIfNoData && appPrefs.getBoolean(PREF_KEY_AUTO_OPEN_PDF, true))
            mdp.setShowDetailNode(true);
        }
      });

      return;
    }

    BibData pdfBD = null;

    try
    {
      pdfBD = PDFBibData.createFromFiles(safeListOf(origFilePath));

      if (BibData.isEmpty(pdfBD))
        pdfBD = null;
    }
    catch (IOException e)
    {
      falseWithErrorMessage("An error occurred while extracting metadata from PDF file: " + e.getMessage());
      return;
    }

    if (pdfBD == null)
    {
      if (launchIfNoData && appPrefs.getBoolean(PREF_KEY_AUTO_OPEN_PDF, true))
        mdp.setShowDetailNode(true);

      return;
    }

    tfDOI.setText(pdfBD.getStr(bfDOI));
    htISBN.clear();

    htISBN.buildRows(pdfBD.getMultiStr(bfISBNs), (row, isbnStr) -> row.setCellValue(0, -1, isbnStr, hdtNone));

    if (doMerge)
    {
      doMerge(pdfBD, null);
    }
    else
    {
      lblAutoPopulated.setText("Fields auto-populated with information extracted from PDF file");
      populateFieldsFromBibData(pdfBD, true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateFieldsFromWebBD(BibData queryBD)
  {
    if (queryBD instanceof CrossrefBibData)
    {
      lblAutoPopulated.setText("Fields auto-populated from Crossref, doi: " + queryBD.getStr(bfDOI));
    }
    else if (queryBD instanceof GoogleBibData)
    {
      String isbn = GoogleBibData.class.cast(queryBD).getQueryIsbn();
      if (isbn.isBlank())
        lblAutoPopulated.setText("Fields have been auto-populated from Google Books");
      else
        lblAutoPopulated.setText("Fields auto-populated from Google Books, isbn: " + isbn);
    }

    populateFieldsFromBibData(queryBD, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doMerge(BibData bd1, BibData bd2)
  {
    getBibDataFromGUI();

    try
    {
      MergeWorksDlgCtrlr mwd = MergeWorksDlgCtrlr.build("Select How to Merge Fields", curBD,
        bd1, bd2, null, curWork, false, curWork.getBibEntryKey().isBlank(), Ternary.Unset, origFilePath);

      if (mwd.showModal())
      {
        lblAutoPopulated.setText("");
        mwd.mergeInto(curBD);
        populateFieldsFromBibData(curBD, true);

        if (db.bibLibraryIsLinked() && curWork.getBibEntryKey().isBlank())
        {
          cbEntryType.getSelectionModel().select(mwd.getEntryType());
          newEntryChoice = mwd.creatingNewEntry();
          chkCreateBibEntry.setSelected(newEntryChoice.isTrue());
          origBDtoUse = new GUIBibData(curBD);

          if (hcbType.selectedRecord() == null)
          {
            if (mwd.getEntryType() == EntryType.etBook)
              hcbType.selectID(HDT_WorkType.get(wtBook).getID());
            else if (mwd.getEntryType() == EntryType.etJournalArticle)
              hcbType.selectID(HDT_WorkType.get(wtPaper).getID());
          }
        }
      }
    }
    catch (IOException e)
    {
      messageDialog("Unable to initialize merge dialog window.", mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void industryIdClick(boolean crossref, String doi, String isbn)
  {
    stopRetrieving();

    lblAutoPopulated.setText("");
    setAllVisible(true, btnStop, progressBar);

    Consumer<BibData> doneHndlr = queryBD ->
    {
      setAllVisible(false, btnStop, progressBar);

      if (queryBD == null)
      {
        if (crossref) lblAutoPopulated.setText("Crossref query yielded no results for doi: "      + doi);
        else          lblAutoPopulated.setText("Google Books query yielded no results for isbn: " + isbn);

        return;
      }

      doMerge(queryBD, null);
    };

    BibData bd = new GUIBibData();
    if (crossref)
    {
      bd.setStr(bfDOI, doi);
      bibDataRetriever = BibDataRetriever.forCrossref(httpClient, bd, doneHndlr);
    }
    else
    {
      bd.setMultiStr(bfISBNs, safeListOf(isbn));
      bibDataRetriever = BibDataRetriever.forGoogleBooks(httpClient, bd, doneHndlr);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean addAuthorToTable(PersonName authorName, boolean editor, boolean trans, HDT_Person author, HyperTable htAuthors, boolean hasShowInFileCol)
  {
    if (authorName.isEmpty()) return false;

    HyperTableRow row = htAuthors.newDataRow();

    if (author != null)
    {
      row.setCellValue(0, author, author.listName());
      row.setCheckboxValue(1, true);
    }
    else
    {
      String authorStr = authorName.getLastFirst();

      htAuthors.getPopulator(0).addEntry(null, -1, authorStr);
      row.setCellValue(0, -1, authorStr, hdtPerson);
    }

    int addend = hasShowInFileCol ? 1 : 0;

    if (editor)
      row.setCheckboxValue(2 + addend, true);

    if (trans)
      row.setCheckboxValue(3 + addend, true);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void loadFromBibAuthors(BibAuthors bibAuthors, HyperTable htAuthors, boolean hasShowInFileCol, HDT_Work destWork)
  {
    if (BibAuthors.isEmpty(bibAuthors)) return;

    List<PersonName> nameList = new ArrayList<>();
    List<HDT_Person> personList = new ArrayList<>();
    Map<PersonName, Boolean> nameToEd = new HashMap<>(), nameToTr = new HashMap<>();

    bibAuthors.getListsForWorkMerge(nameList, personList, nameToEd, nameToTr, destWork);

    htAuthors.clear();
    htAuthors.getPopulator(0).populate(null, false);

    for (int ndx = 0; ndx < nameList.size(); ndx++)
    {
      PersonName name = nameList.get(ndx);

      addAuthorToTable(name, nameToEd.get(name), nameToTr.get(name), personList.get(ndx), htAuthors, hasShowInFileCol);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibData getBibDataFromGUI()
  {
    curBD.setStr(bfYear, tfYear.getText());
    curBD.setStr(bfDOI, tfDOI.getText());
    curBD.setTitle(tfTitle.getText());

    curBD.setMultiStr(bfMisc, convertMultiLineStrToStrList(taMisc.getText(), true));

    curBD.setWorkType(hcbType.selectedRecord());

    curBD.setMultiStr(bfISBNs, htISBN.dataRowStream().map(row -> row.getText(0)).collect(Collectors.toList()));

    curBD.getAuthors().setAllFromTable(getAuthorGroups());

    return curBD;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateFieldsFromBibData(BibData bd, boolean populateAuthors)
  {
    if (bd != curBD)
      curBD.copyAllFieldsFrom(bd, populateAuthors, true);

    if (curBD.entryTypeNotEmpty() && db.bibLibraryIsLinked())
    {
      EntryType entryType = curBD.getEntryType();
      if (cbEntryType.getItems().contains(entryType) == false)
      {
        messageDialog("\"" + entryType.getUserFriendlyName() + "\" is not a valid " +
                      db.getBibLibrary().type().getUserFriendlyName() + " entry type.", mtWarning);
        cbEntryType.getSelectionModel().select(null);
      }
      else
        cbEntryType.getSelectionModel().select(entryType);
    }

    tfYear.setText(curBD.getStr(bfYear));

    alreadyChangingTitle = true;
    tfTitle.setText(curBD.getStr(bfTitle));
    alreadyChangingTitle = false;

    taMisc.setText(curBD.getStr(bfMisc));

    hcbType.selectID(nullSwitch(curBD.getWorkType(), -1, HDT_Record::getID));

    tfDOI.setText(curBD.getStr(bfDOI));

    htISBN.clear();

    htISBN.buildRows(curBD.getMultiStr(bfISBNs), (row, isbnStr) -> row.setCellValue(0, -1, isbnStr, hdtNone));

    if (populateAuthors)
      loadFromBibAuthors(curBD.getAuthors(), htAuthors, true, curWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Ternary promptToCreateBibEntry(BibData bd, CheckBox chkCreateBibEntry, Ternary choice, BibData origBD)
  {
    if (chkCreateBibEntry.isSelected())
    {
      chkCreateBibEntry.setSelected(true);
      return Ternary.True;
    }

    if (choice.isFalse() && ((origBD == null) || BibData.externalFieldsAreSame(bd, origBD)))
      return Ternary.False;

    EnumSet<BibFieldEnum> extFields = bd.fieldsWithExternalData();
    if (extFields.isEmpty()) return choice;

    String msg = "The current work record is not associated with a " +
                 db.getBibLibrary().type().getUserFriendlyName() + " entry. Create one now?\n" +
                 "Otherwise, existing information for these fields will be lost: " +
                 extFields.stream().map(BibFieldEnum::getUserFriendlyName).reduce((s1, s2) -> s1 + ", " + s2).orElse("");

    choice = confirmDialog(msg) ? Ternary.True : Ternary.False;
    chkCreateBibEntry.setSelected(choice.isTrue());
    return choice;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateImportActionDefault()
  {
    if (chkSetDefault.isSelected() == false) return;

    if (rbMove.isSelected())
      db.prefs.put(PREF_KEY_IMPORT_ACTION_DEFAULT, PREF_KEY_IMPORT_ACTION_MOVE);
    else if (rbCopy.isSelected())
      db.prefs.put(PREF_KEY_IMPORT_ACTION_DEFAULT, PREF_KEY_IMPORT_ACTION_COPY);
    else
      db.prefs.put(PREF_KEY_IMPORT_ACTION_DEFAULT, PREF_KEY_IMPORT_ACTION_NONE);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    boolean success = true;
    FilePath newFilePath;

    getBibDataFromGUI();

    if (chkCreateBibEntry.isVisible())
    {
      newEntryChoice = promptToCreateBibEntry(curBD, chkCreateBibEntry, newEntryChoice, origBDtoUse);

      if (chkCreateBibEntry.isSelected() && (getEntryType() == null))
        return falseWithWarningMessage("Select a bibliographic entry type.", cbEntryType);
    }

    if (chkKeepFilenameUnchanged.isSelected() == false)
      if ((tfOrigFile.getText().length() > 0) && tfNewFile.getText().isEmpty())
        return falseWithWarningMessage("Enter destination file name.", tfNewFile);

    if (hcbType.selectedID() < 1)
      return falseWithWarningMessage("Select a work type.", cbType);

    if (tfOrigFile.getText().isEmpty())
    {
      if (oldWorkFile != null)
        return falseWithErrorMessage("Internal error #82709");

      updateImportActionDefault();
      return true;
    }

    if (rbCurrent.isSelected())
    {
      if (chkKeepFilenameUnchanged.isSelected())
        newFilePath = origFilePath;
      else
        newFilePath = origFilePath.getDirOnly().resolve(tfNewFile.getText());
    }
    else
    {
      if (chkKeepFilenameUnchanged.isSelected())
        newFilePath = destFolder.get().filePath().resolve(origFilePath.getNameOnly());
      else
        newFilePath = destFolder.get().filePath().resolve(tfNewFile.getText());
    }

    HDT_RecordWithPath existingFile = HyperPath.getRecordFromFilePath(newFilePath);

    if (existingFile != null)
    {
      if ((existingFile instanceof HDT_MiscFile) || (existingFile instanceof HDT_Person))
        return falseWithErrorMessage(HyperPath.alreadyInUseMessage(newFilePath, existingFile));

      HDT_WorkFile existingWorkFile = (HDT_WorkFile)existingFile;

      if (newWorkFile == null)
      {
        if (existingWorkFile.works.isEmpty())
          return falseWithErrorMessage("Internal error #79002");

        int oldWorkFileID = -1;
        if (oldWorkFile != null)
          oldWorkFileID = oldWorkFile.getID();

        if (oldWorkFileID == existingWorkFile.getID())
          newWorkFile = existingWorkFile;
        else
          return falseWithErrorMessage(HyperPath.alreadyInUseMessage(newFilePath, existingWorkFile));
      }
      else
      {
        if (newWorkFile.getID() != existingFile.getID())
          return falseWithErrorMessage(HyperPath.alreadyInUseMessage(newFilePath, existingFile));
      }
    }
    else
    {
      if ((newWorkFile == null) && (oldWorkFile != null) && (oldWorkFile.works.size() > 1))
        if (confirmDialog("The same file that was associated with this work is associated with other works as well. Should these also be updated?"))
          newWorkFile = oldWorkFile;
    }

    try
    {
      if (rbCopy.isSelected()) // either oldWorkFile is null, or oldWorkFile != newWorkFile
      {
        success = origFilePath.copyTo(newFilePath, true);

        if (success)
        {
          if (newWorkFile == null)
          {
            newWorkFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, newFilePath);
            if (newWorkFile == null)
              return falseWithErrorMessage("Internal error #67830");

            curWork.addWorkFile(newWorkFile.getID());
          }
          else
          {
            newWorkFile.getPath().assign(HyperPath.getFolderFromFilePath(newFilePath.getDirOnly(), true), newFilePath.getNameOnly());
          }
        }
      }
      else
      {
        if (newWorkFile == null)
        {
          if (origFilePath.equals(newFilePath))
            success = true;
          else
          {
            success = origFilePath.moveTo(newFilePath, true);
            if (success)
              db.unmapFilePath(origFilePath);
          }

          if (success)
          {
            newWorkFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, newFilePath);
            if (newWorkFile == null)
              return falseWithErrorMessage("Internal error #67830");

            curWork.addWorkFile(newWorkFile.getID());
          }
        }
        else if (oldWorkFile == newWorkFile)
        {
          if (origFilePath.equals(newFilePath) == false)
          {
            if (origFilePath.moveTo(newFilePath, true) == false) return false;

            db.unmapFilePath(origFilePath);
            newWorkFile.getPath().assign(HyperPath.getFolderFromFilePath(newFilePath.getDirOnly(), true), newFilePath.getNameOnly());
          }

          success = true;
        }
        else
        {
          if (oldWorkFile != null)
            return falseWithErrorMessage("Unable to move the file. Reason: Cannot change assignment from one file to another that is already assigned to a different file record.");

          success = newWorkFile.getPath().moveToFolder(HyperPath.getFolderFromFilePath(newFilePath.getDirOnly(), true).getID(), true, true, newFilePath.getNameOnly().toString());
          if (success) curWork.addWorkFile(newWorkFile.getID());
        }
      }
    }
    catch (IOException e)
    {
      return falseWithErrorMessage("Unable to " + (rbCopy.isSelected() ? "copy" : "move") + "/rename the file. Reason: " + e.getMessage());
    }

    if (success == false)
      return falseWithErrorMessage("Unable to " + (rbCopy.isSelected() ? "copy" : "move") + "/rename the file.");

    if ((oldWorkFile != null) && (newWorkFile.getID() != oldWorkFile.getID()))
      db.getObjectList(rtWorkFileOfWork, curWork, true).remove(oldWorkFile);

    updateImportActionDefault();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EntryType getEntryType()
  {
    EntryType entryType = cbEntryType.getValue();

    if (entryType == null) return null;

    switch (entryType)
    {
      case etUnentered : case etOther : case etNone : return null;
      default: return entryType;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
