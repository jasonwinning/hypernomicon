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

package org.hypernomicon.dialogs;

import java.io.IOException;
import java.time.Month;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javafx.scene.control.*;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;

import org.controlsfx.control.MasterDetailPane;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibDataRetriever;
import org.hypernomicon.bib.data.BibDataStandalone;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.dialogs.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.bib.data.CrossrefBibData;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.data.GUIBibData;
import org.hypernomicon.bib.data.GoogleBibData;
import org.hypernomicon.bib.data.PDFBibData;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_RecordWithPath;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.model.records.HDT_WorkFile.FileNameAuthor;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.previewWindow.PreviewWrapper;

import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.DateControlsWrapper;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTable.CellUpdateHandler;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.MainCtrlr.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import javafx.application.Platform;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.fxml.FXML;
import javafx.geometry.Side;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Screen;
import javafx.stage.Stage;

//---------------------------------------------------------------------------

public class WorkDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apMain;
  @FXML private Button btnRegenerateFilename, btnStop, btnDest, btnPaste;
  @FXML private CheckBox chkSetDefault, chkCreateBibEntry, chkKeepFilenameUnchanged;
  @FXML private ComboBox<EntryType> cbEntryType;
  @FXML private ComboBox<HyperTableCell> cbType;
  @FXML private ComboBox<Month> cbMonth;
  @FXML private Hyperlink hlCase;
  @FXML private Label lblAutoPopulated;
  @FXML private MenuItem mnuPopulateUsingDOI, mnuPopulateFromPDF;
  @FXML private ProgressBar progressBar;
  @FXML private RadioButton rbCopy, rbCurrent, rbMove;
  @FXML private SplitMenuButton btnAutoFill;
  @FXML private TableView<HyperTableRow> tvAuthors, tvISBN;
  @FXML private TextArea taMisc;
  @FXML private TextField tfDest, tfDOI, tfFileTitle, tfNewFile, tfOrigFile, tfTitle, tfYear, tfDay;
  @FXML private ToggleButton btnPreview;
  @FXML private ToggleGroup tgSelect;

  @FXML public Button btnCancel;

  private final AnchorPane apPreview;
  private final MasterDetailPane mdp;
  private final HyperCB hcbType;
  private final HyperTable htAuthors, htISBN;
  private final DateControlsWrapper dateCtrls;
  private final HDT_WorkFile oldWorkFile;
  private final GUIBibData curBD;
  private final HDT_Work curWork;
  private final Property<HDT_Folder> destFolder = new SimpleObjectProperty<>();

  private BibDataRetriever bibDataRetriever = null;
  private HDT_WorkFile newWorkFile = null;
  private PDFJSWrapper jsWrapper = null;
  private FilePath previewFilePath = null, origFilePath = null;
  private GUIBibData origBDtoUse = null;
  private Ternary newEntryChoice;
  private boolean userOverrideDest = false, dontRegenerateFilename = false, previewInitialized = false;

  private final MutableBoolean alreadyChangingTitle = new MutableBoolean(false);

  private static final AsyncHttpClient httpClient = new AsyncHttpClient();

  private static final String MovePrefVal = "move",
                              CopyPrefVal = "copy",
                              NonePrefVal = "none";

  public List<ObjectGroup> getAuthorGroups() { return htAuthors.getAuthorGroups(curWork, 0, 2, 3, 4); }
  public boolean getCreateEntry()            { return chkCreateBibEntry.isVisible() && chkCreateBibEntry.isSelected(); }

  @FXML private void btnLaunchClick()        { launchFile(origFilePath); }

//---------------------------------------------------------------------------

  public WorkDlgCtrlr(FilePath filePathToUse, BibData bdToUse, Ternary newEntryChoice, EntryType newEntryType)
  {
    this("Import New Work File", null, filePathToUse, bdToUse, newEntryChoice, newEntryType);
  }

  public WorkDlgCtrlr(HDT_WorkFile workFileToUse)
  {
    this("Work File", workFileToUse, null, null, Ternary.Unset, EntryType.etUnentered);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private WorkDlgCtrlr(String dialogTitle, HDT_WorkFile workFileToUse, FilePath filePathToUse, BibData bdToUse, Ternary newEntryChoice, EntryType newEntryType)
  {
    super("WorkDlg", dialogTitle, true);

    dateCtrls = new DateControlsWrapper(tfYear, cbMonth, tfDay);

    apPreview = new AnchorPane();
    mdp = addPreview(stagePane, apMain, apPreview, btnPreview);

    mdp.showDetailNodeProperty().addListener((ob, ov, nv) ->
    {
      if (Boolean.TRUE.equals(nv) == false) return;

      if ((previewInitialized == false) && (jxBrowserDisabled == false))
        accommodatePreview(dialogStage, apMain, mdp);

      updatePreview();
    });

    curWork = ui.workHyperTab().activeRecord();
    curBD = new GUIBibData(curWork.getBibData());

    hcbType = initWorkTypeHCB();
    htISBN = initISBNTable();
    htAuthors = initAuthorsTable();

    initControls();

    this.newEntryChoice = newEntryChoice;
    origBDtoUse = bdToUse == null ? null : new GUIBibData(bdToUse);

    chkCreateBibEntry.setSelected(newEntryChoice.isTrue());

    if ((db.bibLibraryIsLinked() == false) || (curWork.getBibEntryKey().length() > 0))
      setAllVisible(false, chkCreateBibEntry, cbEntryType);
    else
      chkCreateBibEntry.setText("Create new " + db.bibLibraryUserFriendlyName() + " entry of type:");

    if (workFileToUse == null)
    {
      oldWorkFile = null;

      switch (db.prefs.get(PrefKey.IMPORT_ACTION_DEFAULT, MovePrefVal))
      {
        case MovePrefVal : rbMove   .setSelected(true); break;
        case CopyPrefVal : rbCopy   .setSelected(true); break;
        case NonePrefVal : rbCurrent.setSelected(true); break;
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

    if (oldWorkFile != null)
    {
      updatePreview();
      tfOrigFile.setText(origFilePath.toString());
    }

    htAuthors.clear();
    htAuthors.getPopulator(0).populate(false);

    boolean atLeastOneInFilename = false;

    for (HyperTableRow origRow : ui.workHyperTab().htAuthors.dataRows())
    {
      int authID = origRow.getID(1);
      String authName = origRow.getText(1);
      Ternary isInFileName = Ternary.Unset;

      if (authID > 0)
        isInFileName = curWork.personIsInFileName(origRow.getRecord());
      else
      {
        htAuthors.getPopulator(0).addEntry(authName);
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

    onShown = () ->
    {
      disableCache(taMisc);

      if (oldWorkFile == null)
      {
        if (filePathToUse == null)
        {
          btnSrcBrowseClick();

          if (FilePath.isEmpty(origFilePath))
            safeFocus(tfTitle);
        }
        else
          useChosenFile(filePathToUse, bdToUse);
      }

      if (newEntryType != EntryType.etUnentered)
        cbEntryType.getSelectionModel().select(newEntryType);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperCB initWorkTypeHCB()
  {
    StandardPopulator pop = new StandardPopulator(hdtWorkType, id -> HDT_WorkType.workTypeIDToEnumVal(id) != wtUnenteredSet);
    HyperCB hyperCB = new HyperCB(cbType, ctEditableLimitedDropDown, pop);

    hyperCB.addListener((oldValue, newValue) ->
    {
      if (newValue == null) return;

      WorkTypeEnum workTypeEnumVal = HDT_WorkType.workTypeIDToEnumVal(HyperTableCell.getCellID(newValue)),
                   oldEnumVal = curWork.getWorkTypeEnum();

      if ((oldEnumVal == wtUnenteredSet) && (workTypeEnumVal != wtUnenteredSet))
      {
        errorPopup("You cannot change the work type after it has been set to Unentered Set of Work Files.");
        Platform.runLater(() -> cbType.setValue(oldValue));
        return;
      }

      if (workTypeEnumVal == wtUnenteredSet)
      {
        if ((oldEnumVal != wtUnenteredSet) && (oldEnumVal != wtNone))
        {
          errorPopup("You cannot change a work with an existing work type into an unentered set of work files.");
          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }

        tfNewFile.disableProperty().unbind();

        disableAll(tfFileTitle, tfNewFile, tfDest, btnDest, chkSetDefault, chkKeepFilenameUnchanged, btnRegenerateFilename, rbMove, rbCopy, rbCurrent);
        dateCtrls.setDisable(true);
      }
      else if (HyperTableCell.getCellID(newValue) > 0)
      {
        enableAll(tfFileTitle, tfDest, btnDest, chkSetDefault, chkKeepFilenameUnchanged, btnRegenerateFilename, rbMove, rbCopy);
        dateCtrls.setDisable(false);

        tfNewFile.disableProperty().bind(chkKeepFilenameUnchanged.selectedProperty());

        updateDest();
      }
    });

    return hyperCB;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTable initISBNTable()
  {
    HyperTable hyperTable = new HyperTable(tvISBN, 0, true, "");

    hyperTable.addTextEditCol(hdtWork, true);

    hyperTable.addContextMenuItem("Use this ISBN to fill in fields",
      row -> row.getText(0).length() > 0,
      row ->
      {
        List<String> list = matchISBN(row.getText(0));
        if (collEmpty(list) == false)
          industryIdClick(false, null, list.get(0));
      });

    return hyperTable;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTable initAuthorsTable()
  {
    HyperTable hyperTable = new HyperTable(tvAuthors, 0, true, TablePrefKey.WORK_DLG);

    hyperTable.addAuthorEditCol(() -> curWork, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      dontRegenerateFilename = true;

      row.setCheckboxValue(1, HyperTableCell.getCellID(cellVal) > 0);

      if (hyperTable.dataRowCount() == 1)
        row.setCheckboxValue(2, true);
      else if (HyperTableCell.getCellID(cellVal) > 0)
      {
        boolean useInFilename = true, keepGoing = true;
        Iterator<HyperTableRow> it = hyperTable.dataRows().iterator();

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
          hyperTable.dataRows().forEach(loopRow -> loopRow.setCheckboxValue(2, false));
          row.setCheckboxValue(2, true);
        }
      }

      dontRegenerateFilename = false;

      btnRegenerateFilenameClick();
    });

    hyperTable.addCheckboxColWithUpdateHandler(createAuthorRecordHandler(hyperTable, () -> curWork));

    CellUpdateHandler handler = (row, cellVal, nextColNdx, nextPopulator) -> btnRegenerateFilenameClick();

    hyperTable.addCheckboxColWithUpdateHandler(handler);
    hyperTable.addCheckboxColWithUpdateHandler(handler);
    hyperTable.addCheckboxColWithUpdateHandler(handler);

    Consumer<HyperTableRow> removeHandler = removedRow ->
    {
      if (hyperTable.dataRowCount() == 0) return;

      HyperTableRow firstRecordRow = null;

      for (HyperTableRow row : hyperTable.dataRows())
      {
        if (row.getCheckboxValue(2))
          return;

        if ((firstRecordRow == null) && (row.getID(0) > 0))
          firstRecordRow = row;
      }

      Objects.requireNonNullElseGet(firstRecordRow, () -> hyperTable.dataRowStream().findFirst().orElseThrow()).setCheckboxValue(2, true);
    };

    hyperTable.addRemoveMenuItem(removeHandler);

    hyperTable.addContextMenuItem("Remove this row", row -> (row.getText(0).length() > 0) && (row.getID(0) < 1), row ->
    {
      hyperTable.removeRow(row);
      removeHandler.accept(row);
    });

    hyperTable.addChangeOrderMenuItem(true);

    return hyperTable;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initControls()
  {
    lblAutoPopulated.setText("");
    tfOrigFile.setEditable(false);

    destFolder.addListener((obs, ov, nv) -> tfDest.setText(nv == null ? "" : (nv.pathNotEmpty() ? db.getRootPath().relativize(nv.filePath()).toString() : "")));

    if (db.bibLibraryIsLinked())
      bibManagerDlg.initEntryTypeCB(cbEntryType);

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

    setToolTip(btnAutoFill, AUTOFILL_TOOLTIP);

    tfTitle.textProperty().addListener((ob, oldValue, newValue) ->
    {
      String fileTitle = newValue;

      fileTitle = fileTitle.replace('?', ':')
                           .replace('/', '-');

      int pos = fileTitle.indexOf(':');
      if (pos >= 0) fileTitle = fileTitle.substring(0, pos);

      fileTitle = FilePath.removeInvalidFileNameChars(fileTitle);

      tfFileTitle.setText(fileTitle.trim());
    });

    tfFileTitle.textProperty().addListener((ob, oldValue, newValue) -> btnRegenerateFilenameClick());

    tfYear.textProperty().addListener((ob, oldValue, newValue) -> btnRegenerateFilenameClick());

    tfOrigFile.textProperty().addListener((ob, oldValue, newValue) -> btnRegenerateFilenameClick());

    hlCase.setOnMouseClicked(event ->
    {
      alreadyChangingTitle.setTrue();
      tfTitle.setText(HDT_Work.fixCase(tfTitle.getText()));
      alreadyChangingTitle.setFalse();
    });

    tfTitle.setTextFormatter(titleFormatter(alreadyChangingTitle));
    changeToClearButton();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static TextFormatter<?> titleFormatter(MutableBoolean alreadyChangingTitle)
  {
    return titleFormatter(alreadyChangingTitle, null);
  }

  public static TextFormatter<?> titleFormatter(MutableBoolean alreadyChangingTitle, Toggle rb)
  {
    return new TextFormatter<>(change ->
    {
      if (alreadyChangingTitle.isTrue()) return change;

      if ((rb != null) && ((change.getText().length() > 0) || ((change.getRangeEnd() - change.getRangeStart()) > 0)))
        rb.setSelected(true);

      if (change.getText().length() > 1)
      {
        alreadyChangingTitle.setTrue();

        String title = convertToSingleLine(change.getControlNewText());
        while (title.contains("  "))
          title = title.replaceAll("  ", " ");

        if (title.equals(title.toUpperCase()) || title.equals(title.toLowerCase()))
          title = HDT_Work.fixCase(title);

        change.setRange(0, change.getControlText().length());
        change.setText(ultraTrim(title));
        alreadyChangingTitle.setFalse();
      }

      return change;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateDest()
  {
    if (userOverrideDest) return;

    destFolder.setValue(db.getImportFolderForWorkType(HDT_WorkType.getEnumVal(hcbType.selectedRecord())));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * When there is a checkbox column next to an editable author column where you specify if you want to create a record for
   * the author, this creates the update handler for the checkbox column so that the Person record gets immediately created
   * when you click it.
   * <p>
   * The work supplied by the workSupplier is used during the process of finding duplicate authors. If the "duplicate" is from
   * the work already being edited, it is not treated as a duplicate.
   *
   * @param htAuthors The table the columns are in
   * @param workSupplier Callback function to get the relevant work for the context the table is in (whether there is a
   * work we are merging into or a work we are editing, etc.)
   * @return The update handler
   */
  public static CellUpdateHandler createAuthorRecordHandler(HyperTable htAuthors, Supplier<HDT_Work> workSupplier)
  {
    return (row, cellVal, nextColNdx, nextPopulator) ->
    {
      // If the user unchecked the Record checkbox while a record is selected, re-check it.

      if (row.getID(0) > 0)
      {
        Platform.runLater(() ->
        {
          row.setCheckboxValue(1, true);
          htAuthors.refresh();
        });

        return;
      }

      // If the user unchecked the Record checkbox while a record was not selected, don't do anything else.

      if (GenericNonRecordHTC.falseCell.equals(cellVal))
        return;

      // Now show the New Person Dialog, which checks for duplicates

      String text = row.getText(0);

      Author author = null;
      HDT_Work work = workSupplier.get();

      if (work != null)
      {
        author = work.getAuthors().getAuthor(new PersonName(text));

        if (author == null)
          author = new Author(work, new PersonName(text), false, false, Ternary.Unset);
      }

      NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(true, text, author);

      if (npdc.showModal())
      {
        htAuthors.getPopulator(0).setChanged(row);              // A new record has been created so force it to repopulate
        htAuthors.selectID(0, row, npdc.getPerson().getID());
      }
      else
      {
        Platform.runLater(() ->
        {
          row.setCheckboxValue(1, row.getID(0) > 0);
          htAuthors.refresh();
        });
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MasterDetailPane addPreview(AnchorPane stagePane, AnchorPane apMain, AnchorPane apPreview, Toggle btnPreview)
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

    if (previewInitialized == false) jsWrapper = new PDFJSWrapper(apPreview);

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

    String ext = FilenameUtils.getExtension(tfOrigFile.getText());
    if (ext.isEmpty())
      ext = FilenameUtils.getExtension(tfNewFile.getText());

    List<FileNameAuthor> authors = new ArrayList<>();

    htAuthors.dataRows().forEach(row ->
    {
      if ((row.getRecord() != null) || (row.getText(0).length() > 0))
        if (row.getCheckboxValue(2))
          authors.add(new FileNameAuthor(row.getText(0), row.getCheckboxValue(3), row.getCheckboxValue(4)));
    });

    String newFileName = "", fileName = HDT_WorkFile.makeFileName(authors, hcbType.selectedRecord(), tfYear.getText(), tfFileTitle.getText(), curBD.getStr(bfContainerTitle), curBD.getStr(bfPublisher), ext);

    if (fileName.isBlank())
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

      newFileName = FilenameUtils.getBaseName(fileName) + (ctr == 1 ? "" : '_' + String.valueOf(1000 + (ctr % 1000)).substring(1, 4)) +
                    FilenameUtils.EXTENSION_SEPARATOR_STR + FilenameUtils.getExtension(fileName);
      nameTaken = false;

      for (HDT_WorkFile file : curWork.workFiles)
      {
        if (file != oldWorkFile)
          if (FilenameUtils.equalsNormalizedOnSystem(file.getPath().getNameStr(), newFileName))
            nameTaken = true;
      }
    }

    tfNewFile.setText(ultraTrim(newFileName));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnDestBrowseClick()
  {
    DirectoryChooser dirChooser = new DirectoryChooser();

    FilePath folderPath = nullSwitch(destFolder.getValue(), null, HDT_Folder::filePath);

    dirChooser.setInitialDirectory(FilePath.isEmpty(folderPath) || (folderPath.exists() == false) ?
      db.topicalPath().toFile()
    :
      folderPath.toFile());

    dirChooser.setTitle("Select Destination Folder");

    FilePath filePath = showDirDialog(dirChooser);

    if (FilePath.isEmpty(filePath)) return;

    HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath, true);

    if ((folder == null) || (folder.getID() == ROOT_FOLDER_ID))
    {
      errorPopup("You must choose a subfolder of the main database folder.");
      return;
    }

    userOverrideDest = true;
    destFolder.setValue(folder);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnSrcBrowseClick()
  {
    FileChooser fileChooser = new FileChooser();

    if (EnumSet.of(wtBook, wtChapter, wtThesis, wtNone, wtPaper).contains(curWork.getWorkTypeEnum()))
      fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Adobe PDF file (*.pdf)", "*.pdf"));

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    fileChooser.setInitialDirectory(db.unenteredPath().toFile());

    useChosenFile(showOpenDialog(fileChooser), null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void useChosenFile(FilePath chosenFile, BibData bdToUse)
  {
    if (FilePath.isEmpty(chosenFile)) return;

    if (db.isProtectedFile(chosenFile, false))
    {
      errorPopup("That file cannot be assigned to a work record.");
      return;
    }

    // See if the chosen file is currently assigned to a file record

    HDT_RecordWithPath file = HyperPath.getRecordFromFilePath(chosenFile);

    if (file != null)
    {
      if ((file instanceof HDT_MiscFile) || (file instanceof HDT_Person))
      {
        errorPopup(HyperPath.alreadyInUseMessage(chosenFile, file));
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
      populateFieldsFromBibData(bdToUse, true, true);
    else if (tfTitle.getText().isEmpty() && BibliographicDate.isEmpty(dateCtrls.getDate()))
      extractDataFromPdf(app.prefs.getBoolean(PrefKey.AUTO_RETRIEVE_BIB, true), false, true);
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

    changeToClearButton();
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
      changeToStopButton();

      bibDataRetriever = new BibDataRetriever(httpClient, curBD, safeListOf(origFilePath), (pdfBD, queryBD, messageShown) ->
      {
        changeToClearButton();

        if ((pdfBD == null) && (queryBD == null))
        {
          if (launchIfNoData && app.prefs.getBoolean(PrefKey.AUTO_OPEN_PDF, true))
            mdp.setShowDetailNode(true);

          return;
        }

        if (doMerge)
          doMerge(pdfBD, queryBD);
        else if ((queryBD instanceof CrossrefBibData) || (queryBD instanceof GoogleBibData))
          populateFieldsFromBibData(queryBD, true, true);
        else
        {
          populateFieldsFromBibData(pdfBD, true, true);

          if (launchIfNoData && app.prefs.getBoolean(PrefKey.AUTO_OPEN_PDF, true))
            mdp.setShowDetailNode(true);
        }
      });

      return;
    }

    PDFBibData pdfBD;

    try
    {
      pdfBD = PDFBibData.createFromFiles(safeListOf(origFilePath));

      if (BibData.isEmpty(pdfBD))
        pdfBD = null;
    }
    catch (IOException e)
    {
      errorPopup("An error occurred while extracting metadata from PDF file: " + getThrowableMessage(e));
      return;
    }

    if (pdfBD == null)
    {
      if (launchIfNoData && app.prefs.getBoolean(PrefKey.AUTO_OPEN_PDF, true))
        mdp.setShowDetailNode(true);

      return;
    }

    tfDOI.setText(pdfBD.getStr(bfDOI));
    htISBN.clear();

    htISBN.buildRows(pdfBD.getMultiStr(bfISBNs), (row, isbnStr) -> row.setCellValue(0, isbnStr, hdtNone));

    if (doMerge)
    {
      doMerge(pdfBD, null);
    }
    else
    {
      populateFieldsFromBibData(pdfBD, true, true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doMerge(BibData bd1, BibData bd2)
  {
    getBibDataFromGUI();
    MergeWorksDlgCtrlr mwd = null;

    try
    {
      mwd = new MergeWorksDlgCtrlr("Select How to Merge Fields", Stream.of(curBD, bd1, bd2), curWork, false, curWork.getBibEntryKey().isBlank(),
                                   chkCreateBibEntry.isSelected() ? Ternary.True : Ternary.Unset, origFilePath);
    }
    catch (IOException e)
    {
      errorPopup("Unable to initialize merge dialog window.");
      return;
    }

    if (mwd.showModal() == false)
      return;

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
        switch (mwd.getEntryType())
        {
          case etBook : hcbType.selectID(HDT_WorkType.getIDbyEnum(wtBook)); break;

          case etJournalArticle : hcbType.selectID(HDT_WorkType.getIDbyEnum(wtPaper)); break;

          case etThesis : case etMastersThesis : case etDoctoralThesis : hcbType.selectID(HDT_WorkType.getIDbyEnum(wtThesis)); break;

          default : break;
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void changeToStopButton()
  {
    ImageView imageView = imgViewFromRelPath("resources/images/cancel.png");
    imageView.setFitHeight(16);
    imageView.setPreserveRatio(true);
    btnStop.setGraphic(imageView);

    progressBar.setVisible(true);

    btnStop.setOnAction(event -> stopRetrieving());
    setToolTip(btnStop, "Stop retrieving");

    btnStop.visibleProperty().unbind();
    btnStop.setVisible(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void changeToClearButton()
  {
    ImageView imageView = imgViewFromRelPath("resources/images/broom.png");
    imageView.setFitHeight(16);
    imageView.setPreserveRatio(true);
    btnStop.setGraphic(imageView);

    progressBar.setVisible(false);

    btnStop.setOnAction(event -> clearFields());
    setToolTip(btnStop, "Clear fields");

    btnStop.visibleProperty().unbind();
    btnStop.visibleProperty().bind(lblAutoPopulated.textProperty().isNotEmpty());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearFields()
  {
    populateFieldsFromBibData(new GUIBibData(), true);
    clearAuthors(htAuthors);
    lblAutoPopulated.setText("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void clearAuthors(HyperTable htAuthors)
  {
    htAuthors.clear();
    htAuthors.getPopulator(0).populate(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void industryIdClick(boolean crossref, String doi, String isbn)
  {
    stopRetrieving();

    lblAutoPopulated.setText("");
    changeToStopButton();

    Consumer<BibDataStandalone> doneHndlr = queryBD ->
    {
      changeToClearButton();

      if (queryBD == null)
      {
        if (crossref) lblAutoPopulated.setText("Crossref query yielded no results for doi: "      + doi);
        else          lblAutoPopulated.setText("Google Books query yielded no results for isbn: " + isbn);

        return;
      }

      doMerge(queryBD, null);
    };

    GUIBibData bd = new GUIBibData();
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

  private static void addAuthorToTable(PersonName authorName, boolean editor, boolean trans, HDT_Person author, HyperTable htAuthors, boolean hasShowInFileCol)
  {
    if (authorName.isEmpty()) return;

    HyperTableRow row = htAuthors.newDataRow();

    if (author != null)
    {
      row.setCellValue(0, author, author.listName());
      row.setCheckboxValue(1, true);
    }
    else
    {
      String authorStr = authorName.getLastFirst();

      htAuthors.getPopulator(0).addEntry(authorStr);
      row.setCellValue(0, authorStr, hdtPerson);
    }

    int addend = hasShowInFileCol ? 1 : 0;

    if (editor)
      row.setCheckboxValue(2 + addend, true);

    if (trans)
      row.setCheckboxValue(3 + addend, true);
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

    clearAuthors(htAuthors);

    for (int ndx = 0; ndx < nameList.size(); ndx++)
    {
      PersonName name = nameList.get(ndx);

      addAuthorToTable(name, nameToEd.get(name), nameToTr.get(name), personList.get(ndx), htAuthors, hasShowInFileCol);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public GUIBibData getBibDataFromGUI()
  {
    curBD.setDate(dateCtrls.getDate());
    curBD.setStr(bfDOI, matchDOI(tfDOI.getText()));
    curBD.setTitle(tfTitle.getText());

    curBD.setMultiStr(bfMisc, convertMultiLineStrToStrList(taMisc.getText(), true));

    curBD.setWorkType(hcbType.selectedRecord());

    curBD.setMultiStr(bfISBNs, htISBN.dataRowStream().map(row -> row.getText(0)).toList());

    curBD.getAuthors().setAllFromTable(getAuthorGroups());

    return curBD;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateFieldsFromBibData(BibData bd, boolean populateAuthors)
  {
    populateFieldsFromBibData(bd, populateAuthors, false);
  }

  private void populateFieldsFromBibData(BibData bd, boolean populateAuthors, boolean setAutopopulatedLabel)
  {
    if (setAutopopulatedLabel)
    {
      if (bd instanceof CrossrefBibData)
      {
        lblAutoPopulated.setText("Fields auto-populated from Crossref, doi: " + bd.getStr(bfDOI));
      }
      else if (bd instanceof GoogleBibData googleBibData)
      {
        String isbn = googleBibData.getQueryIsbn();
        lblAutoPopulated.setText(isbn.isBlank() ?
          "Fields have been auto-populated from Google Books"
        :
          "Fields auto-populated from Google Books, isbn: " + isbn);
      }
      else if (bd instanceof PDFBibData)
      {
        lblAutoPopulated.setText("Fields auto-populated with information extracted from PDF file");
      }
    }

    if (bd != curBD)
      curBD.copyAllFieldsFrom(bd, populateAuthors, true);

    if (curBD.entryTypeNotEmpty() && db.bibLibraryIsLinked())
    {
      EntryType entryType = curBD.getEntryType();
      if (cbEntryType.getItems().contains(entryType) == false)
      {
        warningPopup('"' + entryType.getUserFriendlyName() + "\" is not a valid " + db.bibLibraryUserFriendlyName() + " entry type.");
        cbEntryType.getSelectionModel().select(null);
      }
      else
        cbEntryType.getSelectionModel().select(entryType);
    }

    dateCtrls.setDate(curBD.getDate());

    alreadyChangingTitle.setTrue();
    tfTitle.setText(curBD.getStr(bfTitle));
    alreadyChangingTitle.setFalse();

    taMisc.setText(curBD.getStr(bfMisc));

    hcbType.selectIDofRecord(curBD.getWorkType());

    tfDOI.setText(curBD.getStr(bfDOI));

    htISBN.clear();

    htISBN.buildRows(curBD.getMultiStr(bfISBNs), (row, isbnStr) -> row.setCellValue(0, isbnStr, hdtNone));

    if (populateAuthors)
      loadFromBibAuthors(curBD.getAuthors(), htAuthors, true, curWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Ternary promptToCreateBibEntry(GUIBibData bd, CheckBox chkCreateBibEntry, Ternary choice, GUIBibData origBD)
  {
    if (chkCreateBibEntry.isSelected())
    {
      chkCreateBibEntry.setSelected(true);
      return Ternary.True;
    }

    if (choice.isFalse() && ((origBD == null) || GUIBibData.externalFieldsAreSame(bd, origBD)))
      return Ternary.False;

    EnumSet<BibFieldEnum> extFields = bd.fieldsWithExternalData();
    if (extFields.isEmpty()) return choice;

    String msg = "The current work record is not associated with a " +
                 db.bibLibraryUserFriendlyName() + " entry. Create one now?\n" +
                 "Otherwise, existing information for these fields will be lost: " +
                 extFields.stream().map(BibFieldEnum::getUserFriendlyName).collect(Collectors.joining(", "));

    choice = confirmDialog(msg, false) ? Ternary.True : Ternary.False;
    chkCreateBibEntry.setSelected(choice.isTrue());
    return choice;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateImportActionDefault()
  {
    if (chkSetDefault.isSelected() == false) return;

    if      (rbMove.isSelected()) db.prefs.put(PrefKey.IMPORT_ACTION_DEFAULT, MovePrefVal);
    else if (rbCopy.isSelected()) db.prefs.put(PrefKey.IMPORT_ACTION_DEFAULT, CopyPrefVal);
    else                          db.prefs.put(PrefKey.IMPORT_ACTION_DEFAULT, NonePrefVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    getBibDataFromGUI();

    if (chkCreateBibEntry.isVisible())
    {
      newEntryChoice = promptToCreateBibEntry(curBD, chkCreateBibEntry, newEntryChoice, origBDtoUse);

      if (chkCreateBibEntry.isSelected() && (getEntryType() == null))
        return falseWithWarningPopup("Select a bibliographic entry type.", cbEntryType);
    }

    if (chkKeepFilenameUnchanged.isSelected() == false)
      if ((tfOrigFile.getText().length() > 0) && tfNewFile.getText().isBlank())
        return falseWithWarningPopup("Enter destination file name.", tfNewFile);

    if (hcbType.selectedID() < 1)
      return falseWithWarningPopup("Select a work type.", cbType);

    if (tfTitle.getText().isBlank() && (confirmDialog("Are you sure you want to leave the title blank?", "Yes", GO_BACK_BUTTON_CAPTION, false) == false))
    {
      Platform.runLater(() -> safeFocus(tfTitle));
      return false;
    }

    if (tfOrigFile.getText().isEmpty())
    {
      if (oldWorkFile != null)
        return falseWithInternalErrorPopup(82709);

      updateImportActionDefault();
      return true;
    }

    if (FilePath.isFilenameValid(tfNewFile.getText()) == false)
      return falseWithErrorPopup("Invalid file name: \"" + tfNewFile.getText() + '"', tfNewFile);

    FilePath newFilePath = rbCurrent.isSelected() ?
      (chkKeepFilenameUnchanged.isSelected() ?
        origFilePath
      :
        origFilePath.getDirOnly().resolve(ultraTrim(tfNewFile.getText())))
    :
      (chkKeepFilenameUnchanged.isSelected() ?
        destFolder.getValue().filePath().resolve(origFilePath.getNameOnly())
      :
        destFolder.getValue().filePath().resolve(ultraTrim(tfNewFile.getText())));

    HDT_RecordWithPath existingFile = HyperPath.getRecordFromFilePath(newFilePath);

    if (existingFile != null)
    {
      if ((existingFile instanceof HDT_MiscFile) || (existingFile instanceof HDT_Person))
        return falseWithErrorPopup(HyperPath.alreadyInUseMessage(newFilePath, existingFile));

      HDT_WorkFile existingWorkFile = (HDT_WorkFile)existingFile;

      if (newWorkFile == null)
      {
        if (existingWorkFile.works.isEmpty())
          return falseWithInternalErrorPopup(79002);

        int oldWorkFileID = -1;
        if (oldWorkFile != null)
          oldWorkFileID = oldWorkFile.getID();

        if (oldWorkFileID == existingWorkFile.getID())
          newWorkFile = existingWorkFile;
        else
          return falseWithErrorPopup(HyperPath.alreadyInUseMessage(newFilePath, existingWorkFile));
      }
      else
      {
        if (newWorkFile.getID() != existingFile.getID())
          return falseWithErrorPopup(HyperPath.alreadyInUseMessage(newFilePath, existingFile));
      }
    }
    else
    {
      if ((newWorkFile == null) && (oldWorkFile != null) && (oldWorkFile.works.size() > 1))
        if (confirmDialog("The same file that was associated with this work is associated with other works as well. Should these also be updated?", true))
          newWorkFile = oldWorkFile;
    }

    boolean success = true;

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
              return falseWithInternalErrorPopup(67830);

            curWork.addWorkFile(newWorkFile.getID());
          }
          else
          {
            newWorkFile.getPath().assign(HyperPath.getFolderFromFilePath(newFilePath.getDirOnly(), true), newFilePath.getNameOnly());
          }
        }
      }
      else if (newWorkFile == null)
      {
        if (origFilePath.equals(newFilePath) == false)
        {
          success = origFilePath.moveTo(newFilePath, true);
          if (success)
            db.unmapFilePath(origFilePath);
        }

        if (success)
        {
          newWorkFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, newFilePath);
          if (newWorkFile == null)
            return falseWithInternalErrorPopup(67830);

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
      }
      else
      {
        if (oldWorkFile != null)
          return falseWithErrorPopup("Unable to move the file. Reason: Cannot change assignment from one file to another that is already assigned to a different file record.");

        success = newWorkFile.getPath().moveToFolder(HyperPath.getFolderFromFilePath(newFilePath.getDirOnly(), true).getID(), true, true, newFilePath.getNameOnly().toString());
        if (success) curWork.addWorkFile(newWorkFile.getID());
      }
    }
    catch (IOException | HDB_InternalError e)
    {
      return falseWithErrorPopup("Unable to " + (rbCopy.isSelected() ? "copy" : "move") + "/rename the file. Reason: " + getThrowableMessage(e));
    }

    if (success == false)
      return falseWithErrorPopup("Unable to " + (rbCopy.isSelected() ? "copy" : "move") + "/rename the file.");

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

    return entryType == null ? null : switch (entryType)
    {
      case etUnentered, etOther, etNone -> null;
      default                           -> entryType;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
