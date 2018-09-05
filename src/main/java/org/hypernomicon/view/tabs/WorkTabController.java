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

package org.hypernomicon.view.tabs;

import org.hypernomicon.bib.BibData;
import org.hypernomicon.bib.BibUtils;
import org.hypernomicon.bib.BibUtils.PdfMetadata;
import org.hypernomicon.bib.lib.BibEntry;
import org.hypernomicon.model.SearchKeys;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PositionVerdict;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.ChooseParentWorkFileDialogController;
import org.hypernomicon.view.dialogs.FileDialogController;
import org.hypernomicon.view.dialogs.NewPersonDialogController;
import org.hypernomicon.view.dialogs.SelectWorkDialogController;
import org.hypernomicon.view.dialogs.WorkDialogController;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.ExternalColumnPopulator;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.workMerge.MergeWorksDialogController;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.bib.BibData.BibFieldEnum.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.File;
import java.io.IOException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.UnaryOperator;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.json.simple.parser.ParseException;

import com.adobe.internal.xmp.XMPException;

import javafx.application.Platform;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.MenuButton;
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
import javafx.scene.control.TextFormatter;
import javafx.scene.control.Tooltip;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------  

public class WorkTabController extends HyperTab<HDT_Work, HDT_Work>
{ 
  @FXML private Button btnTree;
  @FXML private Button btnWorldCat;
  @FXML private Button btnScholar;
  @FXML private SplitMenuButton btnDOI;
  @FXML private Button btnOpenLink;
  @FXML private Button btnLaunch;
  @FXML private Button btnNewChapter;
  @FXML private Button btnLargerWork;
  @FXML private Button btnStop;
  @FXML private MenuButton btnWebQuery;  
  @FXML private MenuButton btnPDFMeta;
  @FXML private Button btnUseDOI;
  @FXML private Button btnUseISBN;
  @FXML private Button btnMergeBib;
  @FXML private Button btnBibManager;
  @FXML private MenuItem mnuGoogle;
  @FXML private MenuItem mnuCrossref;
  @FXML private MenuItem mnuFindDOIonCrossref;
  @FXML private MenuItem mnuFindISBNonGoogleBooks;
  @FXML private MenuItem mnuShowMetadata;
  @FXML private MenuItem mnuStoreMetadata;
  @FXML public TextField tfYear;
  @FXML private TextField tfTitle;
  @FXML public TextField tfDOI;
  @FXML private TextField tfSearchKey;
  @FXML private TextField tfLink;
  @FXML private TabPane tpBib;
  @FXML private Tab tabMiscBib;
  @FXML private TextArea taMiscBib;
  @FXML private Tab tabPdfMetadata;
  @FXML private TextArea taPdfMetadata;
  @FXML private Tab tabCrossref;
  @FXML private TextArea taCrossref;
  @FXML private Tab tabGoogleBooks;
  @FXML private TextArea taGoogleBooks;
  @FXML private TableView<HyperTableRow> tvAuthors;
  @FXML private TableView<HyperTableRow> tvLabels;
  @FXML private TableView<HyperTableRow> tvSubworks;
  @FXML private TableView<HyperTableRow> tvInvestigations;
  @FXML private TableView<HyperTableRow> tvArguments;
  @FXML private TableView<HyperTableRow> tvMiscFiles;
  @FXML private TableView<HyperTableRow> tvWorkFiles;
  @FXML private TableView<HyperTableRow> tvISBN;
  @FXML private ComboBox<HyperTableCell> cbType;
  @FXML private ComboBox<HyperTableCell> cbLargerWork;
  @FXML private Tab tabWorkFiles;
  @FXML private Tab tabSubworks;
  @FXML private Tab tabMiscFiles;
  @FXML private Tab tabBibDetails;
  @FXML private Tab tabInvestigations;
  @FXML private TabPane tabPane;
  @FXML private AnchorPane apDescription;
  @FXML private SplitPane spHoriz1;
  @FXML private SplitPane spHoriz2;
  @FXML private SplitPane spVert;
  @FXML private AnchorPane apLowerMid;
  @FXML private AnchorPane apLowerRight;
  @FXML private TableView<HyperTableRow> tvKeyMentions;
  @FXML private TabPane lowerTabPane;
  @FXML private Tab tabArguments;
  @FXML private Tab tabKeyMentions;
  @FXML private Label lblTitle;
  @FXML private Label lblSearchKey;
  @FXML private ProgressBar progressBar;

  public HyperTable htAuthors, htLabels, htSubworks, htInvestigations, htArguments, htMiscFiles, htWorkFiles, htKeyMentioners, htISBN;
  public HyperCB hcbType, hcbLargerWork;    
  public MainTextWrapper mainText;
  public FileDialogController fdc = null;
  public WorkDialogController wdc = null;
  private HashMap<Tab, String> tabCaptions = new HashMap<>();
  private boolean btnFolderAdded, inNormalMode = true, alreadyChangingTitle = false;
  double btnOpenLinkLeftAnchor, tfLinkLeftAnchor, tfLinkRightAnchor;
  private SplitMenuButton btnFolder = null;
  private HDT_Work curWork, lastWork = null;
  private ObjectProperty<BibData> crossrefBD = new SimpleObjectProperty<BibData>(), 
                                  pdfBD = new SimpleObjectProperty<BibData>(), 
                                  googleBD  = new SimpleObjectProperty<BibData>();
  
  public static final AsyncHttpClient httpClient = new AsyncHttpClient();

  @Override public HDT_RecordType getType()              { return hdtWork; }
  @Override public void enable(boolean enabled)          { ui.tabWorks.getContent().setDisable(enabled == false); }
  @Override public void findWithinDesc(String text)      { mainText.hilite(text); }  
  @Override public TextViewInfo getMainTextInfo()        { return mainText.getViewInfo(); }
  @Override public void setRecord(HDT_Work activeRecord) { curWork = activeRecord; }
  @Override public void focusOnSearchKey()               { safeFocus(tfSearchKey); }
  @Override public MainTextWrapper getMainTextWrapper()  { return mainText; }

  public String getTitle()                     { return tfTitle.getText(); }
  private void setTabCaption(Tab tab, int cnt) { tab.setText(tabCaptions.get(tab) + " (" + cnt + ")"); }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override protected void init(TabEnum tabEnum)
  {   
    this.tabEnum = tabEnum;
    mainText = new MainTextWrapper(apDescription);
    
    tabPane.getTabs().forEach(tab -> tabCaptions.put(tab, tab.getText()));
    
    lowerTabPane.getTabs().forEach(tab -> tabCaptions.put(tab, tab.getText()));
    
    htAuthors = new HyperTable(tvAuthors, 1, true, PREF_KEY_HT_WORK_AUTHORS);
    
    htAuthors.addActionCol(ctGoBtn, 1);
    htAuthors.addCol(hdtPerson, ctDropDownList);
    htAuthors.addCheckboxCol();
    htAuthors.addCheckboxCol();
    
    htAuthors.addRemoveMenuItem();
    htAuthors.addChangeOrderMenuItem(true);
    
    htAuthors.addCondRowBasedContextMenuItem("Remove this row",
        row -> (row.getText(1).length() > 0) && (row.getID(1) < 1),
        row -> htAuthors.removeRow(row));

    htAuthors.addCondRowBasedContextMenuItem("Create person record",
        row -> (row.getText(1).length() > 0) && (row.getID(1) < 1),
        row -> 
        {
          if (ui.cantSaveRecord(true)) return;
          
          String text = row.getText(1);
          
          Ternary isInFileName = Ternary.Unset;
          Author author = curWork.getAuthors().getAuthor(new PersonName(text));
          if (author != null)
            isInFileName = author.getInFileName();
          
          HDT_Person otherPerson = otherPersonToUse(text);
          
          if (otherPerson != null)
          {
            htAuthors.selectID(1, row, otherPerson.getID());
            saveToRecord(false);
            curWork.setPersonIsInFileName(otherPerson, isInFileName);
            ui.update();                  
            return;
          }
          
          NewPersonDialogController npdc = NewPersonDialogController.create(true, text, author);
          
          if (npdc.showModal())
          {
            Populator pop = htAuthors.getPopulator(1);
            pop.setChanged(row);                      // A new record has been created so force it to repopulate
            htAuthors.selectID(1, row, npdc.getPerson().getID());
            saveToRecord(false);
            curWork.setPersonIsInFileName(npdc.getPerson(), isInFileName);
            ui.update();
          }
        });

    htLabels = new HyperTable(tvLabels, 2, true, PREF_KEY_HT_WORK_LABELS);
    
    htLabels.addActionCol(ctGoBtn, 2);
    htLabels.addActionCol(ctBrowseBtn, 2);
    htLabels.addCol(hdtWorkLabel, ctDropDownList);

    htLabels.addRemoveMenuItem();
    htLabels.addChangeOrderMenuItem(true);

    htSubworks = new HyperTable(tvSubworks, 1, false, PREF_KEY_HT_WORK_SUB);
    
    htSubworks.addCol(hdtPerson, ctNone);
    htSubworks.addCol(hdtWork, ctNone);
    htSubworks.addCol(hdtWork, ctNone);
    
    htSubworks.addContextMenuItem(hdtPerson, "Go to person record", 
      person -> ui.goToRecord(person, true));

    htSubworks.addContextMenuItem(hdtWork, "Go to work record", 
      work -> ui.goToRecord(work, true));

    RecordListView.addDefaultMenuItems(htSubworks);
    
    htSubworks.addChangeOrderMenuItem(false, () -> curWork.subWorks.reorder(htSubworks.saveToList(1, hdtWork), true));
    
    htKeyMentioners = new HyperTable(tvKeyMentions, 1, false, PREF_KEY_HT_WORK_MENTIONERS);
    
    htKeyMentioners.addCol(hdtNone, ctNone);
    htKeyMentioners.addCol(hdtNone, ctNone);
    htKeyMentioners.addCol(hdtNone, ctNone);
    
    htInvestigations = new HyperTable(tvInvestigations, 2, true, PREF_KEY_HT_WORK_INV);
    
    htInvestigations.addActionCol(ctGoBtn, 2);
    htInvestigations.addColAltPopulatorWithUpdateHandler(hdtPerson, ctDropDownList, new ExternalColumnPopulator(htAuthors, 1), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      ((SubjectPopulator)nextPopulator).setObj(row, HyperTableCell.getRecord(cellVal));
      row.updateCell(nextColNdx, new HyperTableCell(-1, "", nextPopulator.getRecordType(row)));
    });
    htInvestigations.addColAltPopulator(hdtInvestigation, ctDropDownList, new SubjectPopulator(rtPersonOfInv, true));
    
    htInvestigations.addRemoveMenuItem();
    htInvestigations.addChangeOrderMenuItem(true);
    
    htArguments = new HyperTable(tvArguments, 2, false, PREF_KEY_HT_WORK_ARG);
    
    htArguments.addCol(hdtPosition, ctNone);
    htArguments.addCol(hdtNone, ctNone);      // record type = hdtNone so that the column will sort purely based on displayed text
    htArguments.addCol(hdtArgument, ctNone);
    
    htWorkFiles = new HyperTable(tvWorkFiles, 2, true, PREF_KEY_HT_WORK_FILES);
    
    htWorkFiles.addActionColWithButtonHandler(ctEditNewBtn, 2, (row, colNdx) ->
    {
      HDT_WorkFile workFile = null;
      int id = row.getID(colNdx);
      if (id > 0)
        workFile = db.workFiles.getByID(id);
           
      showWorkDialog(workFile);
    });
    
    htWorkFiles.addCheckboxCol();
    htWorkFiles.addCol(hdtWorkFile, ctNone);
    htWorkFiles.addTextEditColWithUpdateHandler(hdtWorkFile, false, true, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      int startPageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);
      if (startPageNum < 0) return;
      
      HDT_WorkFile workFile = (HDT_WorkFile) row.getRecord();
      if (workFile == null) return;
      
      int endPageNum = parseInt(row.getText(nextColNdx), -1);
      
      previewWindow.setPreview(pvsWorkTab, workFile.getPath().getFilePath(), startPageNum, endPageNum, curWork);
    });

    htWorkFiles.addTextEditColWithUpdateHandler(hdtWorkFile, false, true, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      int endPageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);
      if (endPageNum < 0) return;
      
      HDT_WorkFile workFile = (HDT_WorkFile) row.getRecord();
      if (workFile == null) return;
      
      int startPageNum = parseInt(row.getText(nextColNdx - 2), -1);
      
      previewWindow.setPreview(pvsWorkTab, workFile.getPath().getFilePath(), startPageNum, endPageNum, curWork);
    });

    htWorkFiles.addTextEditCol(hdtWorkFile, false, false);
    
    htWorkFiles.setTooltip(0, ButtonAction.baEdit, "Update or rename this work file");
    htWorkFiles.setTooltip(0, ButtonAction.baNew, "Add a new work file");
    
    htWorkFiles.addCondContextMenuItem(hdtWorkFile, "Launch file", 
        record -> HDT_WorkFile.class.cast(record).getPath().isEmpty() == false,
        record -> launchWorkFile(HDT_WorkFile.class.cast(record).getPath().getFilePath(), getCurPageNum(curWork, HDT_WorkFile.class.cast(record), true)));
    
    htWorkFiles.addCondContextMenuItem(hdtWorkFile, "Show in system explorer", 
        record -> HDT_WorkFile.class.cast(record).getPath().isEmpty() == false,
        record -> highlightFileInExplorer(HDT_WorkFile.class.cast(record).getPath().getFilePath()));

    htWorkFiles.addCondContextMenuItem(hdtWorkFile, "Show in File Manager",
        record -> HDT_WorkFile.class.cast(record).getPath().isEmpty() == false,
        record -> ui.goToFileInManager(HDT_WorkFile.class.cast(record).getPath().getFilePath()));
    
    htWorkFiles.addCondContextMenuItem(hdtWorkFile, "Copy path to clipboard",
        record -> HDT_WorkFile.class.cast(record).getPath().isEmpty() == false,
        record -> copyToClipboard(HDT_WorkFile.class.cast(record).getPath().toString()));
    
    htWorkFiles.addCondContextMenuItem(hdtWorkFile, "Update or rename this work file", 
        record -> HDT_WorkFile.class.cast(record).getPath().isEmpty() == false,
        record -> showWorkDialog((HDT_WorkFile)record));
    
    htWorkFiles.addCondContextMenuItemOkayIfBlank("Select parent work file", 
        record -> 
        {
          if (curWork == null) return false;
          if (curWork.largerWork.isNotNull() == false) return false;
          
          boolean noneToSelect = true;
          
          for (HDT_WorkFile workFile : curWork.largerWork.get().workFiles)
            if (curWork.workFiles.contains(workFile) == false)
              noneToSelect = false;
          
          return noneToSelect == false;
        },
        record ->
        {
          ChooseParentWorkFileDialogController ctrlr = ChooseParentWorkFileDialogController.create("Choose Work File", curWork);
          
          if (ctrlr.showModal() == false) return;
          
          HDT_WorkFile workFile = ctrlr.getWorkFile();
          if (workFile == null) return;
          
          HDT_WorkFile oldWorkFile = (HDT_WorkFile) htWorkFiles.selectedRecord();
          
          if (oldWorkFile == null)          
            curWork.addWorkFile(workFile.getID(), true, true);
          else
            curWork.replaceWorkFile(oldWorkFile, workFile);
          
          refreshFiles();          
        });
    
    RecordListView.CondRecordHandler condHandler = record ->
    {
      HDT_WorkFile workFile = HDT_WorkFile.class.cast(record);
      
      if (workFile.getPath().isEmpty()) return false;
      if (inNormalMode) return false;
      
      for (HDT_Work work : workFile.works)
        if (work.getWorkTypeValue() != wtUnenteredSet) return false;
      
      if (curWork.getWorkTypeValue() != wtUnenteredSet) return false;
        
      return true;
    }; 
 
    htWorkFiles.addCondContextMenuItem(hdtWorkFile, "Move to an existing work record", 
        condHandler,
        record -> moveFileToDifferentWork((HDT_WorkFile)record));
    
    htWorkFiles.addCondContextMenuItem(hdtWorkFile, "Move to a new work record", 
        condHandler,
        record -> moveFileToNewWork((HDT_WorkFile)record));   
    
    htWorkFiles.addCondContextMenuItem(hdtWorkFile, "Remove file",
        record -> HDT_WorkFile.class.cast(record).getPath().isEmpty() == false,
        record -> 
        { 
          if (ui.cantSaveRecord(true)) return;
          
          if (confirmDialog("Are you sure you want to remove this file from the work record?") == false) return;
          
          db.getObjectList(rtWorkFileOfWork, curWork, true).remove(record);
          fileManagerDlg.setNeedRefresh();
          ui.update();
        });
    
    htWorkFiles.addChangeOrderMenuItem(true, () ->
    {
      HyperObjList<HDT_Work, HDT_WorkFile> workFiles = db.getObjectList(rtWorkFileOfWork, curWork, true);
      ArrayList<HDT_WorkFile> newList = htWorkFiles.saveToList(2, hdtWorkFile);
      
      workFiles.reorder(newList);
    });
    
    htWorkFiles.setDblClickHandler((record) -> launchWorkFile(HDT_WorkFile.class.cast(record).getPath().getFilePath(), getCurPageNum(curWork, HDT_WorkFile.class.cast(record), true)));
    
    tvWorkFiles.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      if (oldValue == newValue) return;
      
      HDT_WorkFile workFile = (HDT_WorkFile) newValue.getRecord();
      if (workFile == null)
        previewWindow.setPreview(pvsWorkTab, curWork.getPath().getFilePath(), getCurPageNum(curWork, null, true), getCurPageNum(curWork, null, false), curWork);
      else
        previewWindow.setPreview(pvsWorkTab, workFile.getPath().getFilePath(), getCurPageNum(curWork, workFile, true), getCurPageNum(curWork, workFile, false), curWork);
    });
    
    tvSubworks.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      if (oldValue == newValue) return;
      
      HDT_Work subWork = (HDT_Work) newValue.getRecord();
      previewWindow.setPreview(pvsWorkTab, subWork.getPath().getFilePath(), subWork.getStartPageNum(), subWork.getEndPageNum(), subWork);
    });
    
    htMiscFiles = new HyperTable(tvMiscFiles, 1, true, PREF_KEY_HT_WORK_MISC);
    
    htMiscFiles.addActionCol(ctGoNewBtn, 1);
    htMiscFiles.addCol(hdtMiscFile, ctNone);
    
    htMiscFiles.addCondContextMenuItem(hdtMiscFile, "Launch file", 
      record -> HDT_MiscFile.class.cast(record).getPath().isEmpty() == false,
      record -> launchFile(HDT_MiscFile.class.cast(record).getPath().getFilePath()));
    
    htMiscFiles.addContextMenuItem(hdtMiscFile, "Go to file record", 
      record -> ui.goToRecord(record, true));    
    
    htISBN = new HyperTable(tvISBN, 0, true, "");
    
    htISBN.addTextEditCol(hdtWork, true, false);
    
    htISBN.addCondRowBasedContextMenuItem("WorldCat",
      row -> row.getText(0).length() > 0,
      row -> 
      {
        List<String> list = BibUtils.matchISBN(row.getText(0));
        if (list == null) return;
        if (list.size() == 0) return;
        searchWorldCatISBN(list.get(0)); 
      });
          
    htISBN.addCondRowBasedContextMenuItem("Google Books query",
        row -> row.getText(0).length() > 0,
        row -> 
        {
          List<String> list = BibUtils.matchISBN(row.getText(0));
          if (list == null) return;
          if (list.size() == 0) return;
          retrieveBibData(false, list.get(0)); 
        });

    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtWorkType), null);
    hcbLargerWork = new HyperCB(cbLargerWork, ctDropDownList, new StandardPopulator(hdtWork), null);    
    
    btnWorldCat.setOnAction(event -> 
    {
      if (htAuthors.getID(1, 0) > 0)
        searchWorldCat(db.persons.getByID(htAuthors.getID(1, 0)).getLastName(), tfTitle.getText(), tfYear.getText());
      else
        searchWorldCat("", tfTitle.getText(), tfYear.getText()); 
    });
    
    btnScholar.setOnAction(event -> btnScholarClick());
    
    btnDOI.setOnAction(event -> searchDOI(tfDOI.getText()));
    
    btnBibManager.setOnAction(event -> ui.goToWorkInBibManager(curWork));
    
    btnStop.setOnAction(event -> httpClient.stop());
    
    mnuFindDOIonCrossref.setOnAction(event -> retrieveBibData(true, ""));
    mnuFindISBNonGoogleBooks.setOnAction(event -> retrieveBibData(false, ""));
    
    mnuGoogle.setOnAction(event -> 
    {
      if (tfDOI.getText().length() > 0)
        searchGoogle("doi:" + tfDOI.getText(), false);
    });
    
    mnuCrossref.setOnAction(event -> retrieveBibData(true, tfDOI.getText()));
    
    mnuShowMetadata.setOnAction(event -> extractBibDataFromPdf());
    
    btnUseDOI.setOnAction(event -> useDOIClick());
    btnUseISBN.setOnAction(event -> useISBNClick());
    
    btnLaunch.setOnAction(event -> curWork.launch(getCurPageNum(curWork, null, true)));
    
    btnOpenLink.setOnAction(event -> openWebLink(tfLink.getText()));
    
    btnLargerWork.setOnAction(event -> 
    {
      if (inNormalMode)
        ui.goToRecord(HyperTableCell.getRecord(hcbLargerWork.selectedHTC()), true);
      else
        moveAllFiles();
    });
           
    initArgContextMenu();
    btnTree.setOnAction(event -> ui.goToTreeRecord(curWork));
       
    cbType.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {    
      if (newValue == null) return;
      
      WorkTypeEnum workTypeEnumVal = HDT_WorkType.workTypeIDToEnumVal(HyperTableCell.getCellID(newValue));
      WorkTypeEnum oldEnumVal = curWork.getWorkTypeValue();
      
      if (workTypeEnumVal != wtUnenteredSet)
      {
        if (oldEnumVal == wtUnenteredSet) 
        {
          messageDialog("You cannot change the work type after it has been set to Unentered Set of Work Files.", mtError);
          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }
        
        changeToNormalMode();
      }      
      else
      {
        if ((oldEnumVal != wtUnenteredSet) && (oldEnumVal != wtNone))
        {
          messageDialog("You cannot change a work with an existing work type into an unentered set of work files.", mtError);
          Platform.runLater(() -> cbType.setValue(oldValue));
          return;
        }

        changeToUnenteredSetMode();
      }
    });

    Divider div1 = spHoriz1.getDividers().get(0);
    Divider div2 = spHoriz2.getDividers().get(0);
    
    div1.positionProperty().bindBidirectional(div2.positionProperty());
    
    btnFolderAdded = false;
    btnFolder = new SplitMenuButton();
    btnFolder.setText("Folder:");
    
    EventHandler<ActionEvent> handler = event ->
    {
      if (tfLink.getText().length() > 0)
        if (tfLink.getText().charAt(0) != '(')
          launchFile(new FilePath(tfLink.getText()));
    };
    
    btnFolder.setOnAction(handler);
    addFolderMenuItem("Show in system explorer", handler); 

    addFolderMenuItem("Show in file manager", event ->
    {
      if (tfLink.getText().length() > 0)
        if (tfLink.getText().charAt(0) != '(')
          ui.goToFileInManager(new FilePath(tfLink.getText()));
    });
    
    lblSearchKey.setTooltip(new Tooltip("Regenerate search key"));
    
    lblSearchKey.setOnMouseClicked(event -> lblSearchKeyClick());
    
    lblTitle.setTooltip(new Tooltip("Reformat title"));
    
    lblTitle.setOnMouseClicked(event ->
    {
      String title = tfTitle.getText();
      
      title = titleCase(convertToSingleLine(ultraTrim(title)));
      
      alreadyChangingTitle = true;
      tfTitle.setText(title);
      alreadyChangingTitle = false;
      
      safeFocus(tfTitle);
    });
   
    UnaryOperator<TextFormatter.Change> filter = (change) ->
    {
      if (alreadyChangingTitle) return change;
      
      if (change.getText().length() > 1)
      {
        alreadyChangingTitle = true;
        String newText = change.getControlNewText();
        change.setRange(0, change.getControlText().length());
        change.setText(ultraTrim(titleCase(convertToSingleLine(newText))));
        alreadyChangingTitle = false;  
      }
      
      return change;
    };
         
    tfTitle.setTextFormatter(new TextFormatter<>(filter));

    crossrefBD.addListener((observable, oldBD, newBD) -> updateMergeButton());
    pdfBD     .addListener((observable, oldBD, newBD) -> updateMergeButton());
    googleBD  .addListener((observable, oldBD, newBD) -> updateMergeButton());
    
    taMiscBib    .textProperty().addListener((obs, ov, nv) -> updateBibButtons());
    taPdfMetadata.textProperty().addListener((obs, ov, nv) -> updateBibButtons());
    taCrossref   .textProperty().addListener((obs, ov, nv) -> updateBibButtons());
    taGoogleBooks.textProperty().addListener((obs, ov, nv) -> updateBibButtons());
    
    tpBib.getSelectionModel().selectedItemProperty().addListener((obs, ov, nv) -> updateBibButtons());
    
    tabPdfMetadata.setOnClosed(event -> { taPdfMetadata.clear(); pdfBD     .set(null); });
    tabCrossref   .setOnClosed(event -> { taCrossref   .clear(); crossrefBD.set(null); });
    tabGoogleBooks.setOnClosed(event -> { taGoogleBooks.clear(); googleBD  .set(null); });
    
    btnMergeBib.setOnAction(event -> btnMergeBibClick());
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void updateMergeButton()
  {
    boolean disabled = true;
    
    if (crossrefBD.get() != null) disabled = false;
    if (pdfBD.get() != null)      disabled = false;
    if (googleBD.get() != null)   disabled = false;
    
    btnMergeBib.setDisable(disabled);
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static HDT_Person otherPersonToUse(String text)
  {
    HDT_Person otherPerson = HDT_Person.lookUpByName(new PersonName(text));
    
    if (otherPerson == null) return null;

    if (confirmDialog(otherPerson.getNameLastFirst(false) + " is an existing person record in the database. Use existing record?"))
      return otherPerson;
    
    return null;
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void btnScholarClick()
  {
    if (htAuthors.getID(1, 0) > 0)
      searchScholar(db.persons.getByID(htAuthors.getID(1, 0)).getLastName(), tfTitle.getText(), "");
    else
      searchScholar("", tfTitle.getText(), "");
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public int getCurPageNum(HDT_Work work, HDT_WorkFile workFile, boolean isStart)
  {
    if ((curWork == null) || (curWork != work)) return -1;
    if (curWork.workFiles.isEmpty()) return -1;
    
    if (workFile == null)
      workFile = curWork.workFiles.get(0);
    
    for (int rowNdx = 0; rowNdx < htWorkFiles.getDataRowCount(); rowNdx++)
    {      
      HyperTableRow row = htWorkFiles.getRowByRowNdx(rowNdx);
      
      if (workFile == row.getRecord())
      {
        int pageNum;
        
        if (isStart)
          pageNum = parseInt(row.getText(3), -1);
        else
          pageNum = parseInt(row.getText(4), -1);
        
        if (pageNum < 0) return -1;
        return pageNum;
      }
    }
    
    return -1;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void setPageNum(HDT_WorkFile workFile, int val, boolean isStart)
  {
    if (curWork == null) return;
    if (curWork.workFiles.isEmpty()) return;
    if (workFile == null) return;
    
    String str = val < 0 ? "" : String.valueOf(val);
    
    for (int rowNdx = 0; rowNdx < htWorkFiles.getDataRowCount(); rowNdx++)
    {      
      HyperTableRow row = htWorkFiles.getRowByRowNdx(rowNdx);
      
      if (workFile == row.getRecord())
      {
        if (isStart)
          htWorkFiles.setDataItem(3, row, workFile.getID(), str, hdtWorkFile);
        else
          htWorkFiles.setDataItem(4, row, workFile.getID(), str, hdtWorkFile);
      }
    }    
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public boolean update()
  {
    int ndx;
    
    int workFileCnt = 0, miscFileCnt = 0, subworkCnt = 0, invCnt = 0, argCnt = 0, mentionerCnt = 0;
    
    if (db.isLoaded() == false)
    {
      previewWindow.clearPreview(pvsWorkTab);
      return false;
    }
    
    clear();

    if ((curWork != lastWork) || (curWork == null))
      htWorkFiles.tv.getSortOrder().clear();
       
    if (curWork == null)
    {
      enable(false);
      previewWindow.clearPreview(pvsWorkTab);
      bibManagerDlg.workRecordToAssign.set(null);
      return false;
    }
    
    btnTree.setDisable(ui.getTree().getRowsForRecord(curWork).size() == 0);
    
    WorkTypeEnum workTypeEnumVal = curWork.getWorkTypeValue();
    
    if (workTypeEnumVal == wtUnenteredSet)
      changeToUnenteredSetMode();
    else
    {
      changeToNormalMode();
      tfYear.setText(curWork.getYear());
    }
    
    alreadyChangingTitle = true;
    tfTitle.setText(curWork.name());
    alreadyChangingTitle = false;
    
    taMiscBib.setText(curWork.getMiscBib());
    tfDOI.setText(curWork.getDOI());
    tfSearchKey.setText(curWork.getSearchKey());
    tfLink.setText(curWork.getWebLink());
       
    List<String> isbns = curWork.getISBNs();
    ndx = 0; for (String isbn : isbns)
    {
      htISBN.setDataItem(0, ndx, -1, isbn, hdtNone);
      ndx++;
    }
    
    mainText.loadFromRecord(curWork, true, getView().getTextInfo());
    
    if (curWork.workType.isNotNull())
    {
      hcbType.addEntry(curWork.workType.getID(), curWork.workType.get().name(), curWork.workType.getID());     
      ui.tabWorks.setGraphic(getImageViewForRelativePath(ui.getGraphicRelativePath(curWork)));
    }
       
  // Populate authors and investigations
  // -----------------------------------

    ndx = 0; for (Author author : curWork.getAuthors())
    {
      HDT_Person authorRecord = author.getPerson();
      
      if (authorRecord == null)
      {
        Populator pop = htAuthors.getPopulator(1);
        pop.populate(null, false);
        pop.addEntry(null, -1, author.getNameLastFirst());
        htAuthors.setDataItem(1, ndx, -1, author.getNameLastFirst(), hdtPerson);
      }
      else       
        htAuthors.setDataItem(1, ndx, authorRecord.getID(), authorRecord.listName(), hdtPerson);

      htAuthors.setCheckboxValue(2, ndx, author.getIsEditor());
      htAuthors.setCheckboxValue(3, ndx, author.getIsTrans());

      ndx++;
    }
    
    ndx = 0; for (HDT_Investigation inv : curWork.investigations)
    {     
      if (inv.person.isNotNull())
        htInvestigations.setDataItem(1, ndx, inv.person.getID(), inv.person.get().listName(), hdtPerson);
      
      htInvestigations.setDataItem(2, ndx, inv.getID(), inv.listName(), hdtInvestigation);
      ndx++; invCnt++;
    }

  // Populate Labels
  // ------------------

    ndx = 0; for (HDT_WorkLabel label : curWork.labels)
    {
      htLabels.setDataItem(2, ndx, label.getID(), label.getExtendedText(), hdtWorkLabel);
      ndx++;
    }

  // Populate works
  // ----------------------

    if (curWork.largerWork.isNotNull())
      hcbLargerWork.addEntry(curWork.largerWork.getID(), curWork.largerWork.get().getCBText(), curWork.largerWork.getID());

    for (HDT_Work subWork : curWork.subWorks)
    {      
      if (subWork.authorRecords.size() > 0)
        htSubworks.setDataItem(0, subworkCnt, subWork.authorRecords.get(0).getID(), subWork.getLongAuthorsStr(true), hdtPerson);
      else
        htSubworks.setDataItem(0, subworkCnt, subWork.getID(), subWork.getLongAuthorsStr(true), hdtWork);
      
      htSubworks.setDataItem(1, subworkCnt, subWork.getID(), subWork.name(), hdtWork);
      htSubworks.setDataItem(2, subworkCnt, subWork.getID(), subWork.getYear(), hdtWork, HyperCellSortMethod.hsmNumeric);
      
      subworkCnt++;
    }

  // Populate arguments
  // ------------------

    for (HDT_Argument arg : curWork.arguments)
    {
      if (arg.positions.size() > 0)
      {
        HDT_Position position = arg.positions.get(0);
        htArguments.setDataItem(0, argCnt, position.getID(), position.listName(), hdtPosition);
      
        HDT_PositionVerdict verdict = arg.getPosVerdict(position);
        if (verdict != null)
          htArguments.setDataItem(1, argCnt, verdict.getID(), verdict.listName(), hdtPositionVerdict);
      }
      
      htArguments.setDataItem(2, argCnt, arg.getID(), arg.listName(), hdtArgument);
      argCnt++;
    }

  // Populate work files
  // -------------------

    workFileCnt = refreshFiles();
       
  // Populate miscellaneous files
  // ----------------------------

    for (HDT_MiscFile miscFile : curWork.miscFiles)
    {
      htMiscFiles.setDataItem(1, miscFileCnt, miscFile.getID(), miscFile.name(), hdtMiscFile);
      miscFileCnt++;
    }

  // Populate key mentioners
  // -----------------------
    
    mentionerCnt = populateDisplayersAndKeyMentioners(curWork, htKeyMentioners);
       
  // Other stuff
  // -----------
    
    setTabCaption(tabWorkFiles, workFileCnt);
    setTabCaption(tabSubworks, subworkCnt);
    setTabCaption(tabMiscFiles, miscFileCnt);
    setTabCaption(tabInvestigations, invCnt);
    setTabCaption(tabArguments, argCnt);
    setTabCaption(tabKeyMentions, mentionerCnt);
        
    tfSearchKey.setText(curWork.getSearchKey());
    if (tfSearchKey.getText().length() == 0)
      if (curWork.getYear().length() > 0)
        if (curWork.authorRecords.size() > 0)
          if (curWork.authorRecords.get(0).getLastName().length() > 0)
            tfSearchKey.setText(makeWorkSearchKey(curWork.getAuthors(), curWork.getYear(), curWork));
   
    FilePath filePath = curWork.getPath().getFilePath();
    boolean updatePreview = true;
    
    if (curWork == lastWork)
    {
      ArrayList<TableColumn<HyperTableRow, ?>> list = new ArrayList<>();
      list.addAll(htWorkFiles.tv.getSortOrder());
      
      htWorkFiles.tv.getSortOrder().clear();
      htWorkFiles.tv.getSortOrder().addAll(list);
     
      updatePreview = false;
      
      if (FilePath.isEmpty(filePath) == false)
        if (filePath.equals(previewWindow.getFilePath(pvsWorkTab)) == false)
          updatePreview = true;
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
      else if (invCnt > 0)
        tabPane.getSelectionModel().select(tabInvestigations);
      else if (tabPane.getSelectionModel().getSelectedItem() != tabBibDetails)
        tabPane.getSelectionModel().select(tabWorkFiles);
            
      if (FilePath.isEmpty(filePath) == false)
        if (filePath.equals(previewWindow.getFilePath(pvsWorkTab)))
          if (curWork.getStartPageNum() < 2)
            updatePreview = false;
    }
    
    if ((argCnt > 0) || (mentionerCnt == 0))
      lowerTabPane.getSelectionModel().select(tabArguments);
    else
      lowerTabPane.getSelectionModel().select(tabKeyMentions);
    
    if (curWork.getBibEntryKey().length() > 0)
    {
      ImageView iv = getImageViewForRelativePath("resources/images/card-catalog.png");
      iv.setFitWidth(16);
      iv.setFitHeight(16);
      btnBibManager.setGraphic(iv);
      btnBibManager.setTooltip(new Tooltip("Go to external bibliography manager entry"));
    }
    
    if (updatePreview)
      previewWindow.setPreview(pvsWorkTab, filePath, curWork.getStartPageNum(), curWork.getEndPageNum(), curWork);
    else
      previewWindow.refreshControls(pvsWorkTab);
    
    lastWork = curWork;
    
    safeFocus(tfTitle);    
         
    return true; 
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public int refreshFiles()
  {
    HDT_Folder folder = null;
    boolean notInSame = false;
    int workFileCnt = 0;
    
    int ndx = 0; for (HDT_WorkFile file : curWork.workFiles)
    {           
      if (file.getPath().getParentFolder() != null)
      {
        if (folder == null)
          folder = file.getPath().getParentFolder();
        else if (folder != file.getPath().getParentFolder())
          notInSame = true;
      }
      
      htWorkFiles.setCheckboxValue(1, ndx, file.getAnnotated());
      htWorkFiles.setDataItem(2, ndx, file.getID(), file.getPath().getNameStr(), hdtWorkFile);
      
      int pageNum = curWork.getStartPageNum(file);
      if (pageNum > -1)      
        htWorkFiles.setDataItem(3, ndx, file.getID(), String.valueOf(pageNum), hdtWorkFile);
      
      pageNum = curWork.getEndPageNum(file);
      if (pageNum > -1)
        htWorkFiles.setDataItem(4, ndx, file.getID(), String.valueOf(pageNum), hdtWorkFile);
      
      htWorkFiles.setDataItem(5, ndx, file.getID(), file.name(), hdtWorkFile);
      ndx++; workFileCnt++;
      
      file.viewNow();
    }
      
    if (inNormalMode == false)
    {
      if (notInSame)
        tfLink.setText("(The files are located in multiple folders.)");
      else if (folder != null)
        tfLink.setText(folder.getPath().getFilePath().toString());
    }

    return workFileCnt;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static int populateDisplayersAndKeyMentioners(HDT_RecordWithPath record, HyperTable hyperTable)
  {
    Set<HDT_RecordWithConnector> set = db.getKeyWorkMentioners(record);
    int mentionerCnt = 0;
    
    if (record.hasMainText())
    {    
      Set<MainText> displayers = db.getDisplayers(HDT_RecordWithConnector.class.cast(record).getMainText());
      
      for (MainText displayerText : displayers)
      {
        if (set == null)
          set = new HashSet<>();
        
        set.add(displayerText.getRecord());
      }
    }
    
    if (set == null) return 0;
    
    for (HDT_RecordWithConnector mentioner : set)
    {
      String typeStr = "", name;
           
      if (mentioner.getType() == hdtHub)
      {          
        StrongLink link = HDT_Hub.class.cast(mentioner).getLink();
        if (link.getDebate() != null)   typeStr = typeStr + (typeStr.length() == 0 ? "" : ", ") + db.getTypeName(hdtDebate);
        if (link.getPosition() != null) typeStr = typeStr + (typeStr.length() == 0 ? "" : ", ") + db.getTypeName(hdtPosition);
        if (link.getNote() != null)     typeStr = typeStr + (typeStr.length() == 0 ? "" : ", ") + db.getTypeName(hdtNote);
        if (link.getConcept() != null)  typeStr = typeStr + (typeStr.length() == 0 ? "" : ", ") + db.getTypeName(hdtTerm);
        
        if (link.getConcept() != null)       name = link.getConcept().getCBText();
        else if (link.getDebate() != null)   name = link.getDebate().getCBText();
        else if (link.getPosition() != null) name = link.getPosition().getCBText();
        else                                 name = link.getNote().getCBText();
      }
      else
      {
        typeStr = db.getTypeName(mentioner.getType());
        name = mentioner.getCBText();
      }           
      
      hyperTable.setDataItem(0, mentionerCnt, mentioner.getID(), typeStr, mentioner.getType());
      hyperTable.setDataItem(1, mentionerCnt, mentioner.getID(), name, mentioner.getType());
      hyperTable.setDataItem(2, mentionerCnt, mentioner.getID(), mentioner.getMainText().getPlainForDisplay(), mentioner.getType());
      mentionerCnt++;
    }
    
    return mentionerCnt;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  private void changeToNormalMode()
  {
    if (inNormalMode) return;
    
    tfYear.setDisable(false);
    btnNewChapter.setText("New Chapter");
    cbLargerWork.setDisable(false);
    cbLargerWork.setVisible(true);
    GridPane.setColumnSpan(apLowerMid, 1);
    apLowerRight.setVisible(true);
    btnLargerWork.setText("Larger Work:");
    
    btnFolder.setVisible(false);
    btnOpenLink.setVisible(true);
    
    tfLink.setEditable(true);
    AnchorPane.setLeftAnchor(btnOpenLink, btnOpenLinkLeftAnchor);
    AnchorPane.setLeftAnchor(tfLink, tfLinkLeftAnchor);
    AnchorPane.setRightAnchor(tfLink, tfLinkRightAnchor);
    
    apLowerMid.getChildren().remove(tfLink);
    apLowerRight.getChildren().add(tfLink);
    
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
    
    btnOpenLinkLeftAnchor = AnchorPane.getLeftAnchor(btnOpenLink);
    tfLinkLeftAnchor = AnchorPane.getLeftAnchor(tfLink);
    tfLinkRightAnchor = AnchorPane.getRightAnchor(tfLink);
    
    tfYear.setDisable(true);
    cbLargerWork.setDisable(true);
    cbLargerWork.setVisible(false);
    btnNewChapter.setText("Add Multiple Files");
    apLowerRight.setVisible(false);
    GridPane.setColumnSpan(apLowerMid, GridPane.REMAINING);
    btnLargerWork.setText("Move All Files");
    
    if (btnFolderAdded == false)
    {     
      apLowerMid.getChildren().add(btnFolder);
      btnFolderAdded = true;
    }
    
    tfLink.setEditable(false);

    apLowerRight.getChildren().remove(tfLink);
    
    apLowerMid.getChildren().add(tfLink);

    btnOpenLink.setVisible(false);
    btnFolder.setVisible(true);
    
    Platform.runLater(() ->
    {
      AnchorPane.setLeftAnchor(btnFolder, btnLargerWork.getBoundsInParent().getMaxX() + 4.0);
      AnchorPane.setLeftAnchor(tfLink, btnLargerWork.getBoundsInParent().getMaxX() + 4.0 + btnFolder.getWidth() + 4.0);
      AnchorPane.setRightAnchor(tfLink, 2.0);
      AnchorPane.setTopAnchor(btnFolder, AnchorPane.getTopAnchor(btnOpenLink));
      btnFolder.setPrefHeight(btnOpenLink.getPrefHeight());
    });
    
    inNormalMode = false;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void initArgContextMenu()
  {
    htArguments.addContextMenuItem(hdtArgument, "Argument Record...", 
      record -> ui.goToRecord(record, true));    
       
    htArguments.addCondContextMenuItem(hdtArgument, "Position Record...", 
      record -> (((HDT_Argument)record).positions.size() > 0), 
      record -> ui.goToRecord(((HDT_Argument)record).positions.get(0), true));
     
    htArguments.addCondContextMenuItem(hdtArgument, "Debate Record...", 
        
      record ->
      {
        if (((HDT_Argument)record).positions.size() > 0)
          return (((HDT_Argument)record).positions.get(0).getDebate() != null); 
        return false;
      },
      
      record -> ui.goToRecord(((HDT_Argument)record).positions.get(0).getDebate(), true));
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  private void moveFileToNewWork(HDT_WorkFile workFile)
  {
    HDT_Work newWork, oldWork;
    
    if (ui.cantSaveRecord(true)) return;    
    
    oldWork = curWork;
    newWork = db.createNewBlankRecord(hdtWork);
   
    int oldNdx = curWork.workFiles.indexOf(workFile);
    
    int startPage = getCurPageNum(curWork, workFile, true),
        endPage = getCurPageNum(curWork, workFile, false);
    
    newWork.addWorkFile(workFile.getID(), false, false);
    
    curWork.getAuthors().forEach(author -> newWork.getAuthors().add(author));
    
    newWork.setStartPageNum(workFile, startPage);
    newWork.setEndPageNum(workFile, endPage);
    
    ui.goToRecord(newWork, false);
    
    if (showWorkDialog(workFile) == false)
    {
      db.getObjectList(rtWorkFileOfWork, oldWork, true).add(oldNdx, workFile);
      
      ui.deleteCurrentRecord(false);
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void moveFileToDifferentWork(HDT_WorkFile workFile)
  {   
    if (ui.cantSaveRecord(true)) return;

    HDT_Work newWork, oldWork;
    HDT_Person author = null;
    
    if (curWork.authorRecords.size() > 0)
      author = curWork.authorRecords.get(0);
    
    SelectWorkDialogController dlg = SelectWorkDialogController.create("Select a work record", author);
    
    if (dlg.showModal())
    {
      newWork = dlg.getWork();
      oldWork = curWork;
      
      int oldNdx = curWork.workFiles.indexOf(workFile);
      
      int startPage = getCurPageNum(curWork, workFile, true),
          endPage = getCurPageNum(curWork, workFile, false);
      
      newWork.addWorkFile(workFile.getID(), false, false);      
      
      newWork.setStartPageNum(workFile, startPage);
      newWork.setEndPageNum(workFile, endPage);
      
      ui.goToRecord(newWork, false);
      
      if (showWorkDialog(workFile) == false)
      {
        db.getObjectList(rtWorkFileOfWork, oldWork, true).add(oldNdx, workFile);
        db.getObjectList(rtWorkFileOfWork, newWork, true).remove(workFile);
        
        ui.btnBackClick();
      }
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void moveAllFiles()
  {   
    if (ui.cantSaveRecord(true)) return;
    
    if (curWork.workFiles.size() == 0)
    {
      messageDialog("There are no files to move.", mtError);
      return;
    }
    
    FilePathSet files = new FilePathSet();
    
    curWork.workFiles.forEach(workFile -> files.add(workFile.getPath().getFilePath()));
       
    MutableBoolean allSame = new MutableBoolean();
    FilePath folder = pickDirectory(true, files, allSame);
    
    if (folder == null) return;
    
    if (allSame.isTrue())
    {
      messageDialog("All of the files are already located in the destination folder.", mtError);
      return;
    }
    
    HDT_Folder folderRecord;
    
    boolean startWatcher = folderTreeWatcher.stop();
    
    try
    {
      folderRecord = HyperPath.getFolderFromFilePath(folder, true);
      
      for (HDT_WorkFile workFile : curWork.workFiles)
        if (workFile.getPath().moveToFolder(folderRecord.getID(), true, false, "") == false) break;
    }
    catch (IOException e)
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
    HDT_Work newWork;

    if (ui.cantSaveRecord(true)) return;
    
    if (curWork.getWorkTypeValue() == wtUnenteredSet)
    {
      addMultipleFiles();
      return;
    }
    
    newWork = db.createNewBlankRecord(hdtWork);

    db.getObjectList(rtWorkFileOfWork, newWork, true).addAll(curWork.workFiles);
    
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

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Adobe PDF file (*.pdf)", "*.pdf"));       
    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));
    
    fileChooser.setInitialDirectory(db.getPath(PREF_KEY_UNENTERED_PATH, null).toFile());
    
    List<File> files = fileChooser.showOpenMultipleDialog(app.getPrimaryStage());
    
    if (files == null) return;
    
    FilePathSet filePaths = new FilePathSet();
    
    files.forEach(file -> filePaths.add(new FilePath(file)));
     
    for (FilePath filePath : filePaths)
    {
      if (filePath.isDirectory())
      {
        messageDialog("One of the selected files is a directory.", mtError);
        return;
      }
      
      HDT_RecordWithPath fileRecord = HyperPath.getFileFromFilePath(filePath);
      
      if (fileRecord != null)
      {
        if (fileRecord instanceof HDT_MiscFile)
          messageDialog("The file: " + filePath + " is already in use as a miscellaneous file, record ID: " + fileRecord.getID(), mtError);
        else
          messageDialog("The file: " + filePath + " is already assigned to the work record with ID: " + HDT_WorkFile.class.cast(fileRecord).works.get(0).getID(), mtError);
        
        return;
      }
    }
    
    MutableBoolean allSame = new MutableBoolean();
    
    FilePath folder = pickDirectory(false, filePaths, allSame);
    
    if (folder == null) return;
    
    DialogResult moveOrCopy = mrCopy;
    
    if (allSame.booleanValue() == false)
    {
      PopupDialog dlg = new PopupDialog("Should the files be moved or copied from their present location?");
      
      dlg.addButton("Move", mrMove);
      dlg.addButton("Copy", mrCopy);
      
      moveOrCopy = dlg.showModal();
    }

    try
    {
      for (FilePath srcFilePath : filePaths)
      {
        if (moveOrCopy == mrMove)
        {
          if (srcFilePath.canObtainLock() == false)
          {
            messageDialog("Unable to obtain lock on path: \"" + srcFilePath + "\"", mtError);
            return;
          }
        }
        
        FilePath destFilePath = folder.getDirOnly().resolve(srcFilePath.getNameOnly());
        
        if (destFilePath.canObtainLock() == false)
        {
          messageDialog("Unable to obtain lock on path: \"" + destFilePath + "\"", mtError);
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
      
      HDT_WorkFile workFile;
      
      workFile = (HDT_WorkFile) HyperPath.createRecordAssignedToPath(hdtWorkFile, destFilePath);
      if (workFile == null)
      {
        messageDialog("Internal error #67830", mtError);
        ui.update();
        fileManagerDlg.setNeedRefresh();
        
        if (startWatcher)
          folderTreeWatcher.createNewWatcherAndStart();

        return;
      }
      
      curWork.addWorkFile(workFile.getID(), true, true);
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
    
    FilePath destPath;
    
    if (curWork.workFiles.size() > 0)
      destPath = curWork.getPath().getFilePath().getDirOnly();
    else
      destPath = db.getPath(PREF_KEY_UNENTERED_PATH, null);
    
    FilePath folder = null;
    HDT_Folder folderRecord = null;
    
    while (folderRecord == null)
    {
      folder = destPath;
      
      if (moveOnly)
        dirChooser.setTitle("Select location to move files");
      else
        dirChooser.setTitle("Select location to move or copy files");
      
      if (folder.exists() && folder.isDirectory())
        dirChooser.setInitialDirectory(folder.toFile());
      else
      {
        folder = db.getPath(PREF_KEY_UNENTERED_PATH, null);
        if (folder.exists() && folder.isDirectory())
          dirChooser.setInitialDirectory(folder.toFile());
      }
      
      folder = new FilePath(dirChooser.showDialog(app.getPrimaryStage()));
  
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
    btnUseDOI.setDisable(true);
    btnUseISBN.setDisable(true);
    btnMergeBib.setDisable(true);
    
    tfYear.setText("");
    
    taMiscBib.clear();
    disableCache(taMiscBib);
    pdfBD.set(null);  
    crossrefBD.set(null);
    googleBD.set(null);    

    tpBib.getTabs().remove(tabCrossref);
    tpBib.getTabs().remove(tabPdfMetadata);
    tpBib.getTabs().remove(tabGoogleBooks);
    taCrossref.clear();
    taPdfMetadata.clear();
    taGoogleBooks.clear();
    disableCache(taCrossref);
    disableCache(taPdfMetadata);
    disableCache(taGoogleBooks);
    
    httpClient.stop();
    
    tfDOI.setText("");
    htISBN.clear();
    
    alreadyChangingTitle = true;
    tfTitle.setText("");
    alreadyChangingTitle = false;

    tfSearchKey.setText("");
    tfLink.setText("");

    htAuthors.clear();
    htLabels.clear();
    htSubworks.clear();
    htInvestigations.clear();
    htArguments.clear();
    htMiscFiles.clear();
    htWorkFiles.clearKeepSortOrder();
    htKeyMentioners.clear();

    tabPane.getTabs().forEach(tab -> tab.setText(tabCaptions.get(tab)));
    
    hcbType.clear();
    hcbLargerWork.clear();

    mainText.clear(true);
    
    changeToNormalMode();
    
    ui.tabWorks.setGraphic(getImageViewForRelativePath(ui.getGraphicRelativePathByType(hdtWork)));
    
    if (db.bibLibraryIsLinked())
    {
      ImageView iv = getImageViewForRelativePath("resources/images/card-catalog_tr.png");
      iv.setFitWidth(16);
      iv.setFitHeight(16);
      
      btnBibManager.setVisible(true);
      btnBibManager.setGraphic(iv);
      btnBibManager.setTooltip(new Tooltip("Assign to external bibliography manager entry"));
    }
    else
      btnBibManager.setVisible(false);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public static String makeWorkSearchKey(Iterable<Author> authors, String year, HDT_Work work)
  {
    String key = "";
        
    for (Author author : authors)
    {
      if ((author.getIsEditor() == false) && (author.getIsTrans() == false))
        if (author.getPerson() != null)
        {
          key = makeWorkSearchKey(author, year, work);
          if (key.length() > 0) return key;
        }
    }

    for (Author author : authors)
    {
      if (author.getIsTrans() == false)
        if (author.getPerson() != null)
        {
          key = makeWorkSearchKey(author, year, work);
          if (key.length() > 0) return key;
        }
    }

    for (Author author : authors)
    {
      if ((author.getIsEditor() == false) && (author.getIsTrans() == false))
        if (author.getPerson() == null)
        {
          key = makeWorkSearchKey(author, year, work);
          if (key.length() > 0) return key;
        }
    }
    
    for (Author author : authors)
    {
      if (author.getIsTrans() == false)
        if (author.getPerson() == null)
        {
          key = makeWorkSearchKey(author, year, work);
          if (key.length() > 0) return key;
        }
    }
    
    for (Author author : authors)
    {
      if (author.getPerson() != null)
      {
        key = makeWorkSearchKey(author, year, work);
        if (key.length() > 0) return key;
      }
    }
    
    for (Author author : authors)
    {
      if (author.getPerson() == null)
      {
        key = makeWorkSearchKey(author, year, work);
        if (key.length() > 0) return key;
      }
    }

    return "";
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static String makeWorkSearchKey(Author author, String year, HDT_Work work)
  {    
    if (author.getLastName().length() > 0)
      return makeWorkSearchKey(author.getLastName(), year, work);
    else
      return makeWorkSearchKey(author.getFirstName(), year, work);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static String makeWorkSearchKey(String name, String year, HDT_Work work)
  {
    String searchKey = "";
    boolean keyTaken;
    char keyLetter = ' ';
    
    if ((name.length() == 0) || (year.length() == 0))
      return "";
      
    searchKey = name + " " + year;

    do
    {
      SearchKeyword hyperKey = db.getKeyByKeyword((searchKey + keyLetter).trim());
      keyTaken = false;
      
      if (hyperKey != null)
      {
        if (hyperKey.record != work)
        {
          keyTaken = true;
          
          if (keyLetter == 'z') return "";
          
          keyLetter = (keyLetter == ' ' ? 'a' : (char)(keyLetter + 1));
        }  
      }
    } while (keyTaken);
      
    searchKey = (searchKey + keyLetter).trim();
    
    return SearchKeys.prepSearchKey(searchKey);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void lblSearchKeyClick()
  {
    HashMap<Integer, Tag> map = new HashMap<>();

    map.put(-1, tagInFileName);
    map.put(2, tagEditor);
    map.put(3, tagTranslator);
        
    List<ObjectGroup> tableGroups  = htAuthors.getObjectGroupList(curWork, rtAuthorOfWork, 1, map);
    
    List<Author> authList = Authors.getListFromObjectGroups(tableGroups, curWork);
    
    tfSearchKey.setText(makeWorkSearchKey(authList, tfYear.getText(), curWork));
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void saveISBNs()
  {
    ArrayList<String> isbns = new ArrayList<>();
    for (int rowNdx = 0; rowNdx < htISBN.getDataRowCount(); rowNdx++)
      isbns.add(htISBN.getText(0, rowNdx));
    
    curWork.setISBNs(isbns);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public boolean saveToRecord(boolean showMessage)
  {   
    WorkTypeEnum workTypeEnumVal = HDT_WorkType.workTypeIDToEnumVal(hcbType.selectedID());
    
    if (tfSearchKey.getText().length() == 0)
      lblSearchKeyClick();
    else if (curWork.getYear().length() > 0)
    {
      if (tfSearchKey.getText().contains(curWork.getYear()))
        if (workTypeEnumVal != wtUnenteredSet)
          if (tfYear.getText().equals(curWork.getYear()) == false)
            if (confirmDialog("Year has been modified. Update search key?"))
              tfSearchKey.setText(tfSearchKey.getText().replace(curWork.getYear(), tfYear.getText()));
    }
       
    if (!HyperTab.saveSearchKey(curWork, tfSearchKey, showMessage, this)) return false;
    
    curWork.setName(tfTitle.getText());
    curWork.workType.setID(hcbType.selectedID());
    
    for (int rowNdx = 0; rowNdx < htWorkFiles.getDataRowCount(); rowNdx++)
    {      
      HDT_WorkFile file = (HDT_WorkFile) htWorkFiles.getRowByRowNdx(rowNdx).getRecord();
      
      if (file != null)
      {
        file.setAnnotated(htWorkFiles.getCheckboxValue(1, rowNdx));       
        curWork.setStartPageNum(file, parseInt(htWorkFiles.getText(3, rowNdx), -1));
        curWork.setEndPageNum(file, parseInt(htWorkFiles.getText(4, rowNdx), -1));
        file.setName(htWorkFiles.getText(5, rowNdx));        
      }
    }
    
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
    
    if (workTypeEnumVal == wtUnenteredSet)
    {
      curWork.setYear("");
      curWork.setLargerWork(-1, true);
      curWork.setWebLink("");
    }
    else
    {
      curWork.setYear(tfYear.getText());
      curWork.setLargerWork(hcbLargerWork.selectedID(), noIsbnUpdate);
      curWork.setWebLink(tfLink.getText());
    }

    if (needToSaveISBNs)
      saveISBNs();
    
    curWork.setMiscBib(taMiscBib.getText());
    curWork.setDOI(tfDOI.getText());
      
    mainText.save();

    curWork.setAuthors(getAuthorGroups());
    curWork.setInvestigations(htInvestigations);

    curWork.setWorkLabels(htLabels);
       
    return true;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public String getShortAuthorsStr()
  {
    List<ObjectGroup> authGroups = getAuthorGroups();
    ArrayList<Author> authList = new ArrayList<>();
    
    for (ObjectGroup authGroup : authGroups)
    {
      if (authGroup.getPrimary() != null)
        authList.add(new Author(curWork, 
                                HDT_Person.class.cast(authGroup.getPrimary()).getName(), 
                                authGroup.getValue(Tag.tagEditor).bool, 
                                authGroup.getValue(Tag.tagTranslator).bool, 
                                Ternary.True));
      else
        authList.add(new Author(curWork, 
                                new PersonName(authGroup.getPrimaryStr()), 
                                authGroup.getValue(Tag.tagEditor).bool, 
                                authGroup.getValue(Tag.tagTranslator).bool, 
                                Ternary.True)); 
    }
    
    return Authors.getShortAuthorsStr(authList, false, true);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private List<ObjectGroup> getAuthorGroups() { return htAuthors.getAuthorGroups(curWork, 1, -1, 2, 3); }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public void newClick(HDT_RecordType objType, HyperTableRow row)
  {
    switch (objType)
    {
      case hdtMiscFile :
        
        if (ui.cantSaveRecord(true)) return;

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

  public boolean showWorkDialog(HDT_WorkFile workFile)
  {
    return showWorkDialog(workFile, null);
  }
  
  public boolean showWorkDialog(HDT_WorkFile workFile, FilePath filePathToUse)
  {
    boolean result;
    
    if (ui.cantSaveRecord(true)) return false;
    
    if (curWork.getWorkTypeValue() == wtUnenteredSet)
    {
      fdc = FileDialogController.create("Unentered Work File", hdtWorkFile, workFile, curWork, "");
      
      result = fdc.showModal();      
      fdc = null;
      
      if (result == false) return false;
    }
    else
    {
      if ((workFile == null) && (filePathToUse != null))
        wdc = WorkDialogController.create("Import New Work", filePathToUse, this);
      else
        wdc = WorkDialogController.create("Work File", workFile, this);
      
      if (wdc.showModal() == false)
      {
        wdc = null;
        WorkDialogController.httpClient.stop();
        return false;
      }
  
      WorkDialogController.httpClient.stop();
      
      if (wdc.getCreateEntry())
      {
        BibEntry entry = db.getBibLibrary().addEntry(wdc.getEntryType());      
        curWork.setBibEntryKey(entry.getEntryKey());
      }

      curWork.getBibData().copyAllFieldsFrom(wdc.getBibDataFromGUI(), false, false);

      curWork.setAuthors(wdc.htAuthors.getAuthorGroups(curWork, 0, 2, 3, 4));
      
      wdc = null;
    }
    
    ui.update();
    
    return true;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void useDOIClick()
  {
    tfDOI.setText(getDoiFromBibTab());
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private String getDoiFromBibTab()
  {
    if (tpBib.getSelectionModel().getSelectedItem() == tabMiscBib)
      return BibUtils.matchDOI(taMiscBib.getText());

    BibData bd = getBibDataFromBibTab();
    if (bd != null)
      return bd.getStr(bfDOI);
    
    return "";
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void updateBibButtons()
  {
    btnUseDOI.setDisable(getDoiFromBibTab().length() == 0);
    
    List<String> isbns = getIsbnsFromBibTab();
    
    if (isbns == null)
      btnUseISBN.setDisable(true);
    else
      btnUseISBN.setDisable(isbns.size() == 0);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private List<String> getIsbnsFromBibTab()
  {
    if (tpBib.getSelectionModel().getSelectedItem() == tabMiscBib)
      return BibUtils.matchISBN(taMiscBib.getText());

    BibData bd = getBibDataFromBibTab();
    if (bd != null)
      return bd.getMultiStr(bfISBNs);
    
    return new ArrayList<String>();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private BibData getBibDataFromBibTab()
  {
    Tab curTab = tpBib.getSelectionModel().getSelectedItem();
    
    if (curTab == tabPdfMetadata) return pdfBD.get();
    if (curTab == tabCrossref)    return crossrefBD.get();
    if (curTab == tabGoogleBooks) return googleBD.get();
    
    return null;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void useISBNClick()
  {    
    List<String> isbns = getIsbnsFromBibTab();
    if (isbns.size() == 0) return;
    
    int ndx = htISBN.getDataRowCount(); for (String isbn : isbns)
    {
      htISBN.setDataItem(0, ndx, -1, isbn, hdtNone);
      ndx++;
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void addBibLine(String tag, String value, TextArea ta)
  {
    if (value == null) return;
    
    if (ta.getText().length() > 0)
      ta.appendText(System.lineSeparator());
    
    ta.appendText(tag + ": " + value);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void extractBibDataFromPdf()
  {
    ArrayList<FilePath> pdfFilePaths = new ArrayList<>();
    PdfMetadata firstMD = null, lastMD = null, goodMD = null;

    httpClient.stop();
    taPdfMetadata.clear();
    
    if (tpBib.getTabs().contains(tabPdfMetadata) == false)
      tpBib.getTabs().add(tabPdfMetadata);
    
    tpBib.getSelectionModel().select(tabPdfMetadata);
    
    List<String> isbns = new ArrayList<>();
    String doi = "";
    
    if (db.isLoaded() == false) return;
    if (curWork == null) return;

    curWork.workFiles.forEach(workFile ->
    {
      if (workFile.getPath() != null)
        if (workFile.getPath().isEmpty() == false)
          if (workFile.getPath().getFilePath().exists())
            if (getMediaType(workFile.getPath().getFilePath()).toString().contains("pdf"))
              pdfFilePaths.add(workFile.getPath().getFilePath());
    });
    
    if (pdfFilePaths.isEmpty())
    {
      taPdfMetadata.setText("[No PDF file.]");
      return;
    }
        
    try
    {           
      for (FilePath pdfFilePath : pdfFilePaths)
      {
        lastMD = new PdfMetadata();
        
        BibUtils.getPdfMetadata(pdfFilePath, lastMD);
        if (firstMD == null)
          firstMD = lastMD;
                
        if (doi.length() == 0)
        {
          doi = safeStr(lastMD.bd.getStr(bfDOI));
          
          if (doi.length() > 0)
          {
            if (goodMD == null)
              goodMD = lastMD;
          }
        }
        
        List<String> curIsbns = lastMD.bd.getMultiStr(bfISBNs);
        
        if (curIsbns.isEmpty() == false)
        {
          if (isbns.isEmpty())
          {
            if (goodMD == null)
              goodMD = lastMD;
          }
          
          for (String isbn : curIsbns)
            if (isbns.contains(isbn) == false) isbns.add(isbn);
        }
      }
      
      if (goodMD == null)
        goodMD = firstMD;

      pdfBD.set(goodMD.extractBibData());
      
      pdfBD.get().setMultiStr(bfISBNs, isbns);
      
      taPdfMetadata.appendText(pdfBD.get().createReport());
      
      addBibLine("Keywords", goodMD.docInfo.getKeywords(), taPdfMetadata);
      addBibLine("Creator", goodMD.docInfo.getCreator(), taPdfMetadata);
      addBibLine("Producer", goodMD.docInfo.getProducer(), taPdfMetadata);
    }
    catch (IOException | XMPException e)
    {
      taPdfMetadata.setText("[Error: " + e.getMessage() + "]"); 
    }
    
    updateBibButtons();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
 
  private void retrieveBibData(boolean crossref, String industryID)
  {          
    httpClient.stop();
    
    if (db.isLoaded() == false) return;
    if (curWork == null) return;
    
    Tab tab;
    TextArea ta;
    String url = "";

    if (crossref)
    {
      tab = tabCrossref;
      ta = taCrossref;
      url = BibUtils.getCrossrefUrl(curWork, industryID);
    }
    else
    {
      tab = tabGoogleBooks;
      ta = taGoogleBooks;
      url = BibUtils.getGoogleUrl(curWork, industryID);
    }
    
    if (tpBib.getTabs().contains(tab) == false)
      tpBib.getTabs().add(tab);
    
    tpBib.getSelectionModel().select(tab);  
    
    final String finalUrl = url;
    
    ta.clear();
    btnStop.setVisible(true);
    progressBar.setVisible(true);
    
    JsonHttpClient.getObjAsync(url, httpClient, jsonObj ->
    {
      BibData bd = null;
      
      if (crossref)
      {
        bd = BibData.createFromCrossrefJSON(jsonObj);
        crossrefBD.set(bd);
      }
      else
      {
        bd = BibData.createFromGoogleJSON(jsonObj);
        googleBD.set(bd);
      }
      
      ta.setText("Query URL: " + finalUrl + System.lineSeparator());

      btnStop.setVisible(false);
      progressBar.setVisible(false);
      
      if (bd == null)
      {
        ta.appendText("[No results.]");
        return;
      }
      
      ta.appendText(bd.createReport());      
    }, e ->
    {
      btnStop.setVisible(false);
      progressBar.setVisible(false);
      ta.setText("Query URL: " + finalUrl + System.lineSeparator());

      if ((e instanceof ParseException) || (e instanceof TerminateTaskException))
        return;
      
      if (e instanceof UnknownHostException)
        messageDialog("Unable to connect to host: " + e.getMessage(), mtError);
      else
        messageDialog("Error: " + e.getMessage(), mtError);
    });
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void btnMergeBibClick()
  {
    if (ui.cantSaveRecord(true)) return;
    
    MergeWorksDialogController mwd = null;
    boolean creatingNewEntry = false;
    BibData workBibData = curWork.getBibData();
    
    if (db.bibLibraryIsLinked() && (curWork.getBibEntryKey().length() == 0))
    {
      String typeName = db.getBibLibrary().type().getUserReadableName();
      creatingNewEntry = confirmDialog("The current work record is not associated with a " + typeName + " entry. Create one now?");
    }
    
    try
    {
      mwd = MergeWorksDialogController.create("Merge Bibliographic Data", workBibData, 
                                              pdfBD.get(), crossrefBD.get(), googleBD.get(), false, creatingNewEntry);
    }
    catch (IOException e)
    {
      messageDialog("Unable to initialize merge dialog window.", mtError);
      return;
    }
    
    if (mwd.showModal() == false) return;
    
    if (creatingNewEntry)
    {
      BibEntry entry = db.getBibLibrary().addEntry(mwd.getEntryType());      
      curWork.setBibEntryKey(entry.getEntryKey());
      workBibData = entry;
    }
    
    mwd.mergeInto(workBibData);
    bibManagerDlg.refresh();
    update();
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public void setDividerPositions()
  {
    setDividerPosition(spVert, PREF_KEY_WORK_MID_VERT, 0);
    setDividerPosition(spVert, PREF_KEY_WORK_BOTTOM_VERT, 1);
    setDividerPosition(spHoriz1, PREF_KEY_WORK_RIGHT_HORIZ, 0);
  }  

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public void getDividerPositions()
  {
    getDividerPosition(spVert, PREF_KEY_WORK_MID_VERT, 0);
    getDividerPosition(spVert, PREF_KEY_WORK_BOTTOM_VERT, 1);
    getDividerPosition(spHoriz1, PREF_KEY_WORK_RIGHT_HORIZ, 0);
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void getBibDataFromGUI(BibData bd)
  {
    ArrayList<String> isbns = new ArrayList<>();
    for (int rowNdx = 0; rowNdx < htISBN.getDataRowCount(); rowNdx++)
      isbns.add(htISBN.getText(0, rowNdx));
    
    bd.setMultiStr(bfISBNs, isbns);
    
    bd.setTitle(tfTitle.getText());
    bd.setStr(bfYear, tfYear.getText());
    bd.setStr(bfURL, tfLink.getText());
    bd.setStr(bfDOI, tfDOI.getText());
    bd.setWorkType(hcbType.selectedRecord());
    
    bd.setMultiStr(bfMisc, convertMultiLineStrToStrList(taMiscBib.getText(), true));
    
    bd.getAuthors().setAllFromTable(getAuthorGroups());
  }  

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
}
