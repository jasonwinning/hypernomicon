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

package org.hypernomicon.view.tabs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.Exceptions.*;
import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.*;
import org.hypernomicon.dialogs.InvestigationsDlgCtrlr.InvestigationSetting;
import org.hypernomicon.model.authors.*;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.query.personMatch.PersonForDupCheck;
import org.hypernomicon.query.personMatch.PersonMatcher;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.cellValues.BibDateHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.Populator.DisplayKind;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;

import org.hypernomicon.view.wrappers.*;

import static java.util.Collections.*;

import java.io.IOException;
import java.time.Instant;
import java.util.*;
import java.util.function.Predicate;
import java.util.prefs.Preferences;
import java.util.stream.Collectors;

import javafx.application.Platform;
import javafx.concurrent.Worker.State;
import javafx.event.*;
import javafx.fxml.FXML;
import javafx.geometry.Rectangle2D;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.StackPane;

//---------------------------------------------------------------------------

public class PersonTabCtrlr extends HyperTab<HDT_Person, HDT_RecordWithMainText>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private AnchorPane apOverview;
  @FXML private Button btnWebSrch1, btnWebSrch2, btnWebsitePaste, btnOrcidPaste, btnNewWork;
  @FXML private ComboBox<HyperTableCell> cbRank, cbStatus, cbSubfield, cbField;
  @FXML private ImageView ivPerson;
  @FXML private Label lblORCID, lblWebsite, lblPicture, lblSearchKey, lblInvHelp;
  @FXML private SplitMenuButton smbWebSrch1;
  @FXML private SplitPane spTopHoriz, spVert;
  @FXML private Tab tabNew, tabOverview;
  @FXML private TableView<HyperTableRow> tvArguments, tvPersonDept, tvWorks;
  @FXML private TabPane tpPerson;
  @FXML private TextField tfORCID, tfWebsite, tfSearchKey;

  @FXML public TextField tfFirst, tfLast;

  private final List<InvestigationView> invViews = new ArrayList<>();
  private final HyperTable htPersonInst, htWorks, htArguments;
  private final HyperCB hcbRank, hcbStatus, hcbSubfield;
  private final MainTextWrapper mainText;

  public final HyperCB hcbField;

  private static final String TOOLTIP_PREFIX = "Search for this person using ";

  private FilePath curPicture = null;
  private Rectangle2D viewPort = null;
  private HDT_Investigation curInvestigation;
  private HDT_Person curPerson, lastPerson = null;
  private Instant lastArrowKey = Instant.EPOCH;
  private boolean alreadyChangingName = false, alreadyChangingTab = false;

//---------------------------------------------------------------------------

  public FilePath getCurPicture()                { return curPicture; }
  public HDT_Investigation getCurInvestigation() { return curInvestigation; }

  @Override public String recordName()           { return new PersonName(tfFirst.getText(), tfLast.getText()).getLastFirst(); }
  @Override protected RecordType type()          { return hdtPerson; }

//---------------------------------------------------------------------------

  public PersonTabCtrlr(Tab tab) throws IOException
  {
    super(personTabEnum, tab, "view/tabs/PersonTab");

    mainText = new MainTextWrapper(apOverview);

    btnNewWork.setOnAction(event -> ui.importWorkFile(curPerson, null, false));
    setToolTip(btnNewWork, "Create new work record with this person as author");

    htPersonInst = new HyperTable(tvPersonDept, 3, true, TablePrefKey.PERSON_INST);

    htPersonInst.addActionCol(ctGoNewBtn, 2)
      .setTooltip(ButtonAction.baNew, "Add new Institution or Institutional Division here");

    htPersonInst.addCheckboxCol();

    htPersonInst.addColAltPopulatorWithUpdateHandler(hdtInstitution, ctEditableLimitedDropDown, new StandardPopulator(hdtInstitution, InstTabCtrlr.parentPopFilter, DisplayKind.name), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      ((SubjectPopulator)nextPopulator).setObj(row, getRecord(cellVal));
      row.setCellValue(nextColNdx, "", nextPopulator.getRecordType(row));
    });

    htPersonInst.addColAltPopulator(hdtInstitution, ctEditableLimitedDropDown, new SubjectPopulator(rtParentInstOfInst, true, DisplayKind.name));

    htPersonInst.addChangeOrderMenuItem();
    htPersonInst.addRemoveMenuItem();

    htWorks = new HyperTable(tvWorks, 4, false, TablePrefKey.PERSON_WORKS);

    htWorks.addLabelCol(hdtWork    , smStandard  );  // Date
    htWorks.addLabelCol(hdtWorkType, smTextSimple);  // Work Type
    htWorks.addLabelCol(hdtWork    , smTextSimple);  // Ed/Tr

    htWorks.addCol(hdtInvestigation, ctInvSelect)
           .setHeaderTooltip(invHelpTooltip());

    htWorks.addLabelCol(hdtNone);   // Title; can display work or misc. file records
    htWorks.addLabelCol(hdtPerson); // Coauthor(s)

    tvWorks.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (oldValue == newValue)) return;

      HDT_RecordWithPath record = newValue.getRecord();

      if (record == null)
        setDefaultWorkPreview();
      else
        PreviewWindow.setPreview(pvsPersonTab, record);
    });

    htArguments = new HyperTable(tvArguments, 4, false, TablePrefKey.PERSON_ARG);

    htArguments.addIconCol();
    htArguments.addLabelCol(hdtNone);
    htArguments.addLabelCol(hdtPosition);
    htArguments.addLabelCol(hdtArgumentVerdict, smTextSimple);
    htArguments.addLabelCol(hdtArgument);

    hcbRank     = new HyperCB(cbRank    , ctEditableLimitedDropDown  , new StandardPopulator(hdtRank                 ), true);
    hcbStatus   = new HyperCB(cbStatus  , ctEditableLimitedDropDown  , new StandardPopulator(hdtPersonStatus         ), true);
    hcbField    = new HyperCB(cbField   , ctEditableLimitedDropDown  , new StandardPopulator(hdtField                ), true);
    hcbSubfield = new HyperCB(cbSubfield, ctEditableUnlimitedDropDown, new SubjectPopulator (rtFieldOfSubfield, false), true);

    setToolTip(btnWebSrch1, () -> TOOLTIP_PREFIX + btnWebSrch1.getText());
    setToolTip(smbWebSrch1, () -> TOOLTIP_PREFIX + smbWebSrch1.getText());
    setToolTip(btnWebSrch2, () -> TOOLTIP_PREFIX + btnWebSrch2.getText());

    setToolTip(tfFirst, "To indicate what name the person informally goes by, write it in parentheses. For example, \"William (Bill)\"");

    tfFirst.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (alreadyChangingName) return;
      updateSearchKey(new PersonName(newValue, tfLast.getText()), false);
    });

    tfLast.setTextFormatter(new TextFormatter<>(change ->
    {
      if (alreadyChangingName) return change;

      if (change.getText().length() > 1)
      {
        if (tfFirst.getText().isEmpty() && change.getControlText().isEmpty())
        {
          alreadyChangingName = true;
          String newText = change.getControlNewText();

          PersonName personName = new PersonName(newText);

          tfFirst.setText(personName.getFirst());

          change.setRange(0, change.getControlText().length());
          change.setText(personName.getLast());

          alreadyChangingName = false;
          updateSearchKey(personName, false);
          return change;
        }
      }

      if (change.isContentChange())
        updateSearchKey(new PersonName(tfFirst.getText(), change.getControlNewText()), false);

      return change;
    }));

    hcbField.addListener((oldValue, newValue) ->
    {
      if (newValue == null) return;

      if (getCellID(oldValue) != getCellID(newValue))
      {
        ((SubjectPopulator)hcbSubfield.getPopulator()).setObj(getRecord(newValue));
        if (getCellID(oldValue) > 0)
          hcbSubfield.selectID(-1);
      }
    });

    setToolTip(lblSearchKey, "Regenerate search key");

    lblSearchKey.setOnMouseClicked(event -> lblSearchKeyClick());

    MainCtrlr.setSearchKeyToolTip(tfSearchKey);

    lblWebsite.setOnMouseClicked(event -> openWebLink(tfWebsite.getText()));

    setToolTip(lblWebsite, "Click to launch website in browser");

    setToolTip(lblORCID, "Click to search for ORCID or open ORCID profile in browser");

    tpPerson.addEventFilter(KeyEvent.ANY, keyEvent ->
    {
      if (keyEvent.getCode().isArrowKey())
        lastArrowKey = Instant.now();
    });

    tpPerson.getSelectionModel().selectedItemProperty().addListener((ob, oldTab, newTab) ->
    {
      if (alreadyChangingTab) return;

      if (milliDiff(Instant.now(), lastArrowKey) < IGNORE_ARROW_KEYS_IN_TAB_PANE_MS) // Ignore arrow keys
      {
        alreadyChangingTab = true;
        tpPerson.getSelectionModel().select(oldTab);
        alreadyChangingTab = false;

        return;
      }

      tpPersonChange(oldTab, newTab);
    });

    Platform.runLater(() ->
    {
      final double invHelpGap = 20.0;

      StackPane headersRegion = (StackPane) tpPerson.lookup(".headers-region");
      lblInvHelp.setLayoutX(headersRegion.getWidth() + invHelpGap);

      headersRegion.widthProperty().addListener((obs, ov, nv) -> lblInvHelp.setLayoutX(nv.doubleValue() + invHelpGap));
    });

    lblInvHelp.setTooltip(invHelpTooltip());

    WebTooltip.setupClickHandler(lblInvHelp, lblInvHelp);

    lblORCID.setOnMouseClicked(event -> searchORCID(tfORCID.getText(), tfFirst.getText(), tfLast.getText()));

    btnWebSrch1.setOnAction(searchBtnEvent(WebButtonContextPrefKey.PERSON + '1'));
    smbWebSrch1.setOnAction(searchBtnEvent(WebButtonContextPrefKey.PERSON + '1'));
    btnWebSrch2.setOnAction(searchBtnEvent(WebButtonContextPrefKey.PERSON + '2'));

    ivPerson.setOnMouseClicked(event ->
    {
      PictureDlgCtrlr ctrlr = new PictureDlgCtrlr(viewPort);

      if (ctrlr.showModal())
        viewPort = ctrlr.getViewPort();

      PictureDlgCtrlr.httpClient.stop();
      refreshPicture();
    });

    btnWebsitePaste.setOnAction(event -> tfWebsite.setText(getClipboardText(true)));
    setToolTip(btnWebsitePaste, "Paste text from clipboard");

    btnOrcidPaste.setOnAction(event -> tfORCID.setText(getClipboardText(true)));
    setToolTip(btnOrcidPaste, "Paste text from clipboard");

    initWorkContextMenu();
    initArgContextMenu();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Tooltip invHelpTooltip()
  {
    return new WebTooltip("""
        Investigation records are a way of providing a description of a person&rsquo;s ongoing research<br>
        project (i.e., what they are &ldquo;investigating&rdquo;), which can include a number of works they&rsquo;ve<br>
        authored.<br><br>

        Investigations can also be thought of as a way of grouping an author&rsquo;s works by topic.<br><br>

        You add an Investigation record on the Persons tab by clicking the &ldquo;Add new investigation&rdquo;
        sub-tab.<br><br>

        For example, a Person record for Daniel Dennett might have at least 3 Investigation records:<br>
        &ldquo;Agency and Free Will&rdquo;, &ldquo;Intentional Stance&rdquo;, and &ldquo;Consciousness&rdquo;.<br><br>

        You can assign an investigation to a work in the Persons tab by clicking in the &ldquo;Investigation(s)&rdquo;<br>
        column in the list of works.<br><br>

        When an Investigation sub-tab is selected on the Persons tab, you can enter a description for<br>
        that Investigation in the text editor, and only works assigned to that Investigation are displayed<br>
        in the list of works.<br><br>

        As with many other types of records, you can assign a search key to an Investigation on its sub-tab.<br><br>

        Overall, it is a way of grouping a given author&rsquo;s works and prevents you from needing to write<br>
        a large amount of text on the Person&rsquo;s main description field (the &ldquo;Overview&rdquo; sub-tab).""");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setRecord(HDT_RecordWithMainText record)
  {
    if (record instanceof HDT_Person person)
    {
      curPerson = person;
      curInvestigation = null;
      return;
    }

    curInvestigation = record instanceof HDT_Investigation inv ? inv : null;

    curPerson = curInvestigation == null ? null : curInvestigation.person.get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void rescale()
  {
    ivPerson.setFitHeight(scalePropertyValueForDPI(ivPerson.getFitHeight()));
    ivPerson.setFitWidth (scalePropertyValueForDPI(ivPerson.getFitWidth ()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateFromRecord()
  {
    alreadyChangingName = true;

    tfFirst.setText(curPerson.getFirstName());
    tfLast .setText(curPerson.getLastName ());

    alreadyChangingName = false;

    tfORCID    .setText(curPerson.getOrcID    ());
    tfWebsite  .setText(curPerson.getWebURL   ());
    tfSearchKey.setText(curPerson.getSearchKey());

    curPicture = curPerson.filePath   ();
    viewPort   = curPerson.getViewPort();
    refreshPicture();

    hcbRank  .selectIDofRecord(curPerson.rank  );
    hcbStatus.selectIDofRecord(curPerson.status);

    if (curPerson.field.isNotNull())
    {
      hcbField   .selectIDofRecord(curPerson.field   );
      hcbSubfield.selectIDofRecord(curPerson.subfield);
    }

    htPersonInst.buildRows(curPerson.institutions, (row, inst) ->
    {
      row.setCheckboxValue(1, curPerson.instIsPast(inst));

      if (inst.parentInst.isNotNull())
        row.setCellValue(2, inst.parentInst.get(), inst.parentInst.get().name());

      row.setCellValue(3, inst, inst.name());
    });

    Set<HDT_Record> topicRecordsAdded = new HashSet<>();

    curPerson.works.forEach(work ->
    {

// Populate the debates
// --------------------

      work.arguments.forEach(argument -> {
        if (htArguments.containsRecord(argument) == false)
          if (addArgToTopicTable(argument, topicRecordsAdded, emptySet(), emptySet()))
            topicRecordsAdded.add(work); });

// Populate the works
// ------------------

      if (htWorks.containsRecord(work) == false)
      {
        HyperTableRow row = htWorks.newDataRow();
        row.setCellValue(0, new BibDateHTC(work, work.getBibDate()));

        String typeName = "";

        HDT_WorkType workType = work.workType.get();

        if (workType != null)
          typeName = work.workType.get().listName();

        typeName = HDT_Work.addFileIndicator(typeName, work);

        row.setCellValue(1, workType == null ? -1 : workType.getID(), typeName, hdtWorkType);

        String roleText = "";
        if (work.personIsEditor(curPerson)) roleText = "Ed";
        if (work.personIsTranslator(curPerson)) roleText = roleText.isEmpty() ? "Tr" : roleText + ", Tr";

        row.setCellValue(2, work, roleText);

        updateInvInWorkRow(row, work);

        row.setCellValue(4, work, work.name());

        if (work.getAuthors().size() > 1)
        {
          List<RecordAuthor> authors = new ArrayList<>();
          int authorID = -1;

          for (RecordAuthor author : work.getAuthors())
            if (curPerson != author.getPerson())
            {
              authors.add(author);

              if ((authorID == -1) && (author.getPerson() != null))
                authorID = author.getPerson().getID();
            }

          row.setCellValue(5, authorID, Author.getShortAuthorsStr(authors.stream(), false, true), hdtPerson);
        }
      }
    });

    htWorks.buildRows(curPerson.miscFiles.stream().filter(Predicate.not(htWorks::containsRecord)), (row, file) ->
    {
      if (file.work.isNotNull())
        row.setCellValue(0, new BibDateHTC(file, file.work.get().getBibDate()));
      else
        row.setCellValue(0, file, "");

      row.setCellValue(1, file, "File" + (file.fileType.isNull() ? "" : " (" + file.fileType.get().name() + ')'));

      row.setCellValue(2, file, "");

      updateInvInWorkRow(row, file);

      row.setCellValue(4, file, file.name());
    });

 // Add topic records to be populated to sets
 // -----------------------------------------

    Set<HDT_Argument> argsToAdd  = new LinkedHashSet<>();
    Set<HDT_Position> posToAdd   = new LinkedHashSet<>();
    Set<HDT_Record>   otherToAdd = new LinkedHashSet<>();

    db.displayerStream(curPerson).forEachOrdered(displayer ->
    {
      if (topicRecordsAdded.contains(displayer) == false)
      {
        switch (displayer.getType())
        {
          case hdtArgument : argsToAdd .add((HDT_Argument) displayer); break;
          case hdtPosition : posToAdd  .add((HDT_Position) displayer); break;
          default          : otherToAdd.add(displayer);                break;
        }
      }
    });

    curPerson.works.forEach(work -> addMentioners(work, argsToAdd, posToAdd, otherToAdd, topicRecordsAdded));

    curPerson.miscFiles.forEach(file -> addMentioners(file, argsToAdd, posToAdd, otherToAdd, topicRecordsAdded));

 // Populate the topic records from sets
 // ------------------------------------

    argsToAdd.forEach(arg -> addArgToTopicTable(arg, topicRecordsAdded, posToAdd, otherToAdd));

    htArguments.buildRows(posToAdd.stream().filter(Predicate.not(topicRecordsAdded::contains)), (row, pos) ->
    {
      addPosToTopicTable(pos, row, otherToAdd);
      row.setCellValue(0, pos, "");  // This is the icon column
      topicRecordsAdded.add(pos);
    });

    htArguments.buildRows(otherToAdd.stream().filter(Predicate.not(topicRecordsAdded::contains))
                                             .filter(topic -> topic != curPerson), (row, topic) ->
    {
      addOtherToTopicTable(topic, row);
      row.setCellValue(0, topic, "");  // This is the icon column
      topicRecordsAdded.add(topic);
    });

    if (curInvestigation == null)
      mainText.loadFromRecord(curPerson, true, getView().getTextInfo());
    else
      mainText.loadFromRecord(curPerson, false, new TextViewInfo(curPerson));

    loadInvestigations();

    // Always sort works by year on tab update unless the person hasn't changed

    htWorks.getTV().getSortOrder().setAll(curPerson == lastPerson ?
      List.copyOf(htWorks.getTV().getSortOrder())
    :
      List.of(htWorks.getTV().getColumns().get(0)));

    setDefaultWorkPreview();

    lastPerson = curPerson;

    safeFocus(tfLast);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setDefaultWorkPreview()
  {
    if (curPerson.works.isEmpty())
    {
      for (HDT_MiscFile miscFile : curPerson.miscFiles)
        if (miscFile.pathNotEmpty())
        {
          PreviewWindow.setPreview(pvsPersonTab, miscFile);
          return;
        }

      PreviewWindow.setPreview(pvsPersonTab, curPerson);
      return;
    }

    for (HDT_Work work : curPerson.works)
      if (work.canPreview())
      {
        PreviewWindow.setPreview(pvsPersonTab, work);
        return;
      }

    PreviewWindow.setPreview(pvsPersonTab, curPerson.works.get(0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addMentioners(HDT_RecordWithAuthors<? extends RecordAuthors> mentioned, Set<HDT_Argument> argsToAdd, Set<HDT_Position> posToAdd, Set<HDT_Record> otherToAdd, Set<HDT_Record> topicRecordsAdded)
  {
    mentioned.labelStream().forEach(label ->
    {
      if ((label.hasHub() == false) && (topicRecordsAdded.contains(label) == false))
        otherToAdd.add(label);
    });

    db.keyWorkMentionerStream(mentioned, true).forEachOrdered(mentioner ->
    {
      if (topicRecordsAdded.contains(mentioner) == false)
      {
        switch (mentioner.getType())
        {
          case hdtArgument : argsToAdd .add((HDT_Argument) mentioner); break;
          case hdtPosition : posToAdd  .add((HDT_Position) mentioner); break;
          default          : otherToAdd.add(mentioner);                break;
        }
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean addArgToTopicTable(HDT_Argument argument, Set<HDT_Record> topicRecordsAdded, Set<HDT_Position> posToAdd, Set<HDT_Record> otherToAdd)
  {
    if (topicRecordsAdded.contains(argument)) return false;

    if (argument.positions.size() > 0)
    {
      List<HDT_Position> positions = argument.positions.stream().filter(argument::isInFavor).toList();

      htArguments.buildRows(positions.isEmpty() ? argument.positions : positions, (row, position) ->
      {
        addPosToTopicTable(position, row, otherToAdd);
        posToAdd.remove(position);

        row.setCellValue(0, argument, "");  // This is the icon column

        nullSwitch(argument.getPosVerdict(position), verdict -> row.setCellValue(3, argument, verdict.listName()));

        row.setCellValue(4, argument, argument.listName());
      });
    }
    else
    {
      HyperTableRow row = htArguments.newDataRow();
      row.setCellValue(0, argument, "");  // This is the icon column

      nullSwitch(argument.getDebate(), debate ->
      {
        addOtherToTopicTable(debate, row);
        otherToAdd.remove(debate);
      });

      row.setCellValue(4, argument, argument.listName());
    }

    topicRecordsAdded.add(argument);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addPosToTopicTable(HDT_Position position, HyperTableRow row, Set<HDT_Record> otherToAdd)
  {
    row.setCellValue(2, position, position.listName());

    nullSwitch(position.getLargerDebate(), debate ->
    {
      addOtherToTopicTable(debate, row);
      otherToAdd.remove(debate);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addOtherToTopicTable(HDT_Record displayer, HyperTableRow row)
  {
    row.setCellValue(1, displayer, displayer.getType() == hdtWorkLabel ? ((HDT_WorkLabel) displayer).extendedText() : displayer.listName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshPicture()
  {
    if (FilePath.isEmpty(curPicture) == false)
    {
      Image picture = new Image(curPicture.toURI().toString());
      if (picture.isError() == false)
      {
        ivPerson.setImage(picture);
        ivPerson.setViewport(viewPort);
        lblPicture.setVisible(false);
        return;
      }
    }

    ivPerson.setImage(null);
    lblPicture.setVisible(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    alreadyChangingName = true;

    tfFirst.clear();
    tfLast .clear();

    alreadyChangingName = false;

    tfWebsite  .clear();
    tfORCID    .clear();
    tfSearchKey.clear();

    assignPicture(null, true);
    ivPerson.setImage(null);
    viewPort = null;
    lblPicture.setVisible(true);

    hcbRank    .clear();
    hcbStatus  .clear();
    hcbField   .clear();
    hcbSubfield.clear();

    mainText.clear();

    htPersonInst.clear();
    htWorks     .clearKeepSortOrder();
    htArguments .clear();

    clearInvestigations();

    curPerson        = resetRecord ? null : HDT_Record.getCurrentInstance(curPerson);
    lastPerson       = resetRecord ? null : HDT_Record.getCurrentInstance(lastPerson);
    curInvestigation = resetRecord ? null : HDT_Record.getCurrentInstance(curInvestigation);

    if ((curPerson != lastPerson) || (curPerson == null))
      htWorks.getTV().getSortOrder().clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void assignPicture(FilePath newPicture, boolean promptToDelete)
  {
    if (promptToDelete                           &&
        db.isLoaded()                            &&
        (FilePath.isEmpty(curPicture) == false)  &&
        (curPicture.equals(newPicture) == false) &&
        curPicture.exists()                      &&
        HyperPath.getHyperPathSetForFilePath(curPicture).isEmpty())

      db.fileNoLongerInUse(curPicture);

    curPicture = newPicture;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord(boolean saveNameIfBlank)
  {
    if (saveNameIfBlank && nameCheck(tfLast, curPerson.getName().getFull(), tfFirst.getText() + tfLast.getText(), "name") == false)
      return false;

    if (saveSearchKey(curPerson, tfSearchKey) == false) return false;

    if (saveNameIfBlank || ((tfFirst.getText() + tfLast.getText()).isBlank() == false))
    {
      PersonName personName = new PersonName(tfFirst.getText(), tfLast.getText());

      if ((personName.getFirst().equalsIgnoreCase(curPerson.getFirstName()) == false) ||
          (personName.getLast().equalsIgnoreCase(curPerson.getLastName()) == false))
        if (saveNameToRecord(personName) == false) return false;
    }

    // -----------------------------------------------------------------
    // Save investigations
    // -----------------------------------------------------------------

    if (saveInvestigations(null) == false) return false;

    // -----------------------------------------------------------------
    // End of save investigations
    // -----------------------------------------------------------------

    if (FilePath.isEmpty(curPicture))
      curPerson.getPath().clear();
    else
      curPerson.getPath().assign(HyperPath.getFolderFromFilePath(curPicture.getDirOnly(), true), curPicture.getNameOnly());

    curPerson.setViewPort(viewPort);

    curPerson.setWebURL(tfWebsite.getText());
    curPerson.setORCID(tfORCID.getText());
    curPerson.rank.setID(hcbRank.selectedID());
    curPerson.field.setID(hcbField.selectedID());
    curPerson.status.setID(hcbStatus.selectedID());

    htPersonInst.saveObjectsAndSingleNestedItem(curPerson, rtInstOfPerson, tagPast, 3, 1);

    mainText.save();

    // -----------------------------------------------------------------
    // Save subfield
    // -----------------------------------------------------------------

    int subfieldID = hcbSubfield.selectedID();
    if ((subfieldID < 1) && (hcbField.selectedID() > 0))
    {
      if (hcbSubfield.getText().length() > 0)
      {
        HDT_Subfield subfield = db.createNewBlankRecord(hdtSubfield);
        subfieldID = subfield.getID();
        subfield.setName(hcbSubfield.getText());
        subfield.field.setID(hcbField.selectedID());
      }
    }

    HDT_Subfield oldSubfield = curPerson.subfield.get();

    curPerson.subfield.setID((subfieldID > 0) && (hcbField.selectedID() > 0) ? subfieldID : -1);

    if ((oldSubfield != null) && oldSubfield.persons.isEmpty())
      db.deleteRecord(oldSubfield);

    // -----------------------------------------------------------------
    // End of save subfield
    // -----------------------------------------------------------------

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean saveNameToRecord(PersonName personName)
  {
    if (db.viewTestingInProgress || ui.dontInteract())
    {
      curPerson.setName(personName);
      return true;
    }

    PersonForDupCheck personForDupCheck = new PersonForDupCheck(new RecordAuthor(curPerson), personName);

    PersonMatcher matcher = new PersonMatcher();

    HyperTask task = matcher.createDupCheckTask(personForDupCheck, null);

    if (task.runWithProgressDialog() != State.SUCCEEDED) return false;

    List<RecordAuthor> matchedAuthors = matcher.getMatches(personForDupCheck);

    if (matchedAuthors.size() > 0)
      return new NewPersonDlgCtrlr(personName, tfSearchKey.getText(), new RecordAuthor(null, curPerson), matchedAuthors).showModal();

    curPerson.setName(personName);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void lblSearchKeyClick()
  {
    updateSearchKey(new PersonName(tfFirst.getText(), tfLast.getText()), true);
  }

  private void updateSearchKey(PersonName personName, boolean overwrite)
  {
    if ((db.isLoaded() == false) || (curPerson == null)) return;
    if ((overwrite == false) && (curPerson.getSearchKey().length() > 0)) return;

    StringBuilder sb = new StringBuilder();
    HDT_Person.makeSearchKey(personName, curPerson, sb);
    tfSearchKey.setText(sb.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initArgContextMenu()
  {
    htArguments.addDefaultMenuItems();

    EnumSet.of(hdtArgument, hdtPosition, hdtDebate, hdtTerm, hdtNote, hdtWork, hdtMiscFile, hdtInvestigation, hdtPerson).forEach(type ->
      htArguments.addContextMenuItem(getTypeName(type) + " Record...", type.getRecordClass(), record -> ui.goToRecord(record, true)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initWorkContextMenu()
  {
    htWorks.addDefaultMenuItems();

    htWorks.addContextMenuItem("Go to work record", HDT_Work    .class, work     -> ui.goToRecord(work    , true));
    htWorks.addContextMenuItem("Go to file record", HDT_MiscFile.class, miscFile -> ui.goToRecord(miscFile, true));

    htWorks.addContextMenuItem("Assign investigations", HDT_Work    .class, work     -> showInvSelectDialog(htWorks.getRowByRecord(work    )));
    htWorks.addContextMenuItem("Assign investigations", HDT_MiscFile.class, miscFile -> showInvSelectDialog(htWorks.getRowByRecord(miscFile)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void newInstClick(HyperTableRow row, String newName, int colNdx)
  {
    if (ui.cantSaveRecord()) return;

    HDT_Institution parentInst, subInst, oldParent = row.getRecord(2);
    if ((newName.length() > 0) && (colNdx == 2))
      oldParent = null;

    NewInstDlgCtrlr newInstDialog = new NewInstDlgCtrlr(oldParent, newName, colNdx == 2);

    if (newInstDialog.showModal() == false) return;

    if (newInstDialog.rbNewInst.isSelected())
    {
      parentInst = db.createNewBlankRecord(hdtInstitution);
      parentInst.setName(newInstDialog.tfNewParentName.getText());
    }
    else
      parentInst = newInstDialog.hcbParent.selectedRecord();

    if (parentInst == null) return;

    if (newInstDialog.rbNewDiv.isSelected())
    {
      subInst = db.createNewBlankRecord(hdtInstitution);

      subInst.parentInst.set(parentInst);
      subInst.setName(newInstDialog.tfName.getText());
      subInst.instType.setID(newInstDialog.hcbType.selectedID());

      subInst.setCity(parentInst.getCity());
      subInst.region.set(parentInst.region.get());
      subInst.country.set(parentInst.country.get());
    }
    else
      subInst = newInstDialog.hcbExisting.selectedRecord();

    if (subInst == null) return;

    curPerson.institutions.add(subInst);

    if (newInstDialog.rbNewInst.isSelected())
      ui.goToRecord(parentInst, false);
    else
      ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void newClick(RecordType objType, HyperTableRow row)
  {
    switch (objType)
    {
      case hdtInstitution :
        newInstClick(row, "", -1);
        break;

      default:
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public MainTextWrapper mainTextWrapper()
  {
    Tab tab = tpPerson.getSelectionModel().getSelectedItem();

    return tab.equals(tabOverview) ?
      mainText
    :
      nullSwitch(invViewByTab(tab), null, iV -> iV.textWrapper);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spTopHoriz, DividerPositionPrefKey.PERSON_TOP_HORIZ  , 0);
    setDividerPosition(spVert    , DividerPositionPrefKey.PERSON_MID_VERT   , 0);
    setDividerPosition(spVert    , DividerPositionPrefKey.PERSON_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spTopHoriz, DividerPositionPrefKey.PERSON_TOP_HORIZ  , 0);
    getDividerPosition(spVert    , DividerPositionPrefKey.PERSON_MID_VERT   , 0);
    getDividerPosition(spVert    , DividerPositionPrefKey.PERSON_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void updateWebButtons(Preferences node)
  {
    updateWebButtons(node, WebButtonContextPrefKey.PERSON, 2, btnWebSrch1, smbWebSrch1, this::searchBtnEvent);

    btnWebSrch2.setText(ui.webButtonMap.get(WebButtonContextPrefKey.PERSON + '2').getCaption());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EventHandler<ActionEvent> searchBtnEvent(String prefKey)
  {
    return event ->
      ui.webButtonMap.get(prefKey).first(WebButtonField.FirstName, tfFirst.getText())
                                  .next (WebButtonField.QueryName, tfFirst.getText())
                                  .next (WebButtonField.LastName, tfLast.getText())
                                  .next (WebButtonField.SingleName, tfLast.getText().length() > 0 ? tfLast.getText() : tfFirst.getText())
                                  .next (WebButtonField.Field, getCellText(hcbField.selectedHTC()))
                                  .go();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


  /********************************************************************************************************************************/
  /**                                                                                                                            **/
  /**                                                                                                                            **/
  /**                                                                                                                            **/
  /**                                              *********************************                                             **/
  /**                                              *                               *                                             **/
  /**                                              *      Investigation Stuff      *                                             **/
  /**                                              *                               *                                             **/
  /**                                              *********************************                                             **/
  /**                                                                                                                            **/
  /**                                                                                                                            **/
  /**                                                                                                                            **/
  /********************************************************************************************************************************/


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private InvestigationView invViewByTab(Tab tab)
  {
    return findFirst(invViews, iV -> iV.tab == tab);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private InvestigationView invViewByRecord(HDT_Investigation record)
  {
    return findFirst(invViews, iV -> iV.record == record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadInvestigations()
  {
    curPerson.investigations.forEach(inv -> addInvView(inv, true));

    if (curInvestigation != null) nullSwitch(invViewByRecord(curInvestigation), iV ->
    {
      tpPerson.getSelectionModel().select(iV.tab);
      safeFocus(iV.tfName);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private InvestigationView addInvView(HDT_Investigation inv, boolean useViewInfo)
  {
    InvestigationView iV;

    if (useViewInfo && (curInvestigation == inv))
      iV = new InvestigationView(inv, getView().getTextInfo());
    else
      iV = new InvestigationView(inv);

    iV.tab.setOnCloseRequest(this::deleteInvestigation);

    tpPerson.getTabs().add(tpPerson.getTabs().size() - 1, iV.tab);

    invViews.add(iV);

    return iV;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void tpPersonChange(Tab oldValue, Tab newValue)
  {
    if (oldValue == tabOverview)
      mainText.hide();
    else if (oldValue != tabNew)
      nullSwitch(invViewByTab(oldValue), iV -> iV.textWrapper.hide());

    if (newValue == tabNew)
    {
      HDT_Investigation inv = db.createNewBlankRecord(hdtInvestigation);
      inv.person.set(curPerson);
      curInvestigation = inv;

      InvestigationView iV = addInvView(inv, false);
      tpPerson.getSelectionModel().select(iV.tab);

      Platform.runLater(iV.tfName::requestFocus);
      return;
    }

    if (newValue == tabOverview)
    {
      htWorks.clearFilter();
      mainText.showReadOnly();
      curInvestigation = null;
      return;
    }

    InvestigationView iV = invViewByTab(newValue);
    assert(iV != null);
    assert(iV.record != null);

    curInvestigation = iV.record;

    htWorks.setFilter(row ->
    {
      HDT_Record record = row.getRecord();
      if ((record == null) || ((record.getType() != hdtWork) && (record.getType() != hdtMiscFile))) return false;

      return curInvestigation.worksAndMiscFilesStream().anyMatch(record_ -> record == record_);
    });

    curInvestigation.viewNow();

    iV.textWrapper.showReadOnly();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean saveInvestigations(InvestigationView ivNotToSave)
  {
    for (InvestigationView iV : invViews)
    {
      if (ivNotToSave == iV) continue;

      String invName = iV.tfName.getText();

      if (invName.isBlank())
      {
        tpPerson.getSelectionModel().select(iV.tab);
        return falseWithErrorPopup("Enter a name for the investigation.", iV.tfName);
      }

      HDT_Investigation inv = iV.record;

      try
      {
        inv.setSearchKey(iV.tfSearchKey.getText());
      }
      catch (SearchKeyException e)
      {
        tpPerson.getSelectionModel().select(iV.tab);

        String msg = "Unable to save investigation \"" + iV.tfName.getText() + "\". Search key " + (e instanceof SearchKeyTooShortException ? "must be at least 3 characters: " : "already exists: ") + e.getKey();

        return falseWithErrorPopup(msg, iV.tfSearchKey);
      }

      inv.setName(invName);
      iV.textWrapper.saveToRecord(inv);
    }

    if (ivNotToSave == null)
      ui.viewSequence.saveViewToCurrentSlotAndTab(newView(curInvestigation == null ? curPerson : curInvestigation));

    // Now delete the unused investigations
    // ------------------------------------

    List.copyOf(curPerson.investigations).forEach(inv ->
    {
      if (invViewByRecord(inv) == null)
        db.deleteRecord(inv);
    });

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearInvestigations()
  {
    invViews.removeIf(iV ->
    {
      tpPerson.getTabs().remove(iV.tab);
      return true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void deleteInvestigation(Event event)
  {
    event.consume();

    Platform.runLater(() ->   // Needs to be a runLater because otherwise, a mouse click event on the tab being closed gets processed after this, even if this event is consumed
    {
      Tab tab = (Tab) event.getSource();

      InvestigationView view = invViewByTab(tab);

      if ((view == null) || (view.record == null))
        return;

      if ((saveInvestigations(view) == false) || confirmDialog("Are you sure you want to delete the investigation?", false) == false)
        return;

      invViews.remove(view);
      tpPerson.getTabs().remove(view.tab);

      saveInvestigations(null);

      updateInvInWorkTable();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void updateInvInWorkTable()
  {
    htWorks.dataRows().forEach(row ->
    {
      if ((row.getRecordType() != hdtWork) && (row.getRecordType() != hdtMiscFile)) return;

      updateInvInWorkRow(row, row.getRecord());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateInvInWorkRow(HyperTableRow row, HDT_RecordWithAuthors<? extends RecordAuthors> workOrMiscFile)
  {
    HDT_Investigation inv = workOrMiscFile.investigationStream().findFirst().orElse(null);
    row.setCellValue(3, inv == null ? -1 : inv.getID(), getInvText(workOrMiscFile, curPerson), hdtInvestigation);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getInvText(HDT_RecordWithAuthors<? extends RecordAuthors> workOrMiscFile, HDT_Person person)
  {
    return workOrMiscFile.investigationStream().filter(inv -> inv.person.get() == person)
                                               .map(this::invName)
                                               .collect(Collectors.joining(", "));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String invName(HDT_Investigation inv)
  {
    InvestigationView iv = invViewByRecord(inv);
    return iv == null ? inv.name() : iv.tfName.getText();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void showInvSelectDialog(HyperTableRow row)
  {
    HDT_RecordWithAuthors<? extends RecordAuthors> workOrMiscFile = row.getRecord();

    InvestigationsDlgCtrlr dlg = new InvestigationsDlgCtrlr(workOrMiscFile, invViews, curPerson);

    if (dlg.showModal() == false)
      return;

    List<HDT_Investigation> investigations = workOrMiscFile.investigationStream().collect(Collectors.toCollection(ArrayList::new));

    for (InvestigationSetting is : dlg.listView.getItems())
      if (is.getSelected())
      {
        if (investigations.contains(is.inv) == false)
          investigations.add(is.inv);
      }
      else
        investigations.remove(is.inv);

    if (dlg.hasNew())
    {
      HDT_Investigation inv = db.createNewBlankRecord(hdtInvestigation);
      inv.person.set(curPerson);
      inv.setName(dlg.newName());
      investigations.add(inv);
      addInvView(inv, false);
    }

    MainText.setKeyWorkMentioners(workOrMiscFile, investigations, HDT_Investigation.class);

    updateInvInWorkTable();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
