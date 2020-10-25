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

package org.hypernomicon.view.tabs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.model.Exceptions.*;
import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.InvestigationsDlgCtrlr;
import org.hypernomicon.dialogs.NewInstDlgCtrlr;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.dialogs.PictureDlgCtrlr;
import org.hypernomicon.dialogs.InvestigationsDlgCtrlr.InvestigationSetting;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.HyperTableCell.CellSortMethod;

import static java.util.Collections.*;

import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.prefs.Preferences;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.geometry.Rectangle2D;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.SplitMenuButton;
import javafx.scene.control.SplitPane;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

//---------------------------------------------------------------------------

public class PersonTabCtrlr extends HyperTab<HDT_Person, HDT_Person>
{
  @FXML private AnchorPane apOverview;
  @FXML private Button btnWebSrch1, btnWebSrch2, btnPaste, btnNewWork;
  @FXML private ComboBox<HyperTableCell> cbRank, cbStatus, cbSubfield;
  @FXML private ImageView ivPerson;
  @FXML private Label lblORCID, lblWebsite, lblPicture, lblSearchKey;
  @FXML private SplitMenuButton smbWebSrch1;
  @FXML private SplitPane spTopHoriz, spVert;
  @FXML private Tab tabNew, tabOverview;
  @FXML private TabPane tpPerson;
  @FXML private TableView<HyperTableRow> tvArguments, tvPersonDept, tvWorks;
  @FXML private TextField tfORCID, tfWebsite, tfSearchKey;
  @FXML public ComboBox<HyperTableCell> cbField;
  @FXML public TextField tfFirst, tfLast;

  final private List<InvestigationView> invViews = new ArrayList<>();

  private static final String TOOLTIP_PREFIX = "Search for this person using ";

  private HyperTable htPersonInst, htWorks, htArguments;
  private MainTextWrapper mainText;
  public FilePath curPicture = null;
  private Rectangle2D viewPort = null;
  private HDT_Person lastPerson = null;

  private HyperCB hcbRank, hcbStatus, hcbField, hcbSubfield;

  private HDT_Person curPerson;
  private long lastArrowKey = 0;
  private boolean alreadyChangingName = false, alreadyChangingTab = false;

  @Override public String recordName()               { return new PersonName(tfFirst.getText(), tfLast.getText()).getLastFirst(); }
  @Override protected RecordType type()              { return hdtPerson; }
  @Override public void enable(boolean enabled)      { ui.tabPersons.getContent().setDisable(enabled == false); }
  @Override public void setRecord(HDT_Person person) { curPerson = person; }
  @Override public MainTextWrapper mainTextWrapper() { return mainText; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void rescale()
  {
    ivPerson.setFitHeight(ivPerson.getFitHeight() * displayScale);
    ivPerson.setFitWidth(ivPerson.getFitWidth() * displayScale);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean update()
  {
    alreadyChangingName = true;

    tfFirst.setText(curPerson.getFirstName());
    tfLast.setText(curPerson.getLastName());

    alreadyChangingName = false;

    tfORCID.setText(curPerson.getOrcID());
    tfWebsite.setText(curPerson.getWebURL());
    tfSearchKey.setText(curPerson.getSearchKey());

    curPicture = curPerson.filePath();
    viewPort = curPerson.getViewPort();
    refreshPicture();

    hcbRank.addAndSelectEntry(curPerson.rank, HDT_Record::name);
    hcbStatus.addAndSelectEntry(curPerson.status,  HDT_Record::name);

    mainText.loadFromRecord(curPerson, true, getView().getTextInfo());

    if (curPerson.field.isNotNull())
    {
      hcbField.addAndSelectEntry(curPerson.field, HDT_Record::name);
      hcbSubfield.selectID(curPerson.subfield.getID());
    }

    htPersonInst.buildRows(curPerson.institutions, (row, inst) ->
    {
      if (inst.parentInst.isNotNull())
        row.setCellValue(1, inst.parentInst.get(), inst.parentInst.get().name());

      row.setCellValue(2, inst, inst.name());
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
        row.setCellValue(0, work, work.getYear(), CellSortMethod.smNumeric);

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

        row.setCellValue(3, work.investigations.isEmpty() ? -1 : work.investigations.get(0).getID(), work.getInvText(curPerson), hdtInvestigation);

        row.setCellValue(4, work, work.name());

        if (work.getAuthors().size() > 1)
        {
          List<Author> authors = new ArrayList<>();
          int authorID = -1;

          for (Author author : work.getAuthors())
            if (curPerson != author.getPerson())
            {
              authors.add(author);

              if ((authorID == -1) && (author.getPerson() != null))
                authorID = author.getPerson().getID();
            }

          row.setCellValue(5, authorID, Authors.getShortAuthorsStr(authors, false, true), hdtPerson);
        }
      }
    });

    htWorks.buildRows(curPerson.miscFiles.stream().filter(Predicate.not(htWorks::containsRecord)), (row, file) ->
    {
      row.setCellValue(0, file, "");  // it's blank because file records don't have a year

      row.setCellValue(1, file, "File" + (file.fileType.isNull() ? "" : " (" + file.fileType.get().name() + ")"));

      row.setCellValue(2, file, "");
      row.setCellValue(3, file, "");
      row.setCellValue(4, file, file.name());
    });

 // Add topic records to be populated to sets
 // -----------------------------------------

    Set<MainText> displayers = db.getDisplayers(curPerson.getMainText());
    LinkedHashSet<HDT_Argument> argsToAdd = new LinkedHashSet<>();
    LinkedHashSet<HDT_Position> posToAdd = new LinkedHashSet<>();
    LinkedHashSet<HDT_Record> otherToAdd = new LinkedHashSet<>();

    displayers.forEach(displayerText ->
    {
      HDT_RecordWithConnector displayer = displayerText.getRecord();

      if (displayer.getType() == hdtHub)
      {
        StrongLink link = HDT_Hub.class.cast(displayer).getLink();

        if      (link.getDebate  () != null) displayer = link.getDebate();
        else if (link.getPosition() != null) displayer = link.getPosition();
        else if (link.getConcept () != null) displayer = link.getConcept();
        else                                 displayer = link.getNote();
      }

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

    curPerson.investigations.forEach(this::addInvView);

    if (curPerson != lastPerson)
      htWorks.getTV().getSortOrder().setAll(List.of(htWorks.getTV().getColumns().get(0)));
    else
    {
      List<TableColumn<HyperTableRow, ?>> list = new ArrayList<>(htWorks.getTV().getSortOrder());

      htWorks.getTV().getSortOrder().setAll(list);
    }

    setDefaultWorkPreview();

    lastPerson = curPerson;

    safeFocus(tfLast);

    return true;
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
          previewWindow.setPreview(pvsPersonTab, miscFile.filePath(), miscFile);
          return;
        }

      previewWindow.setPreview(pvsPersonTab, curPerson.filePath(), curPerson);
      return;
    }

    for (HDT_Work work : curPerson.works)
      if (work.workFiles.isEmpty() == false)
      {
        previewWindow.setPreview(pvsPersonTab, work.filePath(), work.getStartPageNum(), work.getEndPageNum(), work);
        return;
      }

    HDT_Work work = curPerson.works.get(0);
    previewWindow.setPreview(pvsPersonTab, work.filePath(), work.getStartPageNum(), work.getEndPageNum(), work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addMentioners(HDT_RecordWithPath mentioned, Set<HDT_Argument> argsToAdd, Set<HDT_Position> posToAdd, Set<HDT_Record> otherToAdd, Set<HDT_Record> topicRecordsAdded)
  {
    Consumer<HDT_WorkLabel> consumer = label ->
    {
      if ((label.isLinked() == false) && (topicRecordsAdded.contains(label) == false))
        otherToAdd.add(label);
    };

    if      (mentioned.getType() == hdtWork    ) HDT_Work    .class.cast(mentioned).labels.forEach(consumer);
    else if (mentioned.getType() == hdtMiscFile) HDT_MiscFile.class.cast(mentioned).labels.forEach(consumer);

    Set<HDT_RecordWithConnector> mentioners = db.getKeyWorkMentioners(mentioned);

    mentioners.forEach(mentioner ->
    {
      StrongLink link = mentioner.getLink();

      if (link != null)
      {
        if      (link.getDebate  () != null) mentioner = link.getDebate();
        else if (link.getPosition() != null) mentioner = link.getPosition();
        else if (link.getConcept () != null) mentioner = link.getConcept();
        else                                 mentioner = link.getNote();
      }

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

    List<HDT_Position> positions = new ArrayList<>();

    if (argument.positions.size() > 0)
    {
      argument.positions.forEach(position ->
      {
        if (argument.isInFavor(position))
          positions.add(position);
      });

      if (positions.isEmpty())
        positions.addAll(argument.positions);

      htArguments.buildRows(positions, (row, position) ->
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

  private void addPosToTopicTable(HDT_Position position, HyperTableRow row, Set<HDT_Record> otherToAdd)
  {
    row.setCellValue(2, position, position.listName());

    nullSwitch(position.getDebate(), debate ->
    {
      addOtherToTopicTable(debate, row);
      otherToAdd.remove(debate);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addOtherToTopicTable(HDT_Record displayer, HyperTableRow row)
  {
    row.setCellValue(1, displayer, displayer.getType() == hdtWorkLabel ? HDT_WorkLabel.class.cast(displayer).getExtendedText() : displayer.listName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshPicture()
  {
    if (FilePath.isEmpty(curPicture) == false)
    {
      Image picture = new Image(curPicture.toURI().toString());
      if (!picture.isError())
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

  @Override public void clear()
  {
    alreadyChangingName = true;

    tfFirst.clear();
    tfLast.clear();

    alreadyChangingName = false;

    tfWebsite.clear();
    tfORCID.clear();
    tfSearchKey.clear();

    if (db.isLoaded() && (FilePath.isEmpty(curPicture) == false) && curPicture.exists() && HyperPath.getHyperPathSetForFilePath(curPicture).isEmpty())
      db.fileNoLongerInUse(curPicture);

    curPicture = null;
    ivPerson.setImage(null);
    viewPort = null;
    lblPicture.setVisible(true);

    hcbRank.clear();
    hcbStatus.clear();
    hcbField.clear();
    hcbSubfield.clear();

    mainText.clear(true);

    htPersonInst.clear();
    htWorks.clearKeepSortOrder();
    htArguments.clear();

    invViews.removeIf(iV ->
    {
      tpPerson.getTabs().remove(iV.tab);
      return true;
    });

    if ((curPerson != lastPerson) || (curPerson == null))
      htWorks.getTV().getSortOrder().clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord()
  {
    if (!saveSearchKey(curPerson, tfSearchKey)) return false;

    if (FilePath.isEmpty(curPicture))
      curPerson.getPath().clear();
    else
      curPerson.getPath().assign(HyperPath.getFolderFromFilePath(curPicture.getDirOnly(), true), curPicture.getNameOnly());

    curPerson.setViewPort(viewPort);

    // Save investigations
    // -----------------------------------------------------------------

    for (InvestigationView iV : invViews)
    {
      HDT_Investigation inv = db.investigations.getByID(iV.id);

      try
      {
        inv.setSearchKey(iV.tfSearchKey.getText());
      }
      catch (SearchKeyException e)
      {
        falseWithErrorMessage(e.getTooShort() ?
          "Unable to modify investigation \"" + iV.tfName.getText() + "\": search key must be at least 3 characters."
        :
          "Unable to modify investigation \"" + iV.tfName.getText() + "\": search key already exists.");

        tpPerson.getSelectionModel().select(iV.tab);
        safeFocus(iV.tfSearchKey);
        return false;
      }

      inv.setName(iV.tfName.getText());
      iV.textWrapper.saveToRecord(inv);
    }

    // Now delete the unused investigations
    // ------------------------------------

    new ArrayList<>(curPerson.investigations).forEach(inv ->
    {
      if (invViews.stream().noneMatch(iV -> iV.id == inv.getID()))
        db.deleteRecord(inv);
    });

    // End of save investigations
    // ----------------------------------------------------------------------------------------------------

    PersonName personName = new PersonName(tfFirst.getText(), tfLast.getText());

    if ((personName.getFirst().equalsIgnoreCase(curPerson.getFirstName()) == false) ||
        (personName.getLast().equalsIgnoreCase(curPerson.getLastName()) == false))
      if (saveNameToRecord(personName) == false) return false;

    curPerson.setWebURL(tfWebsite.getText());
    curPerson.setORCID(tfORCID.getText());
    curPerson.rank.setID(hcbRank.selectedID());
    curPerson.field.setID(hcbField.selectedID());
    curPerson.status.setID(hcbStatus.selectedID());

    curPerson.setInstitutions(htPersonInst.saveToList(2, hdtInstitution));

    mainText.save();

 // save subfield

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

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean saveNameToRecord(PersonName personName)
  {
    if (db.viewTestingInProgress)
    {
      curPerson.setName(personName);
      return true;
    }

    List<List<Author>> matchedAuthorsList = new ArrayList<>();

    HyperTask task = NewPersonDlgCtrlr.createDupCheckTask(singletonList(personName), singletonList(new Author(curPerson)), matchedAuthorsList, null);

    if (!HyperTask.performTaskWithProgressDialog(task)) return false;

    List<Author> matchedAuthors = matchedAuthorsList.get(0);

    if (matchedAuthors.size() > 0)
    {
      NewPersonDlgCtrlr npdc = NewPersonDlgCtrlr.build(personName, tfSearchKey.getText(), true, curPerson, null, matchedAuthors);

      if (npdc.showModal() == false) return false;
    }
    else
    {
      curPerson.setName(personName);
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  @Override protected void init()
  {
    mainText = new MainTextWrapper(apOverview);

    btnNewWork.setOnAction(event -> ui.importWorkFile(curPerson, null, false));
    setToolTip(btnNewWork, "Create new work record with this person as author");

    Predicate<Integer> popFilter = id ->
    {
      HDT_Institution inst = db.institutions.getByID(id);

      return (inst.subInstitutions.size() > 0) || inst.parentInst.isNull();
    };

    htPersonInst = new HyperTable(tvPersonDept, 2, true, PREF_KEY_HT_PERSON_INST);

    htPersonInst.addActionCol(ctGoNewBtn, 1);
    htPersonInst.addColAltPopulatorWithUpdateHandler(hdtInstitution, ctDropDownList, new StandardPopulator(hdtInstitution, popFilter, true), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      ((SubjectPopulator)nextPopulator).setObj(row, getRecord(cellVal));
      row.setCellValue(nextColNdx, new HyperTableCell("", nextPopulator.getRecordType(row)));
    });
    htPersonInst.addColAltPopulator(hdtInstitution, ctDropDownList, new SubjectPopulator(rtParentInstOfInst, true, true));

    htPersonInst.addRemoveMenuItem();
    htPersonInst.addChangeOrderMenuItem(true);

    htWorks = new HyperTable(tvWorks, 4, false, PREF_KEY_HT_PERSON_WORKS);

    htWorks.addCol(hdtNone         , ctNone);
    htWorks.addCol(hdtWorkType     , ctNone);
    htWorks.addCol(hdtNone         , ctNone);
    htWorks.addCol(hdtInvestigation, ctInvSelect);
    htWorks.addCol(hdtWork         , ctNone);
    htWorks.addCol(hdtPerson       , ctNone);

    tvWorks.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (oldValue == newValue)) return;

      HDT_RecordWithPath record = newValue.getRecord();

      if (record == null)
        setDefaultWorkPreview();
      else if (record.getType() == hdtWork)
      {
        HDT_Work work = (HDT_Work)record;
        previewWindow.setPreview(pvsPersonTab, work.filePath(), work.getStartPageNum(), work.getEndPageNum(), work);
      }
      else
        previewWindow.setPreview(pvsPersonTab, record.filePath(), record);
    });

    htArguments = new HyperTable(tvArguments, 4, false, PREF_KEY_HT_PERSON_ARG);

    htArguments.addIconCol();
    htArguments.addCol(hdtNone    , ctNone);
    htArguments.addCol(hdtPosition, ctNone);
    htArguments.addCol(hdtNone    , ctNone);   // record type = hdtNone so that the column will sort purely based on the displayed text
    htArguments.addCol(hdtArgument, ctNone);

    hcbRank     = new HyperCB(cbRank    , ctDropDownList, new StandardPopulator(hdtRank                 ), true);
    hcbStatus   = new HyperCB(cbStatus  , ctDropDownList, new StandardPopulator(hdtPersonStatus         ), true);
    hcbField    = new HyperCB(cbField   , ctDropDownList, new StandardPopulator(hdtField                ), true);
    hcbSubfield = new HyperCB(cbSubfield, ctDropDown    , new SubjectPopulator (rtFieldOfSubfield, false), true);

    setToolTip(btnWebSrch1, TOOLTIP_PREFIX + "Google");
    setToolTip(btnWebSrch2, TOOLTIP_PREFIX + "Google Scholar");

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
        ((SubjectPopulator)hcbSubfield.getPopulator()).setObj(null, getRecord(newValue));
        if (getCellID(oldValue) > 0)
          hcbSubfield.selectID(-1);
      }
    });

    setToolTip(lblSearchKey, "Regenerate search key");

    lblSearchKey.setOnMouseClicked(event -> lblSearchKeyClick());

    ui.setSearchKeyToolTip(tfSearchKey);

    lblWebsite.setOnMouseClicked(event -> openWebLink(tfWebsite.getText()));

    tpPerson.addEventFilter(KeyEvent.ANY, event ->
    {
      if (event.getCode().isArrowKey())
        lastArrowKey = Instant.now().toEpochMilli();
    });

    tpPerson.getSelectionModel().selectedItemProperty().addListener((ob, oldTab, newTab) ->
    {
      if (alreadyChangingTab) return;
      
      if ((Instant.now().toEpochMilli() - lastArrowKey) < IGNORE_ARROW_KEYS_IN_TAB_PANE_MS) // Ignore arrow keys
      {
        alreadyChangingTab = true;
        tpPerson.getSelectionModel().select(oldTab);
        alreadyChangingTab = false;

        return;
      }
      
      tpPersonChange(oldTab, newTab);
    });
    
    lblORCID.setOnMouseClicked(event -> searchORCID(tfORCID.getText(), tfFirst.getText(), tfLast.getText()));

    btnWebSrch1.setOnAction(searchBtnEvent(PREF_KEY_PERSON_SRCH + "1"));
    smbWebSrch1.setOnAction(searchBtnEvent(PREF_KEY_PERSON_SRCH + "1"));
    btnWebSrch2.setOnAction(searchBtnEvent(PREF_KEY_PERSON_SRCH + "2"));

    ivPerson.setOnMouseClicked(event ->
    {
      PictureDlgCtrlr ctrlr = PictureDlgCtrlr.build(viewPort);

      if (ctrlr.showModal())
        viewPort = ctrlr.getViewPort();

      PictureDlgCtrlr.httpClient.stop();
      refreshPicture();
    });

    btnPaste.setOnAction(event -> tfWebsite.setText(getClipboardText(true)));
    setToolTip(btnPaste, "Paste text from clipboard");

    initWorkContextMenu();
    initArgContextMenu();
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
      htArguments.addContextMenuItem(db.getTypeName(type) + " Record...", type.getRecordClass(), record -> ui.goToRecord(record, true)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initWorkContextMenu()
  {
    htWorks.addDefaultMenuItems();

    htWorks.addContextMenuItem("Go to work record", HDT_Work.class,
      work -> ui.goToRecord(work, true));

    htWorks.addContextMenuItem("Go to file record", HDT_MiscFile.class,
      miscFile -> ui.goToRecord(miscFile, true));

    htWorks.addContextMenuItem("Assign investigations", HDT_Work.class,
      work -> showInvSelectDialog(htWorks.getRowByRecord(work)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void showInvSelectDialog(HyperTableRow row)
  {
    if (row.getRecordType() != hdtWork) return;

    HDT_Work work = row.getRecord();

    InvestigationsDlgCtrlr dlg = InvestigationsDlgCtrlr.build(work, curPerson);

    if (dlg.showModal() == false)
      return;

    for (InvestigationSetting is : dlg.listView.getItems())
      if (is.getSelected())
      {
        if (work.investigations.contains(is.inv) == false)
          work.investigations.add(is.inv);
      }
      else
        work.investigations.remove(is.inv);

    if (dlg.hasNew())
    {
      HDT_Investigation inv = db.createNewBlankRecord(hdtInvestigation);
      inv.person.set(curPerson);
      inv.setName(dlg.newName());
      work.investigations.add(inv);
      addInvView(inv);
    }

    HyperTableCell newValue = new HyperTableCell(work.investigations.isEmpty() ? -1 : work.investigations.get(0).getID(), work.getInvText(curPerson), hdtInvestigation);
    row.setCellValue(3, newValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  class InvestigationView
  {
    private int id;
    private TextField tfName, tfSearchKey;
    private MainTextWrapper textWrapper;
    private Tab tab;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void deleteInvestigation(Event event)
  {
    Tab tab = (Tab) event.getSource();

    InvestigationView view = findFirst(invViews, iV -> iV.tab.equals(tab));

    if ((view == null) || (view.id < 1))
      return;

    event.consume();

    if (ui.cantSaveRecord() || (confirmDialog("Are you sure you want to delete the investigation?") == false))
      return;

    invViews.remove(view);
    tpPerson.getTabs().remove(view.tab);

    ui.btnSaveClick();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private InvestigationView addInvView(HDT_Investigation inv)
  {
    InvestigationView iV = new InvestigationView();

    iV.id = inv.getID();
    String newName = inv.listName(),
           newSearchKey = inv.getSearchKey();

    iV.tab = new Tab();
    iV.tfName = new TextField(newName);
    iV.tab.setText(newName);
    iV.tfSearchKey = new TextField(newSearchKey);

    ui.setSearchKeyToolTip(iV.tfSearchKey);

    iV.tab.setOnCloseRequest(this::deleteInvestigation);

    BorderPane bPane = new BorderPane();
    AnchorPane aPane = new AnchorPane();

    GridPane gPane = new GridPane();
    gPane.add(new Label("Investigation name:"), 0, 0); // column=2 row=1
    bPane.setTop(gPane);
    gPane.add(iV.tfName, 1, 0);
    gPane.add(new Label("Search key:"), 2, 0);
    gPane.add(iV.tfSearchKey, 3, 0);

    ColumnConstraints cc2 = new ColumnConstraints(),
                      cc4 = new ColumnConstraints();

    cc2.setHgrow(Priority.ALWAYS);
    cc4.setHgrow(Priority.ALWAYS);

    gPane.getColumnConstraints().addAll(new ColumnConstraints(), cc2, new ColumnConstraints(), cc4);

    gPane.setHgap(3);

    iV.textWrapper = new MainTextWrapper(aPane);
    bPane.setCenter(aPane);

    iV.textWrapper.loadFromRecord(inv, false, new TextViewInfo());

    iV.tfName.textProperty().addListener((ob, oldText, newText) -> iV.tab.setText(newText));

    iV.tab.setContent(bPane);
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
      nullSwitch(findFirst(invViews, view -> view.tab == oldValue), iV -> iV.textWrapper.hide());

    if (newValue == tabNew)
    {
      HDT_Investigation inv = db.createNewBlankRecord(hdtInvestigation);
      inv.person.set(curPerson);

      InvestigationView iV = addInvView(inv);
      tpPerson.getSelectionModel().select(iV.tab);

      Platform.runLater(iV.tfName::requestFocus);
      return;
    }

    if (newValue == tabOverview)
    {
      htWorks.clearFilter();
      mainText.showReadOnly();
      return;
    }

    InvestigationView iV = findFirst(invViews, view -> view.tab == newValue);
    if (iV == null) return;

    if (iV.id < 1)
      htWorks.clearFilter();
    else
    {
      final HDT_Investigation inv = db.investigations.getByID(iV.id);

      htWorks.setFilter(row ->
      {
        HDT_Record record = row.getRecord();
        if ((record == null) || (record.getType() != hdtWork)) return false;

        return HDT_Work.class.cast(record).investigations.contains(inv);
      });

      db.investigations.getByID(iV.id).viewNow();
    }

    iV.textWrapper.showReadOnly();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void newInstClick(HyperTableRow row, String newName, int colNdx)
  {
    if (ui.cantSaveRecord()) return;

    HDT_Institution parentInst = null, subInst = null, oldParent = row.getRecord(1);
    if ((newName.length() > 0) && (colNdx == 1))
      oldParent = null;

    NewInstDlgCtrlr newInstDialog = NewInstDlgCtrlr.build(oldParent, newName, colNdx == 1);

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

  public void showInvestigation(int id)
  {
    nullSwitch(findFirst(invViews, iV -> iV.id == id), iV ->
    {
      tpPerson.getSelectionModel().select(iV.tab);
      safeFocus(iV.tfName);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MainTextWrapper getInvMainTextWrapper(int id)
  {
    return findFirst(invViews, iV -> iV.id == id, iV -> iV.textWrapper);
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

  @Override public void findWithinDesc(String text)
  {
    Tab tab = tpPerson.getSelectionModel().getSelectedItem();

    if (tab.equals(tabOverview))
    {
      mainText.hilite(text);
      return;
    }

    nullSwitch(findFirst(invViews, iV -> iV.tab.equals(tab)), iV -> iV.textWrapper.hilite(text));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public TextViewInfo mainTextInfo()
  {
    Tab tab = tpPerson.getSelectionModel().getSelectedItem();

    return tab.equals(tabOverview) ?
      mainText.getViewInfo()
    :
      nullSwitch(findFirst(invViews, iV -> iV.tab.equals(tab)), new TextViewInfo(), iV -> iV.textWrapper.getViewInfo());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spTopHoriz, PREF_KEY_PERSON_TOP_HORIZ, 0);
    setDividerPosition(spVert, PREF_KEY_PERSON_MID_VERT, 0);
    setDividerPosition(spVert, PREF_KEY_PERSON_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spTopHoriz, PREF_KEY_PERSON_TOP_HORIZ, 0);
    getDividerPosition(spVert, PREF_KEY_PERSON_MID_VERT, 0);
    getDividerPosition(spVert, PREF_KEY_PERSON_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void updateWebButtons(Preferences node)
  {
    updateWebButtons(node, PREF_KEY_PERSON_SRCH, 2, btnWebSrch1, smbWebSrch1, TOOLTIP_PREFIX, this::searchBtnEvent);

    btnWebSrch2.setText(ui.webButtonMap.get(PREF_KEY_PERSON_SRCH + "2").getCaption());

    setToolTip(btnWebSrch2, TOOLTIP_PREFIX + btnWebSrch2.getText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EventHandler<ActionEvent> searchBtnEvent(String prefKey)
  {
    return event ->
    {
      ui.webButtonMap.get(prefKey).first(WebButtonField.FirstName, tfFirst.getText())
                                  .next (WebButtonField.QueryName, tfFirst.getText())
                                  .next (WebButtonField.LastName, tfLast.getText())
                                  .next (WebButtonField.SingleName, tfLast.getText().length() > 0 ? tfLast.getText() : tfFirst.getText())
                                  .next (WebButtonField.Field, getCellText(cbField.getSelectionModel().getSelectedItem()))
                                  .go();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
