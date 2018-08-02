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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.model.Exceptions.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PositionVerdict;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.InvestigationsDialogController;
import org.hypernomicon.view.dialogs.InvestigationsDialogController.InvestigationSetting;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.dialogs.NewInstDialogController;
import org.hypernomicon.view.dialogs.NewPersonDialogController;
import org.hypernomicon.view.dialogs.PictureDialogController;
import org.hypernomicon.view.populators.Populator.PopulatorFilter;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.RecordListView;
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;

import static java.util.Collections.*;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.function.UnaryOperator;

import org.apache.commons.lang3.mutable.MutableInt;

import javafx.application.Platform;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.geometry.Rectangle2D;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.SplitPane;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

//---------------------------------------------------------------------------  

public class PersonTabController extends HyperTab<HDT_Person, HDT_Person>
{
  public HyperTable htPersonInst, htWorks, htArguments;
  public ArrayList<InvestigationView> invViews;
  public MainTextWrapper mainText;
  public FilePath curPicture = null;
  public Rectangle2D viewPort = null;
  private HDT_Person lastPerson = null;
  
  public HyperCB hcbRank, hcbStatus, hcbField, hcbSubfield;

  @FXML private Label lblPicture;
  @FXML private Label lblSearchKey;
  @FXML public TextField tfFirst;
  @FXML public TextField tfLast;
  @FXML private TextField tfSearchKey;
  @FXML private TextField tfPersonLink;
  @FXML private TextField tfORCID;
  @FXML private Label lblORCID;
  @FXML private Button btnGoogle;
  @FXML private Button btnScholar;
  @FXML private Button btnNewWork;
  @FXML private Label lblPersonLink;
  @FXML private TableView<HyperTableRow> tvWorks;
  @FXML private TableView<HyperTableRow> tvArguments;
  @FXML private TableView<HyperTableRow> tvPersonDept;
  @FXML private ComboBox<HyperTableCell> cbRank;
  @FXML private ComboBox<HyperTableCell> cbStatus;
  @FXML public ComboBox<HyperTableCell> cbField;
  @FXML private ComboBox<HyperTableCell> cbSubfield;
  @FXML private ImageView ivPerson;
  @FXML private TabPane tpPerson;
  @FXML private Tab tabOverview;
  @FXML private Tab tabNew;
  @FXML private AnchorPane apOverview;
  @FXML private SplitPane spTopHoriz;
  @FXML private SplitPane spVert;
  
  private HDT_Person curPerson;
  private boolean alreadyChangingName = false;
 
  @Override public HDT_RecordType getType()                { return hdtPerson; }
  @Override public void enable(boolean enabled)            { ui.tabPersons.getContent().setDisable(enabled == false); }
  @Override public void focusOnSearchKey()                 { safeFocus(tfSearchKey); }
  @Override public void setRecord(HDT_Person activeRecord) { curPerson = activeRecord; }
  @Override public MainTextWrapper getMainTextWrapper()    { return mainText; }
  
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
    int ndx;
    
    if (db.isLoaded() == false) return false;
    
    clear();
    
    if ((curPerson != lastPerson) || (curPerson == null))
      htWorks.tv.getSortOrder().clear();
    
    if (curPerson == null)
    {
      enable(false);
      return false;
    }

    alreadyChangingName = true;
    
    tfFirst.setText(curPerson.getFirstName());
    tfLast.setText(curPerson.getLastName());
    
    alreadyChangingName = false;
    
    tfORCID.setText(curPerson.getOrcID());
    tfPersonLink.setText(curPerson.getWebLink());
    tfSearchKey.setText(curPerson.getSearchKey());
       
    curPicture = curPerson.getPath().getFilePath();
    viewPort = curPerson.getViewPort();
    refreshPicture();

    if (curPerson.rank.isNotNull())
      hcbRank.addEntry(curPerson.rank.getID(), curPerson.rank.get().name(), curPerson.rank.getID());
    
    if (curPerson.status.isNotNull())
      hcbStatus.addEntry(curPerson.status.getID(),  curPerson.status.get().name(), curPerson.status.getID());
    
    mainText.loadFromRecord(curPerson, true, getView().getTextInfo());
       
    if (curPerson.field.isNotNull())
    {
      hcbField.addEntry(curPerson.field.getID(), curPerson.field.get().name(), curPerson.field.getID());
      hcbSubfield.selectID(curPerson.subfield.getID());
    }    
  
    ndx = 0; for (HDT_Institution inst : curPerson.institutions)
    {
      if (inst.parentInst.isNotNull())
        htPersonInst.setDataItem(1, ndx, inst.parentInst.getID(), inst.parentInst.get().name(), hdtInstitution);
      
      htPersonInst.setDataItem(2, ndx, inst.getID(), inst.name(), hdtInstitution);
      ndx++;
    }
    
    MutableInt argRowNdx = new MutableInt(0);
    int workRowNdx = 0;
    
    HashSet<HDT_Base> topicRecordsAdded = new HashSet<>();
    
    for (HDT_Work work : curPerson.works)
    {
// Populate the debates
// --------------------
      
      for (HDT_Argument argument : work.arguments)
      {
        if (htArguments.containsRecord(argument.getID(), hdtArgument) == false)
        {
          if (addArgToTopicTable(argument, topicRecordsAdded, argRowNdx, emptySet(), emptySet()))
          {            
            topicRecordsAdded.add(work);          
          }
        }
      }

// Populate the works
// ------------------
     
      if (htWorks.containsRecord(work.getID(), hdtWork) == false)
      {
        htWorks.setDataItem(0, workRowNdx, work.getID(), work.getYear(), hdtWork, HyperCellSortMethod.hsmNumeric);
        
        String typeName = "";
        
        HDT_WorkType workType = work.workType.get();
        
        if (workType != null)
          typeName = work.workType.get().listName();
        
        typeName = HDT_Work.addFileIndicator(typeName, work);
                
        htWorks.setDataItem(1, workRowNdx, workType == null ? -1 : workType.getID(), typeName, hdtWorkType);
        
        String roleText = "";
        if (work.personIsEditor(curPerson)) roleText = "Ed";
        if (work.personIsTranslator(curPerson)) roleText = roleText.length() == 0 ? "Tr" : roleText + ", Tr";
        
        htWorks.setDataItem(2, workRowNdx, work.getID(), roleText, hdtWork);
        
        htWorks.setDataItem(3, workRowNdx, work.investigations.isEmpty() ? -1 : work.investigations.get(0).getID(), work.getInvText(curPerson), hdtInvestigation);

        htWorks.setDataItem(4, workRowNdx, work.getID(), work.name(), hdtWork);
        
        if (work.getAuthors().size() > 1)
        {
          ArrayList<Author> authors = new ArrayList<>();
          int authorID = -1;
          
          for (Author author : work.getAuthors())
            if (curPerson != author.getPerson())
            {
              authors.add(author);
              
              if ((authorID == -1) && (author.getPerson() != null))
                authorID = author.getPerson().getID();
            }
          
          htWorks.setDataItem(5, workRowNdx, authorID, Authors.getShortAuthorsStr(authors, false, true), hdtPerson);
        }
        
        workRowNdx++;
      }
    }
      
    for (HDT_MiscFile file : curPerson.miscFiles)
    {      
      if (htWorks.containsRecord(file.getID(), hdtMiscFile) == false)
      {
        htWorks.setDataItem(0, workRowNdx, file.getID(), "", hdtMiscFile);  // it's blank because files don't have a year

        if (file.fileType.isNotNull())
          htWorks.setDataItem(1, workRowNdx, file.getID(), "File (" + file.fileType.get().name() + ")", hdtMiscFile);
        else
          htWorks.setDataItem(1, workRowNdx, file.getID(), "File", hdtMiscFile);

        htWorks.setDataItem(2, workRowNdx, file.getID(), "", hdtMiscFile);
        htWorks.setDataItem(3, workRowNdx, file.getID(), "", hdtMiscFile);
        htWorks.setDataItem(4, workRowNdx, file.getID(), file.name(), hdtMiscFile);
        workRowNdx++;
      }
    }
    
 // Add topic records to be populated to sets
 // -----------------------------------------
    
    Set<MainText> displayers = db.getDisplayers(curPerson.getMainText());
    LinkedHashSet<HDT_Argument> argsToAdd = new LinkedHashSet<>();
    LinkedHashSet<HDT_Position> posToAdd = new LinkedHashSet<>();
    LinkedHashSet<HDT_Base> otherToAdd = new LinkedHashSet<>();
    
    for (MainText displayerText : displayers)
    {
      HDT_RecordWithConnector displayer = displayerText.getRecord();
      
      if (displayer.getType() == hdtHub)
      {
        StrongLink link = HDT_Hub.class.cast(displayer).getLink();
        if (link.getDebate() != null)
          displayer = link.getDebate();
        else if (link.getPosition() != null)
          displayer = link.getPosition();
        else if (link.getConcept() != null)
          displayer = link.getConcept();
        else
          displayer = link.getNote();
      }
      
      if (topicRecordsAdded.contains(displayer) == false)
      {
        switch (displayer.getType())
        {
          case hdtArgument : argsToAdd.add((HDT_Argument) displayer); break;
          case hdtPosition : posToAdd.add((HDT_Position) displayer); break;
          default :          otherToAdd.add(displayer); break;
        }
      }
    }
    
    curPerson.works.forEach(work -> addMentioners(work, argsToAdd, posToAdd, otherToAdd, topicRecordsAdded));

    curPerson.miscFiles.forEach(file -> addMentioners(file, argsToAdd, posToAdd, otherToAdd, topicRecordsAdded));
    
 // Populate the topic records from sets
 // ------------------------------------
    
    for (HDT_Argument arg : argsToAdd)
    {
      addArgToTopicTable(arg, topicRecordsAdded, argRowNdx, posToAdd, otherToAdd);
    }
    
    for (HDT_Position pos : posToAdd)
    {
      if (addPosToTopicTable(pos, topicRecordsAdded, argRowNdx.intValue(), otherToAdd))
      {
        htArguments.setDataItem(0, argRowNdx.intValue(), pos.getID(), pos.listName(), hdtPosition);
        topicRecordsAdded.add(pos);
        argRowNdx.increment();
      }
    }
    
    for (HDT_Base topic : otherToAdd)
    {
      if (addOtherToTopicTable(topic, topicRecordsAdded, argRowNdx.intValue()))
      {
        htArguments.setDataItem(0, argRowNdx.intValue(), topic.getID(), topic.listName(), topic.getType());        
        topicRecordsAdded.add(topic);
        argRowNdx.increment();
      }
    }
    
    curPerson.investigations.forEach(this::addInvView);  
   
    if (curPerson != lastPerson)
    {
      htWorks.tv.getSortOrder().clear();
      htWorks.tv.getSortOrder().add(htWorks.tv.getColumns().get(0));
    }
    else
    {
      ArrayList<TableColumn<HyperTableRow, ?>> list = new ArrayList<>();
      list.addAll(htWorks.tv.getSortOrder());
      
      htWorks.tv.getSortOrder().clear();
      htWorks.tv.getSortOrder().addAll(list);
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
        if (miscFile.getPath().isEmpty() == false)
        {
          previewWindow.setPreview(pvsPersonTab, miscFile.getPath().getFilePath(), -1, -1, miscFile);
          return;
        }
      
      previewWindow.setPreview(pvsPersonTab, null, -1, -1, null);
      return;
    }
    
    for (HDT_Work work : curPerson.works)
      if (work.workFiles.isEmpty() == false)
      {
        previewWindow.setPreview(pvsPersonTab, work.getPath().getFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
        return;
      }

    HDT_Work work = curPerson.works.get(0);
    previewWindow.setPreview(pvsPersonTab, work.getPath().getFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);   
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void addMentioners(HDT_RecordWithPath mentioned, Set<HDT_Argument> argsToAdd, Set<HDT_Position> posToAdd, Set<HDT_Base> otherToAdd, HashSet<HDT_Base> topicRecordsAdded)
  {    
    StrongLink link = null;
    
    if (mentioned.getType() == hdtWork)
    {
      HDT_Work work = HDT_Work.class.cast(mentioned);
      for (HDT_WorkLabel label : work.labels)
        if (label.isLinked() == false)
          if (topicRecordsAdded.contains(label) == false)
            otherToAdd.add(label);
    }
    else if (mentioned.getType() == hdtMiscFile)
    {
      HDT_MiscFile miscFile = HDT_MiscFile.class.cast(mentioned);
      for (HDT_WorkLabel label : miscFile.labels)
        if (label.isLinked() == false)
          if (topicRecordsAdded.contains(label) == false)
            otherToAdd.add(label);
    }

    Set<HDT_RecordWithConnector> mentioners = db.getKeyWorkMentioners(mentioned);
    if (mentioners == null) return;
    
    for (HDT_RecordWithConnector mentioner : mentioners)
    {
      link = null;
      
      if (mentioner.getType() == hdtHub)
        link = HDT_Hub.class.cast(mentioner).getLink();
      
      else if (mentioner.isLinked())
        link = mentioner.getLink();
      
      if (link != null)
      {
        if (link.getDebate() != null)
          mentioner = link.getDebate();
        else if (link.getPosition() != null)
          mentioner = link.getPosition();
        else if (link.getConcept() != null)
          mentioner = link.getConcept();
        else
          mentioner = link.getNote();
      }
      
      if (topicRecordsAdded.contains(mentioner) == false)
      {
        switch (mentioner.getType())
        {
          case hdtArgument : argsToAdd.add((HDT_Argument) mentioner); break;
          case hdtPosition : posToAdd.add((HDT_Position) mentioner); break;
          default :          otherToAdd.add(mentioner); break;
        }
      }
    } 
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private boolean addArgToTopicTable(HDT_Argument argument, HashSet<HDT_Base> topicRecordsAdded, MutableInt argRowNdx,
                                  Set<HDT_Position> posToAdd, Set<HDT_Base> otherToAdd)
  {
    if (topicRecordsAdded.contains(argument)) return false;    
    
    ArrayList<HDT_Position> positions = new ArrayList<>();
    
    if (argument.positions.size() > 0)
    {
      argument.positions.forEach(position ->
      {
        if (argument.isInFavor(position))
          positions.add(position);
      });
      
      if (positions.isEmpty())
        positions.addAll(argument.positions);
      
      positions.forEach(position ->
      {
        addPosToTopicTable(position, topicRecordsAdded, argRowNdx.intValue(), otherToAdd);
        posToAdd.remove(position);
        
        htArguments.setDataItem(0, argRowNdx.intValue(), argument.getID(), argument.listName(), hdtArgument);
        
        HDT_PositionVerdict verdict = argument.getPosVerdict(position);
          if (verdict != null)
            htArguments.setDataItem(3, argRowNdx.intValue(), argument.getID(), verdict.listName(), hdtArgument);
          
        htArguments.setDataItem(4, argRowNdx.intValue(), argument.getID(), argument.listName(), hdtArgument);
        argRowNdx.increment();
      });
    }
    else
    {
      htArguments.setDataItem(0, argRowNdx.intValue(), argument.getID(), argument.listName(), hdtArgument);
      
      HDT_Debate debate = argument.getDebate();
      if (debate != null)
      {
        addOtherToTopicTable(debate, topicRecordsAdded, argRowNdx.intValue());
        otherToAdd.remove(debate);        
      }
      
      htArguments.setDataItem(4, argRowNdx.intValue(), argument.getID(), argument.listName(), hdtArgument);
      argRowNdx.increment();
    }
    
    topicRecordsAdded.add(argument);
    
    return true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private boolean addPosToTopicTable(HDT_Position position, HashSet<HDT_Base> topicRecordsAdded, int argRowNdx, Set<HDT_Base> otherToAdd)
  {
    if (topicRecordsAdded.contains(position)) return false;
    
    htArguments.setDataItem(2, argRowNdx, position.getID(), position.listName(), hdtPosition);

    HDT_Debate debate = position.getDebate();
    if (debate != null)
    {
      addOtherToTopicTable(debate, topicRecordsAdded, argRowNdx);
      otherToAdd.remove(debate); 
    }
    
    return true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private boolean addOtherToTopicTable(HDT_Base displayer, HashSet<HDT_Base> topicRecordsAdded, int argRowNdx)
  {
    if (topicRecordsAdded.contains(displayer)) return false;
    
    if (displayer.getType() == hdtWorkLabel)
      htArguments.setDataItem(1, argRowNdx, displayer.getID(), HDT_WorkLabel.class.cast(displayer).getExtendedText(), hdtWorkLabel);
    else
      htArguments.setDataItem(1, argRowNdx, displayer.getID(), displayer.listName(), displayer.getType());
    
    return true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void refreshPicture()
  {
    boolean noPic = true;
    Image picture = null;
    
    if (FilePath.isEmpty(curPicture) == false)
    {
      picture = new Image(curPicture.toURI().toString());
      if (!picture.isError())
        noPic = false;
    }
      
    if (noPic)
    {
      ivPerson.setImage(null);
      lblPicture.setVisible(true);
    }
    else
    {
      ivPerson.setImage(picture);      
      ivPerson.setViewport(viewPort);      
      lblPicture.setVisible(false);
    }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void clear()
  {
    alreadyChangingName = true;
    
    tfFirst.clear();
    tfLast.clear();
    
    alreadyChangingName = false;
    
    tfPersonLink.clear();
    tfORCID.clear();
    tfSearchKey.clear();
    
    if (db.isLoaded() && (FilePath.isEmpty(curPicture) == false))
      if (curPicture.exists())
      {
        Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(curPicture);
        if (set.isEmpty())
          db.fileNoLongerInUse(curPicture);
      }
    
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
    
    Iterator<InvestigationView> it = invViews.iterator();
      
    while (it.hasNext())
    {
      InvestigationView iV = it.next();
      tpPerson.getTabs().remove(iV.tab);
      it.remove();
    }
  }
   
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public boolean saveToRecord(boolean showMessage)
  {
    int ndx, subfieldID;
    boolean noDelete;
    HDT_Subfield subfield;    
     
    if (!saveSearchKey(curPerson, tfSearchKey, showMessage, this)) return false;
       
    if (FilePath.isEmpty(curPicture))
      curPerson.getPath().assign(db.folders.getByID(HyperDB.PICTURES_FOLDER_ID), new FilePath(""));
    else
      curPerson.getPath().assign(db.folders.getByID(HyperDB.PICTURES_FOLDER_ID), curPicture.getNameOnly());
    
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
        if (e.getTooShort())
          messageDialog("Unable to modify investigation \"" + iV.tfName.getText() + "\": search key must be at least 3 characters.", mtError);
        else
          messageDialog("Unable to modify investigation \"" + iV.tfName.getText() + "\": search key already exists.", mtError);
        
        tpPerson.getSelectionModel().select(iV.tab);
        safeFocus(iV.tfSearchKey);
        return false;
      }

      inv.setName(iV.tfName.getText());
      iV.textWrapper.saveToRecord(inv);
    }
       
    // Now delete the unused investigations
    // ------------------------------------
    
    for (ndx = 0; ndx < curPerson.investigations.size(); ndx++)
    {
      noDelete = false;
      HDT_Investigation inv = curPerson.investigations.get(ndx);

      for (InvestigationView iV : invViews)
      {
        if (iV.id == inv.getID())
          noDelete = true;
      }
      
      if (noDelete == false)
      {
        db.deleteRecord(hdtInvestigation, inv.getID());
        ndx--;
      }      
    }
    
    // End of save investigations
    // ----------------------------------------------------------------------------------------------------
    
    PersonName personName = new PersonName(tfFirst.getText(), tfLast.getText());
    
    if ((personName.getFirst().equalsIgnoreCase(curPerson.getFirstName()) == false) || 
        (personName.getLast().equalsIgnoreCase(curPerson.getLastName()) == false))
      if (saveNameToRecord(personName) == false) return false;
    
    curPerson.setWebLink(tfPersonLink.getText());
    curPerson.setORCID(tfORCID.getText());
    curPerson.rank.setID(hcbRank.selectedID());
    curPerson.field.setID(hcbField.selectedID());
    curPerson.status.setID(hcbStatus.selectedID());
    
    curPerson.setInstitutions(htPersonInst);
    
    mainText.save();
    
 // save subfield
   
    subfieldID = hcbSubfield.selectedID();
    if ((subfieldID < 1) && (hcbField.selectedID() > 0))
    {
      if (hcbSubfield.getText().length() > 0)
      {
        subfield = db.createNewBlankRecord(hdtSubfield);
        subfieldID = subfield.getID();
        subfield.setName(hcbSubfield.getText());
        subfield.field.setID(hcbField.selectedID());
      }
    }
   
    if ((subfieldID > 0) && (hcbField.selectedID() > 0))
      curPerson.subfield.setID(subfieldID);
    else
      curPerson.subfield.setID(-1);
       
    // Now delete the unused subfields
    for (ndx = 0; ndx < db.subfields.size(); ndx++) 
    { 
      subfield = db.subfields.getByIDNdx(ndx);
      if (subfield.persons.isEmpty()) 
      { 
        db.deleteRecord(hdtSubfield, subfield.getID()); 
        ndx--;
      } 
    }
    
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
    
    List<ArrayList<Author>> matchedAuthorsList = new ArrayList<>();
    
    HyperTask task = NewPersonDialogController.createDupCheckTask(singletonList(personName), null, curPerson, matchedAuthorsList, null);
    
    if (!HyperTask.performTaskWithProgressDialog(task)) return false;
    
    ArrayList<Author> matchedAuthors = matchedAuthorsList.get(0);
    
    if (matchedAuthors.size() > 0)
    {
      NewPersonDialogController npdc = NewPersonDialogController.create(personName, tfSearchKey.getText(), true, curPerson, null, matchedAuthors);
      
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

  @Override protected void init(TabEnum tabEnum) 
  {
    this.tabEnum = tabEnum;
    mainText = new MainTextWrapper(apOverview);
    
    PopulatorFilter popFilter = record -> 
    {
      HDT_Institution inst = (HDT_Institution)record;
      
      return (inst.subInstitutions.size() > 0) || (inst.parentInst.isNull());
    };
    
    htPersonInst = new HyperTable(tvPersonDept, 2, true, PREF_KEY_HT_PERSON_INST);

    htPersonInst.addActionCol(ctGoNewBtn, 1);
    htPersonInst.addColAltPopulatorWithUpdateHandler(hdtInstitution, ctDropDownList, new StandardPopulator(hdtInstitution, popFilter, true), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      ((SubjectPopulator)nextPopulator).setObj(row, HyperTableCell.getRecord(cellVal));
      row.updateCell(nextColNdx, new HyperTableCell(-1, "", nextPopulator.getRecordType(row)));
    });
    htPersonInst.addColAltPopulator(hdtInstitution, ctDropDownList, new SubjectPopulator(rtParentInstOfInst, true, true));

    htPersonInst.addRemoveMenuItem();
    htPersonInst.addChangeOrderMenuItem(true);
    
    htWorks = new HyperTable(tvWorks, 4, false, PREF_KEY_HT_PERSON_WORKS);
    
    htWorks.addCol(hdtNone, ctNone);
    htWorks.addCol(hdtWorkType, ctNone);
    htWorks.addCol(hdtNone, ctNone);
    htWorks.addCol(hdtInvestigation, ctInvSelect);
    htWorks.addCol(hdtWork, ctNone);
    htWorks.addCol(hdtPerson, ctNone);
    
    tvWorks.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      if (oldValue == newValue) return;
      
      HDT_RecordWithPath record = (HDT_RecordWithPath) newValue.getRecord();
            
      if (record == null)
        setDefaultWorkPreview();
      else if (record.getType() == hdtWork)
      {
        HDT_Work work = (HDT_Work)record;
        previewWindow.setPreview(pvsPersonTab, work.getPath().getFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
      }
      else
        previewWindow.setPreview(pvsPersonTab, record.getPath().getFilePath(), -1, -1, record);        
    });
    
    htArguments = new HyperTable(tvArguments, 4, false, PREF_KEY_HT_PERSON_ARG);
    
    htArguments.addIconCol();
    htArguments.addCol(hdtNone, ctNone);
    htArguments.addCol(hdtPosition, ctNone);
    htArguments.addCol(hdtNone, ctNone);      // record type = hdtNone so that the column will sort purely based on the displayed text
    htArguments.addCol(hdtArgument, ctNone);
    
    hcbRank = new HyperCB(cbRank, ctDropDownList, new StandardPopulator(hdtRank), null);
    hcbStatus = new HyperCB(cbStatus, ctDropDownList, new StandardPopulator(hdtPersonStatus), null);
    hcbField = new HyperCB(cbField, ctDropDownList, new StandardPopulator(hdtField), null);
    hcbSubfield = new HyperCB(cbSubfield, ctDropDown, new SubjectPopulator(rtFieldOfSubfield, false), null);

    tfFirst.setTooltip(new Tooltip("To indicate what name the person informally goes by, write it in parentheses. For example, \"William (Bill)\""));
    
    tfFirst.textProperty().addListener((observable, oldValue, newValue) ->
    {
      if (alreadyChangingName) return;
      updateSearchKey(new PersonName(newValue, tfLast.getText()), false); 
    });
    
    UnaryOperator<TextFormatter.Change> filter = (change) ->
    {
      if (alreadyChangingName) return change;
      
      if (change.getText().length() > 1)
      {
        if ((tfFirst.getText().length() == 0) && (change.getControlText().length() == 0))
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
    };
         
    tfLast.setTextFormatter(new TextFormatter<>(filter));    
       
    cbField.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> 
    {    
      if (newValue == null) return;
      
      if (HyperTableCell.getCellID(oldValue) != HyperTableCell.getCellID(newValue))
      {
        ((SubjectPopulator)hcbSubfield.getPopulator()).setObj(null, getRecord(newValue));
        if (HyperTableCell.getCellID(oldValue) > 0) 
          hcbSubfield.selectID(-1);
      }
    });
    
    lblSearchKey.setTooltip(new Tooltip("Regenerate search key"));
    
    lblSearchKey.setOnMouseClicked(event -> lblSearchKeyClick());
    
    lblPersonLink.setOnMouseClicked(event -> openWebLink(tfPersonLink.getText()));
    
    tpPerson.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    { 
      tpPersonChange(oldValue, newValue);
    });
    
    lblORCID.setOnMouseClicked(event -> searchORCID(tfORCID.getText(), tfFirst.getText(), tfLast.getText()));
    
    btnGoogle.setOnAction(event -> searchGoogle(tfFirst.getText() + " " + tfLast.getText() + " " + HyperTableCell.getCellText(cbField.getSelectionModel().getSelectedItem()), true));  
    btnScholar.setOnAction(event -> btnScholarClick());
    
    invViews = new ArrayList<InvestigationView>();

    ivPerson.setOnMouseClicked(event ->
    {
      PictureDialogController ctrlr = PictureDialogController.create("Edit Picture", viewPort, this);
      
      if (ctrlr.showModal())
        viewPort = ctrlr.getViewPort();
      
      PictureDialogController.httpClient.stop();
      refreshPicture();
    });
    
    initWorkContextMenu();
    initArgContextMenu();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void btnScholarClick()
  {
    String first1 = ultraTrim(removeFirstParenthetical(tfFirst.getText())),
           first2,
           first3,
           last = ultraTrim(tfLast.getText());
    
    int ndx = first1.indexOf(' ');
    
    if (ndx < 0)
    {
      searchScholar(first1 + " " + last, "", "");
      return;
    }
    
    first3 = String.valueOf(first1.charAt(0));
    
    while (ndx >= 0)
    {
      first3 = first3 + String.valueOf(first1.charAt(ndx + 1));
      ndx = first1.indexOf(' ', ndx + 1);
    }     
    
    first3 = first3.toUpperCase();

    first2 = first1.replaceAll("^[^\\s]\\.", "");
    first2 = ultraTrim(first2.replaceAll("\\s[^\\s]\\.", ""));
    
    ndx = first2.indexOf(' ');
    if (ndx >=0)
      first2 = first2.substring(0, ndx);
    
    PopupDialog dlg = new PopupDialog("How should the name be phrased? Initials often works well with Google Scholar.");
    
    dlg.addButton(first1 + " " + last, mrYes);
    dlg.addButton(first2 + " " + last, mrNo);
    dlg.addButton(first3 + " " + last, mrOk);
    dlg.addButton("Cancel", mrCancel);
    
    DialogResult result = dlg.showModal();
    
    switch (result)
    {
      case mrYes : searchScholar(first1 + " " + last, "", ""); break;
      case mrNo  : searchScholar(first2 + " " + last, "", ""); break;
      case mrOk  : searchScholar(first3 + " " + last, "", ""); break;
      default    : break;
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void lblSearchKeyClick()
  {
    updateSearchKey(new PersonName(tfFirst.getText(), tfLast.getText()), true);
  }
  
  private void updateSearchKey(PersonName personName, boolean overwrite)
  {
    if (db.isLoaded() == false) return;
    if (curPerson == null) return;
    
    if (overwrite == false)
      if (curPerson.getSearchKey().length() > 0) return;

    StringBuilder sb = new StringBuilder();
    HDT_Person.makeSearchKey(personName, curPerson, sb);
    tfSearchKey.setText(sb.toString());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void initArgContextMenu()
  {
    for (HDT_RecordType type : new HDT_RecordType[] { hdtArgument, hdtPosition, hdtDebate, hdtTerm, hdtNote, hdtWork, hdtMiscFile, hdtInvestigation, hdtPerson })
    {
      htArguments.addContextMenuItem(type, db.getTypeName(type) + " Record...", record -> ui.goToRecord(record, true));
    }    
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public void initWorkContextMenu()
  {
    RecordListView.addDefaultMenuItems(htWorks);
    
    htWorks.addContextMenuItem(hdtWork, "Go to work record", record ->
      ui.goToRecord(record, true));
    
    htWorks.addContextMenuItem(hdtMiscFile, "Go to file record", record ->
      ui.goToRecord(record, true));
    
    htWorks.addContextMenuItem(hdtWork, "Assign investigations", record ->
      showInvSelectDialog(htWorks.getRowByRecord(record)));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void showInvSelectDialog(HyperTableRow row)
  {
    if (row.getRecord().getType() != hdtWork) return;
    
    HDT_Work work = (HDT_Work) row.getRecord();
    HDT_Person curPerson = (HDT_Person) ui.activeRecord();
    
    InvestigationsDialogController dlg = InvestigationsDialogController.create("Assign investigations - " + work.name(), work, curPerson);
    
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

      PersonTabController personTabCtrlr = HyperTab.getHyperTab(TabEnum.personTab);
      personTabCtrlr.addInvView(inv);
    }
    
    HyperTableCell newValue = new HyperTableCell(work.investigations.isEmpty() ? -1 : work.investigations.get(0).getID(), work.getInvText(curPerson), hdtInvestigation); 
    row.updateCell(3, newValue);
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  class InvestigationView
  {
    public int id;
    public TextField tfName;
    public TextField tfSearchKey;
    public MainTextWrapper textWrapper;
    public Tab tab;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void deleteInvestigation(Event event)
  {
    InvestigationView view = null;
    Tab tab;
    int invID;
    
    if (confirmDialog("Are you sure you want to delete the investigation?"))
    {
      tab = (Tab) event.getSource();
      
      for (InvestigationView iV : invViews)
      {
        if (iV.tab.equals(tab))
        {
          view = iV;
          break;
        }
      }
      
      if (view == null) return;
      
      if (view.id < 1)
        return;           // this is the only scenario where the event is allowed to continue propagating
      
      invID = view.id;
      
      if (ui.cantSaveRecord(true))
      {
        event.consume();
        return;
      }
      
      Iterator<InvestigationView> it = invViews.iterator();
      
      while (it.hasNext())
      {
        view = it.next();
        
        if (view.id == invID)
        {
          event.consume();
          it.remove();
          tpPerson.getTabs().remove(view.tab);
          
          ui.btnSaveClick();
          return;
        }
      }
    }
    
    event.consume();   
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   
  
  public InvestigationView addInvView(HDT_Investigation inv)
  {
    String newName = "", newSearchKey = "";
    InvestigationView iV = new InvestigationView();
    
    iV.id = inv.getID();
    newName = inv.listName();
    newSearchKey = inv.getSearchKey();
  
    iV.tab = new Tab();
    iV.tfName = new TextField(newName);
    iV.tab.setText(newName);
    iV.tfSearchKey = new TextField(newSearchKey);
    
    iV.tab.setOnCloseRequest(this::deleteInvestigation);

    BorderPane bPane = new BorderPane();
    AnchorPane aPane = new AnchorPane();

    GridPane gPane = new GridPane();
    gPane.add(new Label("Investigation name:"), 0, 0); // column=2 row=1
    bPane.setTop(gPane);      
    gPane.add(iV.tfName, 1, 0);
    gPane.add(new Label("Search key:"), 2, 0);
    gPane.add(iV.tfSearchKey, 3, 0);

    ColumnConstraints cc2 = new ColumnConstraints();
    ColumnConstraints cc4 = new ColumnConstraints();

    cc2.setHgrow(Priority.ALWAYS);
    cc4.setHgrow(Priority.ALWAYS);

    gPane.getColumnConstraints().add(new ColumnConstraints());
    gPane.getColumnConstraints().add(cc2);
    gPane.getColumnConstraints().add(new ColumnConstraints());
    gPane.getColumnConstraints().add(cc4);

    gPane.setHgap(3);
    
    iV.textWrapper = new MainTextWrapper(aPane);
    bPane.setCenter(aPane);
    
    iV.textWrapper.loadFromRecord(inv, false, new TextViewInfo()); 
    
    iV.tfName.textProperty().addListener((observable, oldText, newText) -> iV.tab.setText(newText));
    
    iV.tab.setContent(bPane);
    tpPerson.getTabs().add(tpPerson.getTabs().size() - 1, iV.tab);
    
    invViews.add(iV);
    
    return iV;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void tpPersonChange(Tab oldValue, Tab newValue)
  {
    InvestigationView iV = null;
       
    if (oldValue == tabOverview)
      mainText.hide();
    else if (oldValue != tabNew)
    {
      for (InvestigationView view : invViews)
        if (view.tab == oldValue)
          iV = view;
      
      if (iV != null)
        iV.textWrapper.hide();
    }
    
    if (newValue == tabNew)
    {
      HDT_Investigation inv = db.createNewBlankRecord(hdtInvestigation);
      inv.person.set(curPerson);

      iV = addInvView(inv);
      tpPerson.getSelectionModel().select(iV.tab);
      
      final InvestigationView finalIV = iV;
      Platform.runLater(finalIV.tfName::requestFocus);
    }
    else if (newValue == tabOverview)
    {
      htWorks.clearFilter();
      mainText.showReadOnly();     
    }
    else
    {
      for (InvestigationView view : invViews)
        if (view.tab == newValue)
          iV = view;
      
      if (iV == null) return;
      
      if (iV.id < 1)
        htWorks.clearFilter();
      else
      {
        final HDT_Investigation inv = db.investigations.getByID(iV.id);

        htWorks.setFilter(row -> 
        {
          HDT_Base record = row.getRecord();
          if (record == null) return false;
          if (record.getType() != hdtWork) return false;
          
          return HDT_Work.class.cast(record).investigations.contains(inv);
        });
        db.investigations.getByID(iV.id).viewNow(); 
      }
      
      iV.textWrapper.showReadOnly();
    }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void newInstClick(HyperTableRow row, String newName, int colNdx)
  {
    HDT_Institution subInst;
    HDT_Institution parentInst = null;
 
    if (ui.cantSaveRecord(true)) return;
        
    int oldParentID = row.getID(1);
    if ((newName.length() > 0) && (colNdx == 1))
      oldParentID = -1;
    
    NewInstDialogController newInstDialog = NewInstDialogController.create("New Institution or Institutional Division", oldParentID, newName, colNdx == 1);
    
    if (newInstDialog.showModal())
    {
      if (newInstDialog.rbNew.isSelected())
      {
        parentInst = db.createNewBlankRecord(hdtInstitution);
        parentInst.setName(newInstDialog.tfNewParentName.getText());
      }
      else
        parentInst = db.institutions.getByID(newInstDialog.hcbParent.selectedID());

      if (parentInst == null) return;

      subInst = db.createNewBlankRecord(hdtInstitution);

      subInst.parentInst.set(parentInst);
      subInst.setName(newInstDialog.tfName.getText());
      subInst.instType.setID(newInstDialog.hcbType.selectedID());
      
      subInst.setCity(parentInst.getCity());
      subInst.state.set(parentInst.state.get());
      subInst.country.set(parentInst.country.get());

      curPerson.institutions.add(subInst);
      
      if (newInstDialog.rbNew.isSelected()) 
        ui.goToRecord(parentInst, false);
      else
        ui.update();
    }
  } 

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void showInvestigation(int id)
  {
    for (InvestigationView iV : invViews)
    {
      if (iV.id == id)
      {
        tpPerson.getSelectionModel().select(iV.tab);
        safeFocus(iV.tfName);
        break;
      }
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public MainTextWrapper getInvMainTextWrapper(int id)
  {
    for (InvestigationView iV : invViews)
      if (iV.id == id)
        return iV.textWrapper;
    
    return null;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void newClick(HDT_RecordType objType, HyperTableRow row)
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
  
  @FXML private void btnNewWorkClick()
  {
    if (ui.cantSaveRecord(true)) return;

    HDT_Work work = db.createNewBlankRecord(hdtWork);

    work.getAuthors().add(curPerson);

    ui.goToRecord(work, false);
    
    WorkTabController workCtrlr = HyperTab.getHyperTab(workTab);
    
    if (workCtrlr.showWorkDialog(null) == false)
      ui.deleteCurrentRecord(false);
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
    
    for (InvestigationView iV : invViews)
      if (iV.tab.equals(tab))
      {
        iV.textWrapper.hilite(text);
        return;
      }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
  @Override public TextViewInfo getMainTextInfo()
  {
    Tab tab = tpPerson.getSelectionModel().getSelectedItem();
    
    if (tab.equals(tabOverview))
    {
      return mainText.getViewInfo();
    }
    
    for (InvestigationView iV : invViews)
      if (iV.tab.equals(tab))
      {
        return iV.textWrapper.getViewInfo();
      }

    return new TextViewInfo();
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

}
