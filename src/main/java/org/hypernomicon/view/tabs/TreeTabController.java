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

import java.util.EnumSet;
import java.util.List;

import org.controlsfx.control.MasterDetailPane;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.ChooseParentDialogController;
import org.hypernomicon.view.dialogs.RenameDialogController;
import org.hypernomicon.view.dialogs.VerdictDialogController;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.DragNDropHoverHelper;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.HyperTreeCellValue;
import org.hypernomicon.view.wrappers.TreeModel;
import org.hypernomicon.view.wrappers.TreeRow;
import org.hypernomicon.view.wrappers.TreeWrapper;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.dialogs.RenameDialogController.NameType.*;
import static org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource.*;

import javafx.application.Platform;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ChangeListener;
import javafx.concurrent.Worker;
import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableRow;
import javafx.scene.control.TreeTableView;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public class TreeTabController extends HyperTab<HDT_Base, HDT_Base>
{ 
  @FXML private TreeTableView<TreeRow> ttv;
  @FXML private TextField tfPath;
  @FXML private TreeTableColumn<TreeRow, HyperTreeCellValue> tcName;
  @FXML private TreeTableColumn<TreeRow, String> tcType;
  @FXML private TreeTableColumn<TreeRow, String> tcDesc;
  @FXML private MasterDetailPane spMain;
  @FXML private CheckBox chkShowDesc;
  @FXML private WebView webView;

  TreeModel<TreeRow> debateTree, termTree;
  private TreeModel<TreeRow> labelTree, noteTree;
  private boolean useViewInfo = false;
  public String textToHilite = "";
  private String lastTextHilited = "";
  public TreeWrapper tree;
  
  @Override public HDT_RecordType getType()                  { return hdtNone; }
  @Override public void enable(boolean enabled)              { ui.tabTree.getContent().setDisable(enabled == false); }
  @Override public void clear()                              { tree.clear(); }
  @Override public boolean saveToRecord(boolean showMessage) { return true; }
  @Override public void focusOnSearchKey()                   { return; }
  @Override public void setRecord(HDT_Base activeRecord)     { return; }
  @Override public HDT_Base activeRecord()                   { return tree.selectedRecord(); }
  @Override public TextViewInfo getMainTextInfo()            { return new TextViewInfo(MainTextWrapper.getWebEngineScrollPos(webView.getEngine())); }
  @Override public void setDividerPositions()                { return; }
  @Override public void getDividerPositions()                { return; }

  @Override public void newClick(HDT_RecordType objType, HyperTableRow row) { return; }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public boolean update()
  { 
    if (db.isLoaded() == false)
    {
      tree.clear();
      return true;
    }
       
    ttv.getColumns().forEach(col ->
    {
      col.setVisible(false);
      col.setVisible(true);
    });
    
    tree.sort();
    return true;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override protected void init(TabEnum tabEnum)
  {      
    this.tabEnum = tabEnum;    
    tree = new TreeWrapper(ttv, true, ui.cbTreeGoTo);
       
    debateTree = tree.debateTree;
    termTree   = tree.termTree;
    labelTree  = tree.labelTree;
    noteTree   = tree.noteTree;
    
    spMain.showDetailNodeProperty().bind(chkShowDesc.selectedProperty());
    
    tcName.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue().getNameCell()));
    tcType.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getTypeString()));
    tcDesc.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getDescString()));
    
    ttv.setRowFactory(tTV -> 
    {
      TreeTableRow<TreeRow> row = new TreeTableRow<>();
      
      DragNDropHoverHelper.setupHandlers(row, tree);

      row.itemProperty().addListener((observable, oldValue, newValue) ->
      {
        if (newValue == null)
          row.setContextMenu(null);
        else
          row.setContextMenu(tree.createContextMenu(newValue));
      });
      
      return row;
    });
    
    tree.addCondContextMenuItem(hdtNone, "Select", 
      record -> (ui.treeSubjRecord != null) && (record != null) && (db.isLoaded()), 
      record -> ui.treeSelect());
    
    tree.addCondContextMenuItem(hdtNone, "Go to this record", 
      record -> (record != null) && (db.isLoaded()),
      record -> ui.goToRecord(record, false));
    
    tree.addCondContextMenuItem(hdtNone, "Choose parent to assign", 
      record -> 
      {
        if ((db.isLoaded() == false) || (record == null)) return false;
        return (record.getType() != hdtConcept);
      },
      this::chooseParent);

    tree.addCondContextMenuItem(hdtNone, "Detach from this parent", 
        record -> tree.canDetach(false),
        record -> tree.canDetach(true));
    
    tree.addCondContextMenuItem(hdtWork, "Launch file...", 
        record -> (HDT_Work.class.cast(record).canLaunch()) && (db.isLoaded()),
        record -> 
        { 
          HDT_Work.class.cast(record).launch(-1);
        });

    tree.addCondContextMenuItem(hdtMiscFile, "Launch file...", 
        record -> (HDT_MiscFile.class.cast(record).getPath().isEmpty() == false) && (db.isLoaded()),
        record -> 
        {
          HDT_MiscFile miscFile = HDT_MiscFile.class.cast(record);
          miscFile.viewNow();
          launchFile(miscFile.getPath().getFilePath()); 
        });
    
    tree.addCondContextMenuItem(hdtWorkLabel, "Rename...",
        record -> db.isLoaded(),
        this::renameRecord);
    
    tree.addCondContextMenuItem(hdtGlossary, "Rename...",
        record -> db.isLoaded(),
        this::renameRecord);    
    
    tree.addCondContextMenuItem(hdtWorkLabel, "New label under this record...",
        record -> db.isLoaded(),
        record -> createLabel((HDT_WorkLabel) record));
    
    tree.addCondContextMenuItem(hdtDebate, "New debate under this debate...",
        record -> db.isLoaded(),
        record -> createChild(record, rtParentDebateOfDebate));

    tree.addCondContextMenuItem(hdtDebate, "New position under this debate...",
        record -> db.isLoaded(),
        record -> createChild(record, rtDebateOfPosition));

    tree.addCondContextMenuItem(hdtPosition, "New position under this position...",
        record -> db.isLoaded(),
        record -> createChild(record, rtParentPosOfPos));

    tree.addCondContextMenuItem(hdtPosition, "New argument under this position...",
        record -> db.isLoaded(),
        record -> createChild(record, rtPositionOfArgument));

    tree.addCondContextMenuItem(hdtArgument, "New counterargument under this argument...",
        record -> db.isLoaded(),
        record -> createChild(record, rtCounterOfArgument));

    tree.addCondContextMenuItem(hdtNote, "New note under this note...",
        record -> db.isLoaded(),
        record -> createChild(record, rtParentNoteOfNote));
    
    tree.addCondContextMenuItem(hdtGlossary, "New glossary under this glossary...",
        record -> db.isLoaded(),
        record -> createGlossary((HDT_Glossary) record));
    
    webView.getEngine().titleProperty().addListener((ChangeListener<String>) (observable, oldValue, newValue) ->
    {
      textToHilite = lastTextHilited;      
      String mainText = "";
      
      HDT_Base record = tree.selectedRecord();
      if (record == null) return;
      
      if (record.hasDesc())
        mainText = HDT_RecordWithDescription.class.cast(record).getDesc().getHtml();
      
      MainTextWrapper.handleJSEvent(getHtmlEditorText(mainText), webView.getEngine(), new TextViewInfo());
    });
    
    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());
    
    webView.getEngine().getLoadWorker().stateProperty().addListener((ChangeListener<Worker.State>) (ov, oldState, newState) -> 
    {
      if (newState == Worker.State.SUCCEEDED) 
      {        
        if (textToHilite.length() > 0)
          MainTextWrapper.hiliteText(textToHilite, webView.getEngine());
        
        lastTextHilited = textToHilite;
        textToHilite = "";
      }
    });
    
    MainTextWrapper.webViewAddZoom(webView, PREF_KEY_TREETAB_ZOOM);
    
    ttv.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      boolean clearWV = true, clearPreview = true;
      HDT_Base record = null;
      
      if (newValue != null)
      {
        tfPath.setText(getTreePath(ttv, newValue));
        ui.updateBottomPanel(true);
        
        TreeRow row = newValue.getValue();        
        
        record = row.getRecord();
        if (record != null)
        {
          String desc = "";
          
          if ((record.getType() == hdtWorkLabel) || (record.getType() == hdtGlossary))
            if (record.getID() > 1)
              record.viewNow();
          
          if (record.getType() == hdtWork)
          {
            HDT_Work work = (HDT_Work)record;
            previewWindow.setPreview(pvsTreeTab, work.getPath().getFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
            clearPreview = false;
          }
          else if (record.getType() == hdtMiscFile)
          {
            HDT_MiscFile miscFile = (HDT_MiscFile)record;
            previewWindow.setPreview(pvsTreeTab, miscFile.getPath().getFilePath(), -1, -1, miscFile);
            clearPreview = false;
          }
          
          if (record.hasDesc())
            desc = ((HDT_RecordWithDescription)record).getDesc().getHtml();
          
          if (useViewInfo)
            MainTextWrapper.setReadOnlyHTML(getHtmlEditorText(desc), webView.getEngine(), getView().getTextInfo(), null);
          else
            MainTextWrapper.setReadOnlyHTML(getHtmlEditorText(desc), webView.getEngine(), new TextViewInfo(), null);
          clearWV = false;
        }
      }
      else
        tfPath.clear();
            
      if (clearWV)
        webView.getEngine().loadContent("");
      
      if (clearPreview)
        previewWindow.setPreview(pvsTreeTab, null, -1, -1, null);
    });
    
  //--------------------------------------------------------------------------- 
  //
  // Tree Update Handlers
  //  
  //--------------------------------------------------------------------------- 
    
    db.addDeleteHandler(tree::removeRecord);
    
    noteTree.addKeyWorkRelation(hdtNote, true);
    termTree.addKeyWorkRelation(hdtConcept, true);
    debateTree.addKeyWorkRelation(hdtDebate, true);
    debateTree.addKeyWorkRelation(hdtPosition, true);
    
    debateTree.addParentChildRelation(rtParentDebateOfDebate, true);
    debateTree.addParentChildRelation(rtDebateOfPosition, true);
    debateTree.addParentChildRelation(rtParentPosOfPos, true);
    debateTree.addParentChildRelation(rtPositionOfArgument, true);
    debateTree.addParentChildRelation(rtCounterOfArgument, true);
    debateTree.addParentChildRelation(rtWorkOfArgument, false);
    debateTree.addParentChildRelation(rtParentWorkOfWork, true);
    debateTree.addParentChildRelation(rtWorkOfMiscFile, true);

    noteTree.addParentChildRelation(rtParentNoteOfNote, true);
    
    labelTree.addParentChildRelation(rtParentLabelOfLabel, true);
    labelTree.addParentChildRelation(rtLabelOfFile, true);
    labelTree.addParentChildRelation(rtLabelOfWork, true);
    labelTree.addParentChildRelation(rtParentWorkOfWork, true);
    labelTree.addParentChildRelation(rtWorkOfMiscFile, true);
    labelTree.addParentChildRelation(rtWorkOfArgument, true);
    
    termTree.addParentChildRelation(rtGlossaryOfConcept, true);
    termTree.addParentChildRelation(rtParentGlossaryOfGlossary, true);
    
    db.addCloseDBHandler(tree::reset);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void createChild(HDT_Base parent, RelationType relType)
  {
    HDT_Base child = db.createNewBlankRecord(db.getSubjType(relType));
    
    db.getObjectList(relType, child, true).add(parent); 
    
    ui.goToRecord(child, false);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void createGlossary(HDT_Glossary glossary)
  {
    RenameDialogController dlg = RenameDialogController.create("Glossary name", ntRecord, "");
    
    if (dlg.showModal())
    {
      HDT_Glossary newGlossary = db.createNewBlankRecord(hdtGlossary);
      newGlossary.setActive(true);
      newGlossary.setName(dlg.getNewName());
      newGlossary.parentGlossaries.add(glossary);
      
      Platform.runLater(() -> { tree.sort(); tree.selectRecord(newGlossary, 0, false); });
    }  
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void createLabel(HDT_WorkLabel label)
  {
    RenameDialogController dlg = RenameDialogController.create("Label name", ntRecord, "");
    
    if (dlg.showModal())
    {
      HDT_WorkLabel newLabel = db.createNewBlankRecord(hdtWorkLabel);
      newLabel.setName(dlg.getNewName());
      newLabel.parentLabels.add(label);
      
      Platform.runLater(() -> tree.selectRecord(newLabel, 0, false));
    }  
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void renameRecord(HDT_Base record)
  {
    String typeName = db.getTypeName(record.getType());
    
    if (HyperDB.isUnstoredRecord(record.getID(), record.getType()))
    {
      messageDialog("That " + typeName + " cannot be renamed.", mtError);
      return;
    }
    
    RenameDialogController dlg = RenameDialogController.create(typeName + " name", ntRecord, record.name());
    
    if (dlg.showModal())
    {
      record.setName(dlg.getNewName());
      update();
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  private void chooseParent(HDT_Base child)
  {
    EnumSet<HDT_RecordType> types = EnumSet.noneOf(HDT_RecordType.class);
    HDT_Base parent;
        
    switch (child.getType())
    {
      case hdtWorkLabel : 
        
        types.add(hdtWorkLabel); 
        break;
        
      case hdtNote : 
        
        types.add(hdtNote); 
        break;
        
      case hdtPosition : 
        
        types.add(hdtDebate);
        types.add(hdtPosition);
        break;
      
      case hdtDebate :
        
        types.add(hdtDebate);
        break;
        
      case hdtArgument :
        
        types.add(hdtPosition);
        types.add(hdtArgument);
        break;
        
      case hdtWork :
        
        types.add(hdtArgument);
        types.add(hdtWork);
        types.add(hdtWorkLabel);
        break;
        
      case hdtMiscFile :
        
        types.add(hdtWork);
        types.add(hdtWorkLabel);
        break;
      
      default :
        break;        
    }
    
    ChooseParentDialogController dlg = ChooseParentDialogController.create("Record select", child, types);
    
    if (dlg.showModal() == false) return;

    parent = dlg.parent;
    
    switch (child.getType())
    {
      case hdtWorkLabel : 
        
        HDT_WorkLabel childLabel = (HDT_WorkLabel)child;
        childLabel.parentLabels.add((HDT_WorkLabel) parent);
        break;
        
      case hdtNote : 
        
        HDT_Note childNote = (HDT_Note)child;
        childNote.parentNotes.add((HDT_Note) parent);
        break;
        
      case hdtPosition : 
        
        HDT_Position childPos = (HDT_Position) child;
        if (parent.getType() == hdtDebate)
          childPos.debates.add((HDT_Debate) parent);
        else
          childPos.largerPositions.add((HDT_Position) parent);
        break;
      
      case hdtDebate :

        HDT_Debate childDebate = (HDT_Debate) child;
        childDebate.largerDebates.add((HDT_Debate) parent);
        break;
        
      case hdtArgument :
        
        HDT_Argument childArg = (HDT_Argument) child;
        
        VerdictDialogController vdc = VerdictDialogController.create("Select Verdict for " + childArg.getCBText(), parent);
        
        if (vdc.showModal())
        {
          if (parent.getType() == hdtPosition)
            childArg.addPosition((HDT_Position) parent, vdc.hcbVerdict.selectedRecord());
          else if (parent.getType() == hdtArgument)
            childArg.addCounterArg((HDT_Argument)parent, vdc.hcbVerdict.selectedRecord());
        }
        break;
        
      case hdtWork :
        
        HDT_Work childWork = (HDT_Work) child;
        if (parent.getType() == hdtWork)
          childWork.setLargerWork(parent.getID(), false);
        else if (parent.getType() == hdtArgument)
        {
          childArg = (HDT_Argument)parent;
          childArg.works.add(childWork);
        }
        else
          childWork.labels.add((HDT_WorkLabel) parent);
        break;
        
      case hdtMiscFile :
        
        HDT_MiscFile childFile = (HDT_MiscFile) child;
        if (parent.getType() == hdtWork)
          childFile.work.set((HDT_Work) parent);
        else
          childFile.labels.add((HDT_WorkLabel) parent);
        
        break;
        
      default :
        break;
    }
    
    update();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static String getTreePath(TreeTableView<TreeRow> ttv2, TreeItem<TreeRow> newValue)
  {
    String caption;
    if (newValue == null)
      return "";
    TreeRow row = newValue.getValue();
    if (row == null)
      caption = "";
    else
      caption = newValue.getValue().getName();
    
    if (newValue.getParent() == ttv2.getRoot())
      return caption;
    else
      return getTreePath(ttv2, newValue.getParent()) + " / " + caption;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public void findWithinDesc(String text)
  {
    if (tree.selectedRecord() == null) return;
    MainTextWrapper.hiliteText(text, webView.getEngine());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getRecordCount()
  {
    if (activeRecord() == null) return 0;
    List<TreeRow> list = tree.getRowsForRecord(activeRecord());
    if (list == null) return 0;
    return list.size();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public int getRecordNdx()
  {
    TreeRow row = null;
    
    TreeItem<TreeRow> item = tree.selectedItem();
    if (item != null) 
      row = item.getValue();

    if (row != null)
    {
      List<TreeRow> list = tree.getRowsForRecord(row.getRecord());
          
      if (list != null)
        return list.indexOf(row);
    }
    
    return -1;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public void selectRecord(HDT_Base record, boolean useViewInfo)
  {
    this.useViewInfo = useViewInfo;
    tree.selectRecord(record, record == null ? 0 : db.records(record.getType()).getKeyNdxByID(record.getID()), false);
    this.useViewInfo = false;    
  }
 
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  
}
