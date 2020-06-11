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

import java.util.List;

import org.controlsfx.control.MasterDetailPane;
import org.hypernomicon.dialogs.ChooseParentDlgCtrlr;
import org.hypernomicon.dialogs.RenameDlgCtrlr;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTreeCellValue;
import org.hypernomicon.view.wrappers.RecordTreeEdge;
import org.hypernomicon.view.wrappers.TreeModel;
import org.hypernomicon.view.wrappers.TreeRow;
import org.hypernomicon.view.wrappers.TreeWrapper;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.dialogs.RenameDlgCtrlr.NameType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import javafx.application.Platform;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.concurrent.Worker;
import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableView;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public class TreeTabCtrlr extends HyperTab<HDT_Record, HDT_Record>
{
  @FXML private TreeTableView<TreeRow> ttv;
  @FXML private TextField tfPath;
  @FXML private TreeTableColumn<TreeRow, HyperTreeCellValue> tcName;
  @FXML private TreeTableColumn<TreeRow, String> tcType, tcDesc;
  @FXML private MasterDetailPane spMain;
  @FXML private CheckBox chkShowDesc;
  @FXML private WebView webView;

  private TreeModel<TreeRow> debateTree, termTree, labelTree, noteTree;
  private boolean useViewInfo = false;
  private String lastTextHilited = "";

  public String textToHilite = "";
  private TreeWrapper tree;

  @Override protected RecordType getType()        { return hdtNone; }
  @Override public void enable(boolean enabled)   { ui.tabTree.getContent().setDisable(enabled == false); }
  @Override public void clear()                   { tree.clear(); }
  @Override public boolean saveToRecord()         { return true; }
  @Override public void setRecord(HDT_Record rec) { return; }
  @Override public HDT_Record activeRecord()      { return tree.selectedRecord(); }
  @Override public String recordName()            { return nullSwitch(activeRecord(), "", HDT_Record::getCBText); }
  @Override public TextViewInfo mainTextInfo()    { return new TextViewInfo(MainTextUtil.webEngineScrollPos(webView.getEngine())); }
  @Override public void setDividerPositions()     { return; }
  @Override public void getDividerPositions()     { return; }

  public TreeWrapper getTree() { return tree; }

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

  @Override protected void init()
  {
    tree = new TreeWrapper(ttv, true, ui.cbTreeGoTo, false);

    debateTree = tree.debateTree;
    termTree   = tree.termTree;
    labelTree  = tree.labelTree;
    noteTree   = tree.noteTree;

    spMain.showDetailNodeProperty().bind(chkShowDesc.selectedProperty());

    tcName.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue().getNameCell()));
    tcType.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getTypeString()));
    tcDesc.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getDescString()));

    tree.addContextMenuItem("Select", HDT_Record.class,
      record -> (ui.treeSelector.getBase() != null) && (record != null) && db.isLoaded(),
      record -> ui.treeSelect());

    tree.addContextMenuItem("Go to this record", HDT_Record.class,
      record -> (record != null) && db.isLoaded(),
      record -> ui.goToRecord(record, false));

    tree.addContextMenuItem("Choose parent to assign", HDT_Record.class,
      record ->
      {
        if ((db.isLoaded() == false) || (record == null)) return false;
        return record.getType() != hdtConcept;
      },
      this::chooseParent);

    tree.addContextMenuItem("Detach from this parent",
      row -> tree.canDetach(row, false),
      row -> tree.canDetach(row, true));

    tree.addContextMenuItem("Rename...", HDT_WorkLabel.class,
      label -> db.isLoaded(),
      this::renameRecord);

    tree.addContextMenuItem("Rename...", HDT_Glossary.class,
      glossary -> db.isLoaded(),
      this::renameRecord);

    tree.addContextMenuItem("New label under this record...", HDT_WorkLabel.class,
      label -> db.isLoaded(),
      this::createLabel);

    tree.addContextMenuItem("New debate under this debate...", HDT_Debate.class,
      debate -> db.isLoaded(),
      debate -> createChild(debate, rtParentDebateOfDebate));

    tree.addContextMenuItem("New position under this debate...", HDT_Debate.class,
      debate -> db.isLoaded(),
      debate -> createChild(debate, rtDebateOfPosition));

    tree.addContextMenuItem("New position under this position...", HDT_Position.class,
      pos -> db.isLoaded(),
      pos -> createChild(pos, rtParentPosOfPos));

    tree.addContextMenuItem("New argument under this position...", HDT_Position.class,
      pos -> db.isLoaded(),
      pos -> createChild(pos, rtPositionOfArgument));

    tree.addContextMenuItem("New counterargument under this argument...", HDT_Argument.class,
      arg -> db.isLoaded(),
      arg -> createChild(arg, rtCounterOfArgument));

    tree.addContextMenuItem("New note under this note...", HDT_Note.class,
      note -> db.isLoaded(),
      note -> createChild(note, rtParentNoteOfNote));

    tree.addContextMenuItem("New glossary under this glossary...", HDT_Glossary.class,
      glossary -> db.isLoaded(),
      this::createGlossary);

    tree.addDefaultMenuItems();

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      textToHilite = lastTextHilited;
      String mainText = "";

      HDT_Record record = tree.selectedRecord();
      if (record == null) return;

      if (record.hasDesc())
        mainText = HDT_RecordWithDescription.class.cast(record).getDesc().getHtml();

      MainTextUtil.handleJSEvent(MainTextUtil.prepHtmlForDisplay(mainText), webView.getEngine(), new TextViewInfo());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.getEngine().getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
    {
      if (newState == Worker.State.SUCCEEDED)
      {
        if (textToHilite.length() > 0)
          MainTextWrapper.hiliteText(textToHilite, webView.getEngine());

        lastTextHilited = textToHilite;
        textToHilite = "";
      }
    });

    MainTextUtil.webViewAddZoom(webView, PREF_KEY_TREETAB_ZOOM);

    ttv.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      boolean clearWV = true, clearPreview = true;

      if (newValue != null)
      {
        tfPath.setText(getTreePath(ttv, newValue));
        ui.updateBottomPanel(true);

        TreeRow row = newValue.getValue();

        HDT_Record record = row.getRecord();
        if (record != null)
        {
          switch (record.getType())
          {
            case hdtWorkLabel : case hdtGlossary :

              if (record.getID() > 1)
                record.viewNow();

              break;

            case hdtWork :

              HDT_Work work = (HDT_Work)record;
              previewWindow.setPreview(pvsTreeTab, work.filePath(), work.getStartPageNum(), work.getEndPageNum(), work);
              clearPreview = false;
              break;

            case hdtMiscFile :

              HDT_MiscFile miscFile = (HDT_MiscFile)record;
              previewWindow.setPreview(pvsTreeTab, miscFile.filePath(), -1, -1, miscFile);
              clearPreview = false;
              break;

            default : break;
          }

          String desc = record.hasDesc() ? ((HDT_RecordWithDescription)record).getDesc().getHtml() : "";

          MainTextWrapper.setReadOnlyHTML(desc, webView.getEngine(), useViewInfo ? getView().getTextInfo() : new TextViewInfo(), null);
          clearWV = false;
        }
      }
      else
        tfPath.clear();

      if (clearWV && (ui.isShuttingDown() == false))
        webView.getEngine().loadContent("");

      if (clearPreview)
        previewWindow.setPreview(pvsTreeTab, null, -1, -1, null);
    });

  //---------------------------------------------------------------------------
  //
  // Tree Update Handlers
  //
  //---------------------------------------------------------------------------

  // NOTE: There is some code (like RecordTreeEdge constructor) that assumes that
  //       if the subject and object type is the same, then the child record is the
  //       subject record, so there could be bugs if a non-forward relation is added
  //       where the subject and object type is the same

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

    noteTree.addParentChildRelation(rtParentNoteOfNote, true);

    labelTree.addParentChildRelation(rtParentLabelOfLabel, true);
    labelTree.addParentChildRelation(rtLabelOfFile, true);
    labelTree.addParentChildRelation(rtLabelOfWork, true);
    labelTree.addParentChildRelation(rtWorkOfArgument, true);

    termTree.addParentChildRelation(rtGlossaryOfConcept, true);
    termTree.addParentChildRelation(rtParentGlossaryOfGlossary, true);

    List.of(debateTree, noteTree, labelTree, termTree).forEach(treeModel ->
    {
      treeModel.addParentChildRelation(rtParentWorkOfWork, true);
      treeModel.addParentChildRelation(rtWorkOfMiscFile, true);
    });

    db.addCloseDBHandler(tree::reset);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createChild(HDT_Record parent, RelationType relType)
  {
    HDT_Record child = db.createNewBlankRecord(db.getSubjType(relType));

    db.getObjectList(relType, child, true).add(parent);

    if (ui.treeSelector.getBase() != null)
    {
      RelationType selRelType = ui.treeSelector.getRelTypeForTargetType(child.getType());

      if (selRelType == rtUnited)
      {
        if (ui.treeSelector.selectToUnite((HDT_RecordWithConnector) child, false))
          return;
      }
      else if (selRelType != rtNone)
        ui.treeSelector.select(child, false);
    }

    ui.goToRecord(child, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createGlossary(HDT_Glossary glossary)
  {
    RenameDlgCtrlr dlg = RenameDlgCtrlr.build("Glossary Name", ntRecord, "");

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
    RenameDlgCtrlr dlg = RenameDlgCtrlr.build("Label Name", ntRecord, "");

    if (dlg.showModal())
    {
      HDT_WorkLabel newLabel = db.createNewBlankRecord(hdtWorkLabel);
      newLabel.setName(dlg.getNewName());
      newLabel.parentLabels.add(label);

      Platform.runLater(() -> { tree.sort(); tree.selectRecord(newLabel, 0, false); });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void renameRecord(HDT_Record record)
  {
    String typeName = db.getTypeName(record.getType());

    if (isUnstoredRecord(record.getID(), record.getType()))
    {
      messageDialog("That " + typeName + " cannot be renamed.", mtError);
      return;
    }

    RenameDlgCtrlr dlg = RenameDlgCtrlr.build(typeName + " Name", ntRecord, record.name());

    if (dlg.showModal())
    {
      record.setName(dlg.getNewName());
      ui.update();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void chooseParent(HDT_Record child)
  {
    ChooseParentDlgCtrlr dlg = ChooseParentDlgCtrlr.build(child);

    if (dlg.showModal() == false) return;

    new RecordTreeEdge(dlg.getParent(), child).attach(null, true);

    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getTreePath(TreeTableView<TreeRow> ttv2, TreeItem<TreeRow> newValue)
  {
    if (newValue == null) return "";

    String caption = nullSwitch(newValue.getValue(), "", TreeRow::getName);

    return newValue.getParent() == ttv2.getRoot() ? caption : (getTreePath(ttv2, newValue.getParent()) + " / " + caption);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void findWithinDesc(String text)
  {
    if (tree.selectedRecord() != null)
      MainTextWrapper.hiliteText(text, webView.getEngine());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getRecordCount()
  {
    return nullSwitch(activeRecord(), 0, ar -> tree.getRowsForRecord(ar).size());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getRecordNdx()
  {
    return nullSwitch(nullSwitch(tree.selectedItem(), null, TreeItem::getValue), -1, row -> tree.getRowsForRecord(row.getRecord()).indexOf(row));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectRecord(HDT_Record record, boolean useViewInfo)
  {
    this.useViewInfo = useViewInfo;
    tree.selectRecord(record, record == null ? 0 : db.records(record.getType()).getKeyNdxByID(record.getID()), false);
    this.useViewInfo = false;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
