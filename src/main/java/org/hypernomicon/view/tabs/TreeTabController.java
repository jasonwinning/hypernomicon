/*
 * Copyright 2015-2019 Jason Winning
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
import javafx.scene.control.TreeTableView;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public class TreeTabController extends HyperTab<HDT_Base, HDT_Base>
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

  @Override HDT_RecordType getType()                { return hdtNone; }
  @Override public void enable(boolean enabled)     { ui.tabTree.getContent().setDisable(enabled == false); }
  @Override public void clear()                     { tree.clear(); }
  @Override public boolean saveToRecord(boolean sm) { return true; }
  @Override void focusOnSearchKey()                 { return; }
  @Override public void setRecord(HDT_Base ar)      { return; }
  @Override public HDT_Base activeRecord()          { return tree.selectedRecord(); }
  @Override public TextViewInfo getMainTextInfo()   { return new TextViewInfo(MainTextWrapper.getWebEngineScrollPos(webView.getEngine())); }
  @Override public void setDividerPositions()       { return; }
  @Override public void getDividerPositions()       { return; }

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

  @Override void init(TabEnum tabEnum)
  {
    this.tabEnum = tabEnum;
    tree = new TreeWrapper(ttv, true, ui.cbTreeGoTo, false);

    debateTree = tree.debateTree;
    termTree   = tree.termTree;
    labelTree  = tree.labelTree;
    noteTree   = tree.noteTree;

    spMain.showDetailNodeProperty().bind(chkShowDesc.selectedProperty());

    tcName.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue().getNameCell()));
    tcType.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getTypeString()));
    tcDesc.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getDescString()));

    tree.addContextMenuItem("Select", HDT_Base.class,
      record -> (ui.treeSubjRecord != null) && (record != null) && db.isLoaded(),
      record -> ui.treeSelect());

    tree.addContextMenuItem("Go to this record", HDT_Base.class,
      record -> (record != null) && db.isLoaded(),
      record -> ui.goToRecord(record, false));

    tree.addContextMenuItem("Choose parent to assign", HDT_Base.class,
      record ->
      {
        if ((db.isLoaded() == false) || (record == null)) return false;
        return record.getType() != hdtConcept;
      },
      this::chooseParent);

    tree.addContextMenuItem("Detach from this parent", HDT_Base.class,
      record -> tree.canDetach(false),
      record -> tree.canDetach(true));

    tree.addContextMenuItem("Launch file...", HDT_Work.class,
      work -> work.canLaunch() && db.isLoaded(),
      work -> work.launch(-1));

    tree.addContextMenuItem("Launch file...", HDT_MiscFile.class,
      miscFile -> (miscFile.getPath().isEmpty() == false) && db.isLoaded(),
      miscFile ->
      {
        miscFile.viewNow();
        launchFile(miscFile.getPath().getFilePath());
      });

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

      if (newValue != null)
      {
        tfPath.setText(getTreePath(ttv, newValue));
        ui.updateBottomPanel(true);

        TreeRow row = newValue.getValue();

        HDT_Base record = row.getRecord();
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
              previewWindow.setPreview(pvsTreeTab, work.getPath().getFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
              clearPreview = false;
              break;

            case hdtMiscFile :

              HDT_MiscFile miscFile = (HDT_MiscFile)record;
              previewWindow.setPreview(pvsTreeTab, miscFile.getPath().getFilePath(), -1, -1, miscFile);
              clearPreview = false;
              break;

            default : break;
          }

          String desc = record.hasDesc() ? ((HDT_RecordWithDescription)record).getDesc().getHtml() : "";

          MainTextWrapper.setReadOnlyHTML(getHtmlEditorText(desc), webView.getEngine(), useViewInfo ? getView().getTextInfo() : new TextViewInfo(), null);
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
      ui.update();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void chooseParent(HDT_Base child)
  {
    EnumSet<HDT_RecordType> types = null;

    switch (child.getType())
    {
      case hdtWorkLabel : types = EnumSet.of(hdtWorkLabel);                       break;
      case hdtNote      : types = EnumSet.of(hdtNote);                            break;
      case hdtPosition  : types = EnumSet.of(hdtDebate, hdtPosition);             break;
      case hdtDebate    : types = EnumSet.of(hdtDebate);                          break;
      case hdtArgument  : types = EnumSet.of(hdtPosition, hdtArgument);           break;
      case hdtWork      : types = EnumSet.of(hdtArgument, hdtWork, hdtWorkLabel); break;
      case hdtMiscFile  : types = EnumSet.of(hdtWork, hdtWorkLabel);              break;
      case hdtGlossary  : types = EnumSet.of(hdtGlossary);                        break;
      default           :                                                         break;
    }

    ChooseParentDialogController dlg = ChooseParentDialogController.create("Record select", child, types);

    if (dlg.showModal() == false) return;

    HDT_Base parent = dlg.getParent();

    switch (child.getType())
    {
      case hdtWorkLabel :

        HDT_WorkLabel.class.cast(child).parentLabels.add((HDT_WorkLabel) parent);
        break;

      case hdtNote :

        HDT_Note.class.cast(child).parentNotes.add((HDT_Note) parent);
        break;

      case hdtPosition :

        if (parent.getType() == hdtDebate)
          HDT_Position.class.cast(child).debates.add((HDT_Debate) parent);
        else
          HDT_Position.class.cast(child).largerPositions.add((HDT_Position) parent);
        break;

      case hdtDebate :

        HDT_Debate.class.cast(child).largerDebates.add((HDT_Debate) parent);
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

        if (parent.getType() == hdtWork)
          HDT_MiscFile.class.cast(child).work.set((HDT_Work) parent);
        else
          HDT_MiscFile.class.cast(child).labels.add((HDT_WorkLabel) parent);

        break;

      default :
        break;
    }

    ui.update();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getTreePath(TreeTableView<TreeRow> ttv2, TreeItem<TreeRow> newValue)
  {
    if (newValue == null) return "";

    String caption = nullSwitch(newValue.getValue(), "", TreeRow::getName);

    if (newValue.getParent() == ttv2.getRoot())
      return caption;
    else
      return getTreePath(ttv2, newValue.getParent()) + " / " + caption;
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

  public void selectRecord(HDT_Base record, boolean useViewInfo)
  {
    this.useViewInfo = useViewInfo;
    tree.selectRecord(record, record == null ? 0 : db.records(record.getType()).getKeyNdxByID(record.getID()), false);
    this.useViewInfo = false;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
