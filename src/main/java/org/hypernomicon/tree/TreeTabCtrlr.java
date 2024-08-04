/*
 * Copyright 2015-2024 Jason Winning
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

package org.hypernomicon.tree;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.controlsfx.control.BreadCrumbBar;
import org.controlsfx.control.MasterDetailPane;
import org.hypernomicon.App;
import org.hypernomicon.dialogs.RenameDlgCtrlr;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.model.unities.HDT_RecordWithDescription;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.Highlighter;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.PositionTabCtrlr;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.MenuItemSchema;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.SetMultimap;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.dialogs.RenameDlgCtrlr.NameType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import javafx.application.Platform;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.concurrent.Worker;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.CheckBox;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Tab;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public class TreeTabCtrlr extends HyperTab<HDT_Record, HDT_Record>
{
  @FXML private BreadCrumbBar<TreeRow> bcbPath;
  @FXML private MasterDetailPane spMain;
  @FXML private CheckBox chkShowDesc;
  @FXML private WebView webView;

  private final SetMultimap<RecordType, MenuItemSchema<? extends HDT_Record, TreeRow>> recordTypeToSchemas = LinkedHashMultimap.create();
  private final TreeWrapper tree;
  private final Highlighter highlighter;

  private TreeTableView<TreeRow> ttv;
  private boolean loaded = false;
  private String lastTextHilited = "";
  String textToHilite = "";


//---------------------------------------------------------------------------

  public TreeTabCtrlr(Tab tab) throws IOException
  {
    super(TabEnum.treeTabEnum, tab, "tree/TreeTab");

    tree = new TreeWrapper(bcbPath, true, ui.cbTreeGoTo);

    highlighter = new Highlighter(webView);

    initTTV();

    spMain.showDetailNodeProperty().bind(chkShowDesc.selectedProperty());

    tree.addContextMenuItem("Select", HDT_Record.class,
      record -> (ui.treeSelector.getBase() != null) && (record != null) && db.isLoaded(),
      record -> ui.treeSelector.select(record, true));

    tree.addContextMenuItem("Go to this record", HDT_Record.class,
      record -> (record != null) && db.isLoaded(),
      record -> ui.goToRecord(record, false));

    tree.addContextMenuItem("Choose parent to assign", HDT_Record.class,
      record ->
      {
        if ((db.isLoaded() == false) || (record == null)) return false;
        return (record.getType() != hdtConcept) && (record.getType() != hdtGlossary);
      },
      TreeTabCtrlr::chooseParent);

    tree.addContextMenuItem("Detach from this parent",
      row -> tree.canDetach(row, false),
      row -> tree.canDetach(row, true));

    tree.addContextMenuItem("Rename...", HDT_WorkLabel.class,
      label -> db.isLoaded(),
      TreeTabCtrlr::renameRecord);

    tree.addContextMenuItem("Rename...", HDT_Glossary.class,
      glossary -> db.isLoaded(),
      TreeTabCtrlr::renameRecord);

    addCreateNewSchema(tree.addContextMenuItem("Create new sub-label under this label", HDT_WorkLabel.class,
      label -> db.isLoaded(),
      this::createLabel));

    addCreateNewSchema(tree.addContextMenuItem("Create new sub-debate under this debate", HDT_Debate.class,
      debate -> db.isLoaded(),
      debate -> createChild(debate, rtParentDebateOfDebate)));

    addCreateNewSchema(tree.addContextMenuItem("Create new position under this debate", HDT_Debate.class,
      debate -> db.isLoaded(),
      debate -> createChild(debate, rtParentDebateOfPos)));

    addCreateNewSchema(tree.addContextMenuItem("Create new debate under this position", HDT_Position.class,
      pos -> db.isLoaded(),
      pos -> createChild(pos, rtParentPosOfDebate)));

    addCreateNewSchema(tree.addContextMenuItem("Create new argument for/against this position", HDT_Position.class,
      pos -> db.isLoaded(),
      PositionTabCtrlr::newArgumentClick));

    addCreateNewSchema(tree.addContextMenuItem("Create new position under this position", HDT_Position.class,
      pos -> db.isLoaded(),
      pos -> createChild(pos, rtParentPosOfPos)));

    addCreateNewSchema(tree.addContextMenuItem("Create new counterargument to this argument", HDT_Argument.class,
      arg -> db.isLoaded(),
      arg -> ui.argumentHyperTab().newCounterargumentClick(arg)));

    addCreateNewSchema(tree.addContextMenuItem("Create new note under this note", HDT_Note.class,
      note -> db.isLoaded(),
      note -> createChild(note, rtParentNoteOfNote)));

    addCreateNewSchema(tree.addContextMenuItem("Create new term in this glossary", HDT_Glossary.class,
      glossary -> db.isLoaded(),
      glossary -> ui.goToRecord(HDT_Term.create(glossary), false)));

    addCreateNewSchema(tree.addContextMenuItem("Create new glossary under this glossary", HDT_Glossary.class,
      glossary -> db.isLoaded(),
      this::createGlossary));

    addCreateNewSchema(tree.addContextMenuItem("Create new term in this glossary under this term", HDT_Concept.class,
      concept -> db.isLoaded(),
      concept -> ui.goToRecord(concept.addNewSubConcept(), false)));

    tree.addDefaultMenuItems();

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      textToHilite = lastTextHilited;
      String mainText = "";

      HDT_Record record = tree.selectedRecord();
      if (record == null) return;

      if (record.hasDesc())
        mainText = ((HDT_RecordWithDescription) record).getDesc().getHtml();

      MainTextUtil.handleJSEvent(MainTextUtil.prepHtmlForDisplay(mainText), webView.getEngine());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.getEngine().getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
    {
      if (newState == Worker.State.SUCCEEDED)
      {
        String text = ui.currentFindInDescriptionText();
        if (text.length() > 0)
        {
          highlighter.hilite(text);
          return;
        }

        highlighter.hilite(textToHilite, true);

        lastTextHilited = textToHilite;
        textToHilite = "";
      }
    });

    webView.setOnDragOver(Event::consume);
    webView.setOnDragDropped(Event::consume);

    MainTextUtil.webViewAddZoom(webView, PREF_KEY_TREETAB_ZOOM);

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

    TreeModel<TreeRow> debateTree = tree.debateTree,
                       termTree   = tree.termTree,
                       labelTree  = tree.labelTree,
                       noteTree   = tree.noteTree;

    noteTree  .addKeyWorkRelation(hdtNote     , true);
    termTree  .addKeyWorkRelation(hdtConcept  , true);
    debateTree.addKeyWorkRelation(hdtDebate   , true);
    debateTree.addKeyWorkRelation(hdtPosition , true);
    labelTree .addKeyWorkRelation(hdtWorkLabel, true);

    debateTree.addParentChildRelation(rtParentDebateOfDebate, true);
    debateTree.addParentChildRelation(rtParentDebateOfPos   , true);
    debateTree.addParentChildRelation(rtParentPosOfDebate   , true);
    debateTree.addParentChildRelation(rtParentPosOfPos      , true);
    debateTree.addParentChildRelation(rtPositionOfArgument  , true);
    debateTree.addParentChildRelation(rtCounterOfArgument   , true);
    debateTree.addParentChildRelation(rtWorkOfArgument      , false);

    noteTree.addParentChildRelation(rtParentNoteOfNote, true);

    labelTree.addParentChildRelation(rtParentLabelOfLabel, true);
    labelTree.addParentChildRelation(rtWorkOfArgument    , true);

    termTree.addParentChildRelation(rtParentGlossaryOfGlossary, true);
    termTree.addGlossaryOfConceptRelation();
    termTree.addConceptParentChildRelation();

    List.of(debateTree, noteTree, labelTree, termTree).forEach(treeModel ->
    {
      treeModel.addParentChildRelation(rtParentWorkOfWork, true);
      treeModel.addParentChildRelation(rtWorkOfMiscFile  , true);
    });

    db.addCloseDBHandler(() ->
    {
      try { initTTV(); } catch (IOException e) { e.printStackTrace(); }
    });

    db.addDBLoadedHandler(() -> loaded = true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected RecordType type()        { return hdtNone; }

  @Override public void clear(boolean rstRec)  { tree.clear(); }
  @Override public boolean saveToRecord()      { return true; }
  @Override public HDT_Record activeRecord()   { return tree.selectedRecord(); }
  @Override public HDT_Record viewRecord()     { return activeRecord(); }
  @Override public String recordName()         { return nullSwitch(activeRecord(), "", HDT_Record::getCBText); }
  @Override public void setDividerPositions()  { return; }
  @Override public void getDividerPositions()  { return; }

  @Override public TextViewInfo mainTextInfo(HDT_Record record) { return new TextViewInfo(record, MainTextUtil.webEngineScrollPos(webView.getEngine())); }

  public TreeWrapper getTree()                 { return tree; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateFromRecord()
  {
    if (db.isLoaded() == false)
    {
      tree.clear();
      return;
    }

    ttv.getColumns().forEach(col ->
    {
      if (col.isVisible() == false)
        return;

      col.setVisible(false);
      col.setVisible(true);
    });

    tree.sort();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initTTV() throws IOException
  {
    if (ttv != null)
    {
      TreeItem<TreeRow> root = ttv.getRoot();

      if ((loaded == false) && (root != null) && root.getChildren().stream().filter(Objects::nonNull).allMatch(TreeItem::isLeaf))
      {
        tree.reset(ttv, false, false);
        return;
      }

      HyperTable.saveColWidthsForTable(ttv, ttv.getColumns(), PREF_KEY_HT_TREE);
      removeFromParent(ttv.getParent());

      loaded = false;
    }

    // There is a memory leak in TreeTableView such that it never releases references to values of TreeItems even if they are
    // removed from the tree. So anytime the tree is cleared, we need to release the TTV reference and start from scratch with
    // a new one.

    FXMLLoader loader = new FXMLLoader(App.class.getResource("tree/Tree.fxml"));
    AnchorPane treePane = loader.load();
    spMain.setMasterNode(treePane);
    TreeCtrlr treeCtrlr = loader.getController();

    ttv = treeCtrlr.ttv;

    treeCtrlr.tcName.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue().getNameCell()));
    treeCtrlr.tcDesc.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getDescString()));

    treeCtrlr.tcLinked.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue()));
    treeCtrlr.tcLinked.setCellFactory(row -> TreeRow.typeCellFactory());

    ttv.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      boolean clearWV = true, clearPreview = true;

      tree.setBreadCrumb(newValue);

      if (newValue != null)
      {
        ui.updateBottomPanel(true, false);

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

            case hdtWork : case hdtMiscFile :

              previewWindow.setPreview(pvsTreeTab, (HDT_RecordWithPath)record);
              clearPreview = false;
              break;

            default : break;
          }

          String desc = record.hasDesc() ? ((HDT_RecordWithDescription)record).getDesc().getHtml() : "";

          MainTextWrapper.setReadOnlyHTML(desc, webView.getEngine(), getUseTextViewInfo() ? getView().getTextInfo().scrollPos : 0);
          clearWV = false;
        }
      }

      if (clearWV && (ui.isShuttingDown() == false))
        webView.getEngine().loadContent("");

      if (clearPreview)
        previewWindow.clearPreview(pvsTreeTab);
    });

    scaleNodeForDPI(treePane);

    HyperTable.loadColWidthsForTable(ttv, ttv.getColumns(), PREF_KEY_HT_TREE);

    tree.reset(ttv, false, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<MenuItem> getCreateMenuItems()
  {
    List<MenuItem> items = new ArrayList<>();
    TreeRow row = nullSwitch(tree.selectedItem(), null, TreeItem::getValue);
    if (row == null) return items;

    Set<MenuItemSchema<? extends HDT_Record, TreeRow>> schemas = recordTypeToSchemas.get(row.getRecordType());

    schemas.forEach(schema ->
    {
      if (schema.testWhetherToShow(row) == false) return;

      MenuItem menuItem = new MenuItem(schema.getCaption());
      menuItem.setOnAction(event -> schema.doAction(row));
      items.add(menuItem);
    });

    return items;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addCreateNewSchema(MenuItemSchema<? extends HDT_Record, TreeRow> schema)
  {
    recordTypeToSchemas.put(schema.recordType, schema);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void createChild(HDT_Record parent, RelationType relType)
  {
    ui.treeSelector.attach(db.createNewBlankRecord(db.getSubjType(relType)), parent);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createGlossary(HDT_Glossary glossary)
  {
    RenameDlgCtrlr dlg = new RenameDlgCtrlr("Glossary Name", ntRecord, "");

    if (dlg.showModal() == false) return;

    HDT_Glossary newGlossary = db.createNewBlankRecord(hdtGlossary);
    newGlossary.setActive(true);
    newGlossary.setName(dlg.getNewName());

    ui.treeSelector.attach(newGlossary, glossary);

    Platform.runLater(() -> { tree.sort(); tree.selectRecord(newGlossary, 0, false); });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createLabel(HDT_WorkLabel label)
  {
    RenameDlgCtrlr dlg = new RenameDlgCtrlr("Label Name", ntRecord, "");

    if (dlg.showModal() == false) return;

    HDT_WorkLabel newLabel = db.createNewBlankRecord(hdtWorkLabel);
    newLabel.setName(dlg.getNewName());
    newLabel.parentLabels.add(label);

    Platform.runLater(() -> { tree.sort(); tree.selectRecord(newLabel, 0, false); });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void renameRecord(HDT_Record record)
  {
    String typeName = getTypeName(record.getType());

    if (isUnstoredRecord(record))
    {
      errorPopup("That " + typeName + " cannot be renamed.");
      return;
    }

    RenameDlgCtrlr dlg = new RenameDlgCtrlr(typeName + " Name", ntRecord, record.name());

    if (dlg.showModal())
    {
      record.setName(dlg.getNewName());
      ui.update();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void chooseParent(HDT_Record child)
  {
    ChooseParentDlgCtrlr dlg = new ChooseParentDlgCtrlr(child);

    if (dlg.showModal() == false) return;

    new RecordTreeEdge(dlg.getParent(), child).attach(null, true);

    Platform.runLater(() -> ui.update());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void findWithinDesc()
  {
    if (tree.selectedRecord() != null)
      highlighter.hilite();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void previousSearchResult()
  {
    String text = ui.currentFindInDescriptionText();
    if (text.length() > 0)
    {
      highlighter.previousSearchResult();

      return;
    }

    tree.findAgain(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void nextSearchResult()
  {
    String text = ui.currentFindInDescriptionText();
    if (text.length() > 0)
    {
      highlighter.nextSearchResult();

      return;
    }

    tree.findAgain(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int recordCount()
  {
    return nullSwitch(activeRecord(), 0, ar -> tree.getRowsForRecord(ar).size());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int recordNdx()
  {
    return nullSwitch(nullSwitch(tree.selectedItem(), null, TreeItem::getValue), -1, row -> tree.getRowsForRecord(row.getRecord()).indexOf(row));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setRecord(HDT_Record record)
  {
    if ((record != null) && HDT_Record.isEmpty(record)) return; // Record was probably just deleted; go with whatever is currently selected

    tree.selectRecord(record, record == null ? 0 : record.keyNdx(), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
