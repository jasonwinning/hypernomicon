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

import org.hypernomicon.model.records.*;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.SelectConceptDialogController;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.TreeWrapper.TreeTargetType;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Font;

public class NodeTabController<HDT_RT extends HDT_Base, HDT_CT extends HDT_RecordWithConnector>
{
  @FXML AnchorPane apDescription;
  @FXML private TextField tfName;
  @FXML private TextField tfSearchKey;
  @FXML TableView<HyperTableRow> tvLeftChildren;
  @FXML TableView<HyperTableRow> tvRightChildren;
  @FXML TableView<HyperTableRow> tvParents;
  @FXML private Button btnGoogle;
  @FXML private Button btnIEP;
  @FXML private Button btnSEP;
  @FXML private Button btnWikipedia;
  @FXML private Button btnTree;
  @FXML private Label lblGoTo1;
  @FXML private Label lblGoTo2;
  @FXML private Label lblGoTo3;
  @FXML private Label lblMergeTerms;
  @FXML Label lblParentCaption;
  @FXML GridPane gpToolBar;
  @FXML AnchorPane apLowerPane;
  @FXML SplitPane spMain;
  @FXML SplitPane spChildren;
  @FXML ToolBar tbLinks;

  private Label debateLink, noteLink, labelLink, conceptLink;
  private HDT_RecordType recordType;
  private MainTextWrapper mainText;
  private HyperNodeTab<HDT_RT, HDT_CT> hyperTab;

//---------------------------------------------------------------------------

  public void focusOnSearchKey()              { safeFocus(tfSearchKey); }
  public void hilite(String text)             { mainText.hilite(text); }
  public TextViewInfo getMainTextInfo()       { return mainText.getViewInfo(); }
  public MainTextWrapper getMainTextWrapper() { return mainText; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void init(HDT_RecordType recordType, HyperNodeTab<HDT_RT, HDT_CT> hyperTab)
  {
    this.recordType = recordType;
    this.hyperTab = hyperTab;

    mainText = new MainTextWrapper(apDescription);

    if (recordType != hdtConcept)
    {
      lblGoTo3.setPadding(new Insets(0.0, 0.0, 0.0, 0.0));
      tbLinks.getItems().remove(lblMergeTerms);
    }

    switch (recordType)
    {
      case hdtDebate : case hdtPosition :

        debateLink = null;
        conceptLink = lblGoTo1;
        noteLink = lblGoTo2;
        labelLink = lblGoTo3;
        break;

      case hdtNote :

        debateLink = lblGoTo1;
        conceptLink = lblGoTo2;
        noteLink = null;
        labelLink = lblGoTo3;
        break;

      case hdtConcept :

        debateLink = lblGoTo1;
        conceptLink = null;
        noteLink = lblGoTo2;
        labelLink = lblGoTo3;
        break;

      default :

        debateLink = null;
        conceptLink = null;
        noteLink = null;
        labelLink = null;
    }

    btnGoogle.setOnAction(event -> searchGoogle(tfName.getText(), true));
    btnIEP.setOnAction(event -> searchIEP(tfName.getText()));
    btnSEP.setOnAction(event -> searchSEP(tfName.getText()));
    btnWikipedia.setOnAction(event -> searchWikipedia(tfName.getText()));
    btnTree.setOnAction(event -> ui.goToTreeRecord(ui.viewRecord()));

    double fontSize = appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize < 0) fontSize = lblGoTo1.getFont().getSize();

    lblGoTo1.setFont(new Font(fontSize + 6.0));
    lblGoTo2.setFont(new Font(fontSize + 6.0));
    lblGoTo3.setFont(new Font(fontSize + 6.0));
    lblMergeTerms.setFont(new Font(fontSize + 6.0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MenuItem makeMenuItem(HDT_RecordWithConnector record)
  {
    MenuItem miUnlink = new MenuItem();
    miUnlink.setText("Unlink");
    miUnlink.setOnAction(ae ->
    {
      if (ui.cantSaveRecord(true)) return;
      record.getLink().disconnectRecord(record.getType(), true);
      ui.update();
    });

    return miUnlink;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MenuItem makeMoveConceptItem()
  {
    MenuItem miMove = new MenuItem();
    miMove.setText("Move this definition to a different term");
    miMove.setOnAction(event -> TermTab.class.cast(hyperTab).moveConcept());

    return miMove;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void updateLinkLabels(HDT_CT record)
  {
    HDT_Debate    debate   = null;
    HDT_Position  position = null;
    HDT_Concept   concept  = null;
    HDT_WorkLabel label    = null;
    HDT_Note      note     = null;

    if (record != null)
    {
      if (record.isLinked())
      {
        debate   = record.getLink().getDebate();
        position = record.getLink().getPosition();
        concept  = record.getLink().getConcept();
        label    = record.getLink().getLabel();
        note     = record.getLink().getNote();
      }

      if (record.getType().equals(recordType) == false)
      {
        messageDialog("Internal Error #28788", mtError);
        return;
      }
    }

    if ((recordType != hdtDebate) && (recordType != hdtPosition))
    {
      if ((debate != null) || (position != null))
      {
        debateLink.setStyle("-fx-background-color: blue; -fx-text-fill: white;");

        if (record.getLink().getDebate() != null)
        {
          debateLink.setText("Go to Debate...");
          debateLink.setContextMenu(new ContextMenu(makeMenuItem(debate)));
          setGoToEvent(debateLink, debate);
        }
        else
        {
          debateLink.setText("Go to Position...");
          debateLink.setContextMenu(new ContextMenu(makeMenuItem(position)));
          setGoToEvent(debateLink, position);
        }

        setTooltip(debateLink);
      }
      else
      {
        debateLink.setStyle("");
        debateLink.setText("Link to Debate/Position...");
        debateLink.setTooltip(null);
        debateLink.setContextMenu(null);
        setLinkToEvent(debateLink, hdtDebate);
      }
    }

    if (recordType != hdtConcept)
    {
      if (concept != null)
      {
        conceptLink.setStyle("-fx-background-color: aqua; -fx-text-fill: red;");
        conceptLink.setText("Go to Term...");
        setTooltip(conceptLink);
        conceptLink.setContextMenu(new ContextMenu(makeMenuItem(concept)));
        setGoToEvent(conceptLink, concept);
      }
      else
      {
        conceptLink.setStyle("");
        conceptLink.setText("Link to Term...");
        conceptLink.setTooltip(null);
        conceptLink.setContextMenu(null);
        conceptLink.setOnMouseClicked(mouseEvent ->
        {
          if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
            linkToTermClick();
        });
      }
    }

    if (recordType != hdtNote)
    {
      if (note != null)
      {
        noteLink.setStyle("-fx-background-color: lime; -fx-text-fill: maroon;");
        noteLink.setText("Go to Note...");
        setTooltip(noteLink);
        noteLink.setContextMenu(new ContextMenu(makeMenuItem(note)));
        setGoToEvent(noteLink, note);
      }
      else
      {
        noteLink.setStyle("");
        noteLink.setText("Link to Note...");
        noteLink.setTooltip(null);
        noteLink.setContextMenu(null);
        setLinkToEvent(noteLink, hdtNote);
      }
    }

    if (recordType != hdtWorkLabel)
    {
      if (label != null)
      {
        labelLink.setStyle("-fx-background-color: fuchsia; -fx-text-fill: yellow;");
        labelLink.setText("Go to Label...");
        setTooltip(labelLink);
        labelLink.setContextMenu(new ContextMenu(makeMenuItem(label)));
        labelLink.setOnMouseClicked(mouseEvent ->
        {
          if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
            ui.goToTreeRecord(record.getLink().getLabel());
        });
      }
      else
      {
        labelLink.setStyle("");
        labelLink.setText("Link to Label...");
        labelLink.setTooltip(null);
        labelLink.setContextMenu(null);
        setLinkToEvent(labelLink, hdtWorkLabel);
      }
    }

    if (recordType == hdtConcept)
    {
      lblMergeTerms.setTooltip(new Tooltip("Use right/secondary button to move this definition to a different term"));
      lblMergeTerms.setContextMenu(new ContextMenu(makeMoveConceptItem()));
      lblMergeTerms.setOnMouseClicked(mouseEvent ->
      {
        if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
          TermTab.class.cast(hyperTab).merge();
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setTooltip(Label label)
  {
    label.setTooltip(new Tooltip("Use right/secondary button to unlink"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void linkToTermClick()
  {
    if (ui.cantSaveRecord(true)) return;

    SelectConceptDialogController frmSelectConcept = SelectConceptDialogController.create("Term select", null);

    if (frmSelectConcept.showModal() == false) return;

    HDT_RecordWithConnector source = (HDT_RecordWithConnector) ui.activeRecord();
    HDT_Term term = frmSelectConcept.getTerm();
    HDT_Concept concept;

    if (frmSelectConcept.createNew)
    {
      concept = db.createNewBlankRecord(hdtConcept);

      term.setName(source.listName());
      term.concepts.add(concept);

      concept.glossary.set(db.glossaries.getByID(1));
    }
    else if (frmSelectConcept.getGlossary() != null)
    {
      concept = term.getConcept(frmSelectConcept.getGlossary());
    }
    else
      return;

    ui.uniteRecords(source, concept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setLinkToEvent(Label label, HDT_RecordType type)
  {
    label.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
      {
        HDT_Base source = ui.viewRecord();

        ui.treeSubjRecord = source;
        ui.treeObjRecord = null;
        ui.treeTargetTypes.clear();

        ui.treeTargetTypes.add(new TreeTargetType(rtUnited, type));
        if (type == hdtDebate)
          ui.treeTargetTypes.add(new TreeTargetType(rtUnited, hdtPosition));

        ui.goToTreeRecord(db.records(type).getByID(1));
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setGoToEvent(Label label, HDT_RecordWithConnector record)
  {
    label.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
        ui.goToRecord(record, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    tfName.clear();
    tfSearchKey.clear();

    mainText.clear(true);

    if (recordType != hdtArgument)
      updateLinkLabels(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean save(HDT_CT record, boolean showMessage, HyperNodeTab<HDT_RT, HDT_CT> hyperTab)
  {
    if (record.getType() == hdtConcept)
    {
      if (tfSearchKey.getText().length() == 0)
      {
        messageDialog("Unable to modify record: search key of term cannot be zero-length.", mtError);
        safeFocus(tfSearchKey);
        return false;
      }

      if (tfName.getText().length() == 0)
      {
        messageDialog("Unable to modify record: term cannot be zero-length.", mtError);
        safeFocus(tfName);
        return false;
      }
    }

    if (!hyperTab.saveSearchKey(record, tfSearchKey, showMessage)) return false;

    record.setName(tfName.getText());

    mainText.save();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void update(HDT_CT record)
  {
    tfName.setText(record.name());
    tfSearchKey.setText(record.getSearchKey());

    mainText.loadFromRecord(record, true, hyperTab.getView().getTextInfo());

    if (record.isUnitable())
      updateLinkLabels(record);

    record.viewNow();

    safeFocus(tfName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
