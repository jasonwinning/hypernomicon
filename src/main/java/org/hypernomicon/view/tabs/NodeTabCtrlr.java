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

import org.hypernomicon.model.records.*;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.SelectConceptDlgCtrlr;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.prefs.Preferences;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SplitMenuButton;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.ToolBar;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Font;

public class NodeTabCtrlr<HDT_RT extends HDT_Record, HDT_CT extends HDT_RecordWithConnector>
{
  @FXML AnchorPane apDescription, apLowerPane;
  @FXML GridPane gpToolBar;
  @FXML Label lblParentCaption;
  @FXML SplitMenuButton smbWebSrch1;
  @FXML SplitPane spChildren, spMain;
  @FXML TableView<HyperTableRow> tvLeftChildren, tvParents, tvRightChildren;
  @FXML ToolBar tbLinks, tbButtons;
  @FXML private Button btnWebSrch1, btnWebSrch2, btnWebSrch3, btnWebSrch4, btnTree;
  @FXML private Label lblGoTo1, lblGoTo2, lblGoTo3, lblMergeTerms;
  @FXML private TextField tfName, tfSearchKey;

  private Label debateLink, noteLink, labelLink, conceptLink;
  private HDT_RecordType recordType;
  private MainTextWrapper mainText;
  private HyperNodeTab<HDT_RT, HDT_CT> hyperTab;

  private static final String TOOLTIP_PREFIX = "Search record name using ";

//---------------------------------------------------------------------------

  TextField nameCtrl()              { return tfName; }
  void hilite(String text)          { mainText.hilite(text); }
  TextViewInfo mainTextInfo()       { return mainText.getViewInfo(); }
  MainTextWrapper mainTextWrapper() { return mainText; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void init(HDT_RecordType recordType, HyperNodeTab<HDT_RT, HDT_CT> hyperTab)
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

    btnWebSrch1.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + "1"));
    smbWebSrch1.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + "1"));
    btnWebSrch2.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + "2"));
    btnWebSrch3.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + "3"));
    btnWebSrch4.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + "4"));
    btnTree    .setOnAction(event -> ui.goToTreeRecord(ui.viewRecord()));

    setToolTip(btnWebSrch1, TOOLTIP_PREFIX + "Google");
    setToolTip(btnWebSrch3, TOOLTIP_PREFIX + "Internet Encyclopedia of Philosophy");
    setToolTip(btnWebSrch2, TOOLTIP_PREFIX + "Stanford Encyclopedia of Philosophy");
    setToolTip(btnWebSrch4, TOOLTIP_PREFIX + "Wikipedia");
    
    ui.setSearchKeyToolTip(tfSearchKey);

    double fontSize = appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize < 0) fontSize = lblGoTo1.getFont().getSize();

    lblGoTo1.setFont     (new Font(fontSize + 6.0));
    lblGoTo2.setFont     (new Font(fontSize + 6.0));
    lblGoTo3.setFont     (new Font(fontSize + 6.0));
    lblMergeTerms.setFont(new Font(fontSize + 6.0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private MenuItem makeMenuItem(HDT_RecordWithConnector record)
  {
    MenuItem miUnlink = new MenuItem();
    miUnlink.setText("Unlink");
    miUnlink.setOnAction(ae ->
    {
      if (ui.cantSaveRecord()) return;
      record.getLink().disconnectRecord(record.getType(), true);
      ui.update();
    });

    return miUnlink;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private MenuItem makeMoveConceptItem()
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
        setToolTip(debateLink, "");
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
        setToolTip(conceptLink, "");
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
        setToolTip(noteLink, "");
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
        setToolTip(labelLink, "");
        labelLink.setContextMenu(null);
        setLinkToEvent(labelLink, hdtWorkLabel);
      }
    }

    if (recordType == hdtConcept)
    {
      setToolTip(lblMergeTerms, "Use right/secondary button to move this definition to a different term");
      lblMergeTerms.setContextMenu(new ContextMenu(makeMoveConceptItem()));
      lblMergeTerms.setOnMouseClicked(mouseEvent ->
      {
        if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
          TermTab.class.cast(hyperTab).merge();
      });
    }

    if (record == null) return;

    if (record.getType() == hdtDebate)
      disableAllIff(record.getID() == 1, conceptLink, noteLink, labelLink);
    else if (record.getType() == hdtNote)
      disableAllIff(record.getID() == 1, debateLink, conceptLink, labelLink);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setTooltip(Label label)
  {
    setToolTip(label, "Use right/secondary button to unlink");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void linkToTermClick()
  {
    if (ui.cantSaveRecord()) return;

    SelectConceptDlgCtrlr frmSelectConcept = SelectConceptDlgCtrlr.create(null);

    if (frmSelectConcept.showModal() == false) return;

    HDT_RecordWithConnector source = (HDT_RecordWithConnector) ui.activeRecord();
    HDT_Term term = frmSelectConcept.getTerm();
    HDT_Concept concept;

    if (frmSelectConcept.getCreateNew())
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

    ui.uniteRecords(source, concept, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setLinkToEvent(Label label, HDT_RecordType type)
  {
    label.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
      {
        ui.treeSelector.linking(ui.viewRecord(), type);
        ui.goToTreeRecord(db.records(type).getByID(1));
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setGoToEvent(Label label, HDT_RecordWithConnector record)
  {
    label.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
        ui.goToRecord(record, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    if (btnWebSrch1.isVisible())
    {
      tbButtons.getItems().remove(smbWebSrch1);
      addToParent(btnWebSrch1, tbButtons);
    }
    else
    {
      tbButtons.getItems().remove(btnWebSrch1);
      addToParent(smbWebSrch1, tbButtons);
    }

    tfName.clear();
    tfSearchKey.clear();

    mainText.clear(true);

    if (recordType != hdtArgument)
      updateLinkLabels(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean saveToRecord(HDT_CT record)
  {
    if (record.getType() == hdtConcept)
    {
      if (tfSearchKey.getText().isEmpty())
        return falseWithErrorMessage("Unable to modify record: search key of term cannot be zero-length.", tfSearchKey);

      if (tfName.getText().isEmpty())
        return falseWithErrorMessage("Unable to modify record: term cannot be zero-length.", tfName);
    }

    if (!HyperTab.saveSearchKey(record, tfSearchKey)) return false;

    record.setName(tfName.getText());

    mainText.save();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void update(HDT_CT record)
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

  public void updateWebButtons(Preferences node)
  {
    HyperTab.updateWebButtons(node, PREF_KEY_GEN_SRCH, 4, btnWebSrch1, smbWebSrch1, TOOLTIP_PREFIX, this::searchBtnEvent);

    btnWebSrch2.setText(ui.webButtonMap.get(PREF_KEY_GEN_SRCH + "2").getCaption());
    btnWebSrch3.setText(ui.webButtonMap.get(PREF_KEY_GEN_SRCH + "3").getCaption());
    btnWebSrch4.setText(ui.webButtonMap.get(PREF_KEY_GEN_SRCH + "4").getCaption());

    setToolTip(btnWebSrch2, TOOLTIP_PREFIX + btnWebSrch2.getText());
    setToolTip(btnWebSrch3, TOOLTIP_PREFIX + btnWebSrch3.getText());
    setToolTip(btnWebSrch4, TOOLTIP_PREFIX + btnWebSrch4.getText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EventHandler<ActionEvent> searchBtnEvent(String prefKey)
  {
    return event ->
    {
      ui.webButtonMap.get(prefKey).first(WebButtonField.Name, tfName.getText())
                                  .go();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
