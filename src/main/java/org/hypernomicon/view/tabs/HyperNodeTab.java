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

package org.hypernomicon.view.tabs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import java.io.IOException;
import java.util.prefs.Preferences;

import org.hypernomicon.dialogs.SelectConceptDlgCtrlr;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Note;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Term;
import org.hypernomicon.model.records.HDT_WorkLabel;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTableRow;

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
import javafx.scene.control.Tab;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.ToolBar;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Font;

public abstract class HyperNodeTab<HDT_RT extends HDT_Record, HDT_CT extends HDT_RecordWithMainText> extends HyperTab<HDT_RT, HDT_CT>
{
  @FXML private Button btnWebSrch1, btnWebSrch2, btnWebSrch3, btnWebSrch4, btnTree;
  @FXML private Label lblGoTo1, lblGoTo2, lblGoTo3, lblMergeTerms;
  @FXML private SplitMenuButton smbWebSrch1;
  @FXML private TextField tfName, tfSearchKey;
  @FXML private ToolBar tbLinks, tbButtons;

  @FXML AnchorPane apDescription, apLowerPane;
  @FXML GridPane gpToolBar;
  @FXML Label lblParentCaption;
  @FXML SplitPane spChildren, spMain;
  @FXML TableView<HyperTableRow> tvLeftChildren, tvParents, tvRightChildren;

  private final RecordType nodeRecordType; // This is not always the same as what is returned by type(). If type() == hdtTerm, nodeRecordType will be hdtConcept
  private final MainTextWrapper mainText;
  private final Label debateLink, noteLink, labelLink, conceptLink;

  private static final String TOOLTIP_PREFIX = "Search record name using ";

//---------------------------------------------------------------------------

  HyperNodeTab(TabEnum tabEnum, Tab tab) throws IOException
  {
    super(tabEnum, tab, "view/tabs/NodeTab");

    nodeRecordType = type() == hdtTerm ? hdtConcept : type();

    mainText = new MainTextWrapper(apDescription);

    if (nodeRecordType != hdtConcept)
    {
      lblGoTo3.setPadding(new Insets(0.0, 0.0, 0.0, 0.0));
      tbLinks.getItems().remove(lblMergeTerms);
    }

    switch (nodeRecordType)
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

    btnWebSrch1.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + '1'));
    smbWebSrch1.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + '1'));
    btnWebSrch2.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + '2'));
    btnWebSrch3.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + '3'));
    btnWebSrch4.setOnAction(searchBtnEvent(PREF_KEY_GEN_SRCH + '4'));
    btnTree    .setOnAction(event -> ui.goToTreeRecord(ui.viewRecord()));

    setToolTip(btnWebSrch1, TOOLTIP_PREFIX + "Google");
    setToolTip(btnWebSrch3, TOOLTIP_PREFIX + "Internet Encyclopedia of Philosophy");
    setToolTip(btnWebSrch2, TOOLTIP_PREFIX + "Stanford Encyclopedia of Philosophy");
    setToolTip(btnWebSrch4, TOOLTIP_PREFIX + "Wikipedia");

    setToolTip(btnTree, "Go to this record in Tree tab");

    MainCtrlr.setSearchKeyToolTip(tfSearchKey);

    double fontSize = app.prefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize < 0) fontSize = lblGoTo1.getFont().getSize();

    lblGoTo1.setFont     (new Font(fontSize + 6.0));
    lblGoTo2.setFont     (new Font(fontSize + 6.0));
    lblGoTo3.setFont     (new Font(fontSize + 6.0));
    lblMergeTerms.setFont(new Font(fontSize + 6.0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected abstract HDT_CT getNodeRecord();

  @Override public final MainTextWrapper mainTextWrapper() { return mainText; }
  @Override public String recordName()                     { return tfName.getText(); }

  TextField nameCtrl()                                     { return tfName; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static MenuItem makeMenuItem(HDT_RecordWithMainText record)
  {
    MenuItem miUnlink = new MenuItem();
    miUnlink.setText("Disunite");
    miUnlink.setOnAction(ae ->
    {
      if (ui.cantSaveRecord()) return;
      record.getHub().disuniteRecord(record.getType(), true);
      record.modifyNow();
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
    miMove.setOnAction(event -> ((TermTabCtrlr) this).moveConcept());

    return miMove;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateLinkLabels(HDT_CT record)
  {
    HDT_Debate    debate   = null;
    HDT_Position  position = null;
    HDT_Concept   concept  = null;
    HDT_WorkLabel label    = null;
    HDT_Note      note     = null;

    if (record != null)
    {
      if (record.hasHub())
      {
        debate   = record.getHub().getDebate();
        position = record.getHub().getPosition();
        concept  = record.getHub().getConcept();
        label    = record.getHub().getLabel();
        note     = record.getHub().getNote();
      }

      if (record.getType() != nodeRecordType)
      {
        messageDialog("Internal Error #28788", mtError);
        return;
      }
    }

    if ((nodeRecordType != hdtDebate) && (nodeRecordType != hdtPosition))
    {
      if ((debate != null) || (position != null))
      {
        debateLink.setStyle("-fx-background-color: blue; -fx-text-fill: white;");

        if (debate != null)
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

        setDisuniteTooltip(debateLink);
      }
      else
      {
        debateLink.setStyle("");
        debateLink.setText("Unite with Debate/Position...");
        setUniteTooltip(debateLink);
        debateLink.setContextMenu(null);
        setLinkToEvent(debateLink, hdtDebate);
      }
    }

    if (nodeRecordType != hdtConcept)
    {
      if (concept != null)
      {
        conceptLink.setStyle("-fx-background-color: aqua; -fx-text-fill: red;");
        conceptLink.setText("Go to Term...");
        setDisuniteTooltip(conceptLink);
        conceptLink.setContextMenu(new ContextMenu(makeMenuItem(concept)));
        setGoToEvent(conceptLink, concept);
      }
      else
      {
        conceptLink.setStyle("");
        conceptLink.setText("Unite with Term...");
        setUniteTooltip(conceptLink);
        conceptLink.setContextMenu(null);
        conceptLink.setOnMouseClicked(mouseEvent ->
        {
          if (mouseEvent.getButton() == MouseButton.PRIMARY)
            linkToTermClick();
        });
      }
    }

    if (nodeRecordType != hdtNote)
    {
      if (note != null)
      {
        noteLink.setStyle("-fx-background-color: lime; -fx-text-fill: maroon;");
        noteLink.setText("Go to Note...");
        setDisuniteTooltip(noteLink);
        noteLink.setContextMenu(new ContextMenu(makeMenuItem(note)));
        setGoToEvent(noteLink, note);
      }
      else
      {
        noteLink.setStyle("");
        noteLink.setText("Unite with Note...");
        setUniteTooltip(noteLink);
        noteLink.setContextMenu(null);
        setLinkToEvent(noteLink, hdtNote);
      }
    }

    if (nodeRecordType != hdtWorkLabel)
    {
      if (label != null)
      {
        labelLink.setStyle("-fx-background-color: fuchsia; -fx-text-fill: yellow;");
        labelLink.setText("Go to Label...");
        setDisuniteTooltip(labelLink);
        labelLink.setContextMenu(new ContextMenu(makeMenuItem(label)));
        labelLink.setOnMouseClicked(mouseEvent ->
        {
          if (mouseEvent.getButton() == MouseButton.PRIMARY)
            ui.goToTreeRecord(record.getHub().getLabel());
        });
      }
      else
      {
        labelLink.setStyle("");
        labelLink.setText("Unite with Label...");
        setUniteTooltip(labelLink);
        labelLink.setContextMenu(null);
        setLinkToEvent(labelLink, hdtWorkLabel);
      }
    }

    if (nodeRecordType == hdtConcept)
    {
      setToolTip(lblMergeTerms, "Use right/secondary button to move this definition to a different term");
      lblMergeTerms.setContextMenu(new ContextMenu(makeMoveConceptItem()));
      lblMergeTerms.setOnMouseClicked(mouseEvent ->
      {
        if (mouseEvent.getButton() == MouseButton.PRIMARY)
          ((TermTabCtrlr) this).merge();
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

  private static void setDisuniteTooltip(Label label)
  {
    setToolTip(label, "Use right/secondary button to disunite");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static WebTooltip uniteToolTip = null;

  synchronized private static void setUniteTooltip(Label label)
  {
    if (uniteToolTip == null) uniteToolTip = new WebTooltip("""
      Click this link to choose another record to &ldquo;unite&rdquo; this record with.<br><br>

      To understand what &ldquo;uniting&rdquo; two records means, consider the fact that sometimes, you might have multiple<br>
      records of different types that represent the same topic and contain redundant information. For example, you<br>
      might have a Term record for &ldquo;Cause&rdquo; and a Problem/Debate record for &ldquo;What is the nature of causation?&rdquo;.<br><br>

      Instead of letting each one contain some information and key works relevant to the other and having to manually<br>
      make sure they stay in sync with each other, you can &ldquo;unite&rdquo; these records, which causes them to always have<br>
      the same description text and the same key works. Making a change to the description or key works of one record<br>
      will automatically cause the other to be updated as well. Problem/Debate records, Position records, Term records,<br>
      Note records, and Labels can all be united together (with the exception that a Problem/Debate record cannot be<br>
      united with a Position record).<br><br>

      Uniting records does not actually cause them to become merged into a single record. They will remain distinct<br>
      records possessing some fields not possessed by the record(s) they are united with (a record cannot be united<br>
      with another record of the same type). Once records are united together, you can right/secondary click this<br>
      link to disunite them.""");

    label.setTooltip(uniteToolTip);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void linkToTermClick()
  {
    if (ui.cantSaveRecord()) return;

    HDT_RecordWithMainText source = (HDT_RecordWithMainText) ui.activeRecord();

    SelectConceptDlgCtrlr frmSelectConcept = new SelectConceptDlgCtrlr(null, source);

    if ((frmSelectConcept.showModal() == false) || (frmSelectConcept.getGlossary() == null)) return;

    HDT_Term term = frmSelectConcept.getTerm();
    HDT_Concept concept = term.getConcept(frmSelectConcept.getGlossary(), frmSelectConcept.getSense());

    ui.uniteRecords(source, concept, false);

    if (frmSelectConcept.getCreateNew() == false) return;

    term.setName(source.listName());
    ui.goToRecord(concept, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void setLinkToEvent(Label label, RecordType type)
  {
    label.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton() == MouseButton.PRIMARY)
      {
        ui.treeSelector.linking(ui.viewRecord(), type);
        ui.goToTreeRecord(db.records(type).getByID(1));
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void setGoToEvent(Label label, HDT_RecordWithMainText record)
  {
    label.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton() == MouseButton.PRIMARY)
        ui.goToRecord(record, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
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

    mainText.clear();

    if (nodeRecordType != hdtArgument)
      updateLinkLabels(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord()
  {
    HDT_CT nodeRecord = getNodeRecord();

    if (nodeRecord.getType() == hdtConcept)
    {
      if (tfSearchKey.getText().isEmpty())
        return falseWithErrorMessage("Unable to modify record: search key of term cannot be zero-length.", tfSearchKey);

      if (tfName.getText().isEmpty())
        return falseWithErrorMessage("Unable to modify record: term cannot be zero-length.", tfName);
    }

    if (HyperTab.saveSearchKey(nodeRecord, tfSearchKey) == false) return false;

    nodeRecord.setName(tfName.getText());

    mainText.save();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateFromRecord()
  {
    HDT_CT nodeRecord = getNodeRecord();

    tfName.setText(nodeRecord.name());
    tfSearchKey.setText(nodeRecord.getSearchKey());

    mainText.loadFromRecord(nodeRecord, true, getView().getTextInfo());

    if (nodeRecord.isUnitable())
      updateLinkLabels(nodeRecord);

    nodeRecord.viewNow();

    safeFocus(tfName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void updateWebButtons(Preferences node)
  {
    HyperTab.updateWebButtons(node, PREF_KEY_GEN_SRCH, 4, btnWebSrch1, smbWebSrch1, TOOLTIP_PREFIX, this::searchBtnEvent);

    btnWebSrch2.setText(ui.webButtonMap.get(PREF_KEY_GEN_SRCH + '2').getCaption());
    btnWebSrch3.setText(ui.webButtonMap.get(PREF_KEY_GEN_SRCH + '3').getCaption());
    btnWebSrch4.setText(ui.webButtonMap.get(PREF_KEY_GEN_SRCH + '4').getCaption());

    setToolTip(btnWebSrch2, TOOLTIP_PREFIX + btnWebSrch2.getText());
    setToolTip(btnWebSrch3, TOOLTIP_PREFIX + btnWebSrch3.getText());
    setToolTip(btnWebSrch4, TOOLTIP_PREFIX + btnWebSrch4.getText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EventHandler<ActionEvent> searchBtnEvent(String prefKey)
  {
    return event -> ui.webButtonMap.get(prefKey).first(WebButtonField.Name, tfName.getText())
                                                .go();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
