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
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;

import java.io.IOException;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.prefs.Preferences;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.dialogs.SelectTermDlgCtrlr;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.controls.WebTooltip;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
import javafx.scene.text.Font;

//---------------------------------------------------------------------------

public abstract class HyperNodeTab<HDT_RT extends HDT_Record, HDT_CT extends HDT_RecordWithMainText> extends HyperTab<HDT_RT, HDT_CT>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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
  private final Label debateLink, noteLink, workLabelLink, conceptLink;

  private static final String TOOLTIP_PREFIX    = "Search record name using ",

                              DEBATE_TEXT_COLOR = "white",
                              DEBATE_BG_COLOR   = "blue",
                              NOTE_TEXT_COLOR   = "maroon",
                              NOTE_BG_COLOR     = "lime",
                              TERM_TEXT_COLOR   = "red",
                              TERM_BG_COLOR     = "aqua",
                              LABEL_TEXT_COLOR  = "yellow",
                              LABEL_BG_COLOR    = "fuchsia";

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

    Iterator<Label> linkIt = List.of(lblGoTo1, lblGoTo2, lblGoTo3).iterator();

    debateLink    = EnumSet.of(                        hdtConcept, hdtNote).contains(nodeRecordType) ? linkIt.next() : null;
    conceptLink   = EnumSet.of(hdtDebate, hdtPosition,             hdtNote).contains(nodeRecordType) ? linkIt.next() : null;
    noteLink      = EnumSet.of(hdtDebate, hdtPosition, hdtConcept         ).contains(nodeRecordType) ? linkIt.next() : null;
    workLabelLink = EnumSet.of(hdtDebate, hdtPosition, hdtConcept, hdtNote).contains(nodeRecordType) ? linkIt.next() : null;

    btnWebSrch1.setOnAction(searchBtnEvent(WebButtonContextPrefKey.GEN + '1'));
    smbWebSrch1.setOnAction(searchBtnEvent(WebButtonContextPrefKey.GEN + '1'));
    btnWebSrch2.setOnAction(searchBtnEvent(WebButtonContextPrefKey.GEN + '2'));
    btnWebSrch3.setOnAction(searchBtnEvent(WebButtonContextPrefKey.GEN + '3'));
    btnWebSrch4.setOnAction(searchBtnEvent(WebButtonContextPrefKey.GEN + '4'));
    btnTree    .setOnAction(event -> ui.goToTreeRecord(ui.viewRecord()));

    setToolTip(btnWebSrch1, () -> TOOLTIP_PREFIX + btnWebSrch1.getText());
    setToolTip(smbWebSrch1, () -> TOOLTIP_PREFIX + smbWebSrch1.getText());
    setToolTip(btnWebSrch2, () -> TOOLTIP_PREFIX + btnWebSrch2.getText());
    setToolTip(btnWebSrch3, () -> TOOLTIP_PREFIX + btnWebSrch3.getText());
    setToolTip(btnWebSrch4, () -> TOOLTIP_PREFIX + btnWebSrch4.getText());

    setToolTip(btnTree, "Go to this record in Tree tab");

    MainCtrlr.setSearchKeyToolTip(tfSearchKey);

    double fontSize = app.prefs.getDouble(PrefKey.FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize < 0) fontSize = lblGoTo1.getFont().getSize();

    lblGoTo1     .setFont(new Font(fontSize + 6.0));
    lblGoTo2     .setFont(new Font(fontSize + 6.0));
    lblGoTo3     .setFont(new Font(fontSize + 6.0));
    lblMergeTerms.setFont(new Font(fontSize + 6.0));
  }

//---------------------------------------------------------------------------

  protected abstract HDT_CT getNodeRecord();

  @Override public final MainTextWrapper mainTextWrapper() { return mainText; }
  @Override public String recordName()                     { return tfName.getText(); }

  TextField nameCtrl()                                     { return tfName; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static MenuItem makeMenuItem(HDT_RecordWithMainText record)
  {
    MenuItem mnuDisunite = new MenuItem();
    mnuDisunite.setText("Disunite");
    mnuDisunite.setOnAction(event ->
    {
      if (ui.cantSaveRecord()) return;
      record.getHub().disuniteRecord(record.getType());
      record.modifyNow();
      ui.update();
    });

    return mnuDisunite;
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

  private void updateUniteDisuniteLinks(HDT_CT record)
  {
    HDT_Hub hub = null;

    if (record != null)
    {
      if (record.getType() != nodeRecordType)
      {
        internalErrorPopup(28788);
        return;
      }

      hub = record.getHub();
    }

    updateDebateOrPositionLink(debateLink   , hub);
    updateLink                (conceptLink  , hub == null ? null : hub.getConcept(), "Term" , TERM_TEXT_COLOR , TERM_BG_COLOR , hdtConcept  );
    updateLink                (noteLink     , hub == null ? null : hub.getNote   (), "Note" , NOTE_TEXT_COLOR , NOTE_BG_COLOR , hdtNote     );
    updateLink                (workLabelLink, hub == null ? null : hub.getLabel  (), "Label", LABEL_TEXT_COLOR, LABEL_BG_COLOR, hdtWorkLabel);

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

    if      (record.getType() == hdtDebate) disableAllIff(record.getID() == 1, conceptLink, noteLink   , workLabelLink);
    else if (record.getType() == hdtNote  ) disableAllIff(record.getID() == 1, debateLink , conceptLink, workLabelLink);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateDebateOrPositionLink(Label link, HDT_Hub hub)
  {
    if ((nodeRecordType == hdtDebate) || (nodeRecordType == hdtPosition)) return;

    HDT_Debate   debate   = hub == null ? null : hub.getDebate  ();
    HDT_Position position = hub == null ? null : hub.getPosition();

    if      (debate   != null) updateDisuniteLink(link, debate  , "Debate"  , DEBATE_TEXT_COLOR, DEBATE_BG_COLOR);
    else if (position != null) updateDisuniteLink(link, position, "Position", DEBATE_TEXT_COLOR, DEBATE_BG_COLOR);
    else
      updateUniteLink(link, "Debate/Position", hdtDebate);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateLink(Label link, HDT_RecordWithMainText record, String typeName, String textColor, String bgColor, RecordType type)
  {
    if (nodeRecordType == type) return;

    if (record != null)
      updateDisuniteLink(link, record, typeName, textColor, bgColor);
    else
      updateUniteLink(link, typeName, type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void updateDisuniteLink(Label link, HDT_RecordWithMainText record, String typeName, String textColor, String bgColor)
  {
    link.setStyle("-fx-background-color: " + bgColor + "; -fx-text-fill: " + textColor + ';');
    link.setText("Go to " + typeName + "...");
    setToolTip(link, "Use right/secondary button to disunite");
    link.setContextMenu(new ContextMenu(makeMenuItem(record)));

    link.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton() != MouseButton.PRIMARY) return;

      if (record.getType() == hdtWorkLabel)
        ui.goToTreeRecord(record);
      else
        ui.goToRecord(record, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void updateUniteLink(Label link, String typeName, RecordType type)
  {
    link.setStyle("");
    link.setText("Unite with " + typeName + "...");
    setUniteTooltip(link);
    link.setContextMenu(null);

    link.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton() != MouseButton.PRIMARY) return;

      if (type == hdtConcept)
        uniteWithTermClick();
      else
      {
        ui.treeSelector.uniting(ui.viewRecord(), type);
        ui.goToTreeRecord(db.records(type).getByID(1));
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static WebTooltip uniteToolTip = null;

  synchronized private static void setUniteTooltip(Label label)
  {
    if (uniteToolTip == null) uniteToolTip = new WebTooltip("""
      <html lang="en">
      <head>
        <style>
          .topic      { color: #4682B4; }
          .recname    { color: #FF6347; }
          .large-text { font-size: 1.3em; font-weight: normal; }
        </style>
      </head>
      <body>
        <h4 class="large-text">Click here to <strong>unite</strong> this record with another one.</h4>

        <hr/>

        <p><strong class="topic">What does &ldquo;uniting&rdquo; records mean?</strong> Sometimes, multiple records of different types<br/>
        overlap on the same topic and contain redundant information. For instance, you might<br/>
        have a Term record, <strong class="recname">&ldquo;Cause&rdquo;</strong>, and a Problem/Debate record called <strong class="recname">&ldquo;What is the nature<br/>
        of causation?&rdquo;</strong>. Instead of having each record hold similar information and manually<br/>
        keeping them in sync, you can unite these records. This means they will share the same<br/>
        description and key works. Any updates made to one record will reflect in the other<br/>
        one automatically. You can also unite them with additional records.</p>

        <p><strong class="topic">Which records can be united?</strong> You can unite Problem/Debate records, Position records,<br/>
        Term records, Note records, and Labels. Exception: You cannot unite a Problem/Debate<br/>
        record with a Position record. Additionally, you cannot unite multiple records of the<br/>
        same type.</p>

        <p><strong class="topic">What happens when records are united?</strong> Uniting records does not merge them into a<br/>
        single record. They remain distinct, each maintaining unique fields that the other record<br/>
        types do not possess.</p>

        <p><strong class="topic">Disuniting records:</strong> Once records are united, you can right/secondary-click here to dis-<br/>
        unite them if needed.</p>
      </body>
      </html>
      """);

    label.setTooltip(uniteToolTip);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This opens the Term Select popup and allows you to pick an existing Concept
   * or create a new Term. It does not allow you to create a new Concept to unite with
   * for an existing Term.
   */
  private static void uniteWithTermClick()
  {
    if (ui.cantSaveRecord()) return;

    HDT_RecordWithMainText source = (HDT_RecordWithMainText) ui.activeRecord();

    SelectTermDlgCtrlr selectTermDlgCtrlr = SelectTermDlgCtrlr.showPopupToChooseTermToUniteWith(source);

    if (selectTermDlgCtrlr.showModal() == false) return;

    MutableBoolean createdNewTerm = new MutableBoolean();
    Property<HDT_Concept> conceptProp = new SimpleObjectProperty<>();

    try
    {
      if (selectTermDlgCtrlr.uniteWith(source, createdNewTerm, conceptProp) == false)
        return;
    }
    catch (HyperDataException e)
    {
      errorPopup(e.getMessage());
      return;
    }

    if (createdNewTerm.isFalse())
    {
      ui.update();
      return;
    }

    ui.goToRecord(conceptProp.getValue(), false);
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
      updateUniteDisuniteLinks(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord(boolean saveNameIfBlank)
  {
    HDT_CT nodeRecord = getNodeRecord();

    if (nodeRecord.getType() == hdtConcept)
    {
      if (tfSearchKey.getText().isBlank())
        return falseWithErrorPopup("Unable to modify record: search key of term cannot be blank.", tfSearchKey);

      if (saveNameIfBlank && tfName.getText().isBlank())
        return falseWithErrorPopup("Unable to modify record: term cannot be zero-length.", tfName);
    }
    else if (saveNameIfBlank && nameCheck(tfName, "record name") == false) return false;

    if (saveSearchKey(nodeRecord, tfSearchKey) == false) return false;

    if (saveNameIfBlank || (tfName.getText().isBlank() == false))
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
      updateUniteDisuniteLinks(nodeRecord);

    nodeRecord.viewNow();

    safeFocus(tfName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void updateWebButtons(Preferences node)
  {
    updateWebButtons(node, WebButtonContextPrefKey.GEN, 4, btnWebSrch1, smbWebSrch1, this::searchBtnEvent);

    btnWebSrch2.setText(ui.webButtonMap.get(WebButtonContextPrefKey.GEN + '2').getCaption());
    btnWebSrch3.setText(ui.webButtonMap.get(WebButtonContextPrefKey.GEN + '3').getCaption());
    btnWebSrch4.setText(ui.webButtonMap.get(WebButtonContextPrefKey.GEN + '4').getCaption());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EventHandler<ActionEvent> searchBtnEvent(String prefKey)
  {
    return event -> ui.webButtonMap.get(prefKey).first(WebButtonField.Name, tfName.getText()).go();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
