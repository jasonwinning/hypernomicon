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

package org.hypernomicon.view.mainText;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.hypernomicon.App;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.Connector;
import org.hypernomicon.model.items.KeyWork;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.MainText.DisplayItem;
import org.hypernomicon.model.items.MainText.DisplayItemType;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.view.HyperView.TextViewInfo;

import static org.apache.commons.text.StringEscapeUtils.escapeEcmaScript;
import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.util.MediaUtil.*;

import javafx.concurrent.Worker;
import javafx.event.Event;
import javafx.fxml.FXMLLoader;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import netscape.javascript.JSObject;

//---------------------------------------------------------------------------

public final class MainTextWrapper
{
  private static BorderPane bpEditorRoot;
  private static MainTextCtrlr editCtrlr;
  private static WebView view;
  private static WebEngine we;
  private static MainTextWrapper curWrapper;
  private static String textToHilite = "",
                        lastTextToHilite = "";

  private static StringBuilder jQueryContents  = new StringBuilder(),
                               jHiliteContents = new StringBuilder();

  private AnchorPane parentPane;
  private HDT_RecordWithConnector curRecord;
  private String html, completeHtml;
  private List<DisplayItem> displayItems;
  private List<KeyWork> keyWorks;
  private TextViewInfo viewInfo;

  private boolean editing = false, edited = false, showing = false;

//---------------------------------------------------------------------------

  public MainTextWrapper(AnchorPane parentPane)
  {
    this.parentPane = parentPane;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public boolean isEditing()         { return showing && editing; }
  public WebEngine getEditorEngine() { return editCtrlr == null ? null : editCtrlr.getEngine(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void init()
  {
    view = new WebView();
    webViewAddZoom(view, PREF_KEY_MAINTEXT_ZOOM);

    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/mainText/MainTextEditor.fxml"));

    try { bpEditorRoot = loader.load(); } catch (IOException e) { noOp(); }
    editCtrlr = loader.getController();
    editCtrlr.init();

    setAnchors(bpEditorRoot, 0.0, 0.0, 0.0, 0.0);
    setAnchors(view        , 0.0, 0.0, 0.0, 0.0);

    view.setOnContextMenuRequested(event -> setHTMLContextMenu());

    view.getEngine().getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
    {
      if (newState == Worker.State.SUCCEEDED)
      {
        if (textToHilite.length() > 0)
          lastTextToHilite = new String(textToHilite);

        if (lastTextToHilite.length() > 0)
          hiliteText(textToHilite, we);

        textToHilite = "";
      }
    });

    curWrapper = null;

    try
    {
      readResourceTextFile("resources/jquery.min.js", jQueryContents, false);
      readResourceTextFile("resources/jquery.highlight-4.closure.js", jHiliteContents, false);
    }
    catch (IOException e)
    {
      messageDialog("Unable to initialize find-in-hyperTab capability.", mtError);
    }

    view.setFocusTraversable(false);

    we = view.getEngine();

    view.setOnDragDropped(Event::consume);

    we.titleProperty().addListener((ob, oldTitle, newTitle) -> handleJSEvent(curWrapper.completeHtml, we, curWrapper.viewInfo));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void rescale()
  {
    scaleNodeForDPI(bpEditorRoot);
    setFontSize(bpEditorRoot);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setReadOnlyHTML(String htmlToUse, WebEngine weToUse, TextViewInfo viewInfo, HDT_Record recordToHilite)
  {
    setReadOnlyHTML(htmlToUse, weToUse, viewInfo, recordToHilite, false);
  }

  private static void setReadOnlyHTML(String htmlToUse, WebEngine weToUse, TextViewInfo viewInfo, HDT_Record recordToHilite, boolean alreadyPrepped)
  {
    if (alreadyPrepped == false)
      htmlToUse = prepHtmlForDisplay(htmlToUse);

    if (textToHilite.isEmpty())
      lastTextToHilite = "";

    if (viewInfo.scrollPos > 0)
      htmlToUse = htmlToUse.replace("<body", "<body onload='window.scrollTo(0," + viewInfo.scrollPos + ")'");

    Document doc = makeDocLinksExternal(Jsoup.parse(htmlToUse.replace("contenteditable=\"true\"", "contenteditable=\"false\"")));

    addLinks(doc.body(), recordToHilite);

    weToUse.loadContent(doc.html().replace("</head>", headContent));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear(boolean show)
  {
    removeFromParent(bpEditorRoot);
    removeFromParent(view);

    html = ""; completeHtml = "";
    curRecord = null;
    editing = false;
    edited = false;
    displayItems = null;
    keyWorks = null;

    if (show && (ui.isShuttingDown() == false)) showReadOnly();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void hide()
  {
    if (editing)
    {
      keyWorks = new ArrayList<>();
      html = editCtrlr.getHtmlAndKeyWorks(keyWorks);

      displayItems = new ArrayList<>(editCtrlr.getDisplayItems());

      setCompleteHtml();
    }

    showing = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void showReadOnly()
  {
    editCtrlr.clear();

    editing = false;

    removeFromParent(bpEditorRoot);
    removeFromParent(view);

    parentPane.getChildren().setAll(view);

    if (curRecord == null)
    {
      we.loadContent("");
      return;
    }

    MainTextWrapper mainTextWrapper = this;

    view.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2) && canEdit())
        mainTextWrapper.beginEditing(true);
    });

    boolean noDisplayRecords = displayItems == null ? true : displayItems.stream().noneMatch(item -> item.type == DisplayItemType.diRecord);

    int keyWorksSize = keyWorks == null ? 0 : getNestedKeyWorkCount(curRecord, keyWorks);

    if (Jsoup.parse(html).text().trim().isEmpty() && (keyWorksSize == 0) && noDisplayRecords && canEdit())
      beginEditing(false);
    else
      setReadOnlyHTML(completeHtml, we, viewInfo, null, true);

    showing = true;
    curWrapper = this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean canEdit()
  {
    return curRecord == null ? false : (HyperDB.isUnstoredRecord(curRecord.getID(), curRecord.getType()) == false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void hilite(String string)
  {
    if (editing)
    {
      if (editCtrlr.isEmpty())
        return;

      hide();
      textToHilite = string;
      showReadOnly();
    }
    else
    {
      textToHilite = string;
      hiliteText(string, we);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void hiliteText(String string, WebEngine weToUse)
  {
    string = escapeEcmaScript(string);

    weToUse.executeScript(jQueryContents.toString() + System.lineSeparator() + jHiliteContents.toString());
    weToUse.executeScript("$('body').removeHighlight().highlight('" + string + "')");
    weToUse.executeScript("var els = document.getElementsByClassName('highlight'); if (typeof(els[0]) != 'undefined') els[0].scrollIntoView();");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TextViewInfo getViewInfo()
  {
    if (curRecord == null)
      return new TextViewInfo(0);

    if (editing)
      return new TextViewInfo(editCtrlr.getScrollPos());

    if (viewInfo == null)
      viewInfo = new TextViewInfo();

    viewInfo.scrollPos = webEngineScrollPos(we);

    boolean sortByName = db.prefs.getBoolean(PREF_KEY_KEY_WORK_SORT_BY_NAME, true);

    JSObject divits = (JSObject) we.executeScript("var i,prefix,openDivits = [],elements = document.getElementsByTagName('details');\n" +
                                                  "for(i=0; i<elements.length; i++)\n{\n" +
                                                  "  prefix = elements[i].id.slice(0,3);\n" +
                                                  "  if ((prefix === \"" + (sortByName ? "alp" : "num") + "\") && (elements[i].open === true)) openDivits.push(elements[i].id.slice(3));\n" +
                                                  "  if ((prefix !== \"alp\") && (prefix !== \"num\") && (elements[i].open === true)) openDivits.push(elements[i].id);\n}\n" +
                                                  "openDivits");
    viewInfo.openDivits = new HashSet<>();

    int len = Integer.class.cast(divits.getMember("length")).intValue();
    String divitID = "";

    for (int ndx = 0; ndx < len; ndx++)
    {
      divitID = (String) divits.getSlot(ndx);

      if (divitID.length() > 0)
        viewInfo.openDivits.add(divitID);
    }

    return viewInfo;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void loadFromRecord(HDT_RecordWithConnector record, boolean show, TextViewInfo viewInfo)
  {
    curRecord = record;
    this.viewInfo = viewInfo;

    MainText mainText = record.getMainText();

    displayItems = mainText.getDisplayItemsCopy();

    if (displayItems == null)
    {
      displayItems = new ArrayList<>();
      MainText.addDefaultItemsToList(record, displayItems);
    }

    html = mainText.getHtml();
    keyWorks = mainText.getKeyWorks();

    setCompleteHtml();

    edited = false;

    if (show) showReadOnly();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_WorkLabel getLabelOfRecord(HDT_RecordWithConnector record)
  {
    HDT_RecordWithConnector mainTextRecord = record.getMainText().getRecord();

    if (mainTextRecord.getType() == hdtWorkLabel) return (HDT_WorkLabel) mainTextRecord;
    if (mainTextRecord.getType() != hdtHub)       return null;

    return HDT_Hub.class.cast(mainTextRecord).getLink().getLabel();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int getNestedKeyWorkCount(HDT_RecordWithConnector record, List<KeyWork> passedKeyWorks)
  {
    int keyWorkCount = passedKeyWorks.size();

    HDT_WorkLabel parentLabel = getLabelOfRecord(record);
    if (parentLabel == null) return keyWorkCount;

    for (HDT_WorkLabel label : parentLabel.subLabels)
      keyWorkCount += getNestedKeyWorkCount(label, label.getMainText().getKeyWorks());

    return keyWorkCount;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<HDT_Concept> getRelatedConcepts(StrongLink link)
  {
    List<HDT_Concept> concepts = new ArrayList<>();
    if (link == null) return concepts;

    link.getSpokes().forEach(spoke -> { switch (spoke.getType())
    {
      case hdtDebate :

        HDT_Debate debate = link.getDebate();

        addLinkedTerms(debate.largerDebates, concepts);
        addLinkedTerms(debate.subDebates, concepts);
        addLinkedTerms(debate.positions, concepts);
        break;

      case hdtPosition :

        HDT_Position position = link.getPosition();

        addLinkedTerms(position.debates, concepts);
        addLinkedTerms(position.largerPositions, concepts);
        addLinkedTerms(position.subPositions, concepts);
        break;

      case hdtNote :

        HDT_Note note = link.getNote();

        addLinkedTerms(note.parentNotes, concepts);
        addLinkedTerms(note.subNotes, concepts);
        break;

      case hdtWorkLabel :

        HDT_WorkLabel label = link.getLabel();

        addLinkedTerms(label.parentLabels, concepts);
        addLinkedTerms(label.subLabels, concepts);
        break;

      default :
        break;
    }});

    concepts.removeIf(this::displayerIsAlreadyShowing);

    concepts.sort(sortBasis(HDT_Record::getSortKey));

    return concepts;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addLinkedTerms(List<? extends HDT_RecordWithConnector> uRecords, List<HDT_Concept> concepts)
  {
    uRecords.forEach(uRecord ->
    {
      if (uRecord.isLinked() == false) return;

      nullSwitch(uRecord.getLink().getConcept(), concept ->
      {
        if (concepts.contains(concept) == false)
          concepts.add(concept);
      });
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String KEYWORKS_DIVIT_ID = "keyWorks";

  private void setCompleteHtml()
  {
    HDT_WorkLabel curLabel = getLabelOfRecord(curRecord);
    Document doc = Jsoup.parse(prepHtmlForDisplay(html));
    StringBuilder innerHtml = new StringBuilder("");
    MutableBoolean firstOpen = new MutableBoolean(false);
    int keyWorksSize = getNestedKeyWorkCount(curRecord, keyWorks);

    Element styleTag = doc.head().getElementsByTag("style").stream().findFirst().orElse(null);
    if ((styleTag != null) && (styleTag.outerHtml().contains("margin-right") == false))
    {
      doc.getElementsByTag("style").forEach(Element::remove);
      styleTag = null;
    }
    
    if (styleTag == null)
      doc.head().prepend(mainTextHeadStyleTag());

    if (doc.body().text().trim().isEmpty())
      firstOpen.setTrue();

    MutableInt tagNdx = new MutableInt(0);

    displayItems.forEach(item ->
    {
      switch (item.type)
      {
        case diDescription:

          StringBuilder relRecordsHtml = new StringBuilder();
          if (curRecord.getType() == hdtConcept)
          {
            List<HDT_Concept> concepts = getRelatedConcepts(curRecord.getLink());

            concepts.forEach(concept ->
            {
              relRecordsHtml.append(relRecordsHtml.length() == 0 ? "<b hypncon-no-links=true>Related concepts: </b>" : "; ");
              relRecordsHtml.append(getGoToRecordAnchor(concept, "", concept.getExtendedName()));
            });
          }
          else
          {
            Set<Connector> displayers = curRecord.getMainText().getDisplayers();

            displayers.removeIf(displayer -> displayerIsAlreadyShowing(displayer.getSpoke()));

            displayers.forEach(displayer ->
            {
              relRecordsHtml.append(relRecordsHtml.length() == 0 ? "<b hypncon-no-links=true>Displayers: </b>" : "; ");
              relRecordsHtml.append(getGoToRecordAnchor(displayer.getSpoke(), "", displayer.getSpoke().getCBText()));
            });
          }

          String plainText = doc.body().text().trim();

          if ((relRecordsHtml.length() > 0) || (plainText.length() > 0))
          {
            if (innerHtml.length() > 0)
              innerHtml.append("<br>");

            if (relRecordsHtml.length() > 0)
            {
              innerHtml.append(relRecordsHtml + "<br>");

              if (plainText.length() > 0) innerHtml.append("<br>");
            }

            if (plainText.length() > 0)
              innerHtml.append(doc.body().html());
          }

          break;

        case diKeyWorks:

          if (keyWorksSize > 0)
          {
            boolean sortByName = db.prefs.getBoolean(PREF_KEY_KEY_WORK_SORT_BY_NAME, true);

            if (innerHtml.length() > 0)
              innerHtml.append("<br>");

            innerHtml.append(detailsTag(KEYWORKS_DIVIT_ID, viewInfo, true) + "<summary><b>Key Works</b>&nbsp;")
                     .append("<a hypncon=\"true\" href=\"\" title=\"Turn key work details on/off\" onclick=\"javascript:callToJava(" + String.valueOf(JS_EVENT_DETAILED_KEY_WORKS) + "); return false;\"><img border=0 width=16 height=16 src=\"" + imgDataURI("resources/images/key-work-details.png") + "\"></img></a>")
                     .append("<span style=\"display: " + (sortByName ? "inline" : "none") + ";\" class=\"" + ALPHA_SORTED_OUTER_CLASS + "\"><a hypncon=\"true\" title=\"Sort by year\" href=\"\" onclick=\"javascript:switchTo19(); return false;\"><img border=0 width=16 height=16 src=\"" + imgDataURI("resources/images/sort_19.png") + "\"></img></a></span>")
                     .append("<span style=\"display: " + (sortByName ? "none" : "inline") + ";\" class=\"" + NUMERIC_SORTED_OUTER_CLASS + "\"><a hypncon=\"true\" title=\"Sort alphabetically\" href=\"\" onclick=\"javascript:switchToAZ(); return false;\"><img border=0 width=16 height=16 src=\"" + imgDataURI("resources/images/sort_az.png") + "\"></img></a></span>")
                     .append("</summary><br><div class=\"" + NUMERIC_SORTED_OUTER_CLASS + "\" style=\"margin-left: 3.5em; display: " + (sortByName ? "none" : "block") + ";\">");
            appendKeyWorkSpanAndBody(curRecord, innerHtml, false, tagNdx, true, viewInfo);

            if ((curLabel != null) && (curLabel.subLabels.isEmpty() == false))
            {
              if (keyWorks.size() > 0) innerHtml.append("<br>");
              appendSubLabelsKeyWorkBody(curLabel, innerHtml, false, tagNdx, viewInfo, "num" + makeElementID(curLabel));
            }

            innerHtml.append("</div><div class=\"" + ALPHA_SORTED_OUTER_CLASS + "\" style=\"margin-left: 3.5em; display: " + (sortByName ? "block" : "none") + ";\">");
            appendKeyWorkSpanAndBody(curRecord, innerHtml, true, tagNdx, true, viewInfo);

            if ((curLabel != null) && (curLabel.subLabels.isEmpty() == false))
            {
              if (keyWorks.size() > 0) innerHtml.append("<br>");
              appendSubLabelsKeyWorkBody(curLabel, innerHtml, true, tagNdx, viewInfo, "alp" + makeElementID(curLabel));
            }

            innerHtml.append("</div></details>");
          }
          else
          {
            if (viewInfo.openDivits == null)
              viewInfo.openDivits = new HashSet<>();

            viewInfo.openDivits.add(KEYWORKS_DIVIT_ID); // make sure keyworks divit becomes open once a keywork is added
          }

          break;

        case diRecord:

          if (innerHtml.length() > 0)
            innerHtml.append("<br>");

          String cbText = item.record.getCBText();

          if (firstOpen.isTrue())
          {
            innerHtml.append(detailsTag(makeElementID(item.record), viewInfo, true) + "<summary><b>");
            firstOpen.setFalse();
          }
          else
            innerHtml.append(detailsTag(makeElementID(item.record), viewInfo, false) + "<summary><b>");

          innerHtml.append(db.getTypeName(item.record.getType()) + ": " +
                           getKeywordLink(cbText, new KeywordLink(0, cbText.length(), new SearchKeyword(cbText, item.record)), "text-decoration: none;") +
                           "</b></summary><br><div style=\"margin-left: 3.5em;\">");

          String secondaryHtml = getSecondaryDisplayHtml(item.record, tagNdx, viewInfo);

          innerHtml.append(secondaryHtml).append("</div></details>");
          break;

        default:
          break;
      }
    });

    doc.body().html(innerHtml.toString());
    completeHtml = doc.html();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean displayerIsAlreadyShowing(HDT_RecordWithConnector displayer)
  {
    for (DisplayItem displayItem : displayItems)
    {
      if (displayItem.record == displayer)
        return true;

      if (nullSwitch(displayItem.record, false, record -> nullSwitch(record.getHub(), false, hub -> hub == displayer.getHub())))
        if (displayItem.record.name().equals(displayer.name()))
          return true;
    }

    switch (curRecord.getType())
    {
      case hdtPosition :

        HDT_Position position = (HDT_Position) curRecord;
        return position.subPositions.contains(displayer) || position.arguments.contains(displayer);

      case hdtDebate :

        HDT_Debate debate = (HDT_Debate) curRecord;
        return debate.positions.contains(displayer) || debate.subDebates.contains(displayer);

      case hdtArgument :

        HDT_Argument arg = (HDT_Argument) curRecord;
        return arg.counterArgs.contains(displayer);

      default : return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void beginEditing(boolean focus)
  {
    if (canEdit() == false) return;

    removeFromParent(bpEditorRoot);
    removeFromParent(view);

    parentPane.getChildren().setAll(bpEditorRoot);

    if (edited)
      editCtrlr.set(curRecord, html, displayItems, keyWorks);
    else if (curRecord != null)
      editCtrlr.setFromMainText(curRecord.getMainText());
    else
      editCtrlr.clear();

    if (focus) editCtrlr.focus();

    editing = true;
    edited = true;
    showing = true;
    curWrapper = this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveToRecord(HDT_RecordWithConnector record)
  {
    curRecord = record;
    save();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void save()
  {
    if ((curRecord == null) || (edited == false))
      return;

    if (showing && editing)
    {
      keyWorks = new ArrayList<>();
      curRecord.getMainText().setHtml(editCtrlr.getHtmlAndKeyWorks(keyWorks));
      curRecord.getMainText().setKeyWorksFromList(keyWorks, true);
      curRecord.getMainText().setDisplayItemsFromList(editCtrlr.getDisplayItems());
    }
    else
    {
      curRecord.getMainText().setHtml(html);
      curRecord.getMainText().setDisplayItemsFromList(displayItems);
      curRecord.getMainText().setKeyWorksFromList(keyWorks, true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}