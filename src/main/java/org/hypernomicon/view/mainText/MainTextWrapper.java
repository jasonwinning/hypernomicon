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

package org.hypernomicon.view.mainText;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.function.Predicate;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.KeyWork;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.model.unities.MainText.DisplayItem;
import org.hypernomicon.model.unities.MainText.DisplayItemType;
import org.hypernomicon.view.HyperView.TextViewInfo;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;
import static org.hypernomicon.view.mainText.MainTextWrapper.MTW_State.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import javafx.event.Event;
import javafx.scene.input.MouseButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import netscape.javascript.JSObject;

//---------------------------------------------------------------------------

public final class MainTextWrapper
{
  public enum MTW_State
  {
    showingReadOnly,
    editing,
    hidden
  }

  private static BorderPane bpEditorRoot;
  private static MainTextCtrlr editCtrlr;
  private static WebView view;
  private static WebEngine we;
  private static Highlighter highlighter;
  private static MainTextWrapper curWrapper;

  private final AnchorPane parentPane;
  private HDT_RecordWithMainText curRecord;
  private String html, completeHtml;
  private List<DisplayItem> displayItems;
  private List<KeyWork> keyWorks;
  private TextViewInfo textViewInfo;

  private boolean edited = false;
  private MTW_State state = hidden;

//---------------------------------------------------------------------------

  public MainTextWrapper(AnchorPane parentPane)
  {
    this.parentPane = parentPane;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean isEditing()                { return state == editing; }
  private boolean canEdit()          { return (curRecord != null) && (isUnstoredRecord(curRecord) == false); }
  static WebEngine getEditorEngine() { return editCtrlr == null ? null : editCtrlr.getEngine(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void init() throws IOException
  {
    view = new WebView();
    webViewAddZoom(view, PREF_KEY_MAINTEXT_ZOOM);

    editCtrlr = new MainTextCtrlr();
    bpEditorRoot = editCtrlr.getRootNode();

    setAnchors(bpEditorRoot, 0.0, 0.0, 0.0, 0.0);
    setAnchors(view        , 0.0, 0.0, 0.0, 0.0);

    view.setOnContextMenuRequested(event -> setHTMLContextMenu());

    curWrapper = null;

    MainTextUtil.init();
    Highlighter.init();

    view.setFocusTraversable(false);

    we = view.getEngine();
    highlighter = new Highlighter(view);

    view.setOnDragDropped(Event::consume);

    we.titleProperty().addListener((ob, oldTitle, newTitle) -> handleJSEvent(curWrapper.completeHtml, we, curWrapper.textViewInfo));
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

  public static void setReadOnlyHTML(String htmlToUse, WebEngine weToUse)
  {
    setReadOnlyHTML(htmlToUse, weToUse, 0, null, false);
  }

  public static void setReadOnlyHTML(String htmlToUse, WebEngine weToUse, int scrollPos)
  {
    setReadOnlyHTML(htmlToUse, weToUse, scrollPos, null, false);
  }

  public static void setReadOnlyHTML(String htmlToUse, WebEngine weToUse, int scrollPos, HDT_Record recordToHilite)
  {
    setReadOnlyHTML(htmlToUse, weToUse, scrollPos, recordToHilite, false);
  }

  private static void setReadOnlyHTML(String htmlToUse, WebEngine weToUse, int scrollPos, HDT_Record recordToHilite, boolean alreadyPrepped)
  {
    if (alreadyPrepped == false)
      htmlToUse = prepHtmlForDisplay(htmlToUse);

    if (scrollPos > 0)
      htmlToUse = htmlToUse.replace("<body", "<body onload='setTimeout(function() { window.scrollTo(0," + scrollPos + "); }, 0);'"); // only scroll after all other events on the queue are processed

    Document doc = makeDocLinksExternal(jsoupParse(htmlToUse.replace("contenteditable=\"true\"", "contenteditable=\"false\"")));

    addLinks(new HtmlTextNodeList(doc.body()), recordToHilite);

    weToUse.loadContent(doc.html().replace("</head>", headContent));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    removeFromParent(bpEditorRoot);
    removeFromParent(view);

    html = ""; completeHtml = "";
    curRecord = null;
    edited = false;
    displayItems = null;
    keyWorks = null;
    textViewInfo = null;

    if (ui.isShuttingDown() == false) showReadOnly();
    state = hidden;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void hide()
  {
    if (state == hidden)
      return;

    if (state == editing)
    {
      commit();

      renderCompleteHtml();
    }
    else
      textViewInfo = getViewInfo(curRecord);

    state = hidden;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private long lastRender = 0L;

  public void showReadOnly()
  {
    editCtrlr.clear();

    state = showingReadOnly;

    lastRender = Instant.now().toEpochMilli();

    removeFromParent(bpEditorRoot);
    removeFromParent(view);

    parentPane.getChildren().setAll(view);

    if (curRecord == null)
    {
      we.loadContent("");
      return;
    }

    view.setOnMouseClicked(mouseEvent ->
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2) && canEdit())
      {
        textViewInfo = getViewInfo(curRecord);
        beginEditing(true);
      }
    });

    boolean noDisplayRecords = (displayItems == null) || displayItems.stream().noneMatch(item -> item.type == DisplayItemType.diRecord);

    int keyWorksSize = keyWorks == null ? 0 : getNestedKeyWorkCount(curRecord, keyWorks);

    if (jsoupParse(html).text().trim().isEmpty() && ((keyWorksSize == 0) || (curRecord.getType() == hdtInvestigation)) && noDisplayRecords && canEdit())
      beginEditing(false);
    else
      setReadOnlyHTML(completeHtml, we, textViewInfo.scrollPos, null, true);

    curWrapper = this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void hilite()
  {
    if (state == editing)
      editCtrlr.hilite();
    else if (state == showingReadOnly)
      highlighter.hilite();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void nextSearchResult()
  {
    if (state == editing)
      editCtrlr.nextSearchResult();
    else if (state == showingReadOnly)
      highlighter.nextSearchResult();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void previousSearchResult()
  {
    if (state == editing)
      editCtrlr.previousSearchResult();
    else if (state == showingReadOnly)
      highlighter.previousSearchResult();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TextViewInfo getViewInfo(HDT_Record viewRecord)
  {
    if (curRecord == null)
      return new TextViewInfo(viewRecord);

    assert(curRecord == viewRecord);

    if (state == editing)
    {
      assert(textViewInfo.record == viewRecord);

      textViewInfo.scrollPos = editCtrlr.getScrollPos();
      return textViewInfo;
    }

    if (state == hidden)
    {
      if (textViewInfo == null)
        textViewInfo = new TextViewInfo(viewRecord);

      return textViewInfo;
    }

    assert(textViewInfo != null);
    assert(textViewInfo.record == viewRecord);

    if ((Instant.now().toEpochMilli() - lastRender) < 200) // Assume user didn't interact with WebView in less than 200 ms
      return textViewInfo;

    textViewInfo.scrollPos = webEngineScrollPos(we);

    boolean sortByName = db.prefs.getBoolean(PREF_KEY_KEY_WORK_SORT_BY_NAME, true);

    JSObject divits = (JSObject) we.executeScript("var i,prefix,openDivits = [],elements = document.getElementsByTagName('details');\n" +
                                                  "for(i=0; i<elements.length; i++)\n{\n" +
                                                  "  prefix = elements[i].id.slice(0,3);\n" +
                                                  "  if ((prefix === \"" + (sortByName ? "alp" : "num") + "\") && (elements[i].open === true)) openDivits.push(elements[i].id.slice(3));\n" +
                                                  "  if ((prefix !== \"alp\") && (prefix !== \"num\") && (elements[i].open === true)) openDivits.push(elements[i].id);\n}\n" +
                                                  "openDivits");
    textViewInfo.openDivits = new HashSet<>();

    for (int len = (Integer) divits.getMember("length"), ndx = 0; ndx < len; ndx++)
    {
      String divitID = (String) divits.getSlot(ndx);

      if (divitID.length() > 0)
        textViewInfo.openDivits.add(divitID);
    }

    return textViewInfo;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void loadFromRecord(HDT_RecordWithMainText record, boolean show, TextViewInfo textViewInfo)
  {
    curRecord = record;
    this.textViewInfo = textViewInfo;

    MainText mainText = record.getMainText();

    displayItems = mainText.getDisplayItemsCopy();

    if (displayItems == null)
    {
      displayItems = new ArrayList<>();
      MainText.addDefaultItemsToList(record, displayItems);
    }

    html = mainText.getHtml();
    keyWorks = mainText.getKeyWorksUnmod();

    renderCompleteHtml();

    edited = false;

    if (show) showReadOnly();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_WorkLabel getLabelOfRecord(HDT_RecordWithMainText record)
  {
    HDT_RecordWithMainText mainTextRecord = record.getMainText().getRecord();

    if (mainTextRecord.getType() == hdtWorkLabel) return (HDT_WorkLabel) mainTextRecord;
    if (mainTextRecord.getType() != hdtHub)       return null;

    return mainTextRecord.getHub().getLabel();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int getNestedKeyWorkCount(HDT_RecordWithMainText record, List<KeyWork> passedKeyWorks)
  {
    int keyWorkCount = passedKeyWorks.size();

    HDT_WorkLabel parentLabel = getLabelOfRecord(record);
    if (parentLabel == null) return keyWorkCount;

    for (HDT_WorkLabel label : parentLabel.subLabels)
      keyWorkCount += getNestedKeyWorkCount(label, label.getMainText().getKeyWorksUnmod());

    return keyWorkCount;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<HDT_Concept> getRelatedConcepts(HDT_Hub hub)
  {
    List<HDT_Concept> concepts = new ArrayList<>();
    if (hub == null) return concepts;

    hub.getSpokes().forEachOrdered(spoke -> { switch (spoke.getType())
    {
      case hdtDebate :

        HDT_Debate debate = hub.getDebate();

        addLinkedTerms(debate.largerDebates, concepts);
        addLinkedTerms(debate.subDebates   , concepts);
        addLinkedTerms(debate.subPositions , concepts);
        break;

      case hdtPosition :

        HDT_Position position = hub.getPosition();

        addLinkedTerms(position.largerDebates  , concepts);
        addLinkedTerms(position.largerPositions, concepts);
        addLinkedTerms(position.subPositions   , concepts);
        addLinkedTerms(position.subDebates     , concepts);
        break;

      case hdtNote :

        HDT_Note note = hub.getNote();

        addLinkedTerms(note.parentNotes, concepts);
        addLinkedTerms(note.subNotes   , concepts);
        break;

      case hdtWorkLabel :

        HDT_WorkLabel label = hub.getLabel();

        addLinkedTerms(label.parentLabels, concepts);
        addLinkedTerms(label.subLabels   , concepts);
        break;

      default :
        break;
    }});

    concepts.removeIf(this::displayerIsAlreadyShowing);

    nullSwitch(hub.getConcept(), concept ->
    {
      concepts.removeAll(concept.parentConcepts);
      concepts.removeAll(concept.subConcepts);
      concepts.removeAll(concept.term.get().concepts);
    });

    concepts.sort(Comparator.comparing(HDT_Record::getSortKey));

    return concepts;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addLinkedTerms(List<? extends HDT_RecordWithMainText> uRecords, List<HDT_Concept> concepts)
  {
    uRecords.forEach(uRecord ->
    {
      if (uRecord.hasHub() == false) return;

      nullSwitch(uRecord.getHub().getConcept(), concept ->
      {
        if (concepts.contains(concept) == false)
          concepts.add(concept);
      });
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String KEYWORKS_DIVIT_ID = "keyWorks";

  private void renderCompleteHtml()
  {
    Document doc = jsoupParse(prepHtmlForDisplay(html));

    Element styleTag = doc.head().getElementsByTag("style").stream().findFirst().orElse(null);
    if ((styleTag != null) && (styleTag.outerHtml().contains("margin-right") == false))
    {
      doc.getElementsByTag("style").forEach(Element::remove);
      styleTag = null;
    }

    if (styleTag == null)
      doc.head().prepend(styleTag());

    MutableBoolean firstOpen = new MutableBoolean(doc.body().text().trim().isEmpty()),
                   haventRenderedKeyWorkDisplayOptionsYet = new MutableBoolean(true);
    StringBuilder innerHtml = new StringBuilder();
    MutableInt tagNdx = new MutableInt(0);
    HDT_WorkLabel curLabel = getLabelOfRecord(curRecord);
    int keyWorksSize = getNestedKeyWorkCount(curRecord, keyWorks);

    boolean sortByName = db.prefs.getBoolean(PREF_KEY_KEY_WORK_SORT_BY_NAME, true);

    displayItems.forEach(item ->
    {
      switch (item.type)
      {
        case diDescription:

          renderDescription(innerHtml, doc);
          break;

        case diKeyWorks:

          if (curRecord.getType() != hdtInvestigation)
          {
            if (keyWorksSize > 0)
            {
              renderKeyWorks(innerHtml, tagNdx, curLabel, sortByName, haventRenderedKeyWorkDisplayOptionsYet);
            }
            else
            {
              if (textViewInfo.openDivits == null)
                textViewInfo.openDivits = new HashSet<>();

              textViewInfo.openDivits.add(KEYWORKS_DIVIT_ID); // make sure keyworks divit becomes open once a keywork is added
            }
          }

          break;

        case diRecord:

          if (innerHtml.length() > 0) innerHtml.append("<br>");

          String cbText = htmlEscaper.escape(item.record.getCBText());

          if (firstOpen.isTrue())
          {
            innerHtml.append(detailsTag(makeElementID(item.record), textViewInfo, true)).append("<summary><b>");
            firstOpen.setFalse();
          }
          else
            innerHtml.append(detailsTag(makeElementID(item.record), textViewInfo, false)).append("<summary><b>");

          innerHtml.append(getTypeName(item.record.getType())).append(": ")
                   .append(getKeywordLink(cbText, new KeywordLink(0, cbText.length(), new SearchKeyword(cbText, item.record)), "text-decoration: none;"))
                   .append("</b>&nbsp;");

          renderKeyWorkDisplayOptions(innerHtml, sortByName, haventRenderedKeyWorkDisplayOptionsYet);

          innerHtml.append("</summary><br><div style=\"margin-left: 3.5em;\">");

          String secondaryHtml = getSecondaryDisplayHtml(item.record, tagNdx, textViewInfo);

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

  private void renderDescription(StringBuilder innerHtml, Document doc)
  {
    StringBuilder relRecordsHtml = new StringBuilder();
    if (curRecord.getType() == hdtConcept)
    {
      List<HDT_Concept> concepts = getRelatedConcepts(curRecord.getHub());

      concepts.forEach(concept ->
      {
        relRecordsHtml.append(relRecordsHtml.length() == 0 ? "<b " + NO_LINKS_ATTR + "=true>Related concepts: </b>" : "; ");
        relRecordsHtml.append(getGoToRecordAnchor(concept, "", concept.extendedName()));
      });
    }
    else
    {
      db.displayerStream(curRecord).filter(Predicate.not(this::displayerIsAlreadyShowing)).forEachOrdered(displayer ->
      {
        relRecordsHtml.append(relRecordsHtml.length() == 0 ? "<b " + NO_LINKS_ATTR + "=true>Displayers: </b>" : "; ");
        relRecordsHtml.append(getGoToRecordAnchor(displayer, "", displayer.getCBText()));
      });
    }

    String plainText = doc.body().text().trim();

    if ((relRecordsHtml.length() > 0) || (plainText.length() > 0))
    {
      if (innerHtml.length() > 0)
        innerHtml.append("<br>");

      if (relRecordsHtml.length() > 0)
      {
        innerHtml.append(relRecordsHtml).append("<br>");

        if (plainText.length() > 0) innerHtml.append("<br>");
      }

      if (plainText.length() > 0)
        innerHtml.append(doc.body().html());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void renderKeyWorkDisplayOptions(final StringBuilder html, boolean sortByName, MutableBoolean haventRenderedKeyWorkDisplayOptionsYet)
  {
    if (haventRenderedKeyWorkDisplayOptionsYet.isFalse())
      return;

    html.append("<a hypncon=\"true\" href=\"\" title=\"Turn key work details on/off\" onclick=\"javascript:callToJava(").append(JS_EVENT_DETAILED_KEY_WORKS).append("); return false;\"><img border=0 width=16 height=16 src=\"").append(imgDataURI("resources/images/key-work-details.png")).append("\"></img></a>")
        .append("<span style=\"display: ").append(sortByName ? "inline" : "none"  ).append(";\" class=\"").append(ALPHA_SORTED_OUTER_CLASS  ).append("\"><a hypncon=\"true\" title=\"Sort key works by year\" href=\"\" onclick=\"javascript:switchTo19(); return false;\"><img border=0 width=16 height=16 src=\"").append(imgDataURI("resources/images/sort_19.png")).append("\"></img></a></span>")
        .append("<span style=\"display: ").append(sortByName ? "none"   : "inline").append(";\" class=\"").append(NUMERIC_SORTED_OUTER_CLASS).append("\"><a hypncon=\"true\" title=\"Sort key works alphabetically\" href=\"\" onclick=\"javascript:switchToAZ(); return false;\"><img border=0 width=16 height=16 src=\"").append(imgDataURI("resources/images/sort_az.png")).append("\"></img></a></span>");

    haventRenderedKeyWorkDisplayOptionsYet.setFalse();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void renderKeyWorks(final StringBuilder innerHtml, final MutableInt tagNdx, final HDT_WorkLabel curLabel, boolean sortByName, MutableBoolean haventRenderedKeyWorkDisplayOptionsYet)
  {
    if (innerHtml.length() > 0) innerHtml.append("<br>");

    innerHtml.append(detailsTag(KEYWORKS_DIVIT_ID, textViewInfo, true)).append("<summary><b>Key Works</b>&nbsp;");

    renderKeyWorkDisplayOptions(innerHtml, sortByName, haventRenderedKeyWorkDisplayOptionsYet);

    innerHtml.append("</summary>");

    if (keyWorks.size() > 0) innerHtml.append("<br>");

    innerHtml.append("<div class=\"").append(NUMERIC_SORTED_OUTER_CLASS).append("\" style=\"margin-left: 3.5em; display: ").append(sortByName ? "none" : "block").append(";\">");

    appendKeyWorkSpanAndBody(curRecord, innerHtml, false, tagNdx, true, textViewInfo);

    if ((curLabel != null) && (curLabel.subLabels.isEmpty() == false))
    {
      if (keyWorks.size() > 0) innerHtml.append("<br>");
      appendSubLabelsKeyWorkBody(curLabel, innerHtml, false, tagNdx, textViewInfo, "num" + makeElementID(curLabel));
    }

    innerHtml.append("</div><div class=\"").append(ALPHA_SORTED_OUTER_CLASS).append("\" style=\"margin-left: 3.5em; display: ").append(sortByName ? "block" : "none").append(";\">");
    appendKeyWorkSpanAndBody(curRecord, innerHtml, true, tagNdx, true, textViewInfo);

    if ((curLabel != null) && (curLabel.subLabels.isEmpty() == false))
    {
      if (keyWorks.size() > 0) innerHtml.append("<br>");
      appendSubLabelsKeyWorkBody(curLabel, innerHtml, true, tagNdx, textViewInfo, "alp" + makeElementID(curLabel));
    }

    innerHtml.append("</div></details>");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean displayerIsAlreadyShowing(HDT_RecordWithMainText displayer)
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
        return position.subPositions.contains(displayer) || position.subDebates.contains(displayer) || position.arguments.contains(displayer);

      case hdtDebate :

        HDT_Debate debate = (HDT_Debate) curRecord;
        return debate.subPositions.contains(displayer) || debate.subDebates.contains(displayer);

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

    state = editing;
    edited = true;
    curWrapper = this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveToRecord(HDT_RecordWithMainText record)
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

    MainText mainText = curRecord.getMainText();

    if (state == editing)
      commit();

    mainText.setHtml(html);
    mainText.setDisplayItemsFromList(displayItems);

    if (curRecord.getType() != hdtInvestigation) mainText.setKeyWorksFromList(keyWorks);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void commit()
  {
    keyWorks = new ArrayList<>();
    String tempHtml = editCtrlr.getHtmlAndKeyWorks(keyWorks);
    if (tempHtml.contains("hypernomiconHilite"))
    {
      messageDialog(new HDB_InternalError(28469).getMessage(), mtError);
      return;
    }

    html = tempHtml;
    displayItems = new ArrayList<>(editCtrlr.getDisplayItems());

    textViewInfo = getViewInfo(curRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refreshRecordPtr()
  {
    if (curRecord != null)
      curRecord = HDT_Record.getCurrentInstance(curRecord);

    if (textViewInfo != null)
      textViewInfo = new TextViewInfo(textViewInfo);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
