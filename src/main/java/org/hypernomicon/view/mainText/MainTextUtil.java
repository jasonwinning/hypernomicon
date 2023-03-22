/*
 * Copyright 2015-2023 Jason Winning
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

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.KeywordLinkList.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import static org.apache.commons.text.StringEscapeUtils.*;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Note;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithAuthors;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.KeyWork;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.model.records.HDT_WorkLabel;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.HtmlTextNodeList.HtmlTextNode;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Attribute;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;
import org.jsoup.parser.Parser;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.scene.input.ScrollEvent;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import netscape.javascript.JSException;
import netscape.javascript.JSObject;

public final class MainTextUtil
{
  private MainTextUtil() { throw new UnsupportedOperationException(); }

  public static final String  headContent,
                              scriptContent,
                              EMBEDDED_FILE_TAG = "misc-file";

  static final String         NO_LINKS_ATTR              = "hypncon-no-links",
                              ALPHA_SORTED_OUTER_CLASS   = "sortedKeyWorksAZ",
                              NUMERIC_SORTED_OUTER_CLASS = "sortedKeyWorks19";
  private static final String ALPHA_SORTED_INNER_CLASS   = "keyWorksSpanAZ",
                              NUMERIC_SORTED_INNER_CLASS = "keyWorksSpan19",
                              TOPMOST_CLASS              = "topmostKeyWorksSpan",
                              hiliteStyles;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int JS_EVENT_OPEN_RECORD         = 1,
                           JS_EVENT_OPEN_PREVIEW        = 2,
                           JS_EVENT_OPEN_URL            = 3,
                           JS_EVENT_LAUNCH_FILE         = 4,
                           JS_EVENT_OPEN_FILE           = 5,    // May launch or open preview depending on user setting
                           JS_EVENT_SET_SORT_KEY_METHOD = 6;
  static final int         JS_EVENT_DETAILED_KEY_WORKS  = 7;

  static
  {
    scriptContent = new StringBuilder()

      .append("<script>\n\n")
      .append("var jsToJava = {};\n")
      .append("function openFile(recordType, recordID)\n{\n")
      .append("  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(").append(JS_EVENT_OPEN_FILE).append(");\n")
      .append("}\n\n")
      .append("function openRecord(recordType, recordID)\n{\n")
      .append("  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(").append(JS_EVENT_OPEN_RECORD).append(");\n")
      .append("}\n\n")
      .append("function openPreview(recordType, recordID)\n{\n")
      .append("  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(").append(JS_EVENT_OPEN_PREVIEW).append(");\n")
      .append("}\n\n")
      .append("function openURL(url)\n{\n")
      .append("  jsToJava.url = url; callToJava(").append(JS_EVENT_OPEN_URL).append(");\n")
      .append("}\n\n")
      .append("function callToJava(eventType)\n{\n")
      .append("  jsToJava.eventType = eventType;\n")
      .append("  jsToJava.eventID = (new Date()).getTime();\n")
      .append("  jsToJava.scrollTop = document.body.scrollTop;\n")
      .append("  document.title = \"\" + jsToJava.eventID;\n")
      .append("}\n\n")
      .append("function switchToAZ()\n{\n")
      .append("  var i,elements = document.getElementsByTagName('details');\n")
      .append("  for (i=0; i<elements.length; i++)\n  {\n")
      .append("    if (elements[i].id.slice(0,3) === \"num\")\n    {\n")
      .append("      document.getElementById(\"alp\" + elements[i].id.slice(3)).open = elements[i].open;\n    }\n  }\n")
      .append("  elements = document.getElementsByClassName('").append(NUMERIC_SORTED_OUTER_CLASS).append("');\n")
      .append("  for(i=0; i<elements.length; i++) { elements[i].style.display = 'none'; }\n")
      .append("  elements = document.getElementsByClassName('").append(ALPHA_SORTED_OUTER_CLASS).append("');\n")
      .append("  for(i=0; i<elements.length; i++) { elements[i].style.display = (elements[i].tagName === 'SPAN' ? 'inline' : 'block'); }\n")
      .append("  jsToJava.sortByName = true; callToJava(").append(JS_EVENT_SET_SORT_KEY_METHOD).append(");\n")
      .append("}\n\n")
      .append("function switchTo19()\n{\n")
      .append("  var i,elements = document.getElementsByTagName('details');\n")
      .append("  for (i=0; i<elements.length; i++)\n  {\n")
      .append("    if (elements[i].id.slice(0,3) === \"alp\")\n    {\n")
      .append("      document.getElementById(\"num\" + elements[i].id.slice(3)).open = elements[i].open;\n    }\n  }\n")
      .append("  elements = document.getElementsByClassName('").append(ALPHA_SORTED_OUTER_CLASS).append("');\n")
      .append("  for(i=0; i<elements.length; i++) { elements[i].style.display = 'none'; }\n")
      .append("  elements = document.getElementsByClassName('").append(NUMERIC_SORTED_OUTER_CLASS).append("');\n")
      .append("  for(i=0; i<elements.length; i++) { elements[i].style.display = (elements[i].tagName === 'SPAN' ? 'inline' : 'block'); }\n")
      .append("  jsToJava.sortByName = false; callToJava(").append(JS_EVENT_SET_SORT_KEY_METHOD).append(");\n")
      .append("}\n\n")
      .append("</script>\n\n")
      .toString();

    hiliteStyles = ".hypernomiconHilite { background-color: yellow; } .hypernomiconHilite.hypernomiconHiliteCurrent { background-color: orange; }";

    headContent = new StringBuilder(scriptContent)

      .append("<style type=\"text/css\">\n")
      .append("  ").append(hiliteStyles).append('\n')
      .append("  details summary { outline: none; }\n")
      .append("  a:link {color:#0000FF; } a:visited {color:#0000FF; }\n")
      .append("</style></head>")
      .toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static double lastEventID = -1;

  public static void handleJSEvent(String htmlToUse, WebEngine weToUse, TextViewInfo viewInfo)
  {
    int recordID = -1;
    RecordType recordType = hdtNone;
    JSObject jsToJava;

    // It might seem strange to do this instead of passing an object to javascript with methods the script can call
    // but for some reason WebEngine.setMember doesn't seem to work while the mentions index thread is running.

    try { jsToJava = (JSObject) weToUse.executeScript("jsToJava"); } catch (JSException e) { return; }

    Object obj = jsToJava.getMember("eventType");
    if ((obj instanceof Integer) == false) return;

    int jsEvent = (Integer)obj;
    double jsEventID = (Double)jsToJava.getMember("eventID");

    if (jsEventID == lastEventID) return;
    lastEventID = jsEventID;

    if (jsEvent == JS_EVENT_OPEN_FILE)
      jsEvent = ui.btnPointerLaunch.isSelected() ? JS_EVENT_LAUNCH_FILE : JS_EVENT_OPEN_PREVIEW;

    if ((jsEvent == JS_EVENT_OPEN_RECORD) || (jsEvent == JS_EVENT_LAUNCH_FILE) || (jsEvent == JS_EVENT_OPEN_PREVIEW))
    {
      recordID = (Integer)jsToJava.getMember("recordID");
      int recordTypeOrd = (Integer)jsToJava.getMember("recordType");
      recordType = getEnumVal(recordTypeOrd, RecordType.class);

      if ((recordType == hdtNote) && (jsEvent == JS_EVENT_OPEN_PREVIEW))
        jsEvent = JS_EVENT_LAUNCH_FILE;
    }

    switch (jsEvent)
    {
      case JS_EVENT_OPEN_RECORD :

        if (recordType == hdtWorkLabel)
          ui.goToTreeRecord(db.records(recordType).getByID(recordID));
        else
          ui.goToRecord(db.records(recordType).getByID(recordID), true);
        break;

      case JS_EVENT_OPEN_PREVIEW :

        if (recordType == hdtWork)
        {
          HDT_Work work = db.works.getByID(recordID);
          previewWindow.setPreview(pvsOther, work.filePathIncludeExt(), work.getStartPageNum(), work.getEndPageNum(), work);
        }
        else
        {
          HDT_MiscFile miscFile = db.miscFiles.getByID(recordID);
          previewWindow.setPreview(pvsOther, miscFile.filePath(), miscFile);
        }

        ui.openPreviewWindow(pvsOther);
        break;

      case JS_EVENT_OPEN_URL :

        openWebLink((String) jsToJava.getMember("url"));
        break;

      case JS_EVENT_LAUNCH_FILE :

        if (recordType == hdtMiscFile)
        {
          HDT_MiscFile file = db.miscFiles.getByID(recordID);

          if (file.getPath().isEmpty()) return;

          launchFile(file.filePath());
        }
        else if (recordType == hdtNote)
        {
          HDT_Note note = db.notes.getByID(recordID); // If these two lines are combined into one, for some unknown reason there
          launchFile(note.getPath().filePath());      // will be "The type HDT_Note is not visible" false-positive build errors
        }
        else
          db.works.getByID(recordID).launch(-1);

        break;

      case JS_EVENT_SET_SORT_KEY_METHOD :

        db.prefs.putBoolean(PREF_KEY_KEY_WORK_SORT_BY_NAME, (Boolean)jsToJava.getMember("sortByName"));
        break;

      case JS_EVENT_DETAILED_KEY_WORKS :

        viewInfo.detailedWorks = !viewInfo.detailedWorks;

        Document doc = Jsoup.parse(htmlToUse);

        toggleDetailedKeyWorks(doc, NUMERIC_SORTED_INNER_CLASS, false, weToUse, viewInfo.detailedWorks);
        toggleDetailedKeyWorks(doc, ALPHA_SORTED_INNER_CLASS, true, weToUse, viewInfo.detailedWorks);

        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Document makeDocLinksExternal(Document doc)
  {
    doc.getElementsByTag("a").forEach(link ->
    {
      if (link.hasAttr("hypncon")) return;

      String url = link.hasAttr("href") ? link.attr("href") : "javascript:void(0);";
      link.attributes().put(new Attribute("onclick", "openURL('" + url + "'); return false;"));
    });

    return doc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String makeLinksExternal(String s)
  {
    return makeDocLinksExternal(jsoupParse(s)).html();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private enum LinkKind { none, web, keyword }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void addLinks(HtmlTextNodeList nodes, HDT_Record recordToHilite)
  {
    String entirePlainText = nodes.toString();
    Iterator<KeywordLink> keywordLinkIterator = KeywordLinkList.generate(entirePlainText).iterator();

    KeywordLink keywordLink = keywordLinkIterator.hasNext() ? keywordLinkIterator.next() : null;

    for (int curMatchNdx = 0; curMatchNdx < entirePlainText.length();)
    {
      LinkKind kind = LinkKind.none;

      if ("http".equalsIgnoreCase(safeSubstring(entirePlainText, curMatchNdx, curMatchNdx + 4)))
        kind = LinkKind.web;
      else if ((keywordLink != null) && (curMatchNdx == keywordLink.offset))
        kind = LinkKind.keyword;

      if (kind != LinkKind.none) // Got a match
      {
        int linkTextLen = kind == LinkKind.web ? getWebLinkLen(curMatchNdx, entirePlainText) : keywordLink.length;

        for (HtmlTextNode node : nodes.getLinkNodes(curMatchNdx, curMatchNdx + linkTextLen)) // 1. Get list of node objects corresponding to matching text
        {
          TextNode textNode = node.getTextNode(); // 2. For each node object in list:

          if (curMatchNdx > node.getStartNdx()) // 3. split the textnode if match start ndx does not match start ndx of text in textnode
            textNode = node.updateStartNdx(curMatchNdx, true);

          String displayText = safeSubstring(node.getText(), 0, linkTextLen);

          if (kind == LinkKind.web)
          {
            textNode.before("<a href=\"\" onclick=\"openURL('" + displayText + "'); return false;\">" + displayText + "</a>"); // 4. Insert anchor
          }
          else
          {
            String klass = keywordLink.key.record.equals(recordToHilite) ? "hypernomiconHilite" : "";

            textNode.before(getKeywordLink(displayText, keywordLink, "", klass));  // 4. Insert anchor
          }

          int offset = displayText.length();
          node.updateStartNdx(curMatchNdx + offset, false);
          textNode.text(node.getText()); // 5. Remove link text from text node

          curMatchNdx += offset; // 6. Update oldNdx
          linkTextLen -= offset; // 7. Reduce amount of text that still needs to be turned into a link
        }

        if (kind == LinkKind.keyword)
          keywordLink = keywordLinkIterator.hasNext() ? keywordLinkIterator.next() : null;
      }
      else // there was no link this time
      {
        curMatchNdx++;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int getWebLinkLen(int startNdx, String text)
  {
    int ndx = startNdx;

    while ((ndx < text.length()) && charIsPartOfWebLink(text, ndx)) ndx++;

    return ndx - startNdx;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getKeywordLink(String text, KeywordLink link)
  {
    return getKeywordLink(text, link, "", "");
  }

  static String getKeywordLink(String text, KeywordLink link, String style)
  {
    return getKeywordLink(text, link, style, "");
  }

  private static String getKeywordLink(String text, KeywordLink link, String style, String klass)
  {
    HDT_Record record = link.key.record;

    if (record == null) return text;

    if (record.getType() == hdtHub)
    {
      record = ((HDT_Hub)record).mainSpoke();

      if (record == null)
      {
        messageDialog("Internal error #28587", mtError);
        return text;
      }
    }

    if (style.length() > 0) style = " style=\"" + style + '"';

    if (klass.length() > 0) klass = " class=\"" + klass + '"';

    if (record.getType() == hdtMiscFile)
      return getGoToRecordAnchor(record, style + klass, text) + "&nbsp;" +
        "<a hypncon=\"true\" href=\"\" title=\"Jump to this record\" onclick=\"javascript:openRecord(" + getOpenRecordParms(record) + "); return false;\">" + "<img border=0 width=16 height=16 src=\"" + imgDataURI("resources/images/view-form.png") + "\"></img></a>";

    return getGoToRecordAnchor(record, style + klass, text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getOpenRecordParms(HDT_Record record)
  {
    return record.getType().ordinal() + "," + record.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String getGoToRecordAnchor(HDT_Record record, String extraAttribs, String content)
  {
    String parms = getOpenRecordParms(record),
           anchor = "<a title=\"" + recordTooltip(record) + '"' + extraAttribs + " hypncon=\"true\" ";

    switch (record.getType())
    {
      case hdtMiscFile :
        return anchor + "href=\"\" onclick=\"javascript:openFile(" + parms + "); return false;\">" + content + "</a>";

      case hdtWork : case hdtNote :
        return anchor + "oncontextmenu=\"openFile(" + parms + "); return false;\" href=\"\" onclick=\"javascript:openRecord(" + parms + "); return false;\">" + content + "</a>";

      default :
        return anchor + "href=\"\" onclick=\"javascript:openRecord(" + parms + "); return false;\">" + content + "</a>";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String recordTooltip(HDT_Record record)
  {
    String typeName = getTypeName(record.getType());

    switch (record.getType())
    {
      case hdtWork :

        HDT_Work work = (HDT_Work) record;

        if (work.workType.isNotNull())
          typeName = work.workType.get().listName();

        StringBuilder tooltip = new StringBuilder("(").append(typeName).append(')');

        if (work.getAuthors().size() == 1)
          tooltip.append(' ').append(work.getAuthors().get(0).singleName());
        else if (work.getAuthors().size() == 2)
          tooltip.append(' ').append(work.getAuthors().get(0).singleName()).append(" & ").append(work.getAuthors().get(1).singleName());
        else if (work.getAuthors().size() > 2)
        {
          for (int ndx = 0; ndx < (work.getAuthors().size() - 1); ndx++)
            tooltip.append(' ').append(work.getAuthors().get(ndx).singleName()).append(',');

          tooltip.append(" & ").append(work.getAuthors().get(work.getAuthors().size() - 1).singleName());
        }

        if (work.getYear().length() > 0)
          tooltip.append(" (").append(work.getYear()).append(')');

        tooltip.append(' ').append(work.name());

        return htmlEscaper.escape(tooltip.toString());

      case hdtMiscFile :

        HDT_MiscFile miscFile = (HDT_MiscFile) record;

        if (miscFile.fileType.isNotNull())
          typeName = typeName + " - " + miscFile.fileType.get().listName();

        if (miscFile.pathNotEmpty())
          return htmlEscaper.escape('(' + typeName + ") " + miscFile.getPath().getNameStr());

        // fall through

      default :

        return htmlEscaper.escape('(' + typeName + ") " + record.getCBText());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void toggleDetailedKeyWorks(Document doc, String className, boolean sortByName, WebEngine weToUse, boolean detailed)
  {
    doc.getElementsByClass(className).forEach(span ->
    {
      int recordID = parseInt(span.attr("hypnconID"), -1),
          recordTypeOrd = parseInt(span.attr("hypnconType"), -1);
      RecordType recordType = getEnumVal(recordTypeOrd, RecordType.class);

      HDT_RecordWithMainText record = (HDT_RecordWithMainText) db.records(recordType).getByID(recordID);

      StringBuilder innerHtml = new StringBuilder();

      if (detailed)
        appendDetailedKeyWorkBody(record.getMainText().getKeyWorksUnmod(), innerHtml, sortByName, span.hasClass(TOPMOST_CLASS));
      else
        appendKeyWorkBody(record.getMainText().getKeyWorksUnmod(), innerHtml, sortByName);

      weToUse.executeScript("document.getElementById(\"" + span.id() + "\").innerHTML = \"" + escapeEcmaScript(innerHtml.toString()) + "\";");
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void appendKeyWorkSpanAndBody(HDT_RecordWithMainText recordWMT, StringBuilder innerHtml, boolean sortByName, MutableInt tagNdx, boolean topmost, TextViewInfo viewInfo)
  {
    innerHtml.append(keyWorkSpanElement(recordWMT, tagNdx, sortByName, topmost));

    List<KeyWork> keyWorks = recordWMT.getMainText().getKeyWorksUnmod();

    if (viewInfo.detailedWorks)
      appendDetailedKeyWorkBody(keyWorks, innerHtml, sortByName, topmost);
    else
      appendKeyWorkBody(keyWorks, innerHtml, sortByName);

    innerHtml.append("</span>");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void appendKeyWorkBody(List<KeyWork> keyWorks, StringBuilder innerHtml, boolean sortByName)
  {
    List<String> searchKeys = new ArrayList<>();

    Map<String, String> linkMap = keyWorkLinkMap(keyWorks, searchKeys, sortByName);

    innerHtml.append(searchKeys.stream().map(searchKey -> new StringBuilder(linkMap.get(searchKey)))
                               .reduce((sb1, sb2) -> sb1.append(", ").append(sb2)).orElse(new StringBuilder()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Returns HTML for record description that is being embedded within another record's description

  static String getSecondaryDisplayHtml(HDT_RecordWithMainText recordWMT, MutableInt tagNdx, TextViewInfo viewInfo)
  {
    MainText mainText = recordWMT.getMainText();
    List<KeyWork> keyWorks = mainText.getKeyWorksUnmod();
    Document doc = jsoupParse(prepHtmlForDisplay(mainText.getHtml()));
    String embeddedHtml = doc.body().html();

    if (collEmpty(keyWorks))
      return embeddedHtml;

    boolean sortByName = db.prefs.getBoolean(PREF_KEY_KEY_WORK_SORT_BY_NAME, true);

    StringBuilder secondaryHtml = new StringBuilder("<div class=\"").append(NUMERIC_SORTED_OUTER_CLASS).append("\" style=\"display: ").append(sortByName ? "none" : "block").append(";\"><b>Key Works:&nbsp;</b>");
    appendKeyWorkSpanAndBody(recordWMT, secondaryHtml, false, tagNdx, false, viewInfo);

    secondaryHtml.append("</div><div class=\"").append(ALPHA_SORTED_OUTER_CLASS).append("\" style=\"display: ").append(sortByName ? "block" : "none").append(";\"><b>Key Works:&nbsp;</b>");
    appendKeyWorkSpanAndBody(recordWMT, secondaryHtml, true, tagNdx, false, viewInfo);

    secondaryHtml.append("</div>");

    if ((ultraTrim(convertToSingleLine(mainText.getPlain())).length() > 0) || mainText.getHtml().contains("&lt;" + EMBEDDED_FILE_TAG + ' '))
      secondaryHtml.append("<br>").append(embeddedHtml);

    return secondaryHtml.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void appendDetailedKeyWorkBody(List<KeyWork> keyWorks, StringBuilder innerHtml, boolean sortByName, boolean topmost)
  {
    List<KeyWork> sortedKeys = new ArrayList<>(keyWorks);
    MutableBoolean firstOne = new MutableBoolean(true);

    if (sortByName)
      sortedKeys.sort(Comparator.comparing(keyWork -> ((HDT_RecordWithAuthors<?>)keyWork.getRecord()).getShortAuthorsStr(true)));
    else
      sortedKeys.sort(null);

    sortedKeys.forEach(key ->
    {
      if (firstOne.isFalse() || !topmost)
        innerHtml.append("<br>");

      firstOne.setFalse();

      innerHtml.append("<a hypncon=\"true\" href=\"\" title=\"Show in Preview Window\" onclick=\"javascript:openPreview(").append(getOpenRecordParms(key.getRecord())).append("); return false;\">")
               .append("<img border=0 width=16 height=16 src=\"").append(imgDataURIbyRecord(key.getRecord())).append("\"></img></a>");

      String authorBibStr;

      switch (key.getRecord().getType())
      {
        case hdtWork :

          HDT_Work work = (HDT_Work) key.getRecord();

          authorBibStr = work.getShortAuthorsStr(true);
          if (authorBibStr.length() > 0)
            innerHtml.append("&nbsp;<span ").append(NO_LINKS_ATTR).append("=true>").append(authorBibStr).append("</span>");

          if (work.getYear().length() > 0)
            innerHtml.append("&nbsp;(").append(work.getYear()).append(')');

          innerHtml.append("&nbsp;").append(getGoToRecordAnchor(work, "", work.name()));

          break;

        case hdtMiscFile :

          HDT_MiscFile miscFile = (HDT_MiscFile) key.getRecord();

          authorBibStr = miscFile.getShortAuthorsStr(true);
          if (authorBibStr.length() > 0)
            innerHtml.append("&nbsp;<span ").append(NO_LINKS_ATTR).append("=true>").append(authorBibStr).append("</span>");

          innerHtml.append("&nbsp;").append(getGoToRecordAnchor(miscFile, "", miscFile.name())).append("&nbsp;")
                   .append("<a hypncon=\"true\" href=\"\" title=\"Jump to this record\" onclick=\"javascript:openRecord(").append(getOpenRecordParms(miscFile)).append("); return false;\">").append("<img border=0 width=16 height=16 src=\"").append(imgDataURI("resources/images/view-form.png")).append("\"></img></a>");

          break;

        default :

          break;
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static int webEngineScrollPos(WebEngine theWE)
  {
    return (Integer) theWE.executeScript("document.body.scrollTop");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void appendImgTagsForLabel(HDT_WorkLabel label, StringBuilder innerHtml, boolean addSpace)
  {
    innerHtml.append("<span style=\"display:inline-block; font-size:12pt; width:16px; height:16px; padding:0px; border:0px; margin:0px;\">")
             .append(getGoToRecordAnchor(label, " style=\"width:16px; height:16px; padding:0px; border:0px; margin:0px; background-repeat: no-repeat; background-image:url('" + imgDataURIbyRecordType(hdtWorkLabel) + "'); text-decoration: none;\"",
                                         "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
             .append("</span>");

    if (addSpace) innerHtml.append("&nbsp;"); // Seems to be an inconsistency in how img tags (and spans with images)
                                              // are handled by the rendering engine; in a <summary> tag, there is no space after images

    nullSwitch(label.getHub(), hub -> hub.getSpokes().forEachOrdered(spoke ->
    {
      if (spoke.getType() == hdtWorkLabel) return;

      innerHtml.append("<span style=\"display:inline-block; font-size:12pt; width:16px; height:16px; padding:0px; border:0px; margin:0px;\">")
               .append(getGoToRecordAnchor(spoke, " style=\"width:16px; height:16px; padding:0px; border:0px; margin:0px; background-repeat: no-repeat;background-image:url('" + imgDataURIbyRecordType(spoke.getType()) + "'); text-decoration: none;\"",
                                           "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
               .append("</span>");

      if (addSpace) innerHtml.append("&nbsp;"); // Seems to be an inconsistency in how img tags (and spans with images)
    }));                                        // are handled by the rendering engine; in a <summary> tag, there is no space after images
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getAnchorForUnitable(HDT_RecordWithMainText uRecord)
  {
    uRecord = uRecord.mainSpoke();

    String recordName = uRecord.getType() == hdtConcept ? ((HDT_Concept) uRecord).extendedName() : uRecord.name();

    return getKeywordLink(recordName, new KeywordLink(0, uRecord.name().length(), new SearchKeyword(uRecord.name(), uRecord)), "text-decoration: none;");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String keyWorkSpanElement(HDT_RecordWithMainText recordWMT, MutableInt tagNdx, boolean sortByName, boolean topmost)
  {
    tagNdx.increment();

    String span = "<span id=SKW" + tagNdx + " hypnconType=" + recordWMT.getType().ordinal() + " hypnconID=" + recordWMT.getID();

    return span + " class=\"" + (sortByName ? ALPHA_SORTED_INNER_CLASS : NUMERIC_SORTED_INNER_CLASS) + (topmost ? ' ' + TOPMOST_CLASS : "") + "\">";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String detailsTag(String divitID, TextViewInfo viewInfo, boolean defOpen)
  {
    boolean open = false;

    String strippedDivitID = divitID;

    if (divitID.startsWith("alp") || divitID.startsWith("num"))
      strippedDivitID = divitID.substring(3);

    if (viewInfo.openDivits == null)
      open = defOpen;
    else if (viewInfo.openDivits.contains(strippedDivitID))
      open = true;

    if (open)
    {
      if (viewInfo.openDivits == null)
        viewInfo.openDivits = new HashSet<>();

      viewInfo.openDivits.add(strippedDivitID);
    }

    return "<details id=\"" + divitID + "\" " + (open ? "open" : "closed") + '>';
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void appendSubLabelsKeyWorkBody(HDT_WorkLabel parentLabel, StringBuilder innerHtml, boolean sortByName, MutableInt tagNdx, TextViewInfo viewInfo, String parentDivitID)
  {
    if ((parentLabel == null) || parentLabel.subLabels.isEmpty()) return;

    List<HDT_WorkLabel> sortedLabels = new ArrayList<>(parentLabel.subLabels);
    sortedLabels.sort(Comparator.comparing(HDT_Record::name));

    sortedLabels.forEach(label ->
    {
      if (label.subLabels.isEmpty() == false)
      {
        String divitID = parentDivitID + makeElementID(label);

        innerHtml.append("<br>").append(detailsTag(divitID, viewInfo, false)).append("<summary>");

        appendImgTagsForLabel(label, innerHtml, true);

        innerHtml.append("<b>").append(getAnchorForUnitable(label)).append(":</b>&nbsp;");

        appendKeyWorkSpanAndBody(label, innerHtml, sortByName, tagNdx, false, viewInfo);

        innerHtml.append("</summary><div style=\"margin-left: 3.5em;\">");

        appendSubLabelsKeyWorkBody(label, innerHtml, sortByName, tagNdx, viewInfo, divitID);

        innerHtml.append("</div></details>");
      }
      else
      {
        innerHtml.append("<br>");

        appendImgTagsForLabel(label, innerHtml, false);

        innerHtml.append("<b>").append(getAnchorForUnitable(label)).append(":</b>&nbsp;");

        appendKeyWorkSpanAndBody(label, innerHtml, sortByName, tagNdx, false, viewInfo);

        innerHtml.append("<br>");
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Map<String, String> keyWorkLinkMap(List<KeyWork> keyWorks, List<String> sortedKeys, boolean sortByName)
  {
    Map<String, String> linkMap = new HashMap<>();
    Map<String, KeyWork> keyToKeyWork = new HashMap<>();

    keyWorks.forEach(keyWork ->
    {
      String searchKey = keyWork.getSearchKey(true).replace(" ", "&nbsp;");

      linkMap.put(searchKey, getKeywordLink(searchKey, new KeywordLink(0, searchKey.length(), new SearchKeyword(searchKey, keyWork.getRecord()))));
      keyToKeyWork.put(searchKey, keyWork);
      sortedKeys.add(searchKey);
    });

    sortedKeys.sort(sortByName ? String::compareToIgnoreCase : Comparator.comparing(keyToKeyWork::get));

    return linkMap;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String convertPlainMainTextToHtml(String input)
  {
    input = trimLines(input).replace("\t", "<span class=\"Apple-tab-span\" style=\"white-space:pre\"> </span>");

    while (input.contains("\n\n"))
      input = input.replace("\n\n", "\n<br>\n");

    return "<html dir=\"ltr\"><head>" +
           styleTag() +
           "</head><body contenteditable=\"true\"><p>" +
           input.replace("\n", "</p><p>") +
           "</p></body></html>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final String MARGIN_STYLE = "margin-right: 20px;";

  static String styleTag()
  {
    return "<style>p { margin-top: 0em; margin-bottom: 0em; } " +
           "body { " + MARGIN_STYLE +
           " font-family: \"" + getDBStylePref(PREF_KEY_DEF_DESC_FONT_FAMILY, "arial") + "\";" +
           " font-size: " + getDBStylePref(PREF_KEY_DEF_DESC_FONT_SIZE, "10pt") + "; } </style>";
  }

  private static String getDBStylePref(String key, String def)
  {
    return db.prefs == null ? def : db.prefs.get(key, def);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_MiscFile nextEmbeddedMiscFile(String str, MutableInt startNdx, MutableInt endNdx, ObjectProperty<Element> elementProp)
  {
    startNdx.setValue(str.indexOf("&lt;" + EMBEDDED_FILE_TAG, startNdx.getValue()));
    elementProp.set(null);

    while (startNdx.getValue() >= 0)
    {
      endNdx.setValue(str.indexOf("&gt;", startNdx.getValue()));

      if (endNdx.getValue() >= 0)
      {
        endNdx.add(4);
        String tag = Parser.unescapeEntities(str.substring(startNdx.getValue(), endNdx.getValue()), true);

        Document doc = jsoupParse(tag);

        elementProp.set(doc.getElementsByTag(EMBEDDED_FILE_TAG).first());

        HDT_MiscFile miscFile = nullSwitch(elementProp.get(), null, element -> db.miscFiles.getByID(parseInt(element.attr("id"), -1)));

        if (miscFile != null) return miscFile;
      }

      startNdx.setValue(str.indexOf("&lt;" + EMBEDDED_FILE_TAG, startNdx.getValue() + 1));
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String extractTextFromHTML(String html)
  {
    return extractTextFromHTML(html, false);
  }

  public static String extractTextFromHTML(String html, boolean forComparison)
  {
    return ultraTrim(jsoupParse(prepHtmlForDisplay(html, forComparison)).text());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String prepHtmlForDisplay(String str)
  {
    return prepHtmlForDisplay(str, false);
  }

  private static String prepHtmlForDisplay(String str, boolean forEditor)
  {
    if (str.contains("</html>") == false)
      return convertPlainMainTextToHtml(str.isEmpty() ? "<br>" : str);

    if (forEditor)
      return str;

    MutableInt startNdx = new MutableInt(0), endNdx = new MutableInt(0);
    ObjectProperty<Element> elementProp = new SimpleObjectProperty<>();

    HDT_MiscFile miscFile = nextEmbeddedMiscFile(str, startNdx, endNdx, elementProp);

    while (miscFile != null)
    {
      String heightAttr = elementProp.get().attr("height"),
             widthAttr  = elementProp.get().attr("width");

      if (heightAttr.isBlank() == false)
        heightAttr = " height=\"" + heightAttr + '"';

      if (widthAttr.isBlank() == false)
        widthAttr = " width=\"" + widthAttr + '"';

      String url = nullSwitch(miscFile.filePath(), "", FilePath::toURLString);

      String tag = "<img src=\"" + url + "\" alt=\"\"" + heightAttr + widthAttr + "/><br>" +
            "<a hypncon=\"true\" href=\"\" title=\"Go to this misc. file record\" onclick=\"javascript:openRecord(" + getOpenRecordParms(miscFile) + "); return false;\">" + miscFile.name() + "</a>";

      str = str.substring(0, startNdx.getValue()) + tag + safeSubstring(str, endNdx.getValue(), str.length());

      startNdx.add(1);
      miscFile = nextEmbeddedMiscFile(str, startNdx, endNdx, elementProp);
    }

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String editingStyles = "a { pointer-events: none; } " + hiliteStyles;

  public static String prepHtmlForEditing(String hyperText)
  {
    hyperText = prepHtmlForDisplay(hyperText, true);

    return hyperText.replace("<style>", "<script>\n" +
        "function insertHtmlAtCursor(html)\n" +
        "{\n" +
        "  document.execCommand('insertHtml', false, html);" +
        "}\n\n" +
        "</script><style>" + editingStyles);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getHtmlFromEditor(String editorHtml)
  {
    Document doc = jsoupParse(editorHtml);

    doc.getElementsByTag("script").forEach(Element::remove);
    doc.getElementsByAttributeValue("id", "key_works").forEach(Element::remove);

    return doc.html().replace(editingStyles, "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final List<Integer> zoomFactors = List.of(25, 33, 50, 67, 75, 80, 90, 100, 110, 125, 150, 175, 200, 250, 300, 400, 500);

  public static void webViewAddZoom(WebView view, String prefID)
  {
    view.addEventFilter(ScrollEvent.SCROLL, event ->
    {
      double deltaY = event.getDeltaY();
      if ((event.isControlDown() == false) || (deltaY == 0)) return;
      event.consume();

      int ndx = app.prefs.getInt(prefID, zoomFactors.indexOf(100)) + (deltaY > 0 ? 1 : -1);

      if ((ndx < 0) || (ndx == zoomFactors.size())) return;

      app.prefs.putInt(prefID, ndx);
      view.setZoom(zoomFactors.get(ndx) / 100.0);
      ui.lblStatus.setText("Zoom: " + zoomFactors.get(ndx) + '%');
    });

    view.setZoom(zoomFactors.get(app.prefs.getInt(prefID, zoomFactors.indexOf(100))) / 100.0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String makeElementID(HDT_Record record)
  {
    return Tag.getTypeTagStr(record.getType()) + record.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Document jsoupParse(String html)
  {
    return Jsoup.parse(html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
