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

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.KeywordLinkList.charIsPartOfWebLink;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import static org.apache.commons.text.StringEscapeUtils.*;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.KeyWork;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Hub;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.records.HDT_WorkLabel;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperView.TextViewInfo;
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

public class MainTextUtil
{
  private static final KeywordLinkList list = new KeywordLinkList();

  public static final String headContent,
                             scriptContent,
                             EMBEDDED_FILE_TAG = "misc-file";

  static final String         ALPHA_SORTED_OUTER_CLASS   = "sortedKeyWorksAZ",
                              NUMERIC_SORTED_OUTER_CLASS = "sortedKeyWorks19";
  private static final String ALPHA_SORTED_INNER_CLASS   = "keyWorksSpanAZ",
                              NUMERIC_SORTED_INNER_CLASS = "keyWorksSpan19",
                              TOPMOST_CLASS              = "topmostKeyWorksSpan";

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
      .append("  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(" + String.valueOf(JS_EVENT_OPEN_FILE) + ");\n")
      .append("}\n\n")
      .append("function openRecord(recordType, recordID)\n{\n")
      .append("  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(" + String.valueOf(JS_EVENT_OPEN_RECORD) + ");\n")
      .append("}\n\n")
      .append("function openPreview(recordType, recordID)\n{\n")
      .append("  jsToJava.recordID = recordID; jsToJava.recordType = recordType; callToJava(" + String.valueOf(JS_EVENT_OPEN_PREVIEW) + ");\n")
      .append("}\n\n")
      .append("function openURL(url)\n{\n")
      .append("  jsToJava.url = url; callToJava(" + String.valueOf(JS_EVENT_OPEN_URL) + ");\n")
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
      .append("  elements = document.getElementsByClassName('" + NUMERIC_SORTED_OUTER_CLASS + "');\n")
      .append("  for(i=0; i<elements.length; i++) { elements[i].style.display = 'none'; }\n")
      .append("  elements = document.getElementsByClassName('" + ALPHA_SORTED_OUTER_CLASS + "');\n")
      .append("  for(i=0; i<elements.length; i++) { elements[i].style.display = (elements[i].tagName === 'SPAN' ? 'inline' : 'block'); }\n")
      .append("  jsToJava.sortByName = true; callToJava(" + String.valueOf(JS_EVENT_SET_SORT_KEY_METHOD) + ");\n")
      .append("}\n\n")
      .append("function switchTo19()\n{\n")
      .append("  var i,elements = document.getElementsByTagName('details');\n")
      .append("  for (i=0; i<elements.length; i++)\n  {\n")
      .append("    if (elements[i].id.slice(0,3) === \"alp\")\n    {\n")
      .append("      document.getElementById(\"num\" + elements[i].id.slice(3)).open = elements[i].open;\n    }\n  }\n")
      .append("  elements = document.getElementsByClassName('" + ALPHA_SORTED_OUTER_CLASS + "');\n")
      .append("  for(i=0; i<elements.length; i++) { elements[i].style.display = 'none'; }\n")
      .append("  elements = document.getElementsByClassName('" + NUMERIC_SORTED_OUTER_CLASS + "');\n")
      .append("  for(i=0; i<elements.length; i++) { elements[i].style.display = (elements[i].tagName === 'SPAN' ? 'inline' : 'block'); }\n")
      .append("  jsToJava.sortByName = false; callToJava(" + String.valueOf(JS_EVENT_SET_SORT_KEY_METHOD) + ");\n")
      .append("}\n\n")
      .append("</script>\n\n")
      .toString();

    headContent = new StringBuilder(scriptContent)

      .append("<style type=\"text/css\">\n")
      .append("  .highlight { background-color: pink; }\n")
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
    int recordID = -1, recordTypeOrd;
    RecordType recordType = hdtNone;
    JSObject jsToJava = null;

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
      recordTypeOrd = (Integer)jsToJava.getMember("recordType");
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
          previewWindow.setPreview(pvsOther, work.filePath(), work.getStartPageNum(), work.getEndPageNum(), work);
        }
        else
        {
          HDT_MiscFile miscFile = db.miscFiles.getByID(recordID);
          previewWindow.setPreview(pvsOther, miscFile.filePath(), miscFile);
        }

        ui.openPreviewWindow(pvsOther);
        break;

      case JS_EVENT_OPEN_URL :

        openWebLink(String.class.cast(jsToJava.getMember("url")));
        break;

      case JS_EVENT_LAUNCH_FILE :

        if (recordType == hdtMiscFile)
        {
          HDT_MiscFile file = db.miscFiles.getByID(recordID);

          if (file.getPath().isEmpty()) return;

          launchFile(file.filePath());
        }
        else if (recordType == hdtNote)
          launchFile(db.notes.getByID(recordID).getPath().filePath());
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
    return makeDocLinksExternal(Jsoup.parse(s)).html();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static enum LinkKind { none, web, keyword }

  static void addLinks(Element element, HDT_Record recordToHilite)
  {
    if (element.tagName().equalsIgnoreCase("summary") || // Don't create any keyword links within collapsible headings
        element.tagName().equalsIgnoreCase("a")       || // Don't create any keyword links within anchor tags (they already link to somewhere)
        element.hasAttr("hypncon-no-links"))
      return;

    for (int nodeNdx = 0; nodeNdx < element.textNodes().size(); nodeNdx++)
    {
      TextNode textNode = element.textNodes().get(nodeNdx);

      String oldStr = textNode.getWholeText();
      list.generate(oldStr);

      int linkNdx = 0,        // index of the current keyword link
          curNodeTextNdx = 0; // character position, in the original text of the current element, where the current TextNode starts

      KeywordLink link = linkNdx >= list.getLinks().size() ? null : list.getLinks().get(linkNdx);

      for (int oldNdx = 0; oldNdx < oldStr.length();)
      {
        LinkKind kind = LinkKind.none;

        if (safeSubstring(oldStr, oldNdx, oldNdx + 4).toLowerCase().equals("http"))
          kind = LinkKind.web;
        else if ((link != null) && (oldNdx == link.offset))
          kind = LinkKind.keyword;

        if (kind != LinkKind.none)
        {
          textNode = textNode.splitText(oldNdx - curNodeTextNdx);  // 1. Set textNode reference equal to the text node that will be after the link
          curNodeTextNdx = oldNdx;
          nodeNdx++;
          String displayText, newStr = textNode.getWholeText();    // 2. Get the text of the link plus the next text node
          int linkTextLen;

          if (kind == LinkKind.web)
          {
            linkTextLen = getWebLinkLen(newStr);    // 3. Get end offset into newStr for the part that will be converted to a link
            displayText = safeSubstring(newStr, 0, linkTextLen);
            textNode.before("<a href=\"\" onclick=\"openURL('" + displayText + "'); return false;\">" + displayText + "</a>"); // 4. Insert anchor
          }
          else
          {
            linkTextLen = link.length;              // 3. Get end offset into newStr for the part that will be converted to a link
            displayText = safeSubstring(newStr, 0, linkTextLen);
            String style = link.key.record.equals(recordToHilite) ? "background-color: pink;" : "";

            textNode.before(getKeywordLink(displayText, link, style));  // 4. Insert anchor
          }

          oldNdx = oldNdx + linkTextLen;                                        // 5. Update oldNdx
          textNode.text(safeSubstring(newStr, linkTextLen, newStr.length()));   // 6. Remove link text from next text node
          curNodeTextNdx = curNodeTextNdx + linkTextLen;                        // 7. Update index of current text node

          if (kind == LinkKind.keyword)
          {
            linkNdx++;
            link = linkNdx >= list.getLinks().size() ? null : list.getLinks().get(linkNdx);
          }
        }

        else // there was no link this time
        {
          oldNdx++;
        }
      }
    }

    element.children().forEach(child -> addLinks(child, recordToHilite));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int getWebLinkLen(String text)
  {
    int ndx = 0;

    while ((ndx < text.length()) && charIsPartOfWebLink(text, ndx)) ndx++;

    return ndx;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String getKeywordLink(String text, KeywordLink link, String style)
  {
    HDT_Record record = link.key.record;

    if (record == null) return text;

    if (record.getType() == hdtHub)
    {
      record = ui.getSpokeToGoTo(HDT_Hub.class.cast(record).getLink());

      if (record == null)
      {
        messageDialog("Internal error #28587", mtError);
        return text;
      }
    }

    if (style.length() > 0) style = " style=\"" + style + "\"";

    if (record.getType() == hdtMiscFile)
      return getGoToRecordAnchor(record, style, text) + "&nbsp;" +
        "<a hypncon=\"true\" href=\"\" title=\"Jump to this record\" onclick=\"javascript:openRecord(" + getOpenRecordParms(record) + "); return false;\">" + "<img border=0 width=16 height=16 src=\"" + imgDataURI("resources/images/view-form.png") + "\"></img></a>";

    return getGoToRecordAnchor(record, style, text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getOpenRecordParms(HDT_Record record)
  {
    return String.valueOf(record.getType().ordinal()) + "," + record.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String getGoToRecordAnchor(HDT_Record record, String style, String content)
  {
    String parms = getOpenRecordParms(record),
           anchor = "<a title=\"" + recordTooltip(record) + "\"" + style + " hypncon=\"true\" ";

    switch (record.getType())
    {
      case hdtMiscFile :
        return anchor + "href=\"\" onclick=\"javascript:openFile(" + parms + "); return false;\">" + content + "</a>";

      case hdtWork :
        return anchor + "oncontextmenu=\"openFile(" + parms + "); return false;\" href=\"\" onclick=\"javascript:openRecord(" + parms + "); return false;\">" + content + "</a>";

      case hdtNote :
        return anchor + "oncontextmenu=\"openFile(" + parms + "); return false;\" href=\"\" onclick=\"javascript:openRecord(" + parms + "); return false;\">" + content + "</a>";

      default :
        return anchor + "href=\"\" onclick=\"javascript:openRecord(" + parms + "); return false;\">" + content + "</a>";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String recordTooltip(HDT_Record record)
  {
    String typeName = db.getTypeName(record.getType());

    if (record.getType() == hdtWork)
    {
      HDT_Work work = (HDT_Work) record;

      if (work.workType.isNotNull())
        typeName = work.workType.get().listName();

      String tooltip = "(" + typeName + ")";

      if (work.getAuthors().size() == 1)
        tooltip = tooltip + " " + work.getAuthors().get(0).singleName();
      else if (work.getAuthors().size() == 2)
        tooltip = tooltip + " " + work.getAuthors().get(0).singleName() + " & " + work.getAuthors().get(1).singleName();
      else if (work.getAuthors().size() > 2)
      {
        for (int ndx = 0; ndx < (work.getAuthors().size() - 1); ndx++)
          tooltip = tooltip + " " + work.getAuthors().get(ndx).singleName() + ",";

        tooltip = tooltip + " & " + work.getAuthors().get(work.getAuthors().size() - 1).singleName();
      }

      if (work.getYear().length() > 0)
        tooltip = tooltip + " (" + work.getYear() + ")";

      tooltip = tooltip + " " + work.name();

      return htmlEscaper.escape(tooltip);
    }

    if (record.getType() == hdtMiscFile)
    {
      HDT_MiscFile miscFile = (HDT_MiscFile) record;

      if (miscFile.fileType.isNotNull())
        typeName = typeName + " - " + miscFile.fileType.get().listName();

      if (miscFile.pathNotEmpty())
        return htmlEscaper.escape("(" + typeName + ") " + miscFile.getPath().getNameStr());
    }

    return htmlEscaper.escape("(" + typeName + ") " + record.getCBText());
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

      HDT_RecordWithConnector record = (HDT_RecordWithConnector) db.records(recordType).getByID(recordID);

      StringBuilder innerHtml = new StringBuilder();

      if (detailed)
        appendDetailedKeyWorkBody(record.getMainText().getKeyWorks(), innerHtml, sortByName, span.hasClass(TOPMOST_CLASS));
      else
        appendKeyWorkBody(record.getMainText().getKeyWorks(), innerHtml, sortByName);

      weToUse.executeScript("document.getElementById(\"" + span.id() + "\").innerHTML = \"" + escapeEcmaScript(innerHtml.toString()) + "\";");
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void appendKeyWorkSpanAndBody(HDT_RecordWithConnector recordWMT, StringBuilder innerHtml, boolean sortByName, MutableInt tagNdx, boolean topmost, TextViewInfo viewInfo)
  {
    innerHtml.append(keyWorkSpanElement(recordWMT, tagNdx, sortByName, topmost));

    List<KeyWork> keyWorks = recordWMT.getMainText().getKeyWorks();

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
  
  static String getSecondaryDisplayHtml(HDT_RecordWithConnector recordWMT, MutableInt tagNdx, TextViewInfo viewInfo)
  {
    MainText mainText = recordWMT.getMainText();
    List<KeyWork> keyWorks = mainText.getKeyWorks();
    Document doc = Jsoup.parse(prepHtmlForDisplay(mainText.getHtml()));
    String embeddedHtml = doc.body().html();
    
    if (collEmpty(keyWorks))
      return embeddedHtml;
    
    boolean sortByName = db.prefs.getBoolean(PREF_KEY_KEY_WORK_SORT_BY_NAME, true);

    StringBuilder secondaryHtml = new StringBuilder("<div class=\"" + NUMERIC_SORTED_OUTER_CLASS + "\" style=\"display: " + (sortByName ? "none" : "block") + ";\"><b>Key Works:&nbsp;</b>");
    appendKeyWorkSpanAndBody(recordWMT, secondaryHtml, false, tagNdx, false, viewInfo);

    secondaryHtml.append("</div><div class=\"" + ALPHA_SORTED_OUTER_CLASS + "\" style=\"display: " + (sortByName ? "block" : "none") + ";\"><b>Key Works:&nbsp;</b>");
    appendKeyWorkSpanAndBody(recordWMT, secondaryHtml, true, tagNdx, false, viewInfo);

    secondaryHtml.append("</div>");
    
    if ((ultraTrim(convertToSingleLine(mainText.getPlain())).length() > 0) || mainText.getHtml().contains("&lt;" + EMBEDDED_FILE_TAG + " "))
      secondaryHtml.append("<br>" + embeddedHtml);

    return secondaryHtml.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void appendDetailedKeyWorkBody(List<KeyWork> keyWorks, StringBuilder innerHtml, boolean sortByName, boolean topmost)
  {
    List<KeyWork> sortedKeys = new ArrayList<>(keyWorks);
    MutableBoolean firstOne = new MutableBoolean(true);

    if (sortByName)
      sortedKeys.sort(sortBasis(keyWork ->
      {
        return keyWork.getRecordType() == hdtWork ?
          HDT_Work.class.cast(keyWork.getRecord()).getShortAuthorsStr(true)
        :
          HDT_MiscFile.class.cast(keyWork.getRecord()).getShortAuthorsStr(true);
      }));
    else
      sortedKeys.sort(null);

    sortedKeys.forEach(key ->
    {
      if (firstOne.isFalse() || !topmost)
        innerHtml.append("<br>");
      firstOne.setFalse();

      innerHtml.append("<a hypncon=\"true\" href=\"\" title=\"Show in Preview Window\" onclick=\"javascript:openPreview(" + getOpenRecordParms(key.getRecord()) + "); return false;\">")
               .append("<img border=0 width=16 height=16 src=\"" + imgDataURIbyRecord(key.getRecord()) + "\"></img></a>");
      String authorBibStr;

      switch (key.getRecord().getType())
      {
        case hdtWork :

          HDT_Work work = (HDT_Work) key.getRecord();

          authorBibStr = work.getShortAuthorsStr(true);
          if (authorBibStr.length() > 0)
            innerHtml.append("&nbsp;<span hypncon-no-links=true>" + authorBibStr + "</span>");

          if (work.getYear().length() > 0)
            innerHtml.append("&nbsp;(" + work.getYear() + ")");

          innerHtml.append("&nbsp;" + getGoToRecordAnchor(work, "", work.name()));

          break;

        case hdtMiscFile :

          HDT_MiscFile miscFile = (HDT_MiscFile) key.getRecord();

          authorBibStr = miscFile.getShortAuthorsStr(true);
          if (authorBibStr.length() > 0)
            innerHtml.append("&nbsp;<span hypncon-no-links=true>" + authorBibStr + "</span>");

          innerHtml.append("&nbsp;" + getGoToRecordAnchor(miscFile, "", miscFile.name()) + "&nbsp;")
                   .append("<a hypncon=\"true\" href=\"\" title=\"Jump to this record\" onclick=\"javascript:openRecord(" + getOpenRecordParms(miscFile) + "); return false;\">" + "<img border=0 width=16 height=16 src=\"" + imgDataURI("resources/images/view-form.png") + "\"></img></a>");

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
    return Integer.class.cast(theWE.executeScript("document.body.scrollTop")).intValue();
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

    nullSwitch(label.getLink(), link -> link.getSpokes().forEach(spoke ->
    {
      if (spoke.getType() == hdtWorkLabel) return;

      innerHtml.append("<span style=\"display:inline-block; font-size:12pt; width:16px; height:16px; padding:0px; border:0px; margin:0px;\">")
               .append(getGoToRecordAnchor(spoke.getSpoke(), " style=\"width:16px; height:16px; padding:0px; border:0px; margin:0px; background-repeat: no-repeat;background-image:url('" + imgDataURIbyRecordType(spoke.getType()) + "'); text-decoration: none;\"",
                                           "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))
               .append("</span>");

      if (addSpace) innerHtml.append("&nbsp;"); // Seems to be an inconsistency in how img tags (and spans with images)
    }));                                        // are handled by the rendering engine; in a <summary> tag, there is no space after images
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getAnchorForUnitable(HDT_RecordWithConnector uRecord)
  {
    StrongLink link = uRecord.getLink();
    String recordName;

    if (link != null) uRecord = ui.getSpokeToGoTo(link);

    if (uRecord.getType() == hdtConcept)
    {
      HDT_Concept concept = (HDT_Concept) uRecord;
      recordName = concept.getExtendedName();
    }
    else
      recordName = uRecord.name();

    return getKeywordLink(recordName, new KeywordLink(0, uRecord.name().length(), new SearchKeyword(uRecord.name(), uRecord)), "text-decoration: none;");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String keyWorkSpanElement(HDT_RecordWithConnector recordWMT, MutableInt tagNdx, boolean sortByName, boolean topmost)
  {
    tagNdx.increment();

    String span = "<span id=SKW" + tagNdx.toString() + " hypnconType=" + String.valueOf(recordWMT.getType().ordinal()) + " hypnconID=" + String.valueOf(recordWMT.getID());

    return span + " class=\"" + (sortByName ? ALPHA_SORTED_INNER_CLASS : NUMERIC_SORTED_INNER_CLASS) + (topmost ? " " + TOPMOST_CLASS : "") + "\">";
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

    return "<details id=\"" + divitID + "\" " + (open ? "open" : "closed") + ">";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void appendSubLabelsKeyWorkBody(HDT_WorkLabel parentLabel, StringBuilder innerHtml, boolean sortByName, MutableInt tagNdx, TextViewInfo viewInfo, String parentDivitID)
  {
    if ((parentLabel == null) || parentLabel.subLabels.isEmpty()) return;

    List<HDT_WorkLabel> sortedLabels = new ArrayList<>(parentLabel.subLabels);
    sortedLabels.sort(sortBasis(HDT_Record::name));

    sortedLabels.forEach(label ->
    {
      if (label.subLabels.isEmpty() == false)
      {
        String divitID = parentDivitID + makeElementID(label);

        innerHtml.append("<br>" + detailsTag(divitID, viewInfo, false) + "<summary>");

        appendImgTagsForLabel(label, innerHtml, true);

        innerHtml.append("<b>" + getAnchorForUnitable(label) + ":</b>&nbsp;");

        appendKeyWorkSpanAndBody(label, innerHtml, sortByName, tagNdx, false, viewInfo);

        innerHtml.append("</summary><div style=\"margin-left: 3.5em;\">");

        appendSubLabelsKeyWorkBody(label, innerHtml, sortByName, tagNdx, viewInfo, divitID);

        innerHtml.append("</div></details>");
      }
      else
      {
        innerHtml.append("<br>");

        appendImgTagsForLabel(label, innerHtml, false);

        innerHtml.append("<b>" + getAnchorForUnitable(label) + ":</b>&nbsp;");

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

      linkMap.put(searchKey, getKeywordLink(searchKey, new KeywordLink(0, searchKey.length(), new SearchKeyword(searchKey, keyWork.getRecord())), ""));
      keyToKeyWork.put(searchKey, keyWork);
      sortedKeys.add(searchKey);
    });

    sortedKeys.sort(sortByName ? String::compareToIgnoreCase : sortBasis(keyToKeyWork::get));

    return linkMap;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String convertPlainMainTextToHtml(String input)
  {
    String output = "<html dir=\"ltr\"><head>" + mainTextHeadStyleTag() + "</head><body contenteditable=\"true\"><p><font face=\"Arial\" size=\"2\">";

    input = trimLines(input);

    input = input.replace("\t", "<span class=\"Apple-tab-span\" style=\"white-space:pre\"> </span>");

    while (input.contains("\n\n"))
      input = input.replace("\n\n", "\n<br>\n");

    input = input.replace("\n", "</font></p><p><font face=\"Arial\" size=\"2\">");

    output = output + input + "</font></p></body></html>";

    return output;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String MARGIN_STYLE = "margin-right: 20px;";
  
  public static String mainTextHeadStyleTag()
  {
    return "<style>p { margin-top: 0em; margin-bottom: 0em; } " +
           "body { " + MARGIN_STYLE + " font-family: arial; font-size: 10pt; } </style>";
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

        Document doc = Jsoup.parse(tag);

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
    return ultraTrim(Jsoup.parse(prepHtmlForDisplay(html, forComparison)).text());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String prepHtmlForDisplay(String str)
  {
    return prepHtmlForDisplay(str, false);
  }

  static String prepHtmlForDisplay(String str, boolean forEditor)
  {
    if (str.indexOf("</html>") > -1)
    {
      if (forEditor == false)
      {
        MutableInt startNdx = new MutableInt(0), endNdx = new MutableInt(0);
        ObjectProperty<Element> elementProp = new SimpleObjectProperty<>();

        HDT_MiscFile miscFile = nextEmbeddedMiscFile(str, startNdx, endNdx, elementProp);

        while (miscFile != null)
        {
          String heightAttr = elementProp.get().attr("height"),
                 widthAttr  = elementProp.get().attr("width");

          if (heightAttr.isBlank() == false)
            heightAttr = " height=\"" + heightAttr + "\"";

          if (widthAttr.isBlank() == false)
            widthAttr = " width=\"" + widthAttr + "\"";

          String url = nullSwitch(miscFile.filePath(), "", FilePath::toURLString);

          String tag = "<img src=\"" + url + "\" alt=\"\"" + heightAttr + widthAttr + "/><br>" +
                "<a hypncon=\"true\" href=\"\" title=\"Go to this misc. file record\" onclick=\"javascript:openRecord(" + getOpenRecordParms(miscFile) + "); return false;\">" + miscFile.name() + "</a>";

          str = str.substring(0, startNdx.getValue()) + tag + safeSubstring(str, endNdx.getValue(), str.length());

          startNdx.add(1);
          miscFile = nextEmbeddedMiscFile(str, startNdx, endNdx, elementProp);
        }
      }

      return str;
    }

    if (str.isEmpty()) str = "<br>";

    return convertPlainMainTextToHtml(str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String disableLinks(String hyperText)
  {
    hyperText = prepHtmlForDisplay(hyperText, true);

    return hyperText.replace("<style>", "<script>\n" +
        "function insertHtmlAtCursor(html)\n" +
        "{\n" +
        "  document.execCommand('insertHtml', false, html);" +
        "}\n\n" +
        "</script><style>a { pointer-events: none; }");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getHtmlFromEditor(String editorHtml)
  {
    Document doc = Jsoup.parse(editorHtml);

    doc.getElementsByTag("script").forEach(Element::remove);
    doc.getElementsByAttributeValue("id", "key_works").forEach(Element::remove);

    editorHtml = doc.html();

    return editorHtml.replace("a { pointer-events: none; }", "");
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

      int ndx = appPrefs.getInt(prefID, zoomFactors.indexOf(100)) + (deltaY > 0 ? 1 : -1);

      if ((ndx < 0) || (ndx == zoomFactors.size())) return;

      appPrefs.putInt(prefID, ndx);
      view.setZoom(zoomFactors.get(ndx) / 100.0);
      ui.lblStatus.setText("Zoom: " + String.valueOf(zoomFactors.get(ndx)) + "%");
    });

    view.setZoom(zoomFactors.get(appPrefs.getInt(prefID, zoomFactors.indexOf(100))) / 100.0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String makeElementID(HDT_Record record)
  {
    return db.getTypeTagStr(record.getType()) + String.valueOf(record.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
