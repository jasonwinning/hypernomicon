/*
 * Copyright 2015-2018 Jason Winning
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.apache.commons.text.StringEscapeUtils.*;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Attribute;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.nodes.TextNode;

import org.hypernomicon.App;
import org.hypernomicon.model.KeywordLinkList;
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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.KeywordLinkList.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource.*;

import javafx.beans.value.ChangeListener;
import javafx.concurrent.Worker;
import javafx.event.Event;
import javafx.fxml.FXMLLoader;
import javafx.scene.input.MouseButton;
import javafx.scene.input.ScrollEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import netscape.javascript.JSException;
import netscape.javascript.JSObject;

//---------------------------------------------------------------------------

public class MainTextWrapper
{
  private AnchorPane parentPane;
  private static BorderPane bpEditorRoot;
  private static MainTextController editCtrlr;
  private static WebView view;
  private static WebEngine we;
  private static KeywordLinkList list = new KeywordLinkList();
  private static StringBuilder jQueryContents = new StringBuilder();
  private static StringBuilder jHiliteContents = new StringBuilder();
  private HDT_RecordWithConnector curRecord;
  private String html, completeHtml;
  private List<DisplayItem> displayItems;
  private List<KeyWork> keyWorks;
  private TextViewInfo viewInfo;
  private static MainTextWrapper curWrapper;
  private static String textToHilite = "";
  private static String lastTextToHilite = "";
  private static String headContent, scriptContent;
  
  private boolean editing, edited, showing;
  
  private static final String ALPHA_SORTED_OUTER_CLASS = "sortedKeyWorksAZ";
  private static final String NUMERIC_SORTED_OUTER_CLASS = "sortedKeyWorks19";
  private static final String ALPHA_SORTED_INNER_CLASS = "keyWorksSpanAZ";
  private static final String NUMERIC_SORTED_INNER_CLASS = "keyWorksSpan19";
  private static final String TOPMOST_CLASS = "topmostKeyWorksSpan";
  
  public static String getScriptContent() { return scriptContent; }
  public static String getHeadContent()   { return headContent; }
  
//---------------------------------------------------------------------------
  
  public static final void rescale()
  {
    scaleNodeForDPI(bpEditorRoot);
    setFontSize(bpEditorRoot);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final void init()
  {
    view = new WebView();
    webViewAddZoom(view, PREF_KEY_MAINTEXT_ZOOM);
        
    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/mainText/MainTextEditor.fxml"));
    
    try { bpEditorRoot = loader.load(); } catch (IOException e) { noOp(); }
    editCtrlr = loader.getController();
    editCtrlr.init();
   
    AnchorPane.setTopAnchor(bpEditorRoot, 0.0);
    AnchorPane.setBottomAnchor(bpEditorRoot, 0.0);
    AnchorPane.setLeftAnchor(bpEditorRoot, 0.0);
    AnchorPane.setRightAnchor(bpEditorRoot, 0.0);

    AnchorPane.setTopAnchor(view, 0.0);
    AnchorPane.setBottomAnchor(view, 0.0);
    AnchorPane.setLeftAnchor(view, 0.0);
    AnchorPane.setRightAnchor(view, 0.0);

    view.setOnContextMenuRequested(event -> setHTMLContextMenu());
    
    view.getEngine().getLoadWorker().stateProperty().addListener((ChangeListener<Worker.State>) (ov, oldState, newState) -> 
    {
      if (newState == Worker.State.SUCCEEDED) 
      {        
        if (textToHilite.length() > 0)
          lastTextToHilite = new String(textToHilite); 
        
        if (lastTextToHilite.length() > 0)
          MainTextWrapper.hiliteText(textToHilite, we);
        
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
    
    we.titleProperty().addListener((title, oldTitle, newTitle) -> handleJSEvent(curWrapper.completeHtml, we, curWrapper.viewInfo));
    
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

  public static final int JS_EVENT_OPEN_RECORD = 1,
                          JS_EVENT_OPEN_PREVIEW = 2,
                          JS_EVENT_OPEN_URL = 3,
                          JS_EVENT_LAUNCH_FILE = 4,
                          JS_EVENT_OPEN_FILE = 5,             // May launch or open preview depending on user setting
                          JS_EVENT_SET_SORT_KEY_METHOD = 6,
                          JS_EVENT_DETAILED_KEY_WORKS = 7;
   
  public static double lastEventID = -1;
  
  public static void handleJSEvent(String htmlToUse, WebEngine weToUse, TextViewInfo viewInfo)
  {
    int recordID = -1, recordTypeOrd;
    HDT_RecordType recordType = hdtNone;
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
    {
      if (ui.btnPointerLaunch.isSelected())
        jsEvent = JS_EVENT_LAUNCH_FILE;
      else
        jsEvent = JS_EVENT_OPEN_PREVIEW;
    }
    
    if ((jsEvent == JS_EVENT_OPEN_RECORD) || (jsEvent == JS_EVENT_LAUNCH_FILE) || (jsEvent == JS_EVENT_OPEN_PREVIEW))
    {
      recordID = (Integer)jsToJava.getMember("recordID");
      recordTypeOrd = (Integer)jsToJava.getMember("recordType");
      recordType = getEnumVal(recordTypeOrd, HDT_RecordType.class);     
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
          previewWindow.setPreview(pvsOther, work.getPath().getFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
        }
        else
        {
          HDT_MiscFile miscFile = db.miscFiles.getByID(recordID);
          previewWindow.setPreview(pvsOther, miscFile.getPath().getFilePath(), -1, -1, miscFile);
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

          launchFile(file.getPath().getFilePath());          
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

  private static void toggleDetailedKeyWorks(Document doc, String className, boolean sortByName, WebEngine weToUse, boolean detailed)
  {
    doc.getElementsByClass(className).forEach(span ->
    {          
      int recordID = parseInt(span.attr("hypnconID"), -1);
      int recordTypeOrd = parseInt(span.attr("hypnconType"), -1);
      HDT_RecordType recordType = getEnumVal(recordTypeOrd, HDT_RecordType.class);
                
      HDT_RecordWithConnector record = (HDT_RecordWithConnector) db.records(recordType).getByID(recordID);
      
      StringBuilder innerHtml = new StringBuilder();            
      
      if (detailed)
        appendDetailedKeyWorkBody(record.getMainText().getKeyWorks(), innerHtml, sortByName, span.hasClass(TOPMOST_CLASS));
      else
        appendKeyWorkBody(record.getMainText().getKeyWorks(), innerHtml, sortByName);
      
      String script = "document.getElementById(\"" + span.id() + "\").innerHTML = \"" + escapeEcmaScript(innerHtml.toString()) + "\";";
      weToUse.executeScript(script);
    });
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void appendDetailedKeyWorkBody(List<KeyWork> keyWorks, StringBuilder innerHtml, boolean sortByName, boolean topmost)
  {
    ArrayList<KeyWork> sortedKeys = new ArrayList<>();
    MutableBoolean firstOne = new MutableBoolean(true);
    
    sortedKeys.addAll(keyWorks);

    if (sortByName)
      sortedKeys.sort((s1, s2) -> 
      {
        String bibAuthors1, bibAuthors2;
        
        if (s1.getRecordType() == hdtWork)
          bibAuthors1 = HDT_Work.class.cast(s1.getRecord()).getShortAuthorsStr(true);
        else
          bibAuthors1 = HDT_MiscFile.class.cast(s1.getRecord()).getShortAuthorsStr(true);

        if (s2.getRecordType() == hdtWork)
          bibAuthors2 = HDT_Work.class.cast(s2.getRecord()).getShortAuthorsStr(true);
        else
          bibAuthors2 = HDT_MiscFile.class.cast(s2.getRecord()).getShortAuthorsStr(true);
        
        return bibAuthors1.compareTo(bibAuthors2); 
      });
    else
      sortedKeys.sort((s1, s2) -> s1.compareTo(s2));
    
    sortedKeys.forEach(key ->
    {
      if (firstOne.isFalse() || !topmost) 
        innerHtml.append("<br>");
      firstOne.setFalse();
            
      innerHtml.append("<a hypncon=\"true\" href=\"\" title=\"Show in Preview Window\" onclick=\"javascript:openPreview(" + getOpenRecordParms(key.getRecord()) + "); return false;\">");
      innerHtml.append("<img border=0 width=16 height=16 src=\"" + getImageDataURI(ui.getGraphicRelativePath(key.getRecord())) + "\"></img></a>");
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
          
          innerHtml.append("&nbsp;" + getGoToRecordAnchor(miscFile, "", miscFile.name()) + "&nbsp;");
          innerHtml.append("<a hypncon=\"true\" href=\"\" title=\"Jump to this record\" onclick=\"javascript:openRecord(" + getOpenRecordParms(miscFile) + "); return false;\">" + "<img border=0 width=16 height=16 src=\"" + getImageDataURI("resources/images/view-form.png") + "\"></img></a>");
          
          break;
          
        default :
          
          break;
      }
    });
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MainTextWrapper(AnchorPane parentPane)
  {
    this.parentPane = parentPane;
  
    editing = false;
    edited = false;
    showing = false;
  }
    
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void clear(boolean show)
  {
    removeFromAnchor(bpEditorRoot);
    removeFromAnchor(view);
    
    html = ""; completeHtml = "";
    curRecord = null;
    editing = false;
    edited = false;
    displayItems = null;
    keyWorks = null;
    
    if (show) showReadOnly();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void hide()
  {
    if (editing)
    {
      keyWorks = new ArrayList<KeyWork>();     
      html = editCtrlr.getHtmlAndKeyWorks(keyWorks);
      
      displayItems = new ArrayList<DisplayItem>();
      displayItems.addAll(editCtrlr.getDisplayItems());
      
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
          
    removeFromAnchor(bpEditorRoot);
    removeFromAnchor(view);
    
    parentPane.getChildren().setAll(view);      
  
    if (curRecord == null) 
    {
      we.loadContent("");
      return;
    }
    
    MainTextWrapper mainTextWrapper = this;
    
    view.setOnMouseClicked(mouseEvent -> 
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY))
        if (mouseEvent.getClickCount() == 2)
          mainTextWrapper.beginEditing(true);
    });
    
    boolean noDisplayRecords = true;
    
    if (displayItems != null)
    {
      for (DisplayItem item : displayItems)
        if (item.type == DisplayItemType.diRecord)
          noDisplayRecords = false;
    }
    
    int keyWorksSize = nullSwitch(keyWorks, 0, () -> getNestedKeyWorkCount(curRecord, keyWorks));
    
    if ((Jsoup.parse(html).text().trim().length() == 0) && (keyWorksSize == 0) && noDisplayRecords)        
      beginEditing(false);
    else
      setReadOnlyHTML(completeHtml, we, viewInfo, null);
  
    showing = true;
    curWrapper = this;
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

  public static enum LinkKind { none, web, keyword }
  
  private static void addLinks(Element element, HDT_Base recordToHilite)
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
            String style;
            
            if (link.key.record.equals(recordToHilite))
              style = "background-color: pink;";
            else
              style = "";
            
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

  private static String getKeywordLink(String text, KeywordLink link, String style)
  {
    HDT_Base record = link.key.record;
    
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
        "<a hypncon=\"true\" href=\"\" title=\"Jump to this record\" onclick=\"javascript:openRecord(" + getOpenRecordParms(record) + "); return false;\">" + "<img border=0 width=16 height=16 src=\"" + getImageDataURI("resources/images/view-form.png") + "\"></img></a>"; 
    
    return getGoToRecordAnchor(record, style, text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getOpenRecordParms(HDT_Base record)
  {
    return String.valueOf(record.getType().ordinal()) + "," + record.getID();
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getGoToRecordAnchor(HDT_Base record, String style, String content)
  {
    String parms = getOpenRecordParms(record);
    
    if (record.getType() == hdtMiscFile)
      return "<a title=\"" + recordTooltip(record) + "\"" + style + " hypncon=\"true\" href=\"\" onclick=\"javascript:openFile(" + parms + "); return false;\">" + content + "</a>";
    
    if (record.getType() == hdtWork)
      return "<a title=\"" + recordTooltip(record) + "\"" + style + " hypncon=\"true\" oncontextmenu=\"openFile(" + parms + "); return false;\" href=\"\" onclick=\"javascript:openRecord(" + parms + "); return false;\">" + content + "</a>";
    
    return "<a title=\"" + recordTooltip(record) + "\"" + style + " hypncon=\"true\" href=\"\" onclick=\"javascript:openRecord(" + parms + "); return false;\">" + content + "</a>";
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String recordTooltip(HDT_Base record)
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
      
      if (miscFile.getPath().isEmpty() == false)
        return htmlEscaper.escape("(" + typeName + ") " + miscFile.getPath().getNameStr());
    }
    
    return htmlEscaper.escape("(" + typeName + ") " + record.getCBText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setReadOnlyHTML(String htmlToUse, WebEngine weToUse, TextViewInfo viewInfo, HDT_Base recordToHilite)
  {   
    if (textToHilite.length() == 0)
      lastTextToHilite = "";
    
    if (viewInfo.scrollPos > 0)
      htmlToUse = htmlToUse.replace("<body", "<body onload='window.scrollTo(0," + viewInfo.scrollPos + ")'");
    
    Document doc = makeDocLinksExternal(Jsoup.parse(htmlToUse.replace("contenteditable=\"true\"", "contenteditable=\"false\"")));
    
    addLinks(doc.body(), recordToHilite);
       
    weToUse.loadContent(doc.html().replace("</head>", headContent));
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
      MainTextWrapper.hiliteText(string, we);
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
    
    viewInfo.scrollPos = getWebEngineScrollPos(we);

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

  public static int getWebEngineScrollPos(WebEngine theWE)
  {
    return Integer.class.cast(theWE.executeScript("document.body.scrollTop")).intValue();
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
      displayItems = new ArrayList<DisplayItem>();
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

  public static void appendImgTagsForLabel(HDT_WorkLabel label, StringBuilder innerHtml, boolean addSpace)
  {
    innerHtml.append("<span style=\"display:inline-block; font-size:12pt; width:16px; height:16px; padding:0px; border:0px; margin:0px;\">");
    innerHtml.append(getGoToRecordAnchor(label, " style=\"width:16px; height:16px; padding:0px; border:0px; margin:0px; background-repeat: no-repeat; background-image:url('" + getImageDataURI(ui.getGraphicRelativePathByType(hdtWorkLabel)) + "'); text-decoration: none;\"",
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")); 
    innerHtml.append("</span>");

    if (addSpace) innerHtml.append("&nbsp;"); // Seems to be an inconsistency in how img tags (and spans with images)
                                              // are handled by the rendering engine; in a <summary> tag, there is no space after images     
    StrongLink link = label.getLink();
    if (link == null) return;
    
    Set<Connector> spokes = link.getSpokes();
    
    spokes.forEach(spoke ->
    {
      if (spoke.getType() != hdtWorkLabel)
      { 
        innerHtml.append("<span style=\"display:inline-block; font-size:12pt; width:16px; height:16px; padding:0px; border:0px; margin:0px;\">");
        innerHtml.append(getGoToRecordAnchor(spoke.getSpoke(), " style=\"width:16px; height:16px; padding:0px; border:0px; margin:0px; background-repeat: no-repeat;background-image:url('" + getImageDataURI(ui.getGraphicRelativePathByType(spoke.getType())) + "'); text-decoration: none;\"",
            "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;")); 
        innerHtml.append("</span>");
        
        if (addSpace) innerHtml.append("&nbsp;"); // Seems to be an inconsistency in how img tags (and spans with images)
      }                                           // are handled by the rendering engine; in a <summary> tag, there is no space after images       
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getAnchorForUnitable(HDT_RecordWithConnector uRecord)
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

  private static String getKeyWorkSpanElement(HDT_RecordWithConnector recordWMT, MutableInt tagNdx, boolean sortByName, boolean topmost)
  {
    tagNdx.increment();
    
    String span = "<span id=SKW" + tagNdx.toString() + " hypnconType=" + String.valueOf(recordWMT.getType().ordinal()) + " hypnconID=" + String.valueOf(recordWMT.getID()); 
    
    if (sortByName)
      return span + " class=\"" + ALPHA_SORTED_INNER_CLASS + (topmost ? " " + TOPMOST_CLASS : "") + "\">";
    
    return span + " class=\"" + NUMERIC_SORTED_INNER_CLASS + (topmost ? " " + TOPMOST_CLASS : "") + "\">";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String detailsTag(String divitID, TextViewInfo viewInfo, boolean defOpen)
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

  public static void appendSubLabelsKeyWorkBody(HDT_WorkLabel parentLabel, StringBuilder innerHtml, boolean sortByName, MutableInt tagNdx, TextViewInfo viewInfo, String parentDivitID)
  {
    if ((parentLabel == null) || parentLabel.subLabels.isEmpty()) return;
    
    ArrayList<HDT_WorkLabel> sortedLabels = new ArrayList<HDT_WorkLabel>(parentLabel.subLabels);
    sortedLabels.sort((label1, label2) -> label1.name().compareToIgnoreCase(label2.name()));
        
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

  int getNestedKeyWorkCount(HDT_RecordWithConnector record, List<KeyWork> passedKeyWorks)
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
    
    concepts.sort((t1, t2) -> t1.getSortKey().compareTo(t2.getSortKey()));
    
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
    Document doc = Jsoup.parse(getHtmlEditorText(html));
    StringBuilder innerHtml = new StringBuilder("");
    MutableBoolean firstOpen = new MutableBoolean(false);
    int keyWorksSize = getNestedKeyWorkCount(curRecord, keyWorks);
    
    if (doc.head().getElementsByTag("style").isEmpty())
      doc.head().prepend(mainTextHeadStyleTag());
    
    if (doc.body().text().trim().length() == 0)
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
              if (relRecordsHtml.length() == 0)
                relRecordsHtml.append("<b hypncon-no-links=true>Related concepts: </b>");
              else
                relRecordsHtml.append("; ");

              relRecordsHtml.append(getGoToRecordAnchor(concept, "", concept.getExtendedName()));
            });
          }
          else
          {
            Set<Connector> displayers = curRecord.getMainText().getDisplayers();                        
            
            displayers.removeIf(displayer -> displayerIsAlreadyShowing(displayer.getSpoke()));
                          
            displayers.forEach(displayer ->
            {
              if (relRecordsHtml.length() == 0)
                relRecordsHtml.append("<b hypncon-no-links=true>Displayers: </b>");
              else
                relRecordsHtml.append("; ");

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
            
            innerHtml.append(detailsTag(KEYWORKS_DIVIT_ID, viewInfo, true) + "<summary><b>Key Works</b>&nbsp;");
            innerHtml.append("<a hypncon=\"true\" href=\"\" title=\"Turn key work details on/off\" onclick=\"javascript:callToJava(" + String.valueOf(JS_EVENT_DETAILED_KEY_WORKS) + "); return false;\"><img border=0 width=16 height=16 src=\"" + getImageDataURI("resources/images/key-work-details.png") + "\"></img></a>");
            innerHtml.append("<span style=\"display: " + (sortByName ? "inline" : "none") + ";\" class=\"" + ALPHA_SORTED_OUTER_CLASS + "\"><a hypncon=\"true\" title=\"Sort by year\" href=\"\" onclick=\"javascript:switchTo19(); return false;\"><img border=0 width=16 height=16 src=\"" + getImageDataURI("resources/images/sort_19.png") + "\"></img></a></span>");
            innerHtml.append("<span style=\"display: " + (sortByName ? "none" : "inline") + ";\" class=\"" + NUMERIC_SORTED_OUTER_CLASS + "\"><a hypncon=\"true\" title=\"Sort alphabetically\" href=\"\" onclick=\"javascript:switchToAZ(); return false;\"><img border=0 width=16 height=16 src=\"" + getImageDataURI("resources/images/sort_az.png") + "\"></img></a></span>");
            innerHtml.append("</summary><br><div class=\"" + NUMERIC_SORTED_OUTER_CLASS + "\" style=\"margin-left: 3.5em; display: " + (sortByName ? "none" : "block") + ";\">");
            appendKeyWorkSpanAndBody(curRecord, innerHtml, false, tagNdx, true, viewInfo);
            
            if (curLabel != null)
              if (curLabel.subLabels.isEmpty() == false)
              {
                if (keyWorks.size() > 0) innerHtml.append("<br>");
                appendSubLabelsKeyWorkBody(curLabel, innerHtml, false, tagNdx, viewInfo, "num" + makeElementID(curLabel));
              }

            innerHtml.append("</div><div class=\"" + ALPHA_SORTED_OUTER_CLASS + "\" style=\"margin-left: 3.5em; display: " + (sortByName ? "block" : "none") + ";\">");            
            appendKeyWorkSpanAndBody(curRecord, innerHtml, true, tagNdx, true, viewInfo);

            if (curLabel != null)
              if (curLabel.subLabels.isEmpty() == false)
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
          
          innerHtml.append(secondaryHtml);
          innerHtml.append("</div></details>");
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

  private static String makeElementID(HDT_Base record)
  {
    return db.getTypeTagStr(record.getType()) + String.valueOf(record.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HashMap<String, String> getKeyWorkLinkMap(List<KeyWork> keyWorks, List<String> sortedKeys, boolean sortByName)
  {
    HashMap<String, String> linkMap = new HashMap<>();
    HashMap<String, KeyWork> keyToKeyWork = new HashMap<>();
        
    keyWorks.forEach(keyWork ->
    {             
      String searchKey = keyWork.getSearchKey(true).replace(" ", "&nbsp;");
      
      linkMap.put(searchKey, getKeywordLink(searchKey, new KeywordLink(0, searchKey.length(), new SearchKeyword(searchKey, keyWork.getRecord())), ""));
      keyToKeyWork.put(searchKey, keyWork);
      sortedKeys.add(searchKey);
    });
    
    if (sortByName)
      sortedKeys.sort((s1, s2) -> s1.compareToIgnoreCase(s2));
    else
      sortedKeys.sort((s1, s2) -> keyToKeyWork.get(s1).compareTo(keyToKeyWork.get(s2)));
    
    return linkMap;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void appendKeyWorkSpanAndBody(HDT_RecordWithConnector recordWMT, StringBuilder innerHtml, boolean sortByName, MutableInt tagNdx, boolean topmost, TextViewInfo viewInfo)
  {    
    innerHtml.append(getKeyWorkSpanElement(recordWMT, tagNdx, sortByName, topmost));
    
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
    List<String> searchKeys = new ArrayList<String>();
    
    HashMap<String, String> linkMap = getKeyWorkLinkMap(keyWorks, searchKeys, sortByName);    
    
    boolean first = true;
    
    for (String searchKey : searchKeys)
    {
      String link = linkMap.get(searchKey);
      
      if (first)
        innerHtml.append(link);
      else
        innerHtml.append(", " + link);
      
      first = false;
    }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public static String getSecondaryDisplayHtml(HDT_RecordWithConnector recordWMT, MutableInt tagNdx, TextViewInfo viewInfo)
  {
    MainText mainText = recordWMT.getMainText();
    List<KeyWork> keyWorks = mainText.getKeyWorks();  
    Document doc = Jsoup.parse(mainText.getHtml());
    
    if (collEmpty(keyWorks))
      return doc.body().html();
    
    boolean sortByName = db.prefs.getBoolean(PREF_KEY_KEY_WORK_SORT_BY_NAME, true);
    
    StringBuilder secondaryHtml = new StringBuilder("<div class=\"" + NUMERIC_SORTED_OUTER_CLASS + "\" style=\"display: " + (sortByName ? "none" : "block") + ";\"><b>Key Works:&nbsp;</b>");
    appendKeyWorkSpanAndBody(recordWMT, secondaryHtml, false, tagNdx, false, viewInfo);
    
    secondaryHtml.append("</div><div class=\"" + ALPHA_SORTED_OUTER_CLASS + "\" style=\"display: " + (sortByName ? "block" : "none") + ";\"><b>Key Works:&nbsp;</b>");
    appendKeyWorkSpanAndBody(recordWMT, secondaryHtml, true, tagNdx, false, viewInfo);
    
    secondaryHtml.append("</div>");
    
    if (ultraTrim(doc.text()).length() > 0)
      secondaryHtml.append("<br>" + doc.body().html());  
    
    return secondaryHtml.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void beginEditing(boolean focus)
  {
    removeFromAnchor(bpEditorRoot);
    removeFromAnchor(view);
    
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
      keyWorks = new ArrayList<KeyWork>();
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
     
  private static List<Integer> zoomFactors = Arrays.asList(25, 33, 50, 67, 75, 80, 90, 100, 110, 125, 150, 175, 200, 250, 300, 400, 500);
  
  public static void webViewAddZoom(WebView view, String prefID)
  {
    view.addEventFilter(ScrollEvent.SCROLL, (ScrollEvent e) ->
    {
      double deltaY = e.getDeltaY();
      if ((e.isControlDown() == false) || (deltaY == 0)) return;
      e.consume();
      
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

}