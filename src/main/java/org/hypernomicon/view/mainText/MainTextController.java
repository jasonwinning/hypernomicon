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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.items.MainText.DisplayItemType.*;
import static org.hypernomicon.view.populators.Populator.*;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.KeyWork;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.MainText.DisplayItem;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.view.controls.HiddenSidesPane;
import org.hypernomicon.view.dialogs.NewLinkDialogController;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.tabs.FileTabController;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.WorkTabController;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.geometry.Bounds;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextArea;
import javafx.scene.control.TitledPane;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.paint.Color;
import javafx.scene.web.HTMLEditor;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;

public class MainTextController
{
  @FXML private Button btnMoveUp;
  @FXML private Button btnMoveDown;
  @FXML private Button btnInsert;
  @FXML private Button btnRemove;
  @FXML private HTMLEditor he;
  @FXML private ListView<MainText.DisplayItem> lvRecords;
  @FXML private ComboBox<HyperTableCell> cbType;
  @FXML private ComboBox<HyperTableCell> cbName;
  @FXML private ComboBox<HyperTableCell> cbKeyType;
  @FXML private ComboBox<HyperTableCell> cbKeyName;
  @FXML private TextArea taKeyWorks;
  @FXML private TitledPane tpKeyWorks;
  @FXML private Button btnAdd;
  @FXML private Button btnNew;
  @FXML private BorderPane borderPane;
  @FXML private AnchorPane anchorPane;
  @FXML private GridPane gridPane;
  @FXML private HiddenSidesPane hsPane;
  
  private HyperCB hcbType, hcbName, hcbKeyType, hcbKeyName;
  private HDT_RecordWithConnector curRecord;
  BooleanProperty prop = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<DisplayItem> getDisplayItems() { return lvRecords.getItems(); }
  private void clearText()                   { he.setHtmlText(disableLinks(getHtmlEditorText(""))); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear() 
  { 
    taKeyWorks.clear();
    clearDisplayItems();
    clearText(); 
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void focus()                        
  { 
    runDelayedInFXThread(5, 100, event ->
    {
      final WebView view = (WebView) he.lookup("WebView");
      Platform.runLater(() -> 
      {
        view.fireEvent(new MouseEvent(MouseEvent.MOUSE_PRESSED, 15, 100, 200, 200, MouseButton.PRIMARY, 1, false, false, false, false, false, false, false, false, false, false, null));
        he.requestFocus();
        view.fireEvent(new MouseEvent(MouseEvent.MOUSE_RELEASED, 15, 100, 200, 200, MouseButton.PRIMARY, 1, false, false, false, false, false, false, false, false, false, false, null));
      });
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("restriction")
  public void init()
  {
    final WebView webview = (WebView) he.lookup("WebView");
    GridPane.setHgrow(webview, Priority.ALWAYS);
    GridPane.setVgrow(webview, Priority.ALWAYS);
           
    RecordTypePopulator rtp = new RecordTypePopulator();
    EnumSet<HDT_RecordType> typeSet = EnumSet.noneOf(HDT_RecordType.class);
    
    EnumSet.allOf(HDT_RecordType.class).forEach(type ->
    {
      if (type.hasConnector() && (type != hdtHub) && (type != hdtWorkLabel))
        typeSet.add(type);
    });
    
    rtp.setTypes(typeSet);
    
    hcbType = new HyperCB(cbType, ctDropDownList, rtp, null);
    hcbName = new HyperCB(cbName, ctDropDownList, new RecordByTypePopulator(), null);
    
    hcbType.getComboBox().getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> 
    {    
      if (newValue == null) return;
      
      HDT_RecordType oldType = HyperTableCell.getCellType(oldValue),
                     newType = HyperTableCell.getCellType(newValue);
      
      if (oldType != newType)
      {        
        appPrefs.put(PREF_KEY_DISPLAY_RECORD_TYPE, db.getTypeTagStr(newType));
        
        ((RecordByTypePopulator)hcbName.getPopulator()).setRecordType(dummyRow, newType);
        if (oldType != hdtNone) 
          hcbName.selectID(-1);
      }
    });
       
    rtp = new RecordTypePopulator();
    rtp.setTypes(EnumSet.of(hdtWork, hdtMiscFile));
    
    hcbKeyType = new HyperCB(cbKeyType, ctDropDownList, rtp, null);
    hcbKeyName = new HyperCB(cbKeyName, ctDropDownList, new RecordByTypePopulator(), null);
    
    Background bg = new Background(new BackgroundFill(Color.SLATEBLUE, null, null));
    
    hsPane.setBackground(bg);
    
    hsPane.setTriggerDistance(32.0);
    
    prop = new SimpleBooleanProperty();
    prop.bind(cbType.focusedProperty().or(cbName.focusedProperty()));
    
    prop.addListener((observable, oldValue, newValue) -> 
    {
      if (newValue)
        hsPane.setPinnedSide(Side.RIGHT);
      else
      {
        hsPane.setPinnedSide(null);
        
        com.sun.glass.ui.Robot robot = com.sun.glass.ui.Application.GetApplication().createRobot();
        int x = robot.getMouseX(), y = robot.getMouseY();
        
        Bounds bounds = hsPane.getBoundsInLocal();
        Bounds screenBounds = hsPane.localToScreen(bounds);
        
        if (screenBounds == null) return;
        
        if (screenBounds.contains(x, y) == false)
          hsPane.hide();
      }
    });
    
    hcbKeyType.getComboBox().getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      
      if (HyperTableCell.getCellType(oldValue) != HyperTableCell.getCellType(newValue))
      {
        ((RecordByTypePopulator)hcbKeyName.getPopulator()).setRecordType(dummyRow, HyperTableCell.getCellType(newValue));
        if (HyperTableCell.getCellType(oldValue) != hdtNone) 
          hcbKeyName.selectID(-1);
      }      
    });
    
    lvRecords.setCellFactory(listView -> new ListCell<DisplayItem>()
    {
      @Override public void updateItem(DisplayItem item, boolean empty) 
      {
        super.updateItem(item, empty);
        
        if (empty || item == null)
        {
          setText(null);
          return;
        }
        
        switch (item.type)
        {
          case diDescription:
            
            setText("This record's description");
            break;
          case diKeyWorks:
            
            setText("Key works");
            break;
          case diRecord:
            
            setText(db.getTypeName(item.record.getType()) + ": " + item.record.getCBText());
            break;
            
          default:
            setText("");
            break;          
        }        
      }      
    });

    btnMoveUp.setDisable(true);
    btnMoveDown.setDisable(true);
    btnRemove.setDisable(true);
    btnInsert.setDisable(true);
    
    cbName.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      HDT_RecordWithConnector record = (HDT_RecordWithConnector) HyperTableCell.getRecord(newValue);
      
      btnInsert.setDisable(record == null);
    });
    
    cbKeyName.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      HDT_Base record = HyperTableCell.getRecord(newValue);
      
      btnAdd.setDisable(record == null);
    });
    
    lvRecords.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) 
      {
        btnMoveUp.setDisable(true);
        btnMoveDown.setDisable(true);
        btnRemove.setDisable(true);
        return;
      }
      
      btnMoveUp.setDisable(false);
      btnMoveDown.setDisable(false);
      
      switch (newValue.type)
      {
        case diRecord:
          btnRemove.setDisable(false);
          break;
          
        default:
          btnRemove.setDisable(true);
          break;
      }
    });
    
    btnMoveUp  .setOnAction(event -> btnMoveUpClick());    
    btnMoveDown.setOnAction(event -> btnMoveDownClick());    
    btnRemove  .setOnAction(event -> btnRemoveClick());    
    btnInsert  .setOnAction(event -> btnInsertClick());
    btnAdd     .setOnAction(event -> btnAddClick());
    btnNew     .setOnAction(event -> btnNewClick());
    
    webview.setOnContextMenuRequested(event ->
    {
      MenuItem menuItem1 = new MenuItem("Paste plain text");
      menuItem1.setOnAction(getPlainTextAction(false));

      MenuItem menuItem2 = new MenuItem("Paste plain text without line breaks");
      menuItem2.setOnAction(getPlainTextAction(true));
      
      setHTMLContextMenu(menuItem1, menuItem2);
    });
    
    he.setOnMouseClicked(event -> event.consume());
    he.setOnMousePressed(event -> event.consume());
    he.setOnMouseReleased(event -> event.consume());
    
    ToolBar bar = (ToolBar) he.lookup(".top-toolbar");
    
    Button btnLink = new Button("", getImageViewForRelativePath("resources/images/world_link.png"));
    btnLink.setTooltip(new Tooltip("Insert web link"));
    btnLink.setOnAction(event -> btnLinkClick());
    
    Button btnClear = new Button("", getImageViewForRelativePath("resources/images/broom.png"));
    btnClear.setTooltip(new Tooltip("Clear"));
    btnClear.setOnAction(event -> 
    {
      clearText();
      safeFocus(he);
    });
    
    Button btnEditLayout = new Button("", getImageViewForRelativePath("resources/images/document_insert.png"));
    btnEditLayout.setTooltip(new Tooltip("Edit layout"));
    btnEditLayout.setOnAction(event ->
    {
      hsPane.show(Side.RIGHT, true);
      runDelayedInFXThread(5, 100, e -> cbType.requestFocus());
    });
       
    bar.getItems().add(btnLink);
    bar.getItems().add(btnClear);
    bar.getItems().add(btnEditLayout);

    he.setFocusTraversable(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private void btnNewClick()
  {
    HDT_RecordType keyType = hcbKeyType.selectedType();
    
    if (ui.cantSaveRecord(true)) return;
    
    ArrayList<KeyWork> keyWorks = new ArrayList<>();
    keyWorks.addAll(curRecord.getMainText().getKeyWorks());

    HDT_RecordWithPath keyRecord = db.createNewBlankRecord(keyType);
    KeyWork keyWork = new KeyWork(keyRecord);

    keyWorks.add(keyWork);
    curRecord.getMainText().setKeyWorksFromList(keyWorks, true);
    ui.goToRecord(keyRecord, false);
    
    if (keyType == hdtWork)
    {
      WorkTabController workCtrlr = HyperTab.getHyperTab(workTab);
      
      if (workCtrlr.showWorkDialog(null) == false)
        ui.deleteCurrentRecord(false);
    }
    else
    {
      FileTabController fileCtrlr = HyperTab.getHyperTab(miscFileTab);
      
      if (fileCtrlr.showFileDialog() == false)
        ui.deleteCurrentRecord(false);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private void btnAddClick()
  {  
    int keyID = hcbKeyName.selectedID();
    if (keyID < 1) return;
    
    HDT_RecordType keyType = hcbKeyType.selectedType();
    
    ArrayList<KeyWork> list = new ArrayList<>();
    
    getKeyWorks(list);
    
    for (KeyWork keyWork : list)
    {
      if (keyWork.getRecordID() == keyID)
        if (keyWork.getRecordType() == keyType)
          return;
    }
    
    KeyWork keyWork = new KeyWork((HDT_RecordWithPath) db.records(keyType).getByID(keyID));
    
    String keyText = taKeyWorks.getText();
    if (keyText.length() == 0)
      keyText = keyWork.getEditorText();
    else
      keyText = keyText + ", " + keyWork.getEditorText();
    
    taKeyWorks.setText(keyText);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private void btnMoveUpClick()
  {
    DisplayItem item = lvRecords.getSelectionModel().getSelectedItem();
    
    if (item == null)
    { 
      safeFocus(lvRecords);
      return;
    }
    
    int ndx = lvRecords.getItems().indexOf(item);
    if (ndx == 0)
    { 
      safeFocus(lvRecords);
      return;
    }
    
    lvRecords.getItems().remove(ndx);
    lvRecords.getItems().add(ndx - 1, item);
    lvRecords.getSelectionModel().clearAndSelect(ndx - 1);
    
    hsPane.requestLayout();
    safeFocus(lvRecords);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private void btnMoveDownClick()
  {
    DisplayItem item = lvRecords.getSelectionModel().getSelectedItem();
    
    if (item == null)
    { 
      safeFocus(lvRecords);
      return;
    }
    
    int ndx = lvRecords.getItems().indexOf(item);
    if (ndx == lvRecords.getItems().size() - 1)
    { 
      safeFocus(lvRecords);
      return;
    }

    lvRecords.getItems().remove(ndx);
    lvRecords.getItems().add(ndx + 1, item);
    lvRecords.getSelectionModel().clearAndSelect(ndx + 1);
    
    hsPane.requestLayout();
    safeFocus(lvRecords);  
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private void btnRemoveClick()
  {
    DisplayItem item = lvRecords.getSelectionModel().getSelectedItem();
    
    if (item != null)
      if (item.type == diRecord)
        lvRecords.getItems().remove(item);
      
    hsPane.requestLayout();
    safeFocus(lvRecords);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private void btnInsertClick()
  {
    HDT_RecordWithConnector record = (HDT_RecordWithConnector) HyperTableCell.getRecord(hcbName.selectedHTC());
    
    if (record == null) return;
    
    DisplayItem item = new DisplayItem(record);
    int ndx = lvRecords.getSelectionModel().getSelectedIndex();
    if (ndx == -1)
      lvRecords.getItems().add(item);
    else
      lvRecords.getItems().add(ndx, item);
    
    hsPane.requestLayout();
    
    safeFocus(lvRecords);
    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  
  
  private void btnLinkClick()
  {
    Node webNode = he.lookup(".web-view");
    if (webNode instanceof WebView) 
    {
      WebView webView = (WebView) webNode;
      WebEngine engine = webView.getEngine();
      
      String selText = (String) engine.executeScript("window.getSelection().getRangeAt(0).toString()");
      
      NewLinkDialogController dlg = NewLinkDialogController.create("Insert Link", convertToSingleLine(selText));
      
      if (dlg.showModal() == false) return;
                
      String urlText = dlg.tfURL.getText();
      
      String anchorTag = "<a title=\"" + htmlEscaper.escape(urlText) + "\" href=\"" + urlText + "\">" + htmlEscaper.escape(dlg.tfDisplayText.getText()) + "</a>";
      
      engine.executeScript("insertHtmlAtCursor('" + anchorTag + "')");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private EventHandler<ActionEvent> getPlainTextAction(boolean noCarriageReturns)
  {
    return event ->
    {
      String text = getClipboardText(noCarriageReturns);
      
      if (text.length() == 0) return;
      
      text =  htmlEscaper.escape(text);
    
      if (noCarriageReturns == false)
      {
        text = text.replaceAll("\\R", "<br>");
        text = text.replaceAll("\\v", "<br>");
      }
      
      WebView webview = (WebView) he.lookup("WebView");
      WebEngine engine = webview.getEngine();
      engine.executeScript("insertHtmlAtCursor('" + text + "')");
    };
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String disableLinks(String hyperText)
  {
    return hyperText.replace("<style>", "<script>\n" +
        "function insertHtmlAtCursor(html)\n" +
        "{\n" +
        "  var range, node;\n" + 
        "  if (window.getSelection && window.getSelection().getRangeAt)\n" +
        "  {\n" +
        "    range = window.getSelection().getRangeAt(0);\n" +
        "    range.deleteContents();\n" +
        "    node = range.createContextualFragment(html);\n" +
        "    range.insertNode(node);\n" +
        "  }\n" +
        "  else if (document.selection && document.selection.createRange)\n" +
        "  {\n" +
        "    range = document.selection.createRange();\n" +
        "    range.deleteContents();\n" +
        "    range.pasteHTML(html);\n" +
        "  }\n" +
        "}\n\n" +   
        "</script><style>a { pointer-events: none; }");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getHtmlFromEditor(String editorHtml)
  {
    Document doc = Jsoup.parse(editorHtml);
    Elements elements;
       
    elements = doc.getElementsByTag("script");
    for (Element element : elements)
      element.remove();
    
    elements = doc.getElementsByAttributeValue("id", "key_works");
    for (Element element : elements)
      element.remove();
    
    editorHtml = doc.html();    
    
    return editorHtml.replace("a { pointer-events: none; }", "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getScrollPos()
  {
    WebView webView = (WebView) he.lookup("WebView");
    if (webView != null) 
    {
      WebEngine engine = webView.getEngine();
      return MainTextWrapper.getWebEngineScrollPos(engine);
    }
    return 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void getKeyWorks(List<KeyWork> keyWorksArg)
  {
    keyWorksArg.clear();
    
    HashSet<HDT_RecordWithPath> keyWorkRecords = new HashSet<>();
    String kwHtml = taKeyWorks.getText();
    Document subDoc = Jsoup.parse(kwHtml);
    Elements aElements = subDoc.getElementsByTag("a");
          
    for (Element aElement : aElements)
    {
      int id = parseInt(aElement.attributes().get("id") , -1);
      HDT_RecordType type = db.parseTypeTagStr(aElement.attributes().get("type"));
      
      if ((id > 0) && (type != hdtNone))
      {
        HDT_RecordWithPath record = (HDT_RecordWithPath) db.records(type).getByID(id);
        if (record != null)
        {
          String text = aElement.ownText();
          keyWorksArg.add(new KeyWork(record.getType(), record.getID(), text, true));
        }
      }
      
      aElement.remove();
    }
    
    KeywordLinkList list = new KeywordLinkList();
    String kwText = extractTextFromHTML(subDoc.html());
    list.generate(kwText);
    
    for (KeywordLink link : list.getLinks())
    {
      SearchKeyword key = link.key;
      String str = kwText.substring(link.offset, link.offset + link.length);
      
      if ((key.record.getType() == hdtWork) || (key.record.getType() == hdtMiscFile))
      {
        HDT_RecordWithPath keyWorkRecord = (HDT_RecordWithPath) key.record;
        
        if (keyWorkRecords.contains(keyWorkRecord) == false)
        {
          keyWorksArg.add(new KeyWork(keyWorkRecord.getType(), keyWorkRecord.getID(), str, true));
          keyWorkRecords.add(keyWorkRecord);
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getHtmlAndKeyWorks(List<KeyWork> keyWorksArg)
  {    
    getKeyWorks(keyWorksArg);
    return getHtmlFromEditor(he.getHtmlText()); 
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setFromMainText(MainText mainText)
  {   
    if (mainText != null)
      set(mainText.getRecord(), mainText.getHtml(), mainText.getDisplayItemsCopy(), mainText.getKeyWorks());
    else
      clearDisplayItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearDisplayItems()
  {
    lvRecords.setItems(FXCollections.observableArrayList());
    
    hcbKeyName.selectID(-1);
    ListView<HyperTableCell> lv = getCBListView(cbKeyName);
    if (lv != null) lv.scrollTo(0);    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void set(HDT_RecordWithConnector record, String html, List<DisplayItem> displayItems, List<KeyWork> keyWorks)
  {
    String keyWorksText = "";
    curRecord = record;
    disableCache(taKeyWorks);
    
    clearDisplayItems();
    
    if ((keyWorks != null) && (MainText.typeHasKeyWorks(record.getType())))
    {
      if (borderPane.getTop() == null)
        borderPane.setTop(tpKeyWorks);
      
      if (keyWorks.size() > 0)
        tpKeyWorks.setExpanded(true);
      else
        tpKeyWorks.setExpanded(record.getType() != hdtPerson);
      
      HashMap<String, String> linkMap = new HashMap<>();
      List<String> searchKeys = new ArrayList<String>();
      
      for (KeyWork keyWork : keyWorks)
      {             
        String searchKey = keyWork.getSearchKey(true);
        
        linkMap.put(searchKey, keyWork.getEditorText());
        searchKeys.add(searchKey);
      }
      
      searchKeys.sort((s1, s2) -> s1.compareToIgnoreCase(s2));
      
      boolean first = true;
      
      for (String searchKey : searchKeys)
      {
        if (first)
          keyWorksText = keyWorksText + linkMap.get(searchKey);
        else
          keyWorksText = keyWorksText + ", " + linkMap.get(searchKey);
        
        first = false;
      }
    }
    else
      if (borderPane.getTop() == tpKeyWorks)
        borderPane.setTop(null);
    
    he.setHtmlText(disableLinks(getHtmlEditorText(html)));
    taKeyWorks.setText(keyWorksText);
    
    if (hcbType.selectedType() == hdtNone)
    {
      HDT_RecordType type = db.parseTypeTagStr(appPrefs.get(PREF_KEY_DISPLAY_RECORD_TYPE, ""));
      
      hcbType.selectType(type == hdtNone ? hdtConcept : type);
    }
    
    hcbName.setChoicesChanged();
    
    if (hcbKeyType.selectedType() == hdtWork)
      hcbKeyName.setChoicesChanged();
    else
      hcbKeyType.selectType(hdtWork);
    
    if (displayItems != null)
      lvRecords.setItems(FXCollections.observableArrayList(displayItems));   
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty()
  {
    return getHtmlAndKeyWorks(new ArrayList<KeyWork>()).trim().length() == 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
