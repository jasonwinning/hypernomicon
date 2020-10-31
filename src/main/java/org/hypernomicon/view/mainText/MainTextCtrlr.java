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
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.items.MainText.DisplayItemType.*;
import static org.hypernomicon.view.populators.Populator.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import com.google.common.collect.EvictingQueue;
import com.sun.javafx.webkit.Accessor;

import org.hypernomicon.dialogs.FileDlgCtrlr;
import org.hypernomicon.dialogs.InsertMiscFileDlgCtrlr;
import org.hypernomicon.dialogs.NewLinkDlgCtrlr;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.items.KeyWork;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.MainText.DisplayItem;
import org.hypernomicon.model.items.MainText.DisplayItemType;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_FileType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.view.controls.HiddenSidesPane;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.collections.ListChangeListener.Change;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.geometry.Bounds;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuButton;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TextArea;
import javafx.scene.control.TitledPane;
import javafx.scene.control.ToolBar;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.paint.Color;
import javafx.scene.robot.Robot;
import javafx.scene.web.HTMLEditor;
import javafx.scene.web.HTMLEditorSkin.Command;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;

public class MainTextCtrlr
{
  @FXML private BorderPane borderPane;
  @FXML private Button btnAdd, btnInsert, btnMoveDown, btnMoveUp, btnNew, btnRemove;
  @FXML private ComboBox<HyperTableCell> cbKeyName, cbKeyType, cbName, cbType;
  @FXML private HTMLEditor he;
  @FXML private HiddenSidesPane hsPane;
  @FXML private ListView<MainText.DisplayItem> lvRecords;
  @FXML private TextArea taKeyWorks;
  @FXML private TitledPane tpKeyWorks;

  private HyperCB hcbType, hcbName, hcbKeyType, hcbKeyName;
  private HDT_RecordWithConnector curRecord;
  private BooleanProperty prop = null;
  private boolean ignoreKeyEvent = false;
  private Queue<KeyEvent> boldEvents = EvictingQueue.create(50);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  List<DisplayItem> getDisplayItems() { return lvRecords.getItems(); }
  boolean isEmpty()                   { return getHtmlAndKeyWorks(new ArrayList<>()).trim().isEmpty(); }

  int getScrollPos()    { return nullSwitch(getWebView(), 0, webView -> webEngineScrollPos(webView.getEngine())); }

  private void clearText()     { he.setHtmlText(disableLinks("")); }
  private WebView getWebView() { return (WebView) he.lookup("WebView"); }
  WebEngine getEngine()        { return getWebView().getEngine(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    taKeyWorks.clear();
    clearDisplayItems();
    clearText();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void focus()
  {
    runDelayedInFXThread(5, 100, () ->
    {
      final WebView view = getWebView();
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
  
  void init()
  {
    final WebView webview = getWebView();
    GridPane.setHgrow(webview, Priority.ALWAYS);
    GridPane.setVgrow(webview, Priority.ALWAYS);

    EnumSet<RecordType> typeSet = EnumSet.noneOf(RecordType.class);

    EnumSet.allOf(RecordType.class).forEach(type ->
    {
      if (type.hasConnector() && (type != hdtHub) && (type != hdtWorkLabel))
        typeSet.add(type);
    });

    RecordTypePopulator rtp = new RecordTypePopulator(typeSet);

    hcbType = new HyperCB(cbType, ctDropDownList, rtp);
    hcbName = new HyperCB(cbName, ctDropDownList, new RecordByTypePopulator());

    hcbType.getComboBox().getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      RecordType oldType = HyperTableCell.getCellType(oldValue),
                 newType = HyperTableCell.getCellType(newValue);

      if (oldType != newType)
      {
        appPrefs.put(PREF_KEY_DISPLAY_RECORD_TYPE, db.getTypeTagStr(newType));

        ((RecordByTypePopulator)hcbName.getPopulator()).setRecordType(dummyRow, newType);
        if (oldType != hdtNone)
          hcbName.selectID(-1);
      }
    });

    rtp = new RecordTypePopulator(EnumSet.of(hdtWork, hdtMiscFile));

    hcbKeyType = new HyperCB(cbKeyType, ctDropDownList, rtp);
    hcbKeyName = new HyperCB(cbKeyName, ctDropDownList, new RecordByTypePopulator());

    Background bg = new Background(new BackgroundFill(Color.SLATEBLUE, null, null));

    hsPane.setBackground(bg);

    hsPane.setTriggerDistance(32.0);

    prop = new SimpleBooleanProperty();
    prop.bind(cbType.focusedProperty().or(cbName.focusedProperty()));

    prop.addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        hsPane.setPinnedSide(Side.RIGHT);
      else
      {
        hsPane.setPinnedSide(null);

        Bounds screenBounds = hsPane.localToScreen(hsPane.getBoundsInLocal());

        if ((screenBounds != null) && screenBounds.contains(new Robot().getMousePosition()) == false)
          hsPane.hide();
      }
    });

    hcbKeyType.getComboBox().getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      if (HyperTableCell.getCellType(oldValue) != HyperTableCell.getCellType(newValue))
      {
        ((RecordByTypePopulator)hcbKeyName.getPopulator()).setRecordType(dummyRow, HyperTableCell.getCellType(newValue));
        if (HyperTableCell.getCellType(oldValue) != hdtNone)
          hcbKeyName.selectID(-1);
      }
    });

    lvRecords.setCellFactory(listView -> new ListCell<>()
    {
      @Override protected void updateItem(DisplayItem item, boolean empty)
      {
        super.updateItem(item, empty);

        if (empty || (item == null))
        {
          setText(null);
          return;
        }

        switch (item.type)
        {
          case diDescription : setText("This record's description"); break;
          case diKeyWorks    : setText("Key works"); break;
          case diRecord      : setText(db.getTypeName(item.record.getType()) + ": " + item.record.getCBText()); break;
          default            : setText(""); break;
        }
      }
    });

    disableAll(btnMoveUp, btnMoveDown, btnRemove, btnInsert);

    hcbName.addListener((oldValue, newValue) -> btnInsert.setDisable(HyperTableCell.getRecord(newValue) == null));

    hcbKeyName.addListener((oldValue, newValue) -> btnAdd.setDisable(HyperTableCell.getRecord(newValue) == null));

    lvRecords.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null)
      {
        disableAll(btnMoveUp, btnMoveDown, btnRemove);
        return;
      }

      enableAll(btnMoveUp, btnMoveDown);

      btnRemove.setDisable(newValue.type != diRecord);
    });

    btnMoveUp  .setOnAction(event -> btnMoveUpClick  ());
    btnMoveDown.setOnAction(event -> btnMoveDownClick());
    btnRemove  .setOnAction(event -> btnRemoveClick  ());
    btnInsert  .setOnAction(event -> btnInsertClick  ());
    btnAdd     .setOnAction(event -> btnAddClick     ());
    btnNew     .setOnAction(event -> btnNewClick     ());

    webview.setOnContextMenuRequested(event ->
    {
      MenuItem menuItem1 = new MenuItem("Paste plain text");
      menuItem1.setOnAction(getPlainTextAction(false));

      MenuItem menuItem2 = new MenuItem("Paste plain text without line breaks");
      menuItem2.setOnAction(getPlainTextAction(true));

      setHTMLContextMenu(menuItem1, menuItem2);
    });

    he.setOnMouseClicked (Event::consume);
    he.setOnMousePressed (Event::consume);
    he.setOnMouseReleased(Event::consume);
    
    // The next 2 event filters address buggy HTMLEditor handling of ctrl-B, ctrl-I, and ctrl-U
    
    he.addEventFilter(KeyEvent.KEY_PRESSED, event ->
    {
      if (event.isControlDown() || event.isMetaDown())
      {
        if ((event.getCode() == KeyCode.B) ||
            (event.getCode() == KeyCode.I) ||
            (event.getCode() == KeyCode.U))
        {
          if (ignoreKeyEvent)
            event.consume();
          else if (event.getCode() == KeyCode.B)
          {
            String selText = (String) getEngine().executeScript("window.getSelection().rangeCount < 1 ? \"\" : window.getSelection().getRangeAt(0).toString()");
            
            if (selText.isEmpty())
            {
              KeyEvent ke = new KeyEvent(event.getSource(), webview, KeyEvent.KEY_RELEASED, event.getCharacter(), event.getText(), event.getCode(), event.isShiftDown(), event.isControlDown(), event.isAltDown(), event.isMetaDown());
              event.consume();
              boldEvents.add(ke);
              webview.fireEvent(ke);
            }
          }
          
          ignoreKeyEvent = true;
        }
      }
    });
    
    he.addEventFilter(KeyEvent.KEY_RELEASED, event ->
    {     
      if (event.isControlDown() || event.isMetaDown())
      {
        if (((event.getCode() == KeyCode.B) && (boldEvents.contains(event) == false)) ||
            (event.getCode() == KeyCode.I) ||
            (event.getCode() == KeyCode.U))
        {
          event.consume();
          
          ignoreKeyEvent = false;
        }
      }
    });
    
    ToolBar bar = (ToolBar) he.lookup(".top-toolbar");

    MenuItem menuItem0 = new MenuItem("Paste");
    menuItem0.setOnAction(event -> Accessor.getPageFor(getEngine()).executeCommand(Command.PASTE.getCommand(), null));

    MenuItem menuItem1 = new MenuItem("Paste plain text");
    menuItem1.setOnAction(getPlainTextAction(false));

    MenuItem menuItem2 = new MenuItem("Paste plain text without line breaks");
    menuItem2.setOnAction(getPlainTextAction(true));

    MenuButton btnPaste = new MenuButton("", imgViewFromRelPath("resources/images/page_paste.png"), menuItem0, menuItem1, menuItem2);
    setToolTip(btnPaste, "Paste");

    Button btnLink = new Button("", imgViewFromRelPath("resources/images/world_link.png"));
    setToolTip(btnLink, "Insert web link");
    btnLink.setOnAction(event -> btnLinkClick());

    Button btnPicture = new Button("", imgViewFromRelPath("resources/images/picture_add.png"));
    setToolTip(btnPicture, "Insert picture");
    btnPicture.setOnAction(event -> btnPictureClick());

    Button btnClear = new Button("", imgViewFromRelPath("resources/images/broom.png"));
    setToolTip(btnClear, "Clear");
    btnClear.setOnAction(event ->
    {
      clearText();
      safeFocus(he);
    });

    Button btnEditLayout = new Button("", imgViewFromRelPath("resources/images/document_insert.png"));
    setToolTip(btnEditLayout, "Edit layout");
    btnEditLayout.setOnAction(event ->
    {
      hsPane.show(Side.RIGHT, true);
      runDelayedInFXThread(5, 100, cbType::requestFocus);
    });
    
    Button btnSubscript = new Button("", imgViewFromRelPath("resources/images/text_subscript.png"));
    setToolTip(btnSubscript, "Toggle subscript for selected text");
    btnSubscript.setOnAction(event -> getEngine().executeScript("document.execCommand('subscript', false, '');"));

    Button btnSuperscript = new Button("", imgViewFromRelPath("resources/images/text_superscript.png"));
    setToolTip(btnSuperscript, "Toggle superscript for selected text");
    btnSuperscript.setOnAction(event -> getEngine().executeScript("document.execCommand('superscript', false, '');"));

    Button btnSymbol = new Button("", imgViewFromRelPath("resources/images/text_letter_omega.png"));
    setToolTip(btnSymbol, "Insert symbol at cursor");
    btnSymbol.setOnAction(event -> SymbolPickerDlgCtrlr.show());

    bar.getItems().addAll(btnLink, btnPicture, btnClear, btnEditLayout, btnSubscript, btnSuperscript, btnSymbol, btnPaste);

    bar.getItems().addListener((Change<? extends Node> c) ->
    {
      while (c.next()) c.getAddedSubList().forEach(node ->
      {
        if (node instanceof Button)
        {
          Button button = (Button)node;
          if (convertToSingleLine(strListToStr(button.getStyleClass(), false)).contains("paste"))
            Platform.runLater(() -> bar.getItems().remove(button));
        }
      });
    });

    he.setFocusTraversable(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnNewClick()
  {
    if (ui.cantSaveRecord()) return;

    RecordType keyType = hcbKeyType.selectedType();
    List<KeyWork> keyWorks = new ArrayList<>(curRecord.getMainText().getKeyWorks());

    HDT_RecordWithPath keyRecord = db.createNewBlankRecord(keyType);
    keyWorks.add(new KeyWork(keyRecord));

    curRecord.getMainText().setKeyWorksFromList(keyWorks, true);
    ui.goToRecord(keyRecord, false);

    if (keyType == hdtWork)
    {
      if (ui.workHyperTab().showWorkDialog(null) == false)
        ui.deleteCurrentRecord(false);
    }
    else
    {
      if (ui.fileHyperTab().showFileDialog(null) == false)
        ui.deleteCurrentRecord(false);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnAddClick()
  {
    int keyID = hcbKeyName.selectedID();
    if (keyID < 1) return;

    RecordType keyType = hcbKeyType.selectedType();

    List<KeyWork> list = new ArrayList<>();

    getKeyWorks(list);

    if (list.stream().anyMatch(keyWork -> (keyWork.getRecordID() == keyID) && (keyWork.getRecordType() == keyType)))
      return;

    KeyWork keyWork = new KeyWork((HDT_RecordWithPath) db.records(keyType).getByID(keyID));

    String keyText = taKeyWorks.getText();
    keyText = keyText.isEmpty() ? keyWork.getEditorText() : (keyText + ", " + keyWork.getEditorText());

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

    if ((item != null) && (item.type == diRecord))
      lvRecords.getItems().remove(item);

    hsPane.requestLayout();
    safeFocus(lvRecords);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnInsertClick()
  {
    HDT_RecordWithConnector record = HyperTableCell.getRecord(hcbName.selectedHTC());
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

  private void btnPictureClick()
  {
    InsertMiscFileDlgCtrlr imfd = InsertMiscFileDlgCtrlr.build();

    if (imfd.showModal() == false) return;

    HDT_MiscFile miscFile = imfd.getMiscFile();

    if (miscFile == null)
    {
      miscFile = db.createNewBlankRecord(hdtMiscFile);

      FileDlgCtrlr fdc = FileDlgCtrlr.build("Image File Record", miscFile, "", true);

      if (fdc.showModal() == false)
      {
        db.deleteRecord(miscFile);
        return;
      }

      if (miscFile != fdc.getFileRecord())
      {
        db.deleteRecord(miscFile);
        miscFile = fdc.getFileRecord();
      }

      miscFile.setName(fdc.tfRecordName.getText());
      HyperTableCell cell = fdc.cbType.getValue();
      int fileTypeID = HyperTableCell.getCellID(cell);
      
      if ((fileTypeID < 1) && (HyperTableCell.getCellText(cell).length() > 0))
      {
        HDT_FileType fileType = db.createNewBlankRecord(hdtFileType);
        fileTypeID = fileType.getID();
        fileType.setName(HyperTableCell.getCellText(cell));
      }
      
      miscFile.fileType.setID(fileTypeID);
    }

    WebEngine engine = getEngine();
    
    Accessor.getPageFor(engine).executeCommand(Command.INSERT_NEW_LINE.getCommand(), null);

    String imageTag = "<" + EMBEDDED_FILE_TAG + " id=\"" + miscFile.getID() + "\" width=\"300px\"/>";

    engine.executeScript("insertHtmlAtCursor('" + htmlEscaper.escape(imageTag) + "<br>')");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnLinkClick()
  {
    WebEngine engine = getEngine();

    String selText = (String) engine.executeScript("window.getSelection().rangeCount < 1 ? \"\" : window.getSelection().getRangeAt(0).toString()");

    NewLinkDlgCtrlr dlg = NewLinkDlgCtrlr.build(convertToSingleLine(selText));

    if (dlg.showModal() == false) return;

    String urlText = dlg.tfURL.getText();

    String anchorTag = "<a title=\"" + htmlEscaper.escape(urlText) + "\" href=\"" + urlText + "\">" + htmlEscaper.escape(dlg.tfDisplayText.getText()) + "</a>";

    engine.executeScript("insertHtmlAtCursor('" + anchorTag + "')");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EventHandler<ActionEvent> getPlainTextAction(boolean noCarriageReturns)
  {
    return event ->
    {
      String text = getClipboardText(noCarriageReturns);

      if (text.isEmpty()) return;

      text = htmlEscaper.escape(text);

      if (noCarriageReturns == false)
      {
        text = text.replaceAll("\\R", "<br>")
                   .replaceAll("\\v", "<br>");
      }

      getEngine().executeScript("insertHtmlAtCursor('" + text + "')");
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void getKeyWorks(List<KeyWork> keyWorksArg)
  {
    keyWorksArg.clear();

    Document subDoc = Jsoup.parse(taKeyWorks.getText());

    subDoc.getElementsByTag("a").forEach(aElement ->
    {
      int id = parseInt(aElement.attributes().get("id") , -1);
      RecordType type = db.parseTypeTagStr(aElement.attributes().get("type"));

      if ((id > 0) && (type != hdtNone))
      {
        nullSwitch((HDT_RecordWithPath) db.records(type).getByID(id), record ->
        {
          keyWorksArg.add(new KeyWork(record.getType(), record.getID(), aElement.ownText(), true));
        });
      }

      aElement.remove();
    });

    Set<HDT_RecordWithPath> keyWorkRecords = new HashSet<>();
    KeywordLinkList list = new KeywordLinkList();
    String kwText = extractTextFromHTML(subDoc.html());
    list.generate(kwText);

    list.getLinks().forEach(link ->
    {
      HDT_Record record = link.key.record;

      if ((record.getType() == hdtWork) || (record.getType() == hdtMiscFile))
      {
        HDT_RecordWithPath keyWorkRecord = (HDT_RecordWithPath) record;

        if (keyWorkRecords.contains(keyWorkRecord) == false)
        {
          String str = kwText.substring(link.offset, link.offset + link.length);
          keyWorksArg.add(new KeyWork(keyWorkRecord.getType(), keyWorkRecord.getID(), str, true));
          keyWorkRecords.add(keyWorkRecord);
        }
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getHtmlAndKeyWorks(List<KeyWork> keyWorksArg)
  {
    getKeyWorks(keyWorksArg);
    return getHtmlFromEditor(he.getHtmlText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setFromMainText(MainText mainText)
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

  void set(HDT_RecordWithConnector record, String html, List<DisplayItem> displayItems, List<KeyWork> keyWorks)
  {
    String keyWorksText = "";
    curRecord = record;
    disableCache(taKeyWorks);

    clearDisplayItems();

    if ((keyWorks != null) && MainText.typeHasKeyWorks(record.getType()))
    {
      if (borderPane.getTop() == null)
        borderPane.setTop(tpKeyWorks);

      tpKeyWorks.setExpanded((keyWorks.size() > 0) || (record.getType() != hdtPerson));

      Map<String, String> linkMap = new HashMap<>();
      List<String> searchKeys = new ArrayList<>();

      keyWorks.forEach(keyWork ->
      {
        String searchKey = keyWork.getSearchKey(true);

        linkMap.put(searchKey, keyWork.getEditorText());

        addToSortedList(searchKeys, searchKey, String::compareToIgnoreCase);
      });

      keyWorksText = searchKeys.stream().map(linkMap::get).reduce((s1, s2) -> s1 + ", " + s2).orElse("");
    }
    else
      if (borderPane.getTop() == tpKeyWorks)
        borderPane.setTop(null);

    if (html.contains("body { font-family"))
      html = html.replace("body { font-family", "body { " + MARGIN_STYLE + " font-family");
    
    he.setHtmlText(disableLinks(html));
    taKeyWorks.setText(keyWorksText);

    if (hcbType.selectedType() == hdtNone)
    {
      RecordType type = db.parseTypeTagStr(appPrefs.get(PREF_KEY_DISPLAY_RECORD_TYPE, ""));

      hcbType.selectType(type == hdtNone ? hdtConcept : type);
    }

    hcbName.setChoicesChanged();

    if (hcbKeyType.selectedType() == hdtWork)
      hcbKeyName.setChoicesChanged();
    else
      hcbKeyType.selectType(hdtWork);

    if (displayItems == null) return;

    lvRecords.setItems(FXCollections.observableArrayList(displayItems));

    int descNdx = -1;
    for (int ndx = displayItems.size() - 1; ndx >= 0; ndx--)
    {
      DisplayItemType type = displayItems.get(ndx).type;

      if (type == diRecord)
      {
        if ((ndx + 1) < displayItems.size())
          lvRecords.getSelectionModel().select(ndx + 1);

        return;
      }
      else if (type == diDescription)
        descNdx = ndx;
    }

    if (descNdx >= 0)
      lvRecords.getSelectionModel().select(descNdx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
