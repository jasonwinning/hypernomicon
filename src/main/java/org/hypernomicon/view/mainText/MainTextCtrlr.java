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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.jsoup.nodes.Document;

import com.sun.javafx.webkit.Accessor;
import com.sun.webkit.WebPage;

import org.w3c.dom.html.HTMLAnchorElement;

import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.text.StringEscapeUtils;
import org.hypernomicon.App;
import org.hypernomicon.dialogs.InsertPictureDlgCtrlr;
import org.hypernomicon.dialogs.NewLinkDlgCtrlr;
import org.hypernomicon.dialogs.SearchKeySelectDlgCtrlr;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordWithAuthors;
import org.hypernomicon.model.records.HDT_RecordWithPath;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.KeyWork;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.model.unities.MainText.DisplayItem;
import org.hypernomicon.model.unities.MainText.DisplayItemType;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.controls.HiddenSidesPane;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperCB;

import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.application.Platform;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.event.ActionEvent;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
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
import javafx.scene.web.HTMLEditorSkin;
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

  private final WebView webView;
  private final WebEngine engine;
  private final Highlighter highlighter;
  private final HyperCB hcbType, hcbName, hcbKeyType, hcbKeyName;
  private final BooleanProperty prop; // Needs to be a member variable to prevent being garbage-collected

  private HDT_RecordWithMainText curRecord;
  private boolean ignoreKeyEvent = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  List<DisplayItem> getDisplayItems() { return lvRecords.getItems(); }
  int getScrollPos()                  { return webEngineScrollPos(engine); }
  WebEngine getEngine()               { return engine; }
  private void clearText()            { he.setHtmlText(prepHtmlForEditing("")); }
  BorderPane getRootNode()            { return borderPane; }
  void hilite()                       { highlighter.hilite(); }
  public void nextSearchResult()      { highlighter.nextSearchResult(); }
  public void previousSearchResult()  { highlighter.previousSearchResult(); }
  public void updateZoom()            { updateZoomFromPref(webView, PREF_KEY_MAINTEXT_ZOOM); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MainTextCtrlr() throws IOException
  {
    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/mainText/MainTextEditor.fxml"), null, null, klass -> this);

    loader.load();

    webView = (WebView) he.lookup("WebView");
    engine = webView.getEngine();
    webViewAddZoom(webView, PREF_KEY_MAINTEXT_ZOOM);
    highlighter = new Highlighter(engine);

    GridPane.setHgrow(webView, Priority.ALWAYS);
    GridPane.setVgrow(webView, Priority.ALWAYS);

    RecordTypePopulator rtp = new RecordTypePopulator(displayedTypesStream());

    hcbType = new HyperCB(cbType, ctDropDownList, rtp);
    hcbName = new HyperCB(cbName, ctDropDownList, new RecordByTypePopulator());

    hcbType.addListener((oldValue, newValue) ->
    {
      if (newValue == null) return;

      RecordType oldType = HyperTableCell.getCellType(oldValue),
                 newType = HyperTableCell.getCellType(newValue);

      if (oldType != newType)
      {
        app.prefs.put(PREF_KEY_DISPLAY_RECORD_TYPE, Tag.getTypeTagStr(newType));

        ((RecordByTypePopulator)hcbName.getPopulator()).setRecordType(newType);
        if (oldType != hdtNone)
          hcbName.selectID(-1);
      }
    });

    rtp = new RecordTypePopulator(hdtWork, hdtMiscFile);

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

    hcbKeyType.addListener((oldValue, newValue) ->
    {
      if (newValue == null) return;

      if (HyperTableCell.getCellType(oldValue) != HyperTableCell.getCellType(newValue))
      {
        ((RecordByTypePopulator)hcbKeyName.getPopulator()).setRecordType(HyperTableCell.getCellType(newValue));
        if (HyperTableCell.getCellType(oldValue) != hdtNone)
          hcbKeyName.selectID(-1);
      }
    });

    lvRecords.setCellFactory(listView -> new ListCell<>()
    {
      @Override protected void updateItem(DisplayItem item, boolean empty)
      {
        super.updateItem(item, empty);

        setText((empty || (item == null)) ? null : switch (item.type)
        {
          case diDescription -> "This record's description";
          case diKeyWorks    -> "Key works";
          case diRecord      -> getTypeName(item.record.getType()) + ": " + item.record.getCBText();
        });
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

    String shortcutKey, pasteNoLineBreaksKey;
    if (SystemUtils.IS_OS_MAC)
    {
      shortcutKey = "Command";
      pasteNoLineBreaksKey = "Ctrl-Command-V";
    }
    else
    {
      shortcutKey = "Ctrl";
      pasteNoLineBreaksKey = "Ctrl-Alt-V";
    }

    webView.setOnContextMenuRequested(contextMenuEvent ->
    {
      HTMLAnchorElement anchor = (HTMLAnchorElement) engine.executeScript("getAnchorAtCursor()");

      MenuItem menuItem1 = new MenuItem("Paste plain text (" + shortcutKey + "-Shift-V)");
      menuItem1.setOnAction(actionEvent -> pastePlainText(false));

      MenuItem menuItem2 = new MenuItem("Paste plain text without line breaks (" + pasteNoLineBreaksKey + ')');
      menuItem2.setOnAction(actionEvent -> pastePlainText(true));

      if (anchor == null)
      {
        setHTMLContextMenu(menuItem1, menuItem2);
        return;
      }

      MenuItem editLinkItem = new MenuItem("Edit link");
      editLinkItem.setOnAction(actionEvent -> editLink(anchor));
      setHTMLContextMenu(editLinkItem, menuItem1, menuItem2);
    });

    he.focusWithinProperty().addListener((obs, ov, nv) ->
    {
      if (Boolean.TRUE.equals(nv))
        highlighter.clear();
    });

    he.setOnMouseClicked (Event::consume);
    he.setOnMousePressed (Event::consume);
    he.setOnMouseReleased(Event::consume);

    // The next 2 event filters address buggy HTMLEditor handling of ctrl-B, ctrl-I, and ctrl-U

    he.addEventFilter(KeyEvent.KEY_PRESSED, event ->
    {
      highlighter.clear();

      if (shortcutKeyIsDown(event))
      {
        if (event.getCode() == KeyCode.V)
        {
          // On Mac, if you press V while alt is down, it will always insert a checkmark character

          if ((SystemUtils.IS_OS_MAC && event.isControlDown()) || ((SystemUtils.IS_OS_MAC == false) && event.isAltDown()))
          {
            pastePlainText(true);
            event.consume();
          }
        }
        else if ((event.getCode() == KeyCode.B) ||
                 (event.getCode() == KeyCode.I) ||
                 (event.getCode() == KeyCode.U))
        {
          if (ignoreKeyEvent)
            event.consume();
          else if (event.getCode() == KeyCode.B)
          {
            String selText = (String) engine.executeScript("window.getSelection().rangeCount < 1 ? \"\" : window.getSelection().getRangeAt(0).toString()");

            if (selText.isEmpty())
            {
              ((HTMLEditorSkin)he.getSkin()).performCommand(Command.BOLD);
              event.consume();
            }
          }

          ignoreKeyEvent = true;
        }
      }
    });

    he.addEventFilter(KeyEvent.KEY_RELEASED, event ->
    {
      if (shortcutKeyIsDown(event))
      {
        if ((event.getCode() == KeyCode.B) ||
            (event.getCode() == KeyCode.I) ||
            (event.getCode() == KeyCode.U))
        {
          event.consume();

          ignoreKeyEvent = false;
        }
      }
    });

    MenuItem menuItem0 = new MenuItem("Paste (" + shortcutKey + "-V)");
    menuItem0.setOnAction(event -> Accessor.getPageFor(engine).executeCommand(Command.PASTE.getCommand(), null));

    MenuItem menuItem1 = new MenuItem("Paste plain text (" + shortcutKey + "-Shift-V)");
    menuItem1.setOnAction(event -> pastePlainText(false));

    MenuItem menuItem2 = new MenuItem("Paste plain text without line breaks (" + pasteNoLineBreaksKey + ')');
    menuItem2.setOnAction(event -> pastePlainText(true));

    MenuButton btnPaste = new MenuButton("", imgViewFromRelPath("resources/images/page_paste.png"), menuItem0, menuItem1, menuItem2);
    setToolTip(btnPaste, "Paste");

    Button btnWebLink = new Button("", imgViewFromRelPath("resources/images/world_link.png"));
    setToolTip(btnWebLink, "Insert/edit web link");
    btnWebLink.setOnAction(event ->
    {
      HTMLAnchorElement anchor = (HTMLAnchorElement) engine.executeScript("getAnchorAtCursor()");

      if (anchor == null)
        btnLinkClick();
      else
        editLink(anchor);
    });

    Button btnKeywordLink = new Button("", imgViewFromRelPath("resources/images/keyword-link-add.png"));
    setToolTip(btnKeywordLink, "Insert a search key to link to a record");
    btnKeywordLink.setOnAction(event -> btnKeywordLinkClick());

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
    btnSubscript.setOnAction(event -> engine.executeScript("document.execCommand('subscript', false, '');"));

    Button btnSuperscript = new Button("", imgViewFromRelPath("resources/images/text_superscript.png"));
    setToolTip(btnSuperscript, "Toggle superscript for selected text");
    btnSuperscript.setOnAction(event -> engine.executeScript("document.execCommand('superscript', false, '');"));

    Button btnSymbol = new Button("", imgViewFromRelPath("resources/images/text_letter_omega.png"));
    setToolTip(btnSymbol, "Insert symbol at cursor");
    btnSymbol.setOnAction(event -> SymbolPickerDlgCtrlr.show());

    ObservableList<Node> topBarItems = ((ToolBar) he.lookup(".top-toolbar")).getItems();

    topBarItems.addAll(btnWebLink, btnKeywordLink, btnPicture, btnClear, btnEditLayout, btnSubscript, btnSuperscript, btnSymbol, btnPaste);

    topBarItems.addListener((Change<? extends Node> c) ->
    {
      while (c.next()) c.getAddedSubList().forEach(node ->
      {
        if (node instanceof Button button)
        {
          button.addEventFilter(ActionEvent.ACTION, event -> highlighter.clear()); // Make sure user can't copy text with highlighting to clipboard

          if (strListToSpaceDelimitedStr(button.getStyleClass()).contains("paste"))
            Platform.runLater(() -> topBarItems.remove(button));
        }
      });
    });

    menuItem0 = new MenuItem("Make this font the default for this database");
    menuItem0.setOnAction(event -> makeThisFontDefault());

    menuItem1 = new MenuItem("Set/update template for this record type");
    menuItem1.setOnAction(event -> updateTemplate());

    MenuButton btnSettings = new MenuButton("", imgViewFromRelPath("resources/images/gear-wrench.png"), menuItem0, menuItem1);
    setToolTip(btnSettings, "Settings");

    ObservableList<Node> bottomBarItems = ((ToolBar) he.lookup(".bottom-toolbar")).getItems();

    bottomBarItems.addListener((Change<? extends Node> ch) ->
    {
      if (ch.getList().size() == 3)
        Platform.runLater(() -> bottomBarItems.add(3, btnSettings));
    });

    he.setFocusTraversable(false);
  }

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
      webView.fireEvent(new MouseEvent(MouseEvent.MOUSE_PRESSED, 15, 100, 200, 200, MouseButton.PRIMARY, 1, false, false, false, false, false, false, false, false, false, false, null));
      he.requestFocus();
      webView.fireEvent(new MouseEvent(MouseEvent.MOUSE_RELEASED, 15, 100, 200, 200, MouseButton.PRIMARY, 1, false, false, false, false, false, false, false, false, false, false, null));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Stream<RecordType> displayedTypesStream()
  {
    return EnumSet.allOf(RecordType.class).stream().filter(type -> type.hasMainText() && (type != hdtHub) && (type != hdtWorkLabel));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void makeThisFontDefault()
  {
    boolean familyNotSet = false, sizeNotSet = false;
    WebPage webPage = Accessor.getPageFor(engine);

    String family = safeStr(webPage.queryCommandValue(Command.FONT_FAMILY.getCommand()));
    family = family.replace("'", "").replace("\"", "");

    if (family.length() > 0)
      db.prefs.put(PREF_KEY_DEF_DESC_FONT_FAMILY, family);
    else
      familyNotSet = true;

    String size = safeStr(webPage.queryCommandValue(Command.FONT_SIZE.getCommand()));

    if (size.length() > 0)
    {
      switch (size)
      {
        case "1" : size = "8pt" ; break;
        case "2" : size = "10pt"; break;
        case "3" : size = "12pt"; break;
        case "4" : size = "14pt"; break;
        case "5" : size = "18pt"; break;
        case "6" : size = "24pt"; break;
        case "7" : size = "36pt"; break;

        default  :                break;
      }

      db.prefs.put(PREF_KEY_DEF_DESC_FONT_SIZE, size);
    }
    else
      sizeNotSet = true;

    if ((familyNotSet == false) && (sizeNotSet == false)) return;

    String msg = "Unable to set default because font ";

    msg = msg + (familyNotSet ? (sizeNotSet ? "family and size" : "family") : "size");

    warningPopup(msg + " could not be determined");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateTemplate()
  {
    try
    {
      String templateHtml = getHtmlAndKeyWorks(new ArrayList<>());
      if (templateHtml.contains("hypernomiconHilite"))
        throw new HDB_InternalError(28468);

      db.updateMainTextTemplate(curRecord.getType(), templateHtml);
    }
    catch (IOException e)
    {
      errorPopup("An error occurred while saving to the template file: " + getThrowableMessage(e));
    }
    catch (HDB_InternalError e)
    {
      errorPopup(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnNewClick()
  {
    if (ui.cantSaveRecord()) return;

    RecordType keyType = hcbKeyType.selectedType();
    List<KeyWork> keyWorks = curRecord.getMainText().getKeyWorksCopy();

    HDT_RecordWithAuthors<? extends Authors> keyRecord = db.createNewBlankRecord(keyType);
    keyWorks.add(new KeyWork(keyRecord));

    curRecord.getMainText().setKeyWorksFromList(keyWorks);
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

    @SuppressWarnings("unchecked")
    KeyWork keyWork = new KeyWork((HDT_RecordWithAuthors<? extends Authors>) db.records(keyType).getByID(keyID));

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
    HDT_RecordWithMainText record = HyperTableCell.getRecord(hcbName.selectedHTC());
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

  private void btnKeywordLinkClick()
  {
    SearchKeySelectDlgCtrlr dlg = new SearchKeySelectDlgCtrlr();

    if (dlg.showModal())
      engine.executeScript("insertHtmlAtCursor('" + StringEscapeUtils.escapeEcmaScript(htmlEscaper.escape(dlg.getKeyword())) + "')");

    safeFocus(he);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnPictureClick()
  {
    new InsertPictureDlgCtrlr(miscFile ->
    {
      Accessor.getPageFor(engine).executeCommand(Command.INSERT_NEW_LINE.getCommand(), null);

      String imageTag = '<' + EMBEDDED_FILE_TAG + " id=\"" + miscFile.getID() + "\" width=\"300px\"/>";

      engine.executeScript("insertHtmlAtCursor('" + htmlEscaper.escape(imageTag) + "<br>')");

    }).showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnLinkClick()
  {
    String selText = (String) engine.executeScript("window.getSelection().rangeCount < 1 ? \"\" : window.getSelection().getRangeAt(0).toString()");

    NewLinkDlgCtrlr dlg = new NewLinkDlgCtrlr(convertToSingleLine(selText));

    if (dlg.showModal())
    {
      String urlText = StringEscapeUtils.escapeEcmaScript(htmlEscaper.escape(dlg.tfURL.getText().trim()));

      String anchorTag = "<a title=\"" + urlText + "\" href=\"" + urlText + "\">" + StringEscapeUtils.escapeEcmaScript(htmlEscaper.escape(dlg.tfDisplayText.getText())) + "</a>";

      engine.executeScript("insertHtmlAtCursor('" + anchorTag + "')");
    }

    safeFocus(he);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void editLink(HTMLAnchorElement anchor)
  {
    new NewLinkDlgCtrlr(anchor).showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pastePlainText(boolean noCarriageReturns)
  {
    String text = getClipboardText(noCarriageReturns);

    if (text.isEmpty()) return;

    text = htmlEscaper.escape(text);

    if (noCarriageReturns == false)
    {
      text = text.replaceAll("\\R", "<br>")
                 .replaceAll("\\v", "<br>");
    }

    engine.executeScript("insertHtmlAtCursor('" + StringEscapeUtils.escapeEcmaScript(text) + "')");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void getKeyWorks(List<KeyWork> keyWorksArg)
  {
    keyWorksArg.clear();

    Document subDoc = jsoupParse(taKeyWorks.getText());

    subDoc.getElementsByTag("a").forEach(aElement ->
    {
      int id = parseInt(aElement.attributes().get("id") , -1);
      RecordType type = Tag.parseTypeTagStr(aElement.attributes().get("type"));

      if ((id > 0) && (type != hdtNone))
      {
        nullSwitch((HDT_RecordWithPath) db.records(type).getByID(id), record ->
          keyWorksArg.add(new KeyWork(record.getType(), record.getID(), aElement.ownText(), true)));
      }

      aElement.remove();
    });

    Set<HDT_RecordWithPath> keyWorkRecords = new HashSet<>();
    String kwText = subDoc.text();

    KeywordLinkList.generate(kwText).forEach(link ->
    {
      HDT_Record record = link.key().record;

      if ((record.getType() == hdtWork) || (record.getType() == hdtMiscFile))
      {
        HDT_RecordWithPath keyWorkRecord = (HDT_RecordWithPath) record;

        if (keyWorkRecords.contains(keyWorkRecord) == false)
        {
          String str = kwText.substring(link.offset(), link.offset() + link.length());
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

    String markedHtml = he.getHtmlText();

    if (highlighter.neverHilited())
      return getHtmlFromEditor(markedHtml);

    highlighter.clear();

    String unmarkedHtml = getHtmlFromEditor(he.getHtmlText());
    he.setHtmlText(markedHtml);
    return unmarkedHtml;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setFromMainText(MainText mainText)
  {
    if (mainText != null)
      set(mainText.getRecord(), mainText.getHtml(), mainText.getDisplayItemsCopy(), mainText.getKeyWorksUnmod());
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

  void set(HDT_RecordWithMainText record, String html, List<DisplayItem> displayItems, List<KeyWork> keyWorks)
  {
    String keyWorksText = "";
    curRecord = record;
    disableCache(taKeyWorks);

    clearDisplayItems();

    if ((keyWorks != null) && (record.getType() != hdtInvestigation) && MainText.typeHasKeyWorks(record.getType()))
    {
      if (borderPane.getTop() == null)
        borderPane.setTop(tpKeyWorks);

      boolean dontOpenEmptyKeyWorks = db.prefs.getBoolean(PREF_KEY_DONT_OPEN_EMPTY_KEY_WORKS, false) || app.prefs.getBoolean(PREF_KEY_DONT_OPEN_EMPTY_KEY_WORKS, false);

      tpKeyWorks.setExpanded((keyWorks.size() > 0) || ((record.getType() != hdtPerson) && (dontOpenEmptyKeyWorks == false)));

      Map<String, String> linkMap = new HashMap<>();
      List<String> searchKeys = new ArrayList<>();

      keyWorks.forEach(keyWork ->
      {
        String searchKey = keyWork.getSearchKey(true);

        linkMap.put(searchKey, keyWork.getEditorText());

        addToSortedList(searchKeys, searchKey, String::compareToIgnoreCase);
      });

      keyWorksText = searchKeys.stream().map(linkMap::get).collect(Collectors.joining(", "));
    }
    else
      if (borderPane.getTop() == tpKeyWorks)
        borderPane.setTop(null);

    if (html.contains("body { font-family"))
      html = html.replace("body { font-family", "body { " + MARGIN_STYLE + " font-family");

    he.setHtmlText(prepHtmlForEditing(html));
    taKeyWorks.setText(keyWorksText);

    if (hcbType.selectedType() == hdtNone)
    {
      RecordType type = Tag.parseTypeTagStr(app.prefs.get(PREF_KEY_DISPLAY_RECORD_TYPE, ""));

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

      if (type == diDescription)
        descNdx = ndx;
    }

    if (descNdx >= 0)
      lvRecords.getSelectionModel().select(descNdx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
