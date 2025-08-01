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

package org.hypernomicon.view.mainText;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.jsoup.nodes.Document;

import com.sun.javafx.webkit.Accessor;
import com.sun.webkit.WebPage;

import org.w3c.dom.html.HTMLAnchorElement;

import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.text.StringEscapeUtils;

import org.hypernomicon.App;
import org.hypernomicon.dialogs.*;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.authors.RecordAuthors;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.*;
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
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.robot.Robot;
import javafx.scene.web.*;
import javafx.scene.web.HTMLEditorSkin.Command;

//---------------------------------------------------------------------------

public class MainTextCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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

  List<DisplayItem> getDisplayItems() { return lvRecords.getItems(); }
  int getScrollPos()                  { return webEngineScrollPos(engine); }
  WebEngine getEngine()               { return engine; }
  private void clearText()            { he.setHtmlText(prepHtmlForEditing("")); }
  BorderPane getRootNode()            { return borderPane; }
  void hilite()                       { highlighter.hilite(); }
  public void nextSearchResult()      { highlighter.nextSearchResult(); }
  public void previousSearchResult()  { highlighter.previousSearchResult(); }
  public void updateZoom()            { updateZoomFromPref(webView, ZoomPrefKey.MAINTEXT); }

//---------------------------------------------------------------------------

  MainTextCtrlr() throws IOException
  {
    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/mainText/MainTextEditor.fxml"), null, null, klass -> this);

    loader.load();

    webView = (WebView) he.lookup(".web-view");
    engine = webView.getEngine();
    webViewAddZoom(webView, ZoomPrefKey.MAINTEXT);
    highlighter = new Highlighter(engine);

    GridPane.setHgrow(webView, Priority.ALWAYS);
    GridPane.setVgrow(webView, Priority.ALWAYS);

    RecordTypePopulator rtp = new RecordTypePopulator(displayedTypesStream());

    hcbType = new HyperCB(cbType, ctEditableLimitedDropDown, rtp);
    hcbName = new HyperCB(cbName, ctEditableLimitedDropDown, new RecordByTypePopulator());

    hcbType.addListener((oldValue, newValue) ->
    {
      if (newValue == null) return;

      RecordType oldType = HyperTableCell.getCellType(oldValue),
                 newType = HyperTableCell.getCellType(newValue);

      if (oldType != newType)
      {
        app.prefs.put(PrefKey.DISPLAY_RECORD_TYPE, Tag.getTypeTagStr(newType));

        ((RecordByTypePopulator)hcbName.getPopulator()).setRecordType(newType);
        if (oldType != hdtNone)
          hcbName.selectID(-1);
      }
    });

    rtp = new RecordTypePopulator(hdtWork, hdtMiscFile);

    hcbKeyType = new HyperCB(cbKeyType, ctEditableLimitedDropDown, rtp);
    hcbKeyName = new HyperCB(cbKeyName, ctEditableLimitedDropDown, new RecordByTypePopulator());

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

      MenuItem mnuPaste                  = createPasteMenuItem                 (shortcutKey),
               mnuPastePlain             = createPastePlainMenuItem            (shortcutKey),
               mnuPastePlainNoLineBreaks = createPastePlainNoLineBreaksMenuItem(pasteNoLineBreaksKey);

      if (anchor == null)
      {
        setHTMLContextMenu(mnuPaste, mnuPastePlain, mnuPastePlainNoLineBreaks);
        return;
      }

      MenuItem editLinkItem = new MenuItem("Edit link");
      editLinkItem.setOnAction(actionEvent -> editLink(anchor));
      setHTMLContextMenu(editLinkItem, mnuPaste, mnuPastePlain, mnuPastePlainNoLineBreaks);
    });

    he.focusWithinProperty().addListener((obs, ov, nv) ->
    {
      if (Boolean.TRUE.equals(nv))
        highlighter.clear();
    });

    he.setOnMouseClicked (Event::consume);
    he.setOnMousePressed (Event::consume);
    he.setOnMouseReleased(Event::consume);

    // The next 2 event filters address buggy HTMLEditor handling of ctrl-B, ctrl-I, and ctrl-U, and other bugginess

    he.addEventFilter(KeyEvent.KEY_PRESSED, this::keyPressFilter);

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

    MenuItem mnuPaste                  = createPasteMenuItem                 (shortcutKey),
             mnuPastePlain             = createPastePlainMenuItem            (shortcutKey),
             mnuPastePlainNoLineBreaks = createPastePlainNoLineBreaksMenuItem(pasteNoLineBreaksKey);

    MenuButton btnPaste = new MenuButton("", imgViewFromRelPath("resources/images/page_paste.png"), mnuPaste, mnuPastePlain, mnuPastePlainNoLineBreaks);
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

    MenuItem mnuMakeFontDefault = new MenuItem("Make this font the default for this database");
    mnuMakeFontDefault.setOnAction(event -> makeThisFontDefault());

    MenuItem mnuUpdateTemplate = new MenuItem("Set/update template for this record type");
    mnuUpdateTemplate.setOnAction(event -> updateTemplate());

    MenuButton btnSettings = new MenuButton("", imgViewFromRelPath("resources/images/gear-wrench.png"), mnuMakeFontDefault, mnuUpdateTemplate);
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

  /**
   * Determines whether the given {@link KeyEvent} represents a non-printing key or key combination.
   * <p>
   * A non-printing key or key combination is one that does not directly change the text that is
   * selected by overwriting it. This includes modifier keys (e.g. Ctrl, Alt), navigation keys
   * (e.g. arrows, Home, End), function keys (e.g. F1–F12), media keys, and special keys
   * such as Escape, Insert, and PrintScreen.
   * <p>
   * The method also accounts for platform-specific modifier keys (e.g. Meta, Command, Alt),
   * dead keys used in international input, and cases where Shift is pressed but no printable
   * character is produced (e.g. Shift+Delete).
   *
   * @param event the {@link KeyEvent} to evaluate
   * @return {@code true} if the event corresponds to a non-printing key; {@code false} otherwise
   */
  private static boolean keyEventIsNonPrinting(KeyEvent event)
  {
    KeyCode code = event.getCode();

    boolean hasModifier = event.isAltDown() || event.isControlDown() || event.isMetaDown();

    boolean isSpecialKey = code.isModifierKey() || code.isNavigationKey() ||
                           code.isFunctionKey() || code.isMediaKey     () ||
                           code.getName().startsWith("Dead");

    boolean isExplicitNonPrinting = switch (code)
    {
      case INSERT, ESCAPE, CAPS, NUM_LOCK, SCROLL_LOCK, PRINTSCREEN,
           PAUSE, CONTEXT_MENU, HELP, WINDOWS, COMMAND, SHORTCUT, UNDEFINED -> true;

      default -> false;
    };

    // Covers cases where Shift is pressed but no printable character is produced (e.g. Shift+Delete)
    boolean noTextWithShift = (event.getText().isEmpty() || (code == KeyCode.TAB)) && event.isShiftDown();

    return hasModifier || isSpecialKey || isExplicitNonPrinting || noTextWithShift;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern LI_PATTERN = Pattern.compile("<li\\b", Pattern.CASE_INSENSITIVE);

  private static final String selectionScript =
      "(() => {"
    + "  const sel = window.getSelection();"
    + "  if (!sel.rangeCount) return '';"
    + "  const frag = document.createElement('div');"
    + "  frag.appendChild(sel.getRangeAt(0).cloneContents());"
    + "  return frag.innerHTML;"
    + "})()";

  private void keyPressFilter(KeyEvent event)
  {
    // Check for non-printing keys or key combinations

    if (keyEventIsNonPrinting(event))
    {
      String selectedHtml = (String) engine.executeScript(selectionScript),
             selectedText = jsoupParse(selectedHtml).wholeText();

      // Check for a leading blank line

      if (selectedText.startsWith("\n"))
      {
        // Count <li> occurrences

        Matcher liMatcher = LI_PATTERN.matcher(selectedHtml);
        int liCount = 0;

        while (liMatcher.find())
        {
          liCount++;
          if (liCount == 2) break;
        }

        // Consume if blank line + 2 or more list items

        if (liCount == 2)
        {
          // Fix for https://sourceforge.net/p/hypernomicon/tickets/82/

          // This is a workaround for a bug in HTMLEditorSkin. There is an event handler for
          // key press events that begins as follows:
          //
          //   webView.addEventHandler(KeyEvent.KEY_PRESSED, event -> {
          //     applyTextFormatting();
          //
          // applyTextFormatting takes what is entered in the Format dropdown, which usually
          // has "Paragraph" selected, and applies that formatting to the selected text. This
          // causes the selected list items to get all messed up if (a) the selection starts
          // with a blank line, and (b) the selection contains 2 or more list items. So we just
          // consume the event if the event is not going to overwrite the selection.

          event.consume();

          if (event.isShortcutDown() && ((event.getCode() == KeyCode.C) || (event.getCode() == KeyCode.INSERT)))
            Accessor.getPageFor(engine).executeCommand(Command.COPY.getCommand(), null);

          return;
        }
      }
    }

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
      db.prefs.put(PrefKey.DEF_DESC_FONT_FAMILY, family);
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

      db.prefs.put(PrefKey.DEF_DESC_FONT_SIZE, size);
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

    HDT_RecordWithAuthors<? extends RecordAuthors> keyRecord = db.createNewBlankRecord(keyType);
    keyWorks.add(new KeyWork(keyRecord));

    curRecord.getMainText().setKeyWorksFromList(keyWorks);
    ui.goToRecord(keyRecord, false);

    if (keyType == hdtWork)
    {
      if (ui.workHyperTab().showWorkDialog(null, false) == false)
        ui.deleteCurrentRecord(false);
    }
    else
    {
      if (ui.fileHyperTab().showFileDialog(null, false) == false)
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
    KeyWork keyWork = new KeyWork((HDT_RecordWithAuthors<? extends RecordAuthors>) db.records(keyType).getByID(keyID));

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
      String urlText = StringEscapeUtils.escapeEcmaScript(htmlEscaper.escape(dlg.tfURL.getText().strip()));

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

  private MenuItem createPasteMenuItem(String shortcutKey)
  {
    MenuItem menuItem = new MenuItem("Paste (" + shortcutKey + "-V)");
    menuItem.setOnAction(event -> Accessor.getPageFor(engine).executeCommand(Command.PASTE.getCommand(), null));
    return menuItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private MenuItem createPastePlainMenuItem(String shortcutKey)
  {
    MenuItem menuItem = new MenuItem("Paste plain text (" + shortcutKey + "-Shift-V)");
    menuItem.setOnAction(actionEvent -> pastePlainText(false));
    return menuItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private MenuItem createPastePlainNoLineBreaksMenuItem(String pasteNoLineBreaksKey)
  {
    MenuItem menuItem = new MenuItem("Paste plain text without line breaks (" + pasteNoLineBreaksKey + ')');
    menuItem.setOnAction(actionEvent -> pastePlainText(true));
    return menuItem;
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
    nullSwitch(getCBListView(cbKeyName), lv -> lv.scrollTo(0));
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

      boolean dontOpenEmptyKeyWorks = db.prefs.getBoolean(PrefKey.DONT_OPEN_EMPTY_KEY_WORKS, false) || app.prefs.getBoolean(PrefKey.DONT_OPEN_EMPTY_KEY_WORKS, false);

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
      RecordType type = Tag.parseTypeTagStr(app.prefs.get(PrefKey.DISPLAY_RECORD_TYPE, ""));

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
