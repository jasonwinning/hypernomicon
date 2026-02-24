/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.settings.shortcuts;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.hypernomicon.settings.shortcuts.Shortcut.ShortcutContext.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.settings.shortcuts.Shortcut.KeyCombo;
import org.hypernomicon.view.wrappers.SimpleSelector;

//---------------------------------------------------------------------------

public final class ShortcutEditorDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnOK, btnRemove;
  @FXML private CheckBox primaryCheck, altCheck, shiftCheck, secondaryCheck, superCheck;
  @FXML private ComboBox<KeyCode> cbKey;
  @FXML private Label conflictLabel, previewLabel, titleLabel;

  private final Shortcut initial;
  private final Set<Shortcut> existingShortcutsForConflictCheck;
  private boolean removeClicked = false;

//---------------------------------------------------------------------------

  public ShortcutEditorDlgCtrlr(Shortcut initial, Set<Shortcut> existingShortcutsForConflictCheck)
  {
    super("settings/shortcuts/ShortcutEditorDlg", "Edit Shortcut", false, true);

    this.initial = initial;

    titleLabel.setText("Edit shortcut for: " + initial.action().userReadableName + " (" + initial.context().userReadableName + ')');

    this.existingShortcutsForConflictCheck = existingShortcutsForConflictCheck;

    configurePlatformModifiers();

    SimpleSelector.init(cbKey, populateKeyChoices());

    bindPreview();

    if (initial.keyCombo() != null)
      applyInitial(initial.keyCombo());

    btnOK.disableProperty().bind(cbKey.getSelectionModel().selectedItemProperty().isNull());

    btnRemove.setDisable(initial.keyCombo() == null);

    btnRemove.setOnAction(event ->
    {
      if (confirmDialog("Remove the mapping for this shortcut?", false) == false) return;

      okClicked = true;
      removeClicked = true;
      stage.close();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void configurePlatformModifiers()
  {
    switch (CURRENT_OS)
    {
      case MAC:

        primaryCheck.setText("⌘ Command");
        altCheck.setText("⌥ Option");
        shiftCheck.setText("⇧ Shift");

        secondaryCheck.setText("⌃ Control");
        secondaryCheck.setVisible(true);
        secondaryCheck.setManaged(true);

        superCheck.setVisible(false);
        superCheck.setManaged(false);
        break;

      case LINUX:

        primaryCheck.setText("Ctrl");
        altCheck.setText("Alt");
        shiftCheck.setText("Shift");

        secondaryCheck.setVisible(false);
        secondaryCheck.setManaged(false);

        superCheck.setText("Super");
        superCheck.setVisible(true);
        superCheck.setManaged(true);
        break;

      case WINDOWS:  // fall through
      default:

        primaryCheck.setText("Ctrl");
        altCheck.setText("Alt");
        shiftCheck.setText("Shift");

        secondaryCheck.setVisible(false);
        secondaryCheck.setManaged(false);
        superCheck.setVisible(false);
        superCheck.setManaged(false);
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static SequencedMap<KeyCode, String> populateKeyChoices()
  {
    Collection<KeyCode> keys = new ArrayList<>();

    // Letters and numbers
    for (KeyCode code : KeyCode.values())
      if (code.isLetterKey() || (code.isDigitKey() && !code.isKeypadKey()))
        keys.add(code);

    // Function keys
    int fMax = IS_OS_MAC ? 19 : 24;
    for (int i = 1; i <= fMax; i++)
      keys.add(KeyCode.valueOf("F" + i));

    // Navigation / editing
    keys.addAll(List.of
    (
      KeyCode.ENTER, KeyCode.TAB, KeyCode.SPACE, KeyCode.ESCAPE,
      KeyCode.BACK_SPACE, KeyCode.DELETE, KeyCode.INSERT,
      KeyCode.HOME, KeyCode.END, KeyCode.PAGE_UP, KeyCode.PAGE_DOWN,
      KeyCode.LEFT, KeyCode.RIGHT, KeyCode.UP, KeyCode.DOWN
    ));

    // Numpad
    keys.addAll(List.of
    (
      KeyCode.NUMPAD0, KeyCode.NUMPAD1, KeyCode.NUMPAD2, KeyCode.NUMPAD3, KeyCode.NUMPAD4,
      KeyCode.NUMPAD5, KeyCode.NUMPAD6, KeyCode.NUMPAD7, KeyCode.NUMPAD8, KeyCode.NUMPAD9,
      KeyCode.ADD, KeyCode.SUBTRACT, KeyCode.MULTIPLY, KeyCode.DIVIDE, KeyCode.DECIMAL, KeyCode.ENTER
    ));

    // Punctuation / OEM-ish keys
    keys.addAll(List.of
    (
      KeyCode.BACK_QUOTE, KeyCode.MINUS, KeyCode.EQUALS,
      KeyCode.OPEN_BRACKET, KeyCode.CLOSE_BRACKET, KeyCode.BACK_SLASH,
      KeyCode.SEMICOLON, KeyCode.QUOTE, KeyCode.COMMA,
      KeyCode.PERIOD, KeyCode.SLASH
    ));

    Function<KeyCode, String> nameFunc = keyCode -> IS_OS_MAC ? Shortcut.normalizeKeyLabelForMac(keyCode, true) : keyCode.getName();

    return keys.stream().collect(Collectors.toMap(Function.identity(), nameFunc, (a, b) -> a, LinkedHashMap::new));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void bindPreview()
  {
    primaryCheck  .selectedProperty().addListener((obs, o, n) -> updatePreview());
    altCheck      .selectedProperty().addListener((obs, o, n) -> updatePreview());
    shiftCheck    .selectedProperty().addListener((obs, o, n) -> updatePreview());
    secondaryCheck.selectedProperty().addListener((obs, o, n) -> updatePreview());
    superCheck    .selectedProperty().addListener((obs, o, n) -> updatePreview());

    cbKey         .getSelectionModel().selectedItemProperty().addListener((obs, o, n) -> updatePreview());

    updatePreview();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updatePreview()
  {
    Shortcut shortcut = getShortcutFromUI();
    KeyCombo combo = shortcut.keyCombo();

    previewLabel.setText(combo == null ? "" : combo.toString());

    if (combo == null) return;

    if (reservedCombos().contains(combo))
    {
      conflictLabel.setText("Reserved by system");
      conflictLabel.setStyle("-fx-text-fill: -fx-accent;");
      return;
    }

    Shortcut conflict = findConflict(shortcut, builtInShortcuts());

    if (conflict != null)
    {
      conflictLabel.setText("Reserved by Hypernomicon");
      conflictLabel.setStyle("-fx-text-fill: -fx-accent;");
      return;
    }

    conflict = findConflict(shortcut, existingShortcutsForConflictCheck);

    if (conflict != null)
    {
      conflictLabel.setText("Already in use: " + conflict.action().userReadableName + " (" + conflict.context().userReadableName + ')');
      conflictLabel.setStyle("-fx-text-fill: -fx-accent;");
    }
    else
    {
      conflictLabel.setText("");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Shortcut findConflict(Shortcut needle, Iterable<Shortcut> haystack)
  {
    return (needle == null) || (haystack == null) ? null : findFirst(haystack, needle::conflictsWith);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void applyInitial(KeyCombo keyCombo)
  {
    primaryCheck  .setSelected(keyCombo.primary);
    altCheck      .setSelected(keyCombo.altOrOption);
    shiftCheck    .setSelected(keyCombo.shift);
    secondaryCheck.setSelected(keyCombo.secondaryCtrl);
    superCheck    .setSelected(keyCombo.superKey);

    if (keyCombo.keyCode != null)
      cbKey.getSelectionModel().select(keyCombo.keyCode);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Shortcut getShortcutFromUI()
  {
    KeyCombo combo = removeClicked ?
      null
    :
      new KeyCombo
      (
        cbKey.getSelectionModel().getSelectedItem(),

        primaryCheck  .isSelected(),
        altCheck      .isSelected(),
        shiftCheck    .isSelected(),
        secondaryCheck.isSelected(),
        superCheck    .isSelected()
      );

    return initial.copyWithNewKeyCombo(combo);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean isBareCharacter()
  {
    KeyCode k = cbKey.getSelectionModel().getSelectedItem();
    if (k == null)
      return false;

    boolean anyModifier = primaryCheck  .isSelected()
                       || altCheck      .isSelected()
                       || shiftCheck    .isSelected()
                       || secondaryCheck.isSelected()
                       || superCheck    .isSelected();

    if (anyModifier)
      return false;

    // Treat A-Z, 0-9, punctuation as "character".
    return isCharacterLike(k);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean isCharacterLike(KeyCode code)
  {
    if (code == null)
      return false;

    if (code.isLetterKey() || code.isDigitKey())
      return true;

    return List.of
    (
      KeyCode.BACK_QUOTE, KeyCode.MINUS, KeyCode.EQUALS,
      KeyCode.OPEN_BRACKET, KeyCode.CLOSE_BRACKET, KeyCode.BACK_SLASH,
      KeyCode.SEMICOLON, KeyCode.QUOTE, KeyCode.COMMA,
      KeyCode.PERIOD, KeyCode.SLASH

    ).contains(code);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Set<Shortcut> builtInShortcuts()
  {
    return switch (CURRENT_OS)
    {
      case MAC -> Set.of
      (
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.S            , true , false, false, false, false)), // Cmd+S
        new Shortcut(MainWindow, null, new KeyCombo(KeyCode.S            , true , false, true , false, false)), // Cmd+Shift+S
        new Shortcut(BibManager, null, new KeyCombo(KeyCode.S            , true , false, true , false, false)), // Cmd+Shift+S
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.F            , true , false, false, false, false)), // Cmd+F
        new Shortcut(MainWindow, null, new KeyCombo(KeyCode.F            , true , false, true , false, false)), // Cmd+Shift+F
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.G            , true , false, false, false, false)), // Cmd+G
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.G            , true , false, true , false, false)), // Cmd+Shift+G
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.DOWN         , false, true , false, false, false)), // Option+Down (dropdown menu)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.TAB          , false, false, false, true , false)), // Control+Tab (next tab)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.TAB          , false, false, true , true , false)), // Control+Shift+Tab (prev tab)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.Y            , true , false, false, false, false)), // Cmd+Y (history)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.OPEN_BRACKET , true , false, false, false, false)), // Cmd+[
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.CLOSE_BRACKET, true , false, false, false, false)), // Cmd+]
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.PLUS         , true , false, false, false, false)), // Cmd+Plus
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.MINUS        , true , false, false, false, false)), // Cmd+Hyphen
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.SUBTRACT     , true , false, false, false, false)), // Cmd+Subtract
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.EQUALS       , true , false, false, false, false)), // Cmd + '=' (Zoom in)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.V            , true , false, true , false, false)), // Cmd+Shift+V
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.V            , true , false, false, true , false))  // Cmd+Ctrl+V
      );

      default -> Set.of
      (
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.S       , true , false, false, false, false)), // Ctrl+S
        new Shortcut(MainWindow, null, new KeyCombo(KeyCode.S       , true , false, true , false, false)), // Ctrl+Shift+S
        new Shortcut(BibManager, null, new KeyCombo(KeyCode.S       , true , false, true , false, false)), // Ctrl+Shift+S
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.F       , true , false, false, false, false)), // Ctrl+F
        new Shortcut(MainWindow, null, new KeyCombo(KeyCode.F       , true , false, true , false, false)), // Ctrl+Shift+F
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.F3      , false, false, false, false, false)), // F3
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.F3      , false, false, true , false, false)), // Shift+F3
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.DOWN    , false, true , false, false, false)), // Alt+Down (dropdown menu)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.TAB     , true , false, false, false, false)), // Ctrl+Tab (next tab)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.TAB     , true , false, true , false, false)), // Ctrl+Shift+Tab (prev tab)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.H       , true , false, false, false, false)), // Ctrl+H (history)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.LEFT    , false, true , false, false, false)), // Alt+Left
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.RIGHT   , false, true , false, false, false)), // Alt+Right
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.PLUS    , true , false, false, false, false)), // Ctrl+Plus
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.MINUS   , true , false, false, false, false)), // Ctrl+Hyphen
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.SUBTRACT, true , false, false, false, false)), // Ctrl+Subtract
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.EQUALS  , true , false, false, false, false)), // Ctrl + '=' (Zoom in)
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.V       , true , false, true , false, false)), // Ctrl+Shift+V
        new Shortcut(AllWindows, null, new KeyCombo(KeyCode.V       , true , true , false, false, false))  // Ctrl+Alt+V
      );
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Set<KeyCombo> reservedCombos()
  {
    return switch (CURRENT_OS)
    {
      case MAC -> Set.of
      (
        new KeyCombo(KeyCode.TAB  , true, false, false, false, false), // Cmd+Tab
        new KeyCombo(KeyCode.SPACE, true, false, false, false, false), // Spotlight
        new KeyCombo(KeyCode.Q    , true, false, false, false, false),
        new KeyCombo(KeyCode.W    , true, false, false, false, false), // Quit/Close (optional to reserve)
        new KeyCombo(KeyCode.X    , true, false, false, false, false), // Cmd+X (Cut)
        new KeyCombo(KeyCode.C    , true, false, false, false, false), // Cmd+C (Copy)
        new KeyCombo(KeyCode.V    , true, false, false, false, false), // Cmd+V (Paste)
        new KeyCombo(KeyCode.Z    , true, false, false, false, false)  // Cmd+Z (Undo)
      );

      case LINUX -> Set.of
      (
        new KeyCombo(KeyCode.TAB   , false, true , false, false, false), // "Alt+Tab"
        new KeyCombo(KeyCode.F1    , true , true , false, false, false), // "Ctrl+Alt+F1"
        new KeyCombo(KeyCode.F2    , true , true , false, false, false), // "Ctrl+Alt+F2"
        new KeyCombo(KeyCode.F3    , true , true , false, false, false), // "Ctrl+Alt+F3"
        new KeyCombo(KeyCode.LEFT  , false, false, false, false, true ), // "Super+Left"
        new KeyCombo(KeyCode.RIGHT , false, false, false, false, true ), // "Super+Right"
        new KeyCombo(KeyCode.UP    , false, false, false, false, true ), // "Super+Up"
        new KeyCombo(KeyCode.DOWN  , false, false, false, false, true ), // "Super+Down"
        new KeyCombo(KeyCode.INSERT, true , false, false, false, false), // Ctrl+Insert (Copy)
        new KeyCombo(KeyCode.INSERT, false, false, true , false, false), // Shift+Insert (Paste)
        new KeyCombo(KeyCode.DELETE, false, false, true , false, false)  // Shift+Delete (Cut)
      );

      default -> Set.of
      (
        new KeyCombo(KeyCode.TAB   , false, true , false, false, false), // "Alt+Tab"
        new KeyCombo(KeyCode.DELETE, true , true , false, false, false), // "Ctrl+Alt+Del"
        new KeyCombo(KeyCode.LEFT  , false, false, false, false, true ), // "Win+Left"
        new KeyCombo(KeyCode.RIGHT , false, false, false, false, true ), // "Win+Right"
        new KeyCombo(KeyCode.UP    , false, false, false, false, true ), // "Win+Up"
        new KeyCombo(KeyCode.DOWN  , false, false, false, false, true ), // "Win+Down"
        new KeyCombo(KeyCode.INSERT, true , false, false, false, false), // Ctrl+Insert (Copy)
        new KeyCombo(KeyCode.INSERT, false, false, true , false, false), // Shift+Insert (Paste)
        new KeyCombo(KeyCode.DELETE, false, false, true , false, false)  // Shift+Delete (Cut)
      );
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (cbKey.getSelectionModel().selectedItemProperty() == null)
      return falseWithErrorPopup("Choose a key.", cbKey);

    // Prevent bare letter without any modifier (too invasive for normal typing).
    if (isBareCharacter())
      return falseWithErrorPopup("Add at least one modifier for character keys.");

    Shortcut shortcut = getShortcutFromUI();
    KeyCombo combo = shortcut.keyCombo();

    // Basic reserved/system-wide avoidance.
    if (reservedCombos().contains(combo))
      return falseWithErrorPopup("That shortcut is reserved by the system: " + combo);

    Shortcut conflict = findConflict(shortcut, builtInShortcuts());

    if (conflict != null)
      return falseWithErrorPopup("That shortcut is reserved by Hypernomicon: " + combo);

    // Cross-command conflict check.
    conflict = findConflict(shortcut, existingShortcutsForConflictCheck);

    if (conflict != null)
      return falseWithErrorPopup("That shortcut is already in use. " + conflict.action().userReadableName + " (" + conflict.context().userReadableName + "): " + combo);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
