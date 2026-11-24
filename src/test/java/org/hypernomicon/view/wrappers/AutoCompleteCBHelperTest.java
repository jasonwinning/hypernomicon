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

package org.hypernomicon.view.wrappers;

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.model.records.RecordType.*;

import java.util.List;

import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import org.hypernomicon.util.FxTestUtil;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;

//---------------------------------------------------------------------------

/**
 * How a combo box's autocompletion responds to key releases. The key events are fired at a combo box
 * with no scene, after setting the editor's text and selection the way the key presses would have.
 */
class AutoCompleteCBHelperTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ComboBox<HyperTableCell> cb;
  private AutoCompleteCBHelper helper;

//---------------------------------------------------------------------------

  @BeforeAll
  static void initJfx()
  {
    FxTestUtil.initJfx();
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void createComboBox()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      cb = new ComboBox<>();
      cb.getItems().setAll(List.of(new GenericNonRecordHTC("Smith, John", hdtNone), new GenericNonRecordHTC("Doe, Jane", hdtNone)));
      helper = new AutoCompleteCBHelper(cb, true, text -> new GenericNonRecordHTC(text, hdtNone));
    });
  }

//---------------------------------------------------------------------------

  private TextField editor() { return cb.getEditor(); }

  private void release(KeyCode code) { cb.fireEvent(new KeyEvent(KeyEvent.KEY_RELEASED, KeyEvent.CHAR_UNDEFINED, "", code, false, false, false, false)); }

  // Ctrl and Meta both, so that the shortcut modifier is held on every platform

  private void releaseWithShortcut(KeyCode code) { cb.fireEvent(new KeyEvent(KeyEvent.KEY_RELEASED, KeyEvent.CHAR_UNDEFINED, "", code, false, true, false, true)); }

  /** Leaves the editor holding text typed with the caret at its end, as the press of its last key would, then releases that key */
  private void type(String text, KeyCode lastKey)
  {
    editor().setText(text);
    editor().positionCaret(text.length());
    release(lastKey);
  }

//---------------------------------------------------------------------------

  /** Releasing a key that types nothing does not bring back a completion the user deleted */
  @ParameterizedTest
  @EnumSource(value = KeyCode.class, names = { "SHIFT", "CONTROL", "ALT", "META", "CAPS", "F2" })
  void releasingKeyThatTypesNothingKeepsDeletedCompletionAway(KeyCode code)
  {
    FxTestUtil.runFxAndWait(() ->
    {
      type("smi", KeyCode.I);
      assertEquals("smith, John", editor().getText(), "typing a prefix autocompletes");

      editor().replaceSelection("");  // Backspace deletes the selected completion
      release(KeyCode.BACK_SPACE);

      release(code);

      assertEquals("smi", editor().getText());
      assertNull(helper.getTypedMatch());
    });
  }

//---------------------------------------------------------------------------

  /** When Ctrl+V's V is let go first, letting go of Ctrl autocompletes the pasted text */
  @Test
  void releasingModifierAfterPasteAutocompletes()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      editor().replaceSelection("smi");  // Ctrl+V pastes into the empty editor
      releaseWithShortcut(KeyCode.V);
      assertEquals("smi", editor().getText(), "a shortcut's own key is not handled as typing");

      release(KeyCode.CONTROL);

      assertEquals("smith, John", editor().getText());
      assertEquals("Smith, John", HyperTableCell.getCellText(helper.getTypedMatch()));
    });
  }

//---------------------------------------------------------------------------

  /** After a shortcut that deleted text, like Ctrl+Backspace, letting go of Ctrl does not autocomplete again */
  @Test
  void releasingModifierAfterShortcutDeletionKeepsCompletionAway()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      type("smi", KeyCode.I);

      editor().replaceSelection("");  // Ctrl+Backspace deletes the selected completion
      releaseWithShortcut(KeyCode.BACK_SPACE);
      release(KeyCode.CONTROL);

      assertEquals("smi", editor().getText());
      assertNull(helper.getTypedMatch());
    });
  }

//---------------------------------------------------------------------------

  /** Moving the caret and then tapping Shift leaves the text after the caret alone */
  @Test
  void releasingShiftAfterCaretMoveChangesNothing()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      editor().setText("Smith, Jo");
      editor().positionCaret(0);
      release(KeyCode.HOME);

      editor().positionCaret(5);  // A click puts the caret after "Smith"
      release(KeyCode.SHIFT);

      assertEquals("Smith, Jo", editor().getText());
      assertEquals(0, editor().getSelection().getLength());
    });
  }

//---------------------------------------------------------------------------

}
