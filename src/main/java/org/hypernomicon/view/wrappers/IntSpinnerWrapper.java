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

import static org.hypernomicon.util.Util.*;

import javafx.scene.control.CheckBox;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory.IntegerSpinnerValueFactory;

//---------------------------------------------------------------------------

/**
 * Sets up an editable, range-bounded integer {@link Spinner} (typed values are committed and clamped on
 * focus loss), optionally paired with a "none" {@link CheckBox} that disables the spinner and stands in for a
 * sentinel value such as {@code -1} (never) or {@code 0} (no limit). Read the current value, sentinel-aware,
 * with {@link #getValue()}.
 */
public class IntSpinnerWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Spinner<Integer> spinner;
  private final CheckBox noneChk;   // null when there is no "none" option
  private final int sentinel;       // getValue() returns this while noneChk is selected

  private int lastValidValue;       // last in-range value; the fallback when the editor is left blank (spinner value null)

//---------------------------------------------------------------------------

  private IntSpinnerWrapper(Spinner<Integer> spinner, CheckBox noneChk, int sentinel)
  {
    this.spinner  = spinner;
    this.noneChk  = noneChk;
    this.sentinel = sentinel;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets up a plain editable integer spinner over {@code [min, max]} with the given initial value (clamped
   * into range). {@code onChange} runs whenever the committed value changes.
   */
  public static IntSpinnerWrapper of(Spinner<Integer> spinner, int min, int max, int initial, Runnable onChange)
  {
    IntSpinnerWrapper wrapper = new IntSpinnerWrapper(spinner, null, 0);
    wrapper.init(min, max, initial, onChange);
    return wrapper;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets up an editable integer spinner paired with a "none" checkbox: while {@code noneChk} is selected the
   * spinner is disabled and {@link #getValue()} returns {@code sentinel}; otherwise it returns the spinner's
   * value. The spinner keeps its last numeric value while disabled, so clearing the checkbox restores it.
   * {@code onChange} runs on both spinner-value and checkbox changes.
   */
  public static IntSpinnerWrapper withNone(Spinner<Integer> spinner, int min, int max, int initial,
                                           CheckBox noneChk, int sentinel, boolean noneSelected, Runnable onChange)
  {
    IntSpinnerWrapper wrapper = new IntSpinnerWrapper(spinner, noneChk, sentinel);
    wrapper.init(min, max, initial, onChange);

    noneChk.setSelected(noneSelected);
    spinner.disableProperty().bind(noneChk.selectedProperty());
    noneChk.selectedProperty().addListener((ob, ov, nv) -> onChange.run());

    return wrapper;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(int min, int max, int initial, Runnable onChange)
  {
    spinner.setValueFactory(new IntegerSpinnerValueFactory(min, max, Math.clamp(initial, min, max)));
    spinner.setEditable(true);
    commitOnFocusLoss();

    spinner.valueProperty().addListener((ob, ov, nv) ->
    {
      if (nv == null) return;

      // IntegerSpinnerValueFactory does not clamp setValue, so an editable commit (including JavaFX's own
      // commit-on-focus-loss) can push a typed value outside [min, max]. Re-clamp before anything reads it, so
      // onChange, getValue(), and any persisted value never see an out-of-range number.

      int clamped = Math.clamp(nv, min, max);

      if (clamped != nv)
      {
        spinner.getValueFactory().setValue(clamped);  // re-fires this listener with the in-range value
        return;
      }

      onChange.run();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Makes the editable spinner commit and validate its editor on focus loss: the typed text is parsed and
   * clamped to the value factory's range, reverting to the last in-range value if it is blank or not a number.
   * Without this, JavaFX neither commits a typed value nor restores a cleared editor on focus loss.
   */
  private void commitOnFocusLoss()
  {
    IntegerSpinnerValueFactory vf = (IntegerSpinnerValueFactory) spinner.getValueFactory();

    // Track the last in-range value. Clearing the editor makes JavaFX commit the factory's value to null (its
    // converter maps "" to null), losing the previous value; on a blank or invalid commit, and in getValue(), we
    // revert to this remembered value rather than reading the (possibly null) current value or dropping to the minimum.

    lastValidValue = Math.clamp(nullSwitch(vf.getValue(), vf.getMin()), vf.getMin(), vf.getMax());

    vf.valueProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        lastValidValue = Math.clamp(newValue, vf.getMin(), vf.getMax());
    });

    // Listen on the editor, the real focus owner for an editable spinner, so focus loss is detected reliably

    spinner.getEditor().focusedProperty().addListener((ob, wasFocused, isFocused) ->
    {
      if (isFocused) return;  // act only on focus loss

      // If blank or non-numeric, parseInt falls back to the last good value (already in range, so clamping it is a no-op)

      int value = Math.clamp(parseInt(spinner.getEditor().getText().strip(), lastValidValue), vf.getMin(), vf.getMax());

      vf.setValue(value);                                             // re-fires value listeners (e.g. to persist)
      spinner.getEditor().setText(vf.getConverter().toString(value)); // sync the displayed text
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * @return the sentinel value while the "none" checkbox (if any) is selected, otherwise the spinner's value
   *         (or the last in-range value if the editor was left blank)
   */
  public int getValue()
  {
    return ((noneChk != null) && noneChk.isSelected()) ? sentinel : nullSwitch(spinner.getValue(), lastValidValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
