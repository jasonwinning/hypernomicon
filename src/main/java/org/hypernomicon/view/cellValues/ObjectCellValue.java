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

package org.hypernomicon.view.cellValues;

import java.util.Objects;

//---------------------------------------------------------------------------

/**
 * A cell value for JavaFX table columns where the display text differs from the
 * underlying sortable value. For example, a file size column displays "42 KB" but
 * sorts numerically by the raw byte count, and a date column displays a formatted
 * string but sorts by the underlying {@link java.time.Instant}.
 * <p>
 * Equality and hashing are based on both fields. Sorting is by {@code value} first,
 * with {@code text} as a tiebreaker to ensure stable ordering when distinct display
 * representations share the same underlying value (e.g., directories showing "" vs
 * empty files showing "0 bytes").
 *
 * @param <Comp_T> the type of the underlying comparable value
 */
public class ObjectCellValue<Comp_T extends Comparable<Comp_T>> implements Comparable<ObjectCellValue<Comp_T>>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String text;
  private final Comparable<Comp_T> value;

//---------------------------------------------------------------------------

  public Comparable<Comp_T> getValue() { return value; }

//---------------------------------------------------------------------------

  public ObjectCellValue(String text, Comparable<Comp_T> value)
  {
    this.text  = Objects.requireNonNull(text);
    this.value = Objects.requireNonNull(value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString() { return text; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public int compareTo(ObjectCellValue<Comp_T> other)
  {
    int result = value.compareTo((Comp_T) other.value);
    return result != 0 ? result : text.compareTo(other.text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    return Objects.hash(text, value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    ObjectCellValue<Comp_T> other = (ObjectCellValue<Comp_T>)obj;
    return text.equals(other.text) && value.equals(other.value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
