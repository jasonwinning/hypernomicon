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

package org.hypernomicon.view.cellValues;

//---------------------------------------------------------------------------

public class ObjectCellValue<Comp_T extends Comparable<Comp_T>> implements Comparable<ObjectCellValue<Comp_T>>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String text;
  private final Comparable<Comp_T> sortVal;

//---------------------------------------------------------------------------

  public ObjectCellValue(String text, Comparable<Comp_T> sortVal)
  {
    this.text = text;
    this.sortVal = sortVal;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString() { return text; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public int compareTo(ObjectCellValue<Comp_T> other)
  {
    return sortVal.compareTo((Comp_T) other.sortVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((sortVal == null) ? 0 : sortVal.hashCode());
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    if (sortVal.getClass() != ((ObjectCellValue<Comp_T>) obj).sortVal.getClass()) return false;

    ObjectCellValue<Comp_T> other = (ObjectCellValue<Comp_T>)obj;
    return sortVal.equals(other.sortVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
