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

package org.hypernomicon.tree;

public class HyperTreeCellValue implements Comparable<HyperTreeCellValue>
{
  final private TreeRow row;
  final private String key;

//---------------------------------------------------------------------------

  HyperTreeCellValue(TreeRow treeRow)
  {
    row = treeRow;
    key = makeKey();
  }

//---------------------------------------------------------------------------

  @Override public String toString()                       { return row.getName(); }
  @Override public int compareTo(HyperTreeCellValue other) { return key.compareTo(other.key); }
  @Override public int hashCode()                          { return key.hashCode(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    return compareTo((HyperTreeCellValue)obj) == 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String makeKey()
  {
    String prefix;

    switch (row.getRecordType())
    {
      case hdtDebate      : prefix = "JM."; break;
      case hdtPosition    : prefix = "KM."; break;
      case hdtArgument    : prefix = "LM."; break;
      case hdtWorkLabel   : prefix = "MM."; break;
      case hdtNote        : prefix = "NM."; break;
      case hdtWork        : prefix = "OM."; break;
      case hdtMiscFile    : prefix = "PM."; break;
      case hdtPersonGroup : prefix = "QM."; break;
      case hdtPerson      : prefix = "RM."; break;
      case hdtGlossary    : prefix = "SM."; break;
      case hdtConcept     : prefix = "TM."; break;
      default             : prefix = "ZM.";
    }

    return prefix + toString().toLowerCase();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
