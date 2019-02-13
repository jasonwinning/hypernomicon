/*
 * Copyright 2015-2019 Jason Winning
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

public class HyperTreeCellValue implements Comparable <HyperTreeCellValue>
{
  final private TreeRow row;
  final private String key;

//---------------------------------------------------------------------------

  HyperTreeCellValue(TreeRow treeRow)
  {
    this.row = treeRow;
    key = makeKey();
  }

//---------------------------------------------------------------------------

  @Override public String toString()                       { return row.getName(); }
  @Override public int compareTo(HyperTreeCellValue other) { return key.compareTo(other.key); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String makeKey()
  {
    String prefix = "ZM.";

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
