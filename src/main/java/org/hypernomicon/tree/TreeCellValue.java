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

package org.hypernomicon.tree;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;

import static org.hypernomicon.model.records.RecordType.*;

class TreeCellValue implements Comparable<TreeCellValue>
{
  private final TreeRow row;
  private final String key;

//---------------------------------------------------------------------------

  TreeCellValue(TreeRow treeRow)
  {
    row = treeRow;
    key = makeKey();
  }

//---------------------------------------------------------------------------

  @Override public String toString() { return row.getName(); }
  @Override public int hashCode()    { return key.hashCode(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(TreeCellValue other)
  {
    HDT_Record record1 = row == null ? null : row.getRecord();
    HDT_Record record2 = other.row == null ? null : other.row.getRecord();

    if ((HDT_Record.isEmpty(record1) == false) && (HDT_Record.isEmpty(record2) == false))
    {
      if ((record1.getType() == hdtWork) && (record2.getType() == hdtWork))
        return ((HDT_Work)record1).compareTo((HDT_Work)record2);
    }

    return key.compareTo(other.key);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    return compareTo((TreeCellValue)obj) == 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String makeKey()
  {
    String prefix = switch (row.getRecordType())
    {
      case hdtDebate      -> "JM.";
      case hdtPosition    -> "KM.";
      case hdtArgument    -> "LM.";
      case hdtWorkLabel   -> "MM.";
      case hdtNote        -> "NM.";
      case hdtWork        -> "OM.";
      case hdtMiscFile    -> "PM.";
      case hdtPersonGroup -> "QM.";
      case hdtPerson      -> "RM.";
      case hdtGlossary    -> "SM.";
      case hdtConcept     -> "TM.";
      default             -> "ZM.";
    };

    return prefix + toString().toLowerCase();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
