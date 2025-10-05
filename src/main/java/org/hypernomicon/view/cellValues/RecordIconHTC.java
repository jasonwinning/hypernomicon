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

package org.hypernomicon.view.cellValues;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.MediaUtil.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public class RecordIconHTC extends RecordHTC
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String imgRelPath;  // should only ever be accessed by getImgRelPath

//---------------------------------------------------------------------------

  public RecordIconHTC(HDT_Record record)
  {
    super(record, "");
  }

  @Override public boolean isEmpty() { return (getRecord() == null) || (getRecordType() == RecordType.hdtNone); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getImgRelPath()
  {
    return imgRelPath == null ? (imgRelPath = safeStr(imgRelPath(getRecord(), getRecordType()))) : imgRelPath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Assumes at least one of the cells is a RecordIconHTC
   * @param cell1 First cell to be compared
   * @param cell2 Second cell to be compared
   * @return Usual return value for compare function
   */
  static int compareCells(HyperTableCell cell1, HyperTableCell cell2)
  {
    if (cell1 instanceof RecordIconHTC recordIconHTC1)
    {
      if (cell2 instanceof RecordIconHTC recordIconHTC2)
      {
        // Primary: type
        int result = recordIconHTC1.getRecordType().compareTo(recordIconHTC2.getRecordType());
        if (result != 0) return result;

        // Secondary: icon path
        result = safeStr(recordIconHTC1.getImgRelPath()).compareTo(safeStr(recordIconHTC2.getImgRelPath()));
        if (result != 0) return result;

        // Tertiary: ID
        return recordIconHTC1.getID() - recordIconHTC2.getID();
      }

      return 1;
    }

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
