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

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.MediaUtil;

//---------------------------------------------------------------------------

public class RecordHTC extends HyperTableCell
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final HyperTableCell trueCell  = new RecordHTC(TRUE_ID , "True" , hdtNone),
                                     falseCell = new RecordHTC(FALSE_ID, "False", hdtNone),
                                     unsetCell = new RecordHTC(UNSET_ID, "Unset", hdtNone),
                                     blankCell = new RecordHTC("", hdtNone);

  private int id;
  private String imgRelPath;  // should only ever be accessed by getImgRelPath

  private final String text;
  private final RecordType recordType;

  @Override public int getID()                { return id; }
  @Override public String getText()           { return text; }
  @Override public RecordType getRecordType() { return recordType; }

  @Override public RecordHTC clone() { return (RecordHTC) super.clone(); }

//---------------------------------------------------------------------------

  public RecordHTC(                   String text, RecordType recordType)    { this(-1            , text, recordType); }
  public RecordHTC(int id           , String text, RecordType recordType)    { this(id            , text, recordType, false); }
  public RecordHTC(HDT_Record record, String text                       )    { this(record.getID(), text, record.getType()); }

  public RecordHTC(String text, RecordType recordType, boolean sortToBottom) { this(-1, text, recordType, sortToBottom); }

  private RecordHTC(int id, String text, RecordType recordType, boolean sortToBottom)
  {
    super(sortToBottom);

    this.id = id;
    this.text = text;
    this.recordType = recordType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + id;
    result = prime * result + (text == null ? 0 : text.hashCode());
    result = prime * result + (recordType == null ? 0 : recordType.hashCode());
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    RecordHTC other = (RecordHTC) obj;

    if (getCellType(this) != getCellType(other))
    {
      if ((getCellType(this) == hdtAuxiliary) || (getCellType(other) == hdtAuxiliary))
        return false;

      if ((id < 0) && (other.getID() < 0))
        return safeStr(text).isEmpty() && safeStr(other.getText()).isEmpty();

      return false;
    }

    if (((id >= 0) || (other.id >= 0)) && (id != other.getID())) return false;

    return safeStr(text).equals(safeStr(other.getText()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getImgRelPath()
  {
    if (imgRelPath != null)
      return imgRelPath;

    return imgRelPath = safeStr(nullSwitch(getRecord(this), imgRelPathByType(recordType), MediaUtil::imgRelPath));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public RecordHTC getCopyWithID(int newID)
  {
    RecordHTC newCell = clone();
    newCell.id = newID;
    return newCell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
