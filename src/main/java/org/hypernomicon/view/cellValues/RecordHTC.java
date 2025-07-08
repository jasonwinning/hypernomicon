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

import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.MediaUtil;

//---------------------------------------------------------------------------

public class RecordHTC extends AbstractHTC
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int id;
  private String imgRelPath;  // should only ever be accessed by getImgRelPath

  private final String text;
  private final RecordType recordType;

  @Override public int getID()                { return id; }
  @Override public String getText()           { return text; }
  @Override public RecordType getRecordType() { return recordType; }

  @Override public RecordHTC clone() { return (RecordHTC) super.clone(); }

//---------------------------------------------------------------------------

  public RecordHTC(int id           , String text, RecordType recordType)    { this(id            , text, recordType, false); }
  public RecordHTC(HDT_Record record, String text                       )    { this(record.getID(), text, record.getType()); }

  protected RecordHTC(int id, String text, RecordType recordType, boolean sortToBottom)
  {
    super(sortToBottom);

    this.id = id;
    this.text = text;
    this.recordType = recordType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getImgRelPath()
  {
    if (imgRelPath != null)
      return imgRelPath;

    return imgRelPath = safeStr(nullSwitch(getRecord(), imgRelPathByType(recordType), MediaUtil::imgRelPath));
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
