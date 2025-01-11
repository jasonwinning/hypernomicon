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

import static org.hypernomicon.model.records.RecordType.hdtNone;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public class GenericNonRecordHTC extends RecordHTC
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final HyperTableCell trueCell  = new GenericNonRecordHTC(TRUE_ID , "True" , hdtNone),
                                     falseCell = new GenericNonRecordHTC(FALSE_ID, "False", hdtNone),
                                     unsetCell = new GenericNonRecordHTC(UNSET_ID, "Unset", hdtNone),
                                     blankCell = new GenericNonRecordHTC("", hdtNone);

//---------------------------------------------------------------------------

  public GenericNonRecordHTC(String text, RecordType recordType)
  {
    super(-1, text, recordType);
  }

  public GenericNonRecordHTC(int id, String text, RecordType recordType)
  {
    super(id, text, recordType);
  }

  public GenericNonRecordHTC(String text, RecordType recordType, boolean sortToBottom)
  {
    super(-1, text, recordType, sortToBottom);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public <HDT_T extends HDT_Record> HDT_T getRecord() { return null; }

  public static boolean isEmpty(HyperTableCell cell)        { return (cell == null) || blankCell.equals(cell); }
  public static HyperTableCell fromBoolean(boolean boolVal) { return boolVal ? trueCell : falseCell; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
