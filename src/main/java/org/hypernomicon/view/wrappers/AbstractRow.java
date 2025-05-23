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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

import static org.hypernomicon.util.Util.*;

import javafx.scene.control.TreeItem;

//---------------------------------------------------------------------------

public abstract class AbstractRow<HDT_T extends HDT_Record, RowType extends AbstractRow<HDT_T, RowType>>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract <HDT_T1 extends HDT_T> HDT_T1 getRecord();

  public RecordType getRecordType        () { return nullSwitch(getRecord(), hdtNone, HDT_Record::getType); }
  public int        getRecordID          () { return nullSwitch(getRecord(), -1, HDT_Record::getID); }
  protected TreeItem<RowType> getTreeItem() { return null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  <HDT_T1 extends HDT_T> HDT_T1 getRecordByType(RecordType recordType)
  {
    HDT_T1 record = getRecord();

    return (recordType == hdtNone) || ((record != null) && (record.getType() == recordType)) ? record : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
