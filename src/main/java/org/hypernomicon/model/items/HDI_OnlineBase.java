/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.model.items;

import java.util.List;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public abstract class HDI_OnlineBase<HDI_Derived extends HDI_OfflineBase> extends HDI_Base
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final HDT_Record record;

//---------------------------------------------------------------------------

  public HDI_OnlineBase(HDI_Schema schema, HDT_Record record)
  {
    super(schema);
    this.record = record;
  }

//---------------------------------------------------------------------------

  public void expire() { }

  public abstract void setFromOfflineValue(HDI_Derived val, Tag tag) throws RelationCycleException, HDB_InternalError;

  public abstract void getToOfflineValue(HDI_Derived val, Tag tag);

  /**
   * @throws HDB_InternalError - if a non-expired record has a positive ID
   */
  public void resolvePointers() throws HDB_InternalError { }

  public abstract void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords);

  public abstract String getResultTextForTag(Tag tag);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
