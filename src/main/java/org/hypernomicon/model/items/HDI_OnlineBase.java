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

package org.hypernomicon.model.items;

import java.util.ArrayList;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Base;

public abstract class HDI_OnlineBase<HDI_Derived extends HDI_OfflineBase> extends HDI_Base
{
  protected final HDT_Base record;

  public HDI_OnlineBase(HDI_Schema newSchema, HDT_Base newRecord)
  {
    super(newSchema);
    record = newRecord;
  }

  public void expire() { return; }

  public abstract void setFromOfflineValue(HDI_Derived val, Tag tag) throws RelationCycleException;

  public abstract void getToOfflineValue(HDI_Derived val, Tag tag);

  /**
   * @throws HDB_InternalError
   */
  public void resolvePointers() throws HDB_InternalError { return; }

  public abstract void getStrings(ArrayList<String> list, Tag tag, boolean searchLinkedRecords);

  public abstract String getResultTextForTag(Tag tag);
}
