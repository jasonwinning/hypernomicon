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

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Base;

public class HDI_OnlineBoolean extends HDI_OnlineBase<HDI_OfflineBoolean>
{
  private boolean boolValue;

  public HDI_OnlineBoolean(HDI_Schema newSchema, HDT_Base newRecord)
  {
    super(newSchema, newRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean get()               { return boolValue; }
  public void set(boolean boolValue) { this.boolValue = boolValue; }

  @Override public void setFromOfflineValue(HDI_OfflineBoolean val, Tag tag)
  {
    boolValue = val.get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(ArrayList<String> list, Tag tag, boolean searchLinkedRecords)
  {
    return;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    return boolValue ? "True" : "False";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineBoolean val, Tag tag)
  {
    val.boolValue = boolValue;
  }

}
