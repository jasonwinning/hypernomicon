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

import static org.hypernomicon.model.HyperDB.*;

import java.util.ArrayList;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.relations.RelationSet.RelationType;

import static org.hypernomicon.util.Util.*;

public class HDI_OnlineNestedPointer extends HDI_OnlineBase<HDI_OfflineNestedPointer>
{
  private final RelationType relType;
  private HDT_Base target;

  public HDI_OnlineNestedPointer(HDI_Schema newSchema, HDT_Base newRecord)
  {
    super(newSchema, newRecord);
    relType = schema.getRelType();
    target = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Base get()            { return target; }
  public void set(HDT_Base target) { this.target = target; }

  @Override public String getResultTextForTag(Tag tag) { return nullSwitch(target, "", HDT_Base::listName); }
  @Override public void expire()                       { target = null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers()
  {
    if (HDT_Record.isEmpty(target))
      target = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineNestedPointer val, Tag tag) throws RelationCycleException
  {
    target = val.objID < 0 ? null : db.records(db.getNestedTargetType(relType, mainTag)).getByID(val.objID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineNestedPointer val, Tag tag)
  {
    val.objID = -1;

    if (HDT_Record.isEmpty(target) == false)
      val.objID = target.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(ArrayList<String> list, Tag tag, boolean searchLinkedRecords)
  {
    if (searchLinkedRecords &&(target != null))
      list.add(target.listName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
