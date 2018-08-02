/*
 * Copyright 2015-2018 Jason Winning
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

import static org.hypernomicon.model.records.HDT_RecordState.writeBooleanTag;
import static org.hypernomicon.util.Util.parseBoolean;

import java.util.LinkedHashMap;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;

public class HDI_OfflineTernary extends HDI_OfflineBase
{
  public static enum Ternary
  {
    Unset, False, True;
    
    public boolean boolVal() { return this == True; }
    
    @Override public String toString()
    {
      switch (this)
      {
        case False : return "False";
        case True  : return "True";
        default    : return "Unset";
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Ternary value = Ternary.Unset;

  public HDI_OfflineTernary(HDI_Schema newSchema, HDT_RecordState recordState)
  {
    super(newSchema, recordState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  @Override public void setFromXml(Tag tag, String nodeText, HDT_RecordType objType, int objID, LinkedHashMap<Tag, HDI_OfflineBase> nestedItems)
  {
    value = parseBoolean(nodeText) ? Ternary.True : Ternary.False;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    if (value != Ternary.Unset)
      writeBooleanTag(xml, tag, value.boolVal());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Ternary get()
  {
    return value;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
