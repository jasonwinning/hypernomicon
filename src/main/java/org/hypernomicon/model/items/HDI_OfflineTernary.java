/*
 * Copyright 2015-2020 Jason Winning
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

import static org.hypernomicon.model.records.HDT_RecordState.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.items.HDI_OfflineTernary.Ternary.*;

import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;

public class HDI_OfflineTernary extends HDI_OfflineBase
{
  public static enum Ternary
  {
    Unset, False, True;

    public boolean isTrue () { return this == True ; }
    public boolean isFalse() { return this == False; }
    public boolean isUnset() { return this == Unset; }

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

  Ternary value = Unset;

  public HDI_OfflineTernary(HDI_Schema newSchema, HDT_RecordState recordState)
  {
    super(newSchema, recordState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(Tag tag, String nodeText, HDT_RecordType objType, int objID, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    value = parseBoolean(nodeText) ? True : False;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    if (value != Unset)
      writeBooleanTag(xml, tag, value.isTrue());
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
