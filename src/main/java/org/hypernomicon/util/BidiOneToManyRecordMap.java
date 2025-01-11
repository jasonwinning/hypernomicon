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

package org.hypernomicon.util;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Map;

import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.records.HDT_Record;

import java.util.Set;

import com.google.common.collect.ImmutableSet;

public class BidiOneToManyRecordMap extends BidiOneToManyMap<HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public <HDT_T extends HDT_Record> Set<HDT_T> getForwardRecordSet(HDT_Record fromRecord)
  {
    return (Set<HDT_T>) getForwardSet(fromRecord);
  }

  @SuppressWarnings("unchecked")
  public <HDT_T extends HDT_Record> Set<HDT_T> getReverseRecordSet(HDT_Record fromRecord)
  {
    return (Set<HDT_T>) getReverseSet(fromRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void addForward(HDT_Record fromRecord, HDT_Record toRecord)
  {
    if (toRecord.getType() == hdtHub)
    {
      HDT_Hub hub = (HDT_Hub) toRecord;

      nullSwitch(hub.getNote    (), note    -> super.addForward(fromRecord, note   ));
      nullSwitch(hub.getLabel   (), label   -> super.addForward(fromRecord, label  ));
      nullSwitch(hub.getDebate  (), debate  -> super.addForward(fromRecord, debate ));
      nullSwitch(hub.getPosition(), pos     -> super.addForward(fromRecord, pos    ));
      nullSwitch(hub.getConcept (), concept -> super.addForward(fromRecord, concept));
    }
    else if (fromRecord.getType() == hdtHub)
    {
      HDT_Hub hub = (HDT_Hub) fromRecord;

      nullSwitch(hub.getNote    (), note    -> super.addForward(note   , toRecord));
      nullSwitch(hub.getLabel   (), label   -> super.addForward(label  , toRecord));
      nullSwitch(hub.getDebate  (), debate  -> super.addForward(debate , toRecord));
      nullSwitch(hub.getPosition(), pos     -> super.addForward(pos    , toRecord));
      nullSwitch(hub.getConcept (), concept -> super.addForward(concept, toRecord));
    }
    else
      super.addForward(fromRecord, toRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDT_Record> getAllHeads()
  {
    ImmutableSet.Builder<HDT_Record> builder = ImmutableSet.builder();

    getForwardStream().filter(entry -> (entry.getKey().getID() != -1) && (entry.getValue().isEmpty() == false))
                      .map(Map.Entry::getKey)
                      .forEach(builder::add);

    return builder.build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
