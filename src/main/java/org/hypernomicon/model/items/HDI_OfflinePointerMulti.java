/*
 * Copyright 2015-2024 Jason Winning
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
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;

public class HDI_OfflinePointerMulti extends HDI_OfflineBase
{
  final List<Integer> objIDs = new ArrayList<>();
  private final RecordType objType;
  final Map<Integer, Map<Tag, HDI_OfflineBase>> objIDtoMaps = new LinkedHashMap<>();

  public HDI_OfflinePointerMulti(HDI_Schema schema, RecordState recordState)
  {
    super(schema, recordState);
    objType = db.getObjType(schema.relType());
  }

  public List<Integer> getObjIDs() { return Collections.unmodifiableList(objIDs); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(Tag tag, String nodeText, RecordType objType, int objID, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    objIDs.add(objID);

    if (collEmpty(nestedItems) == false)
      objIDtoMaps.put(objID, nestedItems);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    objIDs.forEach(objID ->
    {
      if (objIDtoMaps.containsKey(objID))
        writePointerTagWithNestedPointers(xml, tag, objID, db.records(objType).getByID(objID).getXMLObjectName(), objIDtoMaps.get(objID));
      else
        writePointerTag(xml, tag, objID, hdtNone, db.records(objType).getByID(objID).getXMLObjectName());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
