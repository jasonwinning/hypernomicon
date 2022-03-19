/*
 * Copyright 2015-2022 Jason Winning
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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;

public abstract class HDI_OfflineBase extends HDI_Base
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String QUOTE = "\"";

  final RecordState recordState;

  public HDI_OfflineBase(HDI_Schema schema, RecordState recordState)
  {
    super(schema);
    this.recordState = recordState;
  }

  public abstract void setFromXml(Tag tag, String nodeText, RecordType objType, int objID, Map<Tag, HDI_OfflineBase> nestedItems);

  public abstract void writeToXml(Tag tag, StringBuilder xml);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void writePointerTagWithNestedPointers(StringBuilder xml, Tag tag, int objID, String value, Map<Tag, HDI_OfflineBase> map)
  {
    writePointerTagWithNestedPointers(xml, tag, objID, value, map, false);
  }

  static void writePointerTagWithNestedPointers(StringBuilder xml, Tag tag, int objID, String value, Map<Tag, HDI_OfflineBase> map, boolean noIDOk)
  {
    if ((objID < 1) && (noIDOk == false)) return;

    if (map.isEmpty())
    {
      writePointerTag(xml, tag, objID, hdtNone, value, noIDOk);
      return;
    }

    String idStr = "";
    if (objID > 0)
      idStr = " id=" + QUOTE + objID + QUOTE;

    xml.append("  <").append(db.getTagStr(tag)).append(idStr).append(">")
       .append(xmlContentEscaper.escape(value))
       .append(System.lineSeparator());

    map.forEach((nestedTag, nestedItem) ->
    {
      xml.append("  ");
      nestedItem.writeToXml(nestedTag, xml);
    });

    xml.append("  </").append(db.getTagStr(tag)).append(">")
       .append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void writePointerTag(StringBuilder xml, Tag tag, int objID, RecordType objType, String value)
  {
    writePointerTag(xml, tag, objID, objType, value, false);
  }

  static void writePointerTag(StringBuilder xml, Tag tag, int objID, RecordType objType, String value, boolean noIDOk)
  {
    if ((objID < 1) && (noIDOk == false)) return;

    String idStr = "", typeStr = "";

    if (objID > 0)          idStr   = " id="   + QUOTE + objID                     + QUOTE;
    if (objType != hdtNone) typeStr = " type=" + QUOTE + db.getTypeTagStr(objType) + QUOTE;

    xml.append("  <").append(db.getTagStr(tag)).append(typeStr).append(idStr).append(">")
       .append(xmlContentEscaper.escape(value))
       .append("</").append(db.getTagStr(tag)).append(">")
       .append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void writeStringTag(StringBuilder xml, Tag tag, String tagText)
  {
    if (tagText.isEmpty()) return;
    xml.append("  <").append(db.getTagStr(tag)).append(">")
       .append(xmlContentEscaper.escape(tagText))
       .append("</").append(db.getTagStr(tag)).append(">")
       .append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void writeBooleanTag(StringBuilder xml, Tag tag, boolean tf)
  {
    xml.append("  <").append(db.getTagStr(tag)).append(">")
       .append(tf ? "true" : "false")
       .append("</").append(db.getTagStr(tag)).append(">")
       .append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
