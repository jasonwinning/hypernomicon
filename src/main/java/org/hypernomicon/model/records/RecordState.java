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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

import java.time.Instant;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Exceptions.InvalidItemException;
import org.hypernomicon.model.items.*;

public class RecordState
{
  public final Map<Tag, HDI_OfflineBase> items;
  private static final String QUOTE = "\"";

  public int id;
  public final RecordType type;
  final String sortKeyAttr, searchKey;
  public String listName;
  String simpleName;
  public Instant creationDate, modifiedDate, viewDate;
  public boolean stored;
  final boolean dummyFlag;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public RecordState(RecordType type, int id, String sortKeyAttr, String simpleName, String searchKey, String listName)
  {
    this(type, id, sortKeyAttr, simpleName, searchKey, listName, false);
  }

  public RecordState(RecordType type, int id, String sortKeyAttr, String simpleName, String searchKey, String listName, boolean dummyFlag)
  {
    if (type.isSimple())
    {
      this.simpleName = simpleName;
      this.searchKey = "";
    }
    else
    {
      this.simpleName = "";
      this.searchKey = searchKey;
    }

    this.dummyFlag = dummyFlag;
    this.id = id;
    this.type = type;
    this.sortKeyAttr = sortKeyAttr;
    this.listName = listName;

    stored = false;

    creationDate = null;
    modifiedDate = null;
    viewDate     = null;

    items = new LinkedHashMap<>();

    Collection<HDI_Schema> schemas = db.getSchemasByRecordType(type);
    if (schemas == null) return;

    schemas.forEach(schema ->
    {
      HDI_OfflineBase item;

      switch (schema.getCategory())
      {
        case hdcBoolean       : item = new HDI_OfflineBoolean      (schema, this); break;
        case hdcTernary       : item = new HDI_OfflineTernary      (schema, this); break;
        case hdcConnector     : item = new HDI_OfflineConnector    (schema, this); break;
        case hdcPersonName    : item = new HDI_OfflinePersonName   (schema, this); break;
        case hdcPath          : item = new HDI_OfflinePath         (schema, this); break;
        case hdcPointerMulti  : item = new HDI_OfflinePointerMulti (schema, this); break;
        case hdcPointerSingle : item = new HDI_OfflinePointerSingle(schema, this); break;
        case hdcString        : item = new HDI_OfflineString       (schema, this); break;
        case hdcBibEntryKey   : item = new HDI_OfflineString       (schema, this); break;
        case hdcAuthors       : item = new HDI_OfflineAuthors      (schema, this); break;
        case hdcHubSpokes     : item = new HDI_OfflineHubSpokes    (schema, this); break;
        default:
          messageDialog("Internal error #78934", mtError);
          return;
      }

      schema.getTags().forEach(tag -> items.put(tag, item));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void loadItemFromXML(Tag tag, String nodeText, RecordType objType, int objID, Map<Tag, HDI_OfflineBase> nestedItems) throws InvalidItemException
  {
    if ((type == hdtHub) && (tag == tagName))
      return;

    if (tag == tagNone)
    {
      simpleName = nodeText;
      return;
    }

    HDI_OfflineBase item = items.get(tag);

    if (item == null)
      throw new InvalidItemException(id, type, db.getTagStr(tag));

    item.setFromXml(tag, nodeText, objType, objID, nestedItems);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void writeToXML(StringBuilder xml)
  {
    stored = true;

    if (type.isSimple()) { writeWholeRecord(xml, simpleName, listName); return; }

    writeRecordOpenTag(xml);

    items.forEach((tag, item) ->
    {
      if ((type != hdtFolder) || (tag != tagName))
        item.writeToXml(tag, xml);
    });

    xml.append("</record>").append(System.lineSeparator()).append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void writeWholeRecord(StringBuilder xml, String nameToUse, String listNameAttr)
  {
    String searchKeyAttr = "", sortKeyAttrXML = "";

    if (listNameAttr.length() > 0)
      listNameAttr = " " + db.getTagStr(tagListName) + "=" + QUOTE + xmlAttributeEscaper.escape(listNameAttr) + QUOTE;

    if (searchKey.length() > 0)
      searchKeyAttr = " search_key=" + QUOTE + xmlAttributeEscaper.escape(searchKey) + QUOTE;

    if (sortKeyAttr.length() > 0)
      sortKeyAttrXML = " sort_key=" + QUOTE + xmlAttributeEscaper.escape(sortKeyAttr) + QUOTE;

    xml.append("<record type=").append(QUOTE).append(db.getTypeTagStr(type)).append(QUOTE).append(" id=").append(QUOTE).append(id).append(QUOTE)
       .append(sortKeyAttrXML).append(searchKeyAttr).append(listNameAttr).append(">").append(xmlContentEscaper.escape(nameToUse)).append("</record>")
       .append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void writeRecordOpenTag(StringBuilder xml)
  {
    String searchKeyAttr = "", typeName = db.getTypeTagStr(type), sortKeyAttrXML = sortKeyAttr;

    if (type != hdtWorkLabel)
    {
      searchKeyAttr = searchKey;

      if (searchKeyAttr.length() > 0)
        searchKeyAttr = " search_key=" + QUOTE + xmlAttributeEscaper.escape(searchKeyAttr) + QUOTE;
    }

    if (sortKeyAttrXML.length() > 0)
      sortKeyAttrXML = " sort_key=" + QUOTE + xmlAttributeEscaper.escape(sortKeyAttrXML) + QUOTE;

    xml.append("<record type=").append(QUOTE).append(typeName).append(QUOTE)
       .append(" id=").append(QUOTE).append(id).append(QUOTE).append(sortKeyAttrXML).append(searchKeyAttr).append(">")
       .append(System.lineSeparator());

    if (creationDate == null) return;

    xml.append("  <creation_date>").append(dateTimeToIso8601offset(creationDate)).append("</creation_date>").append(System.lineSeparator())
       .append("  <modified_date>").append(dateTimeToIso8601offset(modifiedDate)).append("</modified_date>").append(System.lineSeparator())
       .append("  <view_date>"    ).append(dateTimeToIso8601offset(viewDate    )).append("</view_date>"    ).append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
