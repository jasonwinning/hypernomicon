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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.time.Instant;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import com.google.common.escape.Escaper;

import static com.google.common.xml.XmlEscapers.*;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Exceptions.InvalidItemException;
import org.hypernomicon.model.items.*;

public class HDT_RecordState
{
  public LinkedHashMap<Tag, HDI_OfflineBase> items;
  private static final String QUOTE = "\"";

  public int id;
  public final HDT_RecordType type;
  public final String sortKeyAttr, searchKey;
  public String listName, simpleName;
  public Instant creationDate, modifiedDate, viewDate;
  boolean stored;

  public final boolean dummyFlag;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_RecordState(HDT_RecordType type, int id, String sortKeyAttr, String simpleName, String searchKey, String listName)
  {
    this(type, id, sortKeyAttr, simpleName, searchKey, listName, false);
  }

  public HDT_RecordState(HDT_RecordType type, int id, String sortKeyAttr, String simpleName, String searchKey, String listName, boolean dummyFlag)
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

    initItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initItems()
  {
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

  public void loadItemFromXML(Tag tag, String nodeText, HDT_RecordType objType, int objID, LinkedHashMap<Tag, HDI_OfflineBase> nestedItems) throws InvalidItemException
  {
    stored = true;

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

  final void writeToXML(StringBuilder xml)
  {
    if (type.isSimple()) { writeWholeRecord(xml, simpleName, listName); return; }

    writeRecordOpenTag(xml);

    items.forEach((tag, item) ->
    {
      if ((type != hdtFolder) || (tag != tagName))
        item.writeToXml(tag, xml);
    });

    writeRecordCloseTag(xml);

    stored = true;
  }

  private static Escaper xmlContentEscaper = xmlContentEscaper(), xmlAttributeEscaper = xmlAttributeEscaper();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void writeWholeRecord(StringBuilder xml, String nameToUse, String listNameAttr)
  {
    String searchKeyAttr = "", sortKeyAttrXML = "";

    if (listNameAttr.length() > 0)
      listNameAttr = " " + db.getTagStr(tagListName) + "=" + QUOTE + xmlAttributeEscaper.escape(listNameAttr) + QUOTE;

    if (searchKey.length() > 0)
      searchKeyAttr = " search_key=" + QUOTE + xmlAttributeEscaper.escape(searchKey) + QUOTE;

    if (sortKeyAttr.length() > 0)
      sortKeyAttrXML = " sort_key=" + QUOTE + xmlAttributeEscaper.escape(sortKeyAttr) + QUOTE;

    xml.append("<record type=" + QUOTE + db.getTypeTagStr(type) + QUOTE + " id=" + QUOTE + id + QUOTE)
       .append(sortKeyAttrXML + searchKeyAttr + listNameAttr + ">" + xmlContentEscaper.escape(nameToUse) + "</record>")
       .append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void writeRecordOpenTag(StringBuilder xml)
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

    xml.append("<record type=" + QUOTE + typeName + QUOTE)
       .append(" id=" + QUOTE + id + QUOTE + sortKeyAttrXML + searchKeyAttr + ">")
       .append(System.lineSeparator());

    if (creationDate == null) return;

    xml.append("  <creation_date>" + dateTimeToIso8601offset(creationDate) + "</creation_date>"); xml.append(System.lineSeparator())
       .append("  <modified_date>" + dateTimeToIso8601offset(modifiedDate) + "</modified_date>"); xml.append(System.lineSeparator())
       .append("  <view_date>"     + dateTimeToIso8601offset(viewDate)     + "</view_date>"    ); xml.append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void writeRecordCloseTag(StringBuilder xml)
  {
    xml.append("</record>" + System.lineSeparator() + System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void writePointerTagWithNestedPointers(StringBuilder xml, Tag tag, int objID, String value, Map<Tag, HDI_OfflineBase> map)
  {
    writePointerTagWithNestedPointers(xml, tag, objID, value, map, false);
  }

  public static void writePointerTagWithNestedPointers(StringBuilder xml, Tag tag, int objID, String value, Map<Tag, HDI_OfflineBase> map, boolean noIDOk)
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

    xml.append("  <" + db.getTagStr(tag) + idStr + ">" + xmlContentEscaper.escape(value))
       .append(System.lineSeparator());

    map.forEach((nestedTag, nestedItem) ->
    {
      xml.append("  ");
      nestedItem.writeToXml(nestedTag, xml);
    });

    xml.append("  </" + db.getTagStr(tag) + ">" + System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void writePointerTag(StringBuilder xml, Tag tag, int objID, HDT_RecordType objType, String value)
  {
    writePointerTag(xml, tag, objID, objType, value, false);
  }

  public static void writePointerTag(StringBuilder xml, Tag tag, int objID, HDT_RecordType objType, String value, boolean noIDOk)
  {
    if ((objID < 1) && (noIDOk == false)) return;

    String idStr = "", typeStr = "";

    if (objID > 0)          idStr   = " id="   + QUOTE + objID                     + QUOTE;
    if (objType != hdtNone) typeStr = " type=" + QUOTE + db.getTypeTagStr(objType) + QUOTE;

    xml.append("  <" + db.getTagStr(tag) + typeStr + idStr + ">" + xmlContentEscaper.escape(value) + "</" + db.getTagStr(tag) + ">")
       .append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void writeStringTag(StringBuilder xml, Tag tag, String tagText)
  {
    if (tagText.length() == 0) return;
    xml.append("  <" + db.getTagStr(tag) + ">" + xmlContentEscaper.escape(tagText) + "</" + db.getTagStr(tag) + ">" + System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void writeBooleanTag(StringBuilder xml, Tag tag, boolean tf)
  {
    xml.append("  <" + db.getTagStr(tag) + ">" + (tf ? "true" : "false") + "</" + db.getTagStr(tag) + ">" + System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setDates(Instant instant)
  {
    creationDate = instant;
    modifiedDate = instant;
    viewDate     = instant;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
