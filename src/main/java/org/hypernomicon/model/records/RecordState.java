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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HDI_Schema.HyperDataCategory.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

import java.time.Instant;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.InvalidItemException;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.unities.HDI_OfflineMainTextAndHub;
import org.hypernomicon.model.unities.HDI_OfflineHubSpokes;

public class RecordState
{
  public final Map<Tag, HDI_OfflineBase> items;
  public final RecordType type;
  final String sortKeyAttr, searchKey;
  final boolean dummyFlag;

  private static final char QUOTE = '"';

  public int id;
  public String listName;
  public Instant creationDate, modifiedDate, viewDate;
  public boolean stored;

  String simpleName;

  public String getSearchKey() { return searchKey; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // This is only called when a new, blank record is being created as a result of
  // user interaction with the UI, like clicking the Create New button

  public RecordState(RecordType type)
  {
    this(type, -1, "", "", "", "", false);

    nullSwitch(db.getMainTextTemplate(type), this::setMainText);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public RecordState(RecordType type, int id)
  {
    this(type, id, "", "", "", "", false);
  }

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
      HDI_OfflineBase item = switch (schema.category())
      {
        case hdcBoolean        -> new HDI_OfflineBoolean       (schema, this);
        case hdcTernary        -> new HDI_OfflineTernary       (schema, this);
        case hdcMainTextAndHub -> new HDI_OfflineMainTextAndHub(schema, this);
        case hdcPersonName     -> new HDI_OfflinePersonName    (schema, this);
        case hdcPath           -> new HDI_OfflinePath          (schema, this);
        case hdcPointerMulti   -> new HDI_OfflinePointerMulti  (schema, this);
        case hdcPointerSingle  -> new HDI_OfflinePointerSingle (schema, this);
        case hdcBibEntryKey,
             hdcString         -> new HDI_OfflineString        (schema, this);
        case hdcAuthors        -> new HDI_OfflineAuthors       (schema, this);
        case hdcHubSpokes      -> new HDI_OfflineHubSpokes     (schema, this);

        default                ->
        {
          messageDialog("Internal error #78934", mtError);
          yield null;
        }
      };

      if (item != null)
        schema.tags().forEach(tag -> items.put(tag, item));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setItemFromXML(Tag tag, String nodeText, RecordType objType, int objID, int ord, Map<Tag, HDI_OfflineBase> nestedItems) throws HyperDataException
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
    {
      if (((type == hdtWork) || (type == hdtMiscFile)) && (tag == tagWorkLabel)) // Backwards compatibility with records XML version 1.5. No conversion needed
        return;                                                                  // because these relations were already redundant with Key Works.

      throw new InvalidItemException(id, type, tag.name);
    }

    if (ord != -1)
    {
      if (item.category() != hdcPointerSingle)
        throw new HyperDataException("Invalid attribute: ord. Record type: " + getTypeTagStr(type) + " ID : " + id);

      ((HDI_OfflinePointerSingle)item).setFromXml(objID, ord, nestedItems);
    }
    else
      item.setFromXml(tag, nodeText, objType, objID, nestedItems);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setMainText(String html)
  {
    Tag tag = db.mainTextTagForRecordType(type);
    ((HDI_OfflineMainTextAndHub) items.get(tag)).setFromXml(tag, html, null, -1, null);
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
      listNameAttr = ' ' + tagListName.name + '=' + QUOTE + xmlAttributeEscaper.escape(listNameAttr) + QUOTE;

    if (searchKey.length() > 0)
      searchKeyAttr = " search_key=" + QUOTE + xmlAttributeEscaper.escape(searchKey) + QUOTE;

    if (sortKeyAttr.length() > 0)
      sortKeyAttrXML = " sort_key=" + QUOTE + xmlAttributeEscaper.escape(sortKeyAttr) + QUOTE;

    xml.append("<record type=").append(QUOTE).append(getTypeTagStr(type)).append(QUOTE).append(" id=").append(QUOTE).append(id).append(QUOTE)
       .append(sortKeyAttrXML).append(searchKeyAttr).append(listNameAttr).append('>').append(xmlContentEscaper.escape(nameToUse)).append("</record>")
       .append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void writeRecordOpenTag(StringBuilder xml)
  {
    String searchKeyAttr = "", typeName = getTypeTagStr(type), sortKeyAttrXML = sortKeyAttr;

    if (type != hdtWorkLabel)
    {
      searchKeyAttr = searchKey;

      if (searchKeyAttr.length() > 0)
        searchKeyAttr = " search_key=" + QUOTE + xmlAttributeEscaper.escape(searchKeyAttr) + QUOTE;
    }

    if (sortKeyAttrXML.length() > 0)
      sortKeyAttrXML = " sort_key=" + QUOTE + xmlAttributeEscaper.escape(sortKeyAttrXML) + QUOTE;

    xml.append("<record type=").append(QUOTE).append(typeName).append(QUOTE)
       .append(" id=").append(QUOTE).append(id).append(QUOTE).append(sortKeyAttrXML).append(searchKeyAttr).append('>')
       .append(System.lineSeparator());

    if (creationDate == null) return;

    xml.append("  <creation_date>").append(dateTimeToIso8601offset(creationDate)).append("</creation_date>").append(System.lineSeparator())
       .append("  <modified_date>").append(dateTimeToIso8601offset(modifiedDate)).append("</modified_date>").append(System.lineSeparator())
       .append("  <view_date>"    ).append(dateTimeToIso8601offset(viewDate    )).append("</view_date>"    ).append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
