/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.model;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Iterator;

import javax.xml.stream.events.Attribute;
import javax.xml.stream.events.StartElement;

import org.apache.commons.lang3.compare.ComparableUtils;

import org.hypernomicon.model.Exceptions.InvalidAttributeException;
import org.hypernomicon.model.Exceptions.InvalidItemException;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.VersionNumber;

//---------------------------------------------------------------------------

/**
 * This class is used to represent any XML element inside a record element but not the record element itself.
 * This includes elements for "top-level" items of the record as well as nested items.
 */
public class HDX_Element
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Tag tag;
  private int objID = -1, ord = -1;
  private RecordType objType = hdtNone;

  public Tag getTag()            { return tag; }
  public int getObjID()          { return objID; }
  public RecordType getObjType() { return objType; }
  public int getOrd()            { return ord; }

//---------------------------------------------------------------------------

  public HDX_Element(Tag tag)
  {
    this.tag = tag;
  }

//---------------------------------------------------------------------------

  private HDX_Element(Tag tag, Iterator<Attribute> attributesIt, RecordState xmlRecord) throws InvalidAttributeException
  {
    this.tag = tag;

    objType = tag.objType;

    while (attributesIt.hasNext())
    {
      Attribute attribute = attributesIt.next();

      switch (attribute.getName().toString())
      {
        case "id" :
          if (objType != hdtNone)
            objID = parseInt(attribute.getValue(), -1);
          break;

        case "type" :
          if (objType == hdtAuxiliary) // this represents that the object type is not given away by the
                                       // tag name, and should be obtained from the "type" attribute
            objType = parseTypeTagStr(attribute.getValue());

          break;

        case "ord" :
          ord = parseInt(attribute.getValue(), -1);
          break;

        default:
          throw new InvalidAttributeException(xmlRecord.id, xmlRecord.type, tag, attribute.getName().toString());
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static HDX_Element create(StartElement startElement, RecordState xmlRecord, VersionNumber dataVersion) throws InvalidItemException, InvalidAttributeException
  {
    String tagName = startElement.getName().getLocalPart();

    Tag tag = Tag.getTag(tagName);

    if (tag == tagNone)
    {
      if ("year".equals(tagName) && ComparableUtils.is(dataVersion).lessThanOrEqualTo(new VersionNumber(1, 7)))
        tag = tagBibDate; // Backwards compatibility for record data version 1.7 or lower

      else if ("counterargument".equals(tagName) && ComparableUtils.is(dataVersion).lessThanOrEqualTo(new VersionNumber(1, 8)))
        tag = tagTargetArgument; // Backwards compatibility for record data version 1.8 or lower

      else if (("comments".equals(tagName) || "why_famous".equals(tagName)) && ComparableUtils.is(dataVersion).lessThanOrEqualTo(new VersionNumber(1, 9)))
        tag = tagDescription; // Backwards compatibility for record data version 1.9 or lower

      else if ("linked_record".equals(tagName) && ComparableUtils.is(dataVersion).lessThanOrEqualTo(new VersionNumber(1, 9)))
        tag = tagSpokeRecord; // Backwards compatibility for record data version 1.9 or lower

      else
        throw new InvalidItemException(xmlRecord.id, xmlRecord.type, tagName);
    }

    if (tag == tagBibDate)
      return new HDX_BibDateElement(startElement.getAttributes(), xmlRecord);

    return new HDX_Element(tag, startElement.getAttributes(), xmlRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
