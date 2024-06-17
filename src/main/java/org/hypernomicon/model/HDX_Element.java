package org.hypernomicon.model;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Iterator;

import javax.xml.stream.events.Attribute;
import javax.xml.stream.events.StartElement;

import org.hypernomicon.model.Exceptions.InvalidAttributeException;
import org.hypernomicon.model.Exceptions.InvalidItemException;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

/**
 * This class is used to represent any XML element inside a record element but not the record element itself.
 * This includes elements for items of the record as well as nested items.
 */
public final class HDX_Element
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Tag tag;
  private int objID = -1, ord = -1;
  private RecordType objType;

  public Tag getTag()            { return tag; }
  public int getObjID()          { return objID; }
  public RecordType getObjType() { return objType; }
  public int getOrd()            { return ord; }

//---------------------------------------------------------------------------

  public HDX_Element(Tag tag)
  {
    this.tag = tag;
    objID = -1;
    ord = -1;
    objType = hdtNone;
  }

//---------------------------------------------------------------------------

  HDX_Element(StartElement startElement, RecordState xmlRecord) throws InvalidItemException, InvalidAttributeException
  {
    tag = Tag.getTag(startElement.getName().getLocalPart());

    if (tag == tagNone)
      throw new InvalidItemException(xmlRecord.id, xmlRecord.type, startElement.getName().getLocalPart());

    objType = tag.objType;

    Iterator<Attribute> it = startElement.getAttributes();

    while (it.hasNext())
    {
      Attribute attribute = it.next();

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
          throw new InvalidAttributeException(xmlRecord.id, xmlRecord.type, tag.name, attribute.getName().toString());
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
