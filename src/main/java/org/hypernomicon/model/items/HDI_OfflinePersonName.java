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

package org.hypernomicon.model.items;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HDX_Element;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.RecordState;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.Util.*;

import java.util.Map;

//---------------------------------------------------------------------------

public class HDI_OfflinePersonName extends HDI_OfflineBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String firstName = "", lastName = "";

//---------------------------------------------------------------------------

  public HDI_OfflinePersonName(HDI_Schema schema, RecordState recordState)
  {
    super(schema, recordState);
  }

//---------------------------------------------------------------------------

  public String getFirstName() { return firstName; }
  public String getLastName () { return lastName;  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(HDX_Element element, String nodeText, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    if      (element.getTag() == tagFirstName) firstName = convertToSingleLine(nodeText).strip();
    else if (element.getTag() == tagLastName)  lastName  = convertToSingleLine(nodeText).strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    if      (tag == tagFirstName) writeStringTag(xml, tag, firstName);
    else if (tag == tagLastName)  writeStringTag(xml, tag, lastName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
