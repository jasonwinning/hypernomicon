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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.items.Ternary.*;
import static org.hypernomicon.util.Util.*;

import java.util.Map;

import org.hypernomicon.model.*;
import org.hypernomicon.model.records.RecordState;

//---------------------------------------------------------------------------

public class HDI_OfflineTernary extends HDI_OfflineBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Ternary value = Unset;

//---------------------------------------------------------------------------

  public HDI_OfflineTernary(HDI_Schema schema, RecordState recordState, Ternary newValue)
  {
    this(schema, recordState);

    value = newValue == null ? Unset : newValue;
  }

  public HDI_OfflineTernary(HDI_Schema schema, RecordState recordState)
  {
    super(schema, recordState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(HDX_Element element, String nodeText, Map<Tag, HDI_OfflineBase> nestedItems)
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
