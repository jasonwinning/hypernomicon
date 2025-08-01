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

import java.util.Map;

import org.hypernomicon.model.*;
import org.hypernomicon.model.records.RecordState;

import static org.hypernomicon.util.StringUtil.*;

//---------------------------------------------------------------------------

public class HDI_OfflineBibDate extends HDI_OfflineBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BibliographicDate value = BibliographicDate.EMPTY_DATE;

//---------------------------------------------------------------------------

  public HDI_OfflineBibDate(HDI_Schema schema, RecordState recordState)
  {
    super(schema, recordState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibliographicDate get()           { return value; }
  public void set(BibliographicDate value) { this.value = value; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(HDX_Element element, String nodeText, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    assert(element instanceof HDX_BibDateElement);

    HDX_BibDateElement bdElement = (HDX_BibDateElement)element;

    value = bdElement.getBibDate();

    if (strNotNullOrBlank(nodeText)) // Backwards compatibility for record data version 1.7 or lower
      value = value.setYear(nodeText, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    writeBibiographicDateTag(xml, tag, value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
