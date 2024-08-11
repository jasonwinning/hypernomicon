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

package org.hypernomicon.model;

import static org.hypernomicon.model.Tag.*;

import java.util.Iterator;

import javax.xml.stream.events.Attribute;

import org.hypernomicon.model.Exceptions.InvalidAttributeException;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.RecordState;

//---------------------------------------------------------------------------

public class HDX_BibDateElement extends HDX_Element
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final BibliographicDate bibDate;

  public BibliographicDate getBibDate() { return bibDate; }

//---------------------------------------------------------------------------

  HDX_BibDateElement(Iterator<Attribute> attributesIt, RecordState xmlRecord) throws InvalidAttributeException
  {
    super(tagBibDate);

    bibDate = BibliographicDate.fromXmlAttribs(attributesIt, xmlRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
