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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.List;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_Country;
import org.hypernomicon.model.relations.HyperObjPointer;

//---------------------------------------------------------------------------

public class HDT_Region extends HDT_RecordBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final List<HDT_Institution> institutions;

  public final HyperObjPointer<HDT_Region, HDT_Country> country;

//---------------------------------------------------------------------------

  public HDT_Region(RecordState xmlState, DatasetAccessor<HDT_Region> dataset)
  {
    super(xmlState, dataset);

    institutions = getSubjList(rtRegionOfInst);
    country = getObjPointer(rtCountryOfRegion);
  }

//---------------------------------------------------------------------------

  /**
   * {@inheritDoc}
   */
  @Override public String defaultCellText() { return getAbbreviation(); }

  public String getAbbreviation()           { return getTagString(tagAbbreviation); }
  public void   setAbbreviation(String str) { updateTagString(tagAbbreviation, str); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
