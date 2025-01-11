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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.List;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_Country;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_InstitutionType;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjList;

//---------------------------------------------------------------------------

public class HDT_Institution extends HDT_RecordBase
{
  public static final int UNIV_SYS_INST_TYPE_ID    = 1,
                          SCHOOL_INST_TYPE_ID      = 5,
                          COLLEGE_SYS_INST_TYPE_ID = 6,
                          FACULTY_INST_TYPE_ID     = 9,
                          DEPARTMENT_INST_TYPE_ID  = 10;

  public final HyperSubjList<HDT_Person     , HDT_Institution> persons;
  public final HyperSubjList<HDT_Institution, HDT_Institution> subInstitutions;

  public final HyperObjPointer<HDT_Institution, HDT_Region         > region;
  public final HyperObjPointer<HDT_Institution, HDT_Country        > country;
  public final HyperObjPointer<HDT_Institution, HDT_InstitutionType> instType;
  public final HyperObjPointer<HDT_Institution, HDT_Institution    > parentInst;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Institution(RecordState xmlState, DatasetAccessor<HDT_Institution> dataset)
  {
    super(xmlState, dataset, tagName);

    subInstitutions = getSubjList(rtParentInstOfInst);
    persons         = getSubjList(rtInstOfPerson    );

    region     = getObjPointer(rtRegionOfInst    );
    country    = getObjPointer(rtCountryOfInst   );
    instType   = getObjPointer(rtTypeOfInst      );
    parentInst = getObjPointer(rtParentInstOfInst);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setCity(String newCity) { updateTagString(tagCity, newCity); }
  public String getCity()             { return getTagString(tagCity); }

  public void setURL(String newURL)   { updateTagString(tagWebURL, newURL); }
  public String getURL()              { return getTagString(tagWebURL); }

  @Override public String listName()                   { return extendedName(false); }
  @Override public String getXMLObjectName()           { return extendedName(true ); }
  @Override protected String makeSortKeyTypeSpecific() { return extendedName(true ); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String extendedName(boolean parentFirst)
  {
    int parentTypeID = parentInst.isNull() ? -1 : parentInst.get().instType.getID();

    if (List.of(-1, UNIV_SYS_INST_TYPE_ID, COLLEGE_SYS_INST_TYPE_ID).contains(parentTypeID))
      return name();

    return parentFirst ? (parentInst.get().name() + ' ' + name()) : (name() + ", " + parentInst.get().name());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    while (subInstitutions.isEmpty() == false)
      db.deleteRecord(subInstitutions.get(0));

    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void overwriteSubInstLocations()
  {
    subInstitutions.forEach(subInst ->
    {
      subInst.setCity(getCity());
      subInst.region.set(region.get());
      subInst.country.set(country.get());

      subInst.overwriteSubInstLocations();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
