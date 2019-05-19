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
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_Country;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_InstitutionType;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjList;

//---------------------------------------------------------------------------

public class HDT_Institution extends HDT_RecordBase
{
  public final HyperSubjList<HDT_Person, HDT_Institution> persons;
  public final HyperSubjList<HDT_Institution, HDT_Institution> subInstitutions;

  public final HyperObjPointer<HDT_Institution, HDT_Region> region;
  public final HyperObjPointer<HDT_Institution, HDT_Country> country;
  public final HyperObjPointer<HDT_Institution, HDT_InstitutionType> instType;
  public final HyperObjPointer<HDT_Institution, HDT_Institution> parentInst;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Institution(HDT_RecordState xmlState, HyperDataset<HDT_Institution> dataset)
  {
    super(xmlState, dataset, tagName);

    subInstitutions = getSubjList(rtParentInstOfInst);
    persons = getSubjList(rtInstOfPerson);

    region = getObjPointer(rtRegionOfInst);
    country = getObjPointer(rtCountryOfInst);
    instType = getObjPointer(rtTypeOfInst);
    parentInst = getObjPointer(rtParentInstOfInst);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setCity(String newCity) { updateTagString(tagCity, newCity); }
  public String getCity()             { return getTagString(tagCity); }
  public boolean isDeptOrFaculty()    { return (instType.getID() == HDT_InstitutionType.FACULTY_INST_TYPE_ID) ||
                                               (instType.getID() == HDT_InstitutionType.DEPARTMENT_INST_TYPE_ID); }

  public void setWebLink(String newLink) { updateTagString(tagWebLink, newLink); }
  public String getWebLink()             { return getTagString(tagWebLink); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getXMLObjectName()
  {
    if (parentInst.isNull())
      return name();

    int parentType = parentInst.get().instType.getID();

    if ((parentType == 1) || (parentType == 6))
      return name();

    return parentInst.get().name() + " " + name();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName()
  {
    if (parentInst.isNull())
      return name();

    int parentType = parentInst.get().instType.getID();

    if ((parentType == 1) || (parentType == 6))
      return name();

    return name() + ", " + parentInst.get().name();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    while (subInstitutions.isEmpty() == false)
      db.deleteRecord(hdtInstitution, subInstitutions.get(0).getID());

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
