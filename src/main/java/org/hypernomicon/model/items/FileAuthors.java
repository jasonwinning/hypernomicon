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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.Map;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.relations.HyperObjList;

//---------------------------------------------------------------------------

public class FileAuthors extends Authors
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperObjList<HDT_MiscFile, HDT_Person> objList, objListNoMod;
  private final HDT_MiscFile miscFile;

//---------------------------------------------------------------------------

  public FileAuthors(HyperObjList<HDT_MiscFile, HDT_Person> objList, HDT_MiscFile miscFile)
  {
    this.objList = objList;
    this.miscFile = miscFile;

    objListNoMod = db.getObjectList(rtAuthorOfFile, miscFile, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int size()                                { return objList.size(); }
  @Override public boolean containsPerson(HDT_Person person) { return objListNoMod.contains(person); }
  @Override void resolvePointers() throws HDB_InternalError  { db.resolvePointersByRelation(rtAuthorOfFile, miscFile); }
  @Override public Author get(int ndx)                       { return new Author(objList.get(ndx)); }
  @Override void clearNoMod()                                { objListNoMod.clear(); }
  @Override void clear()                                     { objList.clear(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void addNoMod(HDT_Person person, Map<Tag, HDI_OfflineBase> tagToNestedItem) throws RelationCycleException
  {
    objListNoMod.add(person);
    objListNoMod.throwLastException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
