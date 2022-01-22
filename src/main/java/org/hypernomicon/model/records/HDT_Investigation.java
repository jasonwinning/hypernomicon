/*
 * Copyright 2015-2022 Jason Winning
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

import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.relations.HyperObjPointer;

public class HDT_Investigation extends HDT_RecordWithConnector
{
  public final HyperObjPointer<HDT_Investigation, HDT_Person> person;

  public HDT_Investigation(RecordState xmlState, HyperDataset<HDT_Investigation> dataset)
  {
    super(xmlState, dataset, tagName);

    person = getObjPointer(rtPersonOfInv);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName() { return name(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
