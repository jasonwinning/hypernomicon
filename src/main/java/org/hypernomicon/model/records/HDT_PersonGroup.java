/*
 * Copyright 2015-2023 Jason Winning
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

import org.hypernomicon.model.HyperDataset;

//---------------------------------------------------------------------------

public class HDT_PersonGroup extends HDT_RecordBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final List<HDT_PersonGroup> parentGroups, subGroups;

//---------------------------------------------------------------------------

  public HDT_PersonGroup(RecordState xmlState, HyperDataset<HDT_PersonGroup> dataset)
  {
    super(xmlState, dataset, tagName);

    parentGroups = getObjList(rtParentGroupOfGroup);
    subGroups = getSubjList(rtParentGroupOfGroup);
  }

//---------------------------------------------------------------------------

  @Override public String listName() { return name(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
