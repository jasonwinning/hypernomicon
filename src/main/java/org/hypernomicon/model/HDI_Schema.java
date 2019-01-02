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

package org.hypernomicon.model;

import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.Arrays;
import java.util.List;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Record.HyperDataCategory;
import org.hypernomicon.model.relations.RelationSet.RelationType;

public final class HDI_Schema
{
  private final List<Tag> tags;
  private final HyperDataCategory category;
  private final RelationType relType;
  
//---------------------------------------------------------------------------

  public final Tag getTag()                    { return tags.get(0); }
  public final List<Tag> getTags()             { return tags; }
  public final HyperDataCategory getCategory() { return category; }
  public final RelationType getRelType()       { return relType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDI_Schema(HyperDataCategory category, Tag... tags)
  {
    this.tags = Arrays.asList(tags);
    this.category = category;
    relType = rtNone;
    
    switch (category)
    {
      case hdcPointerMulti : case hdcPointerSingle : case hdcPath : case hdcAuthors :
        messageDialog("Internal error #42009", mtError);
        break;
        
      default : break;
    }      
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDI_Schema(HyperDataCategory dataCat, RelationType relType, Tag... tags)
  {
    this.tags = Arrays.asList(tags);
    this.category = dataCat;
    this.relType = relType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
