/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.bib.data;

import java.util.EnumSet;

import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

public class GUIBibData extends BibDataStandalone
{
  private HDT_WorkType workType;

  public GUIBibData() { super(); }

  public GUIBibData(BibData bd)
  {
    super();

    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum -> { switch (bibFieldEnum.getType())
    {
      case bftString      : setStr(bibFieldEnum, bd.getStr(bibFieldEnum)); break;
      case bftMultiString : setMultiStr(bibFieldEnum, bd.getMultiStr(bibFieldEnum)); break;
      case bftEntryType   : setEntryType(bd.getEntryType()); break;
      case bftWorkType    : setWorkType(bd.getWorkType()); break;
      case bftAuthor      : break;
    }});

    bd.getAuthors().forEach(authors::add);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setWorkType(HDT_WorkType workType) { this.workType = workType; }
  @Override public HDT_WorkType getWorkType()              { return workType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
