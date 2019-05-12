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

package org.hypernomicon.bib.data;

import java.util.List;

import java.util.ArrayList;
import java.util.Arrays;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.WorkBibAuthors;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;

public class WorkBibData extends BibData
{
  private final HDT_Work work;

  public WorkBibData(HDT_Work work)
  {
    this.work = work;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean linkedToWork()            { return true; }
  @Override public HDT_Work getWork()                { return work; }
  @Override public HDT_WorkType getWorkType()        { return work.workType.get(); }
  @Override public void setWorkType(HDT_WorkType wt) { work.workType.set(wt); }
  @Override public BibAuthors getAuthors()           { return new WorkBibAuthors(work); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BibEntry getBibEntry()
  {
    String entryKey = work.getBibEntryKey();

    return entryKey.length() > 0 ? db.getBibEntryByKey(entryKey) : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public EntryType getEntryType()
  {
    return nullSwitch(getBibEntry(), EntryType.fromWorkType(work.getWorkTypeEnum()), BibEntry::getEntryType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setMultiStr(BibFieldEnum bibFieldEnum, List<String> list)
  {
    switch (bibFieldEnum)
    {
      case bfTitle : work.setName(BibField.buildTitle(list));   return;
      case bfMisc  : work.setMiscBib(strListToStr(list, true)); return;
      case bfISBNs : work.setISBNs(list);                       return;

      default      : break;
    }

    nullSwitch(getBibEntry(), bibEntry -> bibEntry.setMultiStr(bibFieldEnum, list));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setEntryType(EntryType entryType)
  {
    nullSwitch(getBibEntry(), bibEntry -> bibEntry.setEntryType(entryType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    switch (bibFieldEnum)
    {
      case bfYear : work.setYear(newStr);    return;
      case bfDOI  : work.setDOI(newStr);     return;
      case bfURL  : work.setWebLink(newStr); return;

      default     : break;
    }

    nullSwitch(getBibEntry(), bibEntry -> bibEntry.setStr(bibFieldEnum, newStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<String> getMultiStr(BibFieldEnum bibFieldEnum)
  {
    switch (bibFieldEnum)
    {
      case bfTitle : return Arrays.asList(work.name());
      case bfISBNs : return work.getISBNs();
      case bfMisc  : return convertMultiLineStrToStrList(work.getMiscBib(), true);

      default      : return nullSwitch(getBibEntry(), new ArrayList<>(), bibEntry -> bibEntry.getMultiStr(bibFieldEnum));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getStr(BibFieldEnum bibFieldEnum)
  {
    switch (bibFieldEnum)
    {
      case bfDOI   : return work.getDOI();
      case bfYear  : return work.getYear();
      case bfURL   : return work.getWebLink();
      case bfTitle : return work.name();
      case bfMisc  : return work.getMiscBib();

      default      : return nullSwitch(getBibEntry(), "", bibEntry -> bibEntry.getStr(bibFieldEnum));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}