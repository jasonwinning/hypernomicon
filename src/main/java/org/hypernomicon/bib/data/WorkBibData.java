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

package org.hypernomicon.bib.data;

import java.util.List;

import java.util.ArrayList;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.WorkBibAuthors;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class WorkBibData extends BibData
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HDT_Work work;

//---------------------------------------------------------------------------

  public WorkBibData(HDT_Work work)
  {
    this.work = work;
  }

//---------------------------------------------------------------------------

  @Override public HDT_Work getWork()                      { return work; }
  @Override public HDT_WorkType getWorkType()              { return work.workType.get(); }
  @Override protected void setWorkType(HDT_WorkType wt)    { work.workType.set(wt); }
  @Override public BibAuthors getAuthors()                 { return new WorkBibAuthors(work); }
  @Override public BibliographicDate getDate()             { return work.getBibDate(); }
  @Override public void setDate(BibliographicDate newDate) { work.setBibDate(newDate); }

  @Override public void setAllAuthors(Iterable<BibAuthor> otherAuthors) { throw new UnsupportedOperationException("add"); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibEntry<?, ?> getBibEntry()
  {
    String entryKey = work.getBibEntryKey();

    return strNotNullOrEmpty(entryKey) ? db.getBibEntryByKey(entryKey) : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public EntryType getEntryType()
  {
    BibEntry<?, ?> bibEntry = getBibEntry();
    if (bibEntry != null) return bibEntry.getEntryType();

    EntryType entryType = EntryType.fromWorkType(work.getWorkTypeEnum());

    return db.bibLibraryIsLinked() && (db.getBibLibrary().getEntryTypeMap().containsKey(entryType) == false) ?
      EntryType.etUnentered
    :
      entryType;
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
    nullSwitch((BibData)getBibEntry(), bibEntry -> bibEntry.setEntryType(entryType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    switch (bibFieldEnum)
    {
      case bfDOI  : work.setDOI(newStr);  return;
      case bfURL  : work.setURL(newStr);  return;

      default     : break;
    }

    nullSwitch(getBibEntry(), bibEntry -> bibEntry.setStr(bibFieldEnum, newStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<String> getMultiStr(BibFieldEnum bibFieldEnum)
  {
    return switch (bibFieldEnum)
    {
      case bfTitle -> List.of(work.name());
      case bfISBNs -> work.getISBNs();
      case bfMisc  -> convertMultiLineStrToStrList(work.getMiscBib(), true);

      default      -> nullSwitch(getBibEntry(), new ArrayList<>(), bibEntry -> bibEntry.getMultiStr(bibFieldEnum));
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getStr(BibFieldEnum bibFieldEnum)
  {
    return switch (bibFieldEnum)
    {
      case bfDOI   -> work.getDOI();
      case bfURL   -> work.getURL();
      case bfTitle -> work.name();
      case bfMisc  -> work.getMiscBib();

      default      -> nullSwitch(getBibEntry(), "", bibEntry -> bibEntry.getStr(bibFieldEnum));
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
