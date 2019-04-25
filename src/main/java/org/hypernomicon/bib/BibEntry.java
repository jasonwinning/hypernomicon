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

package org.hypernomicon.bib;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.HyperDB.*;

import java.util.List;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.model.records.HDT_Work;

public abstract class BibEntry extends BibData
{
  protected final boolean thisIsBackup;

  public abstract String getEntryKey();
  protected abstract List<String> getCollKeys(boolean deletedOK);
  protected abstract boolean isSynced();
  protected abstract boolean isNewEntry();
  public abstract String getEntryURL();

  public BibEntry(boolean thisIsBackup)      { this.thisIsBackup = thisIsBackup; }

  @Override public HDT_Work getWork()        { return thisIsBackup ? null  : db.getWorkByBibEntryKey(getEntryKey()); }
  @Override protected boolean linkedToWork() { return thisIsBackup ? false : getWork() != null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void unassignWork()
  {
    HDT_Work work = getWork();
    work.setBibEntryKey("");

    BibData bd = work.getBibData();

    setStr(bfDOI, bd.getStr(bfDOI));
    setStr(bfYear, bd.getStr(bfYear));
    setStr(bfURL, bd.getStr(bfURL));
    setMultiStr(bfISBNs, bd.getMultiStr(bfISBNs));
    setTitle(bd.getStr(bfTitle));

    BibAuthors authors = getAuthors();
    authors.clear();

    bd.getAuthors().forEach(authors::add);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + (getEntryKey() == null ? 0 : getEntryKey().hashCode());
    result = prime * result + (thisIsBackup ? 1231 : 1237);
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    BibEntry other = (BibEntry) obj;

    if (getEntryKey() == null)
    {
      if (other.getEntryKey() != null) return false;
    }
    else if (!getEntryKey().equals(other.getEntryKey())) return false;

    return thisIsBackup == other.thisIsBackup;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}