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

package org.hypernomicon.bib.lib;

import static org.hypernomicon.bib.BibData.BibFieldEnum.bfDOI;
import static org.hypernomicon.bib.BibData.BibFieldEnum.bfISBNs;
import static org.hypernomicon.bib.BibData.BibFieldEnum.bfTitle;
import static org.hypernomicon.bib.BibData.BibFieldEnum.bfURL;
import static org.hypernomicon.bib.BibData.BibFieldEnum.bfYear;
import static org.hypernomicon.model.HyperDB.*;
import static java.util.Objects.*;

import java.util.List;

import org.hypernomicon.bib.BibData;
import org.hypernomicon.model.records.HDT_Work;

public abstract class BibEntry extends BibData
{ 
  protected final boolean thisIsBackup;
  
  public abstract String getEntryKey();
  public abstract List<String> getCollKeys(boolean deletedOK);
  public abstract boolean isSynced();
  public abstract boolean isNewEntry();
  public abstract String getEntryURL();

  public BibEntry(boolean thisIsBackup) { this.thisIsBackup = thisIsBackup; }
  
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
    
    getAuthors().clear();
    
    bd.getAuthors().forEach(getAuthors()::add);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  @Override public HDT_Work getWork()
  { 
    if (thisIsBackup) return null;
    
    return db.getWorkByBibEntryKey(getEntryKey()); 
  }  
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  @Override public boolean linkedToWork() 
  { 
    if (thisIsBackup) return false;
    
    return nonNull(getWork()); 
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