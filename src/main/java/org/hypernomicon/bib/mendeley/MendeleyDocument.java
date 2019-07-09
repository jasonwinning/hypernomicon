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

package org.hypernomicon.bib.mendeley;

import java.time.Instant;
import java.util.Collections;
import java.util.List;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.WorkBibAuthors;
import org.hypernomicon.bib.data.BibField;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.util.Util.*;

public class MendeleyDocument extends BibEntry implements MendeleyEntity
{
  @SuppressWarnings("unused")
  private final MendeleyWrapper mWrapper;
  private JsonObj jObj;
  private MendeleyDocument backupItem = null;

  MendeleyDocument(MendeleyWrapper mWrapper, JsonObj jObj, boolean thisIsBackup)
  {
    super(thisIsBackup);

    update(jObj, false, false);
    this.mWrapper = mWrapper;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  MendeleyDocument(MendeleyWrapper mWrapper, EntryType newType)
  {
    super(false);

    jObj = new JsonObj();
    this.mWrapper = mWrapper;

    jObj.put("key", "_!_" + randomAlphanumericStr(12));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public MendeleyEntityType getType() { return MendeleyEntityType.mendeleyDocument; }
  @Override public String getEntryKey()         { return getKey(); }
  @Override public String toString()            { return jObj.toString(); }
  @Override public String getKey()              { return jObj.getStr("id"); }
  @Override protected boolean isNewEntry()      { return jObj.containsKey("last_modified") == false; }
  @Override public Instant lastModified()       { return parseIso8601(jObj.getStr("last_modified")); }

  @Override public String getEntryURL()
  {
    if (isNewEntry()) return "";

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_WorkType getWorkType()
  {
    if (linkedToWork()) return getWork().workType.get();

    return EntryType.toWorkType(getEntryType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setWorkType(HDT_WorkType workType)
  {
    if (linkedToWork()) getWork().workType.set(workType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public BibAuthors getAuthors()
  {
    if (linkedToWork()) return new WorkBibAuthors(getWork());

    return new MendeleyAuthors();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void update(JsonObj jObj, boolean updatingExistingDataFromServer, boolean preMerge)
  {
    this.jObj = jObj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected List<String> getCollKeys(boolean deletedOK)
  {
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static EntryType parseMendeleyType(String mType)
  {
    return MendeleyWrapper.entryTypeMap.inverse().getOrDefault(mType, etOther);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public EntryType getEntryType()
  {
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setEntryType(EntryType entryType)
  {
    if (entryType == getEntryType()) return;

    throw new UnsupportedOperationException("change Mendeley entry type");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setStr(BibFieldEnum bibFieldEnum, String newStr)
  {

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getStr(BibFieldEnum bibFieldEnum)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfDOI   : return getWork().getDOI();
        case bfYear  : return getWork().getYear();
        case bfURL   : return getWork().getURL();
        case bfTitle : return getWork().name();
        default      : break;
      }
    }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setMultiStr(BibFieldEnum bibFieldEnum, List<String> list)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfTitle : getWork().setName(BibField.buildTitle(list));   return;
        case bfISBNs : getWork().setISBNs(list);                       return;
        case bfMisc  : getWork().setMiscBib(strListToStr(list, true)); return;
        default      : break;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<String> getMultiStr(BibFieldEnum bibFieldEnum)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfTitle : return Collections.singletonList(getWork().name());
        case bfISBNs : return getWork().getISBNs();
        case bfMisc  : return convertMultiLineStrToStrList(getWork().getMiscBib(), true);
        default      : break;
      }
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean isSynced()
  {
    if (isNewEntry()) return false;
    if (thisIsBackup) return true;
    if (authorsChanged()) return false;

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean authorsChanged()
  {
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void saveToDisk(JsonArray jArr)
  {
    if (thisIsBackup) return;

    JsonObj jDiskObj = jObj.clone();

    if (backupItem != null)
      jDiskObj.put("backupItem", backupItem.jObj);

    jArr.add(jDiskObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  JsonObj exportJsonObjForUploadToServer(boolean missingKeysOK)
  {
    JsonObj jServerObj = jObj.clone();

    return jServerObj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
