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

package org.hypernomicon.view;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.tabs.HyperTab;

import static org.hypernomicon.view.tabs.HyperTab.*;

import java.util.HashSet;
import java.util.Set;

import static org.hypernomicon.model.HyperDB.*;

//---------------------------------------------------------------------------

public class HyperView<HDT_CT extends HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class TextViewInfo
  {

//---------------------------------------------------------------------------

    public TextViewInfo(HDT_Record record)
    {
      this.record = record;
    }

    public TextViewInfo(HDT_Record record, int scrollPos)
    {
      this(record);

      this.scrollPos = scrollPos;
    }

    public TextViewInfo(TextViewInfo textViewInfo) // Copy constructor
    {
      this(textViewInfo.record, textViewInfo.scrollPos);

      openDivits    = textViewInfo.openDivits == null ? null : new HashSet<>(textViewInfo.openDivits);
      detailedWorks = textViewInfo.detailedWorks;
    }

//---------------------------------------------------------------------------

    public final HDT_Record record;
    public Set<String> openDivits = null;
    public int scrollPos = 0;
    public boolean detailedWorks = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_CT viewRecord;
  private final int tabRecordKeyNdx;
  private final RecordType tabRecordType;
  private final TabEnum tabEnum;
  private final TextViewInfo textViewInfo;

  int getTabRecordKeyNdx()          { return tabRecordKeyNdx; }
  public TextViewInfo getTextInfo() { return new TextViewInfo(textViewInfo); }
  RecordType getTabRecordType()     { return tabRecordType; }
  TabEnum getTabEnum()              { return tabEnum; }
  public HDT_CT getViewRecord()     { return viewRecord; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperView(HDT_CT record)
  {
    this(getTabEnumByRecordType(record.getType()), record);
  }

  public HyperView(TabEnum tabEnum, HDT_CT record)
  {
    this(tabEnum, record, new TextViewInfo(record));
  }

  public HyperView(TabEnum tabEnum, HDT_CT record, TextViewInfo textViewInfo)
  {
    tabRecordKeyNdx = record == null ? 0 : HyperTab.getActiveRecordForViewRecord(record).keyNdx();

    this.tabEnum = tabEnum;
    this.textViewInfo = textViewInfo;

    tabRecordType = getRecordTypeByTabEnum(tabEnum);
    viewRecord = record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public <HyperTabType extends HyperTab<? extends HDT_Record, HDT_CT>> HyperTabType getHyperTab()
  {
    return (HyperTabType) HyperTab.getHyperTab(tabEnum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Update record pointer after saving/reloading database

  @SuppressWarnings("unchecked")
  public void refreshRecordPtr()
  {
    if (viewRecord != null)
      viewRecord = (HDT_CT) db.records(viewRecord.getType()).getByID(viewRecord.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
