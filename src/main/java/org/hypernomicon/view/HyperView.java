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

package org.hypernomicon.view;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.HyperTab.TabEnum;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.tabs.HyperTab.*;

import java.util.Set;

import static org.hypernomicon.model.HyperDB.*;

public class HyperView<HDT_CT extends HDT_Record>
{
  public static class TextViewInfo
  {
    public TextViewInfo()              { return; }
    public TextViewInfo(int scrollPos) { this.scrollPos = scrollPos; }

    public Set<String> openDivits = null;
    public int scrollPos = 0;
    public boolean detailedWorks = false;
  }

  private HDT_CT viewRecord;
  private final int tabRecordKeyNdx;
  private final HDT_RecordType tabRecordType;
  private final TabEnum tabEnum;
  private final TextViewInfo textInfo;

  int getTabRecordKeyNdx()          { return tabRecordKeyNdx; }
  public TextViewInfo getTextInfo() { return textInfo; }
  HDT_RecordType getTabRecordType() { return tabRecordType; }
  public TabEnum getTabEnum()       { return tabEnum; }
  public HDT_CT getViewRecord()     { return viewRecord; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperView(HDT_CT record)                  { this(getTabEnumByRecordType(record.getType()), record); }
  public HyperView(TabEnum tabEnum, HDT_CT record) { this(tabEnum, record, new TextViewInfo()); }

  public HyperView(TabEnum tabEnum, HDT_CT record, TextViewInfo textInfo)
  {
    if (record == null)
      tabRecordKeyNdx = 0;
    else if (record.getType() == hdtConcept)
      tabRecordKeyNdx = db.terms.getKeyNdxByID(HDT_Concept.class.cast(record).term.getID());
    else
      tabRecordKeyNdx = db.records(record.getType()).getKeyNdxByID(record.getID());

    this.tabEnum = tabEnum;
    this.textInfo = textInfo;

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

  @SuppressWarnings("unchecked")
  public void refresh()
  {
    if (viewRecord == null) return;

    viewRecord = (HDT_CT) db.records(viewRecord.getType()).getByID(viewRecord.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
