/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.model.items;

import java.util.List;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public class HDI_OnlineTernary extends HDI_OnlineBase<HDI_OfflineTernary>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Ternary value = Ternary.Unset;

//---------------------------------------------------------------------------

  public HDI_OnlineTernary(HDI_Schema schema, HDT_Record record) { super(schema, record); }

//---------------------------------------------------------------------------

  public Ternary get()           { return value; }
  public void set(Ternary value) { this.value = value == null ? Ternary.Unset : value; }

  /**
   * {@inheritDoc}
   */
  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords, boolean engChar) { }

  @Override public void setFromOfflineValue(HDI_OfflineTernary val, Tag tag)                { value = val.get(); }
  @Override public String getResultTextForTag(Tag tag, boolean limitTo20Items)              { return Ternary.isNullOrUnset(value) ? "" : value.toString(); }
  @Override public int getResultCount(Tag tag)                                              { return Ternary.isNullOrUnset(value) ? 0 : 1; }
  @Override public void getToOfflineValue(HDI_OfflineTernary val, Tag tag)                  { val.value = value; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
