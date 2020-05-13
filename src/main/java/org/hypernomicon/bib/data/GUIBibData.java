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

import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

public class GUIBibData extends BibDataStandalone
{
  public static final BibData NoneFoundBD = new GUIBibData();

  private HDT_WorkType workType;

  public GUIBibData() { super(); }

  public GUIBibData(BibData bd)
  {
    super();

    copyAllFieldsFrom(bd, true, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setWorkType(HDT_WorkType workType) { this.workType = workType; }
  @Override public HDT_WorkType getWorkType()              { return workType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
