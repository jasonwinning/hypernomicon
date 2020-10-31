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

package org.hypernomicon.bib;

import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.wrappers.AbstractRow;

public class BibEntryRow extends AbstractRow<HDT_Work, BibEntryRow>
{
  private final BibEntry entry;

  public BibEntryRow(BibEntry entry) { this.entry = entry; }
  public BibEntry getEntry()         { return entry; }
  public HDT_Work getWork()          { return entry.getWork(); }
  public String getURL()             { return entry.getEntryURL(); }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_Work> HDT_T getRecord() { return (HDT_T) getWork(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
