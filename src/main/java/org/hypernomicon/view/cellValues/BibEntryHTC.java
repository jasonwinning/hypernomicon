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

package org.hypernomicon.view.cellValues;

import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public class BibEntryHTC extends AbstractHTC
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final BibEntry<?, ?> bibEntry;

//---------------------------------------------------------------------------

  public BibEntryHTC(BibEntry<?, ?> bibEntry)
  {
    super(false);

    this.bibEntry = bibEntry;
  }

//---------------------------------------------------------------------------

  @Override public int getID()                { return bibEntry.numericID(); }
  @Override public String getText()           { return bibEntry.getCellText(true, true); }
  @Override public RecordType getRecordType() { return hdtNone; }
  @Override public boolean isEmpty()          { return bibEntry == null; }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_Record> HDT_T getRecord() { return (HDT_T) bibEntry.getWork(); }

  @Override public HyperTableCell getCopyWithID(int newID) { throw new UnsupportedOperationException("copy"); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
