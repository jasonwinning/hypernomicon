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

package org.hypernomicon.bib.authors;

import java.util.Iterator;

import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.AuthorStandalone;
import org.hypernomicon.model.records.HDT_Work;

//---------------------------------------------------------------------------

public class WorkBibAuthors extends BibAuthors
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HDT_Work work;

//---------------------------------------------------------------------------

  public WorkBibAuthors(HDT_Work work) { this.work = work; }

//---------------------------------------------------------------------------

  @Override public Iterator<Author> iterator() { return work.getAuthors().stream().map(AuthorStandalone::new).map(a -> (Author)a).iterator(); }
  @Override public boolean isEmpty()           { return work.getAuthors().isEmpty(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
