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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.db;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.RecordAuthors;

//---------------------------------------------------------------------------

public interface HDT_RecordWithAuthors<AuthorsType extends RecordAuthors> extends HDT_Record
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  AuthorsType getAuthors();

  /**
   * <p>Sets the search key to a generated string instead of using one of the record's active search keys.</p>
   * <p>Tries to create a search key based on information from the work record; if unable to do that,
   * makes one that includes information from the parent work.</p>
   * <p>For a non-work record, just sets it to the record name.</p>
   */
  String makeKeyWorkSearchKey();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  default String getShortAuthorsStr(boolean fnis)         { return Author.getShortAuthorsStr(getAuthors().stream(), false, fnis); }
  default String getLongAuthorsStr ()                     { return Author.getLongAuthorsStr (getAuthors().stream()             ); }

  default Set<HDT_Investigation> investigationSet()       { return investigationStream().collect(Collectors.toSet()); }

  default Stream<HDT_Investigation> investigationStream() { return db.keyWorkMentionerStream(this, HDT_Investigation.class); }
  default Stream<HDT_WorkLabel    > labelStream        () { return db.keyWorkMentionerStream(this, HDT_WorkLabel    .class); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
