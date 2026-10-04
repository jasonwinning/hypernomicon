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

package org.hypernomicon.bib;

import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

/**
 * One object of the reference manager library as Hypernomicon holds it: an entry ({@link BibEntry}, an
 * item in Zotero or a document in Mendeley) or a collection ({@link BibCollection}, a Zotero collection
 * or a Mendeley folder). Each is kept as the JSON the server sent for it. The {@link LibraryWrapper}
 * holds the entities of a library under their keys ({@link #getKey}), synchronizes them with the server,
 * and has each write itself ({@link #saveToJsonArray}) to the library file, Bib.json in the database
 * folder, from which the whole library is read back at the next load. {@link #update} is how an entity
 * takes on new data, from that file or from the server, and {@link #isSynced} says whether it carries
 * local changes the server has not seen; a collection never does, since Hypernomicon does not edit
 * collections.
 */
public interface BibEntity
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean isSynced();

  /**
   * Makes {@code jObj} this entity's data. The object comes from one of four places: the library file,
   * where {@link #saveToJsonArray} wrote it; the server, when the entity was downloaded during a sync,
   * because Hypernomicon did not hold it before, because the server's version is newer than the one held,
   * or because both sides changed it; the server's reply to an upload Hypernomicon just made, which carries
   * the version number and, for a new entity, the key the server assigned; or, when the server reports an
   * upload as unchanged, the entry's own backup copy, taken back as its current data. The entity keeps the
   * object itself, not a copy. An implementation that holds nothing outside the JSON, as a collection does
   * not, has only to keep it.
   * @param jObj the entity's new data
   * @param updatingExistingDataFromServer true when {@code jObj} is the server's current version of an
   *                                       entity Hypernomicon already holds, so that whatever Hypernomicon
   *                                       keeps outside the JSON (the fields of a linked work record, for
   *                                       an entry) must be brought into line with it; false for data read
   *                                       from the library file, for an entity Hypernomicon did not hold
   *                                       before, and for the reply to an upload, whose data Hypernomicon
   *                                       already has
   * @param preMerge true when the caller is about to show the merge dialog for this entity, which will
   *                 settle the differences itself, so that the update must not start a merge of its own;
   *                 an entry is updated this way when the server changed it while it held local changes
   */
  void update(JsonObj jObj, boolean updatingExistingDataFromServer, boolean preMerge);

  String getKey();
  void saveToJsonArray(JsonArray jArr);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
