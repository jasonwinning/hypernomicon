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

package org.hypernomicon.model.relations;

import java.util.*;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.records.HDT_Record;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * Keeps, for one {@link RelationSet}, the facts behind the {@code ord} attribute that a subject's
 * pointer tag is saved with: which objects have had their subject lists arranged by the user, and,
 * while the database is loading, the positions read from the files.
 * <p>
 * The order of a subject list is the list itself; nothing here holds a position once loading is
 * over. What this class answers at save time is whether an object's subjects are to be saved with
 * their positions, which is so once the user has arranged that object's list or a file recorded a
 * position for any of its subjects. A subject added to or moved under such an object therefore gets
 * a position at the next save with no bookkeeping of its own, and a file written by an earlier
 * version with positions for only some of an object's subjects is rewritten with positions for all
 * of them.
 */
final class SubjectOrderTracker<HDT_Subj extends HDT_Record, HDT_Obj extends HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Set<HDT_Obj> arrangedObjects = new HashSet<>();
  private final Map<HDT_Subj, Integer> loadedOrds = new HashMap<>();  // Only meaningful while the database is loading

//---------------------------------------------------------------------------

  boolean isArranged(HDT_Obj obj) { return arrangedObjects.contains(obj); }
  void markArranged(HDT_Obj obj)  { arrangedObjects.add(obj); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Inserts a subject being brought online into its object's subject list at the position its
   * record was saved with, among the siblings brought online before it. A sibling saved without a
   * position (a file written before every subject of an arranged object was saved with one) stays
   * after the siblings that have one, in the order in which it was brought online. The object
   * counts as arranged from then on.
   */
  void placeLoadedSubject(List<HDT_Subj> subjList, HDT_Obj obj, HDT_Subj subj, int ord)
  {
    loadedOrds.put(subj, ord);
    arrangedObjects.add(obj);

    addToSortedList(subjList, subj, Comparator.comparingInt(sibling -> loadedOrds.getOrDefault(sibling, Integer.MAX_VALUE)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Forgets records that have expired, as {@link RelationSet#cleanup} does for its other structures.
   */
  void dropExpired() throws HDB_InternalError
  {
    Iterator<HDT_Obj> objIt = arrangedObjects.iterator();

    while (objIt.hasNext())
      if (HDT_Record.isEmptyThrowsException(objIt.next(), false))
        objIt.remove();

    Iterator<HDT_Subj> subjIt = loadedOrds.keySet().iterator();

    while (subjIt.hasNext())
      if (HDT_Record.isEmptyThrowsException(subjIt.next(), false))
        subjIt.remove();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
