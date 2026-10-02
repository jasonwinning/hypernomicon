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

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.HDT_Work;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Tests {@link SubjectOrderTracker} on its own, with a plain list standing in for a relation's
 * subject list. Records come from the test database only because the tracker is keyed by record.
 */
class SubjectOrderTrackerTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

  private SubjectOrderTracker<HDT_Work, HDT_Work> tracker;
  private List<HDT_Work> chapters;
  private HDT_Work book;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp()
  {
    tracker = new SubjectOrderTracker<>();
    chapters = new ArrayList<>();
    book = db.createNewBlankRecord(hdtWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void loadedSubjectsArePlacedByTheirSavedPositionsWhateverTheirArrivalOrder()
  {
    HDT_Work first = db.createNewBlankRecord(hdtWork), second = db.createNewBlankRecord(hdtWork), third = db.createNewBlankRecord(hdtWork);

    tracker.placeLoadedSubject(chapters, book, second, 2);
    tracker.placeLoadedSubject(chapters, book, third , 3);
    tracker.placeLoadedSubject(chapters, book, first , 1);

    assertEquals(List.of(first, second, third), chapters);
    assertTrue(tracker.isArranged(book));
  }

//---------------------------------------------------------------------------

  @Test
  void subjectsWithoutASavedPositionStayAfterThoseWithOne()
  {
    HDT_Work first = db.createNewBlankRecord(hdtWork), second = db.createNewBlankRecord(hdtWork),
             unplacedEarly = db.createNewBlankRecord(hdtWork), unplacedLate = db.createNewBlankRecord(hdtWork);

    chapters.add(unplacedEarly);  // Brought online before any sibling with a position

    tracker.placeLoadedSubject(chapters, book, second, 2);
    tracker.placeLoadedSubject(chapters, book, first , 1);

    chapters.add(unplacedLate);

    assertEquals(List.of(first, second, unplacedEarly, unplacedLate), chapters);
  }

//---------------------------------------------------------------------------

  @Test
  void anObjectCountsAsArrangedOnlyOnceItHasBeen()
  {
    assertFalse(tracker.isArranged(book));

    tracker.markArranged(book);

    assertTrue(tracker.isArranged(book));
  }

//---------------------------------------------------------------------------

  @Test
  void anExpiredObjectIsForgotten() throws Exception
  {
    tracker.markArranged(book);

    db.deleteRecord(book);
    tracker.dropExpired();

    assertFalse(tracker.isArranged(book));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
