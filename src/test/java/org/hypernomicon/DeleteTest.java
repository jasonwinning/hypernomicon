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

package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

class DeleteTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @Test
  void test()
  {
    HDT_Position position = db.createNewBlankRecord(hdtPosition);

    db.attachOrphansToRoots();

    HDT_Debate rootDebate = position.getLargerDebate();  // Root debate is assigned automatically to new position

    assertNotNull(rootDebate);
    assertEquals(1, rootDebate.getID());

    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);

    db.attachOrphansToRoots();

    position.largerDebates.set(0, debate);

    assertFalse(position.largerDebates.contains(rootDebate));
    assertFalse(rootDebate.subPositions.contains(position));
    assertEquals(1, position.largerDebates.size());
    assertTrue(position.largerDebates.contains(debate));
    assertTrue(debate.subPositions.contains(position));

    HDT_Position subPosition = db.createNewBlankRecord(hdtPosition);
    subPosition.largerPositions.add(position);

    HDT_Note unitedNote = db.createNewBlankRecord(hdtNote);

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(position, unitedNote, debate.getMainText().getHtml()));

    assertTrue(unitedNote.hasHub());

    db.deleteRecord(position);

    assertTrue(HDT_Record.isEmpty(position, false));

    assertTrue(debate.subPositions.isEmpty());
    assertTrue(subPosition.largerPositions.isEmpty());

    assertFalse(unitedNote.hasHub());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
