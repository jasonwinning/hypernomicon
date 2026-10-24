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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.EnumSet;
import java.util.NoSuchElementException;

import org.hypernomicon.model.TestHyperDB;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Tests for the sets of record types that {@link RecordType} defines.
 */
class RecordTypeTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void allRealRecordTypesAreExactlyTheTypesTheDatabaseKeepsRecordsOf()
  {
    for (RecordType type : RecordType.values())
    {
      if (allRealRecordTypes().contains(type))
        assertDoesNotThrow(() -> db.records(type), type + " should have records");
      else
        assertThrows(NoSuchElementException.class, () -> db.records(type), type + " should have no records");
    }
  }

//---------------------------------------------------------------------------

  @Test
  void exceptHubsLeavesOutHubsAndNothingElse()
  {
    EnumSet<RecordType> leftOut = allRealRecordTypes();
    leftOut.removeAll(allRealRecordTypesExceptHubs());

    assertEquals(EnumSet.of(hdtHub), leftOut);
    assertTrue(allRealRecordTypes().containsAll(allRealRecordTypesExceptHubs()));
  }

//---------------------------------------------------------------------------

  @Test
  void eachCallReturnsASetOfItsOwn()
  {
    allRealRecordTypes          ().remove(hdtFolder);
    allRealRecordTypesExceptHubs().remove(hdtFolder);

    assertTrue(allRealRecordTypes          ().contains(hdtFolder), "trimming one set should not affect the next");
    assertTrue(allRealRecordTypesExceptHubs().contains(hdtFolder), "trimming one set should not affect the next");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
