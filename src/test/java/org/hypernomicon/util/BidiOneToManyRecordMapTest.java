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

package org.hypernomicon;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.util.BidiOneToManyRecordMap;

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Set;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

class BidiOneToManyRecordMapTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BidiOneToManyRecordMap map;

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp()
  {
    map = new BidiOneToManyRecordMap();
  }

//---------------------------------------------------------------------------

  @Test
  void testAddForwardWithHub()
  {
    HDT_Note note = db.createNewBlankRecord(hdtNote);
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(note, debate, ""));

    HDT_Person person = db.createNewBlankRecord(hdtPerson);

    map.addForward(person, debate.getHub());
    Set<HDT_Record> forwardSet = map.getForwardSet(person);

    assertTrue(forwardSet.contains(note));

    Set<HDT_Record> reverseSet = map.getReverseSet(debate);

    assertTrue(reverseSet.contains(person));
  }

//---------------------------------------------------------------------------

  @Test
  void testAddForwardFromRecordToHub()
  {
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);
    HDT_Note note = db.createNewBlankRecord(hdtNote);

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(note, label, ""));

    HDT_Person person = db.createNewBlankRecord(hdtPerson);

    map.addForward(note.getHub(), person);
    Set<HDT_Record> reverseSet = map.getReverseSet(person);

    assertTrue(reverseSet.contains(label));
  }

//---------------------------------------------------------------------------

  @Test
  void testGetForwardRecordSet()
  {
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);
    HDT_Note note = db.createNewBlankRecord(hdtNote);

    map.addForward(note, label);
    Set<HDT_Record> forwardSet = map.getForwardRecordSet(note);

    assertTrue(forwardSet.contains(label));
  }

//---------------------------------------------------------------------------

  @Test
  void testGetReverseRecordSet()
  {
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);
    HDT_Note note = db.createNewBlankRecord(hdtNote);

    map.addForward(note, label);
    Set<HDT_Record> reverseSet = map.getReverseRecordSet(label);

    assertTrue(reverseSet.contains(note));
  }

//---------------------------------------------------------------------------

  @Test
  void testGetAllHeads()
  {
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);
    HDT_Note note = db.createNewBlankRecord(hdtNote);

    map.addForward(label, note);

    Set<HDT_Record> heads = map.getAllHeads();
    assertTrue(heads.contains(label));
    assertFalse(heads.contains(note));
  }

//---------------------------------------------------------------------------

  @Test
  void testGetAllHeadsWithEmptyForwardSet()
  {
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);
    HDT_Note note = db.createNewBlankRecord(hdtNote);

    map.addForward(label, note);
    map.removeForward(label, note);

    Set<HDT_Record> heads = map.getAllHeads();
    assertFalse(heads.contains(label));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
