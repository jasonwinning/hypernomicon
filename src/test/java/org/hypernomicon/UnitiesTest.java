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

class UnitiesTest
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
  void testUniteAndDisunite()
  {

// Create the records

    HDT_Position position = db.createNewBlankRecord(hdtPosition);
    HDT_Note note = db.createNewBlankRecord(hdtNote);
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);

    final String html1 = "<html><head></head><body>Hello world!</body></html>";

    position.getMainText().setHtml(html1);

    assertFalse(position.hasHub());
    assertFalse(note    .hasHub());
    assertFalse(label   .hasHub());

// Unite the position and note together

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(position, note, position.getMainText().getHtml()));

    assertTrue(position.hasHub());
    assertTrue(note    .hasHub());

    HDT_Hub hub = position.getHub();
    assertNotNull(hub);

    assertEquals(hub, note.getHub());

    assertEquals(html1, note.getMainText().getHtml());
    assertEquals(html1, hub .getMainText().getHtml());

    assertEquals(note, hub.getNote());
    assertEquals(position, hub.getPosition());

    final String html2 = "<html><head></head><body>Goodbye!</body></html>";

// Unite the label and note together

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(label, note, html2));

    assertEquals(html2, position.getMainText().getHtml());

    assertEquals(label, position.getHub().getLabel());
    assertEquals(html2, label.getMainText().getHtml());

    assertEquals(3, hub.getSpokes().count());

// Disunite the position

    assertTrue(hub.disuniteRecord(hdtPosition, true));

    assertFalse(HDT_Record.isEmpty(hub, false));

    assertFalse(position.hasHub());
    assertNull(position.getHub());

    assertEquals(2, hub.getSpokes().count());

    assertNull(hub.getPosition());

    assertEquals(position.getMainText().getHtml(), html2);
    assertEquals(note    .getMainText().getHtml(), html2);
    assertEquals(label   .getMainText().getHtml(), html2);
    assertEquals(hub     .getMainText().getHtml(), html2);

    position.getMainText().setHtml(html1);

    assertNotEquals(position.getMainText().getHtml(), html2);

    assertEquals(note .getMainText().getHtml(), html2);
    assertEquals(label.getMainText().getHtml(), html2);
    assertEquals(hub  .getMainText().getHtml(), html2);

    final String html3 = "<html><head></head><body>Something else</body></html>";

    note.getMainText().setHtml(html3);

    assertNotEquals(position.getMainText().getHtml(), html3);

    assertEquals(note .getMainText().getHtml(), html3);
    assertEquals(label.getMainText().getHtml(), html3);
    assertEquals(hub  .getMainText().getHtml(), html3);

    assertFalse(hub.disuniteRecord(hdtPosition, true));

    assertFalse(HDT_Record.isEmpty(hub, false));

// Disunite the note

    assertTrue(hub.disuniteRecord(hdtNote, true));

    assertTrue(HDT_Record.isEmpty(hub, false));

    assertFalse(note.hasHub());
    assertNull(note.getHub());

    assertFalse(label.hasHub());
    assertNull(label.getHub());

    assertEquals(html3, note .getMainText().getHtml());
    assertEquals("", label.getMainText().getHtml());    // html gets cleared out when disuniting a work label

    note.getMainText().setHtml(html1);

    assertEquals(html1, note .getMainText().getHtml());
    assertEquals("", label.getMainText().getHtml());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
