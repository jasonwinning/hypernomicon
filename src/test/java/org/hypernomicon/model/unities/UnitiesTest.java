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

package org.hypernomicon.model.unities;

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;

import org.junit.jupiter.api.*;

import java.util.List;

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

    assertTrue(hub.disuniteRecord(hdtPosition));

    assertFalse(HDT_Record.isEmpty(hub, false));

    assertFalse(position.hasHub());
    assertNull(position.getHub());

    assertEquals(2, hub.getSpokes().count());

    assertNull(hub.getPosition());

    assertEquals(html2, position.getMainText().getHtml());
    assertEquals(html2, note    .getMainText().getHtml());
    assertEquals(html2, label   .getMainText().getHtml());
    assertEquals(html2, hub     .getMainText().getHtml());

    position.getMainText().setHtml(html1);

    assertNotEquals(html2, position.getMainText().getHtml());

    assertEquals(html2, note .getMainText().getHtml());
    assertEquals(html2, label.getMainText().getHtml());
    assertEquals(html2, hub  .getMainText().getHtml());

    final String html3 = "<html><head></head><body>Something else</body></html>";

    note.getMainText().setHtml(html3);

    assertNotEquals(html3, position.getMainText().getHtml());

    assertEquals(html3, note .getMainText().getHtml());
    assertEquals(html3, label.getMainText().getHtml());
    assertEquals(html3, hub  .getMainText().getHtml());

    assertFalse(hub.disuniteRecord(hdtPosition));

    assertFalse(HDT_Record.isEmpty(hub, false));

// Disunite the note

    assertTrue(hub.disuniteRecord(hdtNote));

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

  /**
   * Only hand-edited files produce a record that names a hub which does not list it. Such a record
   * shares the hub's text, so deleting it must leave that text, its key works and its display
   * registrations intact for the hub's real spokes.
   */
  @Test
  void deletingARecordItsHubDoesNotListLeavesTheSharedTextAlone()
  {
    HDT_Position position = db.createNewBlankRecord(hdtPosition);
    HDT_Note note = db.createNewBlankRecord(hdtNote);
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    HDT_Debate displayed = db.createNewBlankRecord(hdtDebate);

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(position, note, "<p>shared</p>"));

    HDT_Hub hub = position.getHub();

    position.getMainText().setKeyWorksFromList(List.of(new KeyWork(work)));
    position.getMainText().setDisplayItemsFromList(List.of(new MainText.DisplayItem(displayed)));

    HDT_RecordWithMainText phantom = db.createNewBlankRecord(hdtDebate);
    phantom.hub = hub;                     // What such a file produces: the record names the hub and so
    phantom.mainText = hub.getMainText();  // shares its text, but the hub does not list the record

    db.deleteRecord(phantom);

    assertSame(hub, position.getHub());
    assertEquals(2, hub.getSpokes().count());
    assertEquals(List.of(work), hub.getMainText().keyWorksStream().map(KeyWork::getRecord).toList(), "The shared key works survive");
    assertTrue(db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == position), "and stay registered for the real spokes");
    assertTrue(db.displayerStream(displayed).anyMatch(displayer -> displayer == position), "The shared text still counts as displaying what it displays");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
