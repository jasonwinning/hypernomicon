/*
 * Copyright 2015-2024 Jason Winning
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

import java.util.Set;
import java.util.stream.Collectors;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.HDT_Note;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.util.BidiOneToManyMap;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

class BidiOneToManyMapTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BidiOneToManyMap<String> map;

  @BeforeEach
  void setUp()
  {
    map = new BidiOneToManyMap<>();
  }

  @Test
  void testAddForward()
  {
    map.addForward("key1", "value1");
    assertTrue(map.getForwardSet("key1").contains("value1"));
    assertTrue(map.getReverseSet("value1").contains("key1"));
  }

  @Test
  void testAddForwardDuplicate()
  {
    map.addForward("key1", "value1");
    map.addForward("key1", "value1");
    assertEquals(1, map.getForwardSet("key1").size());
  }

  @Test
  void testRemoveForward()
  {
    map.addForward("key1", "value1");
    map.removeForward("key1", "value1");
    assertFalse(map.getForwardSet("key1").contains("value1"));
    assertFalse(map.getReverseSet("value1").contains("key1"));
  }

  @Test
  void testRemoveForwardNonExistent()
  {
    map.removeForward("key1", "value1");
    assertTrue(map.getForwardSet("key1").isEmpty());
    assertTrue(map.getReverseSet("value1").isEmpty());
  }

  @Test
  void testRemoveItem()
  {
    map.addForward("key1", "value1");
    map.addForward("key1", "value2");
    map.removeItem("key1");
    assertTrue(map.getForwardSet("key1").isEmpty());
    assertFalse(map.getReverseSet("value1").contains("key1"));
    assertFalse(map.getReverseSet("value2").contains("key1"));
  }

  @Test
  void testRemoveReverseKey()
  {
    map.addForward("key1", "value1");
    map.addForward("key2", "value1");
    map.removeReverseKey("value1");
    assertTrue(map.getForwardSet("key1").isEmpty());
    assertTrue(map.getForwardSet("key2").isEmpty());
    assertTrue(map.getReverseSet("value1").isEmpty());
  }

  @Test
  void testReplaceItem()
  {
    map.addForward("key1", "value1");
    map.addForward("key1", "value2");
    map.replaceItem("key1", "key2");
    assertTrue(map.getForwardSet("key2").contains("value1"));
    assertTrue(map.getForwardSet("key2").contains("value2"));
    assertFalse(map.getForwardSet("key1").contains("value1"));
    assertFalse(map.getForwardSet("key1").contains("value2"));
  }

  @Test
  void testReplaceItemNull()
  {
    map.addForward("key1", "value1");
    map.replaceItem("key1", null);
    assertTrue(map.getForwardSet("key1").contains("value1"));
    assertTrue(map.getReverseSet("value1").contains("key1"));
  }

  @Test
  void testReplaceItemSame()
  {
    map.addForward("key1", "value1");
    map.replaceItem("key1", "key1");
    assertTrue(map.getForwardSet("key1").contains("value1"));
    assertTrue(map.getReverseSet("value1").contains("key1"));
  }

  @Test
  void testClear()
  {
    map.addForward("key1", "value1");
    map.clear();
    assertTrue(map.getForwardSet("key1").isEmpty());
    assertTrue(map.getReverseSet("value1").isEmpty());
  }

  @Test
  void testGetForwardStream()
  {
    map.addForward("key1", "value1");
    map.addForward("key1", "value2");
    Set<String> values = map.getForwardStream("key1").collect(Collectors.toSet());
    assertTrue(values.contains("value1"));
    assertTrue(values.contains("value2"));
  }

  @Test
  void testGetForwardStreamEmpty()
  {
    Set<String> values = map.getForwardStream("key1").collect(Collectors.toSet());
    assertTrue(values.isEmpty());
  }

  @Test
  void testGetForwardStreamEntry()
  {
    map.addForward("key1", "value1");
    map.addForward("key2", "value2");
    assertTrue(map.getForwardStream().anyMatch(e -> "key1".equals(e.getKey()) && e.getValue().contains("value1")));
    assertTrue(map.getForwardStream().anyMatch(e -> "key2".equals(e.getKey()) && e.getValue().contains("value2")));
  }

  @Test
  void testGetForwardSet()
  {
    map.addForward("key1", "value1");
    Set<String> forwardSet = map.getForwardSet("key1");
    assertTrue(forwardSet.contains("value1"));
  }

  @Test
  void testGetReverseSet()
  {
    map.addForward("key1", "value1");
    Set<String> reverseSet = map.getReverseSet("value1");
    assertTrue(reverseSet.contains("key1"));
  }

  /**
   * MainText objects should only be equal if they are the numerically same object
   */
  @Test
  void testMainTextEquals()
  {
    TestHyperDB db = TestHyperDB.instance();

    HDT_Note note = db.createNewBlankRecord(RecordType.hdtNote);

    MainText mt1 = new MainText(note),
             mt2 = new MainText(note);

    assertNotEquals(mt1, mt2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
