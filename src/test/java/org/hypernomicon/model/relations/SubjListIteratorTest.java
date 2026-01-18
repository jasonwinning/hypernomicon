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

import java.util.*;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class SubjListIteratorTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;
  private static HDT_Debate debate;
  private static List<HDT_Position> srcList;
  private static HyperSubjList<HDT_Position, HDT_Debate> subjList;
  private static HyperSubjSubList<HDT_Position, HDT_Debate> subjSubList;

  private Iterator<HDT_Position> subjIterator;
  private ListIterator<HDT_Position> subjListIterator;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();

    debate = db.createNewBlankRecord(hdtDebate);

    srcList = new ArrayList<>(Arrays.asList
    (
      db.createNewBlankRecord(hdtPosition),
      db.createNewBlankRecord(hdtPosition),
      db.createNewBlankRecord(hdtPosition),
      db.createNewBlankRecord(hdtPosition),
      db.createNewBlankRecord(hdtPosition)
    ));
  }

//---------------------------------------------------------------------------

  @BeforeEach
  @SuppressWarnings("unchecked")
  void setUp()
  {
    // This code will run before each test

    srcList.forEach(position ->
    {
      position.largerDebates.clear();
      position.largerDebates.add(debate);
    });

    subjIterator = debate.subPositions.iterator();
    subjListIterator = debate.subPositions.listIterator();

    subjList = (HyperSubjList<HDT_Position, HDT_Debate>) debate.subPositions;
    subjSubList = (HyperSubjSubList<HDT_Position, HDT_Debate>) subjList.subList(1, 4); // SubList from index 1 to 4
  }

//---------------------------------------------------------------------------

  @Test
  @Order(1)
  void testHasNext()
  {
    assertTrue(subjIterator.hasNext());
    subjIterator.next();
    subjIterator.next();
    subjIterator.next();
    subjIterator.next();
    subjIterator.next();
    assertFalse(subjIterator.hasNext());

    assertTrue(subjListIterator.hasNext());
    subjListIterator.next();
    subjListIterator.next();
    subjListIterator.next();
    subjListIterator.next();
    subjListIterator.next();
    assertFalse(subjListIterator.hasNext());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(2)
  void testHasPrevious()
  {
    subjListIterator.next();
    subjListIterator.next();
    assertTrue(subjListIterator.hasPrevious());
    subjListIterator.previous();
    subjListIterator.previous();
    assertFalse(subjListIterator.hasPrevious());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(3)
  void testPrevious()
  {
    subjListIterator.next();
    subjListIterator.next();
    assertNotNull(subjListIterator.previous());
    assertNotNull(subjListIterator.previous());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(4)
  void testNextIndex()
  {
    assertEquals(0, subjListIterator.nextIndex());
    subjListIterator.next();
    assertEquals(1, subjListIterator.nextIndex());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(5)
  void testPreviousIndex()
  {
    assertEquals(-1, subjListIterator.previousIndex());
    subjListIterator.next();
    assertEquals(0, subjListIterator.previousIndex());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(10)
  void testNoSuchElementException()
  {
    subjIterator.next();
    subjIterator.next();
    subjIterator.next();
    subjIterator.next();
    subjIterator.next();
    assertThrows(NoSuchElementException.class, () -> subjIterator.next());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(12)
  void testNoSuchElementExceptionList()
  {
    subjListIterator.next();
    subjListIterator.next();
    subjListIterator.next();
    subjListIterator.next();
    subjListIterator.next();
    assertThrows(NoSuchElementException.class, () -> subjListIterator.next());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(14)
  void testComodificationOnDelete()
  {
    HDT_Position newPosition = db.createNewBlankRecord(hdtPosition);
    newPosition.largerDebates.add(debate);

    subjIterator = debate.subPositions.iterator();
    subjListIterator = debate.subPositions.listIterator();

    subjIterator.next();
    subjListIterator.next();

    db.deleteRecord(newPosition);

    assertThrows(ConcurrentModificationException.class, subjIterator::next);

    assertThrows(UnsupportedOperationException.class, subjIterator::remove);

    assertThrows(ConcurrentModificationException.class, subjListIterator::next);
    assertThrows(ConcurrentModificationException.class, subjListIterator::previous);

    assertThrows(UnsupportedOperationException.class, subjListIterator::remove);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(15)
  void testComodificationOnRemove()
  {
    subjIterator.next();
    subjListIterator.next();

    srcList.get(1).largerDebates.remove(debate);

    assertThrows(ConcurrentModificationException.class, subjIterator::next);
    assertThrows(ConcurrentModificationException.class, subjListIterator::next);
    assertThrows(ConcurrentModificationException.class, subjListIterator::previous);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(16)
  void testComodificationOnAdd()
  {
    subjIterator.next();
    subjListIterator.next();

    srcList.get(1).largerDebates.add(db.createNewBlankRecord(hdtDebate));

    subjIterator.next();
    subjListIterator.next();

    HDT_Position newPosition = db.createNewBlankRecord(hdtPosition);
    newPosition.largerDebates.add(debate);

    assertThrows(ConcurrentModificationException.class, subjListIterator::next);
    assertThrows(ConcurrentModificationException.class, subjIterator::next);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(999)
  void testComodificationOnLoadDB()
  {
    subjIterator.next();
    subjListIterator.next();

    db.closeAndOpen();

    assertThrows(NoSuchElementException.class, () -> srcList.getFirst().largerDebates.add(debate));
    assertThrows(NoSuchElementException.class, () -> srcList.getFirst().largerDebates.add(db.createNewBlankRecord(hdtDebate)));

    HDT_Position newPosition = db.createNewBlankRecord(hdtPosition);

    assertThrows(NoSuchElementException.class, () -> newPosition.largerDebates.add(debate));

    assertThrows(NoSuchElementException.class, () -> subjIterator.next());

    assertThrows(NoSuchElementException.class, () -> subjListIterator.next());

    assertThrows(ConcurrentModificationException.class, () -> subjListIterator.previous());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
//
//   SubList Tests
//
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  @Order(100)
  void testSize()
  {
    assertEquals(3, subjSubList.size());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(101)
  void testIsEmpty()
  {
    assertFalse(subjSubList.isEmpty());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(103)
  void testGet()
  {
    assertEquals(subjList.get(1), subjSubList.get(0));
    assertEquals(subjList.get(2), subjSubList.get(1));
    assertEquals(subjList.get(3), subjSubList.get(2));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(104)
  void testToArray()
  {
    Object[] array = subjSubList.toArray();
    assertEquals(3, array.length);
    assertEquals(subjList.get(1), array[0]);
    assertEquals(subjList.get(2), array[1]);
    assertEquals(subjList.get(3), array[2]);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(105)
  void testToArrayTyped()
  {
    HDT_Position[] array = subjSubList.toArray(new HDT_Position[0]);
    assertEquals(3, array.length);
    assertEquals(subjList.get(1), array[0]);
    assertEquals(subjList.get(2), array[1]);
    assertEquals(subjList.get(3), array[2]);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(106)
  void testContains()
  {
    assertTrue(subjSubList.contains(subjList.get(2)));
    assertFalse(subjSubList.contains(subjList.get(0)));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(112)
  void testSubList()
  {
    List<HDT_Position> innerSubList = subjSubList.subList(1, 3);
    assertEquals(2, innerSubList.size());
    assertEquals(subjList.get(2), innerSubList.get(0));
    assertEquals(subjList.get(3), innerSubList.get(1));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(115)
  void testIndexOf()
  {
    assertEquals(0, subjSubList.indexOf(subjList.get(1)));
    assertEquals(1, subjSubList.indexOf(subjList.get(2)));
    assertEquals(-1, subjSubList.indexOf(subjList.get(0))); // Not in sublist
  }

//---------------------------------------------------------------------------

  @Test
  @Order(116)
  void testLastIndexOf()
  {
    assertEquals(0, subjSubList.lastIndexOf(subjList.get(1)));
    assertEquals(2, subjSubList.lastIndexOf(subjList.get(3)));
    assertEquals(-1, subjSubList.lastIndexOf(subjList.get(4))); // Not in sublist
  }

//---------------------------------------------------------------------------

  @Test
  @Order(117)
  void testContainsAll()
  {
    List<HDT_Position> subListRecords = Arrays.asList(subjList.get(1), subjList.get(2), subjList.get(3));
    assertTrue(subjSubList.containsAll(subListRecords));
    assertFalse(subjSubList.containsAll(Arrays.asList(subjList.get(0), subjList.get(1))));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(118)
  @SuppressWarnings("unchecked")
  void testEmptySubList()
  {
    subjSubList = (HyperSubjSubList<HDT_Position, HDT_Debate>) debate.subPositions.subList(1, 1); // Empty sublist
    assertTrue(subjSubList.isEmpty());
    assertEquals(0, subjSubList.size());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(119)
  @SuppressWarnings("unchecked")
  void testBoundarySubList()
  {
    subjSubList = (HyperSubjSubList<HDT_Position, HDT_Debate>) debate.subPositions.subList(0, 5); // Full range
    assertFalse(subjSubList.isEmpty());
    assertEquals(5, subjSubList.size());
    assertEquals(subjList.getFirst(), subjSubList.getFirst());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(120)
  void testInvalidIndices()
  {
    // Invalid get indices
    assertThrows(IndexOutOfBoundsException.class, () -> subjSubList.get(-1));
    assertThrows(IndexOutOfBoundsException.class, () -> subjSubList.get(4)); // size is 3

    // Invalid subjSubList indices
    assertThrows(IndexOutOfBoundsException.class, () -> subjSubList.subList(-1, 2));
    assertThrows(IndexOutOfBoundsException.class, () -> subjSubList.subList(1, 4));
    assertThrows(IndexOutOfBoundsException.class, () -> subjSubList.subList(3, 2));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
