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

import static org.hypernomicon.model.records.RecordType.*;
import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;
import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class SubjListIteratorTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;
  private static HDT_Debate debate;
  private static List<HDT_Position> list;

  private Iterator<HDT_Position> subjIterator;
  private ListIterator<HDT_Position> subjListIterator;

//---------------------------------------------------------------------------

  @BeforeAll
  public static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();

    debate = db.createNewBlankRecord(hdtDebate);

    list = new ArrayList<>(Arrays.asList
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
  public void setUp()
  {
    // This code will run before each test

    list.forEach(position ->
    {
      position.largerDebates.clear();
      position.largerDebates.add(debate);
    });

    subjIterator = debate.subPositions.iterator();
    subjListIterator = debate.subPositions.listIterator();
  }

//---------------------------------------------------------------------------

  @Test
  @Order(1)
  public void testHasNext()
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
  public void testHasPrevious()
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
  public void testPrevious()
  {
    subjListIterator.next();
    subjListIterator.next();
    assertNotNull(subjListIterator.previous());
    assertNotNull(subjListIterator.previous());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(4)
  public void testNextIndex()
  {
    assertEquals(0, subjListIterator.nextIndex());
    subjListIterator.next();
    assertEquals(1, subjListIterator.nextIndex());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(5)
  public void testPreviousIndex()
  {
    assertEquals(-1, subjListIterator.previousIndex());
    subjListIterator.next();
    assertEquals(0, subjListIterator.previousIndex());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(10)
  public void testNoSuchElementException()
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
  public void testNoSuchElementExceptionList()
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
  public void testComodificationOnDelete()
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
  public void testComodificationOnRemove()
  {
    subjIterator.next();
    subjListIterator.next();

    list.get(1).largerDebates.remove(debate);

    assertThrows(ConcurrentModificationException.class, subjIterator::next);
    assertThrows(ConcurrentModificationException.class, subjListIterator::next);
    assertThrows(ConcurrentModificationException.class, subjListIterator::previous);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(16)
  public void testComodificationOnAdd()
  {
    subjIterator.next();
    subjListIterator.next();

    list.get(1).largerDebates.add(db.createNewBlankRecord(hdtDebate));

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
  public void testComodificationOnLoadDB()
  {
    subjIterator.next();
    subjListIterator.next();

    db.closeAndOpen();

    assertThrows(NoSuchElementException.class, () -> list.get(0).largerDebates.add(debate));
    assertThrows(NoSuchElementException.class, () -> list.get(0).largerDebates.add(db.createNewBlankRecord(hdtDebate)));

    HDT_Position newPosition = db.createNewBlankRecord(hdtPosition);

    assertThrows(NoSuchElementException.class, () -> newPosition.largerDebates.add(debate));

    assertThrows(NoSuchElementException.class, () -> subjIterator.next());

    assertThrows(NoSuchElementException.class, () -> subjListIterator.next());

    assertThrows(ConcurrentModificationException.class, () -> subjListIterator.previous());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
