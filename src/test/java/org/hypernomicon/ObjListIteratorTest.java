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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import org.junit.jupiter.api.*;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;

import static org.hypernomicon.model.records.RecordType.*;

//---------------------------------------------------------------------------

@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
public class ObjListIteratorTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;
  private static HDT_Position position;
  private static List<HDT_Debate> list;

  private Iterator<HDT_Debate> objIterator;
  private ListIterator<HDT_Debate> objListIterator;

//---------------------------------------------------------------------------

  @BeforeAll
  public static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();

    position = db.createNewBlankRecord(hdtPosition);

    list = new ArrayList<>(Arrays.asList
    (
      db.createNewBlankRecord(hdtDebate),
      db.createNewBlankRecord(hdtDebate),
      db.createNewBlankRecord(hdtDebate),
      db.createNewBlankRecord(hdtDebate),
      db.createNewBlankRecord(hdtDebate)
    ));
  }

//---------------------------------------------------------------------------

  @BeforeEach
  public void setUp()
  {
    // This code will run before each test

    position.largerDebates.clear();

    list.forEach(position.largerDebates::add);

    objIterator     = position.largerDebates.iterator();
    objListIterator = position.largerDebates.listIterator();
  }

//---------------------------------------------------------------------------

  @Test
  @Order(1)
  public void testHasNext()
  {
    assertTrue(objIterator.hasNext());
    objIterator.next();
    objIterator.next();
    objIterator.next();
    objIterator.next();
    objIterator.next();
    assertFalse(objIterator.hasNext());

    assertTrue(objListIterator.hasNext());
    objListIterator.next();
    objListIterator.next();
    objListIterator.next();
    objListIterator.next();
    objListIterator.next();
    assertFalse(objListIterator.hasNext());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(2)
  public void testHasPrevious()
  {
    objListIterator.next();
    objListIterator.next();
    assertTrue(objListIterator.hasPrevious());
    objListIterator.previous();
    objListIterator.previous();
    assertFalse(objListIterator.hasPrevious());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(3)
  public void testPrevious()
  {
    objListIterator.next();
    objListIterator.next();
    assertNotNull(objListIterator.previous());
    assertNotNull(objListIterator.previous());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(4)
  public void testNextIndex()
  {
    assertEquals(0, objListIterator.nextIndex());
    objListIterator.next();
    assertEquals(1, objListIterator.nextIndex());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(5)
  public void testPreviousIndex()
  {
    assertEquals(-1, objListIterator.previousIndex());
    objListIterator.next();
    assertEquals(0, objListIterator.previousIndex());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(6)
  public void testRemove()
  {
    objIterator.next();
    objIterator.remove();
    assertEquals(4, position.largerDebates.size());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(7)
  public void testListRemove()
  {
    objListIterator.next();
    objListIterator.remove();
    assertEquals(4, position.largerDebates.size());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(8)
  public void testSet()
  {
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    objListIterator.next();
    objListIterator.set(debate);
    assertEquals(debate, position.largerDebates.get(0));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(9)
  public void testAdd()
  {
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    objListIterator.add(debate);
    assertEquals(debate, position.largerDebates.get(0));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(10)
  public void testNoSuchElementException()
  {
    objIterator.next();
    objIterator.next();
    objIterator.next();
    objIterator.next();
    objIterator.next();
    assertThrows(NoSuchElementException.class, objIterator::next);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(11)
  public void testIllegalStateException()
  {
    assertThrows(IllegalStateException.class, objIterator::remove);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(12)
  public void testNoSuchElementExceptionList()
  {
    objListIterator.next();
    objListIterator.next();
    objListIterator.next();
    objListIterator.next();
    objListIterator.next();
    assertThrows(NoSuchElementException.class, objListIterator::next);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(13)
  public void testIllegalStateExceptionList()
  {
    assertThrows(IllegalStateException.class, objListIterator::remove);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(14)
  public void testComodificationOnDelete()
  {
    HDT_Debate newDebate = db.createNewBlankRecord(hdtDebate);
    position.largerDebates.add(newDebate);

    objIterator = position.largerDebates.iterator();
    objListIterator = position.largerDebates.listIterator();

    objIterator.next();
    objListIterator.next();

    db.deleteRecord(newDebate);

    assertThrows(ConcurrentModificationException.class, objIterator::next  );
    assertThrows(ConcurrentModificationException.class, objIterator::remove);

    assertThrows(ConcurrentModificationException.class, objListIterator::next    );
    assertThrows(ConcurrentModificationException.class, objListIterator::previous);
    assertThrows(ConcurrentModificationException.class, objListIterator::remove  );

    assertThrows(ConcurrentModificationException.class, () -> objListIterator.add(db.createNewBlankRecord(hdtDebate)));
    assertThrows(ConcurrentModificationException.class, () -> objListIterator.set(db.createNewBlankRecord(hdtDebate)));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(15)
  public void testComodificationOnRemove()
  {
    objIterator.next();
    objListIterator.next();

    objIterator.remove();

    objIterator.next();

    assertThrows(ConcurrentModificationException.class, objListIterator::next    );
    assertThrows(ConcurrentModificationException.class, objListIterator::previous);
    assertThrows(ConcurrentModificationException.class, objListIterator::remove  );

    assertThrows(ConcurrentModificationException.class, () -> objListIterator.add(db.createNewBlankRecord(hdtDebate)));
    assertThrows(ConcurrentModificationException.class, () -> objListIterator.set(db.createNewBlankRecord(hdtDebate)));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(16)
  public void testComodificationOnAdd()
  {
    objIterator.next();
    objListIterator.add(db.createNewBlankRecord(hdtDebate));

    assertThrows(ConcurrentModificationException.class, objIterator::remove);
    assertThrows(ConcurrentModificationException.class, objIterator::next  );

    Iterator<HDT_Debate> newIterator = position.largerDebates.iterator();

    newIterator.next();
    newIterator.remove();

    assertThrows(ConcurrentModificationException.class, objListIterator::next);
  }

//---------------------------------------------------------------------------

  @Test
  @Order(17)
  public void testEmptyList()
  {
    HDT_Position newPosition = db.createNewBlankRecord(hdtPosition);

    Iterator<HDT_Debate> emptyIterator = newPosition.largerDebates.iterator();

    assertFalse(emptyIterator.hasNext());
    assertThrows(NoSuchElementException.class, () -> emptyIterator.next());

    ListIterator<HDT_Debate> emptyListIterator = newPosition.largerDebates.listIterator();

    // Test hasNext and hasPrevious
    assertFalse(emptyListIterator.hasNext(), "Iterator should not have next element");
    assertFalse(emptyListIterator.hasPrevious(), "Iterator should not have previous element");

    // Test next() should throw NoSuchElementException
    assertThrows(NoSuchElementException.class, emptyListIterator::next, "Calling next() on empty list should throw NoSuchElementException");

    // Test previous() should throw NoSuchElementException
    assertThrows(NoSuchElementException.class, emptyListIterator::previous, "Calling previous() on empty list should throw NoSuchElementException");

    // Test remove() should throw IllegalStateException
    assertThrows(IllegalStateException.class, emptyListIterator::remove, "Calling remove() on empty list should throw IllegalStateException");

    // Test set() should throw IllegalStateException
    assertThrows(IllegalStateException.class, () -> emptyListIterator.set(db.createNewBlankRecord(hdtDebate)), "Calling set() on empty list should throw IllegalStateException");

    // Test nextIndex and previousIndex
    assertEquals(0, emptyListIterator.nextIndex(), "Next index should be 0");
    assertEquals(-1, emptyListIterator.previousIndex(), "Previous index should be -1");
  }

//---------------------------------------------------------------------------

  @Test
  @Order(18)
  public void testAddAndRemoveCombination()
  {
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    objListIterator.add(debate);
    assertEquals(6, position.largerDebates.size());
    objListIterator.previous();
    objListIterator.remove();
    assertEquals(5, position.largerDebates.size());
  }

//---------------------------------------------------------------------------

  @Test
  @Order(19)
  public void testBoundaryConditions()
  {
    objListIterator.next();
    objListIterator.remove();  // Remove first element
    assertEquals(4, position.largerDebates.size());

    objListIterator.next();
    objListIterator.next();
    objListIterator.next();
    objListIterator.next();
    objListIterator.remove();  // Remove last element
    assertEquals(3, position.largerDebates.size());

    HDT_Position newPosition = db.createNewBlankRecord(hdtPosition);
    HDT_Debate firstDebate = db.createNewBlankRecord(hdtDebate);
    HDT_Debate lastDebate = db.createNewBlankRecord(hdtDebate);

    newPosition.largerDebates.add(firstDebate);
    newPosition.largerDebates.add(lastDebate);

    ListIterator<HDT_Debate> oListIterator = newPosition.largerDebates.listIterator();

    // Test hasNext and next at the first element
    assertTrue(oListIterator.hasNext(), "Iterator should have next element");
    assertEquals(firstDebate, oListIterator.next(), "First element should be firstDebate");

    // Test nextIndex and previousIndex at the first element
    assertEquals(1, oListIterator.nextIndex(), "Next index should be 1 after first element");
    assertEquals(0, oListIterator.previousIndex(), "Previous index should be 0 after first element");

    // Move to the last element
    assertTrue(oListIterator.hasNext(), "Iterator should have next element");
    assertEquals(lastDebate, oListIterator.next(), "Next element should be lastDebate");

    // Test nextIndex and previousIndex at the last element
    assertEquals(2, oListIterator.nextIndex(), "Next index should be 2 after last element");
    assertEquals(1, oListIterator.previousIndex(), "Previous index should be 1 after last element");

    // Test previous at the last element
    assertTrue(oListIterator.hasPrevious(), "Iterator should have previous element");
    assertEquals(lastDebate, oListIterator.previous(), "Previous element should be lastDebate");

    // Test remove at the first element
    oListIterator.previous(); // Move back to first element
    oListIterator.remove();
    assertFalse(newPosition.largerDebates.contains(firstDebate), "List should not contain firstDebate after removal");
    assertEquals(1, newPosition.largerDebates.size(), "List size should be 1 after removing first element");

    // Test set at the remaining element
    oListIterator.next();
    HDT_Debate anotherDebate = db.createNewBlankRecord(hdtDebate);
    oListIterator.set(anotherDebate);
    assertEquals(anotherDebate, newPosition.largerDebates.get(0), "Element should be anotherDebate after set");

    // Test add at the boundary
    HDT_Debate yetAnotherDebate = db.createNewBlankRecord(hdtDebate);
    oListIterator.add(yetAnotherDebate);
    assertTrue(newPosition.largerDebates.contains(yetAnotherDebate), "List should contain yetAnotherDebate after add");
    assertEquals(2, newPosition.largerDebates.size(), "List size should be 2 after add");
  }

//---------------------------------------------------------------------------

  @Test
  @Order(20)
  public void testSetWithoutNextOrPrevious()
  {
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    assertThrows(IllegalStateException.class, () -> objListIterator.set(debate));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(21)
  public void testAddAtDifferentPositions()
  {
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);

    objListIterator.add(debate);
    assertEquals(debate, position.largerDebates.get(0));

    HDT_Debate debate2 = db.createNewBlankRecord(hdtDebate);

    objListIterator.next();
    objListIterator.next();
    objListIterator.add(debate2);
    assertEquals(debate2, position.largerDebates.get(3));
  }

//---------------------------------------------------------------------------

  @Test
  @Order(22)
  public void testSingleElementList()
  {
    HDT_Position newPosition = db.createNewBlankRecord(hdtPosition);
    HDT_Debate newDebate = db.createNewBlankRecord(hdtDebate);

    newPosition.largerDebates.add(newDebate);

    ListIterator<HDT_Debate> oListIterator = newPosition.largerDebates.listIterator();

    // Test hasNext and next
    assertTrue(oListIterator.hasNext(), "Iterator should have next element");
    assertEquals(newDebate, oListIterator.next(), "Next element should be newDebate");

    // Test hasPrevious and previous
    assertTrue(oListIterator.hasPrevious(), "Iterator should have previous element");
    assertEquals(newDebate, oListIterator.previous(), "Previous element should be newDebate");

    // Test previous without hasPrevious
    assertThrows(NoSuchElementException.class, oListIterator::previous, "Calling previous() without hasPrevious() should throw NoSuchElementException");

    // Test nextIndex and previousIndex
    assertEquals(0, oListIterator.nextIndex(), "Next index should be 0");
    assertEquals(-1, oListIterator.previousIndex(), "Previous index should be -1");

    // Test remove
    oListIterator.next();
    oListIterator.remove();
    assertFalse(newPosition.largerDebates.contains(newDebate), "List should not contain newDebate after removal");
    assertEquals(0, newPosition.largerDebates.size(), "List should be empty after removal");

    // Add element back for further testing
    newPosition.largerDebates.add(newDebate);
    oListIterator = newPosition.largerDebates.listIterator();

    // Test set
    HDT_Debate anotherDebate = db.createNewBlankRecord(hdtDebate);
    oListIterator.next();
    oListIterator.set(anotherDebate);
    assertEquals(anotherDebate, newPosition.largerDebates.get(0), "Element should be anotherDebate after set");

    // Test add
    HDT_Debate yetAnotherDebate = db.createNewBlankRecord(hdtDebate);
    oListIterator.add(yetAnotherDebate);
    assertTrue(newPosition.largerDebates.contains(yetAnotherDebate), "List should contain yetAnotherDebate after add");
    assertEquals(2, newPosition.largerDebates.size(), "List size should be 2 after add");

    // Final state check
    assertEquals(anotherDebate, newPosition.largerDebates.get(0), "First element should be anotherDebate");
    assertEquals(yetAnotherDebate, newPosition.largerDebates.get(1), "Second element should be yetAnotherDebate");
  }

//---------------------------------------------------------------------------

  @Test
  @Order(999)
  public void testComodificationOnLoadDB()
  {
    objIterator.next();
    objListIterator.next();

    db.closeAndOpen();

    assertThrows(NoSuchElementException.class, () -> position.largerDebates.add(list.get(0)));
    assertThrows(NoSuchElementException.class, () -> position.largerDebates.add(db.createNewBlankRecord(hdtDebate)));

    assertThrows(NoSuchElementException         .class, objIterator    ::next    );
    assertThrows(ConcurrentModificationException.class, objIterator    ::remove  );
    assertThrows(NoSuchElementException         .class, objListIterator::next    );
    assertThrows(ConcurrentModificationException.class, objListIterator::previous);
    assertThrows(IllegalStateException          .class, objListIterator::remove  );

    assertThrows(ConcurrentModificationException.class, () -> objListIterator.add(db.createNewBlankRecord(hdtDebate)));
    assertThrows(IllegalStateException          .class, () -> objListIterator.set(db.createNewBlankRecord(hdtDebate)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
