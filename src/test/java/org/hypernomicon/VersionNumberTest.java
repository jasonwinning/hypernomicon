package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.util.VersionNumber;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class VersionNumberTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testConstructorWithIntegerParts()
  {
    VersionNumber version = new VersionNumber(1, 2, 3);
    assertEquals(3, version.numParts());
    assertEquals(1, version.getPart(0));
    assertEquals(2, version.getPart(1));
    assertEquals(3, version.getPart(2));
  }

  @Test
  void testConstructorWithString()
  {
    VersionNumber version = new VersionNumber("1.2.3");
    assertEquals(3, version.numParts());
    assertEquals(1, version.getPart(0));
    assertEquals(2, version.getPart(1));
    assertEquals(3, version.getPart(2));
  }

  @Test
  void testConstructorWithInsufficientParts()
  {
    VersionNumber version = new VersionNumber(1);
    assertEquals(2, version.numParts());
    assertEquals(1, version.getPart(0));
    assertEquals(0, version.getPart(1));
  }

  @Test
  void testToString()
  {
    VersionNumber version = new VersionNumber(1, 2, 3);
    assertEquals("1.2.3", version.toString());
  }

  @Test
  void testHashCode()
  {
    VersionNumber version1 = new VersionNumber(1, 2, 0);
    VersionNumber version2 = new VersionNumber("1.2.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  void testEquals()
  {
    VersionNumber version1 = new VersionNumber(1, 2, 3);
    VersionNumber version2 = new VersionNumber("1.2.3");
    VersionNumber version3 = new VersionNumber(1, 2);
    assertEquals(version1, version2);
    assertNotEquals(version1, version3);
  }

  @Test
  void testCompareTo()
  {
    VersionNumber version1 = new VersionNumber(1, 2, 3);
    VersionNumber version2 = new VersionNumber("1.2.4");
    VersionNumber version3 = new VersionNumber(1, 2, 3);
    VersionNumber version4 = new VersionNumber(1, 3);

    assertTrue(version1.compareTo(version2) < 0);
    assertEquals(0, version1.compareTo(version3));
    assertTrue(version1.compareTo(version4) < 0);
    assertTrue(version4.compareTo(version1) > 0);
  }

  @Test
  void testWithLeadingZeros()
  {
    VersionNumber version1 = new VersionNumber("001.002.003");
    VersionNumber version2 = new VersionNumber(1, 2, 3);
    assertEquals(version1, version2);
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  void testWithMoreThanTwoParts()
  {
    VersionNumber version = new VersionNumber(1, 2, 3, 4, 5);
    assertEquals(5, version.numParts());
    assertEquals(1, version.getPart(0));
    assertEquals(2, version.getPart(1));
    assertEquals(3, version.getPart(2));
    assertEquals(4, version.getPart(3));
    assertEquals(5, version.getPart(4));
  }

  @Test
  void testLessThanTwoParts()
  {
    VersionNumber version1 = new VersionNumber(1);
    VersionNumber version2 = new VersionNumber(1, 0);
    assertEquals(version2, version1);
  }

  @Test
  void testEmptyConstructor()
  {
    VersionNumber version = new VersionNumber();
    assertEquals(2, version.numParts()); // Default minimum parts
    assertEquals(0, version.getPart(0));
    assertEquals(0, version.getPart(1));
  }

  @Test
  void testConstructorWithEmptyString()
  {
    VersionNumber version = new VersionNumber("");
    assertEquals(2, version.numParts());
    assertEquals(0, version.getPart(0));
    assertEquals(0, version.getPart(1));
  }

  @Test
  void testConstructorWithLeadingAndTrailingDots()
  {
    VersionNumber version = new VersionNumber(".1.2.3.");
    assertEquals(4, version.numParts());
    assertEquals(0, version.getPart(0));
    assertEquals(1, version.getPart(1));
    assertEquals(2, version.getPart(2));
    assertEquals(3, version.getPart(3));
  }

  @Test
  void testConstructorWithConsecutiveDots()
  {
    VersionNumber version = new VersionNumber("1..2..3");
    assertEquals(5, version.numParts());
    assertEquals(1, version.getPart(0));
    assertEquals(0, version.getPart(1));
    assertEquals(2, version.getPart(2));
    assertEquals(0, version.getPart(3));
    assertEquals(3, version.getPart(4));
  }

  @Test
  void testCompareToWithDifferentLengths()
  {
    VersionNumber version1 = new VersionNumber(1, 2);
    VersionNumber version2 = new VersionNumber(1, 2, 0);
    VersionNumber version3 = new VersionNumber(1, 2, 3);

    assertEquals(0, version1.compareTo(version2));
    assertTrue(version1.compareTo(version3) < 0);
    assertTrue(version3.compareTo(version1) > 0);
  }

  @Test
  void testEqualsWithDifferentLengths()
  {
    VersionNumber version1 = new VersionNumber(1, 2);
    VersionNumber version2 = new VersionNumber(1, 2, 0);
    VersionNumber version3 = new VersionNumber(1, 2, 3);

    assertEquals(version1, version2);
    assertNotEquals(version1, version3);
  }

  @Test
  void testWithNegativeParts()
  {
    VersionNumber version = new VersionNumber(-1, 2, -3);
    assertEquals(3, version.numParts());
    assertEquals(-1, version.getPart(0));
    assertEquals(2, version.getPart(1));
    assertEquals(-3, version.getPart(2));
  }

  @Test
  void testToStringWithNegativeParts()
  {
    VersionNumber version = new VersionNumber(-1, 2, -3);
    assertEquals("-1.2.-3", version.toString());
  }

  @Test
  void testCompareToWithNegativeParts()
  {
    VersionNumber version1 = new VersionNumber(-1, 2, 3);
    VersionNumber version2 = new VersionNumber(1, 2, 3);

    assertTrue(version1.compareTo(version2) < 0);
    assertTrue(version2.compareTo(version1) > 0);
  }

  @Test
  void testEqualsWithNegativeParts()
  {
    VersionNumber version1 = new VersionNumber(-1, 2, 3);
    VersionNumber version2 = new VersionNumber(-1, 2, 3);
    VersionNumber version3 = new VersionNumber(1, 2, 3);

    assertEquals(version1, version2);
    assertNotEquals(version1, version3);
  }

  @Test
  void testTrailingZerosInEquality()
  {
    VersionNumber version1 = new VersionNumber("2.3");
    VersionNumber version2 = new VersionNumber("2.30");

    assertNotEquals(version1, version2);
    assertTrue(version1.compareTo(version2) < 0);
    assertTrue(version2.compareTo(version1) > 0);
  }

  @Test
  void testTrailingZerosInComparison()
  {
    VersionNumber version1 = new VersionNumber("1.2.3");
    VersionNumber version2 = new VersionNumber("1.2.3.0");
    VersionNumber version3 = new VersionNumber("1.2.3.0.0");

    assertEquals(version1, version2);
    assertEquals(0, version2.compareTo(version3));
  }

  @Test
  void testHashCodeWithSameVersions()
  {
    VersionNumber version1 = new VersionNumber("1.2.3");
    VersionNumber version2 = new VersionNumber(1, 2, 3);
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  void testHashCodeWithDifferentLengthsSameValues()
  {
    VersionNumber version1 = new VersionNumber("1.2.3");
    VersionNumber version2 = new VersionNumber("1.2.3.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  void testHashCodeWithTrailingZeros()
  {
    VersionNumber version1 = new VersionNumber("1.2.3");
    VersionNumber version2 = new VersionNumber("1.2.3.0.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  void testHashCodeWithNegativeParts()
  {
    VersionNumber version1 = new VersionNumber("-1.2.3");
    VersionNumber version2 = new VersionNumber("-1.2.3.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  void testHashCodeWithDifferentTrailingZeros()
  {
    VersionNumber version1 = new VersionNumber("2.3");
    VersionNumber version2 = new VersionNumber("2.30");
    assertNotEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  void testHashCodeWithZerosOnly()
  {
    VersionNumber version1 = new VersionNumber("0.0.0");
    VersionNumber version2 = new VersionNumber("0.0.0.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  void testNewVersionCheck()
  {
    App.checkForNewVersionInThisThread(version ->
    {
      assertTrue(new VersionNumber(1, 28).compareTo(version) < 0);
      assertTrue(new VersionNumber(5).compareTo(version) > 0);

    }, Assertions::fail);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
