package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.util.VersionNumber;
import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

public class VersionNumberTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  public void testConstructorWithIntegerParts()
  {
    VersionNumber version = new VersionNumber(1, 2, 3);
    assertEquals(3, version.numParts());
    assertEquals(1, version.getPart(0));
    assertEquals(2, version.getPart(1));
    assertEquals(3, version.getPart(2));
  }

  @Test
  public void testConstructorWithString()
  {
    VersionNumber version = new VersionNumber("1.2.3");
    assertEquals(3, version.numParts());
    assertEquals(1, version.getPart(0));
    assertEquals(2, version.getPart(1));
    assertEquals(3, version.getPart(2));
  }

  @Test
  public void testConstructorWithInsufficientParts()
  {
    VersionNumber version = new VersionNumber(1);
    assertEquals(2, version.numParts());
    assertEquals(1, version.getPart(0));
    assertEquals(0, version.getPart(1));
  }

  @Test
  public void testToString()
  {
    VersionNumber version = new VersionNumber(1, 2, 3);
    assertEquals("1.2.3", version.toString());
  }

  @Test
  public void testHashCode()
  {
    VersionNumber version1 = new VersionNumber(1, 2, 0);
    VersionNumber version2 = new VersionNumber("1.2.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  public void testEquals()
  {
    VersionNumber version1 = new VersionNumber(1, 2, 3);
    VersionNumber version2 = new VersionNumber("1.2.3");
    VersionNumber version3 = new VersionNumber(1, 2);
    assertTrue(version1.equals(version2));
    assertFalse(version1.equals(version3));
  }

  @Test
  public void testCompareTo()
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
  public void testWithLeadingZeros()
  {
    VersionNumber version1 = new VersionNumber("001.002.003");
    VersionNumber version2 = new VersionNumber(1, 2, 3);
    assertEquals(version1, version2);
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  public void testWithMoreThanTwoParts()
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
  public void testLessThanTwoParts()
  {
    VersionNumber version1 = new VersionNumber(1);
    VersionNumber version2 = new VersionNumber(1, 0);
    assertEquals(version2, version1);
  }

  @Test
  public void testEmptyConstructor()
  {
    VersionNumber version = new VersionNumber();
    assertEquals(2, version.numParts()); // Default minimum parts
    assertEquals(0, version.getPart(0));
    assertEquals(0, version.getPart(1));
  }

  @Test
  public void testConstructorWithEmptyString()
  {
    VersionNumber version = new VersionNumber("");
    assertEquals(2, version.numParts());
    assertEquals(0, version.getPart(0));
    assertEquals(0, version.getPart(1));
  }

  @Test
  public void testConstructorWithLeadingAndTrailingDots()
  {
    VersionNumber version = new VersionNumber(".1.2.3.");
    assertEquals(4, version.numParts());
    assertEquals(0, version.getPart(0));
    assertEquals(1, version.getPart(1));
    assertEquals(2, version.getPart(2));
    assertEquals(3, version.getPart(3));
  }

  @Test
  public void testConstructorWithConsecutiveDots()
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
  public void testCompareToWithDifferentLengths()
  {
    VersionNumber version1 = new VersionNumber(1, 2);
    VersionNumber version2 = new VersionNumber(1, 2, 0);
    VersionNumber version3 = new VersionNumber(1, 2, 3);

    assertEquals(0, version1.compareTo(version2));
    assertTrue(version1.compareTo(version3) < 0);
    assertTrue(version3.compareTo(version1) > 0);
  }

  @Test
  public void testEqualsWithDifferentLengths()
  {
    VersionNumber version1 = new VersionNumber(1, 2);
    VersionNumber version2 = new VersionNumber(1, 2, 0);
    VersionNumber version3 = new VersionNumber(1, 2, 3);

    assertTrue(version1.equals(version2));
    assertFalse(version1.equals(version3));
  }

  @Test
  public void testWithNegativeParts()
  {
    VersionNumber version = new VersionNumber(-1, 2, -3);
    assertEquals(3, version.numParts());
    assertEquals(-1, version.getPart(0));
    assertEquals(2, version.getPart(1));
    assertEquals(-3, version.getPart(2));
  }

  @Test
  public void testToStringWithNegativeParts()
  {
    VersionNumber version = new VersionNumber(-1, 2, -3);
    assertEquals("-1.2.-3", version.toString());
  }

  @Test
  public void testCompareToWithNegativeParts()
  {
    VersionNumber version1 = new VersionNumber(-1, 2, 3);
    VersionNumber version2 = new VersionNumber(1, 2, 3);

    assertTrue(version1.compareTo(version2) < 0);
    assertTrue(version2.compareTo(version1) > 0);
  }

  @Test
  public void testEqualsWithNegativeParts()
  {
    VersionNumber version1 = new VersionNumber(-1, 2, 3);
    VersionNumber version2 = new VersionNumber(-1, 2, 3);
    VersionNumber version3 = new VersionNumber(1, 2, 3);

    assertTrue(version1.equals(version2));
    assertFalse(version1.equals(version3));
  }

  @Test
  public void testTrailingZerosInEquality()
  {
    VersionNumber version1 = new VersionNumber("2.3");
    VersionNumber version2 = new VersionNumber("2.30");

    assertFalse(version1.equals(version2));
    assertTrue(version1.compareTo(version2) < 0);
    assertTrue(version2.compareTo(version1) > 0);
  }

  @Test
  public void testTrailingZerosInComparison()
  {
    VersionNumber version1 = new VersionNumber("1.2.3");
    VersionNumber version2 = new VersionNumber("1.2.3.0");
    VersionNumber version3 = new VersionNumber("1.2.3.0.0");

    assertTrue(version1.equals(version2));
    assertTrue(version2.compareTo(version3) == 0);
  }

  @Test
  public void testHashCodeWithSameVersions()
  {
    VersionNumber version1 = new VersionNumber("1.2.3");
    VersionNumber version2 = new VersionNumber(1, 2, 3);
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  public void testHashCodeWithDifferentLengthsSameValues()
  {
    VersionNumber version1 = new VersionNumber("1.2.3");
    VersionNumber version2 = new VersionNumber("1.2.3.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  public void testHashCodeWithTrailingZeros()
  {
    VersionNumber version1 = new VersionNumber("1.2.3");
    VersionNumber version2 = new VersionNumber("1.2.3.0.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  public void testHashCodeWithNegativeParts()
  {
    VersionNumber version1 = new VersionNumber("-1.2.3");
    VersionNumber version2 = new VersionNumber("-1.2.3.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  public void testHashCodeWithDifferentTrailingZeros()
  {
    VersionNumber version1 = new VersionNumber("2.3");
    VersionNumber version2 = new VersionNumber("2.30");
    assertNotEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  public void testHashCodeWithZerosOnly()
  {
    VersionNumber version1 = new VersionNumber("0.0.0");
    VersionNumber version2 = new VersionNumber("0.0.0.0");
    assertEquals(version1.hashCode(), version2.hashCode());
  }

  @Test
  public void testNewVersionCheck()
  {
    App.checkForNewVersionInThisThread(version ->
    {
      assertTrue(new VersionNumber(1, 28).compareTo(version) < 0);
      assertTrue(new VersionNumber(5).compareTo(version) > 0);

    }, () -> fail());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
