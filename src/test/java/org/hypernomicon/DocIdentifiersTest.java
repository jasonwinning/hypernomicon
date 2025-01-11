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

import static org.hypernomicon.util.Util.*;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class DocIdentifiersTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testStandardDOI()
  {
    String input = "This is a DOI 10.1000/182";
    assertEquals("10.1000/182", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithOCRErrors()
  {
    String input = "DOI with OCR errors D0i10.1000/abc";
    assertEquals("10.1000/abc", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testInvalidDOI()
  {
    String input = "This string does not contain a DOI.";
    assertEquals("", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithNoise()
  {
    String input = "Some random text 10.1000/xyz more random text.";
    assertEquals("10.1000/xyz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIPrefixSuffix()
  {
    String input = "10.1000/abc";
    assertEquals("10.1000/abc", matchDOI(input));

    input = "10.1000/abc.";
    assertEquals("10.1000/abc", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testCaseInsensitiveDOI()
  {
    String input = "doi10.1000/xyz";
    assertEquals("10.1000/xyz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithSpecialCharacters()
  {
    String input = "Check this DOI: 10.1000/abc-def_ghi";
    assertEquals("10.1000/abc-def_ghi", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithTrailingSlash()
  {
    String input = "Here is a DOI 10.1000/182/";
    assertEquals("10.1000/182/", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithPDFSuffix()
  {
    String input = "Document DOI: 10.1000/xyz.pdf";
    assertEquals("10.1000/xyz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithPunctuationAndPDFSuffix()
  {
    String input = "Document DOI: 10.1000/xyz.pdf;;";
    assertEquals("10.1000/xyz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithMixedCase()
  {
    String input = "Look at this Doi10.1000/MixedCase";
    assertEquals("10.1000/MixedCase", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithUnescapedURL()
  {
    String input = "Check this URL: https://doi.org/10.1000/xyz";
    assertEquals("10.1000/xyz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithSoftHyphen()
  {
    String input = "DOI with soft hyphen 10.1000/x\u00ADyz";
    assertEquals("10.1000/x-yz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithControlCharacter()
  {
    String input = "DOI with control character 10.1000\u0002xyz";
    assertEquals("10.1000/xyz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithMultipleDashes()
  {
    String input = "DOI with multiple dashes 10.1000/x--yz";
    assertEquals("10.1000/x-yz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testPrepareForIDMatchThroughMatchDOI()
  {
    String input = "Testing prepareForIDMatch – Hyphen and STX 10.1000\u0002x–yz";
    String expected = "10.1000/x-yz"; assertEquals(expected, matchDOI(input)); }

//---------------------------------------------------------------------------

  @Test
  void testCorrectOCRErrors()
  {
    String input = "DOI with OCR errors lI oO 10.1000/\u00B0";
    assertEquals("10.1000/0", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithEncodedURL()
  {
    String input = "This is an encoded URL DOI 10.1000%2Fxyz";
    assertEquals("10.1000/xyz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithoutPrefix()
  {
    String input = "URL https://doi.org/10.1000/xyz should be matched";
    assertEquals("10.1000/xyz", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testDOIWithSpaces()
  {
    String input = "DOI with spaces 10.1000 / xyz";
    assertEquals("", matchDOI(input)); // Invalid DOI due to spaces
  }

//---------------------------------------------------------------------------

  @Test
  void testEmptyString()
  {
    String input = "";
    assertEquals("", matchDOI(input));
  }

//---------------------------------------------------------------------------

  @Test
  void testNullString()
  {
    String input = null;
    assertEquals("", matchDOI(input));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
