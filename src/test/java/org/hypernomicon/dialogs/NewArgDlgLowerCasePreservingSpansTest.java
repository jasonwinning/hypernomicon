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

package org.hypernomicon.dialogs;

import static org.hypernomicon.dialogs.NewArgDlgCtrlr.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Unit tests for {@link NewArgDlgCtrlr#lowerCasePreservingSpans(String, List)}.
 *
 * <p>The method is a pure string-offset transformation: lowercase every character
 * of the input except the ones inside the given spans, which keep their original
 * case. These tests lock down the offset arithmetic independently of the
 * {@code KeywordLinkScanner} that produces the spans in production.
 *
 * <p>Spans are {@code int[]{offset, length}} pairs in the caller's text; they
 * must be ascending, non-overlapping, and within bounds. The tests focus on
 * that contract plus the obvious edge cases (empty text, empty spans, spans at
 * the boundaries, adjacent spans, the entire string as one span).
 */
class NewArgDlgLowerCasePreservingSpansTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<int[]> spans(int... offsetLengthPairs)
  {
    assertEquals(0, offsetLengthPairs.length % 2, "spans() requires pairs of (offset, length)");

    List<int[]> result = new java.util.ArrayList<>();
    for (int ndx = 0; ndx < offsetLengthPairs.length; ndx += 2)
      result.add(new int[] { offsetLengthPairs[ndx], offsetLengthPairs[ndx + 1] });

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void emptyStringAndEmptySpans()
  {
    assertEquals("", lowerCasePreservingSpans("", List.of()));
  }

//---------------------------------------------------------------------------

  @Test
  void emptySpansLowercasesEverything()
  {
    assertEquals("smith's argument for x",
      lowerCasePreservingSpans("Smith's Argument for X", List.of()));
  }

//---------------------------------------------------------------------------

  @Test
  void alreadyLowercaseNoSpansIsNoOp()
  {
    assertEquals("dualism about consciousness",
      lowerCasePreservingSpans("dualism about consciousness", List.of()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void singleSpanAtStart_possessive()
  {
    // "Smith's argument for X" with "Smith" (offset 0, length 5) preserved.
    assertEquals("Smith's argument for x",
      lowerCasePreservingSpans("Smith's Argument for X", spans(0, 5)));
  }

//---------------------------------------------------------------------------

  @Test
  void singleSpanInMiddle()
  {
    // "The Smith argument" with "Smith" (offset 4, length 5) preserved.
    assertEquals("the Smith argument",
      lowerCasePreservingSpans("The Smith Argument", spans(4, 5)));
  }

//---------------------------------------------------------------------------

  @Test
  void singleSpanAtEnd()
  {
    // "argument against Smith" with "Smith" (offset 17, length 5) preserved.
    assertEquals("argument against Smith",
      lowerCasePreservingSpans("Argument against Smith", spans(17, 5)));
  }

//---------------------------------------------------------------------------

  @Test
  void spanCoveringEntireString()
  {
    assertEquals("Smith",
      lowerCasePreservingSpans("Smith", spans(0, 5)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void adjacentSpans()
  {
    // "Frege-Russell View" with two spans: "Frege" (0, 5) and "Russell" (6, 7).
    // The hyphen between them is at offset 5, length 1, and should be lowercased
    // even though the two spans are otherwise adjacent.
    assertEquals("Frege-Russell view",
      lowerCasePreservingSpans("Frege-Russell View", spans(0, 5, 6, 7)));
  }

//---------------------------------------------------------------------------

  @Test
  void touchingSpans()
  {
    // Artificial case: two spans with no gap ("Kant" at 0-3, "ian" at 4-6
    // would overlap; instead use "Kant" and "Kantian" logic where the scanner
    // returns a single extended span, so adjacent non-overlapping spans never
    // touch in production — but test the edge case anyway.
    // Here: spans(0,2) and (2,3) cover indices 0-4 of "ABCDEfg" entirely.
    assertEquals("ABCDEfg",
      lowerCasePreservingSpans("ABCDEfg", spans(0, 2, 2, 3)));
  }

//---------------------------------------------------------------------------

  @Test
  void multipleSpansWithGaps()
  {
    // "Kantian idealism vs Humean empiricism"
    //  ^^^^^^^             ^^^^^^
    //  0-6                  20-25
    // Spans preserve "Kantian" and "Humean".
    assertEquals("Kantian idealism vs Humean empiricism",
      lowerCasePreservingSpans("Kantian Idealism vs Humean Empiricism", spans(0, 7, 20, 6)));
  }

//---------------------------------------------------------------------------

  @Test
  void multiWordSpan()
  {
    // "Derek Parfit's argument" with "Derek Parfit" (0, 12) preserved.
    assertEquals("Derek Parfit's argument",
      lowerCasePreservingSpans("Derek Parfit's Argument", spans(0, 12)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void zeroLengthSpanIsNoOp()
  {
    // A zero-length span preserves nothing at that offset; the rest lowercases.
    assertEquals("smith's argument",
      lowerCasePreservingSpans("Smith's Argument", spans(3, 0)));
  }

//---------------------------------------------------------------------------

  @Test
  void spanPreservesUnusualCasing()
  {
    // If the user typed "MCTAGGART" in all caps, we preserve that exactly.
    assertEquals("the MCTAGGART paradox of time",
      lowerCasePreservingSpans("The MCTAGGART Paradox of Time", spans(4, 9)));
  }

//---------------------------------------------------------------------------

  @Test
  void spanInsideLowercaseContext()
  {
    // If user wrote "the Hume of Treatise is different from..." with Hume
    // capitalized and rest lowercase, the span just preserves the already-capitalized
    // word; rest stays lowercase as it was.
    assertEquals("the Hume of treatise is different",
      lowerCasePreservingSpans("the Hume of treatise is different", spans(4, 4)));
  }

//---------------------------------------------------------------------------

  @Test
  void spanAtVeryEnd_zeroGapAfter()
  {
    // Span reaches the exact end of the string.
    assertEquals("argument for Kant",
      lowerCasePreservingSpans("Argument for Kant", spans(13, 4)));
  }

//---------------------------------------------------------------------------

  @Test
  void spanAtVeryStart_zeroGapBefore()
  {
    // Span starts at offset 0.
    assertEquals("Kant argues",
      lowerCasePreservingSpans("Kant argues", spans(0, 4)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void unicodeCharactersInPreservedSpan()
  {
    // A philosopher's name with a non-ASCII character: Gödel.
    // Span length is the Java char length of "Gödel", which is 5 (ö is a single BMP char).
    assertEquals("Gödel's incompleteness theorem",
      lowerCasePreservingSpans("Gödel's Incompleteness Theorem", spans(0, 5)));
  }

//---------------------------------------------------------------------------

  @Test
  void unicodeCharactersInLowercasedPortion()
  {
    // Accented characters outside a preserved span still get lowercased.
    // "Café Society" -> "café society" (default Locale behavior).
    assertEquals("café society",
      lowerCasePreservingSpans("Café Society", List.of()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
