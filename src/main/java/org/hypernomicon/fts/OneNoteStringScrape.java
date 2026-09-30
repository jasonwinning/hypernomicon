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

package org.hypernomicon.fts;

import java.io.*;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Recovers the text of a OneNote section that Tika's parser rejects, by collecting the
 * runs of readable characters in the file.
 * <p>
 * Sections written by OneNote 2007 have the revision-store structure of later sections
 * but predate the format Microsoft documented, and the parser's tree walk rejects them at
 * the first object declaration whose reserved bits are set (Tika issue TIKA-3194, open
 * since 2020). Tika has a string scrape of its own for differently packaged legacy files,
 * but it keeps a run only when it holds at least two words, and a 2007 section stores
 * each paragraph as its own run, so a page of one-word entries (a list of labels, say)
 * would lose every one of them. Such a section stores plain-ASCII paragraphs as 8-bit strings
 * and the rest as UTF-16LE, so both are collected here. The words of the notes are
 * recovered; their order and structure are not, which is acceptable for search. A few
 * short runs that binary data happens to spell come along with them.
 */
public final class OneNoteStringScrape
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Accumulates the characters of one kind of run and keeps each finished run that is
   *  long enough and holds a letter. */
  private static final class RunBuilder
  {
    private final StringBuilder run = new StringBuilder();
    private final int minLength;
    private final List<String> runs;

    private RunBuilder(int minLength, List<String> runs)
    {
      this.minLength = minLength;
      this.runs = runs;
    }

    private void accept(char ch) { run.append(ch); }

    private void end()
    {
      if ((run.length() >= minLength) && run.chars().anyMatch(Character::isLetter))
        runs.add(run.toString().strip());

      run.setLength(0);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int MIN_ASCII_RUN_LENGTH = 4,   // long enough for a short word; shorter runs are mostly chance
                           MIN_UTF16_RUN_LENGTH = 3;   // a code unit is two bytes, so a chance run is far rarer

//---------------------------------------------------------------------------

  private OneNoteStringScrape() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

  private static boolean isPrintableAscii(int byteValue) { return (byteValue >= 0x20) && (byteValue <= 0x7E); }

  /** Printable characters of the Latin scripts and their punctuation, as far as a
   *  two-byte code unit can be judged on its own. */
  private static boolean isPrintableLatin(int codeUnit)
  {
    return ((codeUnit >= 0x20) && (codeUnit <= 0x7E)) || ((codeUnit >= 0xA0) && (codeUnit <= 0x24F)) || ((codeUnit >= 0x2010) && (codeUnit <= 0x203A));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the runs of readable characters in the file, one per line, in the order they
   * end: 8-bit runs of at least four printable ASCII characters, and UTF-16LE runs of at
   * least three printable Latin characters at either byte alignment, each kept only if it
   * holds a letter. Text stored in one encoding is not also read as the other: ASCII text
   * read as UTF-16 gives code units outside the Latin range, and UTF-16 text read as
   * bytes gives single characters between zeros.
   */
  public static String scrape(FilePath filePath) throws IOException
  {
    List<String> runs = new ArrayList<>();

    RunBuilder ascii     = new RunBuilder(MIN_ASCII_RUN_LENGTH, runs),
               utf16Even = new RunBuilder(MIN_UTF16_RUN_LENGTH, runs),
               utf16Odd  = new RunBuilder(MIN_UTF16_RUN_LENGTH, runs);

    try (InputStream in = new BufferedInputStream(Files.newInputStream(filePath.toPath())))
    {
      int prev = -1, cur;
      boolean pairStartsEven = true;   // whether the code unit starting at prev begins at an even offset

      while ((cur = in.read()) != -1)
      {
        if (isPrintableAscii(cur))
          ascii.accept((char) cur);
        else
          ascii.end();

        if (prev != -1)
        {
          // The little-endian code unit that starts at the previous byte; the two
          // alignments each see every other pair, so their runs are contiguous

          int codeUnit = prev | (cur << 8);
          RunBuilder utf16 = pairStartsEven ? utf16Even : utf16Odd;

          if (isPrintableLatin(codeUnit))
            utf16.accept((char) codeUnit);
          else
            utf16.end();

          pairStartsEven = (pairStartsEven == false);
        }

        prev = cur;
      }
    }

    ascii.end();
    utf16Even.end();
    utf16Odd.end();

    return String.join("\n", runs);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
