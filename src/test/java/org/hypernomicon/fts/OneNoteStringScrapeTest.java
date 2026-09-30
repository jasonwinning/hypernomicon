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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Tests for {@link OneNoteStringScrape}, which collects the readable runs of a section
 * Tika's parser rejects. The files here are plain byte sequences, since the scrape reads
 * no structure: the tests pin which runs it keeps and that text stored in one encoding is
 * read once.
 */
class OneNoteStringScrapeTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final byte[] GAP = new byte[4];

  @TempDir Path tempDir;

//---------------------------------------------------------------------------

  private static byte[] ascii(String str) { return str.getBytes(StandardCharsets.US_ASCII); }
  private static byte[] utf16(String str) { return str.getBytes(StandardCharsets.UTF_16LE); }

  private List<String> runsOf(byte[]... parts) throws IOException
  {
    ByteArrayOutputStream bytes = new ByteArrayOutputStream();

    for (byte[] part : parts)
      bytes.writeBytes(part);

    Path file = tempDir.resolve("section.one");
    Files.write(file, bytes.toByteArray());

    return OneNoteStringScrape.scrape(FilePath.of(file)).lines().toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void asciiAndUtf16RunsAreKeptInFileOrder() throws Exception
  {
    assertEquals(List.of("alpha", "bravo charlie", "delta"),
                 runsOf(GAP, ascii("alpha"), GAP, utf16("bravo charlie"), GAP, ascii("delta"), GAP));
  }

//---------------------------------------------------------------------------

  @Test void utf16TextAtAnOddByteOffsetIsFound() throws Exception
  {
    assertEquals(List.of("odd aligned text"), runsOf(new byte[1], utf16("odd aligned text"), GAP));
  }

//---------------------------------------------------------------------------

  @Test void textInOneEncodingIsNotAlsoReadAsTheOther() throws Exception
  {
    assertEquals(List.of("echo foxtrot golf"), runsOf(ascii("echo foxtrot golf"), GAP));
    assertEquals(List.of("hotel india juliet"), runsOf(utf16("hotel india juliet"), GAP));
  }

//---------------------------------------------------------------------------

  @Test void runsShorterThanTheMinimumAreDropped() throws Exception
  {
    assertEquals(List.of("kilo", "abc"), runsOf(ascii("kil"), GAP, ascii("kilo"), GAP, utf16("ab"), GAP, utf16("abc"), GAP));
  }

//---------------------------------------------------------------------------

  @Test void runsWithoutALetterAreDropped() throws Exception
  {
    assertEquals(List.of("12:25 PM"), runsOf(ascii("28[=28["), GAP, utf16("†††††"), GAP, ascii("12:25 PM"), GAP));
  }

//---------------------------------------------------------------------------

  @Test void surroundingSpacesAreStripped() throws Exception
  {
    assertEquals(List.of("lima   mike"), runsOf(ascii("   lima   mike   "), GAP));
  }

//---------------------------------------------------------------------------

  @Test void aFileWithNoTextYieldsBlank() throws Exception
  {
    assertEquals(List.of(), runsOf(GAP, GAP, GAP));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
