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

import static org.junit.jupiter.api.Assertions.*;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.queryparser.classic.ParseException;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Every syntax error in a free-text query must come out of {@link FullTextIndexer#parseQuery} as a
 * ParseException, including the ones Lucene's regex compiler reports as an IllegalArgumentException,
 * so that the search reports them as query errors rather than as unexpected failures.
 */
class FtsQueryParsingTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Analyzer analyzer;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    analyzer = FullTextIndexer.createAnalyzer();
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    analyzer.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void aMalformedRegularExpressionIsAParseError()
  {
    ParseException e = assertThrows(ParseException.class, () -> FullTextIndexer.parseQuery(analyzer, "/(/"));

    assertInstanceOf(IllegalArgumentException.class, e.getCause(), "The regex compiler's exception must be kept as the cause");
    assertTrue(e.getMessage().contains("/(/"), e.getMessage());
  }

//---------------------------------------------------------------------------

  @Test
  void aWellFormedRegularExpressionParses() throws ParseException
  {
    assertNotNull(FullTextIndexer.parseQuery(analyzer, "/kan[dt]/"));
  }

//---------------------------------------------------------------------------

  @Test
  void anOrdinarySyntaxErrorIsStillAParseError()
  {
    assertThrows(ParseException.class, () -> FullTextIndexer.parseQuery(analyzer, "kant AND"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
