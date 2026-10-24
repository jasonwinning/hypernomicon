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

package org.hypernomicon.bib.mendeley;

import static org.hypernomicon.util.Util.*;

import static org.junit.jupiter.api.Assertions.*;

import java.net.http.HttpHeaders;
import java.time.Instant;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * A Mendeley sync resumes from the instant the server stamped on the first read's response, because
 * the server's clock is the one that also stamps the documents' modification times. This computer's
 * clock, read just before the request, is only the fallback for a response without a usable stamp.
 */
class MendeleySyncTimeTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Instant FALLBACK = parseIso8601("2026-10-24T12:00:00Z");

  private static HttpHeaders headersOf(String name, String value)
  {
    return HttpHeaders.of(Map.of(name, List.of(value)), (headerName, headerValue) -> true);
  }

//---------------------------------------------------------------------------

  @Test
  void serverStampIsUsedWhenPresent()
  {
    Instant serverTime = MendeleyWrapper.serverTimeFromHeaders(headersOf("Date", "Sat, 24 Oct 2026 11:59:30 GMT"), FALLBACK);

    assertEquals(parseIso8601("2026-10-24T11:59:30Z"), serverTime, "The Date header should be read as the server's instant.");
  }

//---------------------------------------------------------------------------

  @Test
  void fallbackIsUsedWithoutHeaders()
  {
    assertSame(FALLBACK, MendeleyWrapper.serverTimeFromHeaders(null, FALLBACK), "A response with no headers should fall back to the local instant.");
  }

//---------------------------------------------------------------------------

  @Test
  void fallbackIsUsedWithoutDateHeader()
  {
    assertSame(FALLBACK, MendeleyWrapper.serverTimeFromHeaders(headersOf("Content-Type", "application/json"), FALLBACK),
               "A response without a Date header should fall back to the local instant.");
  }

//---------------------------------------------------------------------------

  @Test
  void fallbackIsUsedForUnreadableDate()
  {
    assertSame(FALLBACK, MendeleyWrapper.serverTimeFromHeaders(headersOf("Date", "yesterday"), FALLBACK),
               "A Date header that is not an HTTP date should fall back to the local instant.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
