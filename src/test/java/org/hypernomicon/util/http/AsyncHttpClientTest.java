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

package org.hypernomicon.util.http;

import static org.hypernomicon.Const.*;
import static org.junit.jupiter.api.Assertions.*;

import java.net.http.HttpRequest;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class AsyncHttpClientTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Every request built through requestBuilder identifies the application: the
   *  services it queries can distinguish its traffic from anonymous crawlers, and
   *  Crossref honors the mailto in the User-Agent for its "polite" pool. */
  @Test
  void requestBuilderSetsTheApplicationUserAgent()
  {
    HttpRequest request = AsyncHttpClient.requestBuilder("https://example.com/").GET().build();

    String userAgent = request.headers().firstValue("User-Agent").orElse("");

    assertEquals(HTTP_USER_AGENT, userAgent);
    assertTrue(userAgent.startsWith("Hypernomicon/"), userAgent);
    assertTrue(userAgent.contains("mailto:" + APP_CONTACT_EMAIL), userAgent);
  }

//---------------------------------------------------------------------------

  /** A caller that needs a different User-Agent (browser-imitating file
   *  downloads) replaces the default with setHeader rather than stacking a
   *  second value with header. */
  @Test
  void setHeaderReplacesTheDefaultUserAgent()
  {
    HttpRequest request = AsyncHttpClient.requestBuilder("https://example.com/").setHeader("User-Agent", "Other/1.0").GET().build();

    assertEquals(1, request.headers().allValues("User-Agent").size());
    assertEquals("Other/1.0", request.headers().firstValue("User-Agent").orElse(""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
