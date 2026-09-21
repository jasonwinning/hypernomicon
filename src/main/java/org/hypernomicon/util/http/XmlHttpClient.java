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

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.io.InputStream;
import java.net.http.*;
import java.time.Duration;
import java.util.function.Consumer;

import org.apache.tika.mime.MediaType;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.parser.Parser;

//---------------------------------------------------------------------------

/**
 * HTTP client for making requests that return XML responses.
 * <p>
 * Responses are parsed with jsoup's XML parser, which yields a {@link Document}
 * that can be queried with CSS selectors. Namespace prefixes are preserved in tag
 * names, so a prefix-agnostic selector such as {@code *|numberOfRecords} matches
 * regardless of which prefix the server used.
 * </p>
 * <p>
 * Unlike {@link JsonHttpClient}, parsing is not gated on the Content-Type header.
 * Servers that return XML while labeling it something else are common enough that
 * gating on the header would silently produce an empty document.
 * </p>
 */
public class XmlHttpClient extends ParsingHttpClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Per-request timeout. The shared client only applies a connect timeout, which is not
   * enough for hosts that accept the connection and then stall; without this, a blocked
   * or black-holed port leaves the caller waiting far longer than a fallback path should.
   */
  private static final Duration REQUEST_TIMEOUT = Duration.ofSeconds(10);

  private Document document = null;

//---------------------------------------------------------------------------

  @Override void clearParsedBody() { document = null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Asynchronously fetches an XML document from the specified URL.
   *
   * @param url          the URL to fetch
   * @param httpClient   the async HTTP client to use for the request
   * @param successHndlr callback invoked on the FX thread with the parsed document
   * @param failHndlr    callback invoked on the FX thread if the request fails, or if
   *                     {@code successHndlr} throws
   */
  public static void getDocAsync(String url, AsyncHttpClient httpClient, Consumer<Document> successHndlr, Consumer<Exception> failHndlr)
  {
    try
    {
      HttpRequest request = AsyncHttpClient.requestBuilder(url).timeout(REQUEST_TIMEOUT).GET().build();

      XmlHttpClient xmlClient = new XmlHttpClient();

      xmlClient.doAsyncRequest(request, httpClient, () -> successHndlr.accept(xmlClient.document), failHndlr);
    }
    catch (IllegalArgumentException e)
    {
      if (failHndlr != null)
        failHndlr.accept(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override boolean handleResponse(HttpResponse<InputStream> response, AsyncHttpClient httpClient,
                                   Runnable successHndlr, Consumer<Exception> failHndlr)
  {
    recordResponse(response);

    if (HttpStatusCode.isError(getStatusCode()))
    {
      try (InputStream is = response.body()) { is.readAllBytes(); }  // Drain so the connection can be reused
      catch (IOException e) { noOp(); }

      dispatchErrorStatus(failHndlr);

      return false;
    }

    try (InputStream is = response.body())
    {
      document = Jsoup.parse(is, charsetName(), getLastUrl(), Parser.xmlParser());
    }
    catch (IOException e)
    {
      dispatchFailure(failHndlr, httpClient, e);

      return false;
    }

    dispatchSuccess(successHndlr, failHndlr);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the charset named by the Content-Type header, or null to let jsoup sniff it
   * from the XML declaration or byte order mark.
   */
  private String charsetName()
  {
    MediaType mediaType = MediaType.parse(getHeaders().firstValue("Content-Type").orElse(""));

    if (mediaType == null) return null;

    String charset = stripSafe(mediaType.getParameters().get("charset"));

    return charset.isBlank() ? null : charset;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
