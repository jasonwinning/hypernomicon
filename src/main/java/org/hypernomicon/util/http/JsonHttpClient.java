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

import static org.hypernomicon.util.json.JsonObj.*;

import java.io.*;
import java.net.http.*;
import java.net.http.HttpResponse.BodyHandlers;
import java.nio.charset.StandardCharsets;
import java.util.function.Consumer;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;

import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

/**
 * HTTP client for making requests that return JSON responses.
 * <p>
 * Provides both synchronous and asynchronous methods for fetching JSON data,
 * with automatic parsing into {@link JsonObj} or {@link JsonArray} objects.
 * </p>
 */
public class JsonHttpClient extends ParsingHttpClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray jsonArray = null;
  private JsonObj jsonObj = null;
  private Exception lastException = null;

//---------------------------------------------------------------------------

  @Override void clearParsedBody() { jsonArray = null; jsonObj = null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Asynchronously fetches a JSON array from the specified URL.
   * <p>
   * If the response contains a JSON object instead of an array, it is wrapped
   * in a single-element array.
   * </p>
   *
   * @param url          the URL to fetch
   * @param httpClient   the async HTTP client to use for the request
   * @param successHndlr callback invoked on the FX thread with the parsed JSON array
   * @param failHndlr    callback invoked on the FX thread if the request fails, or if
   *                     {@code successHndlr} throws
   */
  public static void getArrayAsync(String url, AsyncHttpClient httpClient, Consumer<JsonArray> successHndlr, Consumer<Exception> failHndlr)
  {
    try
    {
      HttpRequest request = AsyncHttpClient.requestBuilder(url).GET().build();

      JsonHttpClient jsonClient = new JsonHttpClient();

      jsonClient.doAsyncRequest(request, httpClient, () -> successHndlr.accept(jsonClient.getArray()), failHndlr);
    }
    catch (IllegalArgumentException e)
    {
      if (failHndlr != null)
        failHndlr.accept(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Asynchronously fetches a JSON object from the specified URL.
   *
   * @param url          the URL to fetch
   * @param httpClient   the async HTTP client to use for the request
   * @param successHndlr callback invoked on the FX thread with the parsed JSON object
   * @param failHndlr    callback invoked on the FX thread if the request fails, or if
   *                     {@code successHndlr} throws
   */
  public static void getObjAsync(String url, AsyncHttpClient httpClient, Consumer<JsonObj> successHndlr, Consumer<Exception> failHndlr)
  {
    try
    {
      HttpRequest request = AsyncHttpClient.requestBuilder(url).GET().build();

      JsonHttpClient jsonClient = new JsonHttpClient();

      jsonClient.doAsyncRequest(request, httpClient, () -> successHndlr.accept(jsonClient.jsonObj), failHndlr);
    }
    catch (IllegalArgumentException e)
    {
      if (failHndlr != null)
        failHndlr.accept(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Synchronously fetches a JSON array using the provided request.
   * <p>
   * This method blocks until the response is received. If the response contains
   * a JSON object instead of an array, it is wrapped in a single-element array.
   * </p>
   *
   * @param request the HTTP request to execute
   * @return the parsed JSON array, or {@code null} if the request failed
   * @throws ParseException if the response body cannot be parsed as JSON
   * @throws IOException    if an I/O error occurs during the request
   */
  public JsonArray requestArrayInThisThread(HttpRequest request) throws ParseException, IOException
  {
    if (doRequestInThisThread(request) == false)
      return null;

    return getArray();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the parsed response as an array: a lone JSON object becomes the only element,
   * and the array is empty if nothing was parsed.
   */
  private JsonArray getArray()
  {
    if (jsonArray == null)
    {
      jsonArray = new JsonArray();

      if (jsonObj != null)
        jsonArray.add(jsonObj);
    }

    return jsonArray;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean doRequestInThisThread(HttpRequest request) throws ParseException, IOException
  {
    boolean rc = false;

    startRequest(request);

    try
    {
      @SuppressWarnings("resource")
      HttpResponse<InputStream> response = AsyncHttpClient.getHttpClient().send(request, BodyHandlers.ofInputStream());
      rc = handleResponse(response, null, null, null);
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
      throw new IOException("Request interrupted", e);
    }

    if (lastException instanceof ParseException pe) { lastException = null; throw pe; }
    if (lastException instanceof IOException   ioe) { lastException = null; throw ioe; }

    return rc;
  }

//----------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override boolean handleResponse(HttpResponse<InputStream> response, AsyncHttpClient httpClient,
                                   Runnable successHndlr, Consumer<Exception> failHndlr)
  {
    recordResponse(response);

    String contentType = getHeaders().firstValue("Content-Type").orElse("");

    boolean parsed = false;

    try (InputStream is = response.body())
    {
      if (contentType.toLowerCase().contains("json"))
      {
        Object obj = jsonParser.parse(new InputStreamReader(is, StandardCharsets.UTF_8));

        if (obj instanceof JSONObject jObj)
        {
          jsonObj = new JsonObj(jObj);
          parsed = true;
        }
        else if (obj instanceof JSONArray jArr)
        {
          jsonArray = new JsonArray(jArr);
          parsed = true;
        }
      }
    }
    catch (ParseException | IOException e)
    {
      lastException = e;

      if (failHndlr != null)
      {
        dispatchFailure(failHndlr, httpClient, e);
        return false;
      }
    }

    if (HttpStatusCode.isError(getStatusCode()))
    {
      // Asynchronous callers get an error status as an exception even when the server
      // described the error with a JSON body, which Google Books does: parsing that body
      // as data used to fire the success handler, making a quota-rejected query look
      // like a clean no-results miss.

      if (failHndlr != null)
      {
        dispatchErrorStatus(failHndlr);
        return false;
      }

      // Synchronous callers (the reference-manager API wrappers) branch on getStatusCode()
      // themselves and read the parsed error payload, so for them an error status is not a
      // failure of this method; the result stays whether a JSON body was parsed.

      return parsed;
    }

    dispatchSuccess(successHndlr, failHndlr);

    return true;
  }

//----------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
