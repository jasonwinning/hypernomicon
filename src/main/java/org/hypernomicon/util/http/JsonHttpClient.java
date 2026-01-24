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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.json.JsonObj.*;

import java.io.*;
import java.net.http.*;
import java.net.http.HttpResponse.BodyHandlers;
import java.nio.charset.StandardCharsets;
import java.util.function.Consumer;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;

import org.hypernomicon.model.Exceptions.CancelledTaskException;
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
public class JsonHttpClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HttpHeaders headers;
  private int statusCode;
  private String lastUrl = "";
  private JsonArray jsonArray = null;
  private JsonObj jsonObj = null;
  private Exception lastException = null;

  /** Returns the HTTP status code from the most recent response. */
  public int getStatusCode()       { return statusCode; }

  /** Returns the HTTP headers from the most recent response. */
  public HttpHeaders getHeaders()  { return headers; }

  /** Returns the URL of the most recent request. */
  public String getLastUrl()       { return lastUrl; }

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
   * @param failHndlr    callback invoked on the FX thread if the request fails
   */
  public static void getArrayAsync(String url, AsyncHttpClient httpClient, Consumer<JsonArray> successHndlr, Consumer<Exception> failHndlr)
  {
    try
    {
      HttpRequest request = AsyncHttpClient.requestBuilder(url).GET().build();

      new JsonHttpClient().doAsyncRequest(request, httpClient, jsonClient -> runInFXThread(() ->
      {
        if (jsonClient.jsonArray == null)
        {
          jsonClient.jsonArray = new JsonArray();

          if (jsonClient.jsonObj != null)
            jsonClient.jsonArray.add(jsonClient.jsonObj);
        }
        successHndlr.accept(jsonClient.jsonArray);
      }), failHndlr);
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
   * @param failHndlr    callback invoked on the FX thread if the request fails
   */
  public static void getObjAsync(String url, AsyncHttpClient httpClient, Consumer<JsonObj> successHndlr, Consumer<Exception> failHndlr)
  {
    try
    {
      HttpRequest request = AsyncHttpClient.requestBuilder(url).GET().build();

      new JsonHttpClient().doAsyncRequest(request, httpClient, jsonClient -> runInFXThread(() -> successHndlr.accept(jsonClient.jsonObj)), failHndlr);
    }
    catch (IllegalArgumentException e)
    {
      if (failHndlr != null)
        failHndlr.accept(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doAsyncRequest(HttpRequest request, AsyncHttpClient httpClient, Consumer<JsonHttpClient> successHndlr, Consumer<Exception> failHndlr)
  {
    jsonArray = null;
    jsonObj = null;
    lastUrl = request.uri().toString();

    httpClient.doRequest(request, response -> handleResponse(response, httpClient, successHndlr, failHndlr), failHndlr);
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
    jsonArray = null;
    jsonObj = null;
    boolean rc = false;

    lastUrl = request.uri().toString();

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

  private boolean handleResponse(HttpResponse<InputStream> response, AsyncHttpClient httpClient,
                                  Consumer<JsonHttpClient> successHndlr, Consumer<Exception> failHndlr)
  {
    statusCode = response.statusCode();

    headers = response.headers();
    String contentType = headers.firstValue("Content-Type").orElse("");

    try (InputStream is = response.body())
    {
      if (contentType.toLowerCase().contains("json"))
      {
        Object obj = jsonParser.parse(new InputStreamReader(is, StandardCharsets.UTF_8));

        if (obj instanceof JSONObject jObj)
        {
          jsonObj = new JsonObj(jObj);

          if (successHndlr != null)
            runInFXThread(() -> successHndlr.accept(this));

          return true;
        }

        if (obj instanceof JSONArray jArr)
        {
          jsonArray = new JsonArray(jArr);

          if (successHndlr != null)
            runInFXThread(() -> successHndlr.accept(this));

          return true;
        }
      }
    }
    catch (ParseException | IOException e)
    {
      lastException = e;

      if (failHndlr != null)
      {
        boolean cancelledByUser = (httpClient != null) && httpClient.wasCancelledByUser();
        runInFXThread(() -> failHndlr.accept(cancelledByUser ? new CancelledTaskException() : e));
      }
    }

    if (HttpStatusCode.isError(statusCode))
    {
      if (failHndlr != null)
        runInFXThread(() -> failHndlr.accept(new HttpResponseException(statusCode, lastUrl)));

      return false;
    }

    if (successHndlr != null)
      runInFXThread(() -> successHndlr.accept(this));

    return true;
  }

//----------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
