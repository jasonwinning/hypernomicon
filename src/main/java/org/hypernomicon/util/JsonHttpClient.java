/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.util;

import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;
import java.util.function.Consumer;

import static java.nio.charset.StandardCharsets.*;

import org.apache.http.Header;
import org.apache.http.HttpEntity;
import org.apache.http.HttpHeaders;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.CloseableHttpClient;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.ParseException;

import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public class JsonHttpClient
{
  private List<Header> headers;
  private int statusCode;
  private String reasonPhrase = "";
  private JsonArray jsonArray = null;
  private JsonObj jsonObj = null;
  private Exception lastException = null;
  private String lastUrl = "";

  public int getStatusCode()       { return statusCode; }
  public List<Header> getHeaders() { return headers; }
  public String getReasonPhrase()  { return reasonPhrase; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void getArrayAsync(String url, AsyncHttpClient httpClient, Consumer<JsonArray> successHndlr, Consumer<Exception> failHndlr)
  {
    try
    {
      new JsonHttpClient().doAsyncRequest(new HttpGet(url), httpClient, jsonClient -> runInFXThread(() ->
      {
        if (jsonClient.jsonArray == null)
        {
          jsonClient.jsonArray = new JsonArray();
          jsonClient.jsonArray.add(jsonClient.jsonObj);
        }
        successHndlr.accept(jsonClient.jsonArray);
      }), failHndlr);
    }
    catch (Exception e)
    {
      if (failHndlr != null)
        failHndlr.accept(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void getObjAsync(String url, AsyncHttpClient httpClient, Consumer<JsonObj> successHndlr, Consumer<Exception> failHndlr)
  {
    try
    {
      new JsonHttpClient().doAsyncRequest(new HttpGet(url), httpClient, jsonClient -> runInFXThread(() -> successHndlr.accept(jsonClient.jsonObj)), failHndlr);
    }
    catch (Exception e)
    {
      if (failHndlr != null)
        failHndlr.accept(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doAsyncRequest(HttpUriRequest request, AsyncHttpClient httpClient, Consumer<JsonHttpClient> successHndlr, Consumer<Exception> failHndlr)
  {
    jsonArray = null;
    jsonObj = null;

    try
    {
      lastUrl = request.getURI().toURL().toString();
    }
    catch (Exception e)
    {
      lastUrl = "";
    }

    httpClient.doRequest(request, getResponseHndlr(successHndlr, failHndlr), failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public JsonArray requestArrayInThisThread(HttpUriRequest request) throws UnsupportedOperationException, ParseException, IOException
  {
    if (!doRequestInThisThread(request))
      return null;

    if (jsonArray == null)
    {
      jsonArray = new JsonArray();
      jsonArray.add(jsonObj);
    }

    return jsonArray;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean doRequestInThisThread(HttpUriRequest request) throws ParseException, UnsupportedOperationException, IOException
  {
    jsonArray = null;
    jsonObj = null;
    boolean rc = false;

    lastUrl = request.getURI().toURL().toString();
    ResponseHandler<Boolean> responseHndlr = getResponseHndlr(null, null);

    try (CloseableHttpClient httpclient = getHTTPClient())
    {
      rc = httpclient.execute(request, responseHndlr);
    }

    if (lastException instanceof ParseException)
    {
      ParseException e = (ParseException) lastException;
      lastException = null;
      throw e;
    }
    else if (lastException instanceof UnsupportedOperationException)
    {
      UnsupportedOperationException e = (UnsupportedOperationException) lastException;
      lastException = null;
      throw e;
    }

    return rc;
  }

//----------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ResponseHandler<Boolean> getResponseHndlr(Consumer<JsonHttpClient> successHndlr, Consumer<Exception> failHndlr) { return response ->
  {
    statusCode = response.getStatusLine().getStatusCode();
    reasonPhrase = response.getStatusLine().getReasonPhrase();

    if (statusCode >= 400)
    {
      if (failHndlr != null)
        runInFXThread(() -> failHndlr.accept(new HttpResponseException(statusCode, "Response from " + lastUrl + ": " + reasonPhrase)));

      return false;
    }

    HttpEntity entity = response.getEntity();

    headers = List.of(response.getAllHeaders());
    String contentType = "";

    for (Header header : headers)
    {
      switch (header.getName())
      {
        case HttpHeaders.CONTENT_TYPE : contentType = header.getValue(); break;
      }
    }

    if (contentType.contains("json"))
    {
      try
      {
        Object obj = jsonParser.parse(new InputStreamReader(entity.getContent(), UTF_8));

        if (obj instanceof JSONObject)
        {
          jsonObj = new JsonObj((JSONObject) obj);

          if (successHndlr != null)
            runInFXThread(() -> successHndlr.accept(this));

          return true;
        }
        else if (obj instanceof JSONArray)
        {
          jsonArray = new JsonArray((JSONArray) obj);

          if (successHndlr != null)
            runInFXThread(() -> successHndlr.accept(this));

          return true;
        }
      }
      catch (UnsupportedOperationException | ParseException e)
      {
        lastException = e;

        if (failHndlr != null)
          runInFXThread(() -> failHndlr.accept(e));
      }
    }

    if (successHndlr != null)
      runInFXThread(() -> successHndlr.accept(this));

    return true;
  }; }

//----------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
