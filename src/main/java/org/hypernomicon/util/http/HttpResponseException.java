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

import java.io.IOException;
import java.io.Serial;

//---------------------------------------------------------------------------

/**
 * Exception thrown when an HTTP response indicates an error status code.
 * Replaces org.apache.http.client.HttpResponseException.
 */
public class HttpResponseException extends IOException
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Serial private static final long serialVersionUID = 1L;

  private final int statusCode;
  private final String url, serverMessage;

//---------------------------------------------------------------------------

  public HttpResponseException(int statusCode)
  {
    this(statusCode, null, null);
  }

//---------------------------------------------------------------------------

  public HttpResponseException(int statusCode, String url)
  {
    this(statusCode, url, null);
  }

//---------------------------------------------------------------------------

  public HttpResponseException(int statusCode, String url, String serverMessage)
  {
    super(buildMessage(statusCode, url, serverMessage));
    this.statusCode = statusCode;
    this.url = url;
    this.serverMessage = serverMessage;
  }

//---------------------------------------------------------------------------

  private static String buildMessage(int statusCode, String url, String serverMessage)
  {
    StringBuilder sb = new StringBuilder("HTTP ").append(statusCode);

    String reasonPhrase = HttpStatusCode.getReasonPhrase(statusCode);
    if (reasonPhrase != null)
      sb.append(' ').append(reasonPhrase);

    if (url != null)
      sb.append(" for ").append(url);

    if (serverMessage != null)
      sb.append(": ").append(serverMessage);

    return sb.toString();
  }

//---------------------------------------------------------------------------

  public int getStatusCode()       { return statusCode; }
  public String getUrl()           { return url; }
  public String getServerMessage() { return serverMessage; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
