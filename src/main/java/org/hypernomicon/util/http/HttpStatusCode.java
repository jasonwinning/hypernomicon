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

import java.util.Map;

//---------------------------------------------------------------------------

/**
 * HTTP status code constants. Replaces org.apache.http.HttpStatus.
 */
public final class HttpStatusCode
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HttpStatusCode() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  public static final int

  // Informational 1xx

    SC_CONTINUE = 100,
    SC_SWITCHING_PROTOCOLS = 101,

  // Successful 2xx

    SC_OK = 200,
    SC_CREATED = 201,
    SC_ACCEPTED = 202,
    SC_NON_AUTHORITATIVE_INFORMATION = 203,
    SC_NO_CONTENT = 204,
    SC_RESET_CONTENT = 205,
    SC_PARTIAL_CONTENT = 206,

  // Redirection 3xx

    SC_MULTIPLE_CHOICES = 300,
    SC_MOVED_PERMANENTLY = 301,
    SC_FOUND = 302,
    SC_SEE_OTHER = 303,
    SC_NOT_MODIFIED = 304,
    SC_USE_PROXY = 305,
    SC_TEMPORARY_REDIRECT = 307,
    SC_PERMANENT_REDIRECT = 308,

  // Client Error 4xx

    SC_BAD_REQUEST = 400,
    SC_UNAUTHORIZED = 401,
    SC_PAYMENT_REQUIRED = 402,
    SC_FORBIDDEN = 403,
    SC_NOT_FOUND = 404,
    SC_METHOD_NOT_ALLOWED = 405,
    SC_NOT_ACCEPTABLE = 406,
    SC_PROXY_AUTHENTICATION_REQUIRED = 407,
    SC_REQUEST_TIMEOUT = 408,
    SC_CONFLICT = 409,
    SC_GONE = 410,
    SC_LENGTH_REQUIRED = 411,
    SC_PRECONDITION_FAILED = 412,
    SC_CONTENT_TOO_LARGE = 413,
    SC_URI_TOO_LONG = 414,
    SC_UNSUPPORTED_MEDIA_TYPE = 415,
    SC_RANGE_NOT_SATISFIABLE = 416,
    SC_EXPECTATION_FAILED = 417,
    SC_UNPROCESSABLE_CONTENT = 422,
    SC_TOO_MANY_REQUESTS = 429,

  // Server Error 5xx

    SC_INTERNAL_SERVER_ERROR = 500,
    SC_NOT_IMPLEMENTED = 501,
    SC_BAD_GATEWAY = 502,
    SC_SERVICE_UNAVAILABLE = 503,
    SC_GATEWAY_TIMEOUT = 504,
    SC_HTTP_VERSION_NOT_SUPPORTED = 505;

  // Reason phrases per RFC 9110
  private static final Map<Integer, String> reasonPhrases = Map.ofEntries
  (
    Map.entry(SC_CONTINUE, "Continue"),
    Map.entry(SC_SWITCHING_PROTOCOLS, "Switching Protocols"),

    Map.entry(SC_OK, "OK"),
    Map.entry(SC_CREATED, "Created"),
    Map.entry(SC_ACCEPTED, "Accepted"),
    Map.entry(SC_NON_AUTHORITATIVE_INFORMATION, "Non-Authoritative Information"),
    Map.entry(SC_NO_CONTENT, "No Content"),
    Map.entry(SC_RESET_CONTENT, "Reset Content"),
    Map.entry(SC_PARTIAL_CONTENT, "Partial Content"),

    Map.entry(SC_MULTIPLE_CHOICES, "Multiple Choices"),
    Map.entry(SC_MOVED_PERMANENTLY, "Moved Permanently"),
    Map.entry(SC_FOUND, "Found"),
    Map.entry(SC_SEE_OTHER, "See Other"),
    Map.entry(SC_NOT_MODIFIED, "Not Modified"),
    Map.entry(SC_USE_PROXY, "Use Proxy"),
    Map.entry(SC_TEMPORARY_REDIRECT, "Temporary Redirect"),
    Map.entry(SC_PERMANENT_REDIRECT, "Permanent Redirect"),

    Map.entry(SC_BAD_REQUEST, "Bad Request"),
    Map.entry(SC_UNAUTHORIZED, "Unauthorized"),
    Map.entry(SC_PAYMENT_REQUIRED, "Payment Required"),
    Map.entry(SC_FORBIDDEN, "Forbidden"),
    Map.entry(SC_NOT_FOUND, "Not Found"),
    Map.entry(SC_METHOD_NOT_ALLOWED, "Method Not Allowed"),
    Map.entry(SC_NOT_ACCEPTABLE, "Not Acceptable"),
    Map.entry(SC_PROXY_AUTHENTICATION_REQUIRED, "Proxy Authentication Required"),
    Map.entry(SC_REQUEST_TIMEOUT, "Request Timeout"),
    Map.entry(SC_CONFLICT, "Conflict"),
    Map.entry(SC_GONE, "Gone"),
    Map.entry(SC_LENGTH_REQUIRED, "Length Required"),
    Map.entry(SC_PRECONDITION_FAILED, "Precondition Failed"),
    Map.entry(SC_CONTENT_TOO_LARGE, "Content Too Large"),
    Map.entry(SC_URI_TOO_LONG, "URI Too Long"),
    Map.entry(SC_UNSUPPORTED_MEDIA_TYPE, "Unsupported Media Type"),
    Map.entry(SC_RANGE_NOT_SATISFIABLE, "Range Not Satisfiable"),
    Map.entry(SC_EXPECTATION_FAILED, "Expectation Failed"),
    Map.entry(SC_UNPROCESSABLE_CONTENT, "Unprocessable Content"),
    Map.entry(SC_TOO_MANY_REQUESTS, "Too Many Requests"),

    Map.entry(SC_INTERNAL_SERVER_ERROR, "Internal Server Error"),
    Map.entry(SC_NOT_IMPLEMENTED, "Not Implemented"),
    Map.entry(SC_BAD_GATEWAY, "Bad Gateway"),
    Map.entry(SC_SERVICE_UNAVAILABLE, "Service Unavailable"),
    Map.entry(SC_GATEWAY_TIMEOUT, "Gateway Timeout"),
    Map.entry(SC_HTTP_VERSION_NOT_SUPPORTED, "HTTP Version Not Supported")
  );

//---------------------------------------------------------------------------

  /**
   * Returns the reason phrase for the given status code, or null if unknown.
   */
  public static String getReasonPhrase(int statusCode)
  {
    return reasonPhrases.get(statusCode);
  }

//---------------------------------------------------------------------------

  public static boolean isInformational(int statusCode) { return (statusCode >= 100) && (statusCode < 200); }
  public static boolean isSuccess      (int statusCode) { return (statusCode >= 200) && (statusCode < 300); }
  public static boolean isRedirect     (int statusCode) { return (statusCode >= 300) && (statusCode < 400); }
  public static boolean isClientError  (int statusCode) { return (statusCode >= 400) && (statusCode < 500); }
  public static boolean isServerError  (int statusCode) { return (statusCode >= 500) && (statusCode < 600); }
  public static boolean isError        (int statusCode) { return statusCode >= 400; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
