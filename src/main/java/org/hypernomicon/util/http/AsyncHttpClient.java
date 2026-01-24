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

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.Exceptions.*;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.http.*;
import java.net.http.HttpResponse.BodyHandlers;
import java.time.Duration;
import java.util.function.Consumer;

//---------------------------------------------------------------------------

/**
 * An asynchronous HTTP client that executes requests in a background thread.
 * <p>
 * This class wraps {@link HttpClient} to provide non-blocking HTTP operations
 * with support for cancellation. Requests are executed on a daemon thread,
 * and responses or errors are delivered via callback handlers.
 * </p>
 */
public class AsyncHttpClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final class RequestThread extends HyperThread
  {
    private final HttpRequest request;
    private final Consumer<HttpResponse<InputStream>> responseHndlr;
    private final Consumer<Exception> failHndlr;

    private RequestThread(HttpRequest request, Consumer<HttpResponse<InputStream>> responseHndlr, Consumer<Exception> failHndlr)
    {
      super("HttpRequest");

      setDaemon(true);

      this.request = request;
      this.responseHndlr = responseHndlr;
      this.failHndlr = failHndlr;
    }

    @Override public void run()
    {
      cancelledByUser = false;

      try
      {
        HttpResponse<InputStream> response = httpClient.send(request, BodyHandlers.ofInputStream());
        responseHndlr.accept(response);
      }
      catch (IOException e)
      {
        runInFXThread(() -> failHndlr.accept(cancelledByUser ? new CancelledTaskException() : e));
      }
      catch (InterruptedException e)
      {
        currentThread().interrupt();
        runInFXThread(() -> failHndlr.accept(cancelledByUser ? new CancelledTaskException() : new IOException("Request interrupted", e)));
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Enumeration of HTTP request method types.
   */
  public enum HttpRequestType { get, post, put, patch, delete, head, options, connect, trace }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final HttpClient httpClient = createClient();

  private volatile boolean cancelledByUser = false;
  private RequestThread requestThread;
  private String lastUrl = "";

  boolean wasCancelledByUser()            { return cancelledByUser; }

  /**
   * Returns the URL of the most recent request.
   *
   * @return the URL string of the last request, or an empty string if no request has been made
   *         or {@link #clearLastUrl()} was called
   */
  public synchronized String lastUrl()    { return lastUrl; }

  /**
   * Clears the stored URL of the most recent request.
   */
  public synchronized void clearLastUrl() { lastUrl = ""; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Executes an HTTP request asynchronously in a background thread.
   * <p>
   * Any previously running request is stopped before the new request begins.
   * The response or error is delivered via the provided callback handlers.
   * </p>
   *
   * @param request       the HTTP request to execute
   * @param responseHndlr callback invoked with the response if the request succeeds
   * @param failHndlr     callback invoked with the exception if the request fails;
   *                      receives a {@link org.hypernomicon.model.Exceptions.CancelledTaskException}
   *                      if the request was cancelled via {@link #stop()}
   */
  synchronized void doRequest(HttpRequest request, Consumer<HttpResponse<InputStream>> responseHndlr, Consumer<Exception> failHndlr)
  {
    stop();

    this.lastUrl = request.uri().toString();

    (requestThread = new RequestThread(request, responseHndlr, failHndlr)).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Stops any currently running request.
   * <p>
   * If a request is in progress, it is interrupted and the failure handler
   * will receive a {@link org.hypernomicon.model.Exceptions.CancelledTaskException}.
   * This method blocks for up to one second waiting for the request thread to terminate.
   * </p>
   */
  public synchronized void stop()
  {
    if (HyperThread.isRunning(requestThread))
    {
      cancelledByUser = true;

      // Interrupt the thread to cancel blocking I/O
      requestThread.interrupt();

      try { requestThread.join(1000); } catch (InterruptedException e) { Thread.currentThread().interrupt(); }
    }

    requestThread = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HttpClient createClient()
  {
    return HttpClient.newBuilder()
      .connectTimeout(Duration.ofSeconds(30))
      .followRedirects(HttpClient.Redirect.NORMAL)
      .build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static HttpClient getHttpClient()
  {
    return httpClient;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates an {@link HttpRequest.Builder} with the given URI string.
   *
   * @param uriStr the URI string for the request
   * @return a new request builder configured with the specified URI
   * @throws IllegalArgumentException if the URI string is invalid
   */
  public static HttpRequest.Builder requestBuilder(String uriStr)
  {
    return HttpRequest.newBuilder().uri(URI.create(uriStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
