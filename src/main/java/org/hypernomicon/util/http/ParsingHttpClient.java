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

import java.io.InputStream;
import java.net.http.*;
import java.util.function.Consumer;

import org.hypernomicon.model.Exceptions.CancelledTaskException;

//---------------------------------------------------------------------------

/**
 * Parent of the HTTP clients that parse a response body into something callers can
 * query: {@link JsonHttpClient} and {@link XmlHttpClient}.
 * <p>
 * It holds what such a client records about its most recent request and response, sends
 * the asynchronous requests, and reports their outcome to the caller's handlers on the
 * JavaFX thread. What to make of a response is left to each client; see
 * {@link #handleResponse handleResponse} for why the two go about it differently.
 * </p>
 */
abstract class ParsingHttpClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HttpHeaders headers;
  private int statusCode;
  private String lastUrl = "";

//---------------------------------------------------------------------------

  /** Returns the HTTP status code from the most recent response. */
  public int getStatusCode()       { return statusCode; }

  /** Returns the HTTP headers from the most recent response. */
  public HttpHeaders getHeaders()  { return headers; }

  /** Returns the URL of the most recent request. */
  public String getLastUrl()       { return lastUrl; }

  /** Forgets whatever was parsed from the previous response. */
  abstract void clearParsedBody();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Handles the response to a request, on the thread that sent it: calls
   * {@link #recordResponse recordResponse}, reads the body or drains it, and reports the
   * outcome to the handlers through the dispatch methods.
   * <p>
   * The two clients differ on purpose in the order they do this in, which is why this is
   * not a shared method with a hook for the parsing:
   * </p>
   * <ul>
   * <li>{@link XmlHttpClient} checks the status first. On an error status it drains the
   * body unparsed, so that the connection can be reused, and reports the status.</li>
   * <li>{@link JsonHttpClient} parses first, because it also serves callers that send
   * their requests synchronously: the reference-manager API wrappers. They pass no
   * handlers, branch on {@link #getStatusCode()} themselves, and read the JSON that a
   * server sends along with an error status, which is where Mendeley says that a token
   * has expired. Only its asynchronous callers get an error status as an exception.</li>
   * </ul>
   *
   * @param response     the response, with its body still unread
   * @param httpClient   the client the request was sent through, or {@code null} if it
   *                     was sent synchronously
   * @param successHndlr run on the JavaFX thread if the response is one the caller can
   *                     use; may be {@code null}
   * @param failHndlr    given the failure on the JavaFX thread otherwise, and given
   *                     whatever {@code successHndlr} throws; {@code null} when the request
   *                     was sent synchronously
   * @return {@code false} if the response amounts to a failure for the caller that sent
   *         the request
   */
  abstract boolean handleResponse(HttpResponse<InputStream> response, AsyncHttpClient httpClient,
                                  Runnable successHndlr, Consumer<Exception> failHndlr);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sends the request through {@code httpClient} on a background thread and leaves it to
   * {@link #handleResponse handleResponse} to report the outcome to the handlers.
   */
  final void doAsyncRequest(HttpRequest request, AsyncHttpClient httpClient, Runnable successHndlr, Consumer<Exception> failHndlr)
  {
    startRequest(request);

    httpClient.doRequest(request, response -> handleResponse(response, httpClient, successHndlr, failHndlr), failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Called before a request is sent: forgets the previous response's parsed body and records the new URL. */
  final void startRequest(HttpRequest request)
  {
    clearParsedBody();

    lastUrl = request.uri().toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Records the status code and the headers of the response, which always change together. */
  final void recordResponse(HttpResponse<?> response)
  {
    statusCode = response.statusCode();
    headers = response.headers();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs the success handler, if there is one, on the JavaFX thread.
   * <p>
   * A success handler is seldom the end of the line: it goes on to build something out of
   * what was parsed, while its caller waits for one of the two handlers to report. So a
   * runtime exception it throws is logged and given to the failure handler. Left to the
   * JavaFX thread, the exception would reach nobody, and the caller would wait forever.
   * </p>
   */
  static void dispatchSuccess(Runnable successHndlr, Consumer<Exception> failHndlr)
  {
    if (successHndlr == null) return;

    runInFXThread(() ->
    {
      try
      {
        successHndlr.run();
      }
      catch (RuntimeException e)
      {
        if (failHndlr == null) throw e;

        logThrowable(e);
        failHndlr.accept(e);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Gives the failure handler, if there is one, the error status of the most recent
   * response as an {@link HttpResponseException}, on the JavaFX thread.
   */
  final void dispatchErrorStatus(Consumer<Exception> failHndlr)
  {
    if (failHndlr != null)
      runInFXThread(() -> failHndlr.accept(new HttpResponseException(statusCode, lastUrl)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Gives the failure handler, if there is one, what went wrong while the body was being
   * read or parsed, on the JavaFX thread.
   * <p>
   * Stopping a request interrupts the read of its body, which then fails with an I/O
   * error. The handler is given a {@link CancelledTaskException} in place of that error
   * (see {@link AsyncHttpClient#failureToReport failureToReport}), because callers tell
   * the two apart in order to say nothing about a request the user gave up on. Whether
   * the request was stopped is settled here, on the request's own thread, and not when
   * the handler runs: by then the same client may have started its next request, which
   * clears the record of the stop.
   * </p>
   */
  static void dispatchFailure(Consumer<Exception> failHndlr, AsyncHttpClient httpClient, Exception e)
  {
    if (failHndlr == null) return;

    Exception failure = (httpClient == null) ? e : httpClient.failureToReport(e);

    runInFXThread(() -> failHndlr.accept(failure));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
