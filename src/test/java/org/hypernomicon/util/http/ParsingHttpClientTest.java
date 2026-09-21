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

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.http.HttpRequest;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.*;
import java.util.function.Consumer;

import com.sun.net.httpserver.*;

import org.json.simple.parser.ParseException;

import org.jsoup.nodes.Document;

import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.util.FxTestUtil;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import javafx.application.Platform;

//---------------------------------------------------------------------------

/**
 * Contract tests for {@link JsonHttpClient} and {@link XmlHttpClient}, run against an
 * HTTP server on the loopback interface, so that real requests are made but nothing
 * leaves the machine.
 * <p>
 * The tests of what the two clients have in common take a {@link ClientKind} and run
 * once per asynchronous entry point; the rest pin what is particular to one client.
 */
class ParsingHttpClientTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The asynchronous entry points, each with a body it parses and the start of one. */
  private enum ClientKind
  {
    jsonObj("application/json", "{\"name\":\"value\"}", "{\"name\":")
    {
      @Override void get(String url, AsyncHttpClient httpClient, Consumer<Object> successHndlr, Consumer<Exception> failHndlr)
      {
        JsonHttpClient.getObjAsync(url, httpClient, successHndlr::accept, failHndlr);
      }
    },

    jsonArray("application/json", "[{\"name\":\"value\"}]", "[{\"name\":")
    {
      @Override void get(String url, AsyncHttpClient httpClient, Consumer<Object> successHndlr, Consumer<Exception> failHndlr)
      {
        JsonHttpClient.getArrayAsync(url, httpClient, successHndlr::accept, failHndlr);
      }
    },

    xml("application/xml", "<root><name>value</name></root>", "<root><name>")
    {
      @Override void get(String url, AsyncHttpClient httpClient, Consumer<Object> successHndlr, Consumer<Exception> failHndlr)
      {
        XmlHttpClient.getDocAsync(url, httpClient, successHndlr::accept, failHndlr);
      }
    };

    private final String contentType, body, partialBody;

    ClientKind(String contentType, String body, String partialBody)
    {
      this.contentType = contentType;
      this.body = body;
      this.partialBody = partialBody;
    }

    abstract void get(String url, AsyncHttpClient httpClient, Consumer<Object> successHndlr, Consumer<Exception> failHndlr);
  }

//---------------------------------------------------------------------------

  /** What an asynchronous request reported, and whether it did so on the JavaFX thread. */
  private record Outcome(Object result, Exception failure, boolean succeeded, boolean onFxThread) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int TIMEOUT_SECONDS = 10;

  private final CountDownLatch releaseStalledResponses = new CountDownLatch(1);

  private ExecutorService serverExecutor;
  private HttpServer server;

//---------------------------------------------------------------------------

  @BeforeAll
  static void initFx()
  {
    FxTestUtil.initJfx();
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void startServer() throws IOException
  {
    serverExecutor = Executors.newCachedThreadPool(runnable -> new HyperThread("TestHttpServer", runnable).asDaemon());

    server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
    server.setExecutor(serverExecutor);
    server.start();
  }

//---------------------------------------------------------------------------

  @AfterEach
  void stopServer()
  {
    releaseStalledResponses.countDown();

    server.stop(0);
    serverExecutor.shutdownNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Serves every request with the handler; returns the URL to request. */
  private String serve(HttpHandler handler)
  {
    server.createContext("/", handler);

    return "http://" + server.getAddress().getAddress().getHostAddress() + ':' + server.getAddress().getPort() + "/test";
  }

//---------------------------------------------------------------------------

  /** Serves every request with the status, the Content-Type, and the body. */
  private String serve(int statusCode, String contentType, byte[] body)
  {
    return serve(exchange ->
    {
      exchange.getResponseHeaders().set("Content-Type", contentType);
      exchange.getResponseHeaders().set("X-Test-Header", "present");
      exchange.sendResponseHeaders(statusCode, body.length == 0 ? -1 : body.length);

      try (OutputStream os = exchange.getResponseBody())
      {
        os.write(body);
      }
    });
  }

//---------------------------------------------------------------------------

  private String serve(int statusCode, String contentType, String body)
  {
    return serve(statusCode, contentType, body.getBytes(StandardCharsets.UTF_8));
  }

//---------------------------------------------------------------------------

  /**
   * Writes the start of the kind's body and leaves the response open; {@code exchange.close()}
   * ends it. Not try-with-resources: a stalled response has to stay open after this returns,
   * and closing the stream of a truncated response (fewer bytes than the announced length)
   * would throw, where {@code exchange.close()} just drops the connection.
   */
  @SuppressWarnings("resource")
  private static void writePartialBody(HttpExchange exchange, ClientKind kind) throws IOException
  {
    OutputStream os = exchange.getResponseBody();
    os.write(kind.partialBody.getBytes(StandardCharsets.UTF_8));
    os.flush();
  }

//---------------------------------------------------------------------------

  /**
   * Serves every request with the headers and the start of a body, then leaves the
   * response open until the test is over, so that the client blocks reading the body.
   */
  private String serveStalledBody(ClientKind kind)
  {
    return serve(exchange ->
    {
      exchange.getResponseHeaders().set("Content-Type", kind.contentType);
      exchange.sendResponseHeaders(HttpStatusCode.SC_OK, 0);

      writePartialBody(exchange, kind);

      try
      {
        releaseStalledResponses.await();
      }
      catch (InterruptedException e)
      {
        Thread.currentThread().interrupt();
      }

      exchange.close();
    });
  }

//---------------------------------------------------------------------------

  /**
   * Serves every request with the headers and the start of a body, then drops the
   * connection well short of the announced length.
   */
  private String serveTruncatedBody(ClientKind kind)
  {
    return serve(exchange ->
    {
      exchange.getResponseHeaders().set("Content-Type", kind.contentType);
      exchange.sendResponseHeaders(HttpStatusCode.SC_OK, 10_000);

      writePartialBody(exchange, kind);

      exchange.close();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Starts a request of the kind and returns a future of what it reports. */
  private static CompletableFuture<Outcome> start(ClientKind kind, String url, AsyncHttpClient httpClient)
  {
    CompletableFuture<Outcome> future = new CompletableFuture<>();

    kind.get(url, httpClient,
      result  -> future.complete(new Outcome(result, null   , true , Platform.isFxApplicationThread())),
      failure -> future.complete(new Outcome(null  , failure, false, Platform.isFxApplicationThread())));

    return future;
  }

//---------------------------------------------------------------------------

  private static Outcome await(CompletableFuture<Outcome> future)
  {
    try
    {
      return future.get(TIMEOUT_SECONDS, TimeUnit.SECONDS);
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
      return fail("Interrupted while waiting for the request to report", e);
    }
    catch (ExecutionException | TimeoutException e)
    {
      return fail("The request reported neither success nor failure", e);
    }
  }

//---------------------------------------------------------------------------

  private static Outcome request(ClientKind kind, String url)
  {
    return await(start(kind, url, new AsyncHttpClient()));
  }

//---------------------------------------------------------------------------

  /**
   * Waits until a request thread is blocked inside a client's response handling, which
   * is where it reads the body. Stopping the request any earlier would exercise the
   * request thread's own failure reporting instead of the clients'. The thread is
   * recognized by a {@code handleResponse} frame, so this has to follow a rename.
   */
  private static void awaitBlockedInResponseHandling()
  {
    long deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(TIMEOUT_SECONDS);

    while (System.nanoTime() < deadline)
    {
      for (var entry : Thread.getAllStackTraces().entrySet())
      {
        Thread.State state = entry.getKey().getState();

        if ((state != Thread.State.WAITING) && (state != Thread.State.TIMED_WAITING))
          continue;

        for (StackTraceElement element : entry.getValue())
          if ("handleResponse".equals(element.getMethodName()) && element.getClassName().startsWith("org.hypernomicon.util.http."))
            return;
      }

      try
      {
        Thread.sleep(10);
      }
      catch (InterruptedException e)
      {
        Thread.currentThread().interrupt();
        fail("Interrupted while waiting for the request thread to read the body", e);
      }
    }

    fail("No request thread started reading the body");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Both handlers run on the JavaFX thread, so callers can touch the UI from them. */
  @ParameterizedTest
  @EnumSource(ClientKind.class)
  void aParsedResponseIsDeliveredOnTheFxThread(ClientKind kind)
  {
    Outcome outcome = request(kind, serve(HttpStatusCode.SC_OK, kind.contentType, kind.body));

    assertTrue(outcome.succeeded(), () -> "Failed with " + outcome.failure());
    assertNotNull(outcome.result());
    assertTrue(outcome.onFxThread());
  }

//---------------------------------------------------------------------------

  /** Callers branch on the status code the exception carries: Crossref and the
   *  Library of Congress both treat 404 as a clean miss. */
  @ParameterizedTest
  @EnumSource(ClientKind.class)
  void anErrorStatusIsReportedAsAnHttpResponseException(ClientKind kind)
  {
    String url = serve(HttpStatusCode.SC_NOT_FOUND, "text/plain", "Not here");

    Outcome outcome = request(kind, url);

    assertFalse(outcome.succeeded());
    assertTrue(outcome.onFxThread());

    HttpResponseException e = assertInstanceOf(HttpResponseException.class, outcome.failure());

    assertEquals(HttpStatusCode.SC_NOT_FOUND, e.getStatusCode());
    assertEquals(url, e.getUrl());
  }

//---------------------------------------------------------------------------

  /** An error status wins even when the body is one the client can parse. Google Books
   *  describes a rejected query with a JSON body; treating that body as data would
   *  make a quota-rejected query look like a clean no-results miss. */
  @ParameterizedTest
  @EnumSource(ClientKind.class)
  void anErrorStatusWithAParsableBodyIsStillAFailure(ClientKind kind)
  {
    Outcome outcome = request(kind, serve(HttpStatusCode.SC_FORBIDDEN, kind.contentType, kind.body));

    assertFalse(outcome.succeeded());

    HttpResponseException e = assertInstanceOf(HttpResponseException.class, outcome.failure());

    assertEquals(HttpStatusCode.SC_FORBIDDEN, e.getStatusCode());
  }

//---------------------------------------------------------------------------

  /** A request the user stopped reports the cancellation, not the I/O error that the
   *  interrupted read of the body ended in, so callers can tell the two apart and
   *  stay quiet about the former. */
  @ParameterizedTest
  @EnumSource(ClientKind.class)
  void stoppingARequestWhileItsBodyIsBeingReadReportsCancellation(ClientKind kind)
  {
    AsyncHttpClient httpClient = new AsyncHttpClient();

    CompletableFuture<Outcome> future = start(kind, serveStalledBody(kind), httpClient);

    awaitBlockedInResponseHandling();

    httpClient.stop();

    Outcome outcome = await(future);

    assertFalse(outcome.succeeded());
    assertInstanceOf(CancelledTaskException.class, outcome.failure());
    assertTrue(outcome.onFxThread());
  }

//---------------------------------------------------------------------------

  /** Without a stop, a body that fails to arrive is reported as the error it is. */
  @ParameterizedTest
  @EnumSource(ClientKind.class)
  void aBodyThatFailsToArriveIsReportedAsTheIOException(ClientKind kind)
  {
    Outcome outcome = request(kind, serveTruncatedBody(kind));

    assertFalse(outcome.succeeded());
    assertTrue(outcome.onFxThread());

    assertInstanceOf(IOException.class, outcome.failure());
    assertFalse(outcome.failure() instanceof HttpResponseException, () -> "Reported " + outcome.failure());
  }

//---------------------------------------------------------------------------

  /** A caller waits for one of its two handlers to report, and a success handler can
   *  throw, because it goes on to parse what arrived: the bibliographic sources build
   *  their records in it. The failure handler is told; the caller would otherwise wait
   *  forever, which for an auto-fill meant until the user pressed Stop. */
  @ParameterizedTest
  @EnumSource(ClientKind.class)
  void whatTheSuccessHandlerThrowsIsReportedToTheFailureHandler(ClientKind kind)
  {
    RuntimeException thrown = new IllegalStateException("Thrown by the success handler");
    CompletableFuture<Outcome> future = new CompletableFuture<>();

    kind.get(serve(HttpStatusCode.SC_OK, kind.contentType, kind.body), new AsyncHttpClient(),
      result  -> { throw thrown; },
      failure -> future.complete(new Outcome(null, failure, false, Platform.isFxApplicationThread())));

    Outcome outcome = await(future);

    assertSame(thrown, outcome.failure());
    assertTrue(outcome.onFxThread());
  }

//---------------------------------------------------------------------------

  /** A URL no request can be built from is reported before the entry point returns,
   *  on the calling thread. */
  @ParameterizedTest
  @EnumSource(ClientKind.class)
  void aUrlNoRequestCanBeBuiltFromIsReportedAtOnce(ClientKind kind)
  {
    CompletableFuture<Outcome> future = start(kind, "http://exa mple.com/", new AsyncHttpClient());

    assertTrue(future.isDone());

    Outcome outcome = await(future);

    assertFalse(outcome.succeeded());
    assertFalse(outcome.onFxThread());
    assertInstanceOf(IllegalArgumentException.class, outcome.failure());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void getObjAsyncDeliversTheParsedObject()
  {
    Outcome outcome = request(ClientKind.jsonObj, serve(HttpStatusCode.SC_OK, "application/json; charset=utf-8", "{\"name\":\"Gödel\"}"));

    JsonObj jsonObj = assertInstanceOf(JsonObj.class, outcome.result());

    assertEquals("Gödel", jsonObj.getStr("name"));
  }

//---------------------------------------------------------------------------

  @Test
  void getArrayAsyncDeliversTheParsedArray()
  {
    Outcome outcome = request(ClientKind.jsonArray, serve(HttpStatusCode.SC_OK, "application/json", "[{\"n\":\"one\"},{\"n\":\"two\"}]"));

    JsonArray jsonArray = assertInstanceOf(JsonArray.class, outcome.result());

    assertEquals(2, jsonArray.size());
    assertEquals("two", jsonArray.getObj(1).getStr("n"));
  }

//---------------------------------------------------------------------------

  @Test
  void getArrayAsyncWrapsALoneObjectInAnArray()
  {
    Outcome outcome = request(ClientKind.jsonArray, serve(HttpStatusCode.SC_OK, "application/json", "{\"n\":\"one\"}"));

    JsonArray jsonArray = assertInstanceOf(JsonArray.class, outcome.result());

    assertEquals(1, jsonArray.size());
    assertEquals("one", jsonArray.getObj(0).getStr("n"));
  }

//---------------------------------------------------------------------------

  /** The JSON client parses only what the server labels as JSON; anything else succeeds
   *  with nothing parsed. */
  @Test
  void aBodyNotLabeledAsJsonIsNotParsed()
  {
    String url = serve(HttpStatusCode.SC_OK, "text/html", "{\"n\":\"one\"}");

    Outcome objOutcome   = request(ClientKind.jsonObj  , url),
            arrayOutcome = request(ClientKind.jsonArray, url);

    assertTrue(objOutcome.succeeded());
    assertNull(objOutcome.result());

    assertTrue(arrayOutcome.succeeded());
    assertEquals(0, assertInstanceOf(JsonArray.class, arrayOutcome.result()).size());
  }

//---------------------------------------------------------------------------

  @Test
  void malformedJsonIsReportedAsAParseException()
  {
    Outcome outcome = request(ClientKind.jsonObj, serve(HttpStatusCode.SC_OK, "application/json", "{\"n\":"));

    assertFalse(outcome.succeeded());
    assertInstanceOf(ParseException.class, outcome.failure());
    assertTrue(outcome.onFxThread());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HttpRequest getRequest(String url)
  {
    return AsyncHttpClient.requestBuilder(url).GET().build();
  }

//---------------------------------------------------------------------------

  @Test
  void aSynchronousRequestReturnsTheArrayAndRecordsTheResponse() throws ParseException, IOException
  {
    String url = serve(HttpStatusCode.SC_OK, "application/json", "[{\"n\":\"one\"}]");

    JsonHttpClient jsonClient = new JsonHttpClient();

    assertEquals("", jsonClient.getLastUrl());

    JsonArray jsonArray = jsonClient.requestArrayInThisThread(getRequest(url));

    assertEquals(1, jsonArray.size());
    assertEquals(HttpStatusCode.SC_OK, jsonClient.getStatusCode());
    assertEquals(url, jsonClient.getLastUrl());
    assertEquals("present", jsonClient.getHeaders().firstValue("X-Test-Header").orElse(""));
  }

//---------------------------------------------------------------------------

  @Test
  void aSynchronousRequestWrapsALoneObjectInAnArray() throws ParseException, IOException
  {
    String url = serve(HttpStatusCode.SC_OK, "application/json", "{\"n\":\"one\"}");

    JsonArray jsonArray = new JsonHttpClient().requestArrayInThisThread(getRequest(url));

    assertEquals(1, jsonArray.size());
    assertEquals("one", jsonArray.getObj(0).getStr("n"));
  }

//---------------------------------------------------------------------------

  /** The reference-manager wrappers branch on the status code themselves and read the
   *  error payload: Mendeley looks for {@code oauth/TOKEN_EXPIRED} in it. */
  @Test
  void aSynchronousRequestReturnsTheJsonBodyOfAnErrorStatus() throws ParseException, IOException
  {
    String url = serve(HttpStatusCode.SC_UNAUTHORIZED, "application/json", "{\"errorId\":\"oauth/TOKEN_EXPIRED\"}");

    JsonHttpClient jsonClient = new JsonHttpClient();

    JsonArray jsonArray = jsonClient.requestArrayInThisThread(getRequest(url));

    assertEquals(HttpStatusCode.SC_UNAUTHORIZED, jsonClient.getStatusCode());
    assertEquals(1, jsonArray.size());
    assertEquals("oauth/TOKEN_EXPIRED", jsonArray.getObj(0).getStr("errorId"));
  }

//---------------------------------------------------------------------------

  @Test
  void aSynchronousRequestReturnsNullForAnErrorStatusWithoutAJsonBody() throws ParseException, IOException
  {
    JsonHttpClient jsonClient = new JsonHttpClient();

    assertNull(jsonClient.requestArrayInThisThread(getRequest(serve(HttpStatusCode.SC_SERVICE_UNAVAILABLE, "text/plain", "Try later"))));

    assertEquals(HttpStatusCode.SC_SERVICE_UNAVAILABLE, jsonClient.getStatusCode());
  }

//---------------------------------------------------------------------------

  @Test
  void aSynchronousRequestThrowsTheParseExceptionForMalformedJson()
  {
    HttpRequest request = getRequest(serve(HttpStatusCode.SC_OK, "application/json", "{\"n\":"));

    assertThrows(ParseException.class, () -> new JsonHttpClient().requestArrayInThisThread(request));
  }

//---------------------------------------------------------------------------

  /** The reference-manager wrappers reuse one client for every request of a sync, so
   *  nothing from one response may survive into the next. */
  @Test
  void aReusedClientForgetsThePreviousResponse() throws ParseException, IOException
  {
    JsonHttpClient jsonClient = new JsonHttpClient();

    assertEquals(1, jsonClient.requestArrayInThisThread(getRequest(serve(HttpStatusCode.SC_OK, "application/json", "[{\"n\":\"one\"}]"))).size());

    server.removeContext("/");

    String url = serve(HttpStatusCode.SC_NOT_MODIFIED, "text/plain", "") + "?second";

    JsonArray jsonArray = jsonClient.requestArrayInThisThread(getRequest(url));

    assertEquals(0, jsonArray.size());
    assertEquals(HttpStatusCode.SC_NOT_MODIFIED, jsonClient.getStatusCode());
    assertEquals(url, jsonClient.getLastUrl());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void getDocAsyncDeliversADocumentThatSelectorsCanQuery()
  {
    String xml = "<zs:response xmlns:zs=\"urn:x\"><zs:numberOfRecords>3</zs:numberOfRecords></zs:response>";

    Outcome outcome = request(ClientKind.xml, serve(HttpStatusCode.SC_OK, "application/xml", xml));

    Document doc = assertInstanceOf(Document.class, outcome.result());

    assertEquals("3", doc.select("*|numberOfRecords").text());
  }

//---------------------------------------------------------------------------

  /** Unlike the JSON client, the XML client parses whatever arrives, because servers
   *  that mislabel their XML are common. */
  @Test
  void xmlIsParsedWhateverItIsLabeledAs()
  {
    Outcome outcome = request(ClientKind.xml, serve(HttpStatusCode.SC_OK, "text/plain", "<root><name>value</name></root>"));

    Document doc = assertInstanceOf(Document.class, outcome.result());

    assertEquals("value", doc.select("name").text());
  }

//---------------------------------------------------------------------------

  @Test
  void xmlIsDecodedWithTheCharsetTheContentTypeNames()
  {
    byte[] body = "<root><name>Gödel</name></root>".getBytes(StandardCharsets.ISO_8859_1);

    Outcome outcome = request(ClientKind.xml, serve(HttpStatusCode.SC_OK, "application/xml; charset=ISO-8859-1", body));

    Document doc = assertInstanceOf(Document.class, outcome.result());

    assertEquals("Gödel", doc.select("name").text());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
