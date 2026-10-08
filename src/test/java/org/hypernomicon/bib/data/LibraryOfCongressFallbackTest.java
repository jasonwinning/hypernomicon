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

package org.hypernomicon.bib.data;

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.LocLinkedDataSearchTest.*;

import java.io.IOException;
import java.io.OutputStream;
import java.net.*;
import java.net.http.HttpTimeoutException;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.*;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.bib.authors.BibAuthorsStandalone;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.authors.Author.Roles;
import org.hypernomicon.model.authors.AuthorStandalone;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.util.FxTestUtil;
import org.hypernomicon.util.http.AsyncHttpClient;
import org.hypernomicon.util.http.HttpResponseException;

//---------------------------------------------------------------------------

/**
 * The fallback of a Library of Congress search from the SRU server to the Linked Data Service,
 * run against an HTTP server on the loopback interface that plays both, so that real requests are
 * made but nothing leaves the machine.
 */
class LibraryOfCongressFallbackTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Answers from the SRU server that say it cannot be used */
  private enum UnusableAnswer
  {
    errorStatus     (500, "text/plain", "Internal error"),

    // The server answers a search that finds nothing with an envelope holding no records, so a
    // 404 says that the endpoint is not there

    notFound        (404, "text/html" , "<html><body>Not Found</body></html>"),

    // Something other than the server answered, such as an intercepting proxy

    notAnSruResponse(200, "text/html" , "<html><body>Proxy error</body></html>");

    private final int statusCode;
    private final String contentType, body;

    UnusableAnswer(int statusCode, String contentType, String body)
    {
      this.statusCode = statusCode;
      this.contentType = contentType;
      this.body = body;
    }
  }

//---------------------------------------------------------------------------

  /** What a search reported */
  private record Outcome(LibraryOfCongressBibData bd, Exception failure) { }

//---------------------------------------------------------------------------

  /** A real record, trimmed, as the Linked Data Service serves it: every element has a namespace prefix */
  private static final String MONTE_CRISTO_MARCXML = """
    <marcxml:record xmlns:marcxml="http://www.loc.gov/MARC21/slim">
      <marcxml:leader>01337cam a2200337 a 4500</marcxml:leader>
      <marcxml:controlfield tag="001">13234345</marcxml:controlfield>
      <marcxml:controlfield tag="008">030611s2003    enk           000 1 eng  </marcxml:controlfield>
      <marcxml:datafield tag="010" ind1=" " ind2=" "><marcxml:subfield code="a">  2003270054</marcxml:subfield></marcxml:datafield>
      <marcxml:datafield tag="020" ind1=" " ind2=" "><marcxml:subfield code="a">0140449264</marcxml:subfield></marcxml:datafield>
      <marcxml:datafield tag="100" ind1="1" ind2=" "><marcxml:subfield code="a">Dumas, Alexandre,</marcxml:subfield><marcxml:subfield code="d">1802-1870.</marcxml:subfield></marcxml:datafield>
      <marcxml:datafield tag="245" ind1="1" ind2="4"><marcxml:subfield code="a">The Count of Monte Cristo /</marcxml:subfield><marcxml:subfield code="c">Alexandre Dumas (père) ; translated and with an introduction and notes by Robin Buss.</marcxml:subfield></marcxml:datafield>
      <marcxml:datafield tag="260" ind1=" " ind2=" "><marcxml:subfield code="a">London, England ;</marcxml:subfield><marcxml:subfield code="a">New York, N.Y. :</marcxml:subfield><marcxml:subfield code="b">Penguin Books,</marcxml:subfield><marcxml:subfield code="c">c2003.</marcxml:subfield></marcxml:datafield>
      <marcxml:datafield tag="700" ind1="1" ind2=" "><marcxml:subfield code="a">Buss, Robin.</marcxml:subfield></marcxml:datafield>
    </marcxml:record>
    """;

  /** The SRU server's answer to a search that finds nothing */
  private static final String NO_RECORDS = """
    <?xml version="1.0"?><zs:searchRetrieveResponse xmlns:zs="http://www.loc.gov/zing/srw/">
      <zs:version>1.1</zs:version><zs:numberOfRecords>0</zs:numberOfRecords>
    </zs:searchRetrieveResponse>
    """;

  private static final String ISBN_SEARCH = "/resources/instances/suggest2/?q=9780140449266&searchtype=keyword&count=1",
                              RECORD      = "/resources/instances/13234345.marcxml.xml";

  private static final int TIMEOUT_SECONDS = 30;

  /** What the server was asked for, in order: "SRU" for the SRU server, the path and decoded query for the Linked Data Service */
  private final List<String> requests = new CopyOnWriteArrayList<>();

  private volatile int sruStatusCode;
  private volatile String sruContentType, sruBody;

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
    Map<String, String> linkedData = Map.of
    (
      "/resources/instances/suggest2/"                , MONTE_CRISTO_ISBN_HITS,
      "/resources/works/suggest2/"                    , MONTE_CRISTO_HITS,
      "/resources/instances/13234345.marcxml.xml"     , MONTE_CRISTO_MARCXML,
      "/resources/instances/in00024333360.marcxml.xml", "<empty/>"
    );

    serverExecutor = Executors.newCachedThreadPool(runnable -> new HyperThread("TestHttpServer", runnable).asDaemon());

    server = HttpServer.create(new InetSocketAddress(InetAddress.getLoopbackAddress(), 0), 0);
    server.setExecutor(serverExecutor);

    server.createContext("/sru", exchange ->
    {
      requests.add("SRU");
      respond(exchange, sruStatusCode, sruContentType, sruBody);
    });

    server.createContext("/resources/", exchange ->
    {
      String path = exchange.getRequestURI().getPath(),
             query = exchange.getRequestURI().getRawQuery(),
             body = linkedData.get(path);

      requests.add(query == null ? path : (path + '?' + URLDecoder.decode(query, StandardCharsets.UTF_8)));

      if (body == null)
        respond(exchange, 404, "text/plain", "Not here");
      else
        respond(exchange, 200, path.endsWith(".xml") ? "application/xml; charset=UTF-8" : "application/x-suggestions+json; charset=utf-8", body);
    });

    server.start();

    LibraryOfCongressBibData.setSruBaseForTesting(serverUrl() + "/sru");
    LocLinkedDataSearch.setBaseUrlForTesting(serverUrl() + "/resources/");
  }

//---------------------------------------------------------------------------

  @AfterEach
  void stopServer()
  {
    LibraryOfCongressBibData.setSruBaseForTesting(null);
    LocLinkedDataSearch.setBaseUrlForTesting(null);

    server.stop(0);
    serverExecutor.shutdownNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String serverUrl()                                                { return "http://" + server.getAddress().getAddress().getHostAddress() + ':' + server.getAddress().getPort(); }

  private void sruAnswers(int statusCode, String contentType, String body) { sruStatusCode = statusCode; sruContentType = contentType; sruBody = body; }

//---------------------------------------------------------------------------

  private static void respond(HttpExchange exchange, int statusCode, String contentType, String body) throws IOException
  {
    byte[] bytes = body.getBytes(StandardCharsets.UTF_8);

    exchange.getResponseHeaders().set("Content-Type", contentType);
    exchange.sendResponseHeaders(statusCode, bytes.length);

    try (OutputStream os = exchange.getResponseBody())
    {
      os.write(bytes);
    }
  }

//---------------------------------------------------------------------------

  /** Looks up the ISBN, and waits for what the search reports */
  private static Outcome searchIsbn(String isbn)
  {
    CompletableFuture<Outcome> future = new CompletableFuture<>();

    LibraryOfCongressBibData.doHttpRequest(new AsyncHttpClient(), List.of(isbn).iterator(), new HashSet<>(),
                                           bd -> future.complete(new Outcome(bd, null)), e -> future.complete(new Outcome(null, e)));
    return await(future);
  }

//---------------------------------------------------------------------------

  /** Searches for the title of a book by Dumas, and waits for what the search reports */
  private static Outcome searchDumasTitle(String title, String yearStr)
  {
    BibAuthorsStandalone authors = new BibAuthorsStandalone();
    authors.add(new AuthorStandalone(Roles.AUTHOR, new PersonName("Alexandre", "Dumas")));

    CompletableFuture<Outcome> future = new CompletableFuture<>();

    LibraryOfCongressBibData.doHttpRequest(new AsyncHttpClient(), title, yearStr, authors, null, new HashSet<>(),
                                           bd -> future.complete(new Outcome(bd, null)), e -> future.complete(new Outcome(null, e)));
    return await(future);
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
      return fail("Interrupted while waiting for the search to report", e);
    }
    catch (ExecutionException | TimeoutException e)
    {
      return fail("The search reported neither success nor failure", e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @ParameterizedTest
  @EnumSource(UnusableAnswer.class)
  void anUnusableAnswerSendsTheSearchToTheLinkedDataService(UnusableAnswer answer)
  {
    sruAnswers(answer.statusCode, answer.contentType, answer.body);

    Outcome outcome = searchIsbn("9780140449266");

    assertNull(outcome.failure(), () -> "Failed with " + outcome.failure());
    assertEquals("The Count of Monte Cristo", outcome.bd().getStr(bfTitle));

    assertEquals(List.of("SRU", ISBN_SEARCH, RECORD), requests);
  }

//---------------------------------------------------------------------------

  /** A refused connection, as from a port nothing listens on, is a server that cannot be reached */
  @Test
  void anUnreachableServerSendsTheSearchToTheLinkedDataService() throws IOException
  {
    int closedPort;

    try (ServerSocket socket = new ServerSocket(0, 1, InetAddress.getLoopbackAddress()))
    {
      closedPort = socket.getLocalPort();
    }

    LibraryOfCongressBibData.setSruBaseForTesting("http://" + InetAddress.getLoopbackAddress().getHostAddress() + ':' + closedPort + "/sru");

    Outcome outcome = searchIsbn("9780140449266");

    assertNull(outcome.failure(), () -> "Failed with " + outcome.failure());
    assertEquals("The Count of Monte Cristo", outcome.bd().getStr(bfTitle));

    assertEquals(List.of(ISBN_SEARCH, RECORD), requests);
  }

//---------------------------------------------------------------------------

  /** Once the SRU server could not be used, later searches leave it alone for a while: on a network that blocks it, every attempt would cost the request timeout first */
  @Test
  void theNextSearchGoesStraightToTheLinkedDataService()
  {
    sruAnswers(500, "text/plain", "Internal error");

    assertNotNull(searchIsbn("9780140449266").bd());

    requests.clear();
    sruAnswers(200, "text/xml", NO_RECORDS);  // Would answer now, but is not asked

    assertNotNull(searchIsbn("9780140449266").bd());
    assertEquals(List.of(ISBN_SEARCH, RECORD), requests);
  }

//---------------------------------------------------------------------------

  /** LoC has blocked this address for excessive traffic, which is not something to get around by asking its other service */
  @Test
  void theBlockPageIsNotGotAround()
  {
    sruAnswers(200, "text/html", "<html><body><h1>LC Distribution Alert</h1><p>Due to excessive traffic, your access to this system has been blocked.</p></body></html>");

    Outcome outcome = searchIsbn("9780140449266");

    assertInstanceOf(LibraryOfCongressBibData.AccessBlockedException.class, outcome.failure());
    assertEquals(List.of("SRU"), requests);
  }

//---------------------------------------------------------------------------

  /** A search that finds nothing is an answer, not a reason to ask the other service */
  @Test
  void aSearchThatFindsNothingIsAMiss()
  {
    sruAnswers(200, "text/xml", NO_RECORDS);

    Outcome outcome = searchIsbn("9780140449266");

    assertNull(outcome.failure(), () -> "Failed with " + outcome.failure());
    assertNull(outcome.bd());
    assertEquals(List.of("SRU"), requests);
  }

//---------------------------------------------------------------------------

  @Test
  void aTitleSearchFallsBackToo()
  {
    sruAnswers(500, "text/plain", "Internal error");

    Outcome outcome = searchDumasTitle("The Count of Monte Cristo", "2003");

    assertNull(outcome.failure(), () -> "Failed with " + outcome.failure());
    assertEquals("The Count of Monte Cristo", outcome.bd().getStr(bfTitle));

    assertEquals(3, requests.size(), requests::toString);
    assertEquals("SRU", requests.getFirst());
    assertTrue(requests.get(1).startsWith("/resources/works/suggest2/?q=\"count of monte cristo\" dumas&searchtype=keyword&"), requests.get(1));
    assertEquals(RECORD, requests.getLast(), "The year picks the 2003 edition among the hits");
  }

//---------------------------------------------------------------------------

  /** The newest records have no MARCXML at the Linked Data Service, only an empty document */
  @Test
  void anInstanceWithNoRecordIsAMiss()
  {
    sruAnswers(500, "text/plain", "Internal error");

    Outcome outcome = searchDumasTitle("The Count of Monte Cristo", "2026");

    assertNull(outcome.failure(), () -> "Failed with " + outcome.failure());
    assertNull(outcome.bd());
    assertEquals("/resources/instances/in00024333360.marcxml.xml", requests.getLast());
  }

//---------------------------------------------------------------------------

  @Test
  void whichFailuresMeanTheSruServerCannotBeUsed()
  {
    assertTrue (LibraryOfCongressBibData.sruUnusable(new HttpResponseException(503, "http://test")));
    assertTrue (LibraryOfCongressBibData.sruUnusable(new HttpTimeoutException("request timed out")), "A blocked port often goes unanswered rather than refused");
    assertTrue (LibraryOfCongressBibData.sruUnusable(new ConnectException("Connection refused")));

    assertFalse(LibraryOfCongressBibData.sruUnusable(new CancelledTaskException()), "A cancellation is the user's");
    assertFalse(LibraryOfCongressBibData.sruUnusable(new IllegalStateException("Thrown while a record was read")), "A fault in reading a record would recur with the other service's copy of it");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
