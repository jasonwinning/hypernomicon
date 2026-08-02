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

package org.hypernomicon.previewWindow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

import com.teamdev.jxbrowser.net.*;
import com.teamdev.jxbrowser.net.callback.InterceptUrlRequestCallback;

import static com.google.common.net.HttpHeaders.*;

import org.hypernomicon.App;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Serves application resources to the browser over the custom {@code hnres:}
 * scheme, registered on the engine at creation ({@link BrowserEngine}).
 * <p>
 * This replaces the JxBrowser 6 {@code jar:} protocol handler and the
 * loadHTML-plus-injected-{@code <base>}-tag machinery, and it is also the only
 * way page content can fetch local files at all: pdf.js 6 is ES modules, which
 * Chromium will not load from {@code file://} (opaque-origin CORS), and a page
 * served from this scheme's origin cannot fetch {@code file://} subresources
 * either. So the interceptor acts as the application's file server:
 * <ul>
 * <li>{@code hnres://app/pdfjs/...} serves the bundled pdf.js distribution and
 *     the application's viewer/extractor JS from the classpath.</li>
 * <li>{@code hnres://app/file/<token>/<name>} serves a local file previously
 *     registered via {@link #urlForFile(FilePath)} (database PDFs, converted
 *     office artifacts, etc.).</li>
 * </ul>
 * Registered files are served with HTTP Range support (Accept-Ranges and
 * Content-Length on every response; 206 partial responses when a Range header
 * is honored). pdf.js only lazy-loads a document when the server offers both;
 * without them it must download the entire file before anything can display,
 * because the cross-reference table it needs first is at the end of the file.
 * With ranges, opening a large PDF fetches the tail and then only the byte
 * ranges the visible pages need, while the rest of the file downloads in the
 * background.
 * <p>
 * Response bodies are streamed in bounded chunks from a small streaming pool
 * rather than the network callback thread. Chunking caps the transient heap
 * cost at one chunk: a whole-file write would put two full copies of the file
 * on the Java heap at once (the read buffer plus the RPC message JxBrowser
 * serializes it into), which caused OutOfMemoryError for multi-hundred-MB PDFs
 * on machines with small default heaps. Streaming off the callback thread lets
 * ranged requests be answered while a long full-file response is still being
 * written; pdf.js issues both kinds concurrently, and lazy loading is defeated
 * if urgent small ranges must wait behind the background full download.
 */
public final class ResourceServer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ResourceServer() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

  static final String SCHEME_NAME = "hnres";

  private static final String BASE_URL          = SCHEME_NAME + "://app",
                              PDFJS_PATH_PREFIX = "/pdfjs/",
                              FILE_PATH_PREFIX  = "/file/",
                              CLASSPATH_PREFIX  = "resources/pdfjs/";  // relative to the org.hypernomicon package

  private static final int FILE_CHUNK_SIZE = 16 * 1024 * 1024;

  private static final Map<String, FilePath> tokenToFile = new ConcurrentHashMap<>();
  private static final Map<FilePath, String> fileToToken = new ConcurrentHashMap<>();
  private static final AtomicInteger nextToken = new AtomicInteger(1);

  /** Streams file response bodies off the network callback thread (see
   *  {@link #streamFileBytes}). Sized for the realistic concurrency: one
   *  full-file background download plus a few ranged fetches per document;
   *  excess requests queue. */
  private static final ExecutorService streamExecutor = Executors.newFixedThreadPool(4, runnable ->
  {
    HyperThread thread = new HyperThread("ResourceServer-stream", runnable);
    thread.setDaemon(true);
    return thread;
  });

  private static final Map<String, String> MIME_TYPES = Map.ofEntries(
    Map.entry("html"      , "text/html"),
    Map.entry("htm"       , "text/html"),
    Map.entry("mjs"       , "text/javascript"),
    Map.entry("js"        , "text/javascript"),
    Map.entry("css"       , "text/css"),
    Map.entry("svg"       , "image/svg+xml"),
    Map.entry("png"       , "image/png"),
    Map.entry("jpg"       , "image/jpeg"),
    Map.entry("jpeg"      , "image/jpeg"),
    Map.entry("gif"       , "image/gif"),
    Map.entry("cur"       , "image/x-icon"),
    Map.entry("pdf"       , "application/pdf"),
    Map.entry("wasm"      , "application/wasm"),
    Map.entry("json"      , "application/json"),
    Map.entry("ftl"       , "text/plain"),
    Map.entry("properties", "text/plain"));

//---------------------------------------------------------------------------

  /** URL of the bundled pdf.js viewer page. The empty {@code file} parameter
   *  tells the stock viewer not to load its default document. */
  static String viewerUrl()           { return BASE_URL + "/pdfjs/web/viewer.html?file="; }

  /** URL of the bundled pdf.js extractor page (off-screen text extraction). */
  public static String extractorUrl() { return BASE_URL + "/pdfjs/web/extractor.html"; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns an {@code hnres:} URL through which the browser can fetch the given
   * local file. Registers the file on first use; the same file always maps to
   * the same URL. The file name is included in the URL for the benefit of
   * anything that sniffs the extension or displays the name.
   */
  public static String urlForFile(FilePath filePath)
  {
    String token = fileToToken.computeIfAbsent(filePath, _filePath ->
    {
      String newToken = String.valueOf(nextToken.getAndIncrement());
      tokenToFile.put(newToken, _filePath);
      return newToken;
    });

    return BASE_URL + FILE_PATH_PREFIX + token + '/' + escapeURL(filePath.getNameOnly().toString(), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static InterceptUrlRequestCallback callback()
  {
    return params ->
    {
      String urlPath = URI.create(params.urlRequest().url()).getPath();

      if (urlPath == null)
        return notFound(params, "(no path)");

      if (urlPath.startsWith(PDFJS_PATH_PREFIX))
        return serveClasspath(params, urlPath.substring(PDFJS_PATH_PREFIX.length()));

      if (urlPath.startsWith(FILE_PATH_PREFIX))
        return serveRegisteredFile(params, urlPath.substring(FILE_PATH_PREFIX.length()));

      return notFound(params, urlPath);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static InterceptUrlRequestCallback.Response serveClasspath(InterceptUrlRequestCallback.Params params, String relPath)
  {
    if (relPath.contains(".."))
      return notFound(params, relPath);

    try (InputStream inputStream = App.class.getResourceAsStream(CLASSPATH_PREFIX + relPath))
    {
      if (inputStream == null)
        return notFound(params, relPath);

      return ok(params, inputStream.readAllBytes(), mimeTypeForName(relPath));
    }
    catch (IOException e)
    {
      System.out.println("ResourceServer: error reading classpath resource " + relPath + ": " + getThrowableMessage(e));
      return notFound(params, relPath);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static InterceptUrlRequestCallback.Response serveRegisteredFile(InterceptUrlRequestCallback.Params params, String tokenAndName)
  {
    int slashNdx = tokenAndName.indexOf('/');

    String token = (slashNdx < 0) ? tokenAndName : tokenAndName.substring(0, slashNdx);

    FilePath filePath = tokenToFile.get(token);

    if ((filePath == null) || (filePath.exists() == false))
      return notFound(params, tokenAndName);

    long fileSize;

    try
    {
      fileSize = filePath.size();
    }
    catch (IOException e)
    {
      System.out.println("ResourceServer: error reading file " + filePath + ": " + getThrowableMessage(e));
      return notFound(params, tokenAndName);
    }

    long start = 0, endInclusive = fileSize - 1;
    boolean partial = false;

    String rangeValue = headerValue(params, RANGE);

    if (rangeValue != null)
    {
      ByteRange range = parseRange(rangeValue, fileSize);

      if (range != null)
      {
        if ((range.start() >= fileSize) || (range.start() > range.endInclusive()))
          return rangeNotSatisfiable(params, fileSize);

        start = range.start();
        endInclusive = range.endInclusive();
        partial = true;
      }
    }

    return okFileBytes(params, filePath, fileSize, start, endInclusive, partial);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A parsed Range header: an inclusive byte interval within the requested file. */
  private record ByteRange(long start, long endInclusive) { }

  /**
   * Parses a Range request header value against the given file size. Returns null
   * when the header is not a single bytes range (including multi-range requests,
   * which Chromium and pdf.js never send); the caller then serves the full file,
   * which is always a valid response to a Range request. A returned range has its
   * end clamped to the file but may still be unsatisfiable (start at or past the
   * end of the file); the caller answers 416.
   */
  private static ByteRange parseRange(String headerValue, long fileSize)
  {
    String value = headerValue.trim();

    if (value.regionMatches(true, 0, "bytes=", 0, 6) == false)
      return null;

    value = value.substring(6).trim();

    if (value.indexOf(',') >= 0)
      return null;

    int dashNdx = value.indexOf('-');

    if (dashNdx < 0)
      return null;

    String startStr = value.substring(0, dashNdx).trim(),
           endStr   = value.substring(dashNdx + 1).trim();

    try
    {
      if (startStr.isEmpty())
      {
        // Suffix form (the last N bytes); a suffix of zero bytes is unsatisfiable,
        // expressed as a start past the end of the file.

        long suffixLen = Long.parseLong(endStr);

        return (suffixLen <= 0) ? new ByteRange(fileSize, fileSize) : new ByteRange(Math.max(0, fileSize - suffixLen), fileSize - 1);
      }

      long start = Long.parseLong(startStr),
           endInclusive = endStr.isEmpty() ? (fileSize - 1) : Math.min(Long.parseLong(endStr), fileSize - 1);

      return new ByteRange(start, endInclusive);
    }
    catch (NumberFormatException e)
    {
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Returns the value of the named request header, or null if absent. */
  private static String headerValue(InterceptUrlRequestCallback.Params params, String name)
  {
    return params.httpHeaders().stream().filter(header -> header.name().equalsIgnoreCase(name))
                                        .map(HttpHeader::value)
                                        .findFirst()
                                        .orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Responds with the given inclusive byte range of the file: the whole file as
   * a 200 when partial is false, otherwise a 206 with a Content-Range. Every
   * response advertises range support and the exact length, which is what lets
   * pdf.js lazy-load large documents. The body is streamed from the streaming
   * pool; see {@link #streamFileBytes}.
   */
  private static InterceptUrlRequestCallback.Response okFileBytes(InterceptUrlRequestCallback.Params params, FilePath filePath,
                                                                  long fileSize, long start, long endInclusive, boolean partial)
  {
    long contentLength = (endInclusive - start) + 1;

    UrlRequestJob.Options.Builder builder = UrlRequestJob.Options.newBuilder(partial ? HttpStatus.PARTIAL_CONTENT : HttpStatus.OK)
      .addHttpHeader(HttpHeader.of(CONTENT_TYPE, mimeTypeForName(filePath.getNameOnly().toString())))
      .addHttpHeader(HttpHeader.of(ACCEPT_RANGES, "bytes"))
      .addHttpHeader(HttpHeader.of(CONTENT_LENGTH, String.valueOf(contentLength)));

    if (partial)
      builder.addHttpHeader(HttpHeader.of(CONTENT_RANGE, "bytes " + start + '-' + endInclusive + '/' + fileSize));

    UrlRequestJob job = params.newUrlRequestJob(builder.build());

    streamExecutor.execute(() -> streamFileBytes(job, filePath, start, contentLength));

    return InterceptUrlRequestCallback.Response.intercept(job);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Opens the file and streams contentLength bytes, starting at the given
   * offset, into the job in {@link #FILE_CHUNK_SIZE} chunks, then completes the
   * job. Runs on a streaming-pool thread rather than the network callback
   * thread; see the class javadoc for why both the chunking and the thread
   * matter. The channel's entire lifecycle is owned here (opened in the
   * try-with-resources header) so no failure can strand it open. A file that
   * vanishes or becomes unreadable after the caller's checks surfaces as a
   * failed request rather than a 404, because the status is already committed;
   * the viewer treats both as a load error.
   */
  private static void streamFileBytes(UrlRequestJob job, FilePath filePath, long start, long contentLength)
  {
    try (SeekableByteChannel channel = Files.newByteChannel(filePath.toPath()))
    {
      channel.position(start);

      long remaining = contentLength;

      byte[] buffer = new byte[(int) Math.min(FILE_CHUNK_SIZE, contentLength)];

      while (remaining > 0)
      {
        int bytesRead = channel.read(ByteBuffer.wrap(buffer, 0, (int) Math.min(buffer.length, remaining)));

        if (bytesRead <= 0) break;

        // job.write serializes the chunk into the RPC message before returning, so the
        // buffer can be reused across iterations.

        job.write((bytesRead == buffer.length) ? buffer : Arrays.copyOf(buffer, bytesRead));

        remaining -= bytesRead;
      }

      if (remaining > 0)
      {
        // The file shrank between the size check and this read; the promised
        // Content-Length can no longer be honored, so fail the request rather
        // than deliver a silently truncated body.

        System.out.println("ResourceServer: file shorter than expected: " + filePath);

        job.fail();
      }
      else
        job.complete();
    }
    catch (IOException e)
    {
      // The status is already committed, so this cannot fall back to notFound; fail
      // the request so the browser sees an error rather than a truncated file.

      System.out.println("ResourceServer: error reading file " + filePath + ": " + getThrowableMessage(e));

      try { job.fail(); }
      catch (RuntimeException runtimeException) { noOp(); }
    }
    catch (RuntimeException e)
    {
      // Chromium closed the connection before the write finished. This is routine:
      // pdf.js cancels the initial full-file request as soon as its headers reveal
      // range support (auto-fetch is disabled; see javaapp.js), and it also happens
      // on rapid preview switching. The response is moot; swallow so the exception
      // cannot escape and destabilize the engine.

      if (app.debugging)
        System.out.println("ResourceServer: connection closed before the response was fully written (request cancelled or superseded)");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static InterceptUrlRequestCallback.Response ok(InterceptUrlRequestCallback.Params params, byte[] data, String mimeType)
  {
    UrlRequestJob job = params.newUrlRequestJob(UrlRequestJob.Options.newBuilder(HttpStatus.OK)
      .addHttpHeader(HttpHeader.of(CONTENT_TYPE, mimeType))
      .build());

    try
    {
      job.write(data);
      job.complete();
    }
    catch (RuntimeException e)
    {
      // The request was superseded and Chromium closed the connection before the
      // write finished (e.g. rapid preview switching). The response is moot;
      // swallow so the exception cannot escape onto the JxBrowser network thread
      // and destabilize the engine.

      if (app.debugging)
        System.out.println("ResourceServer: connection closed before the response was written (superseded request)");
    }

    return InterceptUrlRequestCallback.Response.intercept(job);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** 416 response for a syntactically valid but unsatisfiable byte range. */
  private static InterceptUrlRequestCallback.Response rangeNotSatisfiable(InterceptUrlRequestCallback.Params params, long fileSize)
  {
    UrlRequestJob job = params.newUrlRequestJob(UrlRequestJob.Options.newBuilder(HttpStatus.REQUESTED_RANGE_NOT_SATISFIABLE)
      .addHttpHeader(HttpHeader.of(CONTENT_RANGE, "bytes */" + fileSize))
      .build());

    job.complete();

    return InterceptUrlRequestCallback.Response.intercept(job);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static InterceptUrlRequestCallback.Response notFound(InterceptUrlRequestCallback.Params params, String urlPath)
  {
    if ("favicon.ico".equals(urlPath) == false)
      System.out.println("ResourceServer: 404 for " + urlPath);

    UrlRequestJob job = params.newUrlRequestJob(UrlRequestJob.Options.newBuilder(HttpStatus.NOT_FOUND).build());
    job.complete();

    return InterceptUrlRequestCallback.Response.intercept(job);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String mimeTypeForName(String name)
  {
    int dotNdx = name.lastIndexOf('.');

    String ext = (dotNdx < 0) ? "" : name.substring(dotNdx + 1).toLowerCase();

    return MIME_TYPES.getOrDefault(ext, "application/octet-stream");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
