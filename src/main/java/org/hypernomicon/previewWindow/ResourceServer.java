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
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import com.teamdev.jxbrowser.net.*;
import com.teamdev.jxbrowser.net.callback.InterceptUrlRequestCallback;

import org.hypernomicon.App;
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
 * Registered files are streamed to the browser in bounded chunks. A whole-file
 * write would put two full copies of the file on the Java heap at once (the
 * read buffer plus the RPC message JxBrowser serializes it into), which caused
 * OutOfMemoryError on the network thread for multi-hundred-MB PDFs on machines
 * with small default heaps; chunking caps the transient cost at one chunk.
 */
public final class ResourceServer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ResourceServer() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

  public static final String SCHEME_NAME = "hnres",
                             BASE_URL    = SCHEME_NAME + "://app";

  private static final String PDFJS_PATH_PREFIX = "/pdfjs/",
                              FILE_PATH_PREFIX  = "/file/",
                              CLASSPATH_PREFIX  = "resources/pdfjs/";  // relative to the org.hypernomicon package

  private static final int FILE_CHUNK_SIZE = 16 * 1024 * 1024;

  private static final Map<String, FilePath> tokenToFile = new ConcurrentHashMap<>();
  private static final Map<FilePath, String> fileToToken = new ConcurrentHashMap<>();
  private static final AtomicInteger nextToken = new AtomicInteger(1);

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
  public static String viewerUrl()    { return BASE_URL + "/pdfjs/web/viewer.html?file="; }

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

  public static InterceptUrlRequestCallback callback()
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

    return okFile(params, filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Streams the file to the browser in {@link #FILE_CHUNK_SIZE} chunks. Chunking
   * (rather than one whole-file write) bounds the transient heap cost regardless
   * of file size; see the class javadoc. The stream's entire lifecycle is owned
   * here (opened in the try-with-resources header, after the job exists) so no
   * failure can strand it open. A file that vanishes between the caller's
   * existence check and the open here surfaces as a failed request rather than
   * a 404; the viewer treats both as a load error.
   */
  private static InterceptUrlRequestCallback.Response okFile(InterceptUrlRequestCallback.Params params, FilePath filePath)
  {
    UrlRequestJob job = params.newUrlRequestJob(UrlRequestJob.Options.newBuilder(HttpStatus.OK)
      .addHttpHeader(HttpHeader.of("Content-Type", mimeTypeForName(filePath.getNameOnly().toString())))
      .build());

    try (InputStream inputStream = Files.newInputStream(filePath.toPath()))
    {
      // job.write serializes the chunk into the RPC message before returning, so the
      // buffer can be reused across iterations.

      byte[] buffer = new byte[FILE_CHUNK_SIZE];

      while (true)
      {
        int bytesRead = inputStream.readNBytes(buffer, 0, FILE_CHUNK_SIZE);

        if (bytesRead <= 0) break;

        job.write((bytesRead == FILE_CHUNK_SIZE) ? buffer : Arrays.copyOf(buffer, bytesRead));
      }

      job.complete();
    }
    catch (IOException e)
    {
      // The 200 status is already committed, so this cannot fall back to notFound;
      // fail the request so the browser sees an error rather than a truncated file.

      System.out.println("ResourceServer: error reading file " + filePath + ": " + getThrowableMessage(e));

      try { job.fail(); }
      catch (RuntimeException runtimeException) { noOp(); }
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

  private static InterceptUrlRequestCallback.Response ok(InterceptUrlRequestCallback.Params params, byte[] data, String mimeType)
  {
    UrlRequestJob job = params.newUrlRequestJob(UrlRequestJob.Options.newBuilder(HttpStatus.OK)
      .addHttpHeader(HttpHeader.of("Content-Type", mimeType))
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
