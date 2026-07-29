/*
 * Copyright 2026 Jason Winning
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

package org.hypernomicon.fts;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.PrefKey.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.concurrent.*;

import com.teamdev.jxbrowser.browser.Browser;
import com.teamdev.jxbrowser.browser.callback.InjectJsCallback;
import com.teamdev.jxbrowser.js.JsAccessible;
import com.teamdev.jxbrowser.js.JsObject;

import org.apache.commons.text.StringEscapeUtils;

import org.hypernomicon.previewWindow.BrowserEngine;
import org.hypernomicon.previewWindow.ResourceServer;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Extracts text from PDF files using pdf.js running in an off-screen browser
 * under the shared {@link BrowserEngine}. This produces column-aware text
 * extraction where columns are read in the correct order, preserving phrase
 * adjacency within columns.
 * <p>
 * Lifecycle: call {@link #initialize()} before first use, {@link #extractText}
 * for each PDF, and {@link #dispose()} when done. Each instance is one
 * off-screen browser (a Chromium renderer under the shared engine); closing
 * the browser fully releases its renderer process.
 * <p>
 * Thread safety: This class is NOT thread-safe. Each thread that needs to extract
 * PDFs should have its own instance. Multiple instances can coexist (each runs its
 * own renderer). The single-writer assumption is also load-bearing for the
 * non-atomic {@code ++currentRequestID} in {@link #extractText}: if a future change
 * shares an instance across worker threads, that increment must be made atomic.
 */
public class PDFJSTextExtractor
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Result of a successful text extraction. */
  public record ExtractionResult(String text, int[] pageOffsets, int pageCount) {}

  public static final int DEFAULT_EXTRACTION_TIMEOUT_MINUTES = 15;  // overridable per-computer on the FTS settings page

  private volatile Browser browser = null;

  // currentFuture/currentRequestID correlate an async pdf.js callback to the request that is waiting on it.
  // extractText stamps each request with a new ID and echoes it through the JS; a callback whose ID does not
  // equal currentRequestID is stale (e.g. from a prior extraction that timed out but whose JS finished late)
  // and is ignored, so it cannot complete a later file's future. Both are written on the worker thread and
  // read on the JxBrowser callback thread, hence volatile.

  private volatile CompletableFuture<ExtractionResult> currentFuture = null;
  private volatile int currentRequestID = 0;
  private volatile boolean ready = false;

  // Completed when the extractor page's module finishes loading and calls javaApp.pageReady()
  // (a one-time event in initialize()).
  private volatile CompletableFuture<Void> pageReadyFuture = null;

  // The bridge object injected as window.javaApp; a strong reference is required so it is not
  // garbage collected out from under the page.
  private final JavascriptToJava javascriptToJava = new JavascriptToJava();

  // Count of extractions this instance's browser has performed. Written and read only on the worker
  // thread that owns this extractor (the pool grants one worker exclusive use at a time), so a plain int is safe.
  private int extractionCount = 0;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Bridge object exposed to JavaScript as {@code window.javaApp}.
   * pdf.js extraction calls back into these methods when done. JS numbers
   * arrive as double per the JxBrowser type mapping.
   */
  @JsAccessible
  public class JavascriptToJava
  {
    public void pageReady()
    {
      ready = true;

      CompletableFuture<Void> readyFuture = pageReadyFuture;
      if (readyFuture != null) readyFuture.complete(null);
    }

//---------------------------------------------------------------------------

    public void extractionDone(double requestID, String text, String pageOffsetsJson)
    {
      CompletableFuture<ExtractionResult> future = currentFuture;

      if ((future == null) || (((int) requestID) != currentRequestID)) return;  // no active request, or a stale callback from a timed-out extraction

      try
      {
        int[] offsets = parsePageOffsets(pageOffsetsJson);
        int pageCount = (offsets.length > 0) ? offsets.length - 1 : 0;
        future.complete(new ExtractionResult(text, offsets, pageCount));
      }
      catch (Exception e)
      {
        future.completeExceptionally(e);
      }
    }

//---------------------------------------------------------------------------

    public void extractionFailed(double requestID, String errorMessage)
    {
      CompletableFuture<ExtractionResult> future = currentFuture;

      if ((future == null) || (((int) requestID) != currentRequestID)) return;

      future.completeExceptionally(new IOException("pdf.js extraction failed: " + errorMessage));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Whether this extractor is loaded and able to extract. Set false on an extraction timeout (and by
   *  {@link #abort()}); the pool then disposes and replaces this instance rather than reusing a dead one. */
  public boolean isReady() { return ready; }

  /** Number of extractions this instance has performed; the pool uses it to recycle the browser
   *  periodically, before its accumulated memory footprint grows large enough to risk an OOM. */
  public int extractionCount() { return extractionCount; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The per-computer PDF extraction timeout, in seconds, for both indexing and the diagnostic extraction.
   * Read live from prefs so a change on the FTS settings page applies to the next extraction without a
   * restart. The pref stores minutes; a value of 0 (or less) means "no timeout", for which this returns
   * {@link Long#MAX_VALUE}: TimeUnit saturation makes the timed {@code future.get} effectively
   * unbounded, and it likewise unbounds FullTextIndexer's one-second-sliced pool-wait loop.
   */
  static long extractionTimeoutSeconds()
  {
    int minutes = (app == null) ? DEFAULT_EXTRACTION_TIMEOUT_MINUTES : app.prefs.getInt(FTS_EXTRACTION_TIMEOUT, DEFAULT_EXTRACTION_TIMEOUT_MINUTES);

    return (minutes <= 0) ? Long.MAX_VALUE : (minutes * 60L);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Unblock any in-flight extraction (or page load) so a worker parked in {@link #extractText} returns
   * immediately instead of waiting out the full extraction timeout. Called on shutdown
   * (requestStop/close), on a rebuild request, and during pool teardown. What happens to the held
   * {@code Browser} depends on the caller: pool teardown disposes it right away (dispose() is
   * synchronized, so it blocks until the worker is out), while after a rebuild request the returning
   * worker sees {@code isReady() == false} and disposes/replaces the extractor itself. Completing the
   * future with {@code null} makes {@code extractText} return null; the request-ID check then discards
   * any late pdf.js callback.
   */
  public void abort()
  {
    ready = false;

    CompletableFuture<ExtractionResult> extractionFuture = currentFuture;
    if (extractionFuture != null) extractionFuture.complete(null);

    CompletableFuture<Void> readyFuture = pageReadyFuture;
    if (readyFuture != null) readyFuture.complete(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates the off-screen browser and loads the pdf.js extractor page.
   * Must be called before {@link #extractText}. This is a blocking call that waits
   * for the extractor page to be ready.
   *
   * @throws IOException if the extractor page fails to load; returns without initializing
   *                     (leaving {@link #isReady()} false) if the browser engine is unavailable
   */
  public void initialize() throws IOException
  {
    if ((browser != null) || jxBrowserDisabled) return;

    browser = BrowserEngine.newOffScreenBrowser();

    if (browser == null) return;  // engine unavailable (never initialized, or disabled after a failure/crash)

    // Inject the bridge before page scripts run, so javaApp already exists when the
    // extractor module executes (no readiness polling needed; the module calls
    // javaApp.pageReady() when it finishes loading).

    browser.set(InjectJsCallback.class, params ->
    {
      JsObject window = params.frame().executeJavaScript("window");

      if (window != null)
        window.putProperty("javaApp", javascriptToJava);

      return InjectJsCallback.Response.proceed();
    });

    pageReadyFuture = new CompletableFuture<>();

    browser.navigation().loadUrl(ResourceServer.extractorUrl());

    try
    {
      pageReadyFuture.get(30, TimeUnit.SECONDS);
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
      dispose();
      throw new IOException("Interrupted while loading pdf.js extractor page", e);
    }
    catch (ExecutionException | TimeoutException e)
    {
      dispose();
      throw new IOException("Failed to load pdf.js extractor page: " + getThrowableMessage(e), e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Extracts text from the given PDF file using pdf.js.
   *
   * @param filePath the PDF file to extract text from
   * @return the extraction result, or null if extraction failed
   */
  public ExtractionResult extractText(FilePath filePath) { return extractText(filePath, false, 1); }

  /**
   * Extracts text from the given PDF file using pdf.js.
   *
   * @param filePath the PDF file to extract text from
   * @param debug if true, returns per-item debug info instead of concatenated text
   * @param page page number for debug mode (1-based)
   * @return the extraction result, or null if extraction failed
   */
  public ExtractionResult extractText(FilePath filePath, boolean debug, int page)
  {
    if ((browser == null) || (ready == false)) return null;

    extractionCount++;

    // Bump the request ID BEFORE publishing the future, so a stale callback arriving in the gap sees the new ID
    // and is rejected rather than matching and completing this request's future. Single writer (the pool gives
    // one worker exclusive use of an extractor at a time), so the non-atomic ++ is safe; the JS echoes it back.

    int requestID = ++currentRequestID;

    CompletableFuture<ExtractionResult> future = new CompletableFuture<>();
    currentFuture = future;

    // The file is served through the hnres: scheme; the extractor page cannot fetch file:// URLs
    // (cross-origin from the scheme's origin).

    String fileUrl = ResourceServer.urlForFile(filePath),

           script = debug
             ? "extractDebug(" + requestID + ", \"" + StringEscapeUtils.escapeEcmaScript(fileUrl) + "\", " + page + ");"
             : "extractText (" + requestID + ", \"" + StringEscapeUtils.escapeEcmaScript(fileUrl) + "\");";

    // Async variant: fire the script and return; the bridge callback completes the future.
    // The synchronous executeJavaScript would block this worker thread on the browser, which
    // is pointless here and contrary to the v9 threading guidance.

    browser.mainFrame().ifPresent(frame -> frame.executeJavaScript(script, result -> {}));

    try
    {
      return future.get(extractionTimeoutSeconds(), TimeUnit.SECONDS);
    }
    catch (TimeoutException e)
    {
      System.out.println("Full-text indexer: pdf.js extraction timed out for " + filePath + "; recycling extractor");

      // The JS extraction is still running in (and likely thrashing) the off-screen renderer. Under the memory
      // pressure that causes these timeouts, reloading just the page doesn't reliably recover it. So mark this
      // extractor dead: FullTextIndexer's pool then closes the whole browser (reliably reclaiming its renderer's
      // memory) and replaces it with a fresh instance. The request-ID check discards any late pdf.js callback
      // from the abandoned job.

      ready = false;

      return null;
    }
    catch (InterruptedException e)
    {
      // Interrupt means the indexing thread is being shut down; the extractor pool will be disposed shortly
      // anyway, and the request-ID check prevents any late JS callback from completing a different request's future.

      Thread.currentThread().interrupt();
      return null;
    }
    catch (ExecutionException e)
    {
      System.out.println("Full-text indexer: pdf.js extraction failed for " + filePath + ": " + getThrowableMessage(e));
      return null;
    }
    finally
    {
      currentFuture = null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Closes the off-screen browser, releasing its renderer process.
   * Safe to call multiple times, from any thread.
   */
  public synchronized void dispose()
  {
    ready = false;

    if (browser == null) return;

    Browser toClose = browser;
    browser = null;

    try
    {
      if (toClose.isClosed() == false)
        toClose.close();
    }
    catch (RuntimeException e)
    {
      System.out.println("Full-text indexer: error disposing pdf.js extractor: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int[] parsePageOffsets(String json)
  {
    // JSON is a simple array of integers like [0, 1234, 5678, 9012]

    json = json.trim();

    if ("[]".equals(json))
      return new int[0];

    String inner = json.substring(1, json.length() - 1);
    String[] parts = inner.split(",");

    return Arrays.stream(parts).mapToInt(part -> Integer.parseInt(part.trim())).toArray();  // Integer.parseInt because non-numeric should fail loudly here
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
