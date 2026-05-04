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
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.concurrent.*;

import com.teamdev.jxbrowser.chromium.*;
import com.teamdev.jxbrowser.chromium.events.FinishLoadingEvent;
import com.teamdev.jxbrowser.chromium.events.LoadAdapter;
import com.teamdev.jxbrowser.chromium.internal.Environment;
import com.teamdev.jxbrowser.chromium.internal.ipc.IPCException;

import org.apache.commons.text.StringEscapeUtils;

import org.hypernomicon.App;
import org.hypernomicon.previewWindow.BrowserTracker;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Extracts text from PDF files using pdf.js running in an off-screen JxBrowser
 * instance. This produces column-aware text extraction where columns are read
 * in the correct order, preserving phrase adjacency within columns.
 * <p>
 * Lifecycle: call {@link #initialize()} before first use, {@link #extractText}
 * for each PDF, and {@link #dispose()} when done. The off-screen Chromium process
 * is created on {@link #initialize()} and destroyed on {@link #dispose()}.
 * <p>
 * Thread safety: This class is NOT thread-safe. Each thread that needs to extract
 * PDFs should have its own instance. Multiple instances can coexist (each runs its
 * own Chromium process). The single-writer assumption is also load-bearing for the
 * non-atomic {@code ++currentRequestID} in {@link #extractText}: if a future change
 * shares an instance across worker threads, that increment must be made atomic.
 */
public class PDFJSTextExtractor
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Result of a successful text extraction. */
  public record ExtractionResult(String text, int[] pageOffsets, int pageCount) {}

  static final long EXTRACTION_TIMEOUT_SECONDS = 900;  // 15 minutes; same value for indexing and diagnostic extraction

  private static final String BASE_PLACEHOLDER = "<!-- base placeholder -->";

  private static String extractorHTMLStr = null;

  private volatile Browser browser = null;

  // currentFuture/currentRequestID correlate an async pdf.js callback to the request that is waiting on it.
  // extractText stamps each request with a new ID and echoes it through the JS; a callback whose ID does not
  // equal currentRequestID is stale (e.g. from a prior extraction that timed out but whose JS finished late)
  // and is ignored, so it cannot complete a later file's future. Both are written on the worker thread and
  // read on the JxBrowser callback thread, hence volatile.

  private volatile CompletableFuture<ExtractionResult> currentFuture = null;
  private volatile int currentRequestID = 0;
  private volatile boolean ready = false;

  // Completed by the load listener when the extractor page finishes loading (a one-time event in initialize()).
  private volatile CompletableFuture<Void> pageReadyFuture = null;

  // Count of extractions this instance's Chromium process has performed. Written and read only on the worker
  // thread that owns this extractor (the pool grants one worker exclusive use at a time), so a plain int is safe.
  private int extractionCount = 0;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Bridge object exposed to JavaScript as {@code window.javaApp}.
   * pdf.js extraction calls back into these methods when done.
   */
  public class JavascriptToJava
  {
    public void extractionDone(int requestID, String text, String pageOffsetsJson)
    {
      CompletableFuture<ExtractionResult> future = currentFuture;

      if ((future == null) || (requestID != currentRequestID)) return;  // no active request, or a stale callback from a timed-out extraction

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

    public void extractionFailed(int requestID, String errorMessage)
    {
      CompletableFuture<ExtractionResult> future = currentFuture;

      if ((future == null) || (requestID != currentRequestID)) return;

      future.completeExceptionally(new IOException("pdf.js extraction failed: " + errorMessage));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Whether this extractor is loaded and able to extract. Set false on an extraction timeout (and by
   *  {@link #abort()}); the pool then disposes and replaces this instance rather than reusing a dead one. */
  public boolean isReady() { return ready; }

  /** Number of extractions this instance has performed; the pool uses it to recycle the Chromium process
   *  periodically, before its accumulated memory footprint grows large enough to risk an OOM. */
  public int extractionCount() { return extractionCount; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Unblock any in-flight extraction (or page load) so a worker parked in {@link #extractText} returns
   * immediately instead of waiting out the full extraction timeout. Called during shutdown: a parked worker
   * still holds this extractor's {@code Browser}, which would keep {@code BrowserCore.shutdown()} from
   * succeeding until the timeout elapses. Completing the future with {@code null} makes {@code extractText}
   * return null; the request-ID check then discards any late pdf.js callback. The held {@code Browser} is
   * disposed by the caller after the worker releases it.
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
   * Creates the off-screen Chromium browser and loads the pdf.js extractor page.
   * Must be called before {@link #extractText}. This is a blocking call that waits
   * for the browser to be ready.
   * <p>
   * JxBrowser must already be initialized (via {@link PDFJSWrapper#init()}) before
   * calling this method.
   *
   * @throws IOException if the extractor HTML cannot be loaded
   * @throws RuntimeException if the browser fails to initialize
   */
  public void initialize() throws IOException
  {
    if ((browser != null) || jxBrowserDisabled) return;

    if (jxBrowserInitialized == false)
      PDFJSWrapper.init();

    if (jxBrowserDisabled) return;

    if (extractorHTMLStr == null)
      initExtractorHTML();

    browser = new Browser(BrowserType.LIGHTWEIGHT);

    BrowserTracker.register(browser, "PDFJSTextExtractor");

    PDFJSWrapper.addCustomProtocolHandler(browser, "jar");

    browser.addLoadListener(new LoadAdapter()
    {
      @Override public void onFinishLoadingFrame(FinishLoadingEvent event)
      {
        if (event.isMainFrame() == false) return;

        // Fires when the extractor page finishes loading (the one-time load in initialize()); it completes
        // the pending pageReadyFuture (completing an already-completed one is a harmless no-op).

        CompletableFuture<Void> readyFuture = pageReadyFuture;

        try
        {
          // Both calls can throw IllegalStateException if the browser was disposed mid-load (e.g. a
          // concurrent abort()). Keep both inside the try so either failure completes pageReadyFuture
          // promptly rather than leaving loadExtractorPageAndWait to fall back to its 30s timeout.

          JSValue window = browser.executeJavaScriptAndReturnValue("window");
          window.asObject().setProperty("javaApp", new JavascriptToJava());
        }
        catch (IllegalStateException e)
        {
          if (readyFuture != null) readyFuture.completeExceptionally(e);
          return;
        }

        ready = true;
        if (readyFuture != null) readyFuture.complete(null);
      }
    });

    try
    {
      loadExtractorPageAndWait();
    }
    catch (IOException e)
    {
      dispose();
      throw e;
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

    String fileUrl = filePath.toURLString();

    browser.executeJavaScript(debug
      ? "extractDebug(" + requestID + ", \"" + StringEscapeUtils.escapeEcmaScript(fileUrl) + "\", " + page + ");"
      : "extractText (" + requestID + ", \"" + StringEscapeUtils.escapeEcmaScript(fileUrl) + "\");");

    try
    {
      return future.get(EXTRACTION_TIMEOUT_SECONDS, TimeUnit.SECONDS);
    }
    catch (TimeoutException e)
    {
      System.out.println("Full-text indexer: pdf.js extraction timed out for " + filePath + "; recycling extractor");

      // The JS extraction is still running in (and likely thrashing) the off-screen Chromium. Reloading just the
      // page doesn't reliably recover a wedged process. Under the memory pressure that causes these timeouts, even
      // the reload can hang. So mark this extractor dead: FullTextIndexer's pool then disposes the whole Chromium
      // process (reliably reclaiming its memory) and replaces it with a fresh instance. The request-ID check
      // discards any late pdf.js callback from the abandoned job.

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
   * Creates a fresh {@link #pageReadyFuture}, loads the extractor HTML into the browser, and blocks
   * until the load listener completes the future (or 30s elapses). Used for the initial load in
   * {@link #initialize()}.
   */
  private void loadExtractorPageAndWait() throws IOException
  {
    pageReadyFuture = new CompletableFuture<>();

    browser.loadHTML(extractorHTMLStr);

    try
    {
      pageReadyFuture.get(30, TimeUnit.SECONDS);
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
      throw new IOException("Interrupted while loading pdf.js extractor page", e);
    }
    catch (ExecutionException | TimeoutException e)
    {
      throw new IOException("Failed to load pdf.js extractor page: " + getThrowableMessage(e), e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Disposes the off-screen Chromium browser and frees its resources.
   * Safe to call multiple times.
   */
  public synchronized void dispose()
  {
    ready = false;

    if (browser == null) return;

    Browser toDispose = browser;
    browser = null;

    if (toDispose.isDisposed()) return;

    CountDownLatch latch = new CountDownLatch(1);
    toDispose.addDisposeListener(event -> latch.countDown());

    Runnable runnable = () ->
    {
      try
      {
        toDispose.dispose();
      }
      catch (IPCException e)
      {
        latch.countDown();
        System.out.println("Full-text indexer: error disposing pdf.js extractor: " + getThrowableMessage(e));
      }
    };

    // JxBrowser 6 requires Browser.dispose() to run on a specific thread, and which thread differs by OS:
    // on Linux and macOS it must be the JavaFX Application Thread (the UI thread); on Windows it must NOT
    // be the UI thread. Disposing on the wrong thread deadlocks on the native side. This mirrors
    // PDFJSWrapper.dispose(); FullTextIndexer routes extractor disposal here from the FX thread on
    // Linux/macOS (close() disposes the pool after the background-thread join) so this never blocks the FX thread.

    if (Environment.isWindows())
      runOutsideFXThread(runnable);
    else
      runInFXThread(runnable);

    // Wait for disposal to complete before returning, so that the caller
    // can safely dispose the next browser (sequential disposal required)

    try
    {
      if (latch.await(10, TimeUnit.SECONDS) == false)
        System.out.println("Full-text indexer: timed out waiting for pdf.js extractor disposal; next disposal may overlap.");
    }
    catch (InterruptedException e) { Thread.currentThread().interrupt(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initExtractorHTML() throws IOException
  {
    StringBuilder sb = new StringBuilder();

    readResourceTextFile("resources/pdfjs/web/extractor.html", sb);

    int ndx = sb.indexOf(BASE_PLACEHOLDER);

    String pathStr = App.class.getResource("resources/pdfjs/web").toExternalForm();

    if (pathStr.contains("file:/") && (pathStr.contains("file:///") == false))
      pathStr = pathStr.replace("file:/", "file:///");

    String baseTag = "<base href=\"" + pathStr + "/\" />";

    sb.replace(ndx, ndx + BASE_PLACEHOLDER.length(), baseTag);

    extractorHTMLStr = sb.toString();
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
