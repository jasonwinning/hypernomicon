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

import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.TestContext.*;
import static org.hypernomicon.util.Util.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.previewWindow.ConversionSession.ConversionState;
import org.hypernomicon.previewWindow.ConversionSession.NoOfficeInstallationException;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion;

import org.jodconverter.core.office.OfficeException;
import org.jodconverter.core.office.OfficeUtils;
import org.jodconverter.local.LocalConverter;
import org.jodconverter.local.office.LocalOfficeManager;

//---------------------------------------------------------------------------

/**
 * UI-agnostic service that converts office documents to preview artifacts
 * (PDF, or HTML for spreadsheets) and caches the results by content identity.
 * This class owns what used to be {@code OfficePreviewer}'s conversion
 * internals, with three deliberate differences from the code it replaced:
 * <ul>
 *   <li><b>No UI calls anywhere.</b> Consumers observe progress through their
 *       {@link ConversionSession} subscriptions; the starting-vs-generating
 *       display distinction derives from {@link #converterState()}. (The old
 *       code drove wrapper alt-displays from inside the conversion path, while
 *       holding its queue lock.)</li>
 *   <li><b>Content-keyed sessions.</b> A conversion is identified by
 *       (source path, size, mtime, output format) rather than by
 *       (file, wrapper), so every pane and dialog requesting the same document
 *       shares one conversion and one cached artifact.</li>
 *   <li><b>A real FIFO queue.</b> Requests are queued and all complete; nothing
 *       is displaced or cancelled by a newer request. (The old single-slot
 *       queue cancelled the pending request on every enqueue and compensated
 *       with a cross-tab needs-refresh signal.)</li>
 * </ul>
 * Completed sessions remain registered as an artifact cache, bounded by
 * {@link #CACHE_CAP}: least-recently-used unleased artifacts are evicted (and
 * their files deleted) when the cap is exceeded. A consumer that is displaying
 * an artifact holds a lease on its session ({@link ConversionSession#lease()})
 * so the file cannot be deleted out from under the viewer.
 * <p>
 * Threading: {@link #queueLock} guards only the queue and registry; the office
 * manager is confined to the worker thread and its startup never blocks
 * enqueuers. Session transitions are fired outside all service locks.
 */
final class DocumentArtifactService
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Seam between the conversion worker and the office process, so contract
   * tests can substitute a fake converter for JodConverter/LibreOffice.
   * All methods except {@link #stop()} are called on the worker thread.
   */
  interface ConverterBackend
  {
    /** Whether {@link #ensureRunning} would need to (re)start the office
     *  process for the given office path (drives the STARTING state). */
    boolean needsStart(String officePath);

    /** Ensure a converter for the given office path is running, (re)starting
     *  it if needed.
     *  @return true if able to do conversions; false otherwise */
    boolean ensureRunning(String officePath);

    /** Convert the source document into the target file, whose extension
     *  determines the output format. */
    void convert(FilePath source, FilePath target) throws OfficeException;

    /** Stop the office process. Callable from any thread. */
    void stop();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Content identity of a conversion: same key = same artifact, shareable
   *  across all consumers. Size/mtime are captured at request time so an
   *  edited file gets a fresh conversion. */
  record ArtifactKey(FilePath sourceFile, long size, Instant modified, boolean convertToHtml) { }

  /** Converter-process lifecycle, observable so display adapters can choose
   *  between "starting converter" and "generating preview" messaging. */
  enum ConverterState { STOPPED, STARTING, RUNNING }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private DocumentArtifactService() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------

  /** Maximum number of terminal, unleased artifacts retained in the cache. */
  private static final int CACHE_CAP = 20;

  private static final Object queueLock = new Object();

  /** FIFO of sessions awaiting conversion. A session appears at most once. */
  private static final ArrayDeque<ConversionSession> queue = new ArrayDeque<>();

  /** All live sessions by content key: queued, converting, and terminal-cached. */
  private static final Map<ArtifactKey, ConversionSession> sessions = new ConcurrentHashMap<>();

  /** Completed sessions in least-recently-used order (most recent last), for cache eviction. */
  private static final LinkedHashSet<ConversionSession> completedLru = new LinkedHashSet<>();

  private static ConversionWorker worker = null;

  /** Production backend factory; tests substitute a fake via {@link #setBackendFactoryForTesting}. */
  private static Supplier<ConverterBackend> backendFactory = JodConverterBackend::new;

  /** Production office-home source; tests substitute via {@link #setOfficeHomeSupplierForTesting}. */
  private static Supplier<String> officeHomeSupplier = DesktopUtil::getOfficeHome;

  /** Read under {@code queueLock} by eviction; written only by the test seams, on
   *  the test thread outside the lock, hence volatile. */
  private static volatile int cacheCap = CACHE_CAP;

  private static volatile ConverterState converterState = ConverterState.STOPPED;

  private static volatile boolean shutDown = false;

//---------------------------------------------------------------------------

  static ConverterState converterState()                                      { return converterState; }
  static void setBackendFactoryForTesting(Supplier<ConverterBackend> factory) { assertThatThisIsUnitTestThread(); backendFactory = factory; }
  static void setOfficeHomeSupplierForTesting(Supplier<String> supplier)      { assertThatThisIsUnitTestThread(); officeHomeSupplier = supplier; }
  static void setCacheCapForTesting(int cap)                                  { assertThatThisIsUnitTestThread(); cacheCap = cap; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Test-only: joins any prior worker thread and resets all service state so
   * each test starts fresh. Call after {@link #shutdown()}.
   */
  static void resetForTesting()
  {
    assertThatThisIsUnitTestThread();

    ConversionWorker priorWorker;

    synchronized (queueLock)
    {
      priorWorker = worker;
      shutDown = true;
      queueLock.notifyAll();
    }

    if (priorWorker != null)
    {
      try { priorWorker.join(5000); }
      catch (InterruptedException e) { Thread.currentThread().interrupt(); }
    }

    Thread priorCleanup = cleanupThread;

    if (priorCleanup != null)
    {
      try { priorCleanup.join(5000); }
      catch (InterruptedException e) { Thread.currentThread().interrupt(); }

      cleanupThread = null;
    }

    synchronized (queueLock)
    {
      shutDown = false;
      worker = null;
      convertingSession = null;
      queue.clear();
      completedLru.clear();
    }

    sessions.clear();
    converterState = ConverterState.STOPPED;
    cacheCap = CACHE_CAP;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Find or create the session for the given document. The key is the file's
   * content identity, so all consumers of the same (unmodified) document share
   * one session. A cached COMPLETED session whose artifact file has vanished
   * (external temp cleaner) is evicted and replaced with a fresh session.
   */
  static ConversionSession getOrCreateSession(FilePath filePath, String mimetypeStr)
  {
    ArtifactKey key = keyFor(filePath, mimetypeStr);

    ConversionSession existing = sessions.get(key);

    if ((existing != null) && (existing.state() == ConversionState.COMPLETED) && (existing.convertedPath().exists() == false))
      removeSession(key, existing);

    return sessions.computeIfAbsent(key, _key ->
    {
      ConversionSession session = new ConversionSession(filePath, mimetypeStr, _session -> sessionAbandoned(_key, _session));
      session.setArtifactKey(_key);
      return session;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ArtifactKey keyFor(FilePath filePath, String mimetypeStr)
  {
    long size = 0L;
    Instant modified = Instant.EPOCH;

    // A missing/unreadable file gets a degenerate key; the conversion then
    // fails naturally and the failed session is purged on abandonment.

    try
    {
      size     = filePath.size();
      modified = filePath.lastModified();
    }
    catch (IOException e) { noOp(); }

    return new ArtifactKey(filePath, size, modified, ConversionSession.determineConvertToHtml(mimetypeStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called via each session's {@code onAbandoned} hook when its last subscriber
   * leaves. A COMPLETED session stays registered as a cache entry (evicted
   * LRU); anything else is purged.
   */
  private static void sessionAbandoned(ArtifactKey key, ConversionSession session)
  {
    if (session.state() == ConversionState.COMPLETED)
    {
      trimCache();
      return;
    }

    removeSession(key, session);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void removeSession(ArtifactKey key, ConversionSession session)
  {
    sessions.remove(key, session);

    synchronized (queueLock)
    {
      queue.remove(session);
      completedLru.remove(session);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Evicts least-recently-completed, unleased, subscriber-less artifacts beyond
   * the cache cap, deleting their artifact directories.
   */
  private static void trimCache()
  {
    List<ConversionSession> evicted = new ArrayList<>();

    synchronized (queueLock)
    {
      Iterator<ConversionSession> it = completedLru.iterator();

      while (((completedLru.size() - evicted.size()) > cacheCap) && it.hasNext())
      {
        ConversionSession candidate = it.next();

        if (candidate.isEvictable())
          evicted.add(candidate);
      }

      evicted.forEach(completedLru::remove);
    }

    for (ConversionSession session : evicted)
    {
      sessions.remove(session.artifactKey(), session);
      deleteArtifactDir(session);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void deleteArtifactDir(ConversionSession session)
  {
    FilePath convertedPath = session.convertedPath();

    if (FilePath.isEmpty(convertedPath) == false)
      FileDeletion.ofDirWithContents(convertedPath.getDirOnly()).nonInteractiveFailureOK().execute();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Queue a session for conversion. If no office installation is configured,
   * the session is failed with {@link NoOfficeInstallationException} (display
   * adapters map that to the no-office message). A session that is already
   * terminal, queued, or converting is left alone: subscribers receive the
   * cached outcome or in-progress notifications through their subscriptions.
   */
  static void enqueue(ConversionSession session)
  {
    if (session.state().isTerminal())
      return;

    if (officeHomeSupplier.get().isBlank())
    {
      session.fail(new NoOfficeInstallationException());
      return;
    }

    boolean rejectedByShutdown = false;

    synchronized (queueLock)
    {
      if (shutDown)
      {
        rejectedByShutdown = true;
      }
      else if ((queue.contains(session) == false) && (session != convertingSession))
      {
        queue.addLast(session);

        if (worker == null)
          (worker = new ConversionWorker()).start();

        queueLock.notifyAll();
      }
    }

    // Cancel outside the lock (session callbacks must never run under it); a
    // session enqueued after shutdown would otherwise sit PENDING forever with
    // its subscribers waiting.

    if (rejectedByShutdown)
      session.cancel(new CancellationException("Office previewer shutting down"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The session currently being converted by the worker, if any. Written by
   *  the worker under {@link #queueLock}; read by {@link #enqueue}. */
  private static ConversionSession convertingSession = null;

//---------------------------------------------------------------------------

  private static final class ConversionWorker extends HyperThread
  {
    private final ConverterBackend backend = backendFactory.get();

//---------------------------------------------------------------------------

    /** Stops the office process from outside the worker thread; called during
     *  shutdown so an in-flight conversion is broken rather than waited out. */
    private void stopOffice()
    {
      backend.stop();
    }

//---------------------------------------------------------------------------

    private ConversionWorker()
    {
      super("OfficePreview");

      setDaemon(false);
    }

//---------------------------------------------------------------------------

    @Override public void run()
    {
      while (shutDown == false)
      {
        ConversionSession session;

        synchronized (queueLock)
        {
          while (queue.isEmpty() && (shutDown == false))
          {
            try { queueLock.wait(); }
            catch (InterruptedException e) { currentThread().interrupt(); return; }
          }

          if (shutDown)
            break;

          session = queue.pollFirst();
          convertingSession = session;
        }

        try
        {
          convert(session);
        }
        catch (RuntimeException | Error e)
        {
          // Keep this thread alive no matter what one conversion does. It is the only
          // thread that drains the queue and the only thing that can move a session to a
          // terminal state, so letting an unchecked throwable escape would strand this
          // session and every session enqueued afterwards on their "generating" display,
          // permanently, with no recovery short of restarting the application. The retry
          // loop in convert() only catches OfficeException, and jodconverter throws an
          // unchecked IllegalStateException when its office manager is not running, which
          // is exactly the state a died-and-not-yet-restarted office process produces.

          System.out.println("Office conversion failed unexpectedly: " + getThrowableMessage(e));
          logThrowable(e);

          // fail() is a no-op once the session is terminal, so this is safe even if
          // convert() already failed or completed it before throwing.

          session.fail(e);

          converterState = ConverterState.STOPPED;  // re-evaluated on the next conversion
        }
        finally
        {
          synchronized (queueLock)
          {
            convertingSession = null;

            if (session.state() == ConversionState.COMPLETED)
            {
              completedLru.remove(session);  // re-add moves it to most-recent position
              completedLru.add(session);
            }
          }

          trimCache();
        }
      }

      backend.stop();

      converterState = ConverterState.STOPPED;
    }

//---------------------------------------------------------------------------

    /** Converts one session's document. Runs entirely without service locks:
     *  office-manager startup and the conversion itself never block enqueuers. */
    private void convert(ConversionSession session)
    {
      String officePath = officeHomeSupplier.get();

      if (officePath.isBlank())
      {
        session.fail(new NoOfficeInstallationException());
        return;
      }

      if (backend.needsStart(officePath))
        converterState = ConverterState.STARTING;

      if (backend.ensureRunning(officePath) == false)
      {
        converterState = ConverterState.STOPPED;
        session.fail(new IOException("Office converter unavailable"));
        return;
      }

      // The office manager is up. This transition must happen here, before
      // markConverting's subscriber push, not after the first conversion
      // completes: the progress displays key on it to flip from "starting the
      // office document previewer" to "generating the preview", and a
      // first-ever conversion is exactly when that flip is visible.

      converterState = ConverterState.RUNNING;

      FilePath targetFile;

      try
      {
        FilePath artifactDir = artifactDirFor(session.artifactKey());

        if (artifactDir.exists())  // stale partial output from an interrupted earlier attempt
          FileDeletion.ofDirWithContents(artifactDir).nonInteractiveFailureOK().execute();

        artifactDir.createDirectories();

        targetFile = artifactDir.resolve("preview." + (session.convertToHtml() ? "html" : "pdf"));
      }
      catch (IOException e)
      {
        session.fail(e);
        return;
      }

      session.markConverting();

      FilePath sourceFile = session.source();
      OfficeException conversionFailure = null;

      for (int attemptNdx = 0; attemptNdx < 2; attemptNdx++)
      {
        try
        {
          backend.convert(sourceFile, targetFile);

          conversionFailure = null;
          break;
        }
        catch (OfficeException e)
        {
          // LibreOffice (observed with 26.2.3.2) sometimes exits cleanly right as a
          // conversion finishes; jodconverter then cancels the task ("Task was cancelled")
          // and restarts the office process in the background. The export itself has
          // usually completed by then: if the target is a structurally complete PDF,
          // use it instead of converting again. Otherwise retry against the restarted
          // process, and only fail the session if that attempt also fails.

          if ((session.convertToHtml() == false) && officeFinishedWritingPDF(targetFile))
          {
            conversionFailure = null;
            break;
          }

          conversionFailure = e;

          if (shutDown) break;
        }
      }

      // Transitions fire subscriber callbacks (display on the FX thread,
      // extraction futures on this thread), so they happen with no service
      // lock held.

      if (shutDown && (conversionFailure != null))
        session.cancel(new CancellationException("Conversion stopped by shutdown"));
      else if (conversionFailure != null)
        session.fail(conversionFailure);
      else
        session.complete(targetFile);
    }

//---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Production {@link ConverterBackend}: JodConverter driving a LibreOffice
   *  process. Manager/converter are written on the worker thread but read by
   *  {@link #stop()} from the shutdown cleanup thread, hence volatile. */
  private static final class JodConverterBackend implements ConverterBackend
  {
    private volatile LocalOfficeManager officeManager = null;
    private volatile LocalConverter officeConverter = null;
    private String managerOfficePath = null;

    @Override public boolean needsStart(String officePath)
    {
      return (officeConverter == null) || (officePath.equals(managerOfficePath) == false);
    }

    @Override public boolean ensureRunning(String officePath)
    {
      if (officePath.isBlank())
        return false;

      if (needsStart(officePath))
      {
        if (officeConverter != null)
        {
          OfficeUtils.stopQuietly(officeManager);
          officeConverter = null;
        }

        try
        {
          List<Integer> ports = new ArrayList<>();
          findAvailablePorts(1, ports);

          officeManager = LocalOfficeManager.builder().portNumbers(ports.getFirst())
                                                      .officeHome(officePath)
                                                      .build();

          officeManager.start();

          officeConverter = LocalConverter.make(officeManager);
          managerOfficePath = officePath;
        }
        catch (OfficeException | IllegalStateException | IOException e)
        {
          OfficeUtils.stopQuietly(officeManager);

          officeConverter = null;
          return false;
        }
      }

      return true;
    }

    @Override public void convert(FilePath source, FilePath target) throws OfficeException
    {
      officeConverter.convert(source.toFile()).to(target.toFile()).execute();  // JodConverter takes File
    }

    @Override public void stop()
    {
      OfficeUtils.stopQuietly(officeManager);
      officeConverter = null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether the office process finished writing the PDF: the file starts with
   * the {@code %PDF-} header and contains the {@code %%EOF} trailer marker
   * near the end. LibreOffice and OpenOffice share the same single-pass PDF
   * export (common OpenOffice.org lineage): a fresh file written front to
   * back with {@code %%EOF} last, never an incremental update. So its
   * presence means the export finished even if the office process died before
   * the conversion task could report success.
   *
   * <p>Named for that producer contract on purpose: this is not a general PDF
   * completeness check and should not become one. Incrementally updated PDFs
   * carry several {@code %%EOF} markers, so for arbitrary files a trailing one
   * proves nothing; the inference is sound only for single-pass writers.
   */
  static boolean officeFinishedWritingPDF(FilePath filePath)
  {
    if (filePath.isFile() == false)
      return false;

    try (RandomAccessFile raf = new RandomAccessFile(filePath.toFile(), "r"))
    {
      long len = filePath.size();

      if (len < 32L)
        return false;

      byte[] head = new byte[5];
      raf.readFully(head);

      if ("%PDF-".equals(new String(head, StandardCharsets.US_ASCII)) == false)
        return false;

      int tailLen = (int) Math.min(1024L, len);
      byte[] tail = new byte[tailLen];

      raf.seek(len - tailLen);
      raf.readFully(tail);

      return new String(tail, StandardCharsets.US_ASCII).contains("%%EOF");
    }
    catch (IOException e)
    {
      return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String ARTIFACT_ROOT_PREFIX = "hnTempOfficePreview-";

  /** Root of this application instance's artifact cache. Instance-scoped so
   *  concurrent Hypernomicon instances cannot delete each other's artifacts. */
  private static FilePath artifactRoot()
  {
    return tempDir().resolve(ARTIFACT_ROOT_PREFIX + InterProcClient.getInstanceID());
  }

//---------------------------------------------------------------------------

  /** Directory holding the artifact for the given key: one directory per
   *  content identity, deterministic so an interrupted conversion's partial
   *  output is found and cleared on retry. */
  private static FilePath artifactDirFor(ArtifactKey key)
  {
    int hash = Objects.hash(key.sourceFile().toString().toLowerCase(), key.size(), key.modified(), key.convertToHtml());

    return artifactRoot().resolve("artifact" + Integer.toHexString(hash));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Shuts the service down: fails queued sessions, cancels the registry,
   * stops the office process on a background thread, and deletes this
   * instance's artifact cache directory.
   */
  static void shutdown()
  {
    List<ConversionSession> toCancel;
    ConversionWorker workerToStop;

    synchronized (queueLock)
    {
      shutDown = true;
      queue.clear();
      completedLru.clear();
      toCancel = new ArrayList<>(sessions.values());
      workerToStop = worker;
      queueLock.notifyAll();
    }

    // Cancel outside the lock so callbacks don't deadlock on it.

    for (ConversionSession session : toCancel)
      session.cancel(new CancellationException("Office previewer shutting down"));

    sessions.clear();

    cleanupThread = new HyperThread("OfficePreviewCleanup", () ->
    {
      if (workerToStop != null)
        workerToStop.stopOffice();  // breaks any in-flight conversion rather than waiting it out

      FilePath root = artifactRoot();

      if (root.exists())
        FileDeletion.ofDirWithContents(root).nonInteractiveFailureOK().execute();
    });

    cleanupThread.start();
  }

  /** Captured so {@link #resetForTesting()} can wait out the asynchronous
   *  cache-directory deletion between tests. */
  private static volatile Thread cleanupThread = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
