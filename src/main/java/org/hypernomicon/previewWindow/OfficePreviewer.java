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
import static org.hypernomicon.util.StringUtil.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.concurrent.*;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.previewWindow.ConversionSession.ConversionState;
import org.hypernomicon.previewWindow.ConversionSession.NoOfficeInstallationException;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion;

import org.jodconverter.core.office.OfficeException;
import org.jodconverter.core.office.OfficeUtils;
import org.jodconverter.local.LocalConverter;
import org.jodconverter.local.office.LocalOfficeManager;

//---------------------------------------------------------------------------

final class OfficePreviewer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Background-thread work item: a session to convert plus the pieces of
   * context the bkg thread needs that aren't on the session itself (the
   * {@link PreviewWrapper} for the cross-tab "needs refresh" signal, the
   * {@link PDFJSWrapper} that alt-display calls fall back to for dialog-hosted
   * sessions with no PreviewWrapper, and the {@code officePath} captured at
   * enqueue time for converter-config comparison).
   */
  private record PendingWork(ConversionSession session, PreviewWrapper previewWrapper, PDFJSWrapper jsWrapper, String officePath) { }

  private static OfficePreviewThread bkgThread;

  private static final Object LOCK = new Object();

  private static volatile PendingWork nextWork, currentWork;

  /**
   * Key for {@link #sessions}: a conversion is uniquely identified by
   * (source file, target viewer wrapper). Two wrappers requesting the same
   * file get two sessions, in accordance with the per-wrapper temp-dir model.
   */
  private record SessionKey(FilePath file, PreviewWrapper wrapper) { }

  /**
   * Registry of active {@link ConversionSession} instances. Populated by
   * {@link #getOrCreateSession} and purged via each session's {@code onAbandoned}
   * hook when its last subscriber unsubscribes.
   */
  private static final Map<SessionKey, ConversionSession> sessions = new ConcurrentHashMap<>();

  private OfficePreviewer() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

  static boolean getFirstConversion() { return OfficePreviewThread.firstConversion; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Find or create the {@link ConversionSession} for the given
   * (file, jsWrapper) pair. Callers then attach display or extraction
   * subscribers to drive the UI and receive the converted path.
   *
   * <p>The session is held in the registry while it has at least one
   * subscriber (including during terminal states, so a late subscriber can
   * still pick up a cached result). When the last subscriber unsubscribes,
   * the session is purged.
   */
  static ConversionSession getOrCreateSession(FilePath filePath, PreviewWrapper previewWrapper, String mimetypeStr)
  {
    SessionKey key = new SessionKey(filePath, previewWrapper);

    // A cached COMPLETED session's converted file can be deleted out from under it: the
    // per-wrapper temp-dir pre-clean when a different file is converted for the same
    // wrapper, the full temp-folder clear when the office path changes, or an external
    // temp cleaner. Serving the dangling path would fail extraction and blank the viewer
    // (terminal replay delivers the path and enqueueForConversion early-returns on a
    // terminal session, so nothing reconverts), so validate the artifact and start a
    // fresh session if it is gone. The evicted session's remaining subscribers are
    // unaffected: its onAbandoned hook removes by (key, session) pair, so it cannot
    // dislodge the replacement session from the registry.

    ConversionSession existing = sessions.get(key);

    if ((existing != null) && (existing.state() == ConversionState.COMPLETED) && (existing.convertedPath().exists() == false))
      sessions.remove(key, existing);

    return sessions.computeIfAbsent(key, k ->
      new ConversionSession(filePath, previewWrapper, mimetypeStr, session -> sessions.remove(key, session)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Builds the DisplayCallback used for previewing an office document. On
   * COMPLETED, loads the PDF (or HTML for spreadsheets) into the viewer. On
   * FAILED, shows the unable-to-preview indicator, or the no-office message
   * if the failure is a {@link NoOfficeInstallationException}.
   * On CANCELLED, does nothing;
   * cancellation means either supersession by a newer request or the user
   * navigated away, in which case the wrapper's UI has already moved on.
   *
   * <p>{@code previewWrapper} is null for dialog-hosted previews (WorkDlgCtrlr,
   * SelectWorkDlgCtrlr, MergeWorksDlgCtrlr), which own a bare {@code jsWrapper}
   * with no preview pane; in that case the callback drives {@code jsWrapper}
   * directly, mirroring what the PreviewWrapper load methods do for panes.
   *
   * <p>{@code setStartingConverter}/{@code setGenerating} is handled by
   * {@link #enqueueForConversion} directly (before this callback is ever
   * invoked) so the first-conversion-vs-not distinction can use the current
   * bkg-thread state.
   */
  static ConversionSession.DisplayCallback displayCallbackForPreview(FilePath filePath, PreviewWrapper previewWrapper, PDFJSWrapper jsWrapper, boolean convertToHtml, int pageNum)
  {
    return (state, convertedPath, failure) ->
    {
      switch (state)
      {
        case COMPLETED ->
        {
          if (convertToHtml)
          {
            try
            {
              if (previewWrapper != null)
                previewWrapper.loadConvertedHtml(convertedPath);
              else
              {
                jsWrapper.setContentToShowIsDirect(true);
                jsWrapper.loadFile(convertedPath, false);
              }
            }
            catch (IOException e)
            {
              if (previewWrapper != null)
                previewWrapper.setUnable(filePath);
              else
                jsWrapper.setUnable(filePath);
            }
          }
          else
          {
            if (previewWrapper != null)
              previewWrapper.loadConvertedPdfBytes(convertedPath, pageNum);
            else
            {
              jsWrapper.setContentToShowIsDirect(false);
              jsWrapper.loadPdf(convertedPath, pageNum);
            }
          }
        }

        case FAILED ->
        {
          // For the no-office failure, enqueueForConversion already drove the
          // no-office alt display before failing the session; re-asserting it
          // here (rather than doing nothing) matters for the terminal-state
          // replay case, where a late subscriber sees FAILED without any
          // preceding enqueue-time alt-display call.

          if (failure instanceof NoOfficeInstallationException)
          {
            if (previewWrapper != null)
              previewWrapper.setNoOfficeInstallation();
            else
              jsWrapper.setNoOfficeInstallation();
          }
          else if (previewWrapper != null)
            previewWrapper.setUnable(filePath);
          else
            jsWrapper.setUnable(filePath);
        }

        default -> { /* PENDING, CONVERTING, CANCELLED: no action */ }
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Queue a session for the background conversion thread. Handles:
   * <ul>
   *   <li>Office-path validation (drives the no-office alt display and fails the session
   *       with {@link NoOfficeInstallationException} if no office
   *       install is configured);</li>
   *   <li>altDisplay setup on the wrapper ({@code setStartingConverter} for the first conversion
   *       under the current office install, {@code setGenerating} for subsequent ones);</li>
   *   <li>Cross-tab "needs refresh" signal when a different tab's conversion is being displaced;</li>
   *   <li>Supersession: cancels any pending request for a different session on the same wrapper;</li>
   *   <li>The {@code nextWork} slot replacement and {@code bkgThread} wakeup.</li>
   * </ul>
   *
   * <p>altDisplay calls go through the session's {@link PreviewWrapper} when it
   * has one (so the wrapper's initialized guard applies); for dialog-hosted
   * sessions (WorkDlgCtrlr, SelectWorkDlgCtrlr, MergeWorksDlgCtrlr), which have
   * no PreviewWrapper, they drive {@code jsWrapper} directly. Callers enqueueing
   * a pane-hosted session may pass null for {@code jsWrapper}.
   *
   * <p>Package-private so PreviewWrapper (and any other in-package caller that drives
   * sessions directly) can enqueue after subscribing.
   */
  static void enqueueForConversion(ConversionSession session, PreviewWrapper previewWrapper, PDFJSWrapper jsWrapper)
  {
    // If the session already has a result (or has been cancelled/failed),
    // new subscribers picked up the cached outcome via their own subscribe
    // callback. There's nothing for the background thread to do.

    if (session.state().isTerminal())
      return;

    FilePath       filePath  = session.source();
    PreviewWrapper sessionWrapper = session.previewWrapper();

    ConversionSession supersededSession = null;

    synchronized(LOCK)
    {
      String officePath = getOfficeHome();

      if (officePath.isBlank())
      {
        if (sessionWrapper != null)
          sessionWrapper.setNoOfficeInstallation();
        else
          jsWrapper.setNoOfficeInstallation();

        session.fail(new NoOfficeInstallationException());
        return;
      }

      if (bkgThread == null)
        (bkgThread = new OfficePreviewThread()).start();

      if ((currentWork == null) || (currentWork.officePath().equals(officePath) == false))
      {
        if (sessionWrapper != null)
          sessionWrapper.setStartingConverter();
        else
          jsWrapper.setStartingConverter();
      }
      else
      {
        if (sessionWrapper != null)
          sessionWrapper.setGenerating(filePath, false);
        else
          jsWrapper.setGenerating(filePath, false);
      }

      // If a preview is currently being generated in a different tab from the one a preview is now being requested in, set the other tab as needing refresh

      if ((currentWork != nextWork) && (nextWork != null) && (nextWork.session().previewWrapper() != sessionWrapper) && (nextWork.previewWrapper() != null))
        nextWork.previewWrapper().setNeedsRefresh(nextWork.session().source());

      if ((nextWork != null) && (nextWork.session() != session))
        supersededSession = nextWork.session();

      nextWork = new PendingWork(session, previewWrapper, jsWrapper, officePath);
      LOCK.notifyAll();
    }

    if (supersededSession != null)
      supersededSession.cancel(new CancellationException("Superseded by new conversion request"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class OfficePreviewThread extends HyperThread
  {
    private static volatile LocalOfficeManager officeManager;
    private static volatile LocalConverter officeConverter;

    private static volatile boolean shutDown, firstConversion = true;

//---------------------------------------------------------------------------

    private OfficePreviewThread()
    {
      super("OfficePreview");

      setDaemon(false);
    }

//---------------------------------------------------------------------------

    @Override public void run()
    {
      Map<PreviewWrapper, FilePath> wrapperToTempDir = new HashMap<>();
      File tempPath        = null,
           previewFilePath = null;

      while (shutDown == false)
      {
        synchronized(LOCK)
        {
          while ((nextWork == null) && (shutDown == false))
          {
            try { LOCK.wait(); }
            catch (InterruptedException e) { currentThread().interrupt(); return; }
          }
        }

        if (shutDown)
          break;

        ConversionSession preConvertFailure = null;
        Throwable preConvertCause = null;

        synchronized(LOCK)
        {
          PreviewWrapper nextWrapper = nextWork.session().previewWrapper();

          try
          {
            FilePath tempDir = wrapperToTempDir.get(nextWrapper);
            if (tempDir != null)
              FileDeletion.ofDirWithContents(tempDir).nonInteractiveFailureOK().execute();

            tempDir = tempOfficePreviewFolder(false, false).resolve("preview" + randomAlphanumericStr(8));
            tempPath = tempDir.resolve("preview" + randomAlphanumericStr(8) + '.' + (nextWork.session().convertToHtml() ? "html" : "pdf")).toFile();

            wrapperToTempDir.put(nextWrapper, tempDir);
          }
          catch (IOException e)
          {
            preConvertFailure = nextWork.session();
            preConvertCause = e;
            wrapperToTempDir.remove(nextWrapper);
            nextWork = null;
          }

          if ((preConvertFailure == null) && (updateOfficeConverter(nextWork.officePath()) == false))
          {
            preConvertFailure = nextWork.session();
            preConvertCause = new IOException("Office converter unavailable");
            nextWork = null;
          }

          if (preConvertFailure == null)
          {
            currentWork = nextWork;

            PreviewWrapper curWrapper = currentWork.session().previewWrapper();

            if (curWrapper != null)
              curWrapper.setGenerating(currentWork.session().source(), true);
            else
              currentWork.jsWrapper().setGenerating(currentWork.session().source(), true);

            previewFilePath = currentWork.session().source().toFile();
          }
        }

        if (preConvertFailure != null)
        {
          // session.fail() fires the DisplayCallback (which calls setUnable)
          // and completes extraction futures exceptionally.

          preConvertFailure.fail(preConvertCause);
          continue;
        }

        ConversionSession curSession = currentWork.session();

        curSession.markConverting();

        OfficeException conversionFailure = null;

        for (int attemptNdx = 0; attemptNdx < 2; attemptNdx++)
        {
          try
          {
            officeConverter.convert(previewFilePath).to(tempPath).execute();

            conversionFailure = null;
            firstConversion = false;
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

            if ((curSession.convertToHtml() == false) && (tempPath != null) && isCompletePDF(tempPath))
            {
              conversionFailure = null;
              firstConversion = false;
              break;
            }

            conversionFailure = e;

            if (shutDown) break;
          }
        }

        if (conversionFailure != null)
        {
          synchronized(LOCK)
          {
            if (nextWork == currentWork)
              nextWork = null;
          }

          // session.fail() fires the DisplayCallback (which calls setUnable)
          // and the extraction futures. No direct jsWrapper calls needed here.

          curSession.fail(conversionFailure);
          continue;
        }

        FilePath convertedPath = FilePath.of(tempPath);
        boolean discarded;

        synchronized(LOCK)
        {
          // A superseding request for the same wrapper means the user moved on;
          // discard this result. The session-level check ("no subscribers left")
          // is already covered: session.complete() on a CANCELLED session is a
          // no-op, so we don't need to inspect state here for that case.

          boolean superseded = (nextWork != null) && (currentWork != nextWork) && (curSession.previewWrapper() == nextWork.session().previewWrapper());
          discarded = shutDown || superseded;

          if (nextWork == currentWork)
            nextWork = null;
        }

        // Drive session transition outside the LOCK; callbacks fire
        // DisplayCallbacks via Platform.runLater and complete extraction
        // futures, which may invoke caller-supplied code.

        if (discarded)
          curSession.cancel(new CancellationException("Conversion superseded or stopped"));
        else
          curSession.complete(convertedPath);
      }

      if (officeConverter != null)
        OfficeUtils.stopQuietly(officeManager);
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    /**
     * Make sure an officeManager exists corresponding to the current office installation path; if not,
     * stop the existing officeManager if there is one and create a new one for the current installation path
     * @param officePath The currently configured office installation path
     * @return True if able to do conversions; false otherwise
     */
    private static boolean updateOfficeConverter(String officePath)
    {
      if (officePath.isBlank())
        return false;

      if ((currentWork == null) || (currentWork.officePath().equals(officePath) == false))
      {
        if (officeConverter != null)
        {
          OfficeUtils.stopQuietly(officeManager);
          officeConverter = null;
        }

        try
        {
          tempOfficePreviewFolder(true, true);

          List<Integer> ports = new ArrayList<>();
          findAvailablePorts(1, ports);

          officeManager = LocalOfficeManager.builder().portNumbers(ports.getFirst())
                                                      .officeHome(officePath)
                                                      .build();

          officeManager.start();

          officeConverter = LocalConverter.make(officeManager);

          firstConversion = true;
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

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    /**
     * Whether the file is a structurally complete PDF: starts with the {@code %PDF-}
     * header and contains the {@code %%EOF} trailer marker near the end. LibreOffice
     * writes PDFs in a single pass with {@code %%EOF} last, so its presence means the
     * export finished even if the office process died before the conversion task
     * could report success.
     */
    private static boolean isCompletePDF(File file)
    {
      long len = file.length();

      if ((file.isFile() == false) || (len < 32L))
        return false;

      try (RandomAccessFile raf = new RandomAccessFile(file, "r"))
      {
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

    private static final String tempOfficePreviewFolderName = "hnTempOfficePreview";

    private static FilePath tempOfficePreviewFolder(boolean create, boolean clear) throws IOException
    {
      FilePath filePath = tempDir().resolve(tempOfficePreviewFolderName);

      if ((create == false) && (clear == false))
        return filePath;

      if (filePath.exists())
      {
        if (clear)
          FileDeletion.ofDirContentsOnly(filePath).nonInteractiveFailureOK().execute();
      }
      else
      {
        if (create)
          filePath.createDirectory();
      }

      return filePath;
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private static void cleanup()
    {
      shutDown = true;

      synchronized (LOCK) { LOCK.notifyAll(); }

      OfficeUtils.stopQuietly(officeManager);

      officeConverter = null;
    }

//---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void cleanup()
  {
    ConversionSession[] toCancel;

    synchronized(LOCK)
    {
      nextWork = null;
      toCancel = sessions.values().toArray(ConversionSession[]::new);
    }

    // Cancel outside the lock so callbacks don't deadlock on LOCK.

    for (ConversionSession session : toCancel)
      session.cancel(new CancellationException("Office previewer shutting down"));

    new HyperThread("OfficePreviewCleanup", OfficePreviewThread::cleanup).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
