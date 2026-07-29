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

package org.hypernomicon.previewWindow;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.BooleanSupplier;

import org.hypernomicon.previewWindow.ConversionSession.ConversionState;
import org.hypernomicon.previewWindow.ConversionSession.NoOfficeInstallationException;
import org.hypernomicon.previewWindow.DocumentArtifactService.ConverterBackend;
import org.hypernomicon.previewWindow.DocumentArtifactService.ConverterState;
import org.hypernomicon.util.file.FilePath;

import org.jodconverter.core.office.OfficeException;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * Contract tests for {@link DocumentArtifactService}, run against a fake
 * {@link ConverterBackend} (no LibreOffice involved). These are the executable
 * boundary contracts for the conversion service: keyed join, FIFO without
 * displacement, retry/salvage policy, cache retention/eviction/leasing, and
 * shutdown semantics. Extraction futures (which complete off the FX thread)
 * are used for synchronization throughout, so no JavaFX bootstrap is needed.
 */
class DocumentArtifactServiceTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String DOCX_MIME = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                              FAKE_OFFICE_PATH = "C:\\fake\\office";

  private static final byte[] COMPLETE_PDF_BYTES = "%PDF-1.4 fake artifact content padding padding %%EOF".getBytes(StandardCharsets.US_ASCII);

  @TempDir Path tempDirPath;

  private FakeBackend backend;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Fake converter: successful conversions write a structurally complete PDF
   * to the target; failure behavior is scripted per test via the fields.
   */
  private static final class FakeBackend implements ConverterBackend
  {
    private final List<String> convertedSources = Collections.synchronizedList(new ArrayList<>());

    private volatile boolean running = false, failEnsureRunning = false, writeCompletePdfBeforeThrow = false;
    private volatile int conversionsToFail = 0;

    private volatile CountDownLatch convertStarted = null, convertMayProceed = null,
                                    ensureRunningStarted = null, ensureRunningMayProceed = null;

    @Override public boolean needsStart(String officePath) { return running == false; }

    @Override public boolean ensureRunning(String officePath)
    {
      if (ensureRunningStarted != null)
        ensureRunningStarted.countDown();

      if (ensureRunningMayProceed != null)
      {
        try { ensureRunningMayProceed.await(10, TimeUnit.SECONDS); }
        catch (InterruptedException e) { Thread.currentThread().interrupt(); }
      }

      if (failEnsureRunning) return false;

      running = true;
      return true;
    }

    @Override public void convert(FilePath source, FilePath target) throws OfficeException
    {
      convertedSources.add(source.getNameOnly().toString());

      if (convertStarted != null)
        convertStarted.countDown();

      if (convertMayProceed != null)
      {
        try { convertMayProceed.await(10, TimeUnit.SECONDS); }
        catch (InterruptedException e) { Thread.currentThread().interrupt(); }
      }

      if (conversionsToFail > 0)
      {
        conversionsToFail--;

        if (writeCompletePdfBeforeThrow)
          writeTarget(target);

        throw new OfficeException("fake conversion failure");
      }

      writeTarget(target);
    }

    /** A write failure is reported as OfficeException, like the real backend
     *  would if the target directory vanished. During shutdown this is a
     *  legitimate race: the cleanup thread deletes the artifact root while an
     *  unblocked in-flight conversion is still writing. */
    private static void writeTarget(FilePath target) throws OfficeException
    {
      try { Files.write(target.toPath(), COMPLETE_PDF_BYTES); }
      catch (IOException e) { throw new OfficeException("fake backend could not write target", e); }
    }

    @Override public void stop() { running = false; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @BeforeEach void setUp()
  {
    DocumentArtifactService.resetForTesting();

    backend = new FakeBackend();
    DocumentArtifactService.setBackendFactoryForTesting(() -> backend);
    DocumentArtifactService.setOfficeHomeSupplierForTesting(() -> FAKE_OFFICE_PATH);
  }

  @AfterEach void tearDown()
  {
    DocumentArtifactService.shutdown();
    DocumentArtifactService.resetForTesting();
  }

//---------------------------------------------------------------------------

  private FilePath newSourceDoc(String name) throws IOException
  {
    Path filePath = tempDirPath.resolve(name);
    Files.writeString(filePath, "source content of " + name);
    return FilePath.of(filePath);
  }

  private static ConversionSession sessionFor(FilePath filePath)
  {
    return DocumentArtifactService.getOrCreateSession(filePath, DOCX_MIME);
  }

  private static FilePath convertAndAwait(ConversionSession session) throws Exception
  {
    CompletableFuture<FilePath> future = session.subscribeExtraction();
    DocumentArtifactService.enqueue(session);
    return future.get(10, TimeUnit.SECONDS);
  }

//---------------------------------------------------------------------------

  /**
   * Waits for a condition that becomes true asynchronously: cache eviction runs
   * in the worker's post-conversion bookkeeping, which executes after the
   * extraction future (used for test synchronization) has already completed.
   */
  private static void awaitTrue(BooleanSupplier condition, String message) throws InterruptedException
  {
    long deadline = System.currentTimeMillis() + 5000;

    while (condition.getAsBoolean() == false)
    {
      if (System.currentTimeMillis() > deadline)
        fail(message);

      Thread.sleep(25);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void sameContentSharesOneSession() throws Exception
  {
    FilePath source = newSourceDoc("a.docx");

    assertSame(sessionFor(source), sessionFor(source));
  }

//---------------------------------------------------------------------------

  @Test void modifiedContentGetsFreshSession() throws Exception
  {
    FilePath source = newSourceDoc("a.docx");

    ConversionSession before = sessionFor(source);

    Files.writeString(source.toPath(), "modified content that is longer than before");

    assertNotSame(before, sessionFor(source));
  }

//---------------------------------------------------------------------------

  @Test void conversionCompletesSessionWithArtifact() throws Exception
  {
    ConversionSession session = sessionFor(newSourceDoc("a.docx"));

    FilePath artifact = convertAndAwait(session);

    assertEquals(ConversionState.COMPLETED, session.state());
    assertTrue(artifact.exists());
    assertTrue(artifact.toString().contains("hnTempOfficePreview-"));
    assertEquals(List.of("a.docx"), backend.convertedSources);
  }

//---------------------------------------------------------------------------

  @Test void conversionsRunInFifoOrderAndNothingIsDisplaced() throws Exception
  {
    backend.convertStarted = new CountDownLatch(1);
    backend.convertMayProceed = new CountDownLatch(1);

    ConversionSession first  = sessionFor(newSourceDoc("first.docx")),
                      second = sessionFor(newSourceDoc("second.docx")),
                      third  = sessionFor(newSourceDoc("third.docx"));

    CompletableFuture<FilePath> firstFuture  = first .subscribeExtraction(),
                                secondFuture = second.subscribeExtraction(),
                                thirdFuture  = third .subscribeExtraction();

    DocumentArtifactService.enqueue(first);

    assertTrue(backend.convertStarted.await(10, TimeUnit.SECONDS));  // first conversion under way

    DocumentArtifactService.enqueue(second);
    DocumentArtifactService.enqueue(third);

    backend.convertMayProceed.countDown();

    firstFuture .get(10, TimeUnit.SECONDS);
    secondFuture.get(10, TimeUnit.SECONDS);
    thirdFuture .get(10, TimeUnit.SECONDS);

    assertEquals(List.of("first.docx", "second.docx", "third.docx"), backend.convertedSources);
  }

//---------------------------------------------------------------------------

  @Test void reEnqueueJoinsInFlightConversion() throws Exception
  {
    ConversionSession session = sessionFor(newSourceDoc("a.docx"));

    CompletableFuture<FilePath> future = session.subscribeExtraction();

    DocumentArtifactService.enqueue(session);
    DocumentArtifactService.enqueue(session);
    DocumentArtifactService.enqueue(session);

    future.get(10, TimeUnit.SECONDS);

    assertEquals(1, backend.convertedSources.size());
  }

//---------------------------------------------------------------------------

  @Test void noOfficeConfiguredFailsWithSpecificException() throws Exception
  {
    DocumentArtifactService.setOfficeHomeSupplierForTesting(() -> "");

    ConversionSession session = sessionFor(newSourceDoc("a.docx"));

    CompletableFuture<FilePath> future = session.subscribeExtraction();
    DocumentArtifactService.enqueue(session);

    ExecutionException thrown = assertThrows(ExecutionException.class, () -> future.get(10, TimeUnit.SECONDS));

    assertInstanceOf(NoOfficeInstallationException.class, thrown.getCause());
    assertEquals(ConversionState.FAILED, session.state());
  }

//---------------------------------------------------------------------------

  @Test void converterStartupFailureFailsSession() throws Exception
  {
    backend.failEnsureRunning = true;

    ConversionSession session = sessionFor(newSourceDoc("a.docx"));

    CompletableFuture<FilePath> future = session.subscribeExtraction();
    DocumentArtifactService.enqueue(session);

    ExecutionException thrown = assertThrows(ExecutionException.class, () -> future.get(10, TimeUnit.SECONDS));

    assertInstanceOf(IOException.class, thrown.getCause());
    assertEquals(ConverterState.STOPPED, DocumentArtifactService.converterState());
  }

//---------------------------------------------------------------------------

  @Test void failedConversionIsRetriedOnce() throws Exception
  {
    backend.conversionsToFail = 1;

    ConversionSession session = sessionFor(newSourceDoc("a.docx"));

    convertAndAwait(session);

    assertEquals(ConversionState.COMPLETED, session.state());
    assertEquals(2, backend.convertedSources.size());
  }

//---------------------------------------------------------------------------

  @Test void completePdfIsSalvagedWithoutRetry() throws Exception
  {
    backend.conversionsToFail = 1;
    backend.writeCompletePdfBeforeThrow = true;

    ConversionSession session = sessionFor(newSourceDoc("a.docx"));

    FilePath artifact = convertAndAwait(session);

    assertEquals(ConversionState.COMPLETED, session.state());
    assertTrue(artifact.exists());
    assertEquals(1, backend.convertedSources.size());  // salvage means no second attempt
  }

//---------------------------------------------------------------------------

  @Test void doubleFailureFailsSession() throws Exception
  {
    backend.conversionsToFail = 2;

    ConversionSession session = sessionFor(newSourceDoc("a.docx"));

    CompletableFuture<FilePath> future = session.subscribeExtraction();
    DocumentArtifactService.enqueue(session);

    ExecutionException thrown = assertThrows(ExecutionException.class, () -> future.get(10, TimeUnit.SECONDS));

    assertInstanceOf(OfficeException.class, thrown.getCause());
    assertEquals(2, backend.convertedSources.size());
  }

//---------------------------------------------------------------------------

  @Test void converterStateProgressesStartingToRunning() throws Exception
  {
    assertEquals(ConverterState.STOPPED, DocumentArtifactService.converterState());

    backend.ensureRunningStarted = new CountDownLatch(1);
    backend.ensureRunningMayProceed = new CountDownLatch(1);
    backend.convertStarted = new CountDownLatch(1);

    ConversionSession session = sessionFor(newSourceDoc("a.docx"));
    CompletableFuture<FilePath> future = session.subscribeExtraction();
    DocumentArtifactService.enqueue(session);

    // While the office manager is starting up, the state is STARTING (the
    // "starting office document previewer" progress-message window).

    assertTrue(backend.ensureRunningStarted.await(10, TimeUnit.SECONDS));

    assertEquals(ConverterState.STARTING, DocumentArtifactService.converterState());

    backend.ensureRunningMayProceed.countDown();

    // The manager is up BEFORE the first conversion begins: RUNNING is what
    // flips the progress message from "starting the previewer" to "generating
    // the preview", so it must not wait for the first conversion to complete
    // (a first-ever conversion is exactly when the flip is user-visible).

    assertTrue(backend.convertStarted.await(10, TimeUnit.SECONDS));

    assertEquals(ConverterState.RUNNING, DocumentArtifactService.converterState());

    future.get(10, TimeUnit.SECONDS);

    assertEquals(ConverterState.RUNNING, DocumentArtifactService.converterState());
  }

//---------------------------------------------------------------------------

  @Test void completedSessionIsCachedAfterAbandonment() throws Exception
  {
    FilePath source = newSourceDoc("a.docx");
    ConversionSession session = sessionFor(source);

    convertAndAwait(session);  // future completion also unsubscribes the last subscriber

    assertSame(session, sessionFor(source), "completed session should be served from the cache");
    assertTrue(session.convertedPath().exists());
  }

//---------------------------------------------------------------------------

  @Test void vanishedArtifactEvictsCachedSession() throws Exception
  {
    FilePath source = newSourceDoc("a.docx");
    ConversionSession session = sessionFor(source);

    FilePath artifact = convertAndAwait(session);

    Files.delete(artifact.toPath());  // external temp cleaner

    assertNotSame(session, sessionFor(source), "session with a dangling artifact must be replaced");
  }

//---------------------------------------------------------------------------

  @Test void lruEvictionDeletesOldestUnleasedArtifact() throws Exception
  {
    DocumentArtifactService.setCacheCapForTesting(2);

    ConversionSession first  = sessionFor(newSourceDoc("first.docx")),
                      second = sessionFor(newSourceDoc("second.docx"));

    FilePath firstArtifact  = convertAndAwait(first),
             secondArtifact = convertAndAwait(second);

    ConversionSession third = sessionFor(newSourceDoc("third.docx"));
    FilePath thirdArtifact = convertAndAwait(third);

    awaitTrue(() -> firstArtifact.exists() == false, "oldest artifact should have been evicted and deleted");
    assertTrue(secondArtifact.exists());
    assertTrue(thirdArtifact.exists());
  }

//---------------------------------------------------------------------------

  @Test void leaseProtectsArtifactFromEviction() throws Exception
  {
    DocumentArtifactService.setCacheCapForTesting(1);

    ConversionSession first = sessionFor(newSourceDoc("first.docx"));
    FilePath firstArtifact = convertAndAwait(first);

    first.lease();

    ConversionSession second = sessionFor(newSourceDoc("second.docx"));
    convertAndAwait(second);

    ConversionSession third = sessionFor(newSourceDoc("third.docx"));
    convertAndAwait(third);

    assertTrue(firstArtifact.exists(), "leased artifact must survive eviction pressure");

    first.release();

    // The next completion triggers another trim; now the released artifact can go.

    convertAndAwait(sessionFor(newSourceDoc("fourth.docx")));

    awaitTrue(() -> firstArtifact.exists() == false, "released artifact should be evictable again");
  }

//---------------------------------------------------------------------------

  @Test void shutdownCancelsOutstandingSessions() throws Exception
  {
    backend.convertStarted    = new CountDownLatch(1);
    backend.convertMayProceed = new CountDownLatch(1);

    ConversionSession converting = sessionFor(newSourceDoc("converting.docx")),
                      queued     = sessionFor(newSourceDoc("queued.docx"));

    CompletableFuture<FilePath> convertingFuture = converting.subscribeExtraction(),
                                queuedFuture     = queued    .subscribeExtraction();

    DocumentArtifactService.enqueue(converting);
    assertTrue(backend.convertStarted.await(10, TimeUnit.SECONDS));
    DocumentArtifactService.enqueue(queued);

    DocumentArtifactService.shutdown();
    backend.convertMayProceed.countDown();

    // CompletableFuture surfaces CancellationException unwrapped from get()

    assertThrows(CancellationException.class, () -> convertingFuture.get(10, TimeUnit.SECONDS));
    assertThrows(CancellationException.class, () -> queuedFuture    .get(10, TimeUnit.SECONDS));
  }

//---------------------------------------------------------------------------

  @Test void enqueueAfterShutdownCancelsImmediately() throws Exception
  {
    DocumentArtifactService.shutdown();

    ConversionSession session = new ConversionSession(newSourceDoc("late.docx"), DOCX_MIME, null);

    CompletableFuture<FilePath> future = session.subscribeExtraction();
    DocumentArtifactService.enqueue(session);

    // CompletableFuture surfaces CancellationException unwrapped from get()

    assertThrows(CancellationException.class, () -> future.get(10, TimeUnit.SECONDS));
  }

//---------------------------------------------------------------------------

  @Test void officeFinishedWritingPDFChecksHeaderAndTrailer() throws Exception
  {
    Path valid = tempDirPath.resolve("valid.pdf");
    Files.write(valid, COMPLETE_PDF_BYTES);
    assertTrue(DocumentArtifactService.officeFinishedWritingPDF(FilePath.of(valid)));

    Path noTrailer = tempDirPath.resolve("noTrailer.pdf");
    Files.writeString(noTrailer, "%PDF-1.4 content without the trailer marker padding", StandardCharsets.US_ASCII);
    assertFalse(DocumentArtifactService.officeFinishedWritingPDF(FilePath.of(noTrailer)));

    Path noHeader = tempDirPath.resolve("noHeader.pdf");
    Files.writeString(noHeader, "not a pdf at all but long enough to pass length %%EOF", StandardCharsets.US_ASCII);
    assertFalse(DocumentArtifactService.officeFinishedWritingPDF(FilePath.of(noHeader)));

    Path tooShort = tempDirPath.resolve("short.pdf");
    Files.writeString(tooShort, "%PDF-%%EOF", StandardCharsets.US_ASCII);
    assertFalse(DocumentArtifactService.officeFinishedWritingPDF(FilePath.of(tooShort)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
