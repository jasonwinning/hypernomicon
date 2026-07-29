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

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

import org.hypernomicon.previewWindow.ConversionSession.*;
import org.hypernomicon.util.FxTestUtil;
import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Unit tests for the {@link ConversionSession} state machine and subscriber
 * plumbing. These tests drive transitions directly (calling the package-private
 * {@code markConverting}/{@code complete}/{@code fail}/{@code cancel} methods)
 * so that the state machine can be validated in isolation from the actual
 * conversion worker and JodConverter.
 *
 * <p>All tests run on the JavaFX application thread via
 * {@link FxTestUtil#runFxAndWait} so that display callbacks dispatched via
 * {@link javafx.application.Platform#runLater} are invoked synchronously
 * within the test scope. {@code jsWrapper} is always {@code null}; UI-facing
 * calls inside the session are skipped in that case.
 */
@SuppressWarnings("resource")  // Subscription implements AutoCloseable; tests intentionally leak.
class ConversionSessionTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @BeforeAll
  static void initFx()
  {
    FxTestUtil.initJfx();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ConversionSession newSession()
  {
    return new ConversionSession(FilePath.of("test.docx"),
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document", null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Records DisplayCallback invocations for verification. */
  private static final class DisplayRecorder implements DisplayCallback
  {
    private final List<Event> events = new CopyOnWriteArrayList<>();

    private record Event(ConversionState state, FilePath path, Throwable failure) { }

    @Override public void onStateChange(ConversionState state, FilePath path, Throwable failure)
    {
      events.add(new Event(state, path, failure));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void initialStateIsPending()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();

      assertEquals(ConversionState.PENDING, session.state());
      assertNull  (session.convertedPath());
      assertNull  (session.failure());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void happyPathTransitionsPendingConvertingCompleted()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();
      DisplayRecorder recorder = new DisplayRecorder();

      session.subscribeDisplay(null, recorder);
      assertEquals(1, recorder.events.size());
      assertEquals(ConversionState.PENDING, recorder.events.getFirst().state());

      session.markConverting();
      assertEquals(ConversionState.CONVERTING, session.state());
      assertEquals(2, recorder.events.size());
      assertEquals(ConversionState.CONVERTING, recorder.events.get(1).state());

      FilePath converted = FilePath.of("converted.pdf");
      session.complete(converted);
      assertEquals(ConversionState.COMPLETED, session.state());
      assertEquals(converted, session.convertedPath());
      assertEquals(3, recorder.events.size());
      assertEquals(ConversionState.COMPLETED, recorder.events.get(2).state());
      assertEquals(converted, recorder.events.get(2).path());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void failTerminatesAndDeliversFailure()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session  = newSession();
      DisplayRecorder recorder = new DisplayRecorder();
      session.subscribeDisplay(null, recorder);

      RuntimeException cause = new RuntimeException("conversion broke");
      session.fail(cause);

      assertEquals(ConversionState.FAILED, session.state());
      assertSame(cause, session.failure());

      DisplayRecorder.Event last = recorder.events.getLast();
      assertEquals(ConversionState.FAILED, last.state());
      assertSame (cause, last.failure());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void cancelTerminatesWithCancellationException()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();

      session.cancel();

      assertEquals(ConversionState.CANCELLED, session.state());
      assertInstanceOf(CancellationException.class, session.failure());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void transitionsFromTerminalAreNoOp()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();

      FilePath converted = FilePath.of("converted.pdf");
      session.complete(converted);

      // Subsequent transitions should not change state or clobber the path.

      session.fail(new RuntimeException("late failure"));
      session.cancel();
      session.markConverting();
      session.complete(FilePath.of("different.pdf"));

      assertEquals(ConversionState.COMPLETED, session.state());
      assertEquals(converted, session.convertedPath());
      assertNull(session.failure());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void extractionFutureCompletesOnSessionComplete() throws Exception
  {
    ConversionSession session = newSession();
    CompletableFuture<FilePath> future = session.subscribeExtraction();

    assertFalse(future.isDone());

    FilePath converted = FilePath.of("converted.pdf");
    FxTestUtil.runFxAndWait(() -> session.complete(converted));

    assertTrue(future.isDone());
    assertEquals(converted, future.get());
  }

//---------------------------------------------------------------------------

  @Test
  void extractionFutureFailsOnSessionFail()
  {
    ConversionSession session = newSession();
    CompletableFuture<FilePath> future = session.subscribeExtraction();

    RuntimeException cause = new RuntimeException("broke");
    FxTestUtil.runFxAndWait(() -> session.fail(cause));

    ExecutionException ee = assertThrows(ExecutionException.class, future::get);
    assertSame(cause, ee.getCause());
  }

//---------------------------------------------------------------------------

  @Test
  void extractionFutureFailsOnSessionCancel()
  {
    ConversionSession session = newSession();
    CompletableFuture<FilePath> future = session.subscribeExtraction();

    FxTestUtil.runFxAndWait(session::cancel);

    // For CancellationException specifically, CompletableFuture.get() re-throws
    // it directly rather than wrapping in ExecutionException (JDK behavior).

    assertThrows(CancellationException.class, future::get);
  }

//---------------------------------------------------------------------------

  @Test
  void lateExtractionSubscriberGetsCachedResult() throws Exception
  {
    ConversionSession session = newSession();

    FilePath converted = FilePath.of("converted.pdf");
    FxTestUtil.runFxAndWait(() -> session.complete(converted));

    CompletableFuture<FilePath> late = session.subscribeExtraction();

    assertTrue(late.isDone());
    assertEquals(converted, late.get());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void subscribingSecondDisplaySilentlyDisplacesFirst()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();

      DisplayRecorder a = new DisplayRecorder();
      DisplayRecorder b = new DisplayRecorder();

      session.subscribeDisplay(null, a);
      session.subscribeDisplay(null, b);

      // A should have seen just PENDING (its subscribe-time snapshot).
      // Displacement is silent: A receives no further notifications.
      // This prevents spurious setUnable calls when a consumer re-subscribes
      // under the same key, e.g., a pane re-previewing the same document.

      assertEquals(1, a.events.size());
      assertEquals(ConversionState.PENDING, a.events.getFirst().state());

      // B should have seen just PENDING (the session is still pending).

      assertEquals(1, b.events.size());
      assertEquals(ConversionState.PENDING, b.events.getFirst().state());

      // Now transition the session: only B (the active subscriber) hears it.

      session.complete(FilePath.of("converted.pdf"));

      assertEquals(1, a.events.size());
      assertEquals(2, b.events.size());
      assertEquals(ConversionState.COMPLETED, b.events.get(1).state());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void lastSubscriberUnsubscribeCancelsSession()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();
      Subscription sub = session.subscribeDisplay(null, new DisplayRecorder());

      assertEquals(ConversionState.PENDING, session.state());

      sub.unsubscribe();

      assertEquals(ConversionState.CANCELLED, session.state());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void unsubscribingOneOfManyLeavesSessionAlive()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();
      CompletableFuture<FilePath> extractionFuture = session.subscribeExtraction();
      Subscription displaySub = session.subscribeDisplay(null, new DisplayRecorder());

      displaySub.unsubscribe();

      // Extraction subscriber still present -> session must stay non-terminal.

      assertEquals(ConversionState.PENDING, session.state());
      assertFalse(extractionFuture.isDone());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void onAbandonedFiresWhenLastSubscriberLeaves()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      AtomicInteger abandonedCount = new AtomicInteger();

      ConversionSession session = new ConversionSession(FilePath.of("test.docx"),
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        _ -> abandonedCount.incrementAndGet());

      Subscription sub = session.subscribeDisplay(null, new DisplayRecorder());

      assertEquals(0, abandonedCount.get());

      sub.unsubscribe();

      assertEquals(1, abandonedCount.get());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void extractionFutureCancelUnsubscribes()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();
      CompletableFuture<FilePath> future = session.subscribeExtraction();

      future.cancel(true);

      // Last subscriber gone -> session cancelled.

      assertEquals(ConversionState.CANCELLED, session.state());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void unsubscribeIsIdempotent()
  {
    FxTestUtil.runFxAndWait(() ->
    {
      ConversionSession session = newSession();
      Subscription sub = session.subscribeDisplay(null, new DisplayRecorder());

      sub.unsubscribe();
      sub.unsubscribe();  // must not throw or double-fire abandonment

      assertEquals(ConversionState.CANCELLED, session.state());
    });
  }

//---------------------------------------------------------------------------

  @Test
  void subscribingExtractionAfterCompleteReturnsDoneFuture() throws Exception
  {
    // Covers the "user clicks same FTS row twice" flow: session is already
    // COMPLETED, a new extraction subscription should get the cached path
    // without reconversion.

    ConversionSession session = newSession();

    CompletableFuture<FilePath> firstExtraction = session.subscribeExtraction();

    FilePath converted = FilePath.of("converted.pdf");
    FxTestUtil.runFxAndWait(() -> session.complete(converted));

    assertEquals(converted, firstExtraction.get());

    CompletableFuture<FilePath> secondExtraction = session.subscribeExtraction();

    assertTrue(secondExtraction.isDone());
    assertEquals(converted, secondExtraction.get());
  }

//---------------------------------------------------------------------------

  @Test
  void convertToHtmlDetectionMatchesOfficePreviewer()
  {
    assertTrue (ConversionSession.determineConvertToHtml("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"));
    assertTrue (ConversionSession.determineConvertToHtml("application/vnd.ms-excel"));
    assertTrue (ConversionSession.determineConvertToHtml("text/csv"));
    assertTrue (ConversionSession.determineConvertToHtml("TEXT/CSV"));
    assertTrue (ConversionSession.determineConvertToHtml("text/tab-separated-values"));
    assertTrue (ConversionSession.determineConvertToHtml("application/vnd.oasis.opendocument.spreadsheet"));
    assertTrue (ConversionSession.determineConvertToHtml("application/vnd.sun.xml.calc"));

    assertFalse(ConversionSession.determineConvertToHtml("application/vnd.openxmlformats-officedocument.wordprocessingml.document"));
    assertFalse(ConversionSession.determineConvertToHtml("application/msword"));
    assertFalse(ConversionSession.determineConvertToHtml("application/rtf"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
