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

package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.*;

import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.util.FxTestUtil;
import org.hypernomicon.util.PopupRobot;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.function.Executable;

import javafx.application.Platform;
import javafx.concurrent.Worker.State;
import javafx.scene.control.Alert.AlertType;

//---------------------------------------------------------------------------

/**
 * A task that posts work to the FX thread can wait for that thread to catch up. The wait must
 * return only once everything posted so far has run, and a cancellation made while the task is
 * waiting must end the task without the FX thread's help.
 * <p>
 * A task that fails must tell the user, whatever it died of: a data error carries its own message,
 * and anything else is reported as an unexpected error and logged as well, unless the task was made
 * silent.
 */
class HyperTaskTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final long TIMEOUT_SECS = 10;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    FxTestUtil.initJfx();
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp()
  {
    PopupRobot.setActive(true);
    PopupRobot.clear();
  }

//---------------------------------------------------------------------------

  @AfterEach
  void tearDown()
  {
    PopupRobot.setActive(false);
    PopupRobot.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void awaitQuietly(CountDownLatch latch)
  {
    try
    {
      latch.await(TIMEOUT_SECS, TimeUnit.SECONDS);
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
    }
  }

//---------------------------------------------------------------------------

  private static void assertReached(CountDownLatch latch, String what) throws InterruptedException
  {
    assertTrue(latch.await(TIMEOUT_SECS, TimeUnit.SECONDS), what);
  }

//---------------------------------------------------------------------------

  /**
   * Starts the task on its own thread and returns its final state once its done handlers have run
   * and the FX thread has run everything the task's completion posted to it.
   */
  private static State runToCompletion(HyperTask task) throws InterruptedException
  {
    CountDownLatch done = new CountDownLatch(1);
    AtomicReference<State> finalState = new AtomicReference<>();

    task.addDoneHandler(state ->
    {
      finalState.set(state);
      done.countDown();
    });

    task.startWithNewThread(true);

    assertReached(done, "The task did not finish");

    // The done handlers run before a failed task posts its popup to the FX thread, so a wait posted
    // while they run can return before the popup has been recorded; the second wait is for the popup.

    FxTestUtil.runFxAndWait(() -> {});
    FxTestUtil.runFxAndWait(() -> {});

    return finalState.get();
  }

//---------------------------------------------------------------------------

  /**
   * Runs the action with standard error captured, and returns what was written to it.
   */
  private static String stdErrWhile(Executable action) throws Throwable
  {
    PrintStream origErr = System.err;
    ByteArrayOutputStream captured = new ByteArrayOutputStream();

    System.setErr(new PrintStream(captured, true, StandardCharsets.UTF_8));

    try
    {
      action.execute();
    }
    finally
    {
      System.setErr(origErr);
    }

    return captured.toString(StandardCharsets.UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void theWaitReturnsOnceEverythingPostedSoFarHasRun() throws Exception
  {
    AtomicInteger ran = new AtomicInteger(), ranWhenTheWaitReturned = new AtomicInteger(-1);

    HyperTask task = new HyperTask("WaitTest", "", 1) { @Override protected void call() throws CancelledTaskException
    {
      for (int ndx = 0; ndx < 500; ndx++)
        Platform.runLater(ran::incrementAndGet);

      waitForFXThread();

      ranWhenTheWaitReturned.set(ran.get());
    }};

    assertEquals(State.SUCCEEDED, runToCompletion(task));
    assertEquals(500, ranWhenTheWaitReturned.get(), "Everything posted before the wait must have run when it returned");
  }

//---------------------------------------------------------------------------

  @Test
  void aCancellationDuringTheWaitEndsTheTask() throws Exception
  {
    CountDownLatch fxThreadHeld    = new CountDownLatch(1),
                   releaseFXThread = new CountDownLatch(1),
                   aboutToWait     = new CountDownLatch(1),
                   done            = new CountDownLatch(1);

    AtomicBoolean raisedByTheWait = new AtomicBoolean();
    AtomicReference<State> finalState = new AtomicReference<>();

    HyperTask task = new HyperTask("CancelTest", "", 1) { @Override protected void call() throws CancelledTaskException
    {
      Platform.runLater(() ->  // Hold the FX thread so that the wait cannot return on its own
      {
        fxThreadHeld.countDown();
        awaitQuietly(releaseFXThread);
      });

      aboutToWait.countDown();

      try
      {
        waitForFXThread();
      }
      catch (CancelledTaskException e)
      {
        raisedByTheWait.set(true);
        throw e;
      }
    }};

    task.addDoneHandler(state ->
    {
      finalState.set(state);
      done.countDown();
    });

    task.startWithNewThread(true);

    try
    {
      assertReached(fxThreadHeld, "The FX thread was not held");
      assertReached(aboutToWait, "The task did not reach the wait");

      task.cancel();

      long deadline = System.currentTimeMillis() + (TIMEOUT_SECS * 1000);

      while (task.threadIsAlive() && (System.currentTimeMillis() < deadline))
        Thread.sleep(10);

      assertFalse(task.threadIsAlive(), "The task thread must end while the FX thread is still held");
      assertTrue(raisedByTheWait.get(), "The wait must raise the cancellation");
    }
    finally
    {
      releaseFXThread.countDown();
    }

    assertReached(done, "The task did not finish");
    assertEquals(State.CANCELLED, finalState.get());
  }

//---------------------------------------------------------------------------

  @Test
  void aDataErrorIsReportedWithItsOwnMessage() throws Exception
  {
    HyperTask task = new HyperTask("FailureTest", "", 1) { @Override protected void call() throws HyperDataException
    {
      throw new HyperDataException("Simulated data error");
    }};

    assertEquals(State.FAILED, runToCompletion(task));

    assertEquals(1, PopupRobot.getInvocationCount(), "The failure must be reported with one popup");
    assertEquals(AlertType.ERROR, PopupRobot.getLastType());
    assertEquals("Simulated data error", PopupRobot.getLastMessage());
  }

//---------------------------------------------------------------------------

  @Test
  void anUnexpectedExceptionIsReportedAndLogged() throws Throwable
  {
    assertUnexpectedFailureIsReportedAndLogged(new IllegalStateException("Simulated fault"));
  }

//---------------------------------------------------------------------------

  @Test
  void anErrorIsReportedAndLogged() throws Throwable
  {
    assertUnexpectedFailureIsReportedAndLogged(new OutOfMemoryError("Simulated heap exhaustion"));
  }

//---------------------------------------------------------------------------

  private static void assertUnexpectedFailureIsReportedAndLogged(Throwable failure) throws Throwable
  {
    HyperTask task = new HyperTask("FailureTest", "", 1) { @Override protected void call()
    {
      if (failure instanceof Error error) throw error;

      throw (RuntimeException) failure;
    }};

    String logged = stdErrWhile(() -> assertEquals(State.FAILED, runToCompletion(task)));

    AtomicReference<Throwable> exception = new AtomicReference<>();
    FxTestUtil.runFxAndWait(() -> exception.set(task.getException()));  // A task's exception can only be read on the FX thread

    assertSame(failure, exception.get());

    assertEquals(1, PopupRobot.getInvocationCount(), "The failure must be reported with one popup");
    assertEquals(AlertType.ERROR, PopupRobot.getLastType());
    assertTrue(PopupRobot.getLastMessage().contains(failure.getMessage()), PopupRobot.getLastMessage());

    assertTrue(logged.contains(failure.getClass().getName()), "The stack trace must still be logged");
  }

//---------------------------------------------------------------------------

  @Test
  void aSilentTaskReportsNothing() throws Exception
  {
    HyperTask task = new HyperTask("FailureTest", "", 1) { @Override protected void call()
    {
      throw new IllegalStateException("Simulated fault");
    }}.setSilent(true);

    assertEquals(State.FAILED, runToCompletion(task));

    assertEquals(0, PopupRobot.getInvocationCount(), "A silent task must not report its failure");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
