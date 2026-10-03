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

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.*;

import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.util.FxTestUtil;

import org.junit.jupiter.api.*;

import javafx.application.Platform;
import javafx.concurrent.Worker.State;

//---------------------------------------------------------------------------

/**
 * A task that posts work to the FX thread can wait for that thread to catch up. The wait must
 * return only once everything posted so far has run, and a cancellation made while the task is
 * waiting must end the task without the FX thread's help.
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

    CountDownLatch done = new CountDownLatch(1);
    AtomicReference<State> finalState = new AtomicReference<>();

    task.addDoneHandler(state ->
    {
      finalState.set(state);
      done.countDown();
    });

    task.startWithNewThread(true);

    assertReached(done, "The task did not finish");
    assertEquals(State.SUCCEEDED, finalState.get());
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
//---------------------------------------------------------------------------

}
