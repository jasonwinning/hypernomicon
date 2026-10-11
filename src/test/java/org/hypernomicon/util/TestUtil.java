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

package org.hypernomicon.util;

import static org.junit.jupiter.api.Assertions.*;

import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

//---------------------------------------------------------------------------

/**
 * Helpers shared by unit tests that are not specific to JavaFX (for those, see {@link FxTestUtil}).
 */
public final class TestUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final long DEFAULT_AWAIT_TIMEOUT_MS = 15_000,
                            AWAIT_POLL_INTERVAL_MS   = 25;

//---------------------------------------------------------------------------

  private TestUtil() { }

//---------------------------------------------------------------------------

  /** {@link #awaitTrue(Callable, long, String)} with a timeout of 15 seconds. */
  public static void awaitTrue(Callable<Boolean> condition, String message) throws Exception { awaitTrue(condition, DEFAULT_AWAIT_TIMEOUT_MS, message); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Waits for a condition that becomes true asynchronously, checking it every 25 milliseconds.
   * This is for work that offers nothing to block on; when a latch, future or callback exists,
   * wait on that instead.
   *
   * @param condition checked until it returns true
   * @param timeoutMS how long to wait before failing the test
   * @param message the failure message if the condition is still false when the time is up
   */
  public static void awaitTrue(Callable<Boolean> condition, long timeoutMS, String message) throws Exception
  {
    long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMS);

    while (condition.call() == false)
    {
      if ((System.nanoTime() - deadline) > 0)
        fail(message);

      Thread.sleep(AWAIT_POLL_INTERVAL_MS);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
