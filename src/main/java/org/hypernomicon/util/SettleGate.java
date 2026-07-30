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

package org.hypernomicon.util;

import javafx.animation.PauseTransition;
import javafx.util.Duration;

//---------------------------------------------------------------------------

/**
 * Coalesces rapid repeated requests into the one the requests settle on. A
 * request made after a quiet period runs immediately, so deliberate single
 * actions stay instant; during a burst (key-repeat selection, e.g.), only the
 * latest request runs, once the quiet interval has passed since the most
 * recent one. Used to keep rapid selection from starting expensive work
 * (loads, conversions) for items it merely passes over.
 * <p>
 * FX-thread confined: requests, the timer callback, and the deferred action
 * all run on the JavaFX Application Thread.
 */
public final class SettleGate
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final long quietMillis;
  private final PauseTransition timer;

  private long lastRequestNanos = 0;
  private Runnable pending = null;

//---------------------------------------------------------------------------

  public SettleGate(long quietMillis)
  {
    this.quietMillis = quietMillis;

    timer = new PauseTransition(Duration.millis(quietMillis));
    timer.setOnFinished(event -> timerFinished());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Runs {@code action} now if requests have been quiet and nothing is
   *  pending; otherwise stores it (replacing any stored action; latest wins)
   *  to run once the burst settles. */
  public void request(Runnable action)
  {
    long now = System.nanoTime();
    boolean quiet = ((now - lastRequestNanos) / 1_000_000L) >= quietMillis;

    lastRequestNanos = now;

    if (quiet && (pending == null))
    {
      action.run();
      return;
    }

    pending = action;
    timer.playFromStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Discards any stored action and stops the timer. */
  public void cancel()
  {
    timer.stop();
    pending = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void timerFinished()
  {
    if (pending == null) return;

    // Re-check the wall clock: on a busy FX thread the timer can fire while
    // requests are still queued, i.e. late in wall-clock terms but early
    // relative to the last processed request. Rearm in that case.

    if (((System.nanoTime() - lastRequestNanos) / 1_000_000L) < quietMillis)
    {
      timer.playFromStart();
      return;
    }

    Runnable action = pending;
    pending = null;

    action.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
