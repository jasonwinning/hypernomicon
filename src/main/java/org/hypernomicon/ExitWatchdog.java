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

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

import org.hypernomicon.HyperTask.HyperThread;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * Guarantees process termination after Hypernomicon's own teardown has completed.
 * <p>
 * <b>Why this exists:</b> originally, JxBrowser 6's shared-memory IPC transport could leave
 * non-daemon threads permanently wedged after browser disposal, keeping the JVM alive forever
 * after a completely clean shutdown. The JxBrowser 9 upgrade removed that failure mode, but the
 * watchdog is kept as cheap insurance: any library (or our own code) can leak a non-daemon thread,
 * and a desktop application that sometimes fails to exit is a support burden out of proportion to
 * this class's cost.
 * <p>
 * It is armed as the <b>last</b> step of the shutdown sequence (after the database is closed, the
 * browser engine is shut down, and the main window is closed), and then gives remaining threads a
 * grace period to exit on their own.
 * <p>
 * If the JVM exits organically during the grace period (the normal case), the watchdog is a daemon
 * thread and simply dies with it, having done nothing. If the JVM is still alive when the grace
 * period ends, the watchdog logs every surviving non-daemon thread with its stack and only then
 * forces exit. The log entry is the contract: a forced exit must never hide what it absorbed, so
 * the organic-exit invariant remains verifiable in myLogFile.log even though the process no longer
 * hangs. {@code System.exit(0)} is used first so shutdown hooks run (java.util.prefs flushes via a
 * shutdown hook on Linux; deleteOnExit temp files are cleaned); a {@code Runtime.halt(0)} backstop
 * covers the case where a shutdown hook itself hangs.
 */
public final class ExitWatchdog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ExitWatchdog() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

  /** Minimum time threads get to exit on their own after teardown completes. */
  private static final int GRACE_PERIOD_MS = 8000;

  /** Additional time allowed for System.exit's shutdown hooks before the halt backstop fires. */
  private static final int HALT_BACKSTOP_MS = 5000;

  private static final AtomicBoolean armed = new AtomicBoolean();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Starts the watchdog. Call only when Hypernomicon's teardown is fully complete; the grace
   * period starts now. Idempotent: only the first call arms it.
   */
  public static void arm()
  {
    if (armed.getAndSet(true)) return;

    HyperThread watchdog = new HyperThread("ExitWatchdog", ExitWatchdog::watch);
    watchdog.setDaemon(true);  // On an organic exit during the grace period, this thread must not keep the JVM alive
    watchdog.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void watch()
  {
    sleepForMillis(GRACE_PERIOD_MS);

    // Only reached if the JVM is still alive after the grace period: at least one non-daemon
    // thread failed to exit despite complete teardown.

    logSurvivors();

    // System.exit runs shutdown hooks (prefs flush, deleteOnExit temp cleanup); nuclearOption is
    // the backstop that halts the JVM outright if a shutdown hook itself hangs.

    nuclearOption(HALT_BACKSTOP_MS);

    System.out.println("Shutdown: forcing process exit " + (GRACE_PERIOD_MS / 1000) + "s after teardown completed.");

    System.exit(0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Logs the non-daemon threads still alive, so the forced exit never hides what it absorbed. */
  private static void logSurvivors()
  {
    List<Map.Entry<Thread, StackTraceElement[]>> survivors = Thread.getAllStackTraces().entrySet().stream()
      .filter(entry -> entry.getKey().isAlive() && (entry.getKey().isDaemon() == false))
      .filter(entry -> entry.getKey() != Thread.currentThread())
      .filter(entry -> "DestroyJavaVM".equals(entry.getKey().getName()) == false)
      .toList();

    if (survivors.isEmpty()) return;

    System.out.println("Shutdown: " + survivors.size() + " non-daemon thread(s) still running "
      + (GRACE_PERIOD_MS / 1000) + "s after teardown completed:");

    for (Map.Entry<Thread, StackTraceElement[]> entry : survivors)
    {
      Thread survivor = entry.getKey();

      System.out.println("  \"" + survivor.getName() + "\" (" + survivor.getState() + "): possible resource leak; investigate");

      Arrays.stream(entry.getValue()).limit(10).forEach(frame -> System.out.println("      at " + frame));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
