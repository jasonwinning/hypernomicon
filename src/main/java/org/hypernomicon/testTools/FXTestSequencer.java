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

package org.hypernomicon.testTools;

import java.util.*;

import org.hypernomicon.util.PopupRobot;

import javafx.scene.control.Alert.AlertType;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * FXTestSequencer is a lightweight scheduler for sequencing tasks on the JavaFX
 * Application Thread with either pulse-based or millisecond delays.
 *
 * <p>Each task is a {@code Runnable}. If the task throws an {@code AssertionError}
 * or other exception, the queue halts immediately. Otherwise, the queue continues
 * until all tasks have run.</p>
 *
 * <h3>Lifecycle</h3>
 * <ul>
 *   <li>Tasks may be enqueued with either pulse-based or millisecond delays.</li>
 *   <li>Delays must be configured via {@link #setPulseDelay(int)} or {@link #setDelayMS(long)}
 *       before using delay-based methods; if unset, the sequence aborts.</li>
 *   <li>Progress output prints "Step N of M" for each enqueued task.</li>
 *   <li>An optional finalizer may be registered with {@link #setFinalizer(Runnable)}.
 *       This runnable always executes once when the sequence ends, regardless of
 *       success or failure.</li>
 *   <li>If any task throws an {@code AssertionError} or other exception, the error
 *       message is printed to {@code System.err}, the sequence halts, and the finalizer
 *       still runs.</li>
 * </ul>
 *
 * <h3>Thread Safety</h3>
 * <p>Queue operations are synchronized to ensure consistent state when accessed
 * from test runner threads and the FX thread.</p>
 */
public final class FXTestSequencer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record QueueEntry(int pulses, long millis, Runnable task, boolean printStep) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Queue<QueueEntry> queue = new ArrayDeque<>();

  private volatile boolean running = false;
  private volatile Runnable finalizer;

  private int stepCounter = 0, delayPulses = -1;
  private long stepTotal, delayMS = -1;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets a finalizer runnable to be executed once when the sequence ends,
   * regardless of success or failure.
   *
   * @param finalizer the runnable to execute at the end of the sequence
   */
  public void setFinalizer(Runnable finalizer)
  {
    this.finalizer = finalizer;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Configures the default delay in milliseconds to be used by
   * subsequent delay-based tasks.
   *
   * @param delayMS delay in milliseconds
   * @return this sequencer for chaining
   */
  public FXTestSequencer setDelayMS(long delayMS)
  {
    this.delayMS = delayMS;
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Configures the default delay in JavaFX pulses to be used by
   * subsequent pulse-based tasks.
   *
   * @param pulses number of pulses to delay
   * @return this sequencer for chaining
   */
  public FXTestSequencer setPulseDelay(int pulses)
  {
    this.delayPulses = pulses;
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Enqueues a task to run immediately (without delay).
   *
   * @param task the runnable task to execute
   * @return this sequencer for chaining
   */
  public FXTestSequencer thenRun(Runnable task)
  {
    enqueue(new QueueEntry(0, -1, task, true));
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Enqueues a task to run after the currently configured pulse delay.
   *
   * @param task the runnable task to execute
   * @return this sequencer for chaining
   * @throws IllegalStateException if no pulse delay has been configured
   */
  public FXTestSequencer thenRunAfterPulses(Runnable task)
  {
    if (delayPulses < 0) throw new IllegalStateException("Pulse delay not set");

    final int finalPulses = delayPulses;

    enqueue(new QueueEntry(finalPulses, -1, task, true));
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Enqueues a task to run after the currently configured millisecond delay.
   *
   * @param task the runnable task to execute
   * @return this sequencer for chaining
   * @throws IllegalStateException if no millisecond delay has been configured
   */
  public FXTestSequencer thenRunAfterDelay(Runnable task)
  {
    if (delayMS < 0) throw new IllegalStateException("Delay MS not set");

    long finalDelay = delayMS;

    enqueue(new QueueEntry(-1, finalDelay, task, true));
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Enqueues a verification step that asserts a popup was generated
   * with the expected message and type, after the default number of pulses.
   *
   * @param expectedMsg expected popup message
   * @param expectedType expected alert type
   * @return this queue for chaining
   * @throws AssertionError if PopupRobot is not active or the expectation fails
   */
  public FXTestSequencer thenExpectPopupAfterPulses(String expectedMsg, AlertType expectedType)
  {
    final int finalPulses = delayPulses;

    enqueue(new QueueEntry(finalPulses, -1, () ->
    {
      if (PopupRobot.isActive() == false)
        throw new AssertionError("PopupRobot not active; cannot verify popup expectations");

      String actualMsg = PopupRobot.getLastMessage();
      AlertType actualType = PopupRobot.getLastType();

      if ((expectedMsg.equals(actualMsg) == false) || (expectedType != actualType))
        throw new AssertionError("Popup expectation failed: expected ["
          + expectedType + " : " + expectedMsg + "] but got ["
          + actualType + " : " + actualMsg + ']');

      PopupRobot.clear();
    }, false));

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Enqueues a verification step that asserts a popup was generated
   * with the expected message and type, after the default delay in milliseconds.
   *
   * @param expectedMsg expected popup message
   * @param expectedType expected alert type
   * @return this queue for chaining
   * @throws AssertionError if PopupRobot is not active or the expectation fails
   */
  public FXTestSequencer thenExpectPopupAfterDelay(String expectedMsg, AlertType expectedType)
  {
    long finalDelay = delayMS;

    enqueue(new QueueEntry(-1, finalDelay, () ->
    {
      if (PopupRobot.isActive() == false)
        throw new AssertionError("PopupRobot not active; cannot verify popup expectations");

      String actualMsg = PopupRobot.getLastMessage();
      AlertType actualType = PopupRobot.getLastType();

      if ((expectedMsg.equals(actualMsg) == false) || (expectedType != actualType))
        throw new AssertionError("Popup expectation failed: expected ["
          + expectedType + " : " + expectedMsg + "] but got ["
          + actualType + " : " + actualMsg + ']');

      PopupRobot.clear();
    }, false));

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Enqueues a verification step that asserts a popup was not generated
   * after the default delay in milliseconds.
   *
   * @return this queue for chaining
   * @throws AssertionError if PopupRobot is not active or the expectation fails
   */
  public FXTestSequencer thenExpectLackOfPopupAfterDelay()
  {
    long finalDelay = delayMS;

    enqueue(new QueueEntry(-1, finalDelay, () ->
    {
      if (PopupRobot.isActive() == false)
        throw new AssertionError("PopupRobot not active; cannot verify popup expectations");

      String actualMsg = PopupRobot.getLastMessage();
      AlertType actualType = PopupRobot.getLastType();

      if ((actualMsg != null) || (actualType != null))
        throw new AssertionError("Popup expectation failed: expected [null : null] but got ["
          + actualType + " : " + actualMsg + ']');

    }, false));

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private synchronized void enqueue(QueueEntry entry)
  {
    queue.add(entry);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Starts execution of the queued tasks if not already running.
   * Prints progress messages for each step and halts on any
   * {@code AssertionError} or exception.
   */
  public void start()
  {
    if (running == false)
    {
      running = true;

      stepTotal = queue.stream().filter(QueueEntry::printStep).count();

      scheduleNext();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private synchronized void scheduleNext()
  {
    QueueEntry entry = queue.poll();

    if (entry == null)
    {
      running = false;
      System.out.println("Test sequence completed successfully.");
      runFinalizer();
      return;
    }

    Runnable runner = () ->
    {
      if (entry.printStep)
        System.out.println("Step " + ++stepCounter + " of " + stepTotal);

      try
      {
        entry.task.run();
        scheduleNext();
      }
      catch (AssertionError ae)
      {
        System.err.println(ae.getMessage());
        System.err.println("One or more test steps failed.");
        running = false;
        runFinalizer();
      }
      catch (Exception e)
      {
        e.printStackTrace();
        running = false;
        runFinalizer();
      }
    };

    if (entry.millis > 0)
      runDelayedInFXThread(1, entry.millis, runner);
    else if (entry.pulses >= 0)
      runInFXThreadAfterPulses(entry.pulses, runner);
    else
    {
      System.err.println("No delay (milliseconds or pulses) was set.");
      running = false;
      runFinalizer();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void runFinalizer()
  {
    if (finalizer != null)
    {
      runInFXThread(() ->
      {
        try
        {
          finalizer.run();
        }
        catch (Exception e)
        {
          e.printStackTrace();
          System.err.println("Finalizer threw an exception.");
        }
      }, true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
