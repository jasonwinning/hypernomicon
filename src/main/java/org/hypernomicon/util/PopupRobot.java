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

import static org.hypernomicon.util.PopupDialog.DialogResult.*;

import java.util.*;

import org.hypernomicon.util.PopupDialog.DialogResult;

import javafx.scene.control.Alert.AlertType;

//---------------------------------------------------------------------------

/**
 * PopupRobot is a utility for intercepting and simulating popup dialogs
 * and progress windows. It can be used for test automation or other
 * situations where UI workflows should be run non-interactively.
 *
 * <p>When active, PopupRobot records the message and type of each popup
 * and returns a predictable {@link DialogResult} instead of showing a real
 * UI dialog. This allows integration tests to verify that popups were
 * generated with the expected content and to simulate user responses
 * deterministically.</p>
 *
 * <h3>Lifecycle</h3>
 * <ul>
 *   <li>Call {@link #setActive(boolean)} with true at the start of a test run.</li>
 *   <li>Configure the default response with {@link #setDefaultResponse(DialogResult)}.</li>
 *   <li>Run code that would normally show popups; they will be intercepted.</li>
 *   <li>Verify recorded message and type with {@link #getLastMessage()} and {@link #getLastType()}.</li>
 *   <li>Verify popup count with {@link #getInvocationCount()}.</li>
 *   <li>Deactivate with {@link #setActive(boolean)} false in a finalizer step.</li>
 * </ul>
 *
 * <h3>Response Queue</h3>
 * <p>For multi-dialog flows where successive popups need different responses,
 * use {@link #enqueueResponses(DialogResult...)} to pre-load a FIFO queue.
 * Each call to {@link #getDefaultResponse()} polls the next response from the queue.
 * When the queue is empty, the static {@code defaultResponse} is used as a fallback.</p>
 *
 * <h3>Progress Dialog Interception</h3>
 * <p>{@link org.hypernomicon.dialogs.ProgressDlgCtrlr ProgressDlgCtrlr} checks
 * PopupRobot before showing its modal progress window. Call
 * {@link #setCancelNextProgressDialog(boolean)} with {@code true} to simulate
 * clicking Cancel on the next progress dialog. The flag auto-resets after
 * consumption by {@link #shouldCancelProgressDialog()}, so subsequent progress
 * dialogs proceed normally.</p>
 *
 * <h3>Pre-Response Actions</h3>
 * <p>Use {@link #setPreResponseAction(Runnable)} to register a one-shot action
 * that fires synchronously inside {@link #getDefaultResponse()} just before the
 * response is returned. The action auto-clears after firing.</p>
 *
 * <p>Use {@link #setPreResponseAction(int, Runnable)} with a {@code skipCount}
 * to defer the action past that many {@code getDefaultResponse()} calls.</p>
 *
 * <h3>Resetting State</h3>
 * <p>{@link #clear()} resets all mutable state: recorded message and type,
 * invocation count, progress cancel flag, response queue, and pre-response action.</p>
 *
 * <h3>Thread Safety</h3>
 * <p>All methods are synchronized to ensure consistent state across the FX
 * thread and test runner threads.</p>
 *
 * <h3>Usage Examples</h3>
 * <pre>
 * // Single response for all dialogs
 * PopupRobot.setActive(true);
 * PopupRobot.setDefaultResponse(DialogResult.mrYes);
 *
 * DialogResult result = UIUtil.yesNoCancelDialog("Proceed?");
 *
 * assert "Proceed?".equals(PopupRobot.getLastMessage());
 * assert PopupRobot.getLastType() == Alert.AlertType.CONFIRMATION;
 * assert result == DialogResult.mrYes;
 *
 * PopupRobot.setActive(false);
 * </pre>
 * <pre>
 * // Scripted responses for successive dialogs
 * PopupRobot.setActive(true);
 * PopupRobot.enqueueResponses(DialogResult.mrRetry, DialogResult.mrCancel);
 * // First dialog returns mrRetry, second returns mrCancel
 *
 * PopupRobot.setActive(false);
 * </pre>
 * <pre>
 * // Cancel the next progress dialog
 * PopupRobot.setActive(true);
 * PopupRobot.setCancelNextProgressDialog(true);
 *
 * // The progress dialog will be cancelled automatically;
 * // subsequent progress dialogs are unaffected.
 *
 * PopupRobot.setActive(false);
 * </pre>
 */
public final class PopupRobot
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean active = false, cancelNextProgressDialog = false;
  private static String lastMessage;
  private static AlertType lastType;
  private static DialogResult defaultResponse = mrOk;
  private static int invocationCount = 0;

  private static final Deque<DialogResult> responseQueue = new ArrayDeque<>();

  private static Runnable preResponseAction;
  private static int preResponseSkipCount;

//---------------------------------------------------------------------------

  private PopupRobot() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  public static synchronized void setActive(boolean value)                    { active = value; }
  public static synchronized boolean isActive()                               { return active; }
  public static synchronized String getLastMessage()                          { return lastMessage; }
  public static synchronized AlertType getLastType()                          { return lastType; }
  public static synchronized int getInvocationCount()                         { return invocationCount; }
  public static synchronized void setDefaultResponse(DialogResult response)   { defaultResponse = response; }
  public static synchronized void enqueueResponses(DialogResult... responses) { Collections.addAll(responseQueue, responses); }

  /**
   * Register a one-shot action that fires synchronously inside
   * {@link #getDefaultResponse()} just before the response is returned.
   * The action fires on the very next {@code getDefaultResponse()} call
   * and auto-clears afterward (subsequent calls return the response without
   * running any action). Example use: releasing a file lock between dialog
   * prompts so the next retry succeeds.
   *
   * @param action the action to run; must not block the FX thread for long
   */
  public static synchronized void setPreResponseAction(Runnable action)
  {
    preResponseAction = action;
    preResponseSkipCount = 0;
  }

  /**
   * Register a one-shot action that fires synchronously inside
   * {@link #getDefaultResponse()}, but only after {@code skipCount}
   * invocations of {@code getDefaultResponse()} have passed. Each
   * skipped invocation decrements the counter without running the action.
   * Once the counter reaches zero, the action fires and auto-clears.
   * <p>
   * This is useful when multiple dialogs appear in sequence and the
   * action (e.g. releasing a file lock) should fire at a specific point.
   * For example, {@code setPreResponseAction(1, () -> releaseLock())}
   * skips the first {@code getDefaultResponse()} call (e.g. an overwrite
   * dialog) and fires on the second (e.g. a lock retry dialog).
   *
   * @param skipCount number of {@code getDefaultResponse()} calls to skip
   *                  before firing; 0 fires on the very next call
   * @param action    the action to run; must not block the FX thread for long
   */
  public static synchronized void setPreResponseAction(int skipCount, Runnable action)
  {
    preResponseAction = action;
    preResponseSkipCount = skipCount;
  }

//---------------------------------------------------------------------------

  static synchronized DialogResult getDefaultResponse()
  {
    if (preResponseAction != null)
    {
      if (preResponseSkipCount > 0)
        preResponseSkipCount--;
      else
      {
        Runnable action = preResponseAction;
        preResponseAction = null;
        action.run();
      }
    }

    return responseQueue.isEmpty() ? defaultResponse : responseQueue.poll();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static synchronized void setCancelNextProgressDialog(boolean value) { cancelNextProgressDialog = value; }

  public static synchronized boolean shouldCancelProgressDialog()
  {
    boolean result = cancelNextProgressDialog;
    cancelNextProgressDialog = false;
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static synchronized void record(String msg, AlertType type)
  {
    System.out.println(type.toString() + ": " + msg);

    lastMessage = msg;
    lastType = type;
    invocationCount++;
  }


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static synchronized void clear()
  {
    lastMessage = null;
    lastType = null;
    invocationCount = 0;
    cancelNextProgressDialog = false;
    responseQueue.clear();
    preResponseAction = null;
    preResponseSkipCount = 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
