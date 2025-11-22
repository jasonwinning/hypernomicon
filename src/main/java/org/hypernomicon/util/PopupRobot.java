/*
 * Copyright 2015-2025 Jason Winning
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

import org.hypernomicon.util.PopupDialog.DialogResult;

import javafx.scene.control.Alert.AlertType;

//---------------------------------------------------------------------------

/**
 * PopupRobot is a utility for intercepting and simulating popup dialogs.
 * It can be used for test automation or other situations where UI workflows
 * should be run non-interactively.
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
 *   <li>Deactivate with {@link #setActive(boolean)} false in a finalizer step.</li>
 * </ul>
 *
 * <h3>Thread Safety</h3>
 * All methods are synchronized to ensure consistent state across the FX thread
 * and test runner threads.
 *
 * <h3>Usage Example</h3>
 * <pre>
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
 */
public final class PopupRobot
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean active = false;
  private static String lastMessage;
  private static AlertType lastType;
  private static DialogResult defaultResponse = mrOk;

//---------------------------------------------------------------------------

  private PopupRobot() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  public static synchronized void setActive(boolean value)                  { active = value; }
  public static synchronized boolean isActive()                             { return active; }
  public static synchronized String getLastMessage()                        { return lastMessage; }
  public static synchronized AlertType getLastType()                        { return lastType; }
  public static synchronized void setDefaultResponse(DialogResult response) { defaultResponse = response; }
  static synchronized DialogResult getDefaultResponse()                     { return defaultResponse; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static synchronized void record(String msg, AlertType type)
  {
    System.out.println(type.toString() + ": " + msg);

    lastMessage = msg;
    lastType = type;
  }


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static synchronized void clear()
  {
    lastMessage = null;
    lastType = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
