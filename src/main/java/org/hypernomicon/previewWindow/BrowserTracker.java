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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import com.teamdev.jxbrowser.chromium.Browser;

import org.hypernomicon.App;

//---------------------------------------------------------------------------

/**
 * Records where each live JxBrowser {@link Browser} instance was created, so the shutdown
 * straggler sweep in {@link PreviewWindow} can name any instance that was genuinely never disposed.
 * A never-disposed Browser leaks its native channel and non-daemon IPC threads, so it is worth
 * catching, but that is a secondary concern: the primary exit hang comes from channel threads that
 * stay wedged after a Browser has already disposed (see {@link org.hypernomicon.ExitWatchdog}),
 * which the sweep cannot see and which only the watchdog resolves.
 */
public final class BrowserTracker
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BrowserTracker() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

  private static final Map<Browser, String> browserToOrigin = new ConcurrentHashMap<>();

  private static final String appPackagePrefix = App.class.getPackageName() + '.';

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Registers a newly created Browser along with a summary of the call chain that
   * created it. The entry is removed automatically when the Browser is disposed,
   * so anything still registered at shutdown was leaked.
   *
   * @param browser the Browser that was just created
   * @param creatorLabel short label for the creating class, e.g. "PDFJSWrapper"
   */
  public static void register(Browser browser, String creatorLabel)
  {
    browserToOrigin.put(browser, creatorLabel + " created via: " + callChain());

    browser.addDisposeListener(event -> browserToOrigin.remove(browser));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Where the given Browser was created, if it was registered. */
  static String describe(Browser browser)
  {
    return browserToOrigin.getOrDefault(browser, "(browser of unknown origin)");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Compact summary of the application frames on the current stack, innermost first. */
  private static String callChain()
  {
    return StackWalker.getInstance().walk(frames -> frames
      .map(StackWalker.StackFrame::toStackTraceElement)
      .filter(frame -> frame.getClassName().startsWith(appPackagePrefix))
      .filter(frame -> frame.getClassName().equals(BrowserTracker.class.getName()) == false)
      .limit(6)
      .map(frame -> frame.getClassName().substring(frame.getClassName().lastIndexOf('.') + 1) + '.' + frame.getMethodName() + ':' + frame.getLineNumber())
      .collect(Collectors.joining(" < ")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
