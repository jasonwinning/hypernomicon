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

import static org.hypernomicon.util.DesktopUtil.*;

import java.io.PrintStream;
import java.nio.charset.StandardCharsets;

//---------------------------------------------------------------------------

public final class Main
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Main() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------

  static void main(String[] args)
  {
    System.setErr(new PrintStream(System.out)
    {
      @Override public void write(byte[] buf, int off, int len)
      {
        // This is a cleaner solution than an error-prone practice of always having to include JVM module-path arguments
        // pointing to exact file location of JavaFX modules, or simply setting the classpath as the module path (which
        // causes errors). As far as I can tell, no harm is actually done by loading the JavaFX classes from the classpath.

        if (! (new String(buf, StandardCharsets.UTF_8).contains("Unsupported JavaFX configuration: classes were loaded from 'unnamed module")))
          super.write(buf, off, len);
      }
    });

    // Keep AWT from registering as a second macOS application. Opening files and links
    // (DesktopUtil, via java.awt.Desktop) and reading clipboard images (ClipboardImageHelper,
    // via java.awt.Toolkit) initialize the AWT toolkit, which otherwise claims its own Dock
    // icon next to the JavaFX one and can steal focus as it starts. Must be set before AWT
    // is first touched, so it goes here rather than at the use sites.

    if (IS_OS_MAC)
      System.setProperty("apple.awt.UIElement", "true");

    // Do NOT set glass.accessible.force=false here (tried and reverted, July 2026). By default, JxBrowser
    // on macOS resolves a BrowserView's native window THROUGH the JavaFX accessibility bridge, so disabling
    // that bridge made the lookup fail and every preview pane render blank (Chromium ran, the surface never
    // attached). BrowserEngine.initialize now switches JxBrowser to a reflection-based lookup instead
    // (jxbrowser.javafx.jni.embedding.disabled), which sidesteps the accessibility bridge entirely; should
    // that switch ever be removed, the default lookup and this trap both come back.

    javafx.application.Application.launch(App.class, args);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
