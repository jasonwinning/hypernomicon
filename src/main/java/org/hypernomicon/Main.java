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

package org.hypernomicon;

import java.io.PrintStream;
import java.nio.charset.StandardCharsets;

//---------------------------------------------------------------------------

public final class Main
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Main() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------

  public static void main(String[] args)
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

    javafx.application.Application.launch(App.class, args);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
