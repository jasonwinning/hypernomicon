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

import javafx.application.Platform;

import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.*;

//---------------------------------------------------------------------------

public final class FxTestUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static volatile boolean jfxInitialized = false;

  private FxTestUtil() { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void suppressJavaFxConfigWarning()
  {
    System.setErr(new PrintStream(System.out)
    {
      @Override public void write(byte[] buf, int off, int len)
      {
        String s = new String(buf, off, len, StandardCharsets.UTF_8);

        if (s.contains("Unsupported JavaFX configuration: classes were loaded from 'unnamed module") == false)
          super.write(buf, off, len);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void initJfx()
  {
    if (jfxInitialized)
      return;

    suppressJavaFxConfigWarning();

    try
    {
      CountDownLatch latch = new CountDownLatch(1);
      Platform.startup(latch::countDown);
      if (latch.await(5, TimeUnit.SECONDS) == false)
        fail("JavaFX platform failed to start within timeout");

      jfxInitialized = true;
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
      fail("JavaFX init interrupted", e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void runFxAndWait(Runnable runnable)
  {
    if (Platform.isFxApplicationThread())
    {
      runnable.run();
      return;
    }

    final AtomicReference<Throwable> thrown = new AtomicReference<>();
    CountDownLatch latch = new CountDownLatch(1);

    Platform.runLater(() ->
    {
      try
      {
        runnable.run();
      }
      catch (Throwable t)
      {
        thrown.set(t);
      }
      finally
      {
        latch.countDown();
      }
    });

    try
    {
      if (latch.await(5, TimeUnit.SECONDS) == false)
      {
        fail("JavaFX action did not complete within timeout");
      }
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
      fail("JavaFX action interrupted", e);
    }

    Throwable t = thrown.get();

    if (t != null)
    {
      if (t instanceof RuntimeException re)
        throw re;

      if (t instanceof Error e)
        throw e;

      // Checked exception: wrap with context
      throw new RuntimeException("Exception in JavaFX Application Thread", t);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
