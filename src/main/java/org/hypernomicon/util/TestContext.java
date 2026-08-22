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

//---------------------------------------------------------------------------

/**
 * Detects whether the current thread is running under a unit test, for
 * production code that relaxes or asserts a rule in that context (test-only
 * setters, registry and session guards). Deliberately dependency-free: it is
 * called from core classes whose own initialization must not pull anything
 * else in.
 */
public final class TestContext
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TestContext() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns true if the current thread is running in a JUnit test context.
   */
  public static boolean isUnitTestThread()
  {
    return StackWalker.getInstance().walk(frames -> frames.anyMatch(frame ->
      frame.getClassName().startsWith("org.junit.") || frame.getClassName().startsWith("junit.")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Use this in functions that are only supposed to run in a unit test.
   */
  public static void assertThatThisIsUnitTestThread()
  {
    if (isUnitTestThread() == false)
      throw new AssertionError("Can only run in unit test.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
