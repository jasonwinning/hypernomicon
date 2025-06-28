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

import static org.hypernomicon.util.Util.assertThatThisIsUnitTestThread;

//---------------------------------------------------------------------------

public final class StringUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private StringUtil() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // at class level, initialize a small reusable buffer
  private static final ThreadLocal<char[]> COLLAPSE_BUF = ThreadLocal.withInitial(() -> new char[128]);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void clearCollapseBufferForTest()
  {
    assertThatThisIsUnitTestThread();

    COLLAPSE_BUF.remove();
  }

  /**
   * Collapse runs of two-or-more spaces into one. If no runs found, returns
   * the original String (no GC pressure); Otherwise reuses a ThreadLocal
   * char[] and emits exactly one new String
   */
  public static String collapseSpaces(CharSequence cs)
  {
    int len = cs.length();

    // detect whether we even need to do work

    for (int i = 1; i < len; i++)
    {
      if ((cs.charAt(i) == ' ') && (cs.charAt(i - 1) == ' '))
      {
        // found >1 space, go to slow path

        return collapseWithBuffer(cs, len);
      }
    }

    // no runs of spaces, return original

    return cs.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String collapseWithBuffer(CharSequence cs, int len)
  {
    // grab or grow our shared buffer

    char[] buf = COLLAPSE_BUF.get();

    if (buf.length < len)
    {
      buf = new char[len];
      COLLAPSE_BUF.set(buf);
    }

    // emit chars, skipping extra spaces

    int out = 0;
    char prev = cs.charAt(0);
    buf[out++] = prev;

    for (int i = 1; i < len; i++)
    {
      char c = cs.charAt(i);
      if ((c == ' ') && (prev == ' '))
      {
        // skip this space
        continue;
      }

      buf[out++] = c;
      prev = c;
    }

    // wrap in a String exactly once

    return new String(buf, 0, out);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
