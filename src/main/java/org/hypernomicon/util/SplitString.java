/*
 * Copyright 2015-2023 Jason Winning
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

import static org.hypernomicon.util.Util.*;

import java.util.Iterator;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// Splits strings lazily

public class SplitString implements Iterable<String>, Iterator<String>
{
  private final String wholeStr;
  private final char delim;

  private String nextPart;
  private int fromNdx = 0;
  private boolean hasNext = true;

  public SplitString(String wholeStr, char delim)
  {
    this.wholeStr = safeStr(wholeStr);
    this.delim = delim;

    advance();
  }

//---------------------------------------------------------------------------

  @Override public Iterator<String> iterator() { return this; }
  @Override public boolean hasNext()           { return hasNext; }

  public Stream<String> stream()               { return StreamSupport.stream(spliterator(), false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String next()
  {
    if (hasNext == false) return "";

    String retVal = nextPart;
    advance();
    return retVal;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void advance()
  {
    if (fromNdx < 0)
    {
      hasNext = false;
      return;
    }

    if (fromNdx == wholeStr.length())
    {
      nextPart = "";
      fromNdx = -1;
      return;
    }

    int nextNdx = wholeStr.indexOf(delim, fromNdx);

    if (nextNdx < 0)
    {
      nextPart = wholeStr.substring(fromNdx);
      fromNdx = -1;
      return;
    }

    nextPart = wholeStr.substring(fromNdx, nextNdx);
    fromNdx = nextNdx + 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
