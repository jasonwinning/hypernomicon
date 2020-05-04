/*
 * Copyright 2015-2020 Jason Winning
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.google.common.collect.Lists;

import static org.hypernomicon.util.Util.*;

public class VersionNumber implements Magnitude<VersionNumber>
{
  private final List<Integer> parts;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public VersionNumber(int minParts, Integer... parts)
  {
    List<Integer> tempParts = Lists.newArrayList(parts);

    while (tempParts.size() < minParts)
      tempParts.add(0);

    this.parts = Collections.unmodifiableList(tempParts);
  }

  public VersionNumber(int minParts, String str)
  {
    List<Integer> tempParts = new ArrayList<>();

    Arrays.asList(str.split("\\.")).forEach(partStr -> tempParts.add(parseInt(partStr, 0)));

    while (tempParts.size() < minParts)
      tempParts.add(0);

    parts = Collections.unmodifiableList(tempParts);
  }

  public int numParts() { return parts.size(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getPart(int ndx)
  {
    if (ndx >= parts.size())
      return 0;

    return parts.get(ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString()
  {
    return parts.stream().map(String::valueOf).reduce((part1, part2) -> part1 + "." + part2).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    List<Integer> newList = new ArrayList<>();
    boolean gotNonzero = false;

    for (int ndx = parts.size() - 1; ndx >= 0; ndx--)
    {
      if (parts.get(ndx) > 0)
        gotNonzero = true;

      if (gotNonzero)
        newList.add(0, parts.get(ndx));
    }

    return newList.stream().reduce(1, (num1, num2) -> 31 * num1 + num2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    VersionNumber other = (VersionNumber) obj;

    for (int ndx = 0; ndx < Math.max(parts.size(), other.numParts()); ndx++)
      if (getPart(ndx) != other.getPart(ndx))
        return false;

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(VersionNumber o)
  {
    for (int ndx = 0; ndx < Math.max(parts.size(), o.numParts()); ndx++)
    {
      int cmp = Integer.compare(getPart(ndx), o.getPart(ndx));
      if (cmp != 0) return cmp;
    }

    return 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
