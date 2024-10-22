/*
 * Copyright 2015-2024 Jason Winning
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
import java.util.stream.Collectors;

import com.google.common.collect.Lists;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class VersionNumber implements Comparable<VersionNumber>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<Integer> parts;
  private final int minParts;

  private static final int MINIMUM_PARTS = 2;

//---------------------------------------------------------------------------

  public VersionNumber(Integer... parts)
  {
    List<Integer> tempParts = Lists.newArrayList(parts);

    minParts = MINIMUM_PARTS;

    while (tempParts.size() < minParts)
      tempParts.add(0);

    this.parts = Collections.unmodifiableList(tempParts);
  }

//---------------------------------------------------------------------------

  public VersionNumber(String str)
  {
    List<Integer> tempParts = Arrays.stream(str.split("\\.")).map(partStr -> parseInt(partStr, 0))
                                                             .collect(Collectors.toCollection(ArrayList::new));
    minParts = MINIMUM_PARTS;

    while (tempParts.size() < minParts)
      tempParts.add(0);

    parts = Collections.unmodifiableList(tempParts);
  }

//---------------------------------------------------------------------------

  public int numParts()       { return parts.size(); }
  public int getPart(int ndx) { return ndx < parts.size() ? parts.get(ndx) : 0; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString()
  {
    return parts.stream().map(String::valueOf).collect(Collectors.joining("."));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    List<Integer> relevantParts = new ArrayList<>(parts);

    // Remove trailing zeros
    while ((relevantParts.isEmpty() == false) && (relevantParts.get(relevantParts.size() - 1) == 0))
      relevantParts.remove(relevantParts.size() - 1);

    return relevantParts.hashCode();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if ((obj == null) || (getClass() != obj.getClass())) return false;

    return compareTo((VersionNumber) obj) == 0;
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
