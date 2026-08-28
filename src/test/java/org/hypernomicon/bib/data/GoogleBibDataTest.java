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

package org.hypernomicon.bib.data;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import org.hypernomicon.util.http.AsyncHttpClient;

//---------------------------------------------------------------------------

class GoogleBibDataTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The dedupe set is keyed by canonical ISBN-13, so the ISBN-10 form of an
   *  already-queried book is skipped without a request; with every ISBN skipped
   *  and no title, the request reports a clean miss synchronously. */
  @Test
  void isbn10FormOfCheckedIsbn13IsSkipped()
  {
    Set<String> checkedIDs = new HashSet<>(Set.of("9780140449266"));
    List<GoogleBibData> results = new ArrayList<>();

    GoogleBibData.doHttpRequest(new AsyncHttpClient(), List.of("0140449264").iterator(), checkedIDs, results::add, Assertions::fail);

    assertEquals(1, results.size(), "must complete synchronously with no network involved");
    assertNull(results.getFirst());
    assertEquals(Set.of("9780140449266"), checkedIDs, "nothing new was added, and no blank entry either");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
