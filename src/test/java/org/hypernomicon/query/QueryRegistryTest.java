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

package org.hypernomicon.query;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Query favorites find a row's query by its ID in the registry, so each ID must belong to one query.
 */
class QueryRegistryTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void eachQueryIDBelongsToOneQuery()
  {
    List<Integer> ids = QueryRegistry.all().stream().map(Query::getID).toList();
    Set<Integer> seen = new HashSet<>();

    assertFalse(ids.isEmpty());
    assertEquals(List.of(), ids.stream().filter(id -> seen.add(id) == false).toList(), "IDs used by more than one query");
  }

//---------------------------------------------------------------------------

  @Test
  void eachQueryIsFoundByItsID()
  {
    QueryRegistry.all().forEach(query -> assertSame(query, QueryRegistry.withID(query.getID())));

    assertNull(QueryRegistry.withID(-1));
  }

//---------------------------------------------------------------------------

}
