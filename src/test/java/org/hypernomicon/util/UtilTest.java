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

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Unit test class for the utility functions in the {@code Util} class.
 *
 * @see org.hypernomicon.util.Util
 */
class UtilTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testRemoveDuplicatesInPlace_WithDuplicates()
  {
    List<String> list = new ArrayList<>(List.of("apple", "banana", "apple", "orange", "banana")),
                 expected = List.of("apple", "banana", "orange"),

                 result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "The list should have duplicates removed and maintain the order of first occurrence.");
  }

  @Test
  void testRemoveDuplicatesInPlace_NoDuplicates()
  {
    List<String> list = new ArrayList<>(List.of("apple", "banana", "orange")),
                 expected = List.of("apple", "banana", "orange"),

                 result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "The list should remain unchanged as there are no duplicates.");
  }

  @Test
  void testRemoveDuplicatesInPlace_EmptyList()
  {
    List<String> list = new ArrayList<>(),
                 expected = new ArrayList<>(),

                 result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "An empty list should remain empty.");
  }

  @Test
  void testRemoveDuplicatesInPlace_SingleElementList()
  {
    List<String> list = new ArrayList<>(List.of("apple")),
                 expected = List.of("apple"),

                 result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "A single-element list should remain unchanged.");
  }

  @Test
  void testRemoveDuplicatesInPlace_AllDuplicates()
  {
    List<String> list = new ArrayList<>(List.of("apple", "apple", "apple")),
                 expected = List.of("apple"),

                 result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "The list should have only one element after removing all duplicates.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
