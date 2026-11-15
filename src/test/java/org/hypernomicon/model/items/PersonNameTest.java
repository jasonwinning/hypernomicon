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

package org.hypernomicon.model.items;

import static org.junit.jupiter.api.Assertions.*;

import java.util.HashSet;
import java.util.List;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Tests for the levels at which two person names count as the same, as the {@link PersonName}
 * class description sets them out.
 * <p>
 * Most of the testing of person names is elsewhere. {@code PersonDupTest} (in
 * {@code org.hypernomicon.query.personMatch}) has far more cases: parsing a name from one string
 * into its first and last parts, the fuzzy duplicate check, and OmniFinder's name search.
 * {@code HDT_PersonLookUpByNameTest} covers finding a person record by name, and
 * {@code BibAuthorsTest} covers consolidating an author list.
 */
class PersonNameTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void equalNamesHashAlike()
  {
    PersonName name1 = new PersonName("John", "Smith"),
               name2 = new PersonName("JOHN", "smith");

    assertEquals(name1, name2, "equals should ignore case");
    assertEquals(name1.hashCode(), name2.hashCode(), "Equal names should have the same hash code");
    assertEquals(1, new HashSet<>(List.of(name1, name2)).size(), "A hash set should hold equal names once");
  }

//---------------------------------------------------------------------------

  @Test
  void onePartNameEqualsSameTextInOtherPart()
  {
    PersonName firstOnly = new PersonName("Plato", ""),
               lastOnly  = new PersonName("", "Plato");

    assertEquals(firstOnly, lastOnly);
    assertEquals(firstOnly.hashCode(), lastOnly.hashCode());
  }

//---------------------------------------------------------------------------

  @Test
  void sameNameKeyIgnoresCaseDiacriticsAndEveryParenthetical()
  {
    String key = new PersonName("Jose", "Nunez").sameNameKey();

    assertEquals(key, new PersonName("josé (Pepe) (Pepito)", "NÚÑEZ (Jr.)").sameNameKey());
    assertNotEquals(key, new PersonName("Juan", "Nunez").sameNameKey());
    assertNotEquals(key, new PersonName("Jose", "Nuño").sameNameKey());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
