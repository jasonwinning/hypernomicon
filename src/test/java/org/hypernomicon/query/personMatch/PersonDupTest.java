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

package org.hypernomicon.query.personMatch;

import java.util.*;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.authors.RecordAuthor;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;

import static org.hypernomicon.model.records.RecordType.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

//---------------------------------------------------------------------------

class PersonDupTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;
  private static PersonMatcher matcher;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();

    matcher = new PersonMatcher();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean checkMatch(PersonName name1, PersonName name2)
  {
    return checkMatch(name1, name2, true);
  }

  private static boolean checkMatch(PersonName name1, PersonName name2, boolean tryOtherWayAround)
  {
    matcher.clear();

    assertDoesNotThrow(() -> matcher.doDupCheck(new PersonForDupCheck(name1), List.of(new PersonForDupCheck(name2)), null));

    if (matcher.isEmpty()) return false;

    assertEquals(1, matcher.numMatches());

    assertEquals(name2, matcher.getMatchedAuthor(0).getName());

    return (tryOtherWayAround == false) || checkMatch(name2, name1, false);  // Make sure dup check is always symmetrical
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Work createWork(String newTitle)
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    work.setName(newTitle);
    return work;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Person createPerson(PersonName name)
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    person.setName(name);
    return person;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void dupPersonsTest()
  {
    // Shared works

    HDT_Work w1 = createWork("Work One"),
             w2 = createWork("Work Two"),
             w3 = createWork("Work Three"),
             w4 = createWork("Work Four"),
             w5 = createWork("Work Five");

    // Shared persons

    HDT_Person p1  = createPerson(new PersonName("Jean", "St. James"));
    HDT_Person p2  = createPerson(new PersonName("Jean", "X. St. James"));    // longer initials
    HDT_Person p3  = createPerson(new PersonName("Henry", "St. John"));
    HDT_Person p4  = createPerson(new PersonName("Bøb", "Söderström"));       // Unicode
    HDT_Person p5  = createPerson(new PersonName("Li", "Wei"));               // minimal
    HDT_Person p6  = createPerson(new PersonName("Maria", "de la Cruz"));
    HDT_Person p7  = createPerson(new PersonName("Lí"));                      // one-word
    HDT_Person p8  = createPerson(new PersonName("Mary-Jane", "O’Neil"));
    HDT_Person p9  = createPerson(new PersonName("J. B.", "Smith"));
    HDT_Person p10 = createPerson(new PersonName("J0hn", "W1lliams"));        // OCR artifacts
    HDT_Person p11 = createPerson(new PersonName("von Neumann", "John"));
    HDT_Person p12 = createPerson(new PersonName("Иван", "Иванович"));        // Cyrillic
    HDT_Person p13 = createPerson(new PersonName("Ludwig", "van Beethoven"));
    HDT_Person p14 = createPerson(new PersonName("Confucius"));               // mononym
    HDT_Person p15 = createPerson(new PersonName("Jean", "St. James"));       // duplicate of p1
    HDT_Person p16 = createPerson(new PersonName("José", "Niño"));            // composed
    HDT_Person p17 = createPerson(new PersonName("José", "Niño"));            // decomposed
    HDT_Person p18 = createPerson(new PersonName("Sarah", "Connor"));
    HDT_Person p19 = createPerson(new PersonName("Eve", "Quinn"));
    HDT_Person p20 = createPerson(new PersonName("J"));                       // abbreviation

    // Assign works

    w1.getAuthors().add(p1);
    w2.getAuthors().add(p6);
    w3.getAuthors().add(p13);
    w1.getAuthors().add(p18);  // Share w1
    w4.getAuthors().add(p19);

    RecordAuthor a1 = new RecordAuthor(new PersonName("Donald Davidson"));

    // Construct candidate list

    List<PersonForDupCheck> candidates = List.of
    (
      new PersonForDupCheck(p1),                                                                 // C1
      new PersonForDupCheck(p2),                                                                 // C2
      new PersonForDupCheck(p3),                                                                 // C3
      new PersonForDupCheck(p4),                                                                 // C4
      new PersonForDupCheck(p5),                                                                 // C5
      new PersonForDupCheck(p6),                                                                 // C6
      new PersonForDupCheck(p7),                                                                 // C7
      new PersonForDupCheck(new PersonName("")),                                                 // C8: empty name
      new PersonForDupCheck(p8),                                                                 // C9
      new PersonForDupCheck(p9),                                                                 // C10
      new PersonForDupCheck(p10),                                                                // C11
      new PersonForDupCheck(new PersonName("", "B.")),                                           // C12 one-letter
      new PersonForDupCheck(p11),                                                                // C13
      new PersonForDupCheck(p12),                                                                // C14
      new PersonForDupCheck(p13),                                                                // C15
      new PersonForDupCheck(p14),                                                                // C16
      new PersonForDupCheck(new PersonName(null, "")),                                           // C17
      new PersonForDupCheck(new RecordAuthor(w1, p14)),                                          // C18
      new PersonForDupCheck(p15),                                                                // C19: same person as C1
      new PersonForDupCheck(new RecordAuthor(createWork("Alt Work"), p1)),                       // C20: same person, new work
      new PersonForDupCheck(new RecordAuthor(w2, p1)),                                           // C21: p1 linked to a different work
      new PersonForDupCheck(new RecordAuthor(createWork("Clone Work"),
                            new PersonName("Jean", "St. James"), false, false, Ternary.Unset)),  // C22 deep-clone name
      new PersonForDupCheck(new RecordAuthor(w4, p1)),                                           // C23: work4 contains p1
      new PersonForDupCheck(p16),                                                                // C24: Unicode (composed)
      new PersonForDupCheck(p17),                                                                // C25: Unicode (decomposed)
      new PersonForDupCheck(p20),                                                                // C26: abbreviation-only
      new PersonForDupCheck(p18),                                                                // C27: different person, same work as p1
      new PersonForDupCheck(p19),                                                                // C28: all match-excluding conditions
      new PersonForDupCheck(a1)                                                                  // C29
    );

    PersonMatcher matchResult = findMatches(null, candidates);
    assertEquals(0, matchResult.numMatches());
    assertTrue(matchResult.isEmpty());

    matchResult = findMatches(new PersonForDupCheck(new PersonName("")), candidates);
    assertEquals(0, matchResult.numMatches());
    assertTrue(matchResult.isEmpty());

    matchResult = findMatches(new PersonForDupCheck(new PersonName("Dan Dennett")), candidates);
    assertEquals(0, matchResult.numMatches());
    assertTrue(matchResult.isEmpty());

    matchResult = findMatches(new PersonForDupCheck(new PersonName("Confucius")), candidates);
    assertEquals(2, matchResult.numMatches());
    assertFalse(matchResult.isEmpty());

    matchResult = findMatches(new PersonForDupCheck(new PersonName("Jose Nino")), candidates);
    assertEquals(2, matchResult.numMatches());
    assertSame(p16, matchResult.getMatchedAuthor(0).getPerson());
    assertSame(p17, matchResult.getMatchedAuthor(1).getPerson());

    matchResult = findMatches(new PersonForDupCheck(a1, new PersonName("Magda Arnold")), candidates);
    assertEquals(0, matchResult.numMatches());

    matchResult = findMatches(new PersonForDupCheck(a1, new PersonName("Donald Davidson")), candidates);  // Reject match if Author object is the same, even if name is the same
    assertEquals(0, matchResult.numMatches());

    matchResult = findMatches(new PersonForDupCheck(new RecordAuthor(p9), new PersonName("Fred Dretske")), candidates);
    assertEquals(0, matchResult.numMatches());

    matchResult = findMatches(new PersonForDupCheck(new RecordAuthor(p9), new PersonName(p9.getNameLastFirst(false))), candidates);  // Reject match if Person object is the same, even if name is the same
    assertEquals(0, matchResult.numMatches());

    RecordAuthor a2 = new RecordAuthor(w5, new PersonName("Nancy Cartwright"), false, false, Ternary.Unset),
                 a3 = new RecordAuthor(w5, new PersonName("Nancy Cartwright"), false, false, Ternary.Unset),
                 a4 = new RecordAuthor(w5, new PersonName("Noam Chomsky"    ), false, false, Ternary.Unset);

    w5.getAuthors().add(a2);
    w5.getAuthors().add(a3);
    w5.getAuthors().add(a4);

    // Reject match if authors are in the same work, regardless of whether names match

    matchResult = findMatches(new PersonForDupCheck(a2), List.of(new PersonForDupCheck(a3)));
    assertEquals(0, matchResult.numMatches());

    matchResult = findMatches(new PersonForDupCheck(w5.getAuthors().get(0)), List.of(new PersonForDupCheck(w5.getAuthors().get(1))));
    assertEquals(0, matchResult.numMatches());

//---------------------------------------------------------------------------

    // Create a person1 who always "matches" every non-skipped candidate

    PersonForDupCheck person1 = new PersonForDupCheck(p1)
    {
      @Override public boolean matches(PersonForDupCheck other)
      {
        return true;  // force every non-skipped candidate into the final set
      }
    };

    matchResult = findMatches(person1, candidates);

    List<RecordAuthor> matches = matchResult.getMatches(person1);

    // A) Skipped are: C1, C8, C17, C18, C20, C21, C23, C27

    assertEquals(21, matches.size(), "We should get exactly 21 matches; all the skip-criteria removed 8 candidates");

    // B) None of the SKIPPED candidates' authors appear:

    Set<Integer> skippedSet = new LinkedHashSet<>(List.of(0, 7, 16, 17, 19, 20, 22, 26));

    for (int ndx : skippedSet)
    {
      assertTrue(findMatches(person1, List.of(candidates.get(ndx))).isEmpty(), "Should not match candidate #" + (ndx + 1));
    }

    for (int ndx = 0; ndx < candidates.size(); ndx++)
    {
      if (skippedSet.contains(ndx)) continue;

      assertFalse(findMatches(person1, List.of(candidates.get(ndx))).isEmpty(), "Should match candidate C" + (ndx + 1));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static PersonMatcher findMatches(PersonForDupCheck person1, Iterable<PersonForDupCheck> candidates)
  {
    PersonMatcher result = new PersonMatcher();

    assertDoesNotThrow(() -> result.doDupCheck(person1, candidates, null));

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void nameMatcherTest()
  {
    assertTrue(checkMatch(new PersonName("JC", "Willems"), new PersonName("J. C.", "Willems")));
    assertTrue(checkMatch(new PersonName("J.C.", "Willems"), new PersonName("J. C.", "Willems")));
    assertTrue(checkMatch(new PersonName("JC", "Willems"), new PersonName("J.C.", "Willems")));
    assertTrue(checkMatch(new PersonName("JC", "Willems"), new PersonName("Jan C.", "Willems")));
    assertTrue(checkMatch(new PersonName("J. Camiel", "Willems"), new PersonName("Jan C.", "Willems")));

    assertTrue (checkMatch(new PersonName("JAC", "Willems"), new PersonName("Camiel", "Willems")));
    assertTrue (checkMatch(new PersonName("jac", "Willems"), new PersonName("Camiel", "Willems")));
    assertFalse(checkMatch(new PersonName("JA.C", "Willems"), new PersonName("Camiel", "Willems")));
    assertFalse(checkMatch(new PersonName("Jac", "Willems"), new PersonName("Camiel", "Willems")));

    assertTrue(checkMatch(new PersonName("Laurence Jonathan", "Cohen"), new PersonName("Jonathan", "Cohen")));
    assertTrue(checkMatch(new PersonName("Jonathan D.", "Cohen"), new PersonName("Jonathan", "Cohen")));

    assertFalse(checkMatch(new PersonName("James Stacey", "Taylor"), new PersonName("James Garden", "Taylor")));
    assertFalse(checkMatch(new PersonName("Andrew D.", "Wilson"), new PersonName("Dennis", "Wilson")));

    assertTrue(checkMatch(new PersonName("John Q. Public", "Smith"), new PersonName("J. P.", "Smith")));

    assertTrue(checkMatch(new PersonName("  John   Q.  ", "  Public  "), new PersonName("J. Q.", "Public")));
    assertTrue(checkMatch(new PersonName("John Q.", "Public"), new PersonName("  J.   Q.  ", "Public")));
    assertTrue(checkMatch(new PersonName("J. Q.", "Public"), new PersonName("John   Q.", " Public ")));
    assertTrue(checkMatch(new PersonName("  ", "Public"), new PersonName("", "Public")));

    assertTrue(checkMatch(new PersonName("J. Q.", "Public"), new PersonName("j. q.", "public")));
    assertTrue(checkMatch(new PersonName("J. Q.", "PUBLIC"), new PersonName("JOHN Q.", "public")));
    assertTrue(checkMatch(new PersonName("jan c.", "willems"), new PersonName("Jan C.", "Willems")));

    assertTrue(checkMatch(new PersonName("José", "García"), new PersonName("Jose", "Garcia")));
    assertTrue(checkMatch(new PersonName("François", "Dupont"), new PersonName("Francois", "Dupont")));
    assertTrue(checkMatch(new PersonName("Müller", "Ludwig"), new PersonName("Muller", "Ludwig")));
    assertTrue(checkMatch(new PersonName("Søren", "Kierkegaard"), new PersonName("Soren", "Kierkegaard")));
    assertTrue(checkMatch(new PersonName("Łukasz", "Nowak"), new PersonName("Lukasz", "Nowak")));

    assertFalse(checkMatch(new PersonName("", ""), new PersonName("", "")));
    assertFalse(checkMatch(new PersonName(" ", " "), new PersonName(" ", " ")));
    assertFalse(checkMatch(new PersonName(".", ","), new PersonName(";", ";")));
    assertTrue(checkMatch(new PersonName(" ;., ", " ;., "), new PersonName(" ;., ", " ;., ")));
    assertTrue(checkMatch(new PersonName("", "Smith"), new PersonName(" ", "Smith")));

    assertTrue(checkMatch(new PersonName("J, Q;", "Smith"), new PersonName("John Q.", "Smith")));
    assertTrue(checkMatch(new PersonName("John; Q,", "Smith"), new PersonName("J. Q.", "Smith")));
    assertTrue(checkMatch(new PersonName("J. Q.", "Smith"), new PersonName("J.Q.", "Smith")));

    assertTrue(checkMatch(new PersonName("William (Bill)", "Bechtel"), new PersonName("William", "Bechtel")));
    assertTrue(checkMatch(new PersonName("William (Bill)", "Bechtel"), new PersonName("Bill", "Bechtel")));
    assertFalse(checkMatch(new PersonName("(Bill)", "Bechtel"), new PersonName("(William) (Bill)", "Bechtel")));

    assertTrue(checkMatch(new PersonName("Jason", ""), new PersonName("Jason", "")));
    assertFalse(checkMatch(new PersonName("Jason R.", ""), new PersonName("Jason", "")));
    assertTrue(checkMatch(new PersonName("J. Q.", ""), new PersonName("J. Q.", " ")));
    assertFalse(checkMatch(new PersonName("J. Q.", ""), new PersonName("John Q.", " ")));

    assertTrue(checkMatch(new PersonName("John Stuart Mill", ""), new PersonName("", "John Stuart Mill")));
    assertTrue(checkMatch(new PersonName("### &&& ***", ""), new PersonName("", "### &&& ***")));
    assertFalse(checkMatch(new PersonName("### &&& ***", ""), new PersonName("", "### &&& **")));
    assertTrue(checkMatch(new PersonName("### &&&", "***"), new PersonName("", "### &&& ***")));
    assertTrue(checkMatch(new PersonName("### &&&", "***"), new PersonName("### &&& ***", "")));
    assertTrue(checkMatch(new PersonName("###", "&&& ***"), new PersonName("### &&&", "***")));

    assertFalse(checkMatch(new PersonName("John Stuart", "Mill"), new PersonName("John Stuart Mill", "")));
    assertFalse(checkMatch(new PersonName("John Stuart", "Mill"), new PersonName("", "John Stuart Mill")));
    assertFalse(checkMatch(new PersonName("J. Stuart", "Mill"), new PersonName("", "John Stuart Mill")));
    assertFalse(checkMatch(new PersonName("John Stuart", "Mill"), new PersonName("", "J. Stuart Mill")));
    assertFalse(checkMatch(new PersonName("J. Stuart", "Mill"), new PersonName("John Stuart Mill", "")));
    assertFalse(checkMatch(new PersonName("John Stuart", "Mill"), new PersonName("J. Stuart Mill", "")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void omniSearchTest()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("William (Bill) Bechtel"));

    assertTrue(personFDC.startsWith("B"));
    assertTrue(personFDC.startsWith("Bi"));
    assertTrue(personFDC.startsWith("Bill"));
    assertTrue(personFDC.startsWith("Bill "));
    assertTrue(personFDC.startsWith("Bill B"));
    assertTrue(personFDC.startsWith("Bill Bechtel"));
    assertFalse(personFDC.startsWith("Bill Bechtels"));

    assertFalse(personFDC.startsWith("(B"));
    assertFalse(personFDC.startsWith("Bill Bi"));
    assertFalse(personFDC.startsWith("C"));
    assertFalse(personFDC.startsWith(""));
    assertFalse(personFDC.startsWith(null));

    assertTrue(personFDC.startsWith("W"));
    assertTrue(personFDC.startsWith("Wi"));
    assertTrue(personFDC.startsWith("William"));
    assertTrue(personFDC.startsWith("William "));
    assertFalse(personFDC.startsWith("William ("));
    assertTrue(personFDC.startsWith("William B"));
    assertFalse(personFDC.startsWith("William Bi"));

    assertFalse(personFDC.startsWith("SBech"));
    assertFalse(personFDC.startsWith("Q Becht"));
    assertFalse(personFDC.startsWith("< Will"));
    assertFalse(personFDC.startsWith("tWill"));
    assertFalse(personFDC.startsWith("tW"));

    assertTrue(personFDC.startsWith("Bech"));
    assertTrue(personFDC.startsWith("Bechtel"));
    assertFalse(personFDC.startsWith("Bechtels"));

    assertTrue(personFDC.startsWith("Bechtel, W"));
    assertTrue(personFDC.startsWith("Bechtel, William"));
    assertFalse(personFDC.startsWith("Bechtel, WS"));
    assertFalse(personFDC.startsWith("Bechtel, Williams"));
    assertFalse(personFDC.startsWith("Bechtel, W.W."));

    assertTrue(personFDC.startsWith("B B"));
    assertTrue(personFDC.startsWith("B. B"));
    assertTrue(personFDC.startsWith("B B."));
    assertTrue(personFDC.startsWith("B. B."));
    assertTrue(personFDC.startsWith("W B"));
    assertTrue(personFDC.startsWith("W. B"));
    assertTrue(personFDC.startsWith("W B."));
    assertTrue(personFDC.startsWith("W. B."));
    assertTrue(personFDC.startsWith("B  B"));
    assertTrue(personFDC.startsWith("B.  B"));
    assertTrue(personFDC.startsWith("B  B."));
    assertTrue(personFDC.startsWith("B.  B."));
    assertTrue(personFDC.startsWith("W  B"));
    assertTrue(personFDC.startsWith("W.  B"));
    assertTrue(personFDC.startsWith("W  B."));
    assertTrue(personFDC.startsWith("W.  B."));

    assertFalse(personFDC.startsWith("B Bi"));
    assertFalse(personFDC.startsWith("B. Ba"));
    assertFalse(personFDC.startsWith("B B. t"));
    assertFalse(personFDC.startsWith("B. B.s"));
    assertFalse(personFDC.startsWith("W B4"));
    assertFalse(personFDC.startsWith("W. B("));
    assertFalse(personFDC.startsWith("W B.^"));
    assertFalse(personFDC.startsWith("W. B. >"));
    assertFalse(personFDC.startsWith("B  Br"));
    assertFalse(personFDC.startsWith("B.  Bl"));
    assertFalse(personFDC.startsWith("B  B.W"));
    assertFalse(personFDC.startsWith("B.  B.S"));
    assertFalse(personFDC.startsWith("W  B'"));
    assertFalse(personFDC.startsWith("W.  B\""));
    assertFalse(personFDC.startsWith("W  B.T"));
    assertFalse(personFDC.startsWith("W.  B.E"));

    personFDC = new PersonForDupCheck(new PersonName("Michael Smith"));

    assertFalse(personFDC.startsWith("Smith, Adam"));
    assertFalse(personFDC.startsWith("Michael, Smith"));

    personFDC = new PersonForDupCheck(new PersonName("José García"));

    assertTrue(personFDC.startsWith("Jose"));

    personFDC = new PersonForDupCheck(new PersonName("Georg Wilhelm Friedrich Hegel"));

    assertTrue(personFDC.startsWith("gwf"));
    assertTrue(personFDC.startsWith("gwfh"));
    assertTrue(personFDC.startsWith("GWFH"));
    assertTrue(personFDC.startsWith("gwf hegel"));
    assertTrue(personFDC.startsWith("gwf Hegel"));
    assertTrue(personFDC.startsWith("GWF Hegel"));

    personFDC = new PersonForDupCheck(new PersonName("Martin Luther King"));

    assertTrue(personFDC.startsWith("mlk"));
    assertTrue(personFDC.startsWith("ml king"));
    assertTrue(personFDC.startsWith("ML King"));
    assertFalse(personFDC.startsWith("mlks"));
    assertFalse(personFDC.startsWith("ML Kings"));
  }

  @Test
  void testFullMatches()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("John Stuart", "Mill"));

    assertTrue(personFDC.startsWith("John S Mill"), "Exact 'John S Mill' should match");
    assertTrue(personFDC.startsWith("John Stuart Mill"), "Exact 'John Stuart Mill' should match");
    assertTrue(personFDC.startsWith("Stuart Mill"), "Exact 'Stuart Mill' should match");
    assertTrue(personFDC.startsWith("John Mill"), "Exact 'John Mill' should match");
  }

  @Test
  void testPrefixMatches()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("John S", "Mill"));

    assertTrue(personFDC.startsWith("Jo"), "Prefix 'Jo' should match");
    assertTrue(personFDC.startsWith("S Mill"));
    assertTrue(personFDC.startsWith("S. Mill"));
    assertTrue(personFDC.startsWith("J. Mill"));
    assertFalse(personFDC.startsWith("Jo. Mill"));
    assertFalse(personFDC.startsWith("Jo Mill"));
    assertTrue(personFDC.startsWith("Mill, Jo"));
    assertFalse(personFDC.startsWith("Mill, John St"));
    assertFalse(personFDC.startsWith("Mill, St"));
    assertFalse(personFDC.startsWith("Mill, J St"));
    assertFalse(personFDC.startsWith("Mill, Jo S"));
    assertFalse(personFDC.startsWith("Mill, Jo St"));
    assertFalse(personFDC.startsWith("Mi, John"));
    assertTrue(personFDC.startsWith("john mi"), "Case-insensitive prefix 'john mi' should match");
    assertTrue(personFDC.startsWith("John Mi"), "Mixed-case prefix 'John Mi' should match");
  }

  @Test
  void testInitialSubstitutionMatches()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("John Stuart", "Mill"));

    assertTrue(personFDC.startsWith("J Stuart Mill"), "Initial substitution in first position should match");
    assertTrue(personFDC.startsWith("John S Mill"), "Initial substitution in second position should match");
    assertFalse(personFDC.startsWith("Stuart John Mill"));
    assertFalse(personFDC.startsWith("S. John Mill"));
    assertFalse(personFDC.startsWith("Stuart J. Mill"));
    assertTrue(personFDC.startsWith("J S M"), "Initial substitutions for all positions should match");
    assertFalse(personFDC.startsWith("S J M"));
    assertFalse(personFDC.startsWith("M J S"));
  }

  @Test
  void testNegativeMatches()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("John S", "Mill"));

    assertFalse(personFDC.startsWith("Joh Mill"), "Incomplete across boundary 'Joh Mil' should not match");
    assertFalse(personFDC.startsWith("John Mill Phd"), "Query longer than candidate should not match");
  }

  @Test
  void testCaseAndPunctuationInsensitive()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("J.D.", "Roe"));

    assertTrue(personFDC.startsWith("J.D. Roe"), "Punctuation in query should be stripped");
    assertTrue(personFDC.startsWith("jd roe"), "Lower-case 'jd roe' should match");
    assertTrue(personFDC.startsWith("J D Roe"), "Spaces in place of punctuation should match");
    assertTrue(personFDC.startsWith("j.d. r"), "Partial prefix with punctuation should match");
    assertFalse(personFDC.startsWith("jr"), "Wrong concatenation without space should not match");
  }

  @Test
  void testEmptyQuery()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("Alice B", "Smith"));

    // Empty query should not match
    assertFalse(personFDC.startsWith(""), "Empty query should return false");
  }


  @Test
  void testNoVariants()
  {
    // No name variants provided
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName(""));

    // Only empty query matches
    assertFalse(personFDC.startsWith("Doe"), "With no variants, non-empty query should not match");
    assertFalse(personFDC.startsWith(""), "Empty query should not match even with no variants");
  }

  @Test
  void testDotAndSpaceAfterInitials()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("John D", "Smith"));

    // Single initial with dot and space
    assertTrue(personFDC.startsWith("J. D. Smith"), "Dots and space after initials should match");

    // Extra spaces after the dot should collapse and match
    assertTrue(personFDC.startsWith("J.  D. Smith"), "Multiple spaces after dot should collapse and match");

    // No space between initials (dot only) should match
    assertTrue(personFDC.startsWith("J.D. Smith"), "No space after dot should match");

    // Consecutive initials without punctuation
    assertTrue(personFDC.startsWith("JD Smith"), "Initials without space or dot should match");
  }

  @Test
  void testWhitespaceHandling()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("  Alice  Bob  C", "  Smith  "));

    assertTrue(personFDC.startsWith(" Alice Smith"), "Leading space in query should be trimmed and match");
    assertTrue(personFDC.startsWith("Bob C Smith"), "Extra spaces in query should collapse and match");
    assertFalse(personFDC.startsWith("AliceBob Smith"), "Merged tokens without space should not match");
  }

  @Test
  void testMinimalVariantElements()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("A", "X"));

    assertTrue(personFDC.startsWith("A X"));
    assertTrue(personFDC.startsWith("X"));
  }

  @Test
  void testPunctuationInVariants()
  {
    PersonForDupCheck personFDC = new PersonForDupCheck(new PersonName("O'Connor", "Mc'Gee"));

    assertFalse(personFDC.startsWith("OConnor McGee"), "Punctuation in variant and last name doesn't get stripped");
    assertTrue(personFDC.startsWith("O'Connor Mc'Gee"), "Original punctuation in query should be ignored and match");
    assertFalse(personFDC.startsWith("O’Connor Mc’Gee"), "Unicode conversion must be done by the caller");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void nameParseTest()
  {
    // Todo: figure out how this ideally should work

    assertTrue(checkMatch(new PersonName("A"), new PersonName("", "A")));
    assertTrue(checkMatch(new PersonName("A."), new PersonName("", "A.")));

    assertTrue(checkMatch(new PersonName("Li"), new PersonName("", "Li")));
    assertTrue(checkMatch(new PersonName("Lí"), new PersonName("", "Lí")));
    assertEquals("Lí", new PersonName("Lí").getLast());

    assertTrue(checkMatch(new PersonName("Bob X  . J"), new PersonName("Bob X.", "J")));
    assertTrue(checkMatch(new PersonName("Bob X.J"), new PersonName("Bob X.", "J")));
    assertTrue(checkMatch(new PersonName("Bob Z."), new PersonName("Bob", "Z.")));

    assertTrue(checkMatch(new PersonName("Bøb X  . J"), new PersonName("Bøb X.", "J")));
    assertTrue(checkMatch(new PersonName("Bøb X.J"), new PersonName("Bøb X.", "J")));
    assertTrue(checkMatch(new PersonName("Bøb Z."), new PersonName("Bøb", "Z.")));
    assertEquals("Bøb X.", new PersonName("Bøb X.J").getFirst());

    assertTrue(checkMatch(new PersonName("  Anna   K.  "), new PersonName("Anna", "K.")));
    assertTrue(checkMatch(new PersonName("Bob\tZ."), new PersonName("Bob", "Z.")));

    assertTrue(checkMatch(new PersonName("John B. C."), new PersonName("John B.", "C.")));

    assertTrue(checkMatch(new PersonName("John   A.B. C."), new PersonName("John A.B.", "C.")));
    assertTrue(checkMatch(new PersonName("J. A. B. Smith"), new PersonName("J. A. B.", "Smith")));

    assertTrue(checkMatch(new PersonName("John von Neumann"), new PersonName("John", "von Neumann")));
    assertTrue(checkMatch(new PersonName("Jø von Neumann"), new PersonName("Jø", "von Neumann")));
    assertEquals("Jøhn", new PersonName("Jøhn von Neumann").getFirst());

    assertTrue(checkMatch(new PersonName("Jøvon Neumann"), new PersonName("Jøvon", "Neumann")));
    assertEquals("Jøvon", new PersonName("Jøvon Neumann").getFirst());

    assertTrue(checkMatch(new PersonName("de Gaulle, Charles"), new PersonName("Charles", "de Gaulle")));
    assertTrue(checkMatch(new PersonName("de la Cruz, Maria"), new PersonName("Maria", "de la Cruz")));

    //  Todo: Consider handling other particles
    //
    //  assertTrue(checkMatch(new PersonName("Charles de Gaulle"), new PersonName("Charles", "de Gaulle")));
    //  assertTrue(checkMatch(new PersonName("Maria de la Cruz"), new PersonName("Maria", "de la Cruz")));

    assertTrue(checkMatch(new PersonName("Jean B. St. James"), new PersonName("Jean B.", "St. James")));
    assertTrue(checkMatch(new PersonName("Jean B. X. St. James"), new PersonName("Jean B. X.", "St. James")));

    assertTrue(checkMatch(new PersonName("Jean  B.. St . James"), new PersonName("Jean B.", "St. James")));
    assertEquals("Jean B.", new PersonName("Jean  B.. St . James").getFirst());
    assertEquals("St. James", new PersonName("Jean  B.. St . James").getLast());

    assertTrue(checkMatch(new PersonName("Jean-Baptiste St.-Pierre"), new PersonName("Jean-Baptiste", "St.-Pierre")));
    assertTrue(checkMatch(new PersonName("Anne Marie St. John"), new PersonName("Anne Marie", "St. John")));

    assertTrue(checkMatch(new PersonName("Ludwig van Beethoven"), new PersonName("Ludwig", "van Beethoven")));

    assertTrue(checkMatch(new PersonName("Mary-Jane O'Neil"), new PersonName("Mary-Jane", "O'Neil")));

    assertTrue(checkMatch(new PersonName("Miles J.T. O'Brien"), new PersonName("Miles J.T.", "O'Brien")));

    assertEquals("José", new PersonName("José García").getFirst());
    assertEquals("García", new PersonName("José García").getLast());

//    Todo: Handle OCR errors
//
//    assertTrue(checkMatch(new PersonName("B0b X. J"), new PersonName("Bob X.", "J")));
//    assertTrue(checkMatch(new PersonName("W1lliam J."), new PersonName("William", "J.")));
//    assertTrue(checkMatch(new PersonName("Smith. J."), new PersonName("J.", "Smith")));
//    assertTrue(checkMatch(new PersonName("O.Neil, Mary-Jane"), new PersonName("Mary-Jane", "O'Neil")));

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
