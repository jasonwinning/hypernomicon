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

package org.hypernomicon.query.personMatch;

import java.util.*;

import static org.hypernomicon.util.StringUtil.*;

//---------------------------------------------------------------------------

/**
 * A name similarity matcher that determines whether two personal names plausibly refer
 * to the same individual based on token-level alignment of first names and exact matching of last names.
 *
 * <p>The idea is that there should either be a match in firstName2 for each token in firstName1, or a match
 * in firstName1 of each token in firstName2.
 *
 * <p>Additionally, if one name has no match for a spelled-out token in the other name, then both names
 * should have at least one match that is a spelled-out name in both.
 *
 * <p>So the first condition forces one of the names to loosely match everything in the other, and the second
 * condition forces strong mismatches to be accompanied by a corresponding strong match.
 *
 * <p>This matcher supports flexible name variants including initials, abbreviations, and partial names,
 * while carefully avoiding false positives through a two-part validation rule:
 *
 * <ol>
 *   <li><strong>Directional Coverage:</strong> There must exist at least one direction
 *       (firstName1 -> firstName2 or vice versa) in which all tokens from one name
 *       can be matched injectively against tokens from the other. Tokens match if
 *       they are identical (case-insensitive), or if one is an initial matching the
 *       first letter of the other.
 *   </li>
 *   <li><strong>Conflict Resolution:</strong> If any full-name token (length > 1) on either side
 *       cannot be explained through token matches, the names will still match only if there
 *       exists at least one pair of tokens, one from each side, both spelled out and matching.
 *       This compensating full-name bridge stabilizes the match and justifies accepting it.
 *   </li>
 * </ol>
 *
 * <p>The result is a matcher that behaves similarly to a cautious human reader:
 * it tolerates abbreviations and incomplete forms, but only when supported by concrete
 * shared identity signals. Each token from one side must be accounted for without stretching
 * the meaning of tokens on the other side.
 *
 * <p>Last names must match exactly, using case-insensitive comparison.
 *
 * <p>Examples of accepted matches for first names (assuming last names match):
 * <ul>
 *   <li>"J. Camiel" === "Jan C."</li>
 *   <li>"Laurence Jonathan" === "Jonathan"</li>
 *   <li>"J. L." === "John Langshaw"</li>
 * </ul>
 *
 * <p>Examples of rejected matches for first names (assuming last names match):
 * <ul>
 *   <li>"John J." =/= "John Howland" (token reuse)</li>
 *   <li>"Andrew D." =/= "Dennis" (conflicting spelled-out names)</li>
 *   <li>"James Stacey" =/= "James Garden" (incompatible additions)</li>
 * </ul>
 */
final class NameMatcher
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private NameMatcher() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determines whether two personal names (first and last) are plausibly equivalent
   * based on flexible first-name token matching and strict last-name equality.
   * <p>
   * This does not do English character transliteration; that must be done by the caller.
   *
   * <p>Last names must match exactly (case-insensitive).
   *
   * <p>First name matching is governed by a two-part rule:
   * <ol>
   *   <li><strong>Directional Explanation:</strong> At least one of the two first names must be able
   *       to fully and injectively explain the tokens of the other — meaning each token in one name
   *       must match exactly once to a distinct token in the other. Tokens match if they are identical
   *       (case-insensitive) or if one is a single-letter initial corresponding to the other's first letter.</li>
   *
   *   <li><strong>Conflict Reconciliation:</strong> If there are any unmatched full-name tokens
   *       (i.e., tokens longer than one character) in either direction, then there must also exist
   *       at least one pair of matching tokens between the two names where both tokens are spelled out
   *       (not initials). This ensures that mismatches are tolerated only when anchored by a confident bridge.</li>
   * </ol>
   *
   * <p>The result is a matcher that permits flexible name variants including initials and omitted tokens
   * while defending against false positives due to token reuse or conflicting full names.
   *
   * @param person1 first person
   * @param person2 second person
   * @return true if the names are considered a match under the defined logic, false otherwise.
   */
  static boolean namesMatch(PersonForDupCheck person1, PersonForDupCheck person2)
  {
    String firstName1 = person1.getNameEngChar().getFirst(),
           lastName1  = person1.getNameEngChar().getLast(),
           firstName2 = person2.getNameEngChar().getFirst(),
           lastName2  = person2.getNameEngChar().getLast();

    if (containsNoLetters(lastName1))
    {
      if (containsNoLetters(firstName1))
        return (firstName1 + ' ' + lastName1).strip().equalsIgnoreCase((firstName2 + ' ' + lastName2).strip());

      lastName1 = firstName1;
      firstName1 = "";
    }

    if (containsNoLetters(lastName2))
    {
      if (containsNoLetters(firstName2))
        return (firstName1 + ' ' + lastName1).strip().equalsIgnoreCase((firstName2 + ' ' + lastName2).strip());

      lastName2 = firstName2;
      firstName2 = "";
    }

    if (lastName1 .equalsIgnoreCase(lastName2 ) == false) return false;
    if (firstName1.equalsIgnoreCase(firstName2)         ) return true;

    List<List<String>> variants1 = firstName1.isBlank() ? List.of() : person1.getTokenizations(),
                       variants2 = firstName2.isBlank() ? List.of() : person2.getTokenizations();

    if (variants1.isEmpty() && variants2.isEmpty()) return true;

    for (List<String> tokens1 : variants1)
    {
      for (List<String> tokens2 : variants2)
      {
        if (isInjectivelyExplained(tokens1, tokens2) || isInjectivelyExplained(tokens2, tokens1))
        {
          if (hasNoUnmatchedSpelled(tokens1, tokens2) && hasNoUnmatchedSpelled(tokens2, tokens1))
            return true;

          if (hasSpelledOutBridge(tokens1, tokens2))
            return true;
        }
      }
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determines whether every token in the source list can be uniquely matched
   * to a token in the reference list using token-level identity or initial-name logic.
   * This enforces injective matching: each reference token may be used at most once.
   *
   * @param source The list of tokens to be explained.
   * @param reference The list of tokens providing candidate matches.
   * @return true if all source tokens can be injectively matched to reference tokens.
   */
  private static boolean isInjectivelyExplained(List<String> source, List<String> reference)
  {
    Set<Integer> used = new HashSet<>();

    for (String s : source)
    {
      boolean matched = false;

      for (int i = 0; i < reference.size(); i++)
      {
        if (used.contains(i)) continue;

        String r = reference.get(i);

        if (tokensMatch(s.toLowerCase(), r.toLowerCase()))
        {
          used.add(i);
          matched = true;
          break;
        }
      }

      if (matched == false) return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns false if the source list contains at least one spelled-out token
   * (length > 1) that cannot be matched to any token in the reference list.
   * This indicates the presence of an unexplained full name token that may violate alignment.
   *
   * @param source The list to check for unmatched spelled-out tokens.
   * @param reference The list providing potential matches.
   * @return true if an unmatched full-name token exists in the source.
   */
  private static boolean hasNoUnmatchedSpelled(List<String> source, List<String> reference)
  {
    for (String s : source)
    {
      if (s.length() == 1) continue;

      boolean matched = false;

      for (String r : reference)
      {
        if (tokensMatch(s.toLowerCase(), r.toLowerCase()))
        {
          matched = true;
          break;
        }
      }

      if (matched == false) return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns true if there exists at least one pair of tokens—one from each list—
   * where both tokens are spelled-out names and match according to token rules.
   * This provides a high-confidence anchor to permit matches in the presence of partial mismatches.
   *
   * @param a One list of name tokens.
   * @param b The other list of name tokens.
   * @return true if a matching full-name token pair exists across the two lists.
   */
  private static boolean hasSpelledOutBridge(List<String> a, List<String> b)
  {
    for (String s : a)
    {
      if (s.length() == 1) continue;

      for (String t : b)
      {
        if (t.length() == 1) continue;

        if (tokensMatch(s.toLowerCase(), t.toLowerCase()))
          return true;
      }
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determines whether two tokens match under relaxed identity rules:
   * tokens match if they are equal (case-insensitive),
   * or one is an initial matching the first letter of the other.
   *
   * @param a A name or initial token.
   * @param b Another token to compare with.
   * @return true if a and b match according to token rules.
   */
  private static boolean tokensMatch(String a, String b)
  {
    return (a.equals(b)                             ||
            ((a.length() == 1) && b.startsWith(a))  ||
            ((b.length() == 1) && a.startsWith(b)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
