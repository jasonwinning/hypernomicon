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

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.RecordAuthor;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;

import com.google.common.collect.ImmutableList;

//---------------------------------------------------------------------------

public class PersonForDupCheck
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final RecordAuthor author;
  final String fullLCNameEngChar;

  private final PersonName name, nameEngChar;
  private final List<List<String>> variants;
  private final TrieNode trieRoot = new TrieNode();
  private final String normalizedLastName;

  public PersonForDupCheck(PersonName name)                      { this(new RecordAuthor(name)); }
  public PersonForDupCheck(HDT_Person person)                    { this(new RecordAuthor(person)); }
  public PersonForDupCheck(RecordAuthor author)                  { this(author, author.getName(), author.fullName(true)); }
  public PersonForDupCheck(RecordAuthor author, PersonName name) { this(author, name, convertToEnglishChars(name.getFull())); }

  private static final Pattern FIRST_PARENTHETICAL_PATTERN = Pattern.compile("\\(([^)]*)\\)"),
                               NAME_PUNCTUATION_PATTERN    = Pattern.compile("[.,;]");

//---------------------------------------------------------------------------

  private PersonForDupCheck(RecordAuthor author, PersonName name, String newFullNameEngChar)
  {
    this.author = author == null ? new RecordAuthor(name) : author;
    this.name = name;
    this.nameEngChar = name.toEngChar();

    newFullNameEngChar = removeAllParentheticals(newFullNameEngChar.toLowerCase());

    newFullNameEngChar = collapseSpaces(newFullNameEngChar);

    fullLCNameEngChar = NAME_PUNCTUATION_PATTERN.matcher(newFullNameEngChar.strip()).replaceAll("");

    variants = createTokenizations(nameEngChar.getFirst());

    // Now build Trie for prefix lookup in OmniFinder

    normalizedLastName = normalizeStrForTrie(nameEngChar.getLast());

    buildOmniSearchTrie();
  }

//---------------------------------------------------------------------------

  public HDT_Person getPerson()         { return nullSwitch(author, null, Author::getPerson); }
  public HDT_Work getWork()             { return nullSwitch(author, null, RecordAuthor::getWork); }
  public RecordAuthor getAuthor()       { return author; }
  public PersonName getName()           { return name; }
  public PersonName getNameEngChar()    { return nameEngChar; }

  public List<List<String>> getTokenizations() { return variants; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean matches(PersonForDupCheck person2)
  {
    return NameMatcher.namesMatch(this, person2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Generates tokenization variants for a given first name string.
   * Includes the standard space-separated parsing, and an additional
   * interpretation as individual initials if the name is all-caps or all-lowercase.
   *
   * @param name The full first name string.
   * @return A list of possible token lists representing the name.
   */
  private static List<List<String>> createTokenizations(String name)
  {
    List<List<String>> variants = new ArrayList<>();

    String cleanedName = collapseSpaces(NAME_PUNCTUATION_PATTERN.matcher(name).replaceAll(" "));

    cleanedName = removeAllParentheticals(cleanedName).strip();

    Matcher firstParentheticalMatcher = FIRST_PARENTHETICAL_PATTERN.matcher(name);

    name = name.strip();

    variants.add(tokenize(cleanedName, false));

    if (isUniformLetterCase(name))
      variants.add(tokenize(name, true));

    if (firstParentheticalMatcher.find())
      variants.add(tokenize(firstParentheticalMatcher.group(1), false));

    variants.removeIf(List::isEmpty);

    return ImmutableList.copyOf(variants.stream().map(ImmutableList::copyOf).toList());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Converts a first name string into a list of tokens.
   * If forceInitials is true, breaks the string into individual letters.
   * Otherwise, tokenizes by whitespace and strips periods.
   *
   * @param name The full first name.
   * @param forceInitials Whether to parse as a series of initials.
   * @return A list of normalized tokens.
   */
  private static List<String> tokenize(String name, boolean forceInitials)
  {
    List<String> tokens = new ArrayList<>();

    for (String part : name.split("\\s+"))
    {
      if (part.isEmpty()) continue;

      if (forceInitials)
      {
        for (char c : part.toCharArray())
          if (Character.isLetter(c))
            tokens.add(String.valueOf(c));
      }
      else
      {
        tokens.add(part);
      }
    }

    return tokens;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns true if the normalized queryStr is a prefix of any of the
   * precomputed variant+lastName combinations.
   */
  public boolean startsWith(String queryStr)
  {
    if (strNullOrBlank(queryStr)) return false;

    String normalizedQueryStr = normalizeStrForTrie(queryStr);
    TrieNode trieNode = trieRoot;

    for (char c : normalizedQueryStr.toCharArray())
    {
      trieNode = trieNode.children.get(c);

      if (trieNode == null) return false;
    }

    return true; // we successfully walked the entire query
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void buildOmniSearchTrie()
  {
    // For each variant, build its token variants (full vs initial) and insert
    // every combo into the trie:

    for (List<String> nameVariant : variants)
    {
      if ((nameVariant == null) || (nameVariant.isEmpty())) continue;

      if (nameVariant.size() > 8) continue;  // Prevent running out of memory

      // Step 1: build variants per name piece

      List<List<String>> tokenVariants = new ArrayList<>();

      for (String token : nameVariant)
      {
        if (token == null) continue;

        String normalizedToken = normalizeStrForTrie(token);

        if (normalizedToken.isEmpty()) continue;

        if (normalizedToken.length() == 1)
        {
          // initial and absent

          tokenVariants.add(List.of(normalizedToken, ""));
        }
        else
        {
          // full form, initial, and absent

          tokenVariants.add(List.of(normalizedToken, normalizedToken.substring(0, 1), ""));
        }
      }

      if (tokenVariants.isEmpty()) continue;

      // Step 2: recurse to build every combination of those tokens

      buildNameVariantAndInsert(tokenVariants, new ArrayList<>());

      // Now, e.g. for Georg Wilhelm Friedrich Hegel, we add GWF Hegel and GWFH

      if (tokenVariants.size() >= 2)
      {
        TrieNode trieNode = trieRoot;

        for (List<String> tokenVariant : tokenVariants)
        {
          char c = tokenVariant.get(0).charAt(0);

          trieNode = trieNode.insertChar(c);
        }

        if (strNotNullOrEmpty(normalizedLastName))
        {
          trieNode.insertChar(normalizedLastName.charAt(0));

          trieNode.insertStr(' ' + normalizedLastName);
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // tokenVariantsForAllNames is a list of the person's names, the first, second, up to but not including last.
  // For each name, there is an inner list containing either just the initial or the spelled-out name and
  // initial, as well as a blank representing that that name part is absent, so the inner list always has either
  // 2 or 3 elements.

  // selectedVariantForAllNames is the selected variant for each of the person's names, built up through
  // recursive calls until we reach the second to last name, then we add all those names spelled out and
  // joined with spaces to the Trie.

  private void buildNameVariantAndInsert(List<List<String>> tokenVariantsForAllNames, List<String> selectedVariantForAllNames)
  {
    int idx = selectedVariantForAllNames.size();

    if (idx == tokenVariantsForAllNames.size())
    {
      // insert tokens + lastName into trie
      String firstNames = String.join(" ", selectedVariantForAllNames);

      trieRoot.insertStr(collapseSpaces(firstNames + ' ' + normalizedLastName).strip());

      // insert lastname, joined tokens into trie
      trieRoot.insertStr(collapseSpaces(normalizedLastName + ", " + firstNames).strip());

      return;
    }

    for (String token : tokenVariantsForAllNames.get(idx))
    {
      selectedVariantForAllNames.add(token);
      buildNameVariantAndInsert(tokenVariantsForAllNames, selectedVariantForAllNames);
      selectedVariantForAllNames.remove(selectedVariantForAllNames.size() - 1);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // normalization: strip punctuation, lowercase, collapse spaces

  private static String normalizeStrForTrie(String s)
  {
    return s.replaceAll("[.]", " ").toLowerCase().replaceAll("\\s+", " ").trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // simple trie node

  private static class TrieNode
  {
    private final Map<Character, TrieNode> children = new HashMap<>();

    private TrieNode insertChar(char c)
    {
      return children.computeIfAbsent(c, c_ -> new TrieNode());
    }

    private void insertStr(String s)
    {
      TrieNode trieNode = this;

      for (char c : s.toCharArray())
        trieNode = trieNode.insertChar(c);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
