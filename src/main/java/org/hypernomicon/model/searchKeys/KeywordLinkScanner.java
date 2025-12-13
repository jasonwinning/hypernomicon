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

package org.hypernomicon.model.searchKeys;

import java.util.*;
import java.util.function.Function;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;

//---------------------------------------------------------------------------

/**
 * Scans raw prose text to identify keywords and produce {@link KeywordLink} objects
 * representing linkable spans. This class performs the initial parsing and analysis
 * of text, applying keyword binding rules and boundary checks, but does not generate
 * markup itself.
 *
 * <p>Responsibilities include:
 * <ul>
 *   <li>Scanning text character by character to detect keyword prefixes.</li>
 *   <li>Applying binding rules (start-only, end-only) and longest-match logic
 *       to resolve overlapping keywords.</li>
 *   <li>Extending matches to consume entire tokens (whole words or hyphenated compounds).</li>
 *   <li>Mapping normalized text positions back to original source indices
 *       for accurate link placement.</li>
 * </ul>
 *
 * <p>The result is a list of {@link KeywordLink} objects that downstream components
 * can use to insert hyperlinks into rendered text.</p>
 */
public final class KeywordLinkScanner
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private KeywordLinkScanner() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------

  public static List<KeywordLink> scan(String text)
  {
    return scan(text, db::getKeywordsByPrefix);
  }

  public static List<KeywordLink> scan(String text, Function<String, Iterable<Keyword>> prefixToKeys)
  {
    List<KeywordLink> keywordLinks = new ArrayList<>();

    if (text.isEmpty()) return keywordLinks;

    ArrayList<Integer> posMap = new ArrayList<>();
    text = convertToEnglishCharsWithMap(text, posMap);  // posMap maps output position (key) to input position (value)

    boolean checkPeriods = KeywordBinding.needsPeriodNormalization(text);

    int ndx = 0;

    while (ndx < text.length())
    {
      if (toLowerAscii(text.charAt(ndx)) == 'h')
      {
        String fourChars = safeSubstring(text, ndx, ndx + 4);

        if ("http".equalsIgnoreCase(fourChars))
        {
          for (; (ndx < text.length()) && charIsPartOfWebLink(text, ndx); ndx++);
          continue;
        }

        if ("href".equalsIgnoreCase(fourChars)) // don't convert anything in an anchor tag to a link
        {
          for (; (ndx < text.length()) && (text.charAt(ndx) != '>'); ndx++);
          continue;
        }
      }

      String prefix = safeSubstring(text, ndx, ndx + 3);

      if (checkPeriods) // This happens less than 1 percent of the time
      {
        prefix = prefix.replace(".", ". ");
        prefix = collapseSpaces(prefix);  // remove duplicate spaces

        prefix = safeSubstring(prefix, 0, 3);
      }

      Keyword curKey = null;
      KeywordBinding curBinding = null;
      Set<KeywordBinding> curBindings = null;
      int curMatchLen = 0;

      for (Keyword key : prefixToKeys.apply(prefix))
      {
        int matchLen;

        if (checkPeriods)
          matchLen = matchNormalizedLength(text, ndx, key.normalizedText);
        else if ((text.length() - ndx) >= key.normalizedText.length())
          matchLen = text.regionMatches(true, ndx, key.normalizedText, 0, key.normalizedText.length()) ? key.normalizedText.length() : -1;
        else
          matchLen = -1;

        if (matchLen > 0)
        {
          for (KeywordBinding binding : key.getAllBindings())
          {
            boolean addOK = true;

            if (binding.isStartOnly() && (ndx > 0))
            {
              char c = text.charAt(ndx - 1);
              if (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
                addOK = false;
            }

            if (binding.isEndOnly() && ((ndx + matchLen) < text.length()))
            {
              char c = text.charAt(ndx + matchLen);
              if (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
                addOK = false;
            }

            if (addOK)
            {
              if (key == curKey)
              {
                if (curBindings == null)
                {
                  curBindings = new HashSet<>();
                  curBindings.add(curBinding);
                  curBinding = null;
                }

                curBindings.add(binding);
                curMatchLen = Math.max(matchLen, curMatchLen);
              }
              else if ((curKey == null) || (matchLen > curMatchLen))
              {
                curBinding = binding;
                curBindings = null;
                curKey = key;
                curMatchLen = matchLen;
              }
            }
          }
        }
      }

      if (curKey != null)
        ndx = add(keywordLinks, text, ndx, curMatchLen, curBinding, curBindings, posMap);
      else
        ndx++;
    }

    return keywordLinks;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Try to match keyword pattern against input text starting at offset,
   * treating:
   *   '.' + any # of spaces as ". "
   *   runs of >=2 spaces as one space
   *
   * @param text             the full source string
   * @param textStartOffset  where in <code>input</code> to start matching
   * @param keyword          the keyword text to match (exact chars + single spaces)
   * @return                 the number of input chars consumed on success, or –1 if no match
   */
  private static int matchNormalizedLength(CharSequence text, int textStartOffset, CharSequence keyword)
  {
    int textOffset = textStartOffset,
        textLength = text.length(),
        keywordOffset = 0,
        keywordLength = keyword.length();

    while ((textOffset < textLength) && (keywordOffset < keywordLength))
    {
      char textChar = text.charAt(textOffset),
           keywordChar = keyword.charAt(keywordOffset);

      // 1) Dot + spaces normalization

      if ((textChar == '.') && (keywordChar == '.'))
      {
        textOffset++;
        keywordOffset++;

        // Skip ALL spaces in input text

        while ((textOffset < textLength) && (text.charAt(textOffset) == ' '))
          textOffset++;

        // Skip *one* space in the keyword; it is already normalized

        if (keywordOffset < keywordLength)
          keywordOffset++;

        continue;
      }

      // 2) Multi-space collapse

      if ((textChar == ' ') && (keywordChar == ' '))
      {
        textOffset++;
        keywordOffset++;

        // Skip extra spaces in input
        while ((textOffset < textLength) && (text.charAt(textOffset) == ' '))
          textOffset++;

        // Keyword should not have extra spaces

        continue;
      }

      if (toLowerAscii(textChar) != toLowerAscii(keywordChar))
        return -1;

      textOffset++;
      keywordOffset++;
    }

    // Success only if we've consumed the entire keyword pattern

    return keywordOffset == keywordLength ? (textOffset - textStartOffset) : -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determines whether the character at the specified index should be considered
   * part of a keyword link during text scanning.
   *
   * <p>This method is used by the keyword matching logic to decide whether a
   * matched keyword should be extended forward to consume additional characters
   * as part of the same link. The rules are:
   * <ul>
   *   <li>Alphabetic characters (A–Z, a–z) are always considered part of a link.</li>
   *   <li>A hyphen ('-') is considered part of a link only if it is followed by
   *       another alphabetic character. In that case, the following letter is
   *       treated as the effective character for the check.</li>
   *   <li>All other characters (digits, punctuation, whitespace, end of string)
   *       terminate the link.</li>
   * </ul>
   *
   * <p>By enforcing these rules, links naturally extend to cover entire words in
   * English prose (including hyphenated compounds), while stopping cleanly at
   * punctuation or non-letter boundaries.</p>
   *
   * @param text the character sequence being scanned
   * @param ndx the index of the character to evaluate
   * @return {@code true} if the character at {@code ndx} is part of a keyword link
   *         according to the rules above; {@code false} otherwise
   */
  private static boolean charIsPartOfKeywordLink(CharSequence text, int ndx)
  {
    char c = text.charAt(ndx);

    if (c == '-')
    {
      if ((ndx + 1) >= text.length()) return false;
      c = text.charAt(ndx + 1);
    }

    return ((c >= 'A') && (c <= 'Z')) ||
           ((c >= 'a') && (c <= 'z'));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Adds a new {@link KeywordLink} to the collection based on a matched keyword span
   * in normalized text, extending the match to consume the entire token and mapping
   * positions back to the original source.
   *
   * <p>The return value advances the scanning index to the first character after the
   * consumed token, allowing the caller to continue scanning without re-examining
   * boundary characters.</p>
   *
   * @param keywordLinks the collection to which the new {@link KeywordLink} is added
   * @param text the normalized text being scanned
   * @param ndx the starting index of the keyword match in {@code text}
   * @param matchLen the length of the initial keyword match before extension
   * @param binding the single binding associated with the keyword, or {@code null}
   *                if multiple bindings apply
   * @param bindings the collection of bindings associated with the keyword, used
   *                 when {@code binding} is {@code null}
   * @param posMap a mapping from normalized text indices to original source indices,
   *               used to compute correct offsets and lengths
   * @return the index of the first character after the consumed token, to be used
   *         as the next scanning position
   */
  private static int add(Collection<KeywordLink> keywordLinks, CharSequence text, int ndx, int matchLen, KeywordBinding binding, Collection<KeywordBinding> bindings, List<Integer> posMap)
  {
    int right = ndx + matchLen;

    if (right < text.length())
      while (charIsPartOfKeywordLink(text, right))
        if (++right >= text.length())
          break;

    int replaceLen = right - ndx;

    // The next two lines are for cases where a special character exists in the original html that translates to multiple plain-text characters, e.g., ellipsis

    int realNdx = posMap.get(ndx),
        realLen = (posMap.get(ndx + replaceLen - 1) - realNdx) + 1;

    keywordLinks.add(binding == null ? new KeywordLink(realNdx, realLen, bindings) : new KeywordLink(realNdx, realLen, binding));

    return ndx + replaceLen;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
