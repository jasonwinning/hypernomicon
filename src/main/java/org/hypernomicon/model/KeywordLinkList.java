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

package org.hypernomicon.model;

import java.util.*;
import java.util.function.Function;
import java.util.regex.Pattern;

import org.hypernomicon.model.SearchKeys.SearchKeyword;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;

//---------------------------------------------------------------------------

public final class KeywordLinkList
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public record KeywordLink(int offset, int length, SearchKeyword key) { }

//---------------------------------------------------------------------------

  private KeywordLinkList() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------

  private static final Pattern INITIALS_PATTERN     = Pattern.compile("[a-zA-Z]\\.[a-zA-Z]"),
                               MIDDLE_INITIALS_RULE = Pattern.compile(".*[^a-zA-Z][a-zA-Z]\\.[a-zA-Z].*"),
                               START_INITIALS_RULE  = Pattern.compile("^[a-zA-Z]\\.[a-zA-Z].*");

  public static List<KeywordLink> generate(String text)
  {
    return generate(text, db::getKeysByPrefix);
  }

  public static List<KeywordLink> generate(String text, Function<String, Iterable<SearchKeyword>> prefixToKeys)
  {
    List<KeywordLink> keywordLinks = new ArrayList<>();

    if (text.isEmpty()) return keywordLinks;

    List<Integer> posMap = new ArrayList<>();
    text = convertToEnglishCharsWithMap(text, posMap); // posMap maps output position (key) to input position (value)

    boolean checkPeriods = false;

    if (INITIALS_PATTERN.matcher(text).find())
      if (MIDDLE_INITIALS_RULE.matcher(text).matches()  ||
          START_INITIALS_RULE .matcher(text).matches())
        checkPeriods = true;

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

      SearchKeyword curKey = null;
      int curMatchLen = 0;

      for (SearchKeyword key : prefixToKeys.apply(prefix))
      {
        int matchLen;

        if (checkPeriods)
          matchLen = matchNormalizedLength(text, ndx, key.text);
        else if ((text.length() - ndx) >= key.text.length())
          matchLen = text.regionMatches(true, ndx, key.text, 0, key.text.length()) ? key.text.length() : -1;
        else
          matchLen = -1;

        if (matchLen > 0)
        {
          boolean addOK = true;

          if (key.startOnly && (ndx > 0))
          {
            char c = text.charAt(ndx - 1);
            if (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
              addOK = false;
          }

          if (key.endOnly && ((ndx + matchLen) < text.length()))
          {
            char c = text.charAt(ndx + matchLen);
            if (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
              addOK = false;
          }

          if (addOK && ((curKey == null) || (matchLen > curKey.text.length())))
          {
            curKey = key;
            curMatchLen = matchLen;
          }
        }
      }

      if (curKey != null)
        ndx = add(keywordLinks, text, ndx, curMatchLen, curKey, posMap);

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

  private static int add(Collection<KeywordLink> keywordLinks, CharSequence text, int ndx, int matchLen, SearchKeyword key, List<Integer> posMap)
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

    keywordLinks.add(new KeywordLink(realNdx, realLen, key));

    return ndx + replaceLen;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
