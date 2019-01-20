/*
 * Copyright 2015-2019 Jason Winning
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.hypernomicon.model.SearchKeys.SearchKeyword;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;

public final class KeywordLinkList
{
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class KeywordLink
  {
    public final int offset, length;
    public final SearchKeyword key;

  //---------------------------------------------------------------------------

    public KeywordLink(int offset, int length, SearchKeyword key)
    {
      this.offset = offset;
      this.length = length;
      this.key = key;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final ArrayList<KeywordLink> keys = new ArrayList<>();

  public List<KeywordLink> getLinks() { return Collections.unmodifiableList(keys); }
  public void generate(String text)   { generate(text, false, null); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void generate(String text, boolean overrideSet, SearchKeys searchKeysToUse)
  {
    keys.clear();

    if (text.length() == 0) return;

    ArrayList<Integer> posMap = new ArrayList<>();
    text = convertToEnglishCharsWithMap(text, posMap); // posMap maps output position (key) to input position (value)

    boolean checkPeriods = false;

    if (text.matches(".*[a-zA-Z][.][a-zA-Z].*"))
    {
      if (text.matches(".*[^a-zA-Z][a-zA-Z][.][a-zA-Z].*"))
        checkPeriods = true;
      else if (text.matches(".*^[a-zA-Z][.][a-zA-Z].*"))
        checkPeriods = true;
    }

    int ndx = 0;

    while (ndx < text.length())
    {
      if (safeSubstring(text, ndx, ndx + 4).toLowerCase().equals("http"))
      {
        for (; (ndx < text.length()) && charIsPartOfWebLink(text, ndx); ndx++);
        continue;
      }
      else if (safeSubstring(text, ndx, ndx + 4).toLowerCase().equals("href")) // don't convert anything in an anchor tag to a link
      {
        for (; (ndx < text.length()) && text.charAt(ndx) != '>'; ndx++);
        continue;
      }

      String prefix = safeSubstring(text, ndx, ndx + 3);

      if (checkPeriods) // This happens less than 1 percent of the time
      {
        prefix = prefix.replace(".", ". ");

        while (prefix.contains("  "))
          prefix = prefix.replace("  ", " ");

        prefix = safeSubstring(prefix, 0, 3);
      }

      SearchKeyword curKey = null;
      List<SearchKeyword> keys;
      int curMatchLen = 0;

      if (overrideSet)
        keys = searchKeysToUse.getKeywordsByPrefix(prefix);
      else
        keys = db.getKeysByPrefix(prefix);

      for (SearchKeyword key : keys)
      {
        int matchLen;
        String focusStr = safeSubstring(text, ndx, ndx + key.text.length());

        if (checkPeriods) // This happens less than 1 percent of the time
        {
          matchLen = focusStr.length();
          focusStr = focusStr.replace(".", ". ");

          while (focusStr.contains("  "))
            focusStr = focusStr.replace("  ", " ");

          matchLen = key.text.length() - (focusStr.length() - matchLen);
          focusStr = safeSubstring(focusStr, 0, key.text.length());
        }
        else
          matchLen = key.text.length();

        if (focusStr.equalsIgnoreCase(key.text))
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
        ndx = add(text, ndx, curMatchLen, curKey, posMap);

      ndx++;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean charIsPartOfWebLink(String text, int ndx)
  {
    char c = text.charAt(ndx);

    return (c != '\n') && (c != ' ') && (c != ',') && (c != ';');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean charIsPartOfKeywordLink(String text, int ndx)
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

  private int add(String text, int ndx, int matchLen, SearchKeyword key, ArrayList<Integer> posMap)
  {
    int right = ndx + matchLen, replaceLen;

    if (right < text.length())
      while (charIsPartOfKeywordLink(text, right))
        if (++right >= text.length())
          break;

    replaceLen = right - ndx;

    // The next two lines are for cases where a special character exists in the original html that translates to multiple plain-text characters, e.g., ellipsis
    int realNdx = posMap.get(ndx),
        realLen = (posMap.get(ndx + replaceLen - 1) - realNdx) + 1;

    keys.add(new KeywordLink(realNdx, realLen, key));

    return ndx + replaceLen;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
