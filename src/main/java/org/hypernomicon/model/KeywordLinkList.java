/*
 * Copyright 2015-2018 Jason Winning
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

  private final ArrayList<KeywordLink> keys = new ArrayList<KeywordLink>();
     
  public final List<KeywordLink> getLinks() { return Collections.unmodifiableList(keys); }
  public final void generate(String text)   { generate(text, false, null); }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void generate(String text, boolean overrideSet, SearchKeys searchKeysToUse)
  {
    int matchLen, ndx = 0, curMatchLen = 0;
    String prefix;
    SearchKeyword curKey;
    boolean addOK, checkPeriods = false;
    char c;
    ArrayList<Integer> posMap = new ArrayList<>();
    
    keys.clear();

    if (text.length() == 0) return;
    text = convertToEnglishCharsWithMap(text, posMap); // posMap maps output position (key) to input position (value)
    
    if (text.matches(".*[a-zA-Z][.][a-zA-Z].*"))
    {
      if (text.matches(".*[^a-zA-Z][a-zA-Z][.][a-zA-Z].*"))
        checkPeriods = true;
      else if (text.matches(".*^[a-zA-Z][.][a-zA-Z].*"))
        checkPeriods = true;
    }
             
    for (ndx = 0; ndx < text.length(); ndx++)
    {
      while (safeSubstring(text, ndx, ndx + 4).toLowerCase().equals("http")) // don't convert a URL (outside of anchor tag) to a link
      {
        while (charIsPartOfLink(text, ndx, true))
        {
          ndx++;
          if (ndx >= text.length()) break;
        }

        if (ndx >= text.length()) break;
      }

      while (safeSubstring(text, ndx, ndx + 4).toLowerCase().equals("href")) // don't convert anything in an anchor tag to a link
      {
        while (text.charAt(ndx) != '>')
        {
          ndx++;
          if (ndx >= text.length()) break;
        }

        if (ndx >= text.length()) break;
      }

      prefix = safeSubstring(text, ndx, ndx + 3);
      
      if (checkPeriods) // This happens less than 1 percent of the time
      {
        prefix = prefix.replace(".", ". ");
        
        while (prefix.contains("  "))
          prefix = prefix.replace("  ", " ");
        
        prefix = safeSubstring(prefix, 0, 3);
      }
      
      curKey = null;
      List<SearchKeyword> keys;
      
      if (overrideSet)
        keys = searchKeysToUse.getKeywordsByPrefix(prefix);
      else
        keys = db.getKeysByPrefix(prefix);

      for (SearchKeyword key : keys)
      {
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
          addOK = true;

          if (key.startOnly && (ndx > 0))
          {
            c = text.charAt(ndx - 1);
            if (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
              addOK = false;
          }

          if (key.endOnly && ((ndx + matchLen) < text.length()))
          {
            c = text.charAt(ndx + matchLen);
            if (((c >= 'a') && (c <= 'z')) || ((c >= 'A') && (c <= 'Z')))
              addOK = false;
          }
          
          if (addOK)
          {
            if ((curKey == null) || (matchLen > curKey.text.length())) 
            {
              curKey = key;
              curMatchLen = matchLen;
            }
          }
        }
      }

      if (curKey != null)
        ndx = add(text, ndx, curMatchLen, curKey, posMap);       
    }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public final static boolean charIsPartOfLink(String text, int ndx, boolean webLink)
  {
    char c = text.charAt(ndx);
    
    if (webLink)
      return (c != '\n') && (c != ' ') && (c != ',') && (c != ';');
    
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
  
  private final int add(String text, int ndx, int matchLen, SearchKeyword key, ArrayList<Integer> posMap)
  {
    int right = ndx + matchLen, replaceLen;

    if (right < text.length())
    {
      while (charIsPartOfLink(text, right, false))
      {
        right++;
        if (right >= text.length()) break;
      }
    }

    replaceLen = right - ndx;

    // The next two lines are for cases where a special character exists in the original html that translates to multiple plain-text characters, e.g., ellipsis
    int realNdx = posMap.get(ndx);
    int realLen = (posMap.get(ndx + replaceLen - 1) - realNdx) + 1;
    
    keys.add(new KeywordLink(realNdx, realLen, key));

    return ndx + replaceLen;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
