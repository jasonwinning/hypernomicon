/*
 * Copyright 2015-2023 Jason Winning
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

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//---------------------------------------------------------------------------

public class BasicTextMatcher
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private enum TermModifier
  {
    none,
    required,
    exclude
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class Term
  {
    private final TermModifier modifier;
    private final String text;

  //---------------------------------------------------------------------------

    private Term(String newText, boolean caseSensitive)
    {
      if (caseSensitive == false)
        newText = newText.toLowerCase();

      if (newText.startsWith("+"))
      {
        modifier = TermModifier.required;
        newText = newText.substring(1);
      }
      else if (newText.startsWith("-"))
      {
        modifier = TermModifier.exclude;
        newText = newText.substring(1);
      }
      else
        modifier = TermModifier.none;

      this.text = newText.replace("\"", "");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<Term> terms = new ArrayList<>();
  private final boolean caseSensitive;

//---------------------------------------------------------------------------

  public BasicTextMatcher(String query, boolean caseSensitive)
  {
    this.caseSensitive = caseSensitive;

    Matcher m = Pattern.compile("(\\+?\\-?\\\"([^\"\\s]+ )+[^\"\\s]+\\\")|(\\+?\\-?\\\"?[^\"\\s]+\\\"?)").matcher(query);

    while (m.find())
      terms.add(new Term(m.group(), caseSensitive));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isMatch(String input)
  {
    if (caseSensitive == false)
      input = input.toLowerCase();

    boolean isMatch = false;

    for (Term term : terms)
    {
      switch (term.modifier)
      {
        case exclude :

          if (input.contains(term.text))
            return false;

          break;

        case required :

          if (input.contains(term.text) == false)
            return false;

          isMatch = true;
          break;

        default :

          if (input.contains(term.text))
            isMatch = true;

          break;
      }
    }

    return isMatch;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
