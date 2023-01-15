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

package org.hypernomicon.util.boolEvaluator;

import java.text.ParseException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;

import static org.hypernomicon.util.boolEvaluator.BoolExpressionToken.TokenType.*;

public class BoolExpression implements Iterable<BoolExpressionToken>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String input;
  private final Queue<BoolExpressionToken> tokens;
  private int pos = 0;

  @Override public Iterator<BoolExpressionToken> iterator() { return tokens.iterator(); }

  public static BoolExpression create(String string) throws ParseException  { return new BoolExpression(string); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BoolExpression(String input) throws ParseException
  {
    this.input = input.toLowerCase();

    BoolExpressionToken token = null;
    tokens = new LinkedList<>();

    do
    {
      token = getNext();
      tokens.add(token);
    }
    while (token.type != END);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BoolExpressionToken getNext() throws ParseException
  {
    while (pos < input.length() && Character.isWhitespace(input.charAt(pos)))
      ++pos;

    if (pos >= input.length())
      return new BoolExpressionToken(END, pos);

    int start = pos++;

    switch (input.charAt(start))
    {
      case '1' : case '2' : case '3' : case '4' : case '5' :
      case '6' : case '7' : case '8' : case '9' :

        while ((pos < input.length()) && Character.isDigit(input.charAt(pos)))
          ++pos;

        return new BoolExpressionToken(Integer.parseInt(input.substring(start, pos)), start);

      case '(' :

        return new BoolExpressionToken(LP, start);

      case ')' :

        return new BoolExpressionToken(RP, start);

      case 'n' :

        if (input.startsWith("ot", pos))
        {
          pos += 2;
          return new BoolExpressionToken(NOT, start);
        }

        break;

      case 'a' :

        if (input.startsWith("nd", pos))
        {
          pos += 2;
          return new BoolExpressionToken(AND, start);
        }

        break;

      case 'o' :

        if (input.startsWith("r", pos))
        {
          pos += 1;
          return new BoolExpressionToken(OR, start);
        }

        break;
    }

    throw new ParseException("Invalid character(s)", start);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
