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
import java.util.Map;

import org.hypernomicon.util.boolEvaluator.BoolExpressionToken.TokenType;

public class BoolEvaluator
{
  private final Map<Integer, Boolean> varVals;
  private final Iterator<BoolExpressionToken> it;
  private final boolean result;
  private BoolExpressionToken tokenAtCursor;


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BoolEvaluator(BoolExpression expression, Map<Integer, Boolean> vals) throws ParseException
  {
    this.varVals = vals;

    it = expression.iterator();

    tokenAtCursor = it.next();

    result = evaluateParenableAtCursor();
    verifyTokenAtCursorAndAdvance(TokenType.END);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean evaluate(BoolExpression expression, Map<Integer, Boolean> vals) throws ParseException
  {
    return new BoolEvaluator(expression, vals).result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean evaluateParenableAtCursor() throws ParseException
  {
    boolean lhs = evaluateNottableAtCursor();

    switch (tokenAtCursor.type)
    {
      case AND :

        tokenAtCursor = it.next();
        return evaluateNottableAtCursor() && lhs;

      case OR :

        tokenAtCursor = it.next();
        return evaluateNottableAtCursor() || lhs;

      case RP : case END :

        return lhs;

      default :

        throw new ParseException("Expected boolean expression, found " + tokenAtCursor.toString(), tokenAtCursor.offset);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean evaluateNottableAtCursor() throws ParseException
  {
    switch (tokenAtCursor.type)
    {
      case NOT :

        tokenAtCursor = it.next();
        return evaluateNottableAtCursor() == false;

      case VAR :

        if (varVals.containsKey(tokenAtCursor.varNum) == false)
          throw new ParseException("Undefined variable: " + tokenAtCursor.varNum, tokenAtCursor.offset);

        boolean varVal = varVals.get(tokenAtCursor.varNum);
        tokenAtCursor = it.next();
        return varVal;

      case LP :

        tokenAtCursor = it.next();
        boolean exprVal = evaluateParenableAtCursor();
        verifyTokenAtCursorAndAdvance(TokenType.RP);
        return exprVal;

      default :

        throw new ParseException("Expected operand, found " + tokenAtCursor.toString(), tokenAtCursor.offset);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void verifyTokenAtCursorAndAdvance(TokenType tokenType) throws ParseException
  {
    if (tokenAtCursor.type != tokenType)
      throw new ParseException("Expected " + tokenType.str + ", found " + tokenAtCursor.toString(), tokenAtCursor.offset);

    if (tokenType != TokenType.END)
      tokenAtCursor = it.next();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
