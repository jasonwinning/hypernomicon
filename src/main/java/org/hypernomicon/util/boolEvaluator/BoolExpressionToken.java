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

package org.hypernomicon.util.boolEvaluator;

//---------------------------------------------------------------------------

class BoolExpressionToken
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  enum TokenType
  {
    VAR(""), LP("("), RP(")"), NOT("NOT"), AND("AND"), OR("OR"), END("<end>");

    TokenType(String str) { this.str = str; }

    final String str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final TokenType type;
  final int varNum, offset;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  BoolExpressionToken(TokenType type, int offset)
  {
    this.type = type;
    this.varNum = -1;
    this.offset = offset;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  BoolExpressionToken(int varNum, int offset)
  {
    this.type = TokenType.VAR;
    this.varNum = varNum;
    this.offset = offset;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString()
  {
    return type == TokenType.VAR ? String.valueOf(varNum) : type.str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
