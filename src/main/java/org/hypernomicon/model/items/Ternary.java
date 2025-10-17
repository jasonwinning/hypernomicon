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

package org.hypernomicon.model.items;

//---------------------------------------------------------------------------

public enum Ternary
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Unset, False, True;

//---------------------------------------------------------------------------

  public boolean isTrue () { return this == True ; }
  public boolean isFalse() { return this == False; }
  public boolean isUnset() { return this == Unset; }

//---------------------------------------------------------------------------

  @Override public String toString() { return switch (this)
  {
    case False -> "False";
    case True  -> "True";
    case Unset -> "Unset";
  };}

//---------------------------------------------------------------------------

  public static boolean isNullOrUnset(Ternary val) { return (val == null) || (val == Unset); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
