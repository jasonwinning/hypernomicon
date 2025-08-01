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

import static org.hypernomicon.util.StringUtil.*;

//---------------------------------------------------------------------------

/**
 * All HDT_RecordBase objects hold a reference to an object of this kind.
 */
public class NameItem
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String name = "", nameEngChar = "";

//---------------------------------------------------------------------------

  public String get()        { return name; }
  public String getEngChar() { return nameEngChar; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void set(String newName)
  {
    name = safeStr(newName);
    nameEngChar = convertToEnglishChars(name.strip());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
