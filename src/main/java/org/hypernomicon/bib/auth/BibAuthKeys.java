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

package org.hypernomicon.bib.auth;

import static org.hypernomicon.App.app;

import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.mendeley.auth.MendeleyAuthKeys;
import org.hypernomicon.bib.zotero.auth.ZoteroAuthKeys;

//---------------------------------------------------------------------------

public abstract class BibAuthKeys
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected abstract boolean isEmpty();

  protected final boolean isNotEmpty() { return isEmpty() == false; }

  public static boolean isEmpty   (BibAuthKeys authKeys) { return (authKeys == null) || authKeys.isEmpty(); }

  public static boolean isNotEmpty(BibAuthKeys authKeys) { return (authKeys != null) && authKeys.isNotEmpty(); }

  protected abstract boolean saveToDBSettings() throws Exception;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibAuthKeys loadFromDBSettings(LibraryType libType) throws Exception
  {
    return switch (libType)
    {
      case ltMendeley -> MendeleyAuthKeys.loadFromDBSettings();
      case ltZotero   -> ZoteroAuthKeys  .loadFromDBSettings();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean saveToDBSettings(BibAuthKeys authKeys) throws Exception
  {
    assert(app.debugging);

    return isNotEmpty(authKeys) && authKeys.saveToDBSettings();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
