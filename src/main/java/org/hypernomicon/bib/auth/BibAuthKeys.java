/*
 * Copyright 2015-2026 Jason Winning
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
import static org.hypernomicon.util.StringUtil.*;

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

  @Deprecated
  protected abstract void saveToDBSettings() throws Exception;

  protected abstract boolean saveToKeyringIfUnsaved(String userID);

  protected abstract boolean stillNeedsToBeSavedToKeyring();

  protected abstract void removeFromKeyring(String userID);

//---------------------------------------------------------------------------

  protected static String getReadTaskMessage (LibraryType libType) { return "Loading " + libType.userFriendlyName + " configuration..."; }
  protected static String getWriteTaskMessage(LibraryType libType) { return "Saving "  + libType.userFriendlyName + " configuration..."; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Deprecated
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

  @Deprecated
  public static void saveToDBSettings(BibAuthKeys authKeys) throws Exception
  {
    assert(app.debugging);

    if (isNotEmpty(authKeys))
      authKeys.saveToDBSettings();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Only saves the authKeys to the keyring if authKeys is not null or empty, userID is not null or blank, and the authKeys have not been saved to the keyring yet.
   * @param authKeys Can be null
   * @param userID Can be null
   * @return False if it is non-empty and saving wasn't successful; true otherwise
   */
  public static boolean saveToKeyringIfUnsaved(BibAuthKeys authKeys, String userID)
  {
    return strNullOrBlank(userID) || isEmpty(authKeys) || authKeys.saveToKeyringIfUnsaved(userID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean stillNeedsToBeSavedToKeyring(BibAuthKeys authKeys)
  {
    return isNotEmpty(authKeys) && authKeys.stillNeedsToBeSavedToKeyring();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void removeFromKeyring(BibAuthKeys authKeys, String userID)
  {
    if (strNotNullOrBlank(userID) && isNotEmpty(authKeys))
      authKeys.removeFromKeyring(userID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
