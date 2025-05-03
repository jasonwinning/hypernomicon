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
import static org.hypernomicon.util.Util.*;

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

  protected abstract void saveToKeyringIfUnsaved(String userID);

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
   */
  public static void saveToKeyringIfUnsaved(BibAuthKeys authKeys, String userID)
  {
    if (strNotNullOrBlank(userID) && isNotEmpty(authKeys))
      authKeys.saveToKeyringIfUnsaved(userID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void removeFromKeyring(BibAuthKeys authKeys, String userID)
  {
    if (strNullOrBlank(userID))
    {
      System.out.println("Unable to remove auth keys from keyring: user ID is blank.");
      return;
    }

    if (authKeys == null)
    {
      System.out.println("Unable to remove auth keys from keyring: authKeys is null.");
      return;
    }

    authKeys.removeFromKeyring(userID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
