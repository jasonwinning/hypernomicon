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

package org.hypernomicon.bib.zotero.auth;

import static org.hypernomicon.App.app;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.CryptoUtil.*;
import static org.hypernomicon.util.StringUtil.*;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.auth.BibAuthKeys;

import com.github.scribejava.core.model.OAuth1AccessToken;

//---------------------------------------------------------------------------

public final class ZoteroAuthKeys extends BibAuthKeys
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String apiKey;

  private boolean savedToKeyring = false;

//---------------------------------------------------------------------------

  private ZoteroAuthKeys(String apiKey)
  {
    this.apiKey = safeStr(apiKey);
  }

//---------------------------------------------------------------------------

  public static String getApiKey(ZoteroAuthKeys authKeys) { return authKeys == null ? "" : authKeys.apiKey; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isEmpty()
  {
    return strNullOrEmpty(apiKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Deprecated
  @Override protected void saveToDBSettings() throws Exception
  {
    if (isEmpty())
      return;

    assert(app.debugging);

    db.prefs.put(PrefKey.BIB_API_KEY, encrypt("", apiKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String secretNameForAPIKey(String userID)
  {
    return "Hypernomicon_ZoteroAPIKey_" + userID;
  }

//---------------------------------------------------------------------------

  public static ZoteroAuthKeys loadFromKeyring(String userID)
  {
    char[] tokenChars = readFromKeyring(secretNameForAPIKey(userID), getReadTaskMessage(LibraryType.ltZotero));

    ZoteroAuthKeys authKeys = new ZoteroAuthKeys(tokenChars == null ? "" : new String(tokenChars));

    authKeys.savedToKeyring = true;
    return authKeys;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Deprecated
  public static ZoteroAuthKeys loadFromDBSettings() throws Exception
  {
    String bibEncApiKey = db.prefs.get(PrefKey.BIB_API_KEY, "");

    return new ZoteroAuthKeys(bibEncApiKey.isBlank() ? "" : decrypt("", bibEncApiKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ZoteroAuthKeys createFromOauthToken(OAuth1AccessToken oauth1Token)
  {
    return new ZoteroAuthKeys(oauth1Token.getTokenSecret());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean stillNeedsToBeSavedToKeyring()
  {
    return (savedToKeyring == false) && isNotEmpty();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Save secrets to keyring only if it is not empty and they are not saved to the keyring yet
   * @return False if it is non-empty and saving wasn't successful; true otherwise
   */
  @Override protected boolean saveToKeyringIfUnsaved(String userID)
  {
    if (savedToKeyring || strNullOrBlank(userID) || isEmpty())
      return true;

    if (saveToKeyring(secretNameForAPIKey(userID), apiKey.toCharArray(), "API key for Hypernomicon Zotero integration, user " + userID, getWriteTaskMessage(LibraryType.ltZotero)))
    {
      db.prefs.remove(PrefKey.BIB_API_KEY);
      db.prefs.remove(PrefKey.BIB_ACCESS_TOKEN);
      db.prefs.remove(PrefKey.BIB_REFRESH_TOKEN);

      savedToKeyring = true;
    }

    return savedToKeyring;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void removeFromKeyring(String userID)
  {
    if (strNullOrBlank(userID))
      return;

    if (deleteFromKeyring(secretNameForAPIKey(userID), getWriteTaskMessage(LibraryType.ltZotero)))
    {
      savedToKeyring = false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
