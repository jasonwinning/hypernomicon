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

package org.hypernomicon.bib.mendeley.auth;

import static org.hypernomicon.App.app;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.CryptoUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.auth.BibAuthKeys;

import com.github.scribejava.core.model.OAuth2AccessToken;

//---------------------------------------------------------------------------

public final class MendeleyAuthKeys extends BibAuthKeys
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String accessToken, refreshToken;

  private boolean savedToKeyring = false;

//---------------------------------------------------------------------------

  private MendeleyAuthKeys(String accessToken, String refreshToken)
  {
    this.accessToken  = safeStr(accessToken);
    this.refreshToken = safeStr(refreshToken);
  }

//---------------------------------------------------------------------------

  public static String getAccessToken (MendeleyAuthKeys authKeys) { return authKeys == null ? "" : authKeys.accessToken; }
  public static String getRefreshToken(MendeleyAuthKeys authKeys) { return authKeys == null ? "" : authKeys.refreshToken; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isEmpty()
  {
    return strNullOrEmpty(accessToken) && strNullOrEmpty(refreshToken);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Deprecated
  @Override protected void saveToDBSettings() throws Exception
  {
    if (isEmpty())
      return;

    assert(app.debugging);

    db.prefs.put(PrefKey.BIB_ACCESS_TOKEN , encrypt("", accessToken ));
    db.prefs.put(PrefKey.BIB_REFRESH_TOKEN, encrypt("", refreshToken));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String secretNameForAccessToken (String userID) { return "Hypernomicon_MendeleyAccToken_" + userID; }
  private static String secretNameForRefreshToken(String userID) { return "Hypernomicon_MendeleyRefToken_" + userID; }

//---------------------------------------------------------------------------

  public static MendeleyAuthKeys loadFromKeyring(String userID)
  {
    String taskMessage = getReadTaskMessage(LibraryType.ltMendeley);

    char[] accessTokenChars  = readFromKeyring(secretNameForAccessToken (userID), taskMessage);
    char[] refreshTokenChars = accessTokenChars == null ? null : readFromKeyring(secretNameForRefreshToken(userID), taskMessage);

    MendeleyAuthKeys authKeys = new MendeleyAuthKeys(accessTokenChars  == null ? "" : new String(accessTokenChars ),
                                                     refreshTokenChars == null ? "" : new String(refreshTokenChars));

    authKeys.savedToKeyring = true;  // Set savedToKeyring to true even if empty so that this authKeys object
    return authKeys;                 // is not treated like it came from somewhere other than the keyring
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Deprecated
  public static MendeleyAuthKeys loadFromDBSettings() throws Exception
  {
    String bibEncAccessToken  = db.prefs.get(PrefKey.BIB_ACCESS_TOKEN , ""),
           bibEncRefreshToken = db.prefs.get(PrefKey.BIB_REFRESH_TOKEN, "");

    String accessToken  = bibEncAccessToken .isBlank() ? "" : decrypt("", bibEncAccessToken ),
           refreshToken = bibEncRefreshToken.isBlank() ? "" : decrypt("", bibEncRefreshToken);

    return new MendeleyAuthKeys(accessToken, refreshToken);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MendeleyAuthKeys createFromOauthToken(OAuth2AccessToken oauth2Token)
  {
    return new MendeleyAuthKeys(oauth2Token.getAccessToken(), oauth2Token.getRefreshToken());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void saveToKeyringIfUnsaved(String userID)
  {
    if (savedToKeyring || strNullOrBlank(userID) || isEmpty())
      return;

    if (saveToKeyring(secretNameForAccessToken (userID), accessToken .toCharArray(), "Access token for Hypernomicon Mendeley integration, user "  + userID, getWriteTaskMessage(LibraryType.ltMendeley)) &&
        saveToKeyring(secretNameForRefreshToken(userID), refreshToken.toCharArray(), "Refresh token for Hypernomicon Mendeley integration, user " + userID, getWriteTaskMessage(LibraryType.ltMendeley)))
    {
      db.prefs.remove(PrefKey.BIB_API_KEY);
      db.prefs.remove(PrefKey.BIB_ACCESS_TOKEN);
      db.prefs.remove(PrefKey.BIB_REFRESH_TOKEN);

      savedToKeyring = true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void removeFromKeyring(String userID)
  {
    if (strNullOrBlank(userID))
      return;

    if (deleteFromKeyring(secretNameForAccessToken (userID), getWriteTaskMessage(LibraryType.ltMendeley)) &&
        deleteFromKeyring(secretNameForRefreshToken(userID), getWriteTaskMessage(LibraryType.ltMendeley)))
    {
      savedToKeyring = false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
