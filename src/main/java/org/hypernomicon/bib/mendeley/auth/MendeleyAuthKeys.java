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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.bib.auth.BibAuthKeys;
import org.hypernomicon.util.CryptoUtil;

import com.github.scribejava.core.model.OAuth2AccessToken;

//---------------------------------------------------------------------------

public final class MendeleyAuthKeys extends BibAuthKeys
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String accessToken, refreshToken;

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

  @Override protected boolean saveToDBSettings() throws Exception
  {
    if (isEmpty())
      return false;

    db.prefs.put(PrefKey.BIB_ACCESS_TOKEN , CryptoUtil.encrypt("", accessToken ));
    db.prefs.put(PrefKey.BIB_REFRESH_TOKEN, CryptoUtil.encrypt("", refreshToken));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MendeleyAuthKeys loadFromDBSettings() throws Exception
  {
    String bibEncAccessToken  = db.prefs.get(PrefKey.BIB_ACCESS_TOKEN , ""),
           bibEncRefreshToken = db.prefs.get(PrefKey.BIB_REFRESH_TOKEN, "");

    String accessToken  = bibEncAccessToken .isBlank() ? "" : CryptoUtil.decrypt("", bibEncAccessToken ),
           refreshToken = bibEncRefreshToken.isBlank() ? "" : CryptoUtil.decrypt("", bibEncRefreshToken);

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

}
