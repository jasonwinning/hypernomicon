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

package org.hypernomicon.bib.zotero.auth;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.bib.auth.BibAuthKeys;
import org.hypernomicon.util.CryptoUtil;

import com.github.scribejava.core.model.OAuth1AccessToken;

//---------------------------------------------------------------------------

public final class ZoteroAuthKeys extends BibAuthKeys
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String apiKey;

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

  @Override protected boolean saveToDBSettings() throws Exception
  {
    if (isEmpty())
      return false;

    db.prefs.put(PrefKey.BIB_API_KEY, CryptoUtil.encrypt("", apiKey));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ZoteroAuthKeys loadFromDBSettings() throws Exception
  {
    String bibEncApiKey = db.prefs.get(PrefKey.BIB_API_KEY, "");

    return new ZoteroAuthKeys(bibEncApiKey.isBlank() ? "" : CryptoUtil.decrypt("", bibEncApiKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ZoteroAuthKeys createFromOauthToken(OAuth1AccessToken oauth1Token)
  {
    return new ZoteroAuthKeys(oauth1Token.getTokenSecret());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
