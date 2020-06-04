/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.bib.zotero;

import com.github.scribejava.core.builder.ServiceBuilder;
import com.github.scribejava.core.builder.api.DefaultApi10a;
import com.github.scribejava.core.builder.api.OAuth1SignatureType;
import com.github.scribejava.core.model.OAuth1RequestToken;
import com.github.scribejava.core.model.OAuthConstants;
import com.github.scribejava.core.model.ParameterList;
import com.github.scribejava.core.oauth.OAuth10aService;
import com.github.scribejava.core.services.HMACSha1SignatureService;
import com.github.scribejava.core.services.SignatureService;

public class ZoteroOAuthApi extends DefaultApi10a
{
  private static OAuth10aService service = null;

  @Override public SignatureService getSignatureService() { return new HMACSha1SignatureService(); }
  @Override public OAuth1SignatureType getSignatureType() { return OAuth1SignatureType.HEADER; }
  @Override public String getAccessTokenEndpoint()        { return "https://www.zotero.org/oauth/access"; }
  @Override public String getRequestTokenEndpoint()       { return "https://www.zotero.org/oauth/request"; }
  @Override protected String getAuthorizationBaseUrl()    { return "https://www.zotero.org/oauth/authorize"; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class InstanceHolder     { private static final ZoteroOAuthApi INSTANCE = new ZoteroOAuthApi(); }
  public static ZoteroOAuthApi instance() { return InstanceHolder.INSTANCE; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static OAuth10aService service()
  {
    return service != null ?
      service
    :
      (service = new ServiceBuilder("e9d7bdc517f7cd37a455").apiSecret("89d44b5b0887f9c2a893").build(ZoteroOAuthApi.instance()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getAuthorizationUrl(OAuth1RequestToken requestToken)
  {
    ParameterList parameters = new ParameterList();
    parameters.add(OAuthConstants.TOKEN, requestToken.getToken());
    parameters.add("library_access", "1");
    parameters.add("write_access", "1");
    return parameters.appendTo(getAuthorizationBaseUrl());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
