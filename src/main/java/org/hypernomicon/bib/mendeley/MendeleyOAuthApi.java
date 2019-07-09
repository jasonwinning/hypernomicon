/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.bib.mendeley;

import com.github.scribejava.core.builder.ServiceBuilder;
import com.github.scribejava.core.builder.api.DefaultApi20;
import com.github.scribejava.core.oauth.OAuth20Service;

public class MendeleyOAuthApi extends DefaultApi20
{
  private static OAuth20Service service = null;

  @Override public String getAccessTokenEndpoint()        { return "https://api.mendeley.com/oauth/token"; }
  @Override protected String getAuthorizationBaseUrl()    { return "https://api.mendeley.com/oauth/authorize"; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class InstanceHolder       { private static final MendeleyOAuthApi INSTANCE = new MendeleyOAuthApi(); }
  public static MendeleyOAuthApi instance() { return InstanceHolder.INSTANCE; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static OAuth20Service service()
  {
    if (service == null) service = new ServiceBuilder("7087")
      .apiSecret("ShGQ9F8yP9t3u3nR")
      .callback("http://hypernomicon.org/verification.html")
      .defaultScope("all")
      .build(MendeleyOAuthApi.instance());

    return service;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
