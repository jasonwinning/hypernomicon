/*
 * Copyright 2015-2021 Jason Winning
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

package org.hypernomicon.util;

import static com.google.common.net.HttpHeaders.*;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.apache.http.Header;

public enum HttpHeader
{
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  Accept(ACCEPT),
  Accept_Charset(ACCEPT_CHARSET),
  Accept_Encoding(ACCEPT_ENCODING),
  Accept_Language(ACCEPT_LANGUAGE),
  Accept_Ranges(ACCEPT_RANGES),
  Access_Control_Allow_Credentials(ACCESS_CONTROL_ALLOW_CREDENTIALS),
  Access_Control_Allow_Headers(ACCESS_CONTROL_ALLOW_HEADERS),
  Access_Control_Allow_Methods(ACCESS_CONTROL_ALLOW_METHODS),
  Access_Control_Allow_Origin(ACCESS_CONTROL_ALLOW_ORIGIN),
  Access_Control_Expose_Headers(ACCESS_CONTROL_EXPOSE_HEADERS),
  Access_Control_Max_Age(ACCESS_CONTROL_MAX_AGE),
  Access_Control_Request_Headers(ACCESS_CONTROL_REQUEST_HEADERS),
  Access_Control_Request_Method(ACCESS_CONTROL_REQUEST_METHOD),
  Age(AGE),
  Allow(ALLOW),
  Authorization(AUTHORIZATION),
  Cache_Control(CACHE_CONTROL),
  Connection(CONNECTION),
  Content_Disposition(CONTENT_DISPOSITION),
  Content_Encoding(CONTENT_ENCODING),
  Content_Language(CONTENT_LANGUAGE),
  Content_Length(CONTENT_LENGTH),
  Content_Location(CONTENT_LOCATION),
  Content_MD5(CONTENT_MD5),
  Content_Range(CONTENT_RANGE),
  Content_Security_Policy(CONTENT_SECURITY_POLICY),
  Content_Security_Policy_Report_Only(CONTENT_SECURITY_POLICY_REPORT_ONLY),
  Content_Type(CONTENT_TYPE),
  Cookie(COOKIE),
  D_N_T(DNT),
  Date(DATE),
  Dav(org.apache.http.HttpHeaders.DAV),
  Depth(org.apache.http.HttpHeaders.DEPTH),
  Destination(org.apache.http.HttpHeaders.DESTINATION),
  ETag(ETAG),
  Expect(EXPECT),
  Expires(EXPIRES),
  Follow_Only_When_Prerender_Shown(FOLLOW_ONLY_WHEN_PRERENDER_SHOWN),
  From(FROM),
  Host(HOST),
  If(org.apache.http.HttpHeaders.IF),
  If_Match(IF_MATCH),
  If_Modified_Since(IF_MODIFIED_SINCE),
  If_None_Match(IF_NONE_MATCH),
  If_Range(IF_RANGE),
  If_Unmodified_Since(IF_UNMODIFIED_SINCE),
  Last_Event_ID(LAST_EVENT_ID),
  Last_Modified(LAST_MODIFIED),
  Link(LINK),
  Location(LOCATION),
  Lock_Token(org.apache.http.HttpHeaders.LOCK_TOKEN),
  Max_Forwards(MAX_FORWARDS),
  Origin(ORIGIN),
  Overwrite(org.apache.http.HttpHeaders.OVERWRITE),
  P_3_P(P3P),
  Ping_From(PING_FROM),
  Ping_To(PING_TO),
  Pragma(PRAGMA),
  Proxy_Authenticate(PROXY_AUTHENTICATE),
  Proxy_Authorization(PROXY_AUTHORIZATION),
  Public_Key_Pins(PUBLIC_KEY_PINS),
  Public_Key_Pins_Report_Only(PUBLIC_KEY_PINS_REPORT_ONLY),
  Range(RANGE),
  Referer(REFERER),
  Refresh(REFRESH),
  Retry_After(RETRY_AFTER),
  Server(SERVER),
  Set_Cookie(SET_COOKIE),
  Set_Cookie2(SET_COOKIE2),
  Status_URI(org.apache.http.HttpHeaders.STATUS_URI),
  Strict_Transport_Security(STRICT_TRANSPORT_SECURITY),
  T_E(TE),
  Timeout(org.apache.http.HttpHeaders.TIMEOUT),
  Timing_Allow_Origin(TIMING_ALLOW_ORIGIN),
  Trailer(TRAILER),
  Transfer_Encoding(TRANSFER_ENCODING),
  Upgrade(UPGRADE),
  User_Agent(USER_AGENT),
  Vary(VARY),
  Via(VIA),
  WWW_Authenticate(WWW_AUTHENTICATE),
  Warning(WARNING),
  X_Content_Type_Options(X_CONTENT_TYPE_OPTIONS),
  X_Do_Not_Track(X_DO_NOT_TRACK),
  X_Forwarded_For(X_FORWARDED_FOR),
  X_Forwarded_Proto(X_FORWARDED_PROTO),
  X_Frame_Options(X_FRAME_OPTIONS),
  X_Powered_By(X_POWERED_BY),
  X_Requested_With(X_REQUESTED_WITH),
  X_User_IP(X_USER_IP),
  X_XSS_Protection(X_XSS_PROTECTION),
  None("None");
  
//---------------------------------------------------------------------------

  final private String name;
  private static Map<String, HttpHeader> map = new HashMap<>();

//---------------------------------------------------------------------------

  private HttpHeader(String name) { this.name = name; }

  @Override public String toString() { return name; }
  
//---------------------------------------------------------------------------

  static
  {
    EnumSet.allOf(HttpHeader.class).forEach(header -> map.put(header.name.toLowerCase(), header));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HttpHeader get(Header header) { return map.getOrDefault(header.getName().toLowerCase(), None); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
