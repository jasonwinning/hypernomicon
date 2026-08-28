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

package org.hypernomicon.bib.data;

import static org.hypernomicon.Const.*;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class CrossrefBibDataTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Every Crossref query must carry the mailto parameter, which routes the
   *  request to Crossref's "polite" service pool and gives them a contact for
   *  the tool's operator. */
  @Test
  void mailtoRidesEveryQueryUrl()
  {
    String doiUrl = CrossrefBibData.getQueryUrl(null, null, null, true, "10.1234/abc123");

    assertEquals("https://api.crossref.org/works/10.1234/abc123?mailto=" + APP_CONTACT_EMAIL, doiUrl);

    String titleUrl = CrossrefBibData.getQueryUrl("Naming and Necessity", "1980", null, true, null);

    assertTrue(titleUrl.contains("query.title="), titleUrl);
    assertTrue(titleUrl.contains("&mailto=" + APP_CONTACT_EMAIL), titleUrl);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
