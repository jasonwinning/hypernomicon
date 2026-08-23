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

package org.hypernomicon.bib.mendeley;

import static org.junit.jupiter.api.Assertions.*;

import java.util.prefs.Preferences;

import static org.hypernomicon.util.StringUtil.*;

import org.hypernomicon.App;
import org.hypernomicon.Const.PrefKey;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;

//---------------------------------------------------------------------------

class MendeleyMetadataTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Checks the document types against the Mendeley server, so it runs only when online tests
   * are enabled with {@code HN_ONLINE_TESTS=true}, and then only if a unit-test user is
   * configured, since the call needs that user's access token.
   */
  @Test
  @EnabledIfEnvironmentVariable(named = "HN_ONLINE_TESTS", matches = "true")
  void documentTypesTest()
  {
    Preferences appPrefs = Preferences.userNodeForPackage(App.class);

    // userID is needed to load the AuthKeys from secure storage because
    // the Mendeley server call requires an access token

    String userID = appPrefs.get(PrefKey.BIB_UNIT_TEST_USER_ID, "");

    Assumptions.assumeTrue(strNotNullOrBlank(userID));

    MendeleyWrapper mendeleyWrapper = assertDoesNotThrow(() -> MendeleyWrapper.createForTesting(userID));

    String errorMsg = mendeleyWrapper.checkDocumentTypesFromServer();

    assertTrue(strNullOrBlank(errorMsg), errorMsg);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
