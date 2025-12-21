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

package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import java.util.prefs.Preferences;

import static org.hypernomicon.util.StringUtil.*;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.bib.mendeley.MendeleyWrapper;

import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class MendeleyMetadataTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
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
