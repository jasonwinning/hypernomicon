/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.settings;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;

import java.util.List;

import org.hypernomicon.settings.WorkSearchKeySettings.FinalConjunctionSymbol;

//---------------------------------------------------------------------------

public class ArgumentNamingSettings
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final String truncationIndicator;
  public final boolean lowerCaseTargetNames, multipleAuthors, oxfordSeparator;
  public final FinalConjunctionSymbol finalConjSymbol;
  public final int authorNumToTruncate, authorsToShowWhenTruncating;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ArgumentNamingSettings(String truncationIndicator, boolean lowerCaseTargetNames, boolean multipleAuthors, boolean oxfordSeparator, FinalConjunctionSymbol finalConjSymbol, int authorNumToTruncate, int authorsToShowWhenTruncating)
  {
    this.truncationIndicator = truncationIndicator;
    this.lowerCaseTargetNames = lowerCaseTargetNames;
    this.multipleAuthors = multipleAuthors;
    this.oxfordSeparator = oxfordSeparator;
    this.finalConjSymbol = finalConjSymbol;
    this.authorNumToTruncate = authorNumToTruncate;
    this.authorsToShowWhenTruncating = authorsToShowWhenTruncating;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ArgumentNamingSettings()
  {
    lowerCaseTargetNames = db.prefs.getBoolean(PREF_KEY_LOWER_CASE_TARGET_NAMES    , false);
    multipleAuthors      = db.prefs.getBoolean(PREF_KEY_ARG_NAMING_MULTIPLE_AUTHORS, false);
    oxfordSeparator      = db.prefs.getBoolean(PREF_KEY_ARG_NAMING_OXFORD_COMMA    , true );

    truncationIndicator  = db.prefs.get(PREF_KEY_ARG_TRUNCATION_INDICATOR, " et al.");

    authorNumToTruncate         = db.prefs.getInt(PREF_KEY_ARG_TRUNCATE_NUM   , 4);
    authorsToShowWhenTruncating = db.prefs.getInt(PREF_KEY_ARG_AUTHORS_TO_SHOW, 2);

    finalConjSymbol = FinalConjunctionSymbol.fromPrefVal(db.prefs.get(PREF_KEY_ARG_FINAL_CONJ_SYMBOL, FinalConjunctionSymbol.and.prefVal));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveToPrefNode()
  {
    db.prefs.putBoolean(PREF_KEY_LOWER_CASE_TARGET_NAMES, lowerCaseTargetNames);
    db.prefs.putBoolean(PREF_KEY_ARG_NAMING_MULTIPLE_AUTHORS, multipleAuthors);
    db.prefs.putBoolean(PREF_KEY_ARG_NAMING_OXFORD_COMMA, oxfordSeparator);

    db.prefs.put(PREF_KEY_ARG_TRUNCATION_INDICATOR, truncationIndicator);

    db.prefs.putInt(PREF_KEY_ARG_TRUNCATE_NUM, authorNumToTruncate);
    db.prefs.putInt(PREF_KEY_ARG_AUTHORS_TO_SHOW, authorsToShowWhenTruncating);

    db.prefs.put(PREF_KEY_ARG_FINAL_CONJ_SYMBOL, finalConjSymbol.prefVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String format(List<String> authors)
  {
    StringBuilder str = new StringBuilder();

    WorkSearchKeySettings.WorkSearchKeyConfig.appendCitationAuthors(str, authors, truncationIndicator, multipleAuthors, oxfordSeparator, authorNumToTruncate, authorsToShowWhenTruncating, finalConjSymbol);

    return str.toString().replaceAll("  ", " ");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
