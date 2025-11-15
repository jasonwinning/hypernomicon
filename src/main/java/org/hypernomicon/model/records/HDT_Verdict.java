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

package org.hypernomicon.model.records;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.items.Ternary;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;

//---------------------------------------------------------------------------

public abstract class HDT_Verdict extends HDT_RecordBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_Verdict(RecordState xmlState, DatasetAccessor<? extends HDT_Verdict> dataset)
  {
    super(xmlState, dataset);
  }

  /**
   * {@inheritDoc}
   */
  @Override public String listName()  { return shortName(); }

  public Ternary isInFavor()          { return getTagTernary(tagInFavor); }
  public String shortName()           { return getTagString(tagShortName); }

  private void setInFavor(Ternary newVal) { updateTagTernary(tagInFavor, newVal); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class HDT_PositionVerdict extends HDT_Verdict
  { public HDT_PositionVerdict(RecordState xmlState, DatasetAccessor<HDT_PositionVerdict> dataset) { super(xmlState, dataset);  } }

  public static final class HDT_ArgumentVerdict extends HDT_Verdict
  { public HDT_ArgumentVerdict(RecordState xmlState, DatasetAccessor<HDT_ArgumentVerdict> dataset) { super(xmlState, dataset);  } }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Backwards compatibility with records XML version 1.10
   */
  public static void doInFavorConversion()
  {
    setInFavorIfUnset(hdtPositionVerdict, 1, "True"          , true);
    setInFavorIfUnset(hdtPositionVerdict, 3, "Valuable"      , true);
    setInFavorIfUnset(hdtPositionVerdict, 6, "Reasonable"    , true);
    setInFavorIfUnset(hdtPositionVerdict, 17, "Morally right", true);

    setInFavorIfUnset(hdtPositionVerdict, 2, "False"           , false);
    setInFavorIfUnset(hdtPositionVerdict, 4, "Non-valuable"    , false);
    setInFavorIfUnset(hdtPositionVerdict, 5, "Unsupported"     , false);
    setInFavorIfUnset(hdtPositionVerdict, 7, "Bad methodology" , false);
    setInFavorIfUnset(hdtPositionVerdict, 8, "Untestable"      , false);
    setInFavorIfUnset(hdtPositionVerdict, 9, "Unknowable"      , false);
    setInFavorIfUnset(hdtPositionVerdict, 10, "Uninteresting"  , false);
    setInFavorIfUnset(hdtPositionVerdict, 11, "Inconsequential", false);
    setInFavorIfUnset(hdtPositionVerdict, 12, "Fallacious"     , false);
    setInFavorIfUnset(hdtPositionVerdict, 13, "Meaningless"    , false);
    setInFavorIfUnset(hdtPositionVerdict, 14, "Incoherent"     , false);
    setInFavorIfUnset(hdtPositionVerdict, 15, "Irrational"     , false);
    setInFavorIfUnset(hdtPositionVerdict, 16, "Too radical"    , false);
    setInFavorIfUnset(hdtPositionVerdict, 18, "Morally wrong"  , false);
    setInFavorIfUnset(hdtPositionVerdict, 19, "Incomplete"     , false);
    setInFavorIfUnset(hdtPositionVerdict, 20, "Absurd"         , false);

    setInFavorIfUnset(hdtArgumentVerdict, 108, "Arg succeeds", true);

    setInFavorIfUnset(hdtArgumentVerdict, 101, "Arg fails"         , false);
    setInFavorIfUnset(hdtArgumentVerdict, 102, "Arg misses point"  , false);
    setInFavorIfUnset(hdtArgumentVerdict, 103, "Arg is fallacious" , false);
    setInFavorIfUnset(hdtArgumentVerdict, 104, "Arg is unclear"    , false);
    setInFavorIfUnset(hdtArgumentVerdict, 105, "Arg is confused"   , false);
    setInFavorIfUnset(hdtArgumentVerdict, 106, "Arg is implausible", false);
    setInFavorIfUnset(hdtArgumentVerdict, 107, "Arg is absurd"     , false);

    // 109 is too ambiguous
    // setInFavorIfUnset(hdtArgumentVerdict, 109, "Arg nearly succeeds", false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void setInFavorIfUnset(RecordType type, int id, String shortName, boolean value)
  {
    assert(strNotNullOrBlank(shortName));
    assert((type == hdtPositionVerdict) || (type == hdtArgumentVerdict));

    HDT_Verdict verdict = (HDT_Verdict) db.records(type).getByID(id);

    if (HDT_Record.isEmpty(verdict, false)                             ||
        (shortName.equalsIgnoreCase(verdict.shortName()) == false)     ||
        (Ternary.isNullOrUnset(verdict.isInFavor()) == false)) return;

    verdict.setInFavor(value ? Ternary.True : Ternary.False);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
