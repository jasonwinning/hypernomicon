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

import java.util.EnumSet;

import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;

//---------------------------------------------------------------------------

public class GUIBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final GUIBibData NoneFoundBD = new GUIBibData();

  private HDT_WorkType workType;

//---------------------------------------------------------------------------

  public GUIBibData() { }

//---------------------------------------------------------------------------

  public GUIBibData(BibData bd)
  {
    copyAllFieldsFrom(bd, true, true);
  }

//---------------------------------------------------------------------------

  @Override public void setWorkType(HDT_WorkType workType) { this.workType = workType; }
  @Override public HDT_WorkType getWorkType()              { return workType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether two snapshots of dialog data agree on the fields that are at risk of being lost.
   * <p>
   * A work record stores only some bibliographic fields itself; the rest (publisher, journal
   * title, volume, and so on; see {@link BibFieldEnum#requiresBibEntry()}) can be kept only in
   * a linked reference manager entry. When a dialog has gathered such data for a work that has
   * no entry, the user is offered the chance to create one before the data is dropped (see
   * {@link org.hypernomicon.dialogs.WorkDlgCtrlr#promptToCreateBibEntry}). Once the user has
   * declined, the dialog does not ask again at OK unless the at-risk data differs from what it
   * was when the user declined; this comparison is how that is decided. Only the fields
   * {@link #fieldsWithExternalData()} counts take part: the fields the work stores itself are
   * never at risk, so changes to them are no reason to ask again, and neither is a change of
   * entry type, which is a choice about the entry to be created rather than information that
   * could be lost.
   * @param bd1 One snapshot, typically the dialog's current data
   * @param bd2 The other, typically the data as it stood when the user declined
   * @return True if both have values for the same entry-only fields and those values are equal
   */
  public static boolean externalFieldsAreSame(GUIBibData bd1, GUIBibData bd2)
  {
    EnumSet<BibFieldEnum> set1 = bd1.fieldsWithExternalData(),
                          set2 = bd2.fieldsWithExternalData();

    if (set1.equals(set2) == false) return false;

    return set1.stream().allMatch(field -> bd1.fieldsAreEqual(field, bd2, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The fields whose values would be lost if this data were saved to a work that has no
   * reference manager entry: those that only an entry can store (see
   * {@link BibFieldEnum#requiresBibEntry()}) and that currently hold a value.
   * <p>
   * This is the application's test for whether bibliographic data gathered in a dialog, whether
   * from an online lookup, a PDF's metadata, or a BibTeX or RIS file, needs a reference manager
   * entry to survive. With a library linked, a non-empty result is what prompts the user to
   * create an entry before the work is saved, and the field names in the result are what the
   * prompt lists (see {@link org.hypernomicon.dialogs.WorkDlgCtrlr#promptToCreateBibEntry}).
   * With no library linked there is no entry to offer, so the Merge Works dialog instead shows
   * such fields read-only, captioned as not saved.
   * <p>
   * Entry type is left out even though only an entry can store it. It is chosen through its own
   * control (the entry type selector that accompanies the create-entry check box) rather than
   * carried along as data, so it is neither listed as information at risk nor a reason to ask
   * again.
   * @return The at-risk fields; empty if saving without an entry would lose nothing
   */
  public EnumSet<BibFieldEnum> fieldsWithExternalData()
  {
    EnumSet<BibFieldEnum> set = EnumSet.allOf(BibFieldEnum.class);

    set.removeIf(bibFieldEnum -> (bibFieldEnum == bfEntryType) || (bibFieldEnum.requiresBibEntry() == false) || (fieldNotEmpty(bibFieldEnum) == false));

    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
