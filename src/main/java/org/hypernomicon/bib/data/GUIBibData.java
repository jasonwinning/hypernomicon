/*
 * Copyright 2015-2024 Jason Winning
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

public class GUIBibData extends BibDataStandalone
{
  public static final GUIBibData NoneFoundBD = new GUIBibData();

  private HDT_WorkType workType;

  public GUIBibData() { }

  public GUIBibData(BibData bd)
  {
    copyAllFieldsFrom(bd, true, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setWorkType(HDT_WorkType workType) { this.workType = workType; }
  @Override public HDT_WorkType getWorkType()              { return workType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean externalFieldsAreSame(GUIBibData bd1, GUIBibData bd2)
  {
    EnumSet<BibFieldEnum> set1 = bd1.fieldsWithExternalData(),
                          set2 = bd2.fieldsWithExternalData();

    if (set1.equals(set2) == false) return false;

    return set1.stream().allMatch(field -> bd1.fieldsAreEqual(field, bd2, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EnumSet<BibFieldEnum> fieldsWithExternalData()
  {
    EnumSet<BibFieldEnum> set = EnumSet.allOf(BibFieldEnum.class);

    set.removeIf(bibFieldEnum -> { switch (bibFieldEnum)
    {
      case bfAuthors   : case bfEditors  : case bfTranslators : case bfTitle:
      case bfDOI       : case bfISBNs    : case bfMisc        : case bfYear:
      case bfEntryType : case bfWorkType : case bfURL         :

        return true;

      default:

        return fieldNotEmpty(bibFieldEnum) == false;
    }});

    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
