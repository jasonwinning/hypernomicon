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

package org.hypernomicon.bib.data;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.BibField.BibFieldType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.BibAuthorsStandalone;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.BibField.BibFieldType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

public abstract class BibDataStandalone extends BibData
{
  private EntryType entryType;
  private final Set<BibField> bibFields = new LinkedHashSet<>();
  private final Map<BibFieldEnum, BibField> bibFieldEnumToBibField = new HashMap<>();
  protected YearType yearType;      // Internally-used descriptor indicates where year field came from for purposes of determining priority
  final BibAuthorsStandalone authors = new BibAuthorsStandalone();

  private static final EnumSet<BibFieldType> stringBibFieldTypes = EnumSet.of(bftString, bftMultiString);

  public BibDataStandalone()
  {
    entryType = EntryType.etUnentered;
    setWorkType(null);

    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum ->
    {
      if (stringBibFieldTypes.contains(bibFieldEnum.getType()))
      {
        BibField bibField = new BibField(bibFieldEnum);
        bibFields.add(bibField);
        bibFieldEnumToBibField.put(bibFieldEnum, bibField);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean linkedToWork()                                { return false; }
  @Override public HDT_Work getWork()                                    { return null; }
  @Override public BibAuthors getAuthors()                               { return authors; }
  @Override public EntryType getEntryType()                              { return entryType; }
  @Override public void setMultiStr(BibFieldEnum bfe, List<String> list) { bibFieldEnumToBibField.get(bfe).setAll(list); }
  @Override public void setEntryType(EntryType entryType)                { this.entryType = entryType; }
  @Override public HDT_WorkType getWorkType()                            { return EntryType.toWorkType(getEntryType()); }
  @Override public void setWorkType(HDT_WorkType workType)               { return; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setYear(String text, YearType yearType)
  {
    if ((this.yearType != null) && (this.yearType.ordinal() > yearType.ordinal())) return;

    setStr(bfYear, extractYear(text));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    bibFieldEnumToBibField.get(bibFieldEnum).setStr(newStr);

    if (bibFieldEnum == bfYear)
      yearType = YearType.highestPriority();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<String> getMultiStr(BibFieldEnum bibFieldEnum)
  {
    BibField bibField = bibFieldEnumToBibField.get(bibFieldEnum);

    if (bibField.isMultiStr() == false)
    {
      messageDialog("Internal error #90226", mtError);
      return null;
    }

    return bibField.getMultiStr();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getStr(BibFieldEnum bibFieldEnum)
  {
    if (bibFieldEnum.getType() == bftString)
      return bibFieldEnumToBibField.get(bibFieldEnum).getStr();

    switch (bibFieldEnum)
    {
      case bfEntryType      : return entryType == null ? "" : entryType.getUserFriendlyName();
      case bfWorkType       : return nullSwitch(getWorkType(), "", HDT_WorkType::getCBText);
      case bfContainerTitle : // fall through
      case bfMisc           : // fall through
      case bfTitle          : return bibFieldEnumToBibField.get(bibFieldEnum).getStr();
      case bfAuthors        : return authors.getStr(AuthorType.author);
      case bfEditors        : return authors.getStr(AuthorType.editor);
      case bfTranslators    : return authors.getStr(AuthorType.translator);

      default:
        messageDialog("Internal error #90227", mtError);
        return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void addStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    if (bibFieldEnum.isMultiLine() == false)
    {
      messageDialog("Internal error #90228", mtError);
      return;
    }

    bibFieldEnumToBibField.get(bibFieldEnum).addStr(newStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setStartPage(String value)
  {
    BibField field = bibFieldEnumToBibField.get(bfPages);

    String[] arr = field.getStr().split("-");

    field.setStr(value + "-" + (arr.length > 1 ? arr[1] : ""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setEndPage(String value)
  {
    BibField field = bibFieldEnumToBibField.get(bfPages);

    String[] arr = field.getStr().split("-");

    field.setStr((arr.length > 0 ? arr[0] : "") + "-" + value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
