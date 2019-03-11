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

package org.hypernomicon.bib;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;

import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.bib.BibData.BibFieldEnum.*;
import static org.hypernomicon.bib.BibData.BibFieldType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

public class BibDataStandalone extends BibData
{
  private EntryType entryType;
  private HDT_WorkType workType;
  private final LinkedHashSet<BibField> bibFields = new LinkedHashSet<>();
  private final HashMap<BibFieldEnum, BibField> bibFieldEnumToBibField = new HashMap<>();
  protected YearType yearType;      // Internally-used descriptor indicates where year field came from for purposes of determining priority
  private final BibAuthorsStandalone authors = new BibAuthorsStandalone();

  private static final EnumSet<BibFieldType> stringBibFieldTypes = EnumSet.of(bftString, bftMultiString);

  public BibDataStandalone()
  {
    entryType = EntryType.etUnentered;
    workType = null;

    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum ->
    {
      if (stringBibFieldTypes.contains(bibFieldEnumToType.get(bibFieldEnum)))
      {
        BibField bibField = new BibField(bibFieldEnum);
        bibFields.add(bibField);
        bibFieldEnumToBibField.put(bibFieldEnum, bibField);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibDataStandalone(BibData bd)
  {
    this();

    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum -> { switch (bibFieldEnumToType.get(bibFieldEnum))
    {
      case bftString      : setStr(bibFieldEnum, bd.getStr(bibFieldEnum)); break;
      case bftMultiString : setMultiStr(bibFieldEnum, bd.getMultiStr(bibFieldEnum)); break;
      case bftEntryType   : entryType = bd.getEntryType(); break;
      case bftWorkType    : workType = bd.getWorkType(); break;
      case bftAuthor      : break;
    }});

    bd.getAuthors().forEach(authors::add);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean linkedToWork()                                { return false; }
  @Override public HDT_Work getWork()                                    { return null; }
  @Override public BibAuthors getAuthors()                               { return authors; }
  @Override public EntryType getEntryType()                              { return entryType; }
  @Override public void setMultiStr(BibFieldEnum bfe, List<String> list) { bibFieldEnumToBibField.get(bfe).setAll(list); }
  @Override public void setEntryType(EntryType entryType)                { this.entryType = entryType; }
  @Override public void setWorkType(HDT_WorkType workType)               { this.workType = workType; }
  @Override public HDT_WorkType getWorkType()                            { return workType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setYear(String text, YearType yearType)
  {
    if (this.yearType != null)
      if (this.yearType.ordinal() > yearType.ordinal()) return;

    String year = extractYear(text);

    setStr(bfYear, year);
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
    if (bibFieldEnumToType.get(bibFieldEnum) == bftString)
      return bibFieldEnumToBibField.get(bibFieldEnum).getStr();

    switch (bibFieldEnum)
    {
      case bfEntryType :
        return BibUtils.getEntryTypeName(entryType);

      case bfWorkType :
        return workType == null ? "" : workType.getCBText();

      case bfContainerTitle: case bfMisc: case bfTitle:
        return bibFieldEnumToBibField.get(bibFieldEnum).getStr();

      case bfAuthors:     return authors.getStr(AuthorType.author);
      case bfEditors:     return authors.getStr(AuthorType.editor);
      case bfTranslators: return authors.getStr(AuthorType.translator);

      default:
        messageDialog("Internal error #90227", mtError);
        return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void addStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    if (bibFieldIsMultiLine(bibFieldEnum) == false)
    {
      messageDialog("Internal error #90228", mtError);
      return;
    }

    bibFieldEnumToBibField.get(bibFieldEnum).addStr(newStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
