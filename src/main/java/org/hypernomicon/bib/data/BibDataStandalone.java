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

package org.hypernomicon.bib.data;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.BibField.BibFieldType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.BibAuthorsStandalone;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.BibliographicDate.DateType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.BibField.BibFieldType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * <p>{@code BibDataStandalone} objects, like all {@code BibData} objects, represent bibliographic
 * data about a work. Unlike other classes that extend {@code BibData}, however, they are not
 * dependent on a separate object like a reference manager (e.g., Zotero) library entry JSON
 * object or a Hypernomicon work record to be what actually holds that data in memory.
 * </p>
 */
public abstract class BibDataStandalone extends BibData
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private EntryType entryType;
  private final Map<BibFieldEnum, BibField> bibFieldEnumToBibField = new EnumMap<>(BibFieldEnum.class);
  private BibliographicDate date = BibliographicDate.EMPTY_DATE;
  private DateType dateType;      // Internally-used descriptor indicates where date field came from for purposes of determining priority
  final BibAuthorsStandalone authors = new BibAuthorsStandalone();

  private static final EnumSet<BibFieldType> stringBibFieldTypes = EnumSet.of(bftString, bftMultiString);

//---------------------------------------------------------------------------

  BibDataStandalone()
  {
    entryType = EntryType.etUnentered;

    EnumSet.allOf(BibFieldEnum.class).stream().filter(bibFieldEnum -> stringBibFieldTypes.contains(bibFieldEnum.getType())).forEach(bibFieldEnum ->
      bibFieldEnumToBibField.put(bibFieldEnum, new BibField(bibFieldEnum)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final HDT_Work getWork()                                    { return null; }
  @Override public final BibAuthors getAuthors()                               { return authors; }
  @Override public final EntryType getEntryType()                              { return entryType; }
  @Override public final BibliographicDate getDate()                           { return date; }
  @Override public final void setMultiStr(BibFieldEnum bfe, List<String> list) { bibFieldEnumToBibField.get(bfe).setAll(list); }
  @Override protected final void setEntryType(EntryType entryType)             { this.entryType = entryType; }
  @Override public HDT_WorkType getWorkType()                                  { return EntryType.toWorkType(getEntryType()); }
  @Override protected void setWorkType(HDT_WorkType workType)                  { return; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Set the date for this bibliographic entry.
   * @param newDate The new date
   * @param newDateType What type of date the source says this is.
   * @param okToMerge If true, it will merge information from multiple date types as long as those dates don't have conflicting information.
   */
  void setDate(BibliographicDate newDate, DateType newDateType, boolean okToMerge)
  {
    if (BibliographicDate.isEmpty(newDate))
      newDate = BibliographicDate.EMPTY_DATE;

    if (okToMerge)
    {
      // Merge in information from newDate if it doesn't conflict at all with existing date

      boolean noMerge = (date.hasYear () && newDate.hasYear () && (Objects.equals(date.year , newDate.year ) == false))  ||
                        (date.hasMonth() && newDate.hasMonth() && (Objects.equals(date.month, newDate.month) == false))  ||
                        (date.hasDay  () && newDate.hasDay  () && (Objects.equals(date.day  , newDate.day  ) == false));

      if (!noMerge)
      {
        date = BibliographicDate.combine(date, newDate);

        if ((dateType == null) || (dateType.ordinal() <= newDateType.ordinal()))
          dateType = newDateType;

        return;
      }
    }

    if ((dateType == null) || (dateType.ordinal() <= newDateType.ordinal()))
    {
      date = newDate;

      dateType = newDateType;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    bibFieldEnumToBibField.get(bibFieldEnum).setStr(newStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDate(BibliographicDate newDate)
  {
    date = BibliographicDate.isEmpty(newDate) ? BibliographicDate.EMPTY_DATE : newDate;

    dateType = DateType.highestPriority();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<String> getMultiStr(BibFieldEnum bibFieldEnum)
  {
    BibField bibField = bibFieldEnumToBibField.get(bibFieldEnum);

    if (bibField.isMultiStr() == false)
    {
      internalErrorPopup(90226);
      return null;
    }

    return bibField.getMultiStr();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getStr(BibFieldEnum bibFieldEnum)
  {
    return (bibFieldEnum.getType() == bftString) ?
      bibFieldEnumToBibField.get(bibFieldEnum).getStr()
    :
      switch (bibFieldEnum)
      {
        case bfEntryType       -> entryType == null ? "" : entryType.getUserFriendlyName();
        case bfWorkType        -> nullSwitch(getWorkType(), "", HDT_WorkType::getCBText);   // fall through
        case bfContainerTitle,
             bfMisc,
             bfTitle           -> bibFieldEnumToBibField.get(bibFieldEnum).getStr();
        case bfDate            -> getDateRawStr();
        case bfAuthors         -> authors.getStr(AuthorType.author);
        case bfEditors         -> authors.getStr(AuthorType.editor);
        case bfTranslators     -> authors.getStr(AuthorType.translator);
        default                ->
        {
          internalErrorPopup(90227);
          yield null;
        }
      };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void addStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    if (bibFieldEnum.isMultiLine() == false)
    {
      internalErrorPopup(90228);
      return;
    }

    bibFieldEnumToBibField.get(bibFieldEnum).addStr(newStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setStartPage(String value)
  {
    BibField field = bibFieldEnumToBibField.get(bfPages);

    String[] arr = field.getStr().split("-");

    field.setStr(value + '-' + (arr.length > 1 ? arr[1] : ""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setEndPage(String value)
  {
    BibField field = bibFieldEnumToBibField.get(bfPages);

    String[] arr = field.getStr().split("-");

    field.setStr((arr.length > 0 ? arr[0] : "") + '-' + value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
