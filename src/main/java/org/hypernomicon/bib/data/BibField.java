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

import java.util.*;

import org.apache.commons.lang3.StringUtils;

import static org.hypernomicon.bib.data.BibField.BibFieldType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class BibField
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum BibFieldEnum
  {
    // MergeWorksDlgCtrlr.init assumes that bfPublisher comes after bfPubLoc

    bfEntryType      ("Entry Type"        , bftEntryType),
    bfWorkType       ("Work Type"         , bftWorkType),
    bfAuthors        ("Authors"           , bftAuthor),
    bfContainerTitle ("Container Title"   , bftMultiString), // For articles this is the journal title; for chapters it is the book title
    bfDOI            ("DOI"               , bftString),      // Document Object ID only, without "doi:" or url
    bfEdition        ("Edition"           , bftString),      // Information about publication edition
    bfEditors        ("Editors"           , bftAuthor),
    bfISBNs          ("ISBNs"             , bftMultiString), // International standard book numbers
    bfISSNs          ("ISSNs"             , bftMultiString), // International standard serial numbers
    bfVolume         ("Volume"            , bftString),      // Volume number
    bfIssue          ("Issue"             , bftString),      // Issue number
    bfLanguage       ("Language"          , bftString),      // Language
    bfMisc           ("Misc. Information" , bftMultiString), // Holds uncategorized information
    bfPages          ("Page Numbers"      , bftString),      // Page range
    bfPubLoc         ("Publisher Location", bftString),      // Where published
    bfPublisher      ("Publisher"         , bftString),      // May or may not include city
    bfTitle          ("Title"             , bftMultiString), // Title of this work
    bfTranslators    ("Translators"       , bftAuthor),
    bfURL            ("URL"               , bftString),      // URL where this work can be found
    bfDate           ("Date"              , bftBibDate);     // Main date to show in bibliography (usually publication date)

    private final BibFieldType type;
    private final String userFriendlyName;

    BibFieldEnum(String userFriendlyName, BibFieldType type)
    {
      this.type = type;
      this.userFriendlyName = userFriendlyName;
    }

    public BibFieldType getType()       { return type; }
    public String getUserFriendlyName() { return userFriendlyName; }
    public boolean isMultiLine()        { return type == bftMultiString; }
  }

  public enum BibFieldType { bftString, bftMultiString, bftEntryType, bftWorkType, bftAuthor, bftBibDate }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final BibFieldEnum bibFieldEnum;
  private final BibFieldType type;
  private final List<String> strList = new ArrayList<>();

  private String str;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibField(BibFieldEnum bibFieldEnum)
  {
    this.bibFieldEnum = bibFieldEnum;

    type = bibFieldEnum.type;
  }

  public boolean isMultiStr() { return type == bftMultiString; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setStr(String newStr)
  {
    if (type != bftString)
    {
      internalErrorPopup(90225);
      return;
    }

    str = newStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr()
  {
    return type == bftString ? safeStr(str) : switch (bibFieldEnum)
    {
      case bfContainerTitle,
           bfTitle           -> buildTitle(strList);

      case bfMisc            -> strListToStr(strList, false, true);

      default ->
      {
        internalErrorPopup(90227);
        yield null;
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String buildTitle(List<String> list)
  {
    StringBuilder sb = new StringBuilder();

    list.forEach(titleStr ->
    {
      titleStr = titleStr.strip();

      if (titleStr.isEmpty()) return;

      if (sb.length() > 0)
      {
        if (StringUtils.isAlpha(StringUtils.right(sb.toString(), 1)))
          sb.append(':');

        sb.append(' ');
      }

      sb.append(titleStr);
    });

    return sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setAll(List<String> otherList)
  {
    if (type != bftMultiString)
    {
      internalErrorPopup(90230);
      return;
    }

    strList.clear();
    strList.addAll(otherList);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<String> getMultiStr()
  {
    if (type != bftMultiString)
    {
      internalErrorPopup(90231);
      return null;
    }

    return Collections.unmodifiableList(strList);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addStr(String newStr)
  {
    if (type != bftMultiString)
    {
      internalErrorPopup(90229);
      return;
    }

    if (strNullOrBlank(newStr)) return;

    switch (bibFieldEnum)
    {
      case bfISBNs : matchISBN(newStr, strList); break;
      case bfISSNs : matchISSN(newStr, strList); break;
      default      : strList.add(newStr);        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
