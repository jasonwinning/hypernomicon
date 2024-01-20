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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.bib.data.BibField.BibFieldType.*;

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
    bfIssue          ("Issue"             , bftString),      // Issue number
    bfLanguage       ("Language"          , bftString),      // Language
    bfMisc           ("Misc. Information" , bftMultiString), // Holds uncategorized information
    bfPages          ("Page Numbers"      , bftString),      // Page range
    bfPubLoc         ("Publisher Location", bftString),      // Where published
    bfPublisher      ("Publisher"         , bftString),      // May or may not include city
    bfTitle          ("Title"             , bftMultiString), // Title of this work
    bfTranslators    ("Translators"       , bftAuthor),
    bfURL            ("URL"               , bftString),      // URL where this work can be found
    bfVolume         ("Volume"            , bftString),      // Volume number
    bfYear           ("Year"              , bftString);      // Publication year

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

  public enum BibFieldType { bftString, bftMultiString, bftEntryType, bftWorkType, bftAuthor }

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
      messageDialog("Internal error #90225", mtError);
      return;
    }

    str = newStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr()
  {
    if (type == bftString)
      return safeStr(str);

    switch (bibFieldEnum)
    {
      case bfContainerTitle: case bfTitle:

        return buildTitle(strList);

      case bfMisc:

        return strListToStr(strList, false, true);

      default:
        messageDialog("Internal error #90227", mtError);
        return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String buildTitle(List<String> list)
  {
    StringBuilder sb = new StringBuilder();

    list.forEach(titleStr ->
    {
      titleStr = titleStr.trim();

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
      messageDialog("Internal error #90230", mtError);
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
      messageDialog("Internal error #90231", mtError);
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
      messageDialog("Internal error #90229", mtError);
      return;
    }

    if (ultraTrim(safeStr(newStr)).isEmpty()) return;

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
