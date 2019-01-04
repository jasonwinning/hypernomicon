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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.bib.BibData.BibFieldType.*;
import static org.hypernomicon.bib.BibUtils.*;

import org.hypernomicon.bib.BibData.BibFieldEnum;
import org.hypernomicon.bib.BibData.BibFieldType;

public class BibField
{
  private final BibFieldEnum bibFieldEnum;
  private final BibFieldType type;
  private final List<String> strList = new ArrayList<String>();

  private String str;

  public BibField(BibFieldEnum bibFieldEnum)
  {
    this.bibFieldEnum = bibFieldEnum;

    type = BibData.getFieldType(bibFieldEnum);
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

    String allStr = "";

    switch (bibFieldEnum)
    {
      case bfContainerTitle: case bfTitle:

        for (String titleStr : strList)
          allStr = addTitleComponent(allStr, titleStr);

        return allStr;

      case bfMisc:

        for (String miscStr : strList)
        {
          if (miscStr.length() > 0)
          {
            if (allStr.length() > 0)
              allStr = allStr + System.lineSeparator();

            allStr = allStr + miscStr;
          }
        }

        return allStr;

      default:
        messageDialog("Internal error #90227", mtError);
        return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String addTitleComponent(String str, String titleStr)
  {
    titleStr = titleStr.trim();

    if (titleStr.length() > 0)
    {
      if (str.length() > 0)
      {
        if (StringUtils.isAlpha(StringUtils.right(str, 1)))
          str = str + ":";

        str = str + " ";
      }

      str = str + titleStr;
    }

    return str;
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

  public void clear()
  {
    if (type == bftMultiString)
      strList.clear();
    else
      str = "";
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

    if (ultraTrim(safeStr(newStr)).length() == 0) return;

    switch (bibFieldEnum)
    {
      case bfISBNs :

        matchISBN(newStr, strList);
        break;

      case bfISSNs :

        matchISSN(newStr, strList);
        break;

      default :
        strList.add(newStr);
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
