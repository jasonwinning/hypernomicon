/*
 * Copyright 2015-2021 Jason Winning
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

package org.hypernomicon.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;

import org.apache.commons.lang3.StringUtils;
import org.hypernomicon.util.PopupDialog.DialogResult;

public class WebButton
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static enum WebButtonField
  {
    Name("Record name or title including subtitle; blank for persons"),
    SingleName("Last name of person, or first name if there is no last name"),
    LastName("Last name of person"),
    FirstName("First name of person"),
    QueryName("Name format determined by querying user"),
    Field("Field of person"),
    DivisionName("Division of institution"),
    City("City"),
    Region("State/Region"),
    Country("Country"),
    Title("Title without subtitle"),
    QueryTitle("Title format determined by querying user"),
    NumericYear("Year of work; considered blank if non-numeric"),
    Year("Year of work"),
    doi("doi of work"),
    ISBN("ISBN of work");

    public final String key, toolTip;

  //---------------------------------------------------------------------------

    private WebButtonField(String toolTip)
    {
      key = "::::" + name();
      this.toolTip = toolTip;
    }

  //---------------------------------------------------------------------------

    public boolean nonblank(String str)
    {
      if (safeStr(str).isBlank())
        return false;

      switch (this)
      {
        case NumericYear:

          return StringUtils.isNumeric(str);

        default:

          return true;
      }
    }

  //---------------------------------------------------------------------------

    public static EnumSet<WebButtonField> getFieldsForPrefKey(String prefKey)
    {
      EnumSet<WebButtonField> fields = EnumSet.noneOf(WebButtonField.class);

      switch (prefKey)
      {
        case PREF_KEY_PERSON_SRCH : case PREF_KEY_PERSON_IMG_SRCH :

          fields.add(FirstName); fields.add(LastName); fields.add(SingleName); fields.add(QueryName); fields.add(Field);
          break;

        case PREF_KEY_INST_SRCH :

          fields.add(Name); fields.add(DivisionName);
          break;

        case PREF_KEY_INST_MAP_SRCH :

          fields.add(Name); fields.add(City); fields.add(Region); fields.add(Country);
          break;

        case PREF_KEY_DOI_SRCH :

          fields.add(doi);
          break;

        case PREF_KEY_ISBN_SRCH :

          fields.add(ISBN);
          break;

        case PREF_KEY_WORK_SRCH :

          fields.add(Title); fields.add(QueryTitle); fields.add(NumericYear); fields.add(SingleName); fields.add(ISBN); fields.add(doi);
          break;

        case PREF_KEY_GEN_SRCH :

          fields.add(Name);
          break;
      }

      return fields;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class UrlPattern
  {
    private final EnumSet<WebButtonField> requiredFields;
    public final String str;

    public UrlPattern(EnumSet<WebButtonField> requiredFields, String str)
    {
      this.requiredFields = requiredFields;
      this.str = str;
    }

    public EnumSet<WebButtonField> reqFields() { return EnumSet.copyOf(requiredFields); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<UrlPattern> patterns = new ArrayList<>();
  private final EnumMap<WebButtonField, String> values = new EnumMap<>(WebButtonField.class);
  private String name;
  private String caption;

  public WebButton(String name, String caption) { this.name = name; this.caption = caption; }

  public void addPattern(UrlPattern pattern) { patterns.add(pattern); }
  public List<UrlPattern> getPatterns()      { return Collections.unmodifiableList(patterns); }
  public String getCaption()                 { return caption; }
  public void setCaption(String caption)     { this.caption = caption; }
  public String getName()                    { return name; }
  public void setName(String name)           { this.name = name; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public WebButton first(WebButtonField field, String str)
  {
    values.clear();
    return next(field, str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public WebButton next(WebButtonField field, String str)
  {
    if (field.nonblank(str))
    {
      str = str.replace('\u2018', '\'').replace('\u2019', '\'').replace('\u201c', '"').replace('\u201d', '"');

      values.put(field, str);
    }

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void go()
  {
    nextPattern:

    for (UrlPattern pattern : patterns)
    {
      for (WebButtonField requiredField : pattern.requiredFields)
      {
        if (values.containsKey(requiredField) == false)
          continue nextPattern;
      }

      String str = pattern.str;

      for (WebButtonField field : WebButtonField.values())
      {
        String value = values.getOrDefault(field, "");

        if (str.contains(field.key) == false) continue;

        if ((field == WebButtonField.QueryTitle) || (field == WebButtonField.Title))
          if (value.startsWith("("))
            value = removeFirstParenthetical(value);

        if (field == WebButtonField.QueryTitle)
        {
          String[] array = value.split("[.;:!?()]|--");

          if (array.length > 1)
            if (confirmDialog("Should the subtitle be omitted? Sometimes (e.g., in Google Scholar) this yields better results."))
              value = array[0];
        }
        else if (field == WebButtonField.QueryName)
        {
          String first1 = ultraTrim(removeFirstParenthetical(value));

          int ndx = first1.indexOf(' ');

          if (ndx >= 0)
          {
            String first3 = String.valueOf(first1.charAt(0));

            for (; ndx >= 0; ndx = first1.indexOf(' ', ndx + 1))
              first3 = first3 + String.valueOf(first1.charAt(ndx + 1));

            first3 = first3.toUpperCase();

            String first2 = ultraTrim(first1.replaceAll("^[^\\s]\\.", "")
                                            .replaceAll("\\s[^\\s]\\.", ""));

            ndx = first2.indexOf(' ');
            if (ndx >=0)
              first2 = first2.substring(0, ndx);

            String last = values.get(WebButtonField.LastName);

            DialogResult result = new PopupDialog("How should the name be phrased? Initials often works well with Google Scholar.")

              .addButton(first1 + " " + last, mrYes)
              .addButton(first2 + " " + last, mrNo)
              .addButton(first3 + " " + last, mrOk)
              .addButton("Cancel", mrCancel)

              .showModal();

            switch (result)
            {
              case mrYes : value = first1; break;
              case mrNo  : value = first2; break;
              case mrOk  : value = first3; break;
              default    : return;
            }
          }
          else
            value = first1;
        }

        while (str.contains(field.key))
          str = str.replace(field.key, escapeURL(value, field != WebButtonField.doi));
      }

      DesktopUtil.openWebLink(str);
      return;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
