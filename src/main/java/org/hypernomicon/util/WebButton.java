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

package org.hypernomicon.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.apache.commons.lang3.StringUtils;

public class WebButton
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum WebButtonField
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

    WebButtonField(String toolTip)
    {
      key = "::::" + name();
      this.toolTip = toolTip;
    }

    @Override public String toString() { return key; }

  //---------------------------------------------------------------------------

    private boolean nonblank(String str)
    {
      return (safeStr(str).isBlank() == false) && ((this != NumericYear) || StringUtils.isNumeric(str));
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

    private UrlPattern(String str, WebButtonField... requiredFields)
    {
      this(str, EnumSet.copyOf(Arrays.asList(requiredFields)));
    }

    public UrlPattern(String str, EnumSet<WebButtonField> requiredFields)
    {
      this.str = str;
      this.requiredFields = requiredFields;
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

  public List<UrlPattern> getPatterns()      { return Collections.unmodifiableList(patterns); }
  public void addPattern(UrlPattern pattern) { patterns.add(pattern); }
  public String getCaption()                 { return caption; }
  public void setCaption(String caption)     { this.caption = caption; }
  public String getName()                    { return name; }
  public void setName(String name)           { this.name = name; }

  public void addPattern(String str, WebButtonField...       requiredFields) { patterns.add(new UrlPattern(str, requiredFields)); }
  public void addPattern(String str, EnumSet<WebButtonField> requiredFields) { patterns.add(new UrlPattern(str, requiredFields)); }

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
      values.put(field, str.replace('\u2018', '\'').replace('\u2019', '\'').replace('\u201c', '"').replace('\u201d', '"'));

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getPatternStr()
  {
    for (UrlPattern pattern : patterns)
      if (pattern.requiredFields.stream().allMatch(values::containsKey))
        return pattern.str;

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void go()
  {
    String str = getPatternStr();
    if (str == null) return;

    boolean isScholar = str.toLowerCase().contains("scholar") && str.toLowerCase().contains("google");

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
        {
          String msg = isScholar ?
            "Should the subtitle be omitted? Sometimes (e.g., in Google Scholar) this yields better results."
          :
            "Should the subtitle be omitted? Sometimes this yields better results.";

          if (confirmDialog(msg))
            value = array[0];
        }
      }
      else if (field == WebButtonField.QueryName)
      {
        String first1 = ultraTrim(removeFirstParenthetical(value));

        int ndx = first1.indexOf(' ');

        if (ndx >= 0)
        {
          String first2 = ultraTrim(first1.replaceAll("^[^\\s]\\.", "")
                                          .replaceAll("\\s[^\\s]\\.", ""));

          ndx = first2.indexOf(' ');

          if (ndx >=0)
            first2 = first2.substring(0, ndx);

          String first3 = String.valueOf(first1.charAt(0));

          for (ndx = first1.indexOf(' '); ndx >= 0; ndx = first1.indexOf(' ', ndx + 1))
            first3 = first3 + first1.charAt(ndx + 1);

          first3 = first3.toUpperCase();

          String last = values.get(WebButtonField.LastName);

          switch (new PopupDialog("How should the name be phrased?" + (isScholar ? " Initials often works well with Google Scholar." : ""))

            .addButton(first1 + ' ' + last, mrYes)
            .addButton(first2 + ' ' + last, mrNo)
            .addButton(first3 + ' ' + last, mrOk)
            .addButton("Cancel", mrCancel)

            .showModal())
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
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
