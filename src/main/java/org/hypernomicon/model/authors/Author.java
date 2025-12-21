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

package org.hypernomicon.model.authors;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Person;

import static org.hypernomicon.util.StringUtil.*;

//---------------------------------------------------------------------------

public abstract class Author
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum AuthorType
  {
    author,
    editor,
    translator;

    public static AuthorType fromBibFieldEnum(BibFieldEnum bibFieldEnum)
    {
      return switch (bibFieldEnum)
      {
        case bfAuthors     -> author;
        case bfEditors     -> editor;
        case bfTranslators -> translator;

        default -> throw new IllegalArgumentException("Unexpected value: " + bibFieldEnum);
      };
    }
  }

//---------------------------------------------------------------------------

  private PersonName nameEngChar;

  public abstract PersonName getName();
  public abstract HDT_Person getPerson();
  public abstract boolean getIsEditor();
  public abstract boolean getIsTrans();
  public abstract Ternary getInFileName();

  public final String lastName()                     { return lastName(false); }
  public final String lastName(boolean engChar)      { return getName(engChar).getLast(); }
  public final String firstName()                    { return firstName(false); }
  public final String firstName(boolean engChar)     { return getName(engChar).getFirst(); }
  public final String nameLastFirst(boolean engChar) { return getName(engChar).getLastFirst(); }
  public final String fullName(boolean engChar)      { return getName(engChar).getFull(); }
  public final String singleName()                   { return singleName(false); }
  public final String singleName(boolean engChar)    { return getName(engChar).getSingle(); }
  public final String nameLastFirst()                { return nameLastFirst(false); }
  public final String getBibName()                   { return getName().getBibName(); }
  public final boolean getIsAuthor()                 { return (getIsEditor() == false) && (getIsTrans() == false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final String getSortKey()
  {
    HDT_Person person = getPerson();

    return person != null ?
      person.getSortKey()
    :
      getName(true).getSortKey();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final PersonName getName(boolean engChar)
  {
    HDT_Person person = getPerson();

    if (person != null)
      return person.getName(engChar);

    if (engChar)
    {
      if (nameEngChar == null)
        nameEngChar = getName().toEngChar();

      return nameEngChar;
    }

    return getName();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum NameDisplayMode
  {
    /** Always use initials form for every author. */
    ALWAYS_INITIALS,

    /** Use initials for multiple authors, full name for a single author. */
    INITIALS_MULTIPLE_ONLY,

    /** Always use full last-name-first form for every author. */
    ALWAYS_FULL
  }

  private static final int DEFAULT_AUTHORS_TO_SHOW = 6;

//---------------------------------------------------------------------------

  /**
   * Returns a concise, comma-separated list of author names.
   *
   * <p>If the stream contains exactly one author and fullNameIfSingleton is false,
   * the author’s initials-first form is used; otherwise the last-name-first form is applied.
   * An ampersand (&) precedes the final name when multiple authors are present.
   * Optionally sorts the list and appends editor notation.
   *
   * @param authors             stream of authors to format
   * @param sort                whether to sort authors by their natural order
   * @param fullNameIfSingleton if true and exactly one author, use full name format (e.g., Bechtel, William);
   *                            otherwise use last name and initials (e.g., Bechtel, W.)
   * @return                    formatted, comma-separated author string,
   *                            or empty string if no authors
   */
  public static String getShortAuthorsStr(Stream<? extends Author> authors, boolean sort, boolean fullNameIfSingleton)
  {
    NameDisplayMode mode = fullNameIfSingleton ? NameDisplayMode.INITIALS_MULTIPLE_ONLY : NameDisplayMode.ALWAYS_INITIALS;

    return getAuthorsStr(authors, ',', true, sort, mode, DEFAULT_AUTHORS_TO_SHOW);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a semicolon-separated list of author names in long form.
   *
   * <p>Always uses last-name-first format (no initials) and does not sort.
   * No ampersand is inserted, and the delimiter is a semicolon.
   * Optionally appends editor notation when all authors are editors.
   *
   * @param authors             stream of authors to format
   * @return                    formatted, semicolon-separated author string,
   *                            or empty string if no authors
   */
  public static String getLongAuthorsStr(Stream<? extends Author> authors)
  {
    return getLongAuthorsStr(authors, DEFAULT_AUTHORS_TO_SHOW);
  }

  /**
   * Returns a semicolon-separated list of author names in long form.
   *
   * <p>Always uses last-name-first format (no initials) and does not sort.
   * No ampersand is inserted, and the delimiter is a semicolon.
   * Optionally appends editor notation when all authors are editors.
   *
   * @param authors             stream of authors to format
   * @param limit               maximum number of authors to show
   * @return                    formatted, semicolon-separated author string,
   *                            or empty string if no authors
   */
  public static String getLongAuthorsStr(Stream<? extends Author> authors, int limit)
  {
    return getAuthorsStr(authors, ';', false, false, NameDisplayMode.ALWAYS_FULL, limit);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Constructs a formatted string representation of a list of authors with customizable formatting options.
   * <p>
   * The output may include author names (in full or initials), optional sorting, role suffixes
   * such as "(Eds.)", "(Trs.)", or "(Eds., Trs.)", and will delimit entries with the specified character.
   * If limit > 0, shows a maximum number of authors; additional authors are represented by "et al." if present.
   * When only one author is present, formatting respects {@code nameMode} preferences and includes role suffixes if applicable.
   * </p>
   *
   * @param authorStream     the stream of {@link Author} objects to format
   * @param delimiter        the character used to separate author names (e.g., comma or semicolon)
   * @param amp              whether to insert an ampersand (" & ") before the final author in a multi-author list
   * @param sort             if true, sorts authors using their natural ordering
   * @param nameMode         controls whether first names are spelled out or initialized
   * @param limit            maximum number of authors to show; < 1 means show all
   * @return a formatted string representation of the author list, including role suffixes and optional "et al." if applicable
   */
  private static String getAuthorsStr(Stream<? extends Author> authorStream, char delimiter, boolean amp, boolean sort, NameDisplayMode nameMode, int limit)
  {
    List<Author> authors = authorStream.collect(Collectors.toCollection(ArrayList::new));

    if (authors.isEmpty())
      return "";

    if (limit < 1)
      limit = Integer.MAX_VALUE;

    boolean includeRoleSuffixes = (sort == false);

    String finalRoleSuffix = "";

    if (includeRoleSuffixes && (authors.size() > 1))
    {
      boolean allEd = true, allTr = true, someEd = false, someTr = false;

      for (Author author : authors)
      {
        if (author.getIsEditor()) someEd = true;
        else                      allEd = false;

        if (author.getIsTrans())  someTr = true;
        else                      allTr = false;
      }

      if ((allEd == someEd) && (allTr == someTr))
      {
        if (allEd)
          finalRoleSuffix = allTr ? " (Eds., Trs.)" : " (Eds.)";
        else if (allTr)
          finalRoleSuffix = " (Trs.)";
      }

      if (strNotNullOrEmpty(finalRoleSuffix))
        includeRoleSuffixes = false;
    }

    boolean firstInitials    = (nameMode == NameDisplayMode.ALWAYS_INITIALS) || (nameMode == NameDisplayMode.INITIALS_MULTIPLE_ONLY),
            fullForSingleton = (nameMode == NameDisplayMode.ALWAYS_FULL    ) || (nameMode == NameDisplayMode.INITIALS_MULTIPLE_ONLY);

    if (authors.size() == 1)
    {
      Author author = authors.getFirst();

      return (firstInitials && (fullForSingleton == false) ? author.getBibName() : author.nameLastFirst()) + (includeRoleSuffixes ? author.getRoleSuffix(true) : "");
    }

    if (sort)
      authors.sort(null);

    int num = Math.min(limit, authors.size());
    String peopleStr = "";

    for (int ndx = 0; ndx < num; ndx++)
    {
      if (ndx != 0)
      {
        peopleStr = peopleStr + delimiter + ' ';

        if ((ndx == (authors.size() - 1)) && amp)
          peopleStr = peopleStr.strip() + " & ";
      }

      Author author = authors.get(ndx);

      peopleStr = peopleStr + (firstInitials ? author.getBibName() : author.nameLastFirst()) + (includeRoleSuffixes ? author.getRoleSuffix(false) : "");
    }

    if (num < authors.size())
      peopleStr = peopleStr + " et al.";

    return peopleStr + finalRoleSuffix;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getRoleSuffix(boolean capitalize)
  {
    boolean ed = getIsEditor(),
            tr = getIsTrans();

    String edStr = capitalize ? "Ed." : "ed",
           trStr = capitalize ? "Tr." : "tr";

    if (ed && tr) return " (" + edStr + ", " + trStr + ')';
    if (ed)       return " (" + edStr + ')';
    if (tr)       return " (" + trStr + ')';
    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
