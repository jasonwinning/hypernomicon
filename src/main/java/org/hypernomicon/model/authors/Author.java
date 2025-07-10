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

package org.hypernomicon.model.authors;

import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;

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

}
