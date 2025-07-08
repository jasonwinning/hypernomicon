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

package org.hypernomicon.bib.authors;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.*;

import javafx.concurrent.Worker.State;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.query.personMatch.PersonForDupCheck;
import org.hypernomicon.query.personMatch.PersonMatcher;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public abstract class BibAuthors implements Iterable<BibAuthor>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty()                { return iterator().hasNext() == false; }
  public Stream<BibAuthor> stream()       { return iterableToStream(this); }
  public boolean notAllEngCharLastNames() { return stream().allMatch(author -> author.getName().toEngChar().getLast().equals(author.getName().getLast())); }

  public static boolean isEmpty(BibAuthors bibAuthors) { return (bibAuthors == null) || bibAuthors.isEmpty(); }

  public List<BibAuthor> normalizedList(boolean doLookup) { return normalizeAuthors(this, doLookup); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr()
  {
    Function<? super BibAuthor, String> mapper = bibAuthor ->
      bibAuthor.getName().getLastFirst() + (bibAuthor.getIsEditor() ?
        (bibAuthor.getIsTrans() ? "(ed, tr)" : "(ed)")
      :
        (bibAuthor.getIsTrans() ? "(tr)" : ""));

    return stream().map(mapper).collect(Collectors.joining("; "));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr(AuthorType authorType)
  {
    Predicate<BibAuthor> predicate = bibAuthor -> switch (authorType)
    {
      case author     -> bibAuthor.getIsAuthor();
      case editor     -> bibAuthor.getIsEditor();
      case translator -> bibAuthor.getIsTrans ();
    };

    return stream().filter(predicate)
                   .map(bibAuthor -> bibAuthor.getName().getLastFirst())
                   .collect(Collectors.joining("; "));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static BibAuthor mergeBibAuthors(BibAuthor bibAuthor1, BibAuthor bibAuthor2)
  {
    HDT_Person person = nullSwitch(bibAuthor1.getPerson(), bibAuthor2.getPerson());

    boolean notAuthor = (bibAuthor1.getIsAuthor() == false) && (bibAuthor2.getIsAuthor() == false),

            ed = notAuthor && (bibAuthor1.getIsEditor() || bibAuthor2.getIsEditor()),
            tr = notAuthor && (bibAuthor1.getIsTrans () || bibAuthor2.getIsTrans ());

    return new BibAuthor(bibAuthor1.getName(), person, ed, tr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class BibAuthorKey
  {
    private final PersonName name;
    private final HDT_Person person;

  //---------------------------------------------------------------------------

    private BibAuthorKey(BibAuthor bibAuthor)
    {
      person = bibAuthor.getPerson();

      PersonName realName = bibAuthor.getPerson() == null ? bibAuthor.getName() : bibAuthor.getPerson().getName();

      String firstName = removeAllParentheticals(realName.getFirst()),
             lastName  = removeAllParentheticals(realName.getLast ());

      name = new PersonName(firstName, lastName).toEngChar().toLowerCase();
    }

  //---------------------------------------------------------------------------

    @Override public int hashCode() { return Objects.hash(name); }

  //---------------------------------------------------------------------------

    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if ((obj instanceof BibAuthorKey) == false) return false;

      BibAuthorKey other = (BibAuthorKey) obj;

      return ((person != null) && (other.person != null)) ? (person == other.person) : name.equals(other.name);
    }

  //---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Return a consolidated version of the passed in iterable so that there are no authors that
   * are duplicates in terms of associated person record or exact name match, unless
   * one was an author and the other was an editor and/or translator.
   * @param origAuthors Source for the authors to consolidate
   * @param doLookup Whether to lookup person records by name
   * @return The consolidated list
   */
  public static List<BibAuthor> normalizeAuthors(Iterable<BibAuthor> origAuthors, boolean doLookup)
  {
    List<BibAuthor> outputList = new ArrayList<>();

    if ((origAuthors == null) || (origAuthors.iterator().hasNext() == false)) return outputList;

    Set<PersonName> existingNames   = new HashSet<>();  // These allow us to avoid the sequential search in
    Set<HDT_Person> existingPersons = new HashSet<>();  // the vast majority of cases

    for (BibAuthor bibAuthor : origAuthors)
    {
      HDT_Person person = bibAuthor.getPerson();
      PersonName name = bibAuthor.getName();
      if (name.isEmpty() && (person == null))
        continue;

      if (doLookup && (person == null))
        bibAuthor = new BibAuthor(name, HDT_Person.lookUpByName(name), bibAuthor.getIsEditor(), bibAuthor.getIsTrans());

      BibAuthorKey newKey = new BibAuthorKey(bibAuthor);

      boolean notFound = true;

      if (existingNames.contains(newKey.name) || existingPersons.contains(newKey.person))
      {
        int ndx = 0;

        // Sequential search is used to control which authors get
        // merged when there are multiple duplicates.

        while (notFound && (ndx < outputList.size()))
        {
          BibAuthorKey existingKey = new BibAuthorKey(outputList.get(ndx));

          if (newKey.equals(existingKey))
          {
            notFound = false;

            outputList.set(ndx, mergeBibAuthors(outputList.get(ndx), bibAuthor));
          }
          else
            ndx++;
        }
      }

      if (notFound)
      {
        existingNames  .add(newKey.name);
        existingPersons.add(newKey.person);

        outputList.add(bibAuthor);
      }
    }

    return outputList;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Return a consolidated version of this BibAuthors so that there are no authors that
   * are duplicates in terms of associated person record or exact name match, unless
   * one was an author and the other was an editor and/or translator.
   * <p>
   * This also uses a duplicate search algorithm and prompts the user to merge authors
   * that appear to be a duplicate with one in the database.
   * @param destWork The work record data will be merged into
   * @return The consolidated list
   */
  public List<BibAuthor> getListForWorkMerge(HDT_Work destWork)
  {
    List<BibAuthor> outputList = normalizeAuthors(this, true);

    // At this point, the output list represents a consolidated version of this BibAuthors so that
    // there are no authors that are duplicates in terms of associated person record or exact name
    // match, unless one was an author and the other was an editor and/or translator.

    List<PersonForDupCheck> nonRecordAuthors = new ArrayList<>();
    List<Integer>           outputIndices    = new ArrayList<>(); // index into outputList for this non-record author

    for (int ndx = 0; ndx < outputList.size(); ndx++)
    {
      BibAuthor bibAuthor = outputList.get(ndx);

      if (bibAuthor.getPerson() == null)
      {
        nonRecordAuthors.add(new PersonForDupCheck(new Author(destWork, bibAuthor, Ternary.Unset)));
        outputIndices.add(ndx);
      }
    }

    int startNdx = 0;

    while (startNdx < nonRecordAuthors.size())
    {
      PersonMatcher matcher = new PersonMatcher();

      HyperTask task = matcher.createDupCheckTask(nonRecordAuthors.subList(startNdx, nonRecordAuthors.size()), null);

      if (task.runWithProgressDialog() != State.SUCCEEDED) return outputList;

      int ndx;

      for (ndx = startNdx; ndx < nonRecordAuthors.size(); ndx++)
      {
        PersonForDupCheck person = nonRecordAuthors.get(ndx);

        if (matcher.hasMatchesFor(person) == false) continue;

        int outputListNdx = outputIndices.get(ndx);
        Author origAuthor = person.getAuthor();

        NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(origAuthor.getName(), null, new Author(null, origAuthor.getName(), origAuthor.getIsEditor(), origAuthor.getIsTrans(), Ternary.Unset), matcher.getMatches(person));

        if (npdc.showModal() == false) continue;

        outputList.set(outputListNdx, new BibAuthor(npdc.getName(), npdc.getPerson(), origAuthor.getIsEditor(), origAuthor.getIsTrans()));

        startNdx = ndx + 1;
        break;
      }

      if (ndx == nonRecordAuthors.size())
        startNdx = ndx;
    }

    // Remove duplicates again

    return normalizeAuthors(outputList, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean authorListsAreEqual(List<BibAuthor> list1, List<BibAuthor> list2, boolean ignorePersonRecords, boolean ignoreParenthetical)
  {
    if (list1 == list2) return true;
    if ((list1 == null) != (list2 == null)) return false;
    if (list1.size() != list2.size()) return false;

    return IntStream.range(0, list1.size()).allMatch(ndx -> BibAuthor.areEqual(list1.get(ndx), list2.get(ndx), ignorePersonRecords, ignoreParenthetical));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
