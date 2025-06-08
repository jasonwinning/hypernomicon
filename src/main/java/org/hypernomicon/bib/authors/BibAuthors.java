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

import static org.hypernomicon.util.Util.*;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

//---------------------------------------------------------------------------

public abstract class BibAuthors implements Iterable<BibAuthor>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty()          { return iterator().hasNext() == false; }
  public Stream<BibAuthor> stream() { return iterableToStream(this); }

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

  /**
   * Return a consolidated version of the passed in iterable so that there are no authors that
   * are duplicates in terms of associated person record or exact name match, unless
   * one was an author and the other was an editor and/or translator.
   * @param origAuthors Source for the authors to consolidate
   * @param doLookup Whether to lookup person records by name
   * @return The consolidated list
   */
  private static List<BibAuthor> normalizeAuthors(Iterable<BibAuthor> origAuthors, boolean doLookup)
  {
    List<BibAuthor> outputList = new ArrayList<>();

    if (origAuthors.iterator().hasNext() == false) return outputList;

    BiMap<PersonName, Integer> nameIndices   = HashBiMap.create();
    BiMap<HDT_Person, Integer> personIndices = HashBiMap.create();

    for (BibAuthor bibAuthor : origAuthors)
    {
      HDT_Person person = bibAuthor.getPerson();
      PersonName name = bibAuthor.getName();
      if (name.isEmpty() && (bibAuthor.getPerson() == null))
        continue;

      int authNdx1 = -1, authNdx2 = -1;

      if ((bibAuthor.getPerson() != null) && personIndices.containsKey(bibAuthor.getPerson()))
        authNdx1 = personIndices.get(person);
      else if ((name.isEmpty() == false) && nameIndices.containsKey(name))
        authNdx1 = nameIndices.get(name);

      if (doLookup && (person == null))
      {
        person = HDT_Person.lookUpByName(name);
        if (personIndices.containsKey(person))
        {
          if (authNdx1 == -1)
            authNdx1 = personIndices.get(person);
          else
            authNdx2 = personIndices.get(person);
        }
      }

      if (authNdx1 == authNdx2)
        authNdx2 = -1;

      if (person != null)
        name = person.getName();

      boolean addNew = true;

      if (authNdx1 > -1)
      {
        BibAuthor bibAuthor1 = outputList.get(authNdx1),
                  bibAuthor2 = bibAuthor;

        if (bibAuthor1.getIsAuthor() == bibAuthor2.getIsAuthor())
        {
          addNew = false;
          outputList.set(authNdx1, new BibAuthor(name, person, bibAuthor1.getIsEditor() || bibAuthor2.getIsEditor(),
                                                               bibAuthor1.getIsTrans () || bibAuthor2.getIsTrans ()));
        }
        else
        {
          outputList.set(authNdx1, new BibAuthor(name, person, bibAuthor1.getIsEditor(), bibAuthor1.getIsTrans()));
        }

        nameIndices  .forcePut(name  , authNdx1);
        personIndices.forcePut(person, authNdx1);
      }

      if (authNdx2 > -1)
      {
        BibAuthor bibAuthor1 = outputList.get(authNdx1),
                  bibAuthor2 = outputList.get(authNdx2);

        if (bibAuthor1.getIsAuthor() == bibAuthor2.getIsAuthor())
        {
          // Remove the duplicate with the higher index

          int lowerNdx  = Math.min(authNdx1, authNdx2),
              higherNdx = Math.max(authNdx1, authNdx2);

          nameIndices  .inverse().remove(higherNdx);
          personIndices.inverse().remove(higherNdx);
          outputList             .remove(higherNdx);

          nameIndices  .forcePut(name  , lowerNdx);
          personIndices.forcePut(person, lowerNdx);

          outputList.set(lowerNdx, new BibAuthor(name, person, bibAuthor1.getIsEditor() || bibAuthor2.getIsEditor(),
                                                               bibAuthor1.getIsTrans () || bibAuthor2.getIsTrans ()));
        }
        else
        {
          // Keep both

          nameIndices  .forcePut(name  , authNdx1);
          personIndices.forcePut(person, authNdx1);

          outputList.set(authNdx1, new BibAuthor(name, person, bibAuthor1.getIsEditor(), bibAuthor1.getIsTrans()));

          nameIndices  .forcePut(name  , authNdx2);
          personIndices.forcePut(person, authNdx2);

          outputList.set(authNdx2, new BibAuthor(name, person, bibAuthor2.getIsEditor(), bibAuthor2.getIsTrans()));

          if (addNew)
          {
            // Now need to check if second one is duplicate with current bibAuthor

            bibAuthor1 = outputList.get(authNdx2);
            bibAuthor2 = bibAuthor;

            if (bibAuthor1.getIsAuthor() == bibAuthor2.getIsAuthor())
            {
              addNew = false;
              outputList.set(authNdx1, new BibAuthor(name, person, bibAuthor1.getIsEditor() || bibAuthor2.getIsEditor(),
                                                                   bibAuthor1.getIsTrans () || bibAuthor2.getIsTrans ()));
            }
          }
        }
      }

      if (addNew)
      {
        outputList.add(new BibAuthor(name, person, bibAuthor.getIsEditor(), bibAuthor.getIsTrans()));
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

    List<PersonName> nonRecordNames   = new ArrayList<>(); // list containing only non-record authors
    List<Author>     nonRecordAuthors = new ArrayList<>(); // author objects for each non-record author
    List<Integer>    outputIndices    = new ArrayList<>(); // index into outputList for this non-record author

    for (int ndx = 0; ndx < outputList.size(); ndx++)
    {
      BibAuthor bibAuthor = outputList.get(ndx);

      if (bibAuthor.getPerson() == null)
      {
        PersonName name = bibAuthor.getName();

        nonRecordNames.add(name);
        nonRecordAuthors.add(new Author(destWork, bibAuthor, Ternary.Unset));
        outputIndices.add(ndx);
      }
    }

    int startNdx = 0;

    while (startNdx < nonRecordNames.size())
    {
      ArrayList<ArrayList<Author>> matchedAuthorsList = new ArrayList<>(); // List of matches for each non-record author

      HyperTask task = NewPersonDlgCtrlr.createDupCheckTask(nonRecordNames  .subList(startNdx, nonRecordNames.size()),
                                                            nonRecordAuthors.subList(startNdx, nonRecordNames.size()),
                                                            matchedAuthorsList,
                                                            null);

      if (task.runWithProgressDialog() != State.SUCCEEDED) return outputList;

      int ndx;

      for (ndx = startNdx; ndx < nonRecordNames.size(); ndx++)
      {
        ArrayList<Author> matchedAuthors = matchedAuthorsList.get(ndx - startNdx);

        if (matchedAuthors.isEmpty()) continue;

        int outputListNdx = outputIndices.get(ndx);
        Author origAuthor = nonRecordAuthors.get(ndx);

        NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(origAuthor.getName(), null, false, null, null, matchedAuthors);

        if (npdc.showModal() == false) continue;

        outputList.set(outputListNdx, new BibAuthor(npdc.getName(), npdc.getPerson(), origAuthor.getIsEditor(), origAuthor.getIsTrans()));

        startNdx = ndx + 1;
        break;
      }

      if (ndx == nonRecordNames.size())
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

  public boolean notAllEngCharLastNames()
  {
    for (BibAuthor author : this)
      if (author.getName().toEngChar().getLast().equals(author.getName().getLast()) == false)
        return true;

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
