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

package org.hypernomicon.query.personMatch;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;

import com.google.common.collect.LinkedListMultimap;

//---------------------------------------------------------------------------

public final class PersonMatcher
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final LinkedListMultimap<PersonForDupCheck, Author> matches;

//---------------------------------------------------------------------------

  public PersonMatcher()
  {
    matches = LinkedListMultimap.create();
  }

  public PersonMatcher(PersonForDupCheck personForDupCheck, Iterable<Author> origMatches)
  {
    this();

    matches.putAll(personForDupCheck, origMatches);
  }

//---------------------------------------------------------------------------

  public boolean hasMatchesFor(PersonForDupCheck person)                 { return matches.containsKey(person); }
  public List<Author> getMatches(PersonForDupCheck person)               { return matches.get(person); }
  public void forEachMatch(BiConsumer<PersonForDupCheck, Author> action) { matches.forEach(action); }
  public int numMatches()                                                { return matches.size(); }
  public boolean isEmpty()                                               { return matches.isEmpty(); }
  public void clear()                                                    { matches.clear(); }
  public Author getMatchedAuthor(int ndx)                                { return matches.values().get(ndx); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Generate a list of potential duplicate authors by iterating through every
   * work and person record in the database and adding every author and every
   * non-author HDT_Person record.
   * @return The list
   */
  public static LinkedList<PersonForDupCheck> createListForDupCheck()
  {
    Set<HDT_Person> persons = new HashSet<>();

    LinkedList<PersonForDupCheck> list = db.works.stream()

      .flatMap(work -> work.getAuthors().stream())
      .filter(author -> nullSwitch(author.getPerson(), true, persons::add))
      .map(PersonForDupCheck::new)
      .filter(personForDupCheck -> strNotNullOrEmpty(personForDupCheck.fullLCNameEngChar))
      .collect(Collectors.toCollection(LinkedList::new));

    db.persons.stream()

      .filter(person -> person.works.isEmpty())
      .map(PersonForDupCheck::new)
      .filter(personForDupCheck -> strNotNullOrEmpty(personForDupCheck.fullLCNameEngChar))
      .forEachOrdered(list::add);

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Check whether a person is a likely duplicate of any of the persons in a list
   * @param person1 The person to check
   * @param candidates Persons to check against
   * @param task Task this function is running in; can be null
   * @throws CancelledTaskException if the user clicked Cancel
   */
  public void doDupCheck(PersonForDupCheck person1, Iterable<PersonForDupCheck> candidates, HyperTask task) throws CancelledTaskException
  {
    if ((person1 == null) || strNullOrEmpty(person1.fullLCNameEngChar)) return;

    HDT_Work work1 = person1.getWork();
    HDT_Person personRec1 = person1.getPerson();

    for (PersonForDupCheck person2 : candidates)
    {
      if (task != null)
        task.incrementAndUpdateProgress(10);

      if (strNullOrEmpty(person2.fullLCNameEngChar)) continue;

      if ((person1.author != null) && (person1.author == person2.author)) continue;

      HDT_Person personRec2 = person2.getPerson();

      if ((personRec1 != null) && (personRec1 == personRec2)) continue;

      HDT_Work work2 = person2.getWork();

      // Don't count as a match if the work where person1 is an author
      // is the same as the work where person2 is an author

      if ((work1 != null) && (work1 == work2)) continue;

      if (person1.matches(person2))
      {
        // Don't count as a match if person1 is an HDT_Person record that is already
        // listed as an author on the work where author2 is an author

        if ((personRec1 == null) || (work2 == null) || (work2.getAuthors().containsPerson(personRec1) == false))
          matches.put(person1, person2.author);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a task to check an author name against all authors and person records
   * in the database to see if it is a duplicate.
   * @param queryPerson Person we are checking.
   * @param finishHndlr Code to run when finished
   * @return The task
   */
  public HyperTask createDupCheckTask(PersonForDupCheck queryPerson, Runnable finishHndlr)
  {
    return createDupCheckTask(List.of(queryPerson), finishHndlr);
  }

  /**
   * Create a task to check author names against all authors and person records
   * in the database to see if they are duplicates.
   * @param queryPersons List of persons to check
   * @param finishHndlr Code to run when finished
   * @return The task
   */
  public HyperTask createDupCheckTask(List<PersonForDupCheck> queryPersons, Runnable finishHndlr)
  {
    return new HyperTask("CheckForDupAuthors", "Checking for duplicates...") { @Override protected void call() throws CancelledTaskException
    {
      matches.clear();

      LinkedList<PersonForDupCheck> candidates = createListForDupCheck();

      totalCount = (long) queryPersons.size() * candidates.size();

      for (PersonForDupCheck queryPerson : queryPersons)
        doDupCheck(queryPerson, candidates, this);

      if (finishHndlr != null) runInFXThread(finishHndlr);
    }};
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
