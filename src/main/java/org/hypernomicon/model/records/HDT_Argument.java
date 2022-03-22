/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ArgumentVerdict;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PositionVerdict;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;

import com.google.common.collect.Sets;

//---------------------------------------------------------------------------

public class HDT_Argument extends HDT_RecordWithMainText
{
  public final List<HDT_Position> positions;
  public final List<HDT_Argument> counteredArgs, counterArgs;
  public final List<HDT_Work> works;

  public HDT_Argument(RecordState xmlState, HyperDataset<HDT_Argument> dataset)
  {
    super(xmlState, dataset, tagName);

    positions = Collections.unmodifiableList(getObjList(rtPositionOfArgument));
    counteredArgs = Collections.unmodifiableList(getObjList(rtCounterOfArgument));
    works = getObjList(rtWorkOfArgument);
    counterArgs = getSubjList(rtCounterOfArgument);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName()        { return name(); }

  public void setWorks(List<HDT_Work> list) { updateObjectsFromList(rtWorkOfArgument, list); }

  public static final int truePositionVerdictID  = 1,
                          falsePositionVerdictID = 2,
                          failsArgumentVerdictID = 101;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_PositionVerdict getPosVerdict(HDT_Position position)
  {
    return (HDT_PositionVerdict) db.getNestedPointer(this, position, tagPositionVerdict);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_ArgumentVerdict getArgVerdict(HDT_Argument arg)
  {
    return (HDT_ArgumentVerdict) db.getNestedPointer(this, arg, tagArgumentVerdict);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addCounteredArg(HDT_Argument countered, HDT_ArgumentVerdict verdict) throws RelationCycleException
  {
    HyperObjList<HDT_Argument, HDT_Argument> list = getObjList(rtCounterOfArgument);
    list.add(countered);
    list.throwLastException();

    if (verdict == null) return;

    db.updateNestedPointer(this, countered, tagArgumentVerdict, verdict);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addPosition(HDT_Position position, HDT_PositionVerdict verdict)
  {
    if (getObjList(rtPositionOfArgument).add(position) == false)
      return;

    if (verdict == null) return;

    db.updateNestedPointer(this, position, tagPositionVerdict, verdict);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isInFavor(HDT_Position position)
  {
    return posVerdictIDIsInFavor(nullSwitch(getPosVerdict(position), -1, HDT_Record::getID));
  }

  public static boolean posVerdictIDIsInFavor(int id)
  {
    return Sets.newHashSet(1, 3, 6, 17).contains(id);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Debate getDebate()
  {
    HDT_Debate debate = findFirstHaving(positions, HDT_Position::getLargerDebate);

    return nullSwitch(debate, findFirstHaving(counteredArgs, HDT_Argument::getDebate));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Wrapper class for Author that treats authors the same if they have the same person record. If the
  // person record is null for both authors, then they are the same if they have the same name.

  public static class ArgumentAuthor
  {
    public ArgumentAuthor(Author author)
    {
      person = author.getPerson();
      name = person == null ? author.getName() : null;
      this.author = author;
    }

    public Author getAuthObj() { return author; }

  //---------------------------------------------------------------------------

    @Override public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((name == null) ? 0 : name.hashCode());
      result = prime * result + ((person == null) ? 0 : person.hashCode());
      return result;
    }

  //---------------------------------------------------------------------------

    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if (obj == null) return false;
      if (getClass() != obj.getClass()) return false;

      ArgumentAuthor other = (ArgumentAuthor) obj;

      return Objects.equals(name, other.name) && Objects.equals(person, other.person);
    }

  //---------------------------------------------------------------------------

    private final HDT_Person person;
    private final PersonName name;
    private final Author author;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<ArgumentAuthor> getPeople()
  {
    return works.stream().map(work -> work.getAuthors().stream().map(ArgumentAuthor::new))
                         .reduce(Stream::concat).orElse(Stream.empty());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
