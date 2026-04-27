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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.authors.RecordAuthor;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Verdict.HDT_ArgumentVerdict;
import org.hypernomicon.model.records.HDT_Verdict.HDT_PositionVerdict;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;

//---------------------------------------------------------------------------

public class HDT_Argument extends HDT_RecordWithMainText
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final List<HDT_Position> positions;
  public final List<HDT_Argument> targetArgs, responseArgs;
  public final List<HDT_Work> works;

//---------------------------------------------------------------------------

  public HDT_Argument(RecordState xmlState, DatasetAccessor<HDT_Argument> dataset)
  {
    super(xmlState, dataset);

    positions  = Collections.unmodifiableList(getObjList(rtPositionOfArgument));
    targetArgs = Collections.unmodifiableList(getObjList(rtTargetArgOfArg    ));
    works = getObjList(rtWorkOfArgument);
    responseArgs = getSubjList(rtTargetArgOfArg);
  }

//---------------------------------------------------------------------------

  public String pagesInWork(HDT_Work work)                { return db.getNestedString(this, work, tagPages); }
  public void setPagesInWork(HDT_Work work, String pages) { db.updateNestedString(this, work, tagPages, pages); }
  public Ternary getIsArgument()                          { return getTagTernary(tagIsArgument); }
  public void setIsArgument(Ternary val)                  { updateTagTernary(tagIsArgument, val); }

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

  public void addTargetArg(HDT_Argument target, HDT_ArgumentVerdict verdict) throws RelationCycleException
  {
    HyperObjList<HDT_Argument, HDT_Argument> targets = getObjList(rtTargetArgOfArg);
    targets.add(target);
    targets.throwLastException();

    if (verdict == null) return;

    db.updateNestedPointer(this, target, tagArgumentVerdict, verdict);
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

  public Ternary isInFavor(HDT_Position position)
  {
    return nullSwitch(getPosVerdict(position), Ternary.Unset, HDT_Verdict::isInFavor);
  }

  public Ternary isInFavor(HDT_Argument targetArg)
  {
    return nullSwitch(getArgVerdict(targetArg), Ternary.Unset, HDT_Verdict::isInFavor);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public record DebateAndPosition(HDT_Debate debate, HDT_Position position) {}

  /**
   * Retrieves the first non-null debate associated with this argument/stance, along with the
   * {@link HDT_Position} from which the debate was found.
   * <p>
   * This method first searches for a debate by applying {@link HDT_Position#getLargerDebate} to the
   * positions associated with this argument/stance. If no debate is found, it then searches the arguments
   * that this argument responds to by applying this method recursively to the target arguments.
   * </p>
   * <p>
   * If no debate can be found, the result still contains the first position discoverable by the same
   * recursive walk (this argument's positions, then the positions of target arguments), with a {@code null}
   * debate. If neither a debate nor a position can be found, both fields of the returned record are
   * {@code null}; this method never returns {@code null} itself.
   * </p>
   * <p>
   * The returned position is the entry from {@link #positions} (or, transitively, from a target argument's
   * positions) whose {@link HDT_Position#getLargerDebate} traversal yielded the debate; not the ancestor
   * position that the debate is directly attached to.
   * </p>
   *
   * @return a {@link DebateAndPosition} pair; never {@code null}.
   */
  public DebateAndPosition getDebateAndPosition()
  {
    DebateAndPosition fallbackResult = null;

    for (HDT_Position position : positions)
    {
      HDT_Debate debate = position.getLargerDebate();

      if (debate != null)
        return new DebateAndPosition(debate, position);

      if (fallbackResult == null)
        fallbackResult = new DebateAndPosition(null, position);
    }

    for (HDT_Argument targetArg : targetArgs)
    {
      DebateAndPosition result = targetArg.getDebateAndPosition();

      if (result.debate() != null)
        return result;

      if ((fallbackResult == null) && (result.position() != null))
        fallbackResult = result;
    }

    return fallbackResult != null ? fallbackResult : new DebateAndPosition(null, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Wrapper class for Author that treats authors the same if they have the same person record. If the
  // person record is null for both authors, then they are the same if they have the same name.

  public static final class ArgumentAuthor
  {
    private ArgumentAuthor(RecordAuthor author)
    {
      person = author.getPerson();
      name = person == null ? author.getName() : null;
      this.author = author;
    }

    public RecordAuthor getAuthObj() { return author; }

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
    private final RecordAuthor author;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<ArgumentAuthor> getPeople()
  {
    Function<? super HDT_Work, ? extends Stream<ArgumentAuthor>> mapFunction = work ->
    {
      Stream<RecordAuthor> stream = work.getAuthors().stream();

      boolean hasEditor = false;

      for (RecordAuthor author : work.getAuthors())
      {
        if (author.getIsAuthor())
          return stream.filter(RecordAuthor::getIsAuthor).map(ArgumentAuthor::new);

        if (author.getIsEditor())
          hasEditor = true;
      }

      return hasEditor ?
        stream.filter(RecordAuthor::getIsEditor).map(ArgumentAuthor::new)
      :
        stream.map(ArgumentAuthor::new);
    };

    return works.stream().flatMap(mapFunction);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
