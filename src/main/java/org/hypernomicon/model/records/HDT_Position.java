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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.stream.Collectors;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.records.HDT_Argument.ArgumentAuthor;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;

//---------------------------------------------------------------------------

public class HDT_Position extends HDT_RecordWithMainText
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final List<HDT_Debate> largerDebates, subDebates;
  public final List<HDT_Position> largerPositions, subPositions;
  public final List<HDT_Argument> arguments;

//---------------------------------------------------------------------------

  public HDT_Position(RecordState xmlState, DatasetAccessor<HDT_Position> dataset)
  {
    super(xmlState, dataset);

    largerDebates   = getObjList(rtParentDebateOfPos);
    largerPositions = getObjList(rtParentPosOfPos   );

    arguments    = getSubjList(rtPositionOfArgument);
    subPositions = getSubjList(rtParentPosOfPos    );
    subDebates   = getSubjList(rtParentPosOfDebate );
  }

//---------------------------------------------------------------------------

  @Override public String listName()          { return name(); }
  @Override public final boolean isUnitable() { return true; }

  public boolean setLargerPositions(List<HDT_Position> list) { return updateObjectsFromList(rtParentPosOfPos   , list); }
  public boolean setLargerDebates  (List<HDT_Debate>   list) { return updateObjectsFromList(rtParentDebateOfPos, list); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Retrieves the first debate record from the largerDebates list of this position or its ancestor positions.
   * <p>
   * This method starts from the current position and checks if its largerDebates list is non-empty.
   * If the largerDebates list is empty, the method moves up to the first parent position in the largerPositions list
   * and repeats the check. This process continues until a non-empty largerDebates list is found or
   * there are no more parent positions to check.
   * </p>
   *
   * @return the first {@link HDT_Debate} from a non-empty largerDebates list; {@code null} if none is found.
   */
  public HDT_Debate getLargerDebate()
  {
    HDT_Position currentPosition = this;

    while (currentPosition != null)
    {
      if (currentPosition.largerDebates.isEmpty() == false)
        return currentPosition.largerDebates.getFirst();

      currentPosition = currentPosition.largerPositions.isEmpty() ? null : currentPosition.largerPositions.getFirst();
    }

    return null; // Reached only if no larger debate exists
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public LinkedHashSet<ArgumentAuthor> getPeople()
  {
    LinkedHashSet<ArgumentAuthor> people = subPositions.stream().flatMap(subPos -> subPos.getPeople().stream())
                                                                .collect(Collectors.toCollection(LinkedHashSet::new));

    arguments.stream().filter(arg -> arg.isInFavor(this)).forEachOrdered(arg -> arg.getPeople().forEachOrdered(people::add));

    return people;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class PositionSource
  {
    public HDT_Argument argument = null;
    public HDT_Work work = null;
    public HDT_Person author = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PositionSource getLaunchableWork() { return getSource(true , false, false); }
  public PositionSource getWork()           { return getSource(true , false, true ); }
  public PositionSource getArgument()       { return getSource(true , true , true ); }
  public PositionSource getWorkWithAuthor() { return getSource(false, false, true ); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private PositionSource getSource(boolean noAuthorOK, boolean noWorkOK, boolean notLaunchableOK)
  {
    PositionSource ps = new PositionSource(), webPs = null;

    for (HDT_Argument arg : arguments)
    {
      if (arg.isInFavor(this))
      {
        ps.argument = arg;

        for (HDT_Work work : arg.works)
        {
          if (work.pathNotEmpty())
          {
            if (work.authorRecords.size() > 0)
            {
              ps.work = work;
              ps.author = work.authorRecords.getFirst();
              return ps;
            }

            if (noAuthorOK)
            {
              ps.work = work;
              return ps;
            }
          }

          else if ((work.getURL().isEmpty() == false) && (webPs == null))
          {
            if (work.authorRecords.size() > 0)
            {
              webPs = new PositionSource();
              webPs.work = work;
              webPs.author = work.authorRecords.getFirst();
            }
            else if (noAuthorOK)
            {
              webPs = new PositionSource();
              webPs.work = work;
            }
          }

          if (work.authorRecords.size() > 0)
          {
            if (notLaunchableOK)
            {
              ps.work = work;
              ps.author = work.authorRecords.getFirst();
              return ps;
            }
          }

          if (noAuthorOK && notLaunchableOK)
          {
            ps.work = work;
            return ps;
          }
        }

        if (noWorkOK) return ps;
      }
    }

    return webPs;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
