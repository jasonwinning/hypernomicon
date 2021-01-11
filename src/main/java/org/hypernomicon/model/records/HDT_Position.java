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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.LinkedHashSet;
import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.records.HDT_Argument.ArgumentAuthor;

public class HDT_Position extends HDT_RecordWithConnector
{
  public final List<HDT_Debate> debates;
  public final List<HDT_Position> largerPositions, subPositions;
  public final List<HDT_Argument> arguments;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Position(RecordState xmlState, HyperDataset<HDT_Position> dataset)
  {
    super(xmlState, dataset, tagName);

    debates = getObjList(rtDebateOfPosition);
    largerPositions = getObjList(rtParentPosOfPos);

    arguments = getSubjList(rtPositionOfArgument);
    subPositions = getSubjList(rtParentPosOfPos);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName()    { return name(); }
  @Override public boolean isUnitable() { return true; }

  public void setLargerPositions(List<HDT_Position> list) { updateObjectsFromList(rtParentPosOfPos, list); }
  public void setDebates(List<HDT_Debate> list)           { updateObjectsFromList(rtDebateOfPosition, list); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Debate getDebate()
  {
    HDT_Position position = this;

    while (position.debates.isEmpty() && (position.largerPositions.size() > 0))
      position = position.largerPositions.get(0);

    return position.debates.size() > 0 ? position.debates.get(0) : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public LinkedHashSet<ArgumentAuthor> getPeople()
  {
    LinkedHashSet<ArgumentAuthor> people = new LinkedHashSet<>();

    subPositions.forEach(subPos -> people.addAll(subPos.getPeople()));

    arguments.stream().filter(arg -> arg.isInFavor(this)).forEach(arg -> people.addAll(arg.getPeople()));

    return people;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public class PositionSource
  {
    public HDT_Argument argument = null;
    public HDT_Work work = null;
    public HDT_Person author = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PositionSource getLaunchableWork()   { return getSource(true , false, false); }
  public PositionSource getWork()             { return getSource(true , false, true ); }
  public PositionSource getArgument()         { return getSource(true , true , true ); }
  public PositionSource getWorkWithAuthor()   { return getSource(false, false, true ); }

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
              ps.author = work.authorRecords.get(0);
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
              webPs.author = work.authorRecords.get(0);
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
              ps.author = work.authorRecords.get(0);
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
