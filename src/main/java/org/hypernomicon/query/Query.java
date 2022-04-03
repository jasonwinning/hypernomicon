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

package org.hypernomicon.query;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.ui.QueryTabCtrlr.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.query.sources.AllQuerySource;
import org.hypernomicon.query.sources.DatasetQuerySource;
import org.hypernomicon.query.sources.QuerySource;
import org.hypernomicon.query.ui.QueryView;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public abstract class Query<HDT_T extends HDT_Record>
{

//---------------------------------------------------------------------------

  private final int queryID;
  private final String description;

  private Query(int queryID, String description)
  {
    this.queryID = queryID;
    this.description = description;
  }

  public int getID() { return queryID; }

  public String getDescription() { return description; }

  public QuerySource getSource(QueryType queryType, HyperTableRow row)
  {
    RecordType recordType = queryType.getRecordType();

    QuerySource origSource = recordType == hdtNone ? new AllQuerySource() : new DatasetQuerySource(recordType);

    return getSource(origSource, row.getCell(2), row.getCell(3), row.getCell(4));
  }

  public abstract boolean evaluate(HDT_T record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall) throws HyperDataException;

  public abstract QueryType getQueryType();

  @SuppressWarnings("unused")
  public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return origSource; }

  @SuppressWarnings("unused")  //returns true if subsequent cells need to be updated
  public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3) { return true; }   // queryChange

  public void cleanup() { }

  @SuppressWarnings("unused")  //returns true if subsequent cells need to be updated
  public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  { return true; }

  @SuppressWarnings("unused")  //returns true if subsequent cells need to be updated
  public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  { return true; }

  public boolean needsMentionsIndex() { return false; }

  @SuppressWarnings("unused")
  public boolean hasOperand(int opNum, HyperTableCell prevOp) { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void clearOperands(HyperTableRow row, int startOpNum)
  {
    QueryView.clearOperands(row, startOpNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static Populator operandPopulator()
  {
    return Populator.create(cvtOperand,

        new HyperTableCell(EQUAL_TO_OPERAND_ID        , "Is or includes record", hdtNone),
        new HyperTableCell(NOT_EQUAL_TO_OPERAND_ID    , "Excludes record"      , hdtNone),
        new HyperTableCell(CONTAINS_OPERAND_ID        , "Contains text"        , hdtNone),
        new HyperTableCell(DOES_NOT_CONTAIN_OPERAND_ID, "Doesn't contain text" , hdtNone),
        new HyperTableCell(IS_EMPTY_OPERAND_ID        , "Is empty"             , hdtNone),
        new HyperTableCell(IS_NOT_EMPTY_OPERAND_ID    , "Is not empty"         , hdtNone));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class RecordQuery extends Query<HDT_Record>
  {
    RecordQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtAllRecords; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class ArgumentQuery extends Query<HDT_Argument>
  {
    public ArgumentQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtArguments; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtArgument); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class ConceptQuery extends Query<HDT_Concept>
  {
    public ConceptQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtConcepts; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtConcept); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class DebateQuery extends Query<HDT_Debate>
  {
    public DebateQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtDebates; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtDebate); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class MiscFileQuery extends Query<HDT_MiscFile>
  {
    public MiscFileQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtFiles; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtMiscFile); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class FolderQuery extends Query<HDT_Folder>
  {
    public FolderQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtFolders; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtFolder); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class InstitutionQuery extends Query<HDT_Institution>
  {
    public InstitutionQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtInstitutions; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtInstitution); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class InvestigationQuery extends Query<HDT_Investigation>
  {
    public InvestigationQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtInvestigations; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtInvestigation); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class NoteQuery extends Query<HDT_Note>
  {
    public NoteQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtNotes; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtNote); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class PersonQuery extends Query<HDT_Person>
  {
    public PersonQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtPersons; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtPerson); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class PositionQuery extends Query<HDT_Position>
  {
    public PositionQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtPositions; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtPosition); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class WorkQuery extends Query<HDT_Work>
  {
    public WorkQuery(int queryID, String description) { super(queryID, description); }

    @Override public QueryType getQueryType() { return QueryType.qtWorks; }

    @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return new DatasetQuerySource(hdtWork); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
