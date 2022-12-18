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
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.LinkedHashSet;

import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.query.sources.AllQuerySource;
import org.hypernomicon.query.sources.DatasetQuerySource;
import org.hypernomicon.query.sources.FilteredQuerySource;
import org.hypernomicon.query.sources.QuerySource;
import org.hypernomicon.query.ui.QueryCtrlr;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.concurrent.Worker.State;

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

  public final QuerySource getSource(QueryType queryType, HyperTableRow row)
  {
    RecordType recordType = queryType.getRecordType();

    QuerySource origSource = recordType == hdtNone ? new AllQuerySource() : new DatasetQuerySource(recordType);

    return getSource(origSource, row.getCell(QueryCtrlr.OPERAND_1_COL_NDX), row.getCell(QueryCtrlr.OPERAND_2_COL_NDX), row.getCell(QueryCtrlr.OPERAND_3_COL_NDX));
  }

  /**
   * Override this function to return whether this query should be shown as an
   * option when the query type is set according to the {@code queryType} parameter.
   * @param queryType The value set in the query type column.
   * @param recordType The record type corresponding to the {@code queryType}.
   */
  public abstract boolean show(QueryType queryType, RecordType recordType);

  public abstract boolean evaluate(HDT_T record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException;

  @SuppressWarnings("unused")
  protected QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { return origSource; }

  @SuppressWarnings("unused")  //returns true if subsequent cells need to be updated
  public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3) { return true; }   // queryChange

  /**
   * This function is called when the query begins execution, immediately before the first
   * record is evaluated. Any setup needed for the query to run should be performed here,
   * and anything that needs to be undone as a result of this function should be performed
   * in {@link Query#cleanup(State state) cleanup}.
   * @param op1 First query operand
   * @param op2 Second query operand
   * @param op3 Third query operand
   * @throws HyperDataException
   */
  public void init(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException { }

  /**
   * This function will be called after the query completes regardless of whether it
   * completed successfully, it was cancelled by the user, or it was terminated
   * abnormally due to an exception being thrown.
   * @param state Indicates whether the query completed successfully, was cancelled by
   * the user, or was terminated abnormally due to an exception being thrown.
   */
  public void cleanup(State state) { }

  @SuppressWarnings("unused")  //returns true if subsequent cells need to be updated
  public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  { return true; }

  @SuppressWarnings("unused")  //returns true if subsequent cells need to be updated
  public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  { return true; }

  public boolean needsMentionsIndex() { return false; }

  public boolean autoShowDescription() { return false; }

  /**
   * This determines whether the cell corresponding to an operand number will automatically
   * go into edit mode and the dropdown will be shown, after a value is committed by the
   * user for the previous operand or query selection cell.
   */
  @SuppressWarnings("unused")
  public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static boolean recordByTypeInit(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2)
  {
    vp1.setPopulator(row, new RecordTypePopulator(true));
    vp2.setPopulator(row, new RecordByTypePopulator());
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static boolean recordByTypeOpChange(HyperTableCell op, HyperTableRow row, VariablePopulator nextPop)
  {
    RecordByTypePopulator rtp = nextPop.getPopulator(row);
    rtp.setRecordType(row, HyperTableCell.getCellType(op));
    rtp.populate(row, false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void clearOperands(HyperTableRow row, int startOpNum)
  {
    QueryCtrlr.clearOperands(row, startOpNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final int EQUAL_TO_OPERAND_ID         = 1,
                          NOT_EQUAL_TO_OPERAND_ID     = 2,
                          CONTAINS_OPERAND_ID         = 3,
                          DOES_NOT_CONTAIN_OPERAND_ID = 4,
                          IS_EMPTY_OPERAND_ID         = 5,
                          IS_NOT_EMPTY_OPERAND_ID     = 6;

  static Populator operandPopulator()
  {
    return operandPopulator(false);
  }

  static Populator operandPopulator(boolean excludeRecordOps)
  {
    return Populator.create(cvtOperand,

      new HyperTableCell(EQUAL_TO_OPERAND_ID        , excludeRecordOps ? "Is exactly" : "Is or includes record", hdtNone),
      new HyperTableCell(NOT_EQUAL_TO_OPERAND_ID    , excludeRecordOps ? "Is not"     : "Excludes record"      , hdtNone),
      new HyperTableCell(CONTAINS_OPERAND_ID        , "Contains text"        , hdtNone),
      new HyperTableCell(DOES_NOT_CONTAIN_OPERAND_ID, "Doesn't contain text" , hdtNone),
      new HyperTableCell(IS_EMPTY_OPERAND_ID        , "Is empty"             , hdtNone),
      new HyperTableCell(IS_NOT_EMPTY_OPERAND_ID    , "Is not empty"         , hdtNone));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected abstract static class FilteredQuery<HDT_T1 extends HDT_Record> extends Query<HDT_T1>
  {
    protected final LinkedHashSet<HDT_Record> records = new LinkedHashSet<>();

    FilteredQuery(int queryID, String description)
    {
      super(queryID, description);
    }

    protected abstract void runFilter(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException;

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      records.clear();

      try
      {
        runFilter(op1, op2, op3);
      }
      catch (HyperDataException e)
      {
        messageDialog(e.getMessage(), mtError);
        records.clear();
      }

      return new FilteredQuerySource(origSource, records);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class RecordQuery extends Query<HDT_Record>
  {
    RecordQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtAllRecords; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredRecordQuery extends FilteredQuery<HDT_Record>
  {
    FilteredRecordQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtAllRecords; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class ArgumentQuery extends Query<HDT_Argument>
  {
    public ArgumentQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtArguments; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredArgumentQuery extends FilteredQuery<HDT_Argument>
  {
    FilteredArgumentQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtArguments; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class ConceptQuery extends Query<HDT_Concept>
  {
    public ConceptQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtConcepts; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredConceptQuery extends FilteredQuery<HDT_Concept>
  {
    FilteredConceptQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtConcepts; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class DebateQuery extends Query<HDT_Debate>
  {
    public DebateQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtDebates; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredDebateQuery extends FilteredQuery<HDT_Debate>
  {
    FilteredDebateQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtDebates; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class MiscFileQuery extends Query<HDT_MiscFile>
  {
    public MiscFileQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtFiles; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredMiscFileQuery extends FilteredQuery<HDT_MiscFile>
  {
    FilteredMiscFileQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtFiles; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class FolderQuery extends Query<HDT_Folder>
  {
    public FolderQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtFolders; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredFolderQuery extends FilteredQuery<HDT_Folder>
  {
    FilteredFolderQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtFolders; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class InstitutionQuery extends Query<HDT_Institution>
  {
    public InstitutionQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtInstitutions; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredInstitutionQuery extends FilteredQuery<HDT_Institution>
  {
    FilteredInstitutionQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtInstitutions; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class InvestigationQuery extends Query<HDT_Investigation>
  {
    public InvestigationQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtInvestigations; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredInvestigationQuery extends FilteredQuery<HDT_Investigation>
  {
    FilteredInvestigationQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtInvestigations; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class NoteQuery extends Query<HDT_Note>
  {
    public NoteQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtNotes; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredNoteQuery extends FilteredQuery<HDT_Note>
  {
    FilteredNoteQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtNotes; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class PersonQuery extends Query<HDT_Person>
  {
    public PersonQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtPersons; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredPersonQuery extends FilteredQuery<HDT_Person>
  {
    FilteredPersonQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtPersons; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class PositionQuery extends Query<HDT_Position>
  {
    public PositionQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtPositions; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredPositionQuery extends FilteredQuery<HDT_Position>
  {
    FilteredPositionQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtPositions; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract static class WorkQuery extends Query<HDT_Work>
  {
    public WorkQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtWorks; }

    @Override protected final QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
    {
      return super.getSource(origSource, op1, op2, op3);
    }
  }

  public abstract static class FilteredWorkQuery extends FilteredQuery<HDT_Work>
  {
    FilteredWorkQuery(int queryID, String description) { super(queryID, description); }

    @Override public boolean show(QueryType queryType, RecordType recordType) { return queryType == qtWorks; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
