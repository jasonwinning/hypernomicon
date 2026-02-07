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

package org.hypernomicon.model.data;

import static org.hypernomicon.model.HDI_Schema.HyperDataCategory.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.AbstractHyperDB;
import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_Verdict.HDT_ArgumentVerdict;
import org.hypernomicon.model.records.HDT_Verdict.HDT_PositionVerdict;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.RelationSet;
import org.hypernomicon.model.unities.HDT_Hub;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

//---------------------------------------------------------------------------

public final class HyperDataset<HDT_DT extends HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperCore<HDT_DT> core;
  private final RecordType type;
  private final Collection<HDT_DT> needIDs = new ArrayList<>();
  private final Map<Tag, HDI_Schema> tagToSchema = new LinkedHashMap<>();
  private Tag mainTextTag = null;
  private int idToAssign = -1;

  public boolean online = false;

//---------------------------------------------------------------------------

  private HyperDataset(RecordType type)
  {
    this.type = type;

    core = new HyperCore<>(type);
  }

  public int getNextID()                                  { int id = 0; while (true) if (idAvailable(++id)) return id; }
  public HDI_Schema getSchema(Tag tag)                    { return tagToSchema.get(tag); }
  public Collection<HDI_Schema> getSchemas()              { return tagToSchema.values(); }
  public Tag getMainTextTag()                             { return mainTextTag; }

  /**
   * For each record, calls its resolvePointers method (which may cause the
   * record to become expired), and removes the record from the core ("deletes" it) if it
   * became expired.<br>
   * HDT_Record.expire is where delete handlers get called.
   * @throws HDB_InternalError If the record somehow entered the inconsistent
   * state of its ID being -1 but the expired flag was not set.
   */
  public void resolvePointers() throws HDB_InternalError  { core.resolvePointers(); }

  /**
   * Removes an expired folder from this dataset directly, without iterating over all records.
   * This is used to optimize folder deletion, which doesn't need full pointer resolution.
   * @param id The ID of the record (must be captured before the record was expired, since expire sets ID to -1)
   * @param folder The expired folder to remove (used to verify it is actually an expired folder)
   * @throws HDB_InternalError if the record is not expired or not a folder
   */
  public void removeExpiredFolder(int id, HDT_DT folder) throws HDB_InternalError  { core.removeExpiredFolder(id, folder); }

  public DatasetAccessor<HDT_DT> getAccessor()            { return core; }

  public boolean idAvailable(int id)                      { return (isUnstoredRecord(id, type) == false) && (core.containsID(id) == false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <HDT_DT extends HDT_Record> HyperDataset<HDT_DT> create(RecordType type)
  {
    if (AbstractHyperDB.isCreatingDataset() == false)
      throw new IllegalStateException("HyperDataset can only be created by HyperDB");

    return new HyperDataset<>(type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeAll(boolean leaveOnline)
  {
    needIDs.clear();
    core.clearInternal();
    online = online && leaveOnline;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void assignIDs() throws HDB_InternalError
  {
    int nextID = 1;

    for (HDT_DT record : needIDs)
    {
      while (idAvailable(nextID) == false)
        nextID++;

      idToAssign = nextID++;

      try
      {
        db.changeRecordID(record, idToAssign);
      }
      catch (HDB_InternalError e)
      {
        throw e;
      }
      catch (HyperDataException e)
      {
        throw new HDB_InternalError(49827, e);
      }

      idToAssign = -1;

      try { add(record); } catch (DuplicateRecordException e) { throw newAssertionError(e); }
    }

    needIDs.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void changeRecordID(int oldID, int newID) throws HDB_InternalError
  {
    if (oldID < 1)
    {
      if (newID < 1)
        throw new HDB_InternalError(63869);

      if (newID != idToAssign)
        throw new HDB_InternalError(74102);

      return;  // Return because we are downstream from where assignIDs calls db.changeRecordID;
    }          // the ID will be added to the core in the call to add(record)

    core.changeRecordID(oldID, newID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void add(HDT_DT record) throws DuplicateRecordException
  {
    int id = record.getID();

    if (core.containsID(id))
      throw new DuplicateRecordException(id, record.getType());

    core.add(id, record.makeSortKey(), record);
    RelationSet.addOrphanToAll(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_DT createNewRecord(RecordState recordState, boolean bringOnline) throws DuplicateRecordException, RelationCycleException, HDB_InternalError, SearchKeyException, RestoreException
  {
    if (bringOnline)
    {
      if (online == false)
        throw new HDB_InternalError(89843);

      if (recordState.id < 1)
        for (recordState.id = 1; idAvailable(recordState.id) == false; recordState.id++);
    }

    HDT_DT record = createRecord(recordState);

    if (type.getDisregardDates() == false)
    {
      Instant nowDate = Instant.now();

      if (recordState.creationDate == null) recordState.creationDate = nowDate;
      if (recordState.modifiedDate == null) recordState.modifiedDate = nowDate;
      if (recordState.viewDate     == null) recordState.viewDate     = nowDate;
    }

    if (record.getID() == -1)
      needIDs.add(record);
    else
      add(record);

    if (bringOnline)
      record.bringStoredCopyOnline(false);

    return record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private HDT_DT createRecord(RecordState recordState) { return (HDT_DT) switch (recordState.type)
  {
    case hdtArgument        -> new HDT_Argument        (recordState, (DatasetAccessor<HDT_Argument       >) core);
    case hdtArgumentVerdict -> new HDT_ArgumentVerdict (recordState, (DatasetAccessor<HDT_ArgumentVerdict>) core);
    case hdtConcept         -> new HDT_Concept         (recordState, (DatasetAccessor<HDT_Concept        >) core);
    case hdtConceptSense    -> new HDT_ConceptSense    (recordState, (DatasetAccessor<HDT_ConceptSense   >) core);
    case hdtCountry         -> new HDT_Country         (recordState, (DatasetAccessor<HDT_Country        >) core);
    case hdtDebate          -> new HDT_Debate          (recordState, (DatasetAccessor<HDT_Debate         >) core);
    case hdtField           -> new HDT_Field           (recordState, (DatasetAccessor<HDT_Field          >) core);
    case hdtFileType        -> new HDT_FileType        (recordState, (DatasetAccessor<HDT_FileType       >) core);
    case hdtFolder          -> new HDT_Folder          (recordState, (DatasetAccessor<HDT_Folder         >) core);
    case hdtGlossary        -> new HDT_Glossary        (recordState, (DatasetAccessor<HDT_Glossary       >) core);
    case hdtHub             -> new HDT_Hub             (recordState, (DatasetAccessor<HDT_Hub            >) core);
    case hdtInstitution     -> new HDT_Institution     (recordState, (DatasetAccessor<HDT_Institution    >) core);
    case hdtInstitutionType -> new HDT_InstitutionType (recordState, (DatasetAccessor<HDT_InstitutionType>) core);
    case hdtInvestigation   -> new HDT_Investigation   (recordState, (DatasetAccessor<HDT_Investigation  >) core);
    case hdtMiscFile        -> new HDT_MiscFile        (recordState, (DatasetAccessor<HDT_MiscFile       >) core);
    case hdtNote            -> new HDT_Note            (recordState, (DatasetAccessor<HDT_Note           >) core);
    case hdtPerson          -> new HDT_Person          (recordState, (DatasetAccessor<HDT_Person         >) core);
    case hdtPersonGroup     -> new HDT_PersonGroup     (recordState, (DatasetAccessor<HDT_PersonGroup    >) core);
    case hdtPersonStatus    -> new HDT_PersonStatus    (recordState, (DatasetAccessor<HDT_PersonStatus   >) core);
    case hdtPosition        -> new HDT_Position        (recordState, (DatasetAccessor<HDT_Position       >) core);
    case hdtPositionVerdict -> new HDT_PositionVerdict (recordState, (DatasetAccessor<HDT_PositionVerdict>) core);
    case hdtRank            -> new HDT_Rank            (recordState, (DatasetAccessor<HDT_Rank           >) core);
    case hdtRegion          -> new HDT_Region          (recordState, (DatasetAccessor<HDT_Region         >) core);
    case hdtSubfield        -> new HDT_Subfield        (recordState, (DatasetAccessor<HDT_Subfield       >) core);
    case hdtTerm            -> new HDT_Term            (recordState, (DatasetAccessor<HDT_Term           >) core);
    case hdtWork            -> new HDT_Work            (recordState, (DatasetAccessor<HDT_Work           >) core);
    case hdtWorkFile        -> new HDT_WorkFile        (recordState, (DatasetAccessor<HDT_WorkFile       >) core);
    case hdtWorkLabel       -> new HDT_WorkLabel       (recordState, (DatasetAccessor<HDT_WorkLabel      >) core);
    case hdtWorkType        -> new HDT_WorkType        (recordState, (DatasetAccessor<HDT_WorkType       >) core);

    default -> throw new AssertionError("Cannot create record for type: " + recordState.type.name());
  }; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void writeToXML(StringBuilder xml, HyperTask task) throws HDB_InternalError, CancelledTaskException
  {
    if (core.isEmpty()) return;

    boolean wroteAny = false;
    int ndx = 0;

    for (HDT_DT record : getAccessor())
    {
      ndx++;

      if (isUnstoredRecord(record.getID(), type) || ((record instanceof HDT_Folder folder) && (folder.getPath().isInUse() == false)))
        continue;

      record.saveToStoredState();
      record.writeStoredStateToXML(xml);
      wroteAny = true;

      if (task != null)
        task.updateProgress(task.completedCount + ndx, task.totalCount);
    }

    if (wroteAny)
      xml.append(System.lineSeparator()).append(System.lineSeparator()).append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addSchema(HDI_Schema schema)
  {
    for (Tag tag : schema.tags())
    {
      assert(tagToSchema.containsKey(tag) == false);

      if (tag != tagMainText)
        tagToSchema.put(tag, schema);

      if (schema.category() == hdcMainTextAndHub)
      {
        switch (tag)
        {
          case tagMainText : case tagDisplayRecord : case tagKeyWork : case tagHub :
            break;

          default :
            mainTextTag = tag;
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<Tag> getTags(boolean includeHub, boolean substituteMainText)
  {
    return tagToSchema.keySet().stream().filter(tag -> includeHub || (tag != tagHub))
                                        .map(tag -> substituteMainText && (tag == mainTextTag) ? tagMainText : tag)
                                        .collect(Collectors.toSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
