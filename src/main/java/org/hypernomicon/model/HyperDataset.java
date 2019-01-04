/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.model;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_Record.HyperDataCategory;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.RelationSet;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.NoSuchElementException;
import java.util.Set;

public final class HyperDataset<HDT_DT extends HDT_Base>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final class CoreAccessor implements Iterable<HDT_DT>
  {
    private HyperCore<HDT_DT> core;

    private CoreAccessor(HyperCore<HDT_DT> core) { this.core = core; }

    public int size()                            { return core.idCount(); }
    public boolean containsID(int id)            { return core.containsID(id); }

    public String getKeyByID(int id)             { return core.getKeyByID(id); }

    public int getKeyNdxByID(int id)             { return core.getKeyNdxByID(id); }
    public int getKeyNdxByIDNdx(int ndx)         { return core.getKeyNdxByID(core.getIDbyIDNdx(ndx)); }

    public int getIDbyKeyNdx(int ndx)            { return core.getIDbyKeyNdx(ndx); }
    public int getIDbyIDNdx(int ndx)             { return core.getIDbyIDNdx(ndx); }

    public int getIDNdxByID(int id)              { return core.getIDNdxByID(id); }
    public int getIDNdxByKeyNdx(int ndx)         { return core.getIDNdxByID(core.getIDbyKeyNdx(ndx)); }

    public HDT_DT getByID(int id)                { return core.getRecordByID(id); }
    public HDT_DT getByIDNdx(int ndx)            { return core.getRecordByID(core.getIDbyIDNdx(ndx)); }
    public HDT_DT getByKeyNdx(int ndx)           { return core.getRecordByID(core.getIDbyKeyNdx(ndx)); }

    public Iterable<HDT_DT> keyIterable()        { return this::keyIterator; }
    public Iterator<HDT_DT> keyIterator()        { return new CoreIterator(this, true); }
    public Iterator<HDT_DT> idIterator()         { return new CoreIterator(this, false); }

    @Override public Iterator<HDT_DT> iterator() { return idIterator(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final class CoreIterator implements Iterator<HDT_DT>
  {
    private final CoreAccessor coreAccessor;
    private int nextNdx = 0;
    private final boolean byKey;

    @Override public boolean hasNext() { return nextNdx < coreAccessor.size(); }
    @Override public void remove()     { throw new UnsupportedOperationException("Internal error: A 'remove' call was made to a core iterator."); }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    public CoreIterator(CoreAccessor coreAccessor, boolean byKey)
    {
      this.coreAccessor = coreAccessor;
      this.byKey = byKey;
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    @Override public HDT_DT next()
    {
      if (!hasNext()) throw new NoSuchElementException();

      if (byKey) return coreAccessor.getByKeyNdx(nextNdx++);
      else       return coreAccessor.getByIDNdx(nextNdx++);
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final HyperCore<HDT_DT> core = new HyperCore<HDT_DT>();
  private final HDT_RecordType type;
  private final ArrayList<HDT_DT> needIDs = new ArrayList<HDT_DT>();
  private final LinkedHashMap<Tag, HDI_Schema> tagToSchema = new LinkedHashMap<>();
  private Tag mainTextTag = null;
  private boolean online = false;
  private HDT_Base recordToAssign = null;
  private int idToAssign = -1;

//---------------------------------------------------------------------------

  public HyperDataset(HDT_RecordType type)
  {
    this.type = type;
  }

  // This should ONLY ever be called by HDT_Record.updateSortKey!!!
  public void updateSortKey(String newKey, int id) { core.setKey(id, newKey); }

  HDT_RecordType getType()                         { return type; }
  int getNextID()                                  { int id = 0; while (true) if (idAvailable(++id)) return id; }
  HDI_Schema getSchema(Tag tag)                    { return tagToSchema.get(tag); }
  Collection<HDI_Schema> getSchemas()              { return tagToSchema.values(); }
  Set<Tag> getTags()                               { return tagToSchema.keySet(); }
  void resolvePointers() throws HDB_InternalError  { core.resolvePointers(); }
  CoreAccessor getAccessor()                       { return new CoreAccessor(core); }

  public void changeRecordID(int oldID, int newID) { core.changeRecordID(oldID, newID); }
  public String getKeyByID(int id)                 { return core.getKeyByID(id); }
  public Tag getMainTextTag()                      { return mainTextTag; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int recordIDtoAssign(HDT_Base record) throws HDB_InternalError
  {
    if ((record == null) || (record != recordToAssign) || (idToAssign < 1))
      throw new HDB_InternalError(63869);

    int id = idToAssign;
    idToAssign = -1;
    recordToAssign = null;

    return id;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void removeAll(boolean leaveOnline)
  {
    needIDs.clear();
    core.clear();
    online = leaveOnline;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean idAvailable(int id)
  {
    if (isUnstoredRecord(id, type))
      return false;

    return core.containsID(id) == false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void bringAllRecordsOnline() throws RelationCycleException, HDB_InternalError, SearchKeyException, HubChangedException, TerminateTaskException
  {
    if (online) throw new HDB_InternalError(89842);

    for (HDT_DT record : getAccessor())
    {
      if (db.task.isCancelled()) throw new TerminateTaskException();

      db.curTaskCount++;

      record.bringStoredCopyOnline();
      db.addToInitialNavList(record);

      if ((db.curTaskCount % 10) == 0)
        db.task.updateProgress(db.curTaskCount, db.totalTaskCount);
    }

    online = true;
    return;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void assignIDs() throws HDB_InternalError
  {
    int nextID = 1;

    for (HDT_DT record : needIDs)
    {
      while (idAvailable(nextID) == false)
        nextID++;

      idToAssign = nextID++;
      recordToAssign = record;
      record.assignID();

      try { add(record); } catch (DuplicateRecordException e) { noOp(); }
    }

    needIDs.clear();
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

  HDT_DT createNewRecord(HDT_RecordState recordState, boolean bringOnline) throws DuplicateRecordException, RelationCycleException, HDB_InternalError, SearchKeyException, HubChangedException
  {
    if (bringOnline)
    {
      if (online == false)
        throw new HDB_InternalError(89843);

      if (recordState.id < 1)
      {
        recordState.id = 1;

        while (idAvailable(recordState.id) == false)
          recordState.id++;
      }
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
      record.bringStoredCopyOnline();

    return record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private final HDT_DT createRecord(HDT_RecordState recordState)
  {
    switch (recordState.type)
    {
      case hdtPerson :          return (HDT_DT) new HDT_Person          (recordState, (HyperDataset<HDT_Person>)          this);
      case hdtPersonStatus :    return (HDT_DT) new HDT_PersonStatus    (recordState, (HyperDataset<HDT_PersonStatus>)    this);
      case hdtRank :            return (HDT_DT) new HDT_Rank            (recordState, (HyperDataset<HDT_Rank>)            this);
      case hdtInstitution :     return (HDT_DT) new HDT_Institution     (recordState, (HyperDataset<HDT_Institution>)     this);
      case hdtInstitutionType : return (HDT_DT) new HDT_InstitutionType (recordState, (HyperDataset<HDT_InstitutionType>) this);
      case hdtInvestigation :   return (HDT_DT) new HDT_Investigation   (recordState, (HyperDataset<HDT_Investigation>)   this);
      case hdtDebate :          return (HDT_DT) new HDT_Debate          (recordState, (HyperDataset<HDT_Debate>)          this);
      case hdtArgument :        return (HDT_DT) new HDT_Argument        (recordState, (HyperDataset<HDT_Argument>)        this);
      case hdtPosition :        return (HDT_DT) new HDT_Position        (recordState, (HyperDataset<HDT_Position>)        this);
      case hdtTerm :            return (HDT_DT) new HDT_Term            (recordState, (HyperDataset<HDT_Term>)            this);
      case hdtConcept :         return (HDT_DT) new HDT_Concept         (recordState, (HyperDataset<HDT_Concept>)         this);
      case hdtField :           return (HDT_DT) new HDT_Field           (recordState, (HyperDataset<HDT_Field>)           this);
      case hdtSubfield :        return (HDT_DT) new HDT_Subfield        (recordState, (HyperDataset<HDT_Subfield>)        this);
      case hdtWorkType :        return (HDT_DT) new HDT_WorkType        (recordState, (HyperDataset<HDT_WorkType>)        this);
      case hdtWorkLabel :       return (HDT_DT) new HDT_WorkLabel       (recordState, (HyperDataset<HDT_WorkLabel>)       this);
      case hdtWork :            return (HDT_DT) new HDT_Work            (recordState, (HyperDataset<HDT_Work>)            this);
      case hdtState :           return (HDT_DT) new HDT_State           (recordState, (HyperDataset<HDT_State>)           this);
      case hdtCountry :         return (HDT_DT) new HDT_Country         (recordState, (HyperDataset<HDT_Country>)         this);
      case hdtPositionVerdict : return (HDT_DT) new HDT_PositionVerdict (recordState, (HyperDataset<HDT_PositionVerdict>) this);
      case hdtArgumentVerdict : return (HDT_DT) new HDT_ArgumentVerdict (recordState, (HyperDataset<HDT_ArgumentVerdict>) this);
      case hdtMiscFile :        return (HDT_DT) new HDT_MiscFile        (recordState, (HyperDataset<HDT_MiscFile>)        this);
      case hdtNote :            return (HDT_DT) new HDT_Note            (recordState, (HyperDataset<HDT_Note>)            this);
      case hdtHub :             return (HDT_DT) new HDT_Hub             (recordState, (HyperDataset<HDT_Hub>)             this);
      case hdtFileType :        return (HDT_DT) new HDT_FileType        (recordState, (HyperDataset<HDT_FileType>)        this);
      case hdtPersonGroup :     return (HDT_DT) new HDT_PersonGroup     (recordState, (HyperDataset<HDT_PersonGroup>)     this);
      case hdtWorkFile :        return (HDT_DT) new HDT_WorkFile        (recordState, (HyperDataset<HDT_WorkFile>)        this);
      case hdtFolder :          return (HDT_DT) new HDT_Folder          (recordState, (HyperDataset<HDT_Folder>)          this);
      case hdtGlossary :        return (HDT_DT) new HDT_Glossary        (recordState, (HyperDataset<HDT_Glossary>)        this);

      default :                 return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void writeToXML(StringBuilder xml) throws HDB_InternalError, TerminateTaskException
  {
    if (core.idCount() == 0) return;

    int ndx = 0;

    for (HDT_DT record : getAccessor())
    {
      boolean write = !isUnstoredRecord(record.getID(), type);
      ndx++;

      if (write && (type == hdtFolder))
      {
        HDT_Folder folder = HDT_Folder.class.cast(record);

        if (isProtectedRecord(record.getID(), type) == false)
          if (folder.hasNoNonFolderRecordDependencies())
            write = false;
      }

      if (write)
      {
        record.saveToStoredState();
        record.writeStoredStateToXML(xml);
        db.task.updateProgress(db.curTaskCount + ndx, db.totalTaskCount);
      }

      if (db.task.isCancelled()) throw new TerminateTaskException();
    }

    xml.append(System.lineSeparator());
    xml.append(System.lineSeparator());
    xml.append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addSchema(HDI_Schema schema, Tag[] tags) throws HDB_InternalError
  {
    for (Tag tag : tags)
    {
      if (tagToSchema.containsKey(tag))
        throw new HDB_InternalError(98921);

      tagToSchema.put(tag, schema);
    }

    if (schema.getCategory() == HyperDataCategory.hdcConnector)
      mainTextTag = tags[0];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
