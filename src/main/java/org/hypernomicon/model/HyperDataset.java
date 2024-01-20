/*
 * Copyright 2015-2024 Jason Winning
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
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory.*;

import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.RelationSet;

import java.lang.reflect.InvocationTargetException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class HyperDataset<HDT_DT extends HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final class CoreAccessor implements Iterable<HDT_DT>
  {
    private CoreAccessor()                       { }

    public int size()                            { return core.size(); }
    public Stream<HDT_DT> stream()               { return core.stream(); }

    public int getKeyNdxByID(int id)             { return core.getKeyNdxByID(id); }
    public int getIDNdxByID(int id)              { return core.getIDNdxByID(id); }

    public HDT_DT getByID(int id)                { return core.getRecordByID(id); }
    public HDT_DT getByKeyNdx(int ndx)           { return core.getRecordByID(core.getIDbyKeyNdx(ndx)); }
    private HDT_DT getByIDNdx(int ndx)           { return core.getRecordByID(core.getIDbyIDNdx(ndx)); }

    public Iterable<HDT_DT> keyIterable()        { return this::keyIterator; }
    public Iterator<HDT_DT> keyIterator()        { return new CoreIterator(this, true); }

    @Override public Iterator<HDT_DT> iterator() { return new CoreIterator(this, false); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final class CoreIterator implements Iterator<HDT_DT>
  {
    private final CoreAccessor coreAccessor;
    private final boolean byKey;

    private int nextNdx = 0;

    @Override public boolean hasNext() { return nextNdx < coreAccessor.size(); }
    @Override public void remove()     { throw new UnsupportedOperationException("Internal error: A 'remove' call was made to a core iterator."); }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private CoreIterator(CoreAccessor coreAccessor, boolean byKey)
    {
      this.coreAccessor = coreAccessor;
      this.byKey = byKey;
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    @Override public HDT_DT next()
    {
      if (hasNext() == false) throw new NoSuchElementException();

      return byKey ? coreAccessor.getByKeyNdx(nextNdx++) : coreAccessor.getByIDNdx(nextNdx++);
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final HyperCore<HDT_DT> core = new HyperCore<>();
  private final RecordType type;
  private final List<HDT_DT> needIDs = new ArrayList<>();
  private final Map<Tag, HDI_Schema> tagToSchema = new LinkedHashMap<>();
  private Tag mainTextTag = null;
  private boolean online = false;
  private HDT_Record recordToAssign = null;
  private int idToAssign = -1;

//---------------------------------------------------------------------------

  HyperDataset(RecordType type)
  {
    this.type = type;
  }

  public void updateSortKey(HDT_Record record)     { core.setKey(record.getID(), record.makeSortKey()); }
  int getNextID()                                  { int id = 0; while (true) if (idAvailable(++id)) return id; }
  HDI_Schema getSchema(Tag tag)                    { return tagToSchema.get(tag); }
  Collection<HDI_Schema> getSchemas()              { return tagToSchema.values(); }
  Tag getMainTextTag()                             { return mainTextTag; }
  void resolvePointers() throws HDB_InternalError  { core.resolvePointers(); }
  CoreAccessor getAccessor()                       { return new CoreAccessor(); }
  boolean idAvailable(int id)                      { return (isUnstoredRecord(id, type) == false) && (core.containsID(id) == false); }
  public String getKeyByID(int id)                 { return core.getKeyByID(id); }

  public void changeRecordID(int oldID, int newID) throws HDB_InternalError { core.changeRecordID(oldID, newID); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int recordIDtoAssign(HDT_Record record) throws HDB_InternalError
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
    online = online && leaveOnline;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void bringAllRecordsOnline() throws RelationCycleException, HDB_InternalError, SearchKeyException, RestoreException, CancelledTaskException
  {
    if (online) throw new HDB_InternalError(89842);

    for (HDT_DT record : getAccessor())
    {
      if (db.task.isCancelled()) throw new CancelledTaskException();

      record.bringStoredCopyOnline(false);
      db.addToInitialNavList(record);

      if ((++db.curTaskCount % 10) == 0)
        db.task.updateProgress(db.curTaskCount, db.totalTaskCount);
    }

    online = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void assignIDs() throws HDB_InternalError
  {
    int nextID = 1;

    for (HDT_DT record : needIDs)
    {
      for (; idAvailable(nextID) == false; nextID++);

      idToAssign = nextID++;
      recordToAssign = record;
      record.assignID();

      try { add(record); } catch (DuplicateRecordException e) { throw new AssertionError(e.getMessage(), e); }
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

  HDT_DT createNewRecord(RecordState recordState, boolean bringOnline) throws DuplicateRecordException, RelationCycleException, HDB_InternalError, SearchKeyException, RestoreException
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
  private HDT_DT createRecord(RecordState recordState)
  {
    Class<HDT_DT> klazz = (Class<HDT_DT>) recordState.type.getRecordClass();

    try
    {
      return klazz.getConstructor(RecordState.class, getClass()).newInstance(recordState, this);
    }
    catch (NoSuchMethodException    | InstantiationException    | IllegalAccessException |
           IllegalArgumentException | InvocationTargetException | SecurityException e)
    {
      e.printStackTrace();
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void writeToXML(StringBuilder xml) throws HDB_InternalError, CancelledTaskException
  {
    if (core.size() == 0) return;

    int ndx = 0;

    for (HDT_DT record : getAccessor())
    {
      boolean write = !isUnstoredRecord(record.getID(), type);
      ndx++;

      if (write && (type == hdtFolder))
      {
        if (((HDT_Folder)record).hasNoNonFolderRecordDependencies())
          write = false;
      }

      if (write)
      {
        record.saveToStoredState();
        record.writeStoredStateToXML(xml);
        db.task.updateProgress(db.curTaskCount + ndx, db.totalTaskCount);
      }

      if (db.task.isCancelled()) throw new CancelledTaskException();
    }

    xml.append(System.lineSeparator()).append(System.lineSeparator()).append(System.lineSeparator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addSchema(HDI_Schema schema) throws HDB_InternalError
  {
    for (Tag tag : schema.getTags())
    {
      if (tagToSchema.containsKey(tag))
        throw new HDB_InternalError(98921);

      if (tag != tagMainText)
        tagToSchema.put(tag, schema);

      if (schema.getCategory() == hdcMainTextAndHub)
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

  Set<Tag> getTags(boolean includeHub, boolean substituteMainText)
  {
    return tagToSchema.keySet().stream().filter(tag -> includeHub || (tag != tagHub))
                                        .map(tag -> substituteMainText && (tag == mainTextTag) ? tagMainText : tag)
                                        .collect(Collectors.toSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
