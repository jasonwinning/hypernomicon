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

package org.hypernomicon.model.data;

import static org.hypernomicon.model.HDI_Schema.HyperDataCategory.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.AbstractHyperDB;
import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.RelationSet;

import java.lang.reflect.InvocationTargetException;
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
  public void resolvePointers() throws HDB_InternalError  { core.resolvePointers(); }
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

      db.changeRecordID(record, idToAssign);

      idToAssign = -1;

      try { add(record); } catch (DuplicateRecordException e) { throw new AssertionError(getThrowableMessage(e), e); }
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
  private HDT_DT createRecord(RecordState recordState)
  {
    Class<HDT_DT> klazz = (Class<HDT_DT>) recordState.type.getRecordClass();

    try
    {
      return klazz.getConstructor(RecordState.class, DatasetAccessor.class).newInstance(recordState, core);
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

  public void writeToXML(StringBuilder xml, HyperTask task) throws HDB_InternalError, CancelledTaskException
  {
    if (core.isEmpty()) return;

    int ndx = 0;

    for (HDT_DT record : getAccessor())
    {
      ndx++;

      if (isUnstoredRecord(record .getID(), type) || ((record.getType() == hdtFolder) && ((HDT_Folder)record).hasNoNonFolderRecordDependencies()))
        continue;

      record.saveToStoredState();
      record.writeStoredStateToXML(xml);

      if (task != null)
        task.updateProgress(task.completedCount + ndx, task.totalCount);
    }

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
