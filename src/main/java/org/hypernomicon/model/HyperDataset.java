/*
 * Copyright 2015-2018 Jason Winning
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
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Record.HyperDataCategory;
import org.hypernomicon.model.relations.RelationSet;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;

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
    
    public Iterator<HDT_DT> keyIterator()        { return new CoreIterator(this, true); }
    public Iterator<HDT_DT> idIterator()         { return new CoreIterator(this, false); }
  
    @Override public Iterator<HDT_DT> iterator() { return idIterator(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final class CoreIterator implements Iterator<HDT_DT>
  {
    private CoreAccessor coreAccessor;
    private int nextNdx = 0;
    private boolean byKey;
  
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
    
    return (core.containsID(id) == false); 
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  void bringAllRecordsOnline() throws RelationCycleException, HDB_InternalError, SearchKeyException, HubChangedException, TerminateTaskException
  {
    if (online) throw new HDB_InternalError(89842);
    
    if (core.idCount() == 0)
    {
      online = true;
      return;
    }
    
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

  void assignIDs()
  {
    int nextID = 1;
    
    for (HDT_DT record : needIDs)
    {
      while (idAvailable(nextID) == false)
        nextID++;
      
      record.assignID(nextID);
      try { add(record); } catch (DuplicateRecordException e) { noOp(); }      
    }
    
    needIDs.clear();
  }
   
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void add(HDT_DT record) throws DuplicateRecordException
  {
    if (record.getID() == -1)
      needIDs.add(record);
    else
    {
      if (core.containsID(record.getID()))
        throw new DuplicateRecordException(record.getID(), record.getType());
      
      core.add(record.getID(), record.makeSortKey(), record);
      RelationSet.addOrphan(record);
    }
  }
  
//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
   
  HDT_DT createNewRecord(HDT_RecordState recordState, boolean bringOnline) throws DuplicateRecordException, RelationCycleException, HDB_InternalError, SearchKeyException, HubChangedException
  {
    HDT_DT record;
    Instant nowDate;
    
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

    record = HDT_Record.createRecord(recordState, this);
    
    if (type.getDisregardDates() == false)
    {
      nowDate = Instant.now();
      
      if (recordState.creationDate == null) recordState.creationDate = nowDate;      
      if (recordState.modifiedDate == null) recordState.modifiedDate = nowDate;      
      if (recordState.viewDate     == null) recordState.viewDate     = nowDate;
    }
    
    add(record);
    
    if (bringOnline)
      record.bringStoredCopyOnline();

    return record;  
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void writeToXML(StringBuilder xml) throws HDB_InternalError, TerminateTaskException
  {    
    boolean write;
    int ndx = 0;
    
    if (core.idCount() == 0) return;
    
    for (HDT_DT record : getAccessor())
    {
      write = !isUnstoredRecord(record.getID(), type);
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
