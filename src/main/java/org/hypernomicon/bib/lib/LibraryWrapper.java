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

package org.hypernomicon.bib.lib;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import org.apache.http.client.methods.HttpUriRequest;
import org.json.simple.parser.ParseException;

import com.google.common.collect.EnumHashBiMap;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.BibData.EntryType;
import org.hypernomicon.bib.BibEntryRow;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.model.HyperDB.db;

public abstract class LibraryWrapper<BibEntry_T extends BibEntry, BibCollection_T extends BibCollection>
{
  //---------------------------------------------------------------------------
  
  public static enum LibraryType 
  { 
    ltZotero("zotero", "Zotero"), 
    ltMendeley("mendeley", "Mendeley");
    
    private LibraryType(String descriptor, String userReadableName) { this.descriptor = descriptor; this.userReadableName = userReadableName; }
    private String descriptor, userReadableName;        
    
    public String getDescriptor()       { return descriptor; }
    public String getUserReadableName() { return userReadableName; }
    
    public static LibraryType getByDescriptor(String descriptor)
    {
      for (LibraryType type : LibraryType.values())
        if (type.descriptor.equals(descriptor))
          return type;
      
      return null;
    }
  };
  
  //---------------------------------------------------------------------------
  
  @FunctionalInterface public static interface KeyChangeHandler { public void handle(String oldKey, String newKey); }
  
  public static abstract class SyncTask extends HyperTask 
  { 
    protected boolean changed = false;
    public boolean getChanged() { return changed; }
  }

  public LibraryWrapper()
  {
    keyToAllEntry = new HashMap<>();
    keyToTrashEntry = new HashMap<>();
    keyToColl = new HashMap<>();
  }
  
  protected final Map<String, BibEntry_T> keyToAllEntry, keyToTrashEntry;
  protected final Map<String, BibCollection_T> keyToColl;
  
  protected SyncTask syncTask = null;
  protected HttpUriRequest request = null;
  private KeyChangeHandler keyChangeHndlr;

  public abstract SyncTask createNewSyncTask();
  public abstract void saveToDisk(FilePath filePath);
  public abstract void loadFromDisk(FilePath filePath) throws FileNotFoundException, IOException, ParseException;
  public abstract Set<BibEntry_T> getNonTrashEntries();
  public abstract Set<BibEntry_T> getCollectionEntries(String collKey);          
  public abstract Set<BibEntry_T> getUnsorted();
  public abstract BibEntry_T addEntry(EntryType newType);
  public abstract LibraryType type();
  public abstract EnumHashBiMap<EntryType, String> getEntryTypeMap();
  public abstract String getHtml(BibEntryRow row);
  
  public final Set<BibCollection_T> getColls()                  { return new LinkedHashSet<>(keyToColl.values()); }
  public final Set<BibEntry_T> getTrash()                       { return new LinkedHashSet<>(keyToTrashEntry.values()); }
  public final Set<BibEntry_T> getAllEntries()                  { return new LinkedHashSet<>(keyToAllEntry.values()); }
  public final Map<String, BibCollection> getKeyToColl()        { return Collections.unmodifiableMap(keyToColl); }
  public final void setKeyChangeHandler(KeyChangeHandler hndlr) { this.keyChangeHndlr = hndlr; }
  
  public BibEntry_T getEntryByKey(String key)                   { return keyToAllEntry.get(key); }
  
  //---------------------------------------------------------------------------  
  //---------------------------------------------------------------------------

  public final void stop()
  {
    if (syncTask != null)
      if (syncTask.isRunning())
        syncTask.cancel();
    
    if (request != null)
      request.abort();
  }

  //---------------------------------------------------------------------------  
  //---------------------------------------------------------------------------

  protected void updateKey(String oldKey, String newKey)
  {
    if (keyToAllEntry.containsKey(oldKey))
      keyToAllEntry.put(newKey, keyToAllEntry.remove(oldKey));

    if (keyToTrashEntry.containsKey(oldKey))
      keyToTrashEntry.put(newKey, keyToTrashEntry.remove(oldKey));
    
    if (keyToColl.containsKey(oldKey))
      keyToColl.put(newKey, keyToColl.remove(oldKey));
    
    HDT_Work work = db.getWorkByBibEntryKey(oldKey);
    
    if (work != null)
      work.setBibEntryKey(newKey);
    
    keyChangeHndlr.handle(oldKey, newKey);
  }
   
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
}
