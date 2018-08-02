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

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.mutable.MutableBoolean;

import javafx.application.Platform;
import org.hypernomicon.HyperTask;
import org.hypernomicon.model.HyperDB.DatabaseEvent;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.items.KeyWork;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.MainText.DisplayItem;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.BidiOneToManyRecordMap;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.noOp;
import static org.hypernomicon.model.items.MainText.DisplayItemType.*;

public class MentionsIndex
{
  private BidiOneToManyRecordMap mentionedInDescToMentioners;
  private BidiOneToManyRecordMap mentionedAnywhereToMentioners;
  private List<DatabaseEvent> ndxCompleteHandlers;
  private RebuildThread thread = null;
  private HyperTask task = null;
  private KeywordLinkList linkList;
  private EnumSet<HDT_RecordType> types;
  private double ctr, total;
  private boolean stopRequested = false;
  private ArrayList<String> strList = new ArrayList<String>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MentionsIndex(List<DatabaseEvent> ndxCompleteHandlers)
  {
    this.ndxCompleteHandlers = ndxCompleteHandlers;
    
    mentionedInDescToMentioners = new BidiOneToManyRecordMap();
    mentionedAnywhereToMentioners = new BidiOneToManyRecordMap();
    
    types = EnumSet.allOf(HDT_RecordType.class);
    types.remove(hdtNone);
    types.remove(hdtAuxiliary);
    types.remove(hdtHub);
    
    linkList = new KeywordLinkList();
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
 
  public void removeRecord(HDT_Base record)
  {
    if (thread != null)
      if (thread.isAlive())
      {
        startRebuild();
        return;
      }

    mentionedInDescToMentioners.removeRecord(record);
    mentionedAnywhereToMentioners.removeRecord(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateMentioner(HDT_Base record)
  {   
    if (thread != null)
      if (thread.isAlive())
      {
        startRebuild();
        return;
      }
    
    if (record.isUnitable())
    {
      HDT_RecordWithConnector uRecord = (HDT_RecordWithConnector) record;
      if (uRecord.isLinked())
      {              
        StrongLink link = uRecord.getLink();
        
        reindexMentioner(link.getNote());
        reindexMentioner(link.getLabel());
        reindexMentioner(link.getDebate());
        reindexMentioner(link.getPosition());
        reindexMentioner(link.getConcept());
        return;                    
      }
    }
    
    reindexMentioner(record);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void reindexMentioner(HDT_Base record)
  {
    int strNdx;
       
    if (record == null) return;
    
    strList.clear();

    record.getAllStrings(strList, true);
    if (strList.size() == 0)
      return;
    
    mentionedAnywhereToMentioners.removeReverseKey(record);
    mentionedInDescToMentioners.removeReverseKey(record);

    for (strNdx = 0; strNdx < strList.size(); strNdx++)
    {
      linkList.generate(strList.get(strNdx).toLowerCase());

      for (KeywordLink link : linkList.getLinks())
        mentionedAnywhereToMentioners.addForward(link.key.record, record);
    }
    
    if (record.hasMainText())
    {
      MainText mainText = ((HDT_RecordWithConnector)record).getMainText();
      String plainText = mainText.getPlain();
      
      if (plainText.length() > 0)
      {
        linkList.generate(plainText);
  
        for (KeywordLink link : linkList.getLinks())
          mentionedInDescToMentioners.addForward(link.key.record, record);
      }
      
      for (DisplayItem displayItem : mainText.getDisplayItemsUnmod())
        if (displayItem.type == diRecord)
        {
          mentionedAnywhereToMentioners.addForward(displayItem.record, record);
          mentionedInDescToMentioners.addForward(displayItem.record, record);
        }
        else if (displayItem.type == diKeyWorks)
        {
          for (KeyWork keyWork : mainText.getKeyWorks())
          {
            HDT_RecordWithPath keyWorkRecord = keyWork.getRecord();
            
            mentionedAnywhereToMentioners.addForward(keyWorkRecord, record);
            mentionedInDescToMentioners.addForward(keyWorkRecord, record);
          }
        }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean waitUntilRebuildIsDone()
  {
    if (isRebuilding() == false) return true;
    
    HyperTask.performTaskWithProgressDialog(task);
    
    return task.isDone();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isRebuilding()
  {
    if (thread == null) return false;
    return thread.isAlive();    
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  class RebuildThread extends Thread
  {
    RebuildThread(HyperTask task)
    {
      super(task);
      setDaemon(true);
      task.setThread(this);
      start();
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean startRebuild()
  {
    stopRebuild();
    
    task = new HyperTask()
    {
      @Override protected void done()
      {       
        Thread oldThread = getThread();
        super.done();
        
        Platform.runLater(() ->
        {
          try { oldThread.join(); } catch (InterruptedException e) { noOp(); }
          
          ndxCompleteHandlers.forEach(DatabaseEvent::handle);
        });
      }
      
      @Override protected Boolean call() throws Exception
      {
        updateMessage("The requested operation will be performed after indexing has completed...");
        
        int recordNdx;
        HDT_Base record;
        
        mentionedInDescToMentioners.clear();
        mentionedAnywhereToMentioners.clear();
        
        ctr = -1.0; total = 0.0;
        for (HDT_RecordType type : types)
          total += db.records(type).size();
        
        for (HDT_RecordType type : types)
        {
          for (recordNdx = 0; recordNdx < db.records(type).size(); recordNdx++)
          {
            record = db.records(type).getByIDNdx(recordNdx);
            ctr++;
            
            if ((((int)ctr) % 50) == 0)
            {
              if (stopRequested)
              {
                updateProgress(total, total);
                stopRequested = false;
                return true;
              }
              
              updateProgress(ctr, total);
            }
            
            try
            {
              reindexMentioner(record);
            }
            catch (Exception e)
            {
              e.printStackTrace();
              throw(e);
            }
          }
        }
        
        updateProgress(total, total);
        return true;
      }
    };
    
    task.progressProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue.doubleValue() == 1.0)
        Platform.runLater(() -> ui.updateProgress("", -1.0));
      else
        Platform.runLater(() -> ui.updateProgress("Indexing:", ctr / total));
    });
    
    thread = new RebuildThread(task);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void stopRebuild()
  {
    if (thread != null)
      if (thread.isAlive())
      {
        stopRequested = true;
        try { thread.join(); } catch (InterruptedException e) { noOp(); }
      }    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDT_Base> getMentionerSet(HDT_Base target, boolean descOnly)
  {
    return getMentionerSet(target, descOnly, new MutableBoolean(false));
  }
  
  public Set<HDT_Base> getMentionerSet(HDT_Base target, boolean descOnly, MutableBoolean choseNotToWait)
  {
    choseNotToWait.setValue(!waitUntilRebuildIsDone());
    if (choseNotToWait.isTrue())
      return null;
    
    if (descOnly)
      return mentionedInDescToMentioners.getForwardSet(target);
    
    return mentionedAnywhereToMentioners.getForwardSet(target);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDT_Base> getMentionedSet(HDT_Base mentioner, boolean descOnly)
  {
    return getMentionedSet(mentioner, descOnly, new MutableBoolean(false));
  }
  
  public Set<HDT_Base> getMentionedSet(HDT_Base mentioner, boolean descOnly, MutableBoolean choseNotToWait)
  {
    choseNotToWait.setValue(!waitUntilRebuildIsDone());
    if (choseNotToWait.isTrue())
      return null;
    
    if (descOnly)
      return mentionedInDescToMentioners.getReverseSet(mentioner);
    
    return mentionedAnywhereToMentioners.getReverseSet(mentioner);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean firstMentionsSecond(HDT_Base mentioner, HDT_Base target, boolean descOnly)
  {
    return firstMentionsSecond(mentioner, target, descOnly, new MutableBoolean(false));
  }
  
  public boolean firstMentionsSecond(HDT_Base mentioner, HDT_Base target, boolean descOnly, MutableBoolean choseNotToWait)
  {
    choseNotToWait.setValue(!waitUntilRebuildIsDone());
    if (choseNotToWait.isTrue())
      return false;
    
    if (descOnly)
      return mentionedInDescToMentioners.getForwardSet(target).contains(mentioner);
    
    return mentionedAnywhereToMentioners.getForwardSet(target).contains(mentioner);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
