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

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.mutable.MutableBoolean;

import javafx.application.Platform;
import org.hypernomicon.HyperTask;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.BidiOneToManyRecordMap;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.items.MainText.DisplayItemType.*;

class MentionsIndex
{
  private final BidiOneToManyRecordMap mentionedInDescToMentioners   = new BidiOneToManyRecordMap(),
                                       mentionedAnywhereToMentioners = new BidiOneToManyRecordMap();
  private final List<Runnable> ndxCompleteHandlers;
  private final KeywordLinkList linkList = new KeywordLinkList();
  private final EnumSet<HDT_RecordType> types;
  private final ArrayList<String> strList = new ArrayList<>();

  private RebuildThread thread = null;
  private HyperTask task = null;
  private double ctr, total;
  private boolean stopRequested = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MentionsIndex(List<Runnable> ndxCompleteHandlers)
  {
    this.ndxCompleteHandlers = ndxCompleteHandlers;

    types = EnumSet.allOf(HDT_RecordType.class);
    types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void removeRecord(HDT_Record record)
  {
    if ((thread != null) && thread.isAlive())
    {
      startRebuild();
      return;
    }

    mentionedInDescToMentioners.removeRecord(record);
    mentionedAnywhereToMentioners.removeRecord(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void updateMentioner(HDT_Record record)
  {
    if ((thread != null) && thread.isAlive())
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

  private void reindexMentioner(HDT_Record record)
  {
    if (record == null) return;

    strList.clear();

    record.getAllStrings(strList, true);

    mentionedAnywhereToMentioners.removeReverseKey(record);
    mentionedInDescToMentioners.removeReverseKey(record);

    strList.forEach(str ->
    {
      linkList.generate(str.toLowerCase());
      linkList.getLinks().forEach(link -> mentionedAnywhereToMentioners.addForward(link.key.record, record));
    });

    if (record.hasMainText())
    {
      MainText mainText = ((HDT_RecordWithConnector)record).getMainText();
      String plainText = mainText.getPlain();

      if (plainText.length() > 0)
      {
        linkList.generate(plainText);
        linkList.getLinks().forEach(link -> mentionedInDescToMentioners.addForward(link.key.record, record));
      }

      mainText.getDisplayItemsUnmod().forEach(displayItem ->
      {
        if (displayItem.type == diRecord)
        {
          mentionedAnywhereToMentioners.addForward(displayItem.record, record);
          mentionedInDescToMentioners.addForward(displayItem.record, record);
        }
        else if (displayItem.type == diKeyWorks)
        {
          mainText.getKeyWorks().forEach(keyWork ->
          {
            HDT_RecordWithPath keyWorkRecord = keyWork.getRecord();

            mentionedAnywhereToMentioners.addForward(keyWorkRecord, record);
            mentionedInDescToMentioners.addForward(keyWorkRecord, record);
          });
        }
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean waitUntilRebuildIsDone()
  {
    if (isRebuilding() == false) return true;

    HyperTask.performTaskWithProgressDialog(task);

    return task.isDone();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean isRebuilding()
  {
    return nullSwitch(thread, false, Thread::isAlive);
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
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean startRebuild()
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

          ndxCompleteHandlers.forEach(Runnable::run);
        });
      }

      @Override protected Boolean call() throws Exception
      {
        updateMessage("The requested operation will be performed after indexing has completed...");

        mentionedInDescToMentioners.clear();
        mentionedAnywhereToMentioners.clear();

        ctr = -1.0; total = 0.0;
        types.forEach(type -> total += db.records(type).size());

        for (HDT_RecordType type : types) for (HDT_Record record : db.records(type))
        {
          if ((((int)ctr++) % 50) == 0)
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

        updateProgress(total, total);
        return true;
      }
    };

    task.progressProperty().addListener((ob, oldValue, newValue) ->
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

  void stopRebuild()
  {
    if ((thread != null) && thread.isAlive())
    {
      stopRequested = true;
      try { thread.join(); } catch (InterruptedException e) { noOp(); }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Set<HDT_Record> getMentionerSet(HDT_Record target, boolean descOnly)
  {
    return getMentionerSet(target, descOnly, new MutableBoolean(false));
  }

  Set<HDT_Record> getMentionerSet(HDT_Record target, boolean descOnly, MutableBoolean choseNotToWait)
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

  boolean firstMentionsSecond(HDT_Record mentioner, HDT_Record target, boolean descOnly, MutableBoolean choseNotToWait)
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
