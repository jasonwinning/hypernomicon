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

package org.hypernomicon.model;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;

import javafx.application.Platform;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.concurrent.Worker.State;

import org.hypernomicon.HyperTask;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.util.BidiOneToManyRecordMap;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.jsoup.nodes.Element;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;

class MentionsIndex
{
  private final BidiOneToManyRecordMap mentionedInDescToMentioners   = new BidiOneToManyRecordMap(),
                                       mentionedAnywhereToMentioners = new BidiOneToManyRecordMap();
  private final Set<HDT_Record> removedRecords = Collections.newSetFromMap(new ConcurrentHashMap<>());
  private final List<Runnable> ndxCompleteHandlers;
  private final EnumSet<RecordType> types;
  private final List<String> strList = new ArrayList<>();

  private HyperTask task = null;
  private double ctr, total;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MentionsIndex(List<Runnable> ndxCompleteHandlers)
  {
    this.ndxCompleteHandlers = ndxCompleteHandlers;

    types = EnumSet.allOf(RecordType.class);
    types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void removeRecord(HDT_Record record)
  {
    if (isRebuilding())
    {
      startRebuild();
      return;
    }

    mentionedInDescToMentioners.removeRecord(record);
    mentionedAnywhereToMentioners.removeRecord(record);
    removedRecords.add(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void updateMentioner(HDT_Record record)
  {
    if (removedRecords.contains(record))
      return;

    if (isRebuilding())
    {
      startRebuild();
      return;
    }

    if (record.isUnitable())
    {
      HDT_RecordWithMainText uRecord = (HDT_RecordWithMainText) record;
      if (uRecord.hasHub())
      {
        uRecord.getHub().getSpokes().forEach(this::reindexMentioner);
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
      KeywordLinkList.generate(str.toLowerCase()).forEach(link -> mentionedAnywhereToMentioners.addForward(link.key.record, record)));

    if (record.hasMainText())
    {
      MainText mainText = ((HDT_RecordWithMainText)record).getMainText();

      MutableInt startNdx = new MutableInt(0), endNdx = new MutableInt(0);
      ObjectProperty<Element> elementProp = new SimpleObjectProperty<>();

      HDT_MiscFile miscFile = MainTextUtil.nextEmbeddedMiscFile(mainText.getHtml(), startNdx, endNdx, elementProp);

      while (miscFile != null)
      {
        mentionedAnywhereToMentioners.addForward(miscFile, record);
        mentionedInDescToMentioners.addForward(miscFile, record);

        startNdx.add(1);
        miscFile = MainTextUtil.nextEmbeddedMiscFile(mainText.getHtml(), startNdx, endNdx, elementProp);
      }

      String plainText = mainText.getPlain();

      if (plainText.length() > 0)
        KeywordLinkList.generate(plainText).forEach(link -> mentionedInDescToMentioners.addForward(link.key.record, record));

      mainText.getDisplayItemsUnmod().forEach(displayItem ->
      {
        if (displayItem.type == diRecord)
        {
          mentionedAnywhereToMentioners.addForward(displayItem.record, record);
          mentionedInDescToMentioners.addForward(displayItem.record, record);
        }
        else if (displayItem.type == diKeyWorks)
        {
          mainText.getKeyWorksUnmod().forEach(keyWork ->
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

    return task.runWithProgressDialog() == State.SUCCEEDED;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean isRebuilding()
  {
    return (task != null) && task.threadIsAlive();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class RebuildThread extends HyperThread
  {
    private RebuildThread(HyperTask task)
    {
      super(task);
      setDaemon(true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void startRebuild()
  {
    stopRebuild();

    task = new HyperTask("MentionsIndex")
    {
      @Override protected void done()
      {
        updateProgress(total, total);

        Platform.runLater(() ->
        {
          waitUntilThreadDies();

          ndxCompleteHandlers.forEach(Runnable::run);
        });
      }

      @Override protected void call() throws CancelledTaskException
      {
        updateMessage("The requested operation will be performed after indexing has completed...");

        mentionedInDescToMentioners.clear();
        mentionedAnywhereToMentioners.clear();
        removedRecords.clear();

        ctr = -1.0; total = 0.0;
        types.forEach(type -> total += db.records(type).size());

        for (RecordType type : types) for (HDT_Record record : db.records(type))
        {
          if ((((int)ctr++) % 50) == 0)
          {
            if (isCancelled())
              throw new CancelledTaskException();

            updateProgress(ctr, total);
          }

          reindexMentioner(record);
        }
      }
    };

    task.progressProperty().addListener((ob, oldValue, newValue) ->
      Platform.runLater(newValue.doubleValue() == 1.0 ?
        () -> ui.updateProgress("", -1.0)
      :
        () -> ui.updateProgress("Indexing:", ctr / total)));

    new RebuildThread(task).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void stopRebuild()
  {
    if (isRebuilding() == false)
      return;

    task.cancelAndWait();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Set<HDT_Record> getMentionerSet(HDT_Record target, boolean descOnly)
  {
    return getMentionerSet(target, descOnly, new MutableBoolean(false));
  }

  Set<HDT_Record> getMentionerSet(HDT_Record target, boolean descOnly, MutableBoolean choseNotToWait)
  {
    choseNotToWait.setValue(waitUntilRebuildIsDone() == false);
    if (choseNotToWait.isTrue())
      return null;

    if (target.getType() == hdtConcept)
      target = ((HDT_Concept)target).term.get();

    return descOnly ?
      mentionedInDescToMentioners.getForwardSet(target)
    :
      mentionedAnywhereToMentioners.getForwardSet(target);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean firstMentionsSecond(HDT_Record mentioner, HDT_Record target, boolean descOnly, MutableBoolean choseNotToWait)
  {
    choseNotToWait.setValue(waitUntilRebuildIsDone() == false);
    if (choseNotToWait.isTrue())
      return false;

    if (target.getType() == hdtConcept)
      target = ((HDT_Concept)target).term.get();

    return descOnly ?
      mentionedInDescToMentioners.getForwardSet(target).contains(mentioner)
    :
      mentionedAnywhereToMentioners.getForwardSet(target).contains(mentioner);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
