/*
 * Copyright 2015-2025 Jason Winning
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
import java.util.Collection;
import java.util.EnumSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;

import javafx.application.Platform;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.concurrent.Worker.State;

import org.hypernomicon.HyperTask;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.util.BidiOneToManyRecordMap;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.jsoup.nodes.Element;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

class MentionsIndex
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final BidiOneToManyRecordMap mentionedInDescToMentioners   = new BidiOneToManyRecordMap(),
                                       mentionedAnywhereToMentioners = new BidiOneToManyRecordMap();
  private final Set<HDT_Record> removedRecords = ConcurrentHashMap.newKeySet();
  private final List<Runnable> ndxCompleteHandlers;
  private final EnumSet<RecordType> types;
  private final List<String> strList = new ArrayList<>();
  private final boolean asynchronous;

  private HyperTask task = null;

//---------------------------------------------------------------------------

  MentionsIndex(List<Runnable> ndxCompleteHandlers, boolean asynchronous)
  {
    this.ndxCompleteHandlers = ndxCompleteHandlers;
    this.asynchronous = asynchronous;

    types = EnumSet.allOf(RecordType.class);
    types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub));
  }

//---------------------------------------------------------------------------

  boolean waitUntilRebuildIsDone() { return (asynchronous == false) || (isRebuilding() == false) || (task.runWithProgressDialog() == State.SUCCEEDED); }
  boolean isRebuilding()           { return (task != null) && task.threadIsAlive(); }
  void stopRebuild()               { if (isRebuilding()) task.cancelAndWait(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    mentionedInDescToMentioners  .clear();
    mentionedAnywhereToMentioners.clear();
    removedRecords.clear();
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

    mentionedInDescToMentioners  .removeItem(record);
    mentionedAnywhereToMentioners.removeItem(record);
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
    mentionedInDescToMentioners  .removeReverseKey(record);

    strList.forEach(str -> KeywordLinkList.generate(str.toLowerCase()).forEach(link ->
    {
      HDT_Record otherRecord = link.key().record;

      // A record should not be considered as "mentioning" itself, and a term should not
      // be considered as "mentioning" its concepts or vice versa. This happens because
      // getAllStrings for concepts includes the term's search key.

      if ((record == otherRecord) ||
          ((record.getType() == hdtTerm) && ((HDT_Term)record).concepts.contains(otherRecord)) ||
          ((record.getType() == hdtConcept) && (((HDT_Concept)record).term.get() == otherRecord)))
        return;

      mentionedAnywhereToMentioners.addForward(link.key().record, record);
    }));

    if (record.hasMainText())
    {
      MainText mainText = ((HDT_RecordWithMainText)record).getMainText();

      MutableInt startNdx = new MutableInt(0), endNdx = new MutableInt(0);
      Property<Element> elementProp = new SimpleObjectProperty<>();

      Optional<HDT_MiscFile> optMiscFile = MainTextUtil.nextEmbeddedMiscFile(mainText.getHtml(), startNdx, endNdx, elementProp);

      while (optMiscFile != null)
      {
        if (optMiscFile.isPresent())
        {
          HDT_MiscFile miscFile = optMiscFile.get();

          mentionedAnywhereToMentioners.addForward(miscFile, record);
          mentionedInDescToMentioners  .addForward(miscFile, record);
        }

        startNdx.add(1);
        optMiscFile = MainTextUtil.nextEmbeddedMiscFile(mainText.getHtml(), startNdx, endNdx, elementProp);
      }

      String plainText = mainText.getPlain();

      if (strNotNullOrBlank(plainText))
        KeywordLinkList.generate(plainText).forEach(link -> mentionedInDescToMentioners.addForward(link.key().record, record));

      mainText.getDisplayItemsUnmod().forEach(displayItem ->
      {
        if (displayItem.type == diRecord)
        {
          mentionedAnywhereToMentioners.addForward(displayItem.record, record);
          mentionedInDescToMentioners  .addForward(displayItem.record, record);
        }
        else if (displayItem.type == diKeyWorks)
        {
          mainText.getKeyWorksUnmod().forEach(keyWork ->
          {
            HDT_RecordWithAuthors<? extends Authors> keyWorkRecord = keyWork.getRecord();

            mentionedAnywhereToMentioners.addForward(keyWorkRecord, record);
            mentionedInDescToMentioners  .addForward(keyWorkRecord, record);
          });
        }
      });
    }
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

  /**
   * If asynchronous is true, the rebuild task will be started in its own thread. Otherwise,
   * the mentionsIndex is completely rebuilt in this thread before this function returns.
   */
  void startRebuild()
  {
    if (asynchronous)
    {
      startRebuildTask();
      return;
    }

    for (RecordType type : types)
      for (HDT_Record record : db.records(type))
        reindexMentioner(record);

    ndxCompleteHandlers.forEach(Runnable::run);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void startRebuildTask()
  {
    stopRebuild();

    task = new HyperTask("MentionsIndex", "The requested operation will be performed after indexing has completed...")
    {
      @Override protected void call() throws CancelledTaskException
      {
        clear();

        totalCount = types.stream().map(db::records).mapToLong(Collection::size).sum();

        for (RecordType type : types)
          for (HDT_Record record : db.records(type))
          {
            incrementAndUpdateProgress(50);

            reindexMentioner(record);
          }
      }
    };

    task.addDoneHandler(state ->
    {
      ui.updateProgress("", -1);

      ndxCompleteHandlers.forEach(Runnable::run);
    });

    task.progressProperty().addListener((ob, oldValue, newValue) -> Platform.runLater(() ->
    {
      if (task.isDone() == false)
        ui.updateProgress("Indexing:", (double)task.completedCount / (double)task.totalCount);
    }));

    new RebuildThread(task).start();
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

    BidiOneToManyRecordMap map = descOnly ? mentionedInDescToMentioners : mentionedAnywhereToMentioners;

    Stream<HDT_Record> mentioners = map.getForwardStream(target);

    mentioners = switch(target.getType())
    {
      // A term's "mentioners" should include mentioners of its concepts because they may be
      // displayed records.

      case hdtConcept -> Stream.concat(mentioners, map.getForwardStream(((HDT_Concept)target).term.get()));
      case hdtTerm    -> Stream.concat(mentioners, ((HDT_Term)target).concepts.stream().flatMap(map::getForwardStream));

      // Include mentioners of investigations as mentioners of the corresponding person

      case hdtPerson  -> Stream.concat(mentioners, ((HDT_Person)target).investigations.stream().flatMap(map::getForwardStream));

      default -> mentioners;
    };

    return mentioners.collect(Collectors.toSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean firstMentionsSecond(HDT_Record mentioner, HDT_Record target, boolean descOnly, MutableBoolean choseNotToWait)
  {
    return nullSwitch(getMentionerSet(target, descOnly, choseNotToWait), false, set -> set.contains(mentioner));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
