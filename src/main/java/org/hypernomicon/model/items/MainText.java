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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.model.items.MainText.DisplayItemType.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.HyperDB.RelationChangeHandler;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;

public class MainText
{
//---------------------------------------------------------------------------

  public static enum DisplayItemType { diKeyWorks, diDescription, diRecord }

//---------------------------------------------------------------------------

  public static class DisplayItem
  {
    DisplayItem(DisplayItemType type)                  { this.type = type; this.record = null; }
    public DisplayItem(HDT_RecordWithConnector record) { this.type = diRecord; this.record = record; }

    public final DisplayItemType type;
    public final HDT_RecordWithConnector record;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    @Override public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + (((type != diRecord) || (record == null)) ? 0 : record.hashCode());
      result = prime * result + (type == null ? 0 : type.hashCode());
      return result;
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if (obj == null) return false;

      if (getClass() != obj.getClass()) return false;

      DisplayItem other = (DisplayItem) obj;

      if (type != other.type) return false;
      if (type != diRecord)  return true;

      if ((record == null) && (other.record == null)) return true;
      if ((record == null) || (other.record == null)) return false;

      return record.equals(other.record);
    }
  }

//---------------------------------------------------------------------------

  final List<DisplayItem> displayItems;
  final List<KeyWork> keyWorks;  // this can be works or miscFiles
  private String plainText = "";
  private String htmlText = "";
  final private Connector connector;

  public String getHtml()                         { return htmlText; }
  public HDT_RecordWithConnector getRecord()      { return connector.getSpoke(); }
  public String getPlain()                        { return plainText; }
  private boolean hasKeyWork(HDT_Base rec)        { return getKeyWork(rec) != null; }
  public List<DisplayItem> getDisplayItemsUnmod() { return Collections.unmodifiableList(displayItems); }
  public List<KeyWork> getKeyWorks()              { return Collections.unmodifiableList(keyWorks); }
  public List<DisplayItem> getDisplayItemsCopy()  { return new ArrayList<>(displayItems); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getKeyWorksString()
  {
    return keyWorks.stream().map(keyWork -> keyWork.getSearchKey(true)).reduce((s1, s2) -> s1 + " " + s2).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getDisplayItemsString()
  {
    return displayItems.stream().filter(item -> item.type == diRecord)
                                .map(item -> item.record.listName() + " (" + db.getTypeName(item.record.getType()) + ")")
                                .reduce((s1, s2) -> s1 + "; " + s2).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getPlainForDisplay()
  {
    String keyWorksStr = getKeyWorksString();

    if (keyWorksStr.length() == 0)
      return plainText;

    return ultraTrim(plainText + " Key works: " + keyWorksStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void init(HyperDB db) // called by DB constructor
  {
    RelationChangeHandler handler = (child, parent, affirm) ->
    {
      HDT_WorkLabel label = (HDT_WorkLabel) parent;
      MainText mainText = label.getMainText();

      HDT_RecordWithPath kwRecord = (HDT_RecordWithPath) child;
      KeyWork keyWork = mainText.getKeyWork(kwRecord);

      if (affirm)
      {
        if (keyWork == null)
        {
          mainText.keyWorks.add(new KeyWork(kwRecord));
          mainText.runKeyWorkHandler(kwRecord, true);
          db.updateMentioner(label);
        }
      }
      else
      {
        if (keyWork != null)
        {
          mainText.keyWorks.remove(keyWork);
          mainText.runKeyWorkHandler(kwRecord, false);
          db.updateMentioner(label);
        }
      }
    };

    db.addRelationChangeHandler(rtLabelOfWork, handler);
    db.addRelationChangeHandler(rtLabelOfFile, handler);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public KeyWork getKeyWork(HDT_Base child)
  {
    synchronized (keyWorks)
    {
      return findFirst(keyWorks, keyWork -> keyWork.getRecord().equals(child));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MainText(Connector connector)  // called by Connector constructor
  {
    this.connector = connector;

    displayItems = Collections.synchronizedList(new ArrayList<>());

    addDefaultItems();

    keyWorks = Collections.synchronizedList(new ArrayList<>());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MainText(MainText mainText, Connector connector)  // called by StrongLink.disconnectRecords
  {
    this.connector = connector;

    keyWorks = Collections.synchronizedList(new ArrayList<>());

    mainText.keyWorks.forEach(keyWork -> keyWorks.add(keyWork.getOnlineCopy()));

    displayItems = Collections.synchronizedList(new ArrayList<>());

    if (connector.getType() == hdtWorkLabel)
    {
      HDT_WorkLabel.class.cast(connector.getSpoke()).refreshSubjects();
      addDefaultItems();
    }
    else
    {
      mainText.displayItems.forEach(srcItem ->
      {
        if (srcItem.type == diRecord)
        {
          displayItems.add(new DisplayItem(srcItem.record));
          db.handleDisplayRecord(this, srcItem.record.getMainText(), true);
        }
        else
          displayItems.add(new DisplayItem(srcItem.type));

        setInternal(mainText.htmlText, extractTextFromHTML(mainText.htmlText).trim());
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MainText(MainText src1, MainText src2, Connector hubConnector, String newHtml)  // called by StrongLink.connectRecords
  {
    connector = hubConnector;

    displayItems = Collections.synchronizedList(new ArrayList<>(src1.displayItems));

    ArrayList<Connector> src1Connectors = new ArrayList<>(),
                         src2Connectors = new ArrayList<>();

    if (src1.getRecord().getType() == hdtHub)
    {
      StrongLink link = src1.connector.getLink();

      if (link.getDebate  () != null) src1Connectors.add(link.debateSpoke  );
      if (link.getPosition() != null) src1Connectors.add(link.positionSpoke);
      if (link.getConcept () != null) src1Connectors.add(link.conceptSpoke );
      if (link.getNote    () != null) src1Connectors.add(link.noteSpoke    );
    }
    else
      src1Connectors.add(src1.connector);

    if (src2.getRecord().getType() == hdtHub)
    {
      StrongLink link = src2.connector.getLink();

      if (link.getDebate  () != null) src2Connectors.add(link.debateSpoke  );
      if (link.getPosition() != null) src2Connectors.add(link.positionSpoke);
      if (link.getConcept () != null) src2Connectors.add(link.conceptSpoke );
      if (link.getNote    () != null) src2Connectors.add(link.noteSpoke    );
    }
    else
      src2Connectors.add(src2.connector);

    for (int ndx = 0; ndx < src2.displayItems.size(); ndx++)
    {
      DisplayItem displayItem = src2.displayItems.get(ndx);

      if (displayItems.contains(displayItem) == false)
      {
        if (ndx == 0)
          displayItems.add(0, displayItem);
        else
          displayItems.add(displayItem);
      }
    }

    keyWorks = Collections.synchronizedList(new ArrayList<>());

    src1.keyWorks.forEach(keyWork ->
    {
      keyWorks.add(keyWork.getOnlineCopy());

      if (src2.hasKeyWork(keyWork.getRecord()) == false)
        src2Connectors.forEach(curConn ->  db.handleKeyWork(curConn.getSpoke(), keyWork.getRecord(), true));
    });

    src2.keyWorks.forEach(keyWork ->
    {
      if (src1.hasKeyWork(keyWork.getRecord()) == false)
      {
        keyWorks.add(keyWork.getOnlineCopy());

        src1Connectors.forEach(curConn -> db.handleKeyWork(curConn.getSpoke(), keyWork.getRecord(), true));
      }
    });

    setInternal(newHtml, extractTextFromHTML(newHtml).trim());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setHtml(String newHtml)
  {
    boolean modify = true;
    String newPlainText = extractTextFromHTML(newHtml).trim();

    if (plainText.replaceAll("\\h*", "").equalsIgnoreCase(newPlainText.replaceAll("\\h*", "")))  // Remove all horizontal whitespaces and then compare
      modify = false;

    if (ultraTrim(convertToSingleLine(newPlainText)).length() == 0)
      if (ultraTrim(convertToSingleLine(plainText)).length() == 0)
        modify = false;

    setInternal(newHtml, newPlainText);

    if (modify == false) return;

    if (connector != null)
      connector.modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setInternal(String newHtmlText, String newPlainText)
  {
    plainText = safeStr(newPlainText);

    if (ultraTrim(convertToSingleLine(plainText)).length() == 0)
    {
      htmlText = "";
      plainText = "";
    }
    else
      htmlText = safeStr(newHtmlText);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void expire()
  {
    Iterator<KeyWork> keyWorkIT = keyWorks.iterator();

    while (keyWorkIT.hasNext())
    {
      KeyWork keyWork = keyWorkIT.next();

      keyWorkIT.remove();
      HDT_RecordWithPath kwRecord = keyWork.getRecord();
      if (kwRecord != null)
        runKeyWorkHandler(kwRecord, false);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void resolvePointers()
  {
    displayItems.removeIf(item -> (item.type == diRecord) && (HDT_Record.isEmpty(item.record)));

    Iterator<KeyWork> keyWorkIT = keyWorks.iterator();

    while (keyWorkIT.hasNext())
    {
      KeyWork keyWork = keyWorkIT.next();

      if (keyWork.isExpired())
      {
        keyWorkIT.remove();
        HDT_RecordWithPath kwRecord = keyWork.getRecord();
        if (kwRecord != null)
          runKeyWorkHandler(kwRecord, false);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean typeHasKeyWorks(HDT_RecordType type)
  {
    switch (type)
    {
      case hdtWork : case hdtMiscFile : case hdtInvestigation : case hdtArgument :
        return false;

      default :
        return true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addDefaultItems()
  {
    addDefaultItemsToList(getRecord(), displayItems);
  }

  public static void addDefaultItemsToList(HDT_RecordWithConnector record, List<DisplayItem> displayItems)
  {
    if (typeHasKeyWorks(record.getType()))
      displayItems.add(new MainText.DisplayItem(diKeyWorks));

    displayItems.add(new MainText.DisplayItem(diDescription));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Set<HDT_RecordWithConnector> getRecordDisplayItems()
  {
    return displayItems.stream().filter(item -> item.type == diRecord).map(item -> item.record).collect(Collectors.toSet());
  }

  public void setDisplayItemsFromList(List<DisplayItem> src)
  {
    Set<HDT_RecordWithConnector> oldSet = getRecordDisplayItems();

    displayItems.clear();
    displayItems.addAll(src);

    Set<HDT_RecordWithConnector> newSet = getRecordDisplayItems();

    boolean modify = false;

    for (HDT_RecordWithConnector displayRecord : oldSet)
      if (newSet.contains(displayRecord) == false)
      {
        db.handleDisplayRecord(this, displayRecord.getMainText(), false);
        modify = true;
      }

    for (HDT_RecordWithConnector displayRecord : newSet)
      if (oldSet.contains(displayRecord) == false)
      {
        db.handleDisplayRecord(this, displayRecord.getMainText(), true);
        modify = true;
      }

    if (modify) connector.modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void runKeyWorkHandler(HDT_RecordWithPath keyWorkRecord, boolean affirm)
  {
    if (connector == null) return;

    StrongLink link = connector.getLink();

    if (link == null)
    {
      db.handleKeyWork(getRecord(), keyWorkRecord, affirm);
      return;
    }

    nullSwitch(link.getDebate  (), debate  -> db.handleKeyWork(debate , keyWorkRecord, affirm));
    nullSwitch(link.getPosition(), pos     -> db.handleKeyWork(pos    , keyWorkRecord, affirm));
    nullSwitch(link.getNote    (), note    -> db.handleKeyWork(note   , keyWorkRecord, affirm));
    nullSwitch(link.getConcept (), concept -> db.handleKeyWork(concept, keyWorkRecord, affirm));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setKeyWorksFromList(List<KeyWork> src, boolean refreshSubjects)
  {
    boolean modify = false;
    Iterator<KeyWork> it = keyWorks.iterator();

    while (it.hasNext())
    {
      KeyWork keyWork = it.next();
      if (src.contains(keyWork) == false)
      {
        if (refreshSubjects) runKeyWorkHandler(keyWork.getRecord(), false);
        it.remove();
        modify = true;
      }
    }

    it = src.iterator();

    while (it.hasNext())
    {
      KeyWork keyWork = it.next();
      if (keyWorks.contains(keyWork) == false)
      {
        if (refreshSubjects) runKeyWorkHandler(keyWork.getRecord(), true);
        keyWorks.add(keyWork);
        modify = true;
      }
    }

    if (connector == null) return;

    if (modify)
      connector.modifyNow();

    if (refreshSubjects)
      nullSwitch(connector.getLink(), link -> nullSwitch(link.getLabel(), HDT_WorkLabel::refreshSubjects));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<Connector> getDisplayers()
  {
    Set<MainText> displayers = db.getDisplayers(this);
    HashSet<Connector> displayerConns = new HashSet<>();

    displayers.forEach(displayerText ->
    {
      Connector displayer = displayerText.connector;

      if (displayer.getType() == hdtHub)
      {
        StrongLink link = displayer.getLink();

        if      (link.getDebate  () != null) displayer = link.getSpoke(hdtDebate  );
        else if (link.getPosition() != null) displayer = link.getSpoke(hdtPosition);
        else if (link.getConcept () != null) displayer = link.getSpoke(hdtConcept );
        else                                 displayer = link.getSpoke(hdtNote    );
      }

      displayerConns.add(displayer);
    });

    return displayerConns;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
