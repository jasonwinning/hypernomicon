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

package org.hypernomicon.model.unities;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

import java.util.*;
import java.util.stream.Collectors;

import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.*;

/**
 * Every record that has a main HTML description field refers to an object of
 * this class, which stores the HTML. All such record classes are subclasses of
 * {@link HDT_RecordWithMainText HDT_RecordWithMainText}.
 * <br><br>
 * Some of those record types, but not all, also can be "united" to other
 * records so that they will refer to the same MainText object.
 *
 * @author  Jason Winning
 * @since   1.0
 */
public class MainText
{
//---------------------------------------------------------------------------

  public enum DisplayItemType { diKeyWorks, diDescription, diRecord }

//---------------------------------------------------------------------------

  public static class DisplayItem
  {
    DisplayItem(DisplayItemType type)                 { this.type = type; record = null; }
    public DisplayItem(HDT_RecordWithMainText record) { type = diRecord; this.record = record; }

    public final DisplayItemType type;
    public final HDT_RecordWithMainText record;

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

      if (type != diRecord) return type == other.type;

      return Objects.equals(record, other.record);
    }
  }

//---------------------------------------------------------------------------

  final List<DisplayItem> displayItems;
  final List<KeyWork> keyWorks = Collections.synchronizedList(new ArrayList<>());

  private final HDT_RecordWithMainText recordWMT;

  private HtmlAndPlainText htmlAndPlainText = new HtmlAndPlainText("");

  public String getHtml()                         { return htmlAndPlainText.getHtml(); }
  public String getPlain()                        { return htmlAndPlainText.getPlainText(); }
  void setInternal(String newHtml)                { htmlAndPlainText = new HtmlAndPlainText(newHtml); }
  public HDT_RecordWithMainText getRecord()       { return recordWMT; }
  private boolean hasKeyWork(HDT_Record rec)      { return getKeyWork(rec) != null; }
  public List<DisplayItem> getDisplayItemsUnmod() { return Collections.unmodifiableList(displayItems); }
  public List<DisplayItem> getDisplayItemsCopy()  { return new ArrayList<>(displayItems); }
  public List<KeyWork> getKeyWorksUnmod()         { return Collections.unmodifiableList(keyWorks); }
  public List<KeyWork> getKeyWorksCopy()          { return new ArrayList<>(keyWorks); }
  void expire()                                   { removeKeyWorks(false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty()
  {
    return getPlain().isBlank() && keyWorks.isEmpty() && displayItems.stream().noneMatch(displayItem -> displayItem.type == diRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getKeyWorksString()
  {
    return keyWorks.stream().map(keyWork -> keyWork.getSearchKey(true)).collect(Collectors.joining(" "));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getDisplayItemsString()
  {
    return displayItems.stream().filter(item -> item.type == diRecord)
                                .map(item -> item.record.listName() + " (" + getTypeName(item.record.getType()) + ')')
                                .collect(Collectors.joining("; "));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getPlainForDisplay()
  {
    String keyWorksStr = getRecord().getType() == hdtInvestigation ? "" : getKeyWorksString();

    return ultraTrim(htmlAndPlainText.getPlainText() + (keyWorksStr.isEmpty() ? "" : " Key works: " + keyWorksStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private KeyWork getKeyWork(HDT_Record child)
  {
    synchronized (keyWorks)
    {
      return findFirst(keyWorks, keyWork -> keyWork.getRecord().equals(child));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MainText(HDT_RecordWithMainText recordWMT)  // called by HDT_RecordWithMainText constructor
  {
    this.recordWMT = recordWMT;

    displayItems = Collections.synchronizedList(new ArrayList<>());

    addDefaultItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MainText(MainText mainText, HDT_RecordWithMainText recordWMT)  // called by HDT_Hub.disuniteRecords
  {
    this.recordWMT = recordWMT;

    mainText.keyWorks.forEach(keyWork -> keyWorks.add(keyWork.getOnlineCopy()));

    displayItems = Collections.synchronizedList(new ArrayList<>());

    if (recordWMT.getType() == hdtWorkLabel)
    {
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

        setInternal(mainText.getHtml());
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  MainText(MainText src1, MainText src2, HDT_Hub hub, String newHtml)  // called by HDT_Hub.uniteRecords
  {
    recordWMT = hub;
    displayItems = Collections.synchronizedList(new ArrayList<>(src1.displayItems));

    List<HDT_RecordWithMainText> src1Spokes = src1.getRecord() == hub ? List.copyOf(hub.spokes.values()) : List.of(src1.getRecord()),
                                 src2Spokes = src2.getRecord() == hub ? List.copyOf(hub.spokes.values()) : List.of(src2.getRecord());

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

    src1.keyWorks.forEach(keyWork ->
    {
      keyWorks.add(keyWork.getOnlineCopy());

      if (src2.hasKeyWork(keyWork.getRecord()) == false)
        src2Spokes.forEach(spoke ->  db.handleKeyWork(spoke, keyWork.getRecord(), true));
    });

    src2.keyWorks.forEach(keyWork ->
    {
      if (src1.hasKeyWork(keyWork.getRecord()) == false)
      {
        keyWorks.add(keyWork.getOnlineCopy());

        src1Spokes.forEach(spoke -> db.handleKeyWork(spoke, keyWork.getRecord(), true));
      }
    });

    setInternal(newHtml);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setHtml(String newHtml)
  {
    boolean modify = true;
    String oldPlainText = extractTextFromHTML(htmlAndPlainText.getHtml(), true).trim(),
           newPlainText = extractTextFromHTML(newHtml, true).trim();

    if (oldPlainText.replaceAll("\\h+", "").equalsIgnoreCase(newPlainText.replaceAll("\\h+", "")))  // Remove all horizontal whitespaces and then compare
      modify = false;

    if (ultraTrim(convertToSingleLine(newPlainText)).isEmpty())
      if (ultraTrim(convertToSingleLine(oldPlainText)).isEmpty())
        modify = false;

    setInternal(newHtml);

    if (modify)
      recordWMT.modifyMainText();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void resolvePointers()
  {
    displayItems.removeIf(item -> (item.type == diRecord) && HDT_Record.isEmpty(item.record, false));

    removeKeyWorks(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeKeyWorks(boolean onlyIfExpired)
  {
    Iterator<KeyWork> keyWorkIT = keyWorks.iterator();

    while (keyWorkIT.hasNext())
    {
      KeyWork keyWork = keyWorkIT.next();

      if ((onlyIfExpired == false) || keyWork.isExpired())
      {
        keyWorkIT.remove();
        nullSwitch(keyWork.getRecord(), kwRecord -> runKeyWorkHandler(kwRecord, false));
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean typeHasKeyWorks(RecordType type) { return switch (type)
  {
    case hdtWork, hdtMiscFile, hdtArgument -> false;
    default                                -> true;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addDefaultItems()
  {
    addDefaultItemsToList(getRecord(), displayItems);
  }

  public static void addDefaultItemsToList(HDT_RecordWithMainText record, List<DisplayItem> displayItems)
  {
    RecordType recordType = record.getType();

    if (typeHasKeyWorks(recordType))
      displayItems.add(new MainText.DisplayItem(diKeyWorks));

    displayItems.add(new MainText.DisplayItem(diDescription));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Set<HDT_RecordWithMainText> getRecordDisplayItems()
  {
    return displayItems.stream().filter(item -> item.type == diRecord).map(item -> item.record).collect(Collectors.toSet());
  }

  public void setDisplayItemsFromList(List<DisplayItem> src)
  {
    Set<HDT_RecordWithMainText> oldSet = getRecordDisplayItems();

    displayItems.clear();
    displayItems.addAll(src);

    Set<HDT_RecordWithMainText> newSet = getRecordDisplayItems();

    boolean modify = false;

    for (HDT_RecordWithMainText displayRecord : oldSet)
      if (newSet.contains(displayRecord) == false)
      {
        db.handleDisplayRecord(this, displayRecord.getMainText(), false);
        modify = true;
      }

    for (HDT_RecordWithMainText displayRecord : newSet)
      if (oldSet.contains(displayRecord) == false)
      {
        db.handleDisplayRecord(this, displayRecord.getMainText(), true);
        modify = true;
      }

    if (modify) recordWMT.modifyMainText();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void runKeyWorkHandler(HDT_RecordWithAuthors<? extends Authors> keyWorkRecord, boolean affirm)
  {
    if (recordWMT == null) return;

    HDT_Hub hub = recordWMT.getHub();

    if (hub == null)
    {
      db.handleKeyWork(getRecord(), keyWorkRecord, affirm);
      return;
    }

    nullSwitch(hub.getDebate  (), debate  -> db.handleKeyWork(debate , keyWorkRecord, affirm));
    nullSwitch(hub.getPosition(), pos     -> db.handleKeyWork(pos    , keyWorkRecord, affirm));
    nullSwitch(hub.getNote    (), note    -> db.handleKeyWork(note   , keyWorkRecord, affirm));
    nullSwitch(hub.getConcept (), concept -> db.handleKeyWork(concept, keyWorkRecord, affirm));
    nullSwitch(hub.getLabel   (), label   -> db.handleKeyWork(label  , keyWorkRecord, affirm));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setKeyWorksFromList(List<KeyWork> src)
  {
    boolean modify = false;
    Iterator<KeyWork> it = keyWorks.iterator();

    while (it.hasNext())
    {
      KeyWork keyWork = it.next();
      if (src.contains(keyWork) == false)
      {
        runKeyWorkHandler(keyWork.getRecord(), false);
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
        runKeyWorkHandler(keyWork.getRecord(), true);
        keyWorks.add(keyWork);
        modify = true;
      }
    }

    if (recordWMT == null) return;

    if (modify)
      recordWMT.modifyMainText();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <HDT_MT extends HDT_RecordWithMainText> void setKeyWorkMentioners(HDT_RecordWithAuthors<? extends Authors> kwRecord, Collection<HDT_MT> newCol, RecordType mentionerType)
  {
    setKeyWorkMentioners(kwRecord, newCol, (Class<HDT_MT>) mentionerType.getRecordClass());
  }

  public static <HDT_MT extends HDT_RecordWithMainText> void setKeyWorkMentioners(HDT_RecordWithAuthors<? extends Authors> kwRecord, Collection<HDT_MT> newCol, Class<HDT_MT> klazz)
  {
    Collection<HDT_MT> oldCol = db.keyWorkMentionerStream(kwRecord, klazz).collect(Collectors.toSet());

    oldCol.forEach(recordWMT ->
    {
      if (newCol.contains(recordWMT)) return;

      MainText mainText = recordWMT.getMainText();
      List<KeyWork> keyWorks = mainText.getKeyWorksCopy();

      keyWorks.removeIf(keyWork -> keyWork.getRecord() == kwRecord);
      mainText.setKeyWorksFromList(keyWorks);
    });

    newCol.forEach(recordWMT ->
    {
      if (oldCol.contains(recordWMT)) return;

      MainText mainText = recordWMT.getMainText();
      List<KeyWork> keyWorks = mainText.getKeyWorksCopy();

      keyWorks.add(new KeyWork(kwRecord));
      mainText.setKeyWorksFromList(keyWorks);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addKeyworksIfNotPresent()
  {
    boolean hasKeyworks = displayItems.stream().anyMatch(displayItem -> displayItem.type == diKeyWorks);

    if (hasKeyworks == false)
      displayItems.add(new DisplayItem(diKeyWorks));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
