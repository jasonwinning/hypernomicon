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

package org.hypernomicon.model.records;

import java.time.Instant;
import java.util.Collection;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.stream.IntStream;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjList;
import org.hypernomicon.model.relations.HyperSubjPointer;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.*;
import org.hypernomicon.model.unities.HDI_OnlineMainTextAndHub;
import org.hypernomicon.model.unities.HDI_OnlineHubSpokes;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public abstract class HDT_RecordBase implements HDT_Record
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperDataset<? extends HDT_Record> dataset;
  private final Tag nameTag;
  private final NameItem name;
  private final Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>> items;
  private final boolean dummyFlag;
  private String sortKeyAttr;
  private final RecordType type;

  private int id;
  private Instant creationDate, modifiedDate, viewDate;
  private RecordState xmlState;

  private boolean online = false, expired = false;

  @Override public final Instant getViewDate()             { return type.getDisregardDates() ? null : viewDate; }
  @Override public final Instant getCreationDate()         { return type.getDisregardDates() ? null : creationDate; }
  @Override public final Tag getNameTag()                  { return nameTag; }
  @Override public final boolean isDummy()                 { return dummyFlag; }
  @Override public final int getID()                       { return id; }
  @Override public final int keyNdx()                      { return db.records(type).getKeyNdxByID(id); }
  @Override public final void viewNow()                    { if (db.viewTestingInProgress == false) viewDate = Instant.now(); }
  @Override public final String getSortKeyAttr()           { return sortKeyAttr; }
  @Override public final String getSortKey()               { return safeStr(dataset.getKeyByID(id)); }
  @Override public final boolean isExpired()               { return expired; }
  @Override public final Set<Tag> getAllTags()             { return items.keySet().isEmpty() ? EnumSet.noneOf(Tag.class) : EnumSet.copyOf(items.keySet()); }
  @Override public final boolean getTagBoolean(Tag tag)    { return ((HDI_OnlineBoolean)items.get(tag)).get(); }
  @Override public final boolean hasStoredState()          { return xmlState.stored; }
  @Override public final void updateSortKey()              { dataset.updateSortKey(this); }
  @Override public final HDI_Schema getSchema(Tag tag)     { return nullSwitch(items.get(tag), null, HDI_Base::getSchema); }
  @Override public final RecordType getType()              { return type; }

  @Override public Instant getModifiedDate()               { return type.getDisregardDates() ? null : modifiedDate; }
  @Override public String name()                           { return name.get(); }
  @Override public void setName(String str)                { setNameInternal(str, true); }
  @Override public String getNameEngChar()                 { return name.getEngChar(); }
  @Override public String getCBText()                      { return listName(); }
  @Override public String getXMLObjectName()               { return listName(); }
  @Override public boolean isUnitable()                    { return false; }
  @Override public boolean hasMainText()                   { return false; }
  @Override public boolean hasDesc()                       { return false; }
  @Override public String getSearchKey()                   { return db.getSearchKey(this); }
  @Override public Iterable<SearchKeyword> getSearchKeys() { return db.getKeysByRecord(this); }
  @Override public String firstActiveKeyWord()             { return db.firstActiveKeyWord(this); }

  @Override public final void writeStoredStateToXML(StringBuilder xml)        { xmlState.writeToXML(xml); }
  @Override public void setSearchKey(String newKey) throws DuplicateSearchKeyException, SearchKeyTooShortException
  { setSearchKey(newKey, false, true); }

  @Override public void setSearchKey(String newKey, boolean noMod, boolean rebuildMentions) throws DuplicateSearchKeyException, SearchKeyTooShortException
  { db.setSearchKey(this, newKey, noMod, rebuildMentions); }

  @SuppressWarnings("unchecked")
  protected final <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> HyperObjList<HDT_SubjType, HDT_ObjType> getObjList(RelationType relType)
  { return (HyperObjList<HDT_SubjType, HDT_ObjType>) db.getObjectList(relType, this, true); }

  @SuppressWarnings("unchecked")
  protected final <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> HyperSubjList<HDT_SubjType, HDT_ObjType> getSubjList(RelationType relType)
  { return (HyperSubjList<HDT_SubjType, HDT_ObjType>) db.getSubjectList(relType, this); }

  @SuppressWarnings("unchecked")
  protected final <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> HyperObjPointer<HDT_SubjType, HDT_ObjType> getObjPointer(RelationType relType)
  { return (HyperObjPointer<HDT_SubjType, HDT_ObjType>) db.getObjPointer(relType, this); }

  @SuppressWarnings("unchecked")
  protected final <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> HyperSubjPointer<HDT_SubjType, HDT_ObjType> getSubjPointer(RelationType relType)
  { return (HyperSubjPointer<HDT_SubjType, HDT_ObjType>) db.getSubjPointer(relType, this); }

  protected boolean isOnline() { return online; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected HDT_RecordBase(RecordState xmlState, HyperDataset<? extends HDT_Record> dataset, Tag nameTag)
  {
    name = new NameItem();
    type = typeByRecordClass(getClass());

    this.xmlState = xmlState;
    id = xmlState.id;
    dummyFlag = xmlState.dummyFlag;
    this.dataset = dataset;
    this.nameTag = nameTag;
    sortKeyAttr = safeStr(xmlState.sortKeyAttr);

    items = new LinkedHashMap<>();

    initItems();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void setNameInternal(String str, boolean update)
  {
    name.set(update ? updateString(name.get(), str) : safeStr(str));
    updateSortKey();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final void setSortKeyAttr(String sortKeyAttr)
  {
    if (Objects.equals(this.sortKeyAttr, sortKeyAttr))
      return;

    this.sortKeyAttr = sortKeyAttr;
    updateSortKey();
    modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initItems()
  {
    Collection<HDI_Schema> schemas = db.getSchemasByRecordType(type);
    if (schemas == null) return;

    for (HDI_Schema schema : schemas)
    {
      HDI_OnlineBase<? extends HDI_OfflineBase> item = switch (schema.category())
      {
        case hdcMainTextAndHub -> new HDI_OnlineMainTextAndHub(schema, (HDT_RecordWithMainText  ) this);
        case hdcPath           -> new HDI_OnlinePath          (schema, (HDT_RecordWithPath      ) this);
        case hdcBibEntryKey    -> new HDI_OnlineBibEntryKey   (schema, (HDT_Work                ) this);
        case hdcAuthors        -> new HDI_OnlineAuthors       (schema, (HDT_RecordWithAuthors<?>) this);
        case hdcHubSpokes      -> new HDI_OnlineHubSpokes     (schema, (HDT_Hub                 ) this);
        case hdcPersonName     -> new HDI_OnlinePersonName    (schema, (HDT_Person              ) this);

        case hdcBibDate        -> new HDI_OnlineBibDate       (schema, this);
        case hdcBoolean        -> new HDI_OnlineBoolean       (schema, this);
        case hdcTernary        -> new HDI_OnlineTernary       (schema, this);
        case hdcPointerMulti   -> new HDI_OnlinePointerMulti  (schema, this);
        case hdcPointerSingle  -> new HDI_OnlinePointerSingle (schema, this);
        case hdcString         -> new HDI_OnlineString        (schema, this);

        case hdcNestedPointer  ->
        {
          internalErrorPopup(78933); // Nested items are only created in RelationSet.getNestedItem
          yield null;
        }
      };

      if (item != null)
        for (Tag tag : schema.tags()) items.put(tag, item);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    if ((db.isDeletionInProgress() == false) && (db.resolvingPointers() == false))
    {
      internalErrorPopup(29948);
      return;
    }

    if (expired) return;

    if (dummyFlag == false)
      db.getRecordDeleteHandlers().forEach(handler -> handler.accept(this));

    items.values().forEach(HDI_OnlineBase::expire);

    id = -1;
    expired = true;

    db.setResolvePointersAgain();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void modifyNow()
  {
    if (db.runningConversion == false)
    {
      modifiedDate = Instant.now();
      //System.out.println("Modified: " + db.getTypeName(type) + " " + id + " " + dateTimeToUserReadableStr(modifiedDate));
    }

    if (online && (type != hdtFolder))
      db.updateMentioner(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final boolean changeID(int newID)
  {
    if ((type == hdtNone) ||
        db.isProtectedRecord(id, type, false) ||
        db.idAvailable(type, newID) == false)
      return false;

    int oldID = id;
    id = newID;

    try { dataset.changeRecordID(oldID, newID); } catch (HDB_InternalError e) { throw new AssertionError(e); }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void bringStoredCopyOnline(boolean rebuildMentions) throws RelationCycleException, SearchKeyException, RestoreException, HDB_InternalError
  {
    restoreTo(xmlState, rebuildMentions);
    if (rebuildMentions)
      db.resolvePointers();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override @SuppressWarnings({ "unchecked", "rawtypes" })
  public void restoreTo(RecordState backupState, boolean rebuildMentions) throws RelationCycleException, SearchKeyException, RestoreException, HDB_InternalError
  {
    boolean revertingConcept = online && (type == hdtConcept);
    online = true;

    if (type.getDisregardDates() == false)
    {
      creationDate = backupState.creationDate;
      modifiedDate = backupState.modifiedDate;
      viewDate     = backupState.viewDate;
    }

    for (Entry<Tag, HDI_OfflineBase> backupEntry : backupState.items.entrySet())
    {
      Tag tag = backupEntry.getKey();

      if (revertingConcept && ((tag == tagGlossary) || (tag == tagParentConcept) || (tag == nameTag)))
        continue;

      HDI_OnlineBase liveValue = items.get(tag);
      HDI_OfflineBase backupValue = backupEntry.getValue();

      if (tag == tagFirstName)
        ((HDT_Person)this).setFirstNameInternal(((HDI_OfflinePersonName)backupValue).getFirstName(), false);
      else if (tag == tagLastName)
        ((HDT_Person)this).setLastNameInternal(((HDI_OfflinePersonName)backupValue).getLastName(), false);
      else if (tag == nameTag)
        setNameInternal(((HDI_OfflineString)backupValue).get(), false);
      else
        liveValue.setFromOfflineValue(backupValue, tag);
    }

    if (revertingConcept == false)
      setSearchKey(backupState.searchKey, true, rebuildMentions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final void saveToStoredState() throws HDB_InternalError
  {
    if (online == false)
      throw new HDB_InternalError(62039);

    xmlState = getRecordStateBackup();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override @SuppressWarnings({ "unchecked" })
  public final RecordState getRecordStateBackup()
  {
    String searchKey = type == hdtWorkLabel ? "" : getSearchKey();
    RecordState newState = new RecordState(type, id, getSortKeyAttr(), name(), searchKey, "");
    newState.stored = false;

    if (type.getDisregardDates() == false)
    {
      newState.creationDate = getCreationDate();
      newState.modifiedDate = getModifiedDate();
      newState.viewDate     = getViewDate();
    }

    newState.items.forEach((tag, offlineItem) -> ((HDI_OnlineBase<HDI_OfflineBase>) items.get(tag)).getToOfflineValue(offlineItem, tag));

    return newState;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final void assignID() throws HDB_InternalError
  {
    if (id != -1)
      throw new HDB_InternalError(74102);

    id = dataset.recordIDtoAssign(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String updateString(String dest, String val)
  {
    val = safeStr(val);
    if (dest.replace("\r", "").equalsIgnoreCase(val.replace("\r", "")) == false)
      modifyNow();

    return val;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final void updateBibEntryKey(String val) // No need to change modified date for record
  {
    ((HDI_OnlineBibEntryKey)items.get(tagBibEntryKey)).set(val);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final void updateTagString(Tag tag, String val)
  {
    if (tag == nameTag)
      setNameInternal(val, true);
    else
      ((HDI_OnlineString)items.get(tag)).set(updateString(getTagString(tag), val));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final String getBibEntryKeyInternal()
  {
    return ((HDI_OnlineBibEntryKey)items.get(tagBibEntryKey)).get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final String getTagString(Tag tag)
  {
    return tag == nameTag ? name.get() : ((HDI_OnlineString)items.get(tag)).get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final BibliographicDate getBibDateInternal()
  {
    return ((HDI_OnlineBibDate)items.get(tagBibDate)).get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final void updateBibDate(BibliographicDate newBibDate)
  {
    HDI_OnlineBibDate item = (HDI_OnlineBibDate)items.get(tagBibDate);

    if (item.get().equals(newBibDate)) return;

    modifyNow();
    item.set(newBibDate);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final void updateTagBoolean(Tag tag, boolean val)
  {
    HDI_OnlineBoolean item = (HDI_OnlineBoolean)items.get(tag);

    if (item.get() == val) return;

    modifyNow();
    item.set(val);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final boolean updateObjectGroups(RelationType relType, List<ObjectGroup> newGroups, Collection<Tag> tags)
  {
    List<ObjectGroup> oldGroups = db.getObjectGroupList(relType, this, tags);

    try
    {
      if ((newGroups.size() != oldGroups.size()) ||
          (IntStream.range(0, newGroups.size()).anyMatch(ndx -> newGroups.get(ndx).equals(oldGroups.get(ndx)) == false)))
        db.updateObjectGroups(relType, this, newGroups);
    }
    catch (RelationCycleException e)
    {
      errorPopup(e);
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final <HDT_T extends HDT_RecordBase> boolean updateObjectsFromList(RelationType relType, List<HDT_T> list)
  {
    HyperObjList<HDT_Record, HDT_Record> objList = getObjList(relType);
    if (objList.equals(list)) return true;

    try
    {
      objList.cycleCheck(list);
    }
    catch (RelationCycleException e)
    {
      errorPopup(e);
      return false;
    }

    objList.clear();

    objList.addAll(list);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers() throws HDB_InternalError
  {
    if (db.resolvingPointers() == false) throw new HDB_InternalError(59928);

    for (HDI_OnlineBase<? extends HDI_OfflineBase> item : items.values())
      item.resolvePointers();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final String makeSortKey()
  {
    return makeSortKeyByType(makeSortKeyTypeSpecific(), type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected String makeSortKeyTypeSpecific()
  {
    return safeStr(sortKeyAttr).length() > 0 ? sortKeyAttr : name();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String makeSortKeyByType(String base, RecordType type)
  {
    switch (type)
    {
      case hdtFolder : case hdtWorkFile :

        return base;

      case hdtPerson :

        return convertToEnglishChars(base).toLowerCase().replace('|', '\u0000');

      default :

        if (base.toLowerCase().startsWith("the "))
          base = base.substring(4);
    }

    return convertToEnglishChars(base).toLowerCase().replace("\"", "").replace("'", "").replace("(", "").replace(")", "").trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getAllStrings(List<String> list, boolean searchLinkedRecords)
  {
    getSearchKeys().forEach(key -> list.add(key.text));

    items.forEach((tag, item) ->
    {
      if (tag == nameTag)
        list.add(name());
      else
        item.getStrings(list, tag, searchLinkedRecords);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String resultTextForTag(Tag tag)
  {
    return (tag == nameTag) || (tag == tagName) ?
      listName()
    :
      nullSwitch(items.get(tag), "", item -> item.getResultTextForTag(tag));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setRootRecordDates()
  {
    Instant dbCreationDate = db.getCreationDate();

    EnumSet.of(hdtFolder, hdtDebate, hdtNote, hdtWorkLabel, hdtPersonGroup, hdtGlossary).forEach(type ->
    {
      HDT_RecordBase record = (HDT_RecordBase) db.records(type).getByID(1);

      record.modifiedDate = dbCreationDate;
      record.creationDate = dbCreationDate;
      record.viewDate     = dbCreationDate;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
