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

package org.hypernomicon.model.records;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjList;
import org.hypernomicon.model.relations.HyperSubjPointer;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory.*;

//---------------------------------------------------------------------------

public abstract class HDT_RecordBase implements HDT_Record
{
//---------------------------------------------------------------------------

  public static enum HyperDataCategory
  {
    hdcPointerSingle, hdcPointerMulti, hdcNestedPointer,
    hdcString,        hdcConnector,    hdcBoolean,
    hdcTernary,       hdcPath,         hdcPersonName,
    hdcBibEntryKey,   hdcAuthors,      hdcHubSpokes
  }

//---------------------------------------------------------------------------

  public static enum HDT_DateType { dateTypeCreation, dateTypeModified, dateTypeView; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperDataset<? extends HDT_Record> dataset;
  protected final Tag nameTag;
  private final NameItem name;
  private final LinkedHashMap<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>> items;
  private final boolean dummyFlag;
  private final String sortKeyAttr;

  private int id;
  private Instant creationDate, modifiedDate, viewDate;
  private HDT_RecordState xmlState;

  private boolean online = false, expired = false;

  @Override public final Instant getModifiedDate()      { return getType().getDisregardDates() ? null : modifiedDate; }
  @Override public final Instant getViewDate()          { return getType().getDisregardDates() ? null : viewDate; }
  @Override public final Instant getCreationDate()      { return getType().getDisregardDates() ? null : creationDate; }
  @Override public final Tag getNameTag()               { return nameTag; }
  @Override public String getNameEngChar()              { return name.getEngChar(); }
  @Override public String getCBText()                   { return listName(); }
  @Override public String getXMLObjectName()            { return listName(); }
  @Override public boolean isUnitable()                 { return false; }
  @Override public final boolean isDummy()              { return dummyFlag; }
  @Override public final boolean hasMainText()          { return this instanceof HDT_RecordWithConnector; }
  @Override public final boolean hasDesc()              { return this instanceof HDT_RecordWithDescription; }
  @Override public final int getID()                    { return id; }
  @Override public final void viewNow()                 { if (db.viewTestingInProgress == false) viewDate = Instant.now(); }
  @Override public final String getSortKeyAttr()        { return sortKeyAttr; }
  @Override public String name()                        { return name.get(); }
  @Override public final String getSortKey()            { return dataset.getKeyByID(id); }
  @Override public String getSearchKey()                { return db.getSearchKey(this); }
  @Override public List<SearchKeyword> getSearchKeys()  { return db.getKeysByRecord(this); }
  @Override public String getFirstActiveKeyWord()       { return db.getFirstActiveKeyWord(this); }
  @Override public final boolean isExpired()            { return expired; }
  @Override public void setName(String str)             { setNameInternal(str, true); }
  @Override public final Set<Tag> getAllTags()          { return items.keySet().isEmpty() ? EnumSet.noneOf(Tag.class) : EnumSet.copyOf(items.keySet()); }
  @Override public final boolean getTagBoolean(Tag tag) { return HDI_OnlineBoolean.class.cast(items.get(tag)).get(); }
  @Override public final boolean hasStoredState()       { return xmlState.stored; }
  @Override public final void updateSortKey()           { if (dataset != null) dataset.updateSortKey(makeSortKey(), id); }
  @Override public final HDI_Schema getSchema(Tag tag)  { return nullSwitch(items.get(tag), null, HDI_Base::getSchema); }

  @Override public final void writeStoredStateToXML(StringBuilder xml)        { xmlState.writeToXML(xml); }
  @Override public void setSearchKey(String newKey) throws SearchKeyException { setSearchKey(newKey, false, false); }

  @Override public void setSearchKey(String newKey, boolean noMod, boolean dontRebuildMentions) throws SearchKeyException
  { db.setSearchKey(this, newKey, noMod, dontRebuildMentions); }

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

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HDT_RecordBase(HDT_RecordState xmlState, HyperDataset<? extends HDT_Record> dataset, Tag nameTag)
  {
    name = new NameItem();

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
    String curName = name.get();

    curName = update ? updateString(curName, str) : safeStr(str);
    name.set(curName);
    updateSortKey();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final void initItems()
  {
    Collection<HDI_Schema> schemas = db.getSchemasByRecordType(getType());
    if (schemas == null) return;

    for (HDI_Schema schema : schemas)
    {
      HDI_OnlineBase<? extends HDI_OfflineBase> item;

      switch (schema.getCategory())
      {
        case hdcConnector     : item = new HDI_OnlineConnector    (schema, (HDT_RecordWithConnector) this); break;
        case hdcPath          : item = new HDI_OnlinePath         (schema, (HDT_RecordWithPath     ) this); break;
        case hdcBibEntryKey   : item = new HDI_OnlineBibEntryKey  (schema, (HDT_Work               ) this); break;
        case hdcAuthors       : item = new HDI_OnlineAuthors      (schema, (HDT_Work               ) this); break;
        case hdcHubSpokes     : item = new HDI_OnlineHubSpokes    (schema, (HDT_Hub                ) this); break;

        case hdcBoolean       : item = new HDI_OnlineBoolean      (schema, this); break;
        case hdcTernary       : item = new HDI_OnlineTernary      (schema, this); break;
        case hdcPersonName    : item = new HDI_OnlinePersonName   (schema, this); break;
        case hdcPointerMulti  : item = new HDI_OnlinePointerMulti (schema, this); break;
        case hdcPointerSingle : item = new HDI_OnlinePointerSingle(schema, this); break;
        case hdcString        : item = new HDI_OnlineString       (schema, this); break;

        case hdcNestedPointer :
          messageDialog("Internal error #78933", mtError); // Nested items are only created in RelationSet.getNestedItem
          return;

        default : item = null;
      }

      schema.getTags().forEach(tag -> items.put(tag, item));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    if ((db.isDeletionInProgress() == false) && (db.resolvingPointers() == false))
    {
      messageDialog("Internal error #29948", mtError);
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
      //System.out.println("Modified: " + db.getTypeName(getType()) + " " + getID() + " " + dateTimeToUserReadableStr(modifiedDate));
    }

    if (online && (getType() != hdtFolder))
      db.updateMentioner(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final boolean changeID(int newID)
  {
    HDT_RecordType type = getType();

    if ((type == hdtNone) ||
        isProtectedRecord(id, type) ||
        db.idAvailable(type, newID) == false)
      return false;

    int oldID = id;
    id = newID;
    dataset.changeRecordID(oldID, newID);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void bringStoredCopyOnline(boolean dontRebuildMentions) throws RelationCycleException, SearchKeyException, HubChangedException
  {
    restoreTo(xmlState, dontRebuildMentions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override @SuppressWarnings({ "unchecked", "rawtypes" })
  public final void restoreTo(HDT_RecordState backupState, boolean dontRebuildMentions) throws RelationCycleException, SearchKeyException, HubChangedException
  {
    if (online && isUnitable())
    {
      HDT_RecordWithConnector uRecord = (HDT_RecordWithConnector)this;
      int curHubID = -1;

      if (uRecord.getHub() != null)
        curHubID = uRecord.getHub().getID();

      if (curHubID != HDI_OfflineConnector.class.cast(backupState.items.get(tagHub)).getHubID())
        throw new HubChangedException(curHubID >= 1);
    }

    online = true;

    if (getType().getDisregardDates() == false)
    {
      creationDate = backupState.creationDate;
      modifiedDate = backupState.modifiedDate;
      viewDate     = backupState.viewDate;
    }

    if (this instanceof HDT_SimpleRecord)
      setNameInternal(backupState.simpleName, false);

    int hubID = nullSwitch((HDI_OfflineConnector)backupState.items.get(tagHub), -1, HDI_OfflineConnector::getHubID);

    for (Entry<Tag, HDI_OfflineBase> backupEntry : backupState.items.entrySet())
    {
      Tag tag = backupEntry.getKey();

      if (tag == tagHub) continue; // handle hub after loop ends

      HDI_OnlineBase liveValue = items.get(tag);
      if ((liveValue.getCategory() == hdcConnector) && (hubID > 0)) // Correct data will be in hub's record state,
        continue;                                                   // not this one's

      HDI_OfflineBase backupValue = backupEntry.getValue();

      if (tag == tagFirstName)
        HDT_Person.class.cast(this).setFirstNameInternal(HDI_OfflinePersonName.class.cast(backupValue).getFirstName(), false);
      else if (tag == tagLastName)
        HDT_Person.class.cast(this).setLastNameInternal(HDI_OfflinePersonName.class.cast(backupValue).getLastName(), false);
      else if (tag == nameTag)
        setNameInternal(HDI_OfflineString.class.cast(backupValue).get(), false);
      else
        liveValue.setFromOfflineValue(backupValue, tag);
    }

    if (hubID > 0)  // this is being done last so it can overwrite an existing hypernomicon.view.mainText item
                    // See HDI_OnlineConnector constructor
      ((HDT_RecordWithConnector)this).connector.initFromHub(db.hubs.getByID(hubID));

    setSearchKey(backupState.searchKey, true, dontRebuildMentions);
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
  public final HDT_RecordState getRecordStateBackup()
  {
    HDT_RecordType type = getType();
    String searchKey = type == hdtWorkLabel ? "" : getSearchKey();
    HDT_RecordState newState = new HDT_RecordState(type, id, getSortKeyAttr(), name(), searchKey, "");
    newState.stored = false;

    if (getType().getDisregardDates() == false)
    {
      newState.creationDate = getCreationDate();
      newState.modifiedDate = getModifiedDate();
      newState.viewDate     = getViewDate();
    }

    newState.items.forEach((tag, offlineItem) -> HDI_OnlineBase.class.cast(items.get(tag)).getToOfflineValue(offlineItem, tag));

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

  protected final String updateString(String dest, String val)
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
    HDI_OnlineBibEntryKey.class.cast(items.get(tagBibEntryKey)).set(val);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final void updateTagString(Tag tag, String val)
  {
    if (tag == nameTag)
      setNameInternal(val, true);
    else
      HDI_OnlineString.class.cast(items.get(tag)).set(updateString(getTagString(tag), val));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final String getBibEntryKeyString()
  {
    return HDI_OnlineBibEntryKey.class.cast(items.get(tagBibEntryKey)).get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final String getTagString(Tag tag)
  {
    return tag == nameTag ? name.get() : HDI_OnlineString.class.cast(items.get(tag)).get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final void updateTagBoolean(Tag tag, boolean val)
  {
    HDI_OnlineBoolean item = HDI_OnlineBoolean.class.cast(items.get(tag));

    if (item.get() == val) return;

    modifyNow();
    item.set(val);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void updateObjectGroups(RelationType relType, List<ObjectGroup> newGroups, Collection<Tag> tags)
  {
    List<ObjectGroup> oldGroups = db.getObjectGroupList(relType, this, tags);

    if (newGroups.size() == oldGroups.size())
    {
      boolean theSame = true;

      for (int ndx = 0; ndx < newGroups.size(); ndx++)
        if (newGroups.get(ndx).equals(oldGroups.get(ndx)) == false)
          theSame = false;

      if (theSame) return;
    }

    db.updateObjectGroups(relType, this, newGroups);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final <HDT_T extends HDT_RecordBase> void updateObjectsFromList(RelationType relType, List<HDT_T> list)
  {
    HyperObjList<HDT_Record, HDT_Record> objList = getObjList(relType);
    if (objList.equals(list)) return;

    objList.clear();

    list.forEach(obj -> { if (objList.add(obj) == false)
    {
      try                              { objList.throwLastException(); }
      catch (RelationCycleException e) { messageDialog(e.getMessage(), mtError); }
    }});
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final void resolvePointers() throws HDB_InternalError
  {
    if (db.resolvingPointers() == false) throw new HDB_InternalError(59928);

    for (HDI_OnlineBase<? extends HDI_OfflineBase> item : items.values())
      item.resolvePointers();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final String makeSortKey()
  {
    String sortKey;
    HDT_RecordType type = getType();

    try
    {
      switch (type)
      {
        case hdtInstitution :

          HDT_Institution inst = (HDT_Institution)this;
          if (inst.parentInst.isNotNull())
          {
            int parentType = inst.parentInst.get().instType.getID();

            if ((parentType == 1) || (parentType == 6))
              sortKey = inst.name();
            else
              sortKey = inst.parentInst.get().name() + " " + inst.name();
          }
          else
            sortKey = inst.name();

          break;

        case hdtConcept :

          sortKey = HDT_Concept.class.cast(this).term.get().name();
          break;

        case hdtFolder : case hdtWorkFile :

          sortKey = HDT_RecordWithPath.class.cast(this).getPath().getNameStr();
          break;

        default :

          sortKey = sortKeyAttr.length() > 0 ? sortKeyAttr : name.get();
          break;
      }
    }
    catch (NullPointerException e)
    {
      return "";
    }

    return makeSortKeyByType(sortKey, type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final String makeSortKeyByType(String base, HDT_RecordType type)
  {
    switch (type)
    {
      case hdtFolder : case hdtWorkFile :

        return base;

      case hdtPerson :

        return convertToEnglishChars(base).toLowerCase().replace('|', '\u0000');

      default :

        if (base.toLowerCase().startsWith("the "))
          base = base.substring(4, base.length());
    }

    return convertToEnglishChars(base).toLowerCase().replace("\"", "").replace("'", "").replace("(", "").replace(")", "").trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getAllStrings(ArrayList<String> list, boolean searchLinkedRecords)
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

  @Override public final String getResultTextForTag(Tag tag)
  {
    if ((tag == nameTag) || (tag == tagName))
      return listName();

    return nullSwitch(items.get(tag), "", item -> item.getResultTextForTag(tag));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setDates(Instant newDate)
  {
    modifiedDate = newDate;
    creationDate = newDate;
    viewDate     = newDate;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setRootRecordDates()
  {
    Instant dbCreationDate = db.getCreationDate();

    HDT_RecordBase.class.cast(db.folders     .getByID(1)).setDates(dbCreationDate);
    HDT_RecordBase.class.cast(db.debates     .getByID(1)).setDates(dbCreationDate);
    HDT_RecordBase.class.cast(db.notes       .getByID(1)).setDates(dbCreationDate);
    HDT_RecordBase.class.cast(db.workLabels  .getByID(1)).setDates(dbCreationDate);
    HDT_RecordBase.class.cast(db.personGroups.getByID(1)).setDates(dbCreationDate);
    HDT_RecordBase.class.cast(db.glossaries  .getByID(1)).setDates(dbCreationDate);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
