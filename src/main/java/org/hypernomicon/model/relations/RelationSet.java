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

package org.hypernomicon.model.relations;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HyperDB.RelationChangeHandler;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_Record.*;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.util.EnumBasedTable;

import com.google.common.collect.ArrayListMultimap;

import static java.util.Objects.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.common.collect.HashBasedTable;
import com.google.common.collect.Table.Cell;

import javafx.application.Platform;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.records.HDT_Record.HyperDataCategory.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.HyperDB.Tag.*;

//---------------------------------------------------------------------------

public final class RelationSet<HDT_Subj extends HDT_Base, HDT_Obj extends HDT_Base>
{
  private final HashSet<HDT_Subj> orphans = new HashSet<>();
  private ArrayListMultimap<HDT_Obj, HDT_Subj> objToSubjList = ArrayListMultimap.create();
  private ArrayListMultimap<HDT_Subj, HDT_Obj> subjToObjList = ArrayListMultimap.create();
  private final HashBasedTable<HDT_Subj, HDT_Obj, LinkedHashMap<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> objectGroups = HashBasedTable.create();
  private final LinkedHashMap<Tag, HDI_Schema> tagToSchema = new LinkedHashMap<>();
  private final Map<Tag, HDT_RecordType> tagToTargetType = new EnumMap<>(Tag.class);
  private final ArrayList<RelationChangeHandler> changeHandlers = new ArrayList<>();

  private static final EnumMap<HDT_RecordType, Set<RelationSet<? extends HDT_Base, ? extends HDT_Base>>> orphanTypeToRelSets = new EnumMap<>(HDT_RecordType.class);
  private static final EnumBasedTable<HDT_RecordType, HDT_RecordType, RelationType> typeMappings = new EnumBasedTable<>(HDT_RecordType.class, HDT_RecordType.class);

  private final RelationType type;
  private final HDT_RecordType objType, subjType;
  private final boolean hasNestedItems;

  private boolean trackOrphans = false;

  public HDT_RecordType getObjType()                      { return objType; }
  public HDT_RecordType getSubjType()                     { return subjType; }
  public HDI_Schema getSchema(Tag tag)                    { return tagToSchema == null ? null : tagToSchema.get(tag); }
  public Collection<HDI_Schema> getSchemas()              { return tagToSchema.values(); }
  public HDT_RecordType getTargetType(Tag tag)            { return tagToTargetType.get(tag); }
  public boolean getHasNestedItems()                      { return hasNestedItems; }
  public void addChangeHandler(RelationChangeHandler rch) { changeHandlers.add(rch); }
  private void addObjAndMod(HDT_Subj subj, HDT_Obj obj)   { new HyperObjList<>(this, subj, true).add(obj); }
  List<HDT_Obj> getUnmodifiableObjectList(HDT_Subj subj)  { return Collections.unmodifiableList(subjToObjList.get(subj)); }
  List<HDT_Subj> getUnmodifiableSubjectList(HDT_Obj obj)  { return Collections.unmodifiableList(objToSubjList.get(obj)); }
  int getSubjectCount(HDT_Obj obj)                        { return objToSubjList.get(obj).size(); }
  int getObjectCount(HDT_Subj subj)                       { return subjToObjList.get(subj).size(); }
  HDT_Subj getSubject(HDT_Obj obj, int ndx)               { return objToSubjList.get(obj).get(ndx); }
  int getSubjectNdx(HDT_Obj obj, HDT_Subj subj)           { return objToSubjList.get(obj).indexOf(subj); }
  int getLastObjectNdx(HDT_Subj subj, HDT_Obj obj)        { return subjToObjList.get(subj).lastIndexOf(obj); }
  int getObjectNdx(HDT_Subj subj, HDT_Obj obj)            { return subjToObjList.get(subj).indexOf(obj); }
  HDT_Obj getObject(HDT_Subj subj, int ndx)               { return subjToObjList.get(subj).get(ndx); }
  boolean alreadyHasAsObject(HDT_Subj subj, HDT_Obj obj)  { return subjToObjList.containsEntry(subj, obj); }
  boolean alreadyHasAsSubject(HDT_Obj obj, HDT_Subj subj) { return objToSubjList.containsEntry(obj, subj); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private RelationSet(RelationType newType) throws HDB_InternalError
  {
    type = newType;

    switch (type)
    {
      case rtParentWorkOfWork :         hasNestedItems = false; subjType = hdtWork;          objType = hdtWork;            break;
      case rtParentGroupOfGroup :       hasNestedItems = false; subjType = hdtPersonGroup;   objType = hdtPersonGroup;     break;
      case rtParentLabelOfLabel :       hasNestedItems = false; subjType = hdtWorkLabel;     objType = hdtWorkLabel;

        trackOrphans = true; break;

      case rtCounterOfArgument :        hasNestedItems = true;  subjType = hdtArgument;      objType = hdtArgument;

        addNestedItem(hdcNestedPointer, tagArgumentVerdict, hdtArgumentVerdict);                   break;

      case rtParentDebateOfDebate :     hasNestedItems = false; subjType = hdtDebate;        objType = hdtDebate;

        trackOrphans = true; break;

      case rtParentNoteOfNote :         hasNestedItems = false; subjType = hdtNote;          objType = hdtNote;

        trackOrphans = true; break;

      case rtParentPosOfPos :           hasNestedItems = false; subjType = hdtPosition;      objType = hdtPosition;

        trackOrphans = true; break;

      case rtWorkOfArgument :           hasNestedItems = false; subjType = hdtArgument;      objType = hdtWork;            break;
      case rtDebateOfPosition :         hasNestedItems = false; subjType = hdtPosition;      objType = hdtDebate;

        trackOrphans = true; break;

      case rtPositionOfArgument :       hasNestedItems = true;  subjType = hdtArgument;      objType = hdtPosition;

        addNestedItem(hdcNestedPointer, tagPositionVerdict, hdtPositionVerdict);                   break;

      case rtAuthorOfWork :             hasNestedItems = true;  subjType = hdtWork;          objType = hdtPerson;

        addNestedItem(hdcTernary, tagInFileName);
        addNestedItem(hdcBoolean, tagEditor);
        addNestedItem(hdcBoolean, tagTranslator);                                                  break;

      case rtAuthorOfFile :             hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtPerson;          break;
      case rtStatusOfPerson :           hasNestedItems = false; subjType = hdtPerson;        objType = hdtPersonStatus;    break;
      case rtFieldOfPerson :            hasNestedItems = false; subjType = hdtPerson;        objType = hdtField;           break;
      case rtSubfieldOfPerson :         hasNestedItems = false; subjType = hdtPerson;        objType = hdtSubfield;        break;
      case rtFieldOfSubfield :          hasNestedItems = false; subjType = hdtSubfield;      objType = hdtField;           break;
      case rtRankOfPerson :             hasNestedItems = false; subjType = hdtPerson;        objType = hdtRank;            break;
      case rtInvestigationOfWork :      hasNestedItems = false; subjType = hdtWork;          objType = hdtInvestigation;   break;
      case rtPersonOfInv :              hasNestedItems = false; subjType = hdtInvestigation; objType = hdtPerson;          break;
      case rtInstOfPerson :             hasNestedItems = false; subjType = hdtPerson;        objType = hdtInstitution;     break;
      case rtTypeOfInst :               hasNestedItems = false; subjType = hdtInstitution;   objType = hdtInstitutionType; break;
      case rtParentInstOfInst :         hasNestedItems = false; subjType = hdtInstitution;   objType = hdtInstitution;     break;
      case rtStateOfInst :              hasNestedItems = false; subjType = hdtInstitution;   objType = hdtState;           break;
      case rtCountryOfInst :            hasNestedItems = false; subjType = hdtInstitution;   objType = hdtCountry;         break;
      case rtTypeOfWork :               hasNestedItems = false; subjType = hdtWork;          objType = hdtWorkType;        break;
      case rtTypeOfFile :               hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtFileType;        break;
      case rtConceptOfTerm :            hasNestedItems = false; subjType = hdtTerm;          objType = hdtConcept;         break;
      case rtGlossaryOfConcept :        hasNestedItems = false; subjType = hdtConcept;       objType = hdtGlossary;        break;
      case rtParentGlossaryOfGlossary : hasNestedItems = false; subjType = hdtGlossary;      objType = hdtGlossary;        break;
      case rtLabelOfWork :              hasNestedItems = false; subjType = hdtWork;          objType = hdtWorkLabel;       break;
      case rtLabelOfFile :              hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtWorkLabel;       break;
      case rtWorkOfMiscFile :           hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtWork;            break;
      case rtWorkFileOfWork :           hasNestedItems = true;  subjType = hdtWork;          objType = hdtWorkFile;

        addNestedItem(hdcString, tagStartPageNum);
        addNestedItem(hdcString, tagEndPageNum);                                                   break;

      case rtFolderOfWorkFile :         hasNestedItems = false; subjType = hdtWorkFile;      objType = hdtFolder;          break;
      case rtFolderOfMiscFile :         hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtFolder;          break;
      case rtParentFolderOfFolder :     hasNestedItems = false; subjType = hdtFolder;        objType = hdtFolder;          break;
      case rtFolderOfNote :             hasNestedItems = false; subjType = hdtNote;          objType = hdtFolder;          break;

      default :                         hasNestedItems = false; subjType = hdtNone;          objType = hdtNone;

        messageDialog("Internal error #84723", mtError);                                           break;
    }

    typeMappings.put(subjType, objType, type);

    if (trackOrphans)
    {
      Set<RelationSet<? extends HDT_Base, ? extends HDT_Base>> relSets = orphanTypeToRelSets.get(subjType);
      if (relSets == null)
      {
        relSets = new HashSet<>();
        orphanTypeToRelSets.put(subjType, relSets);
      }

      relSets.add(this);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDT_Subj> getOrphans()
  {
    if (orphans.isEmpty()) return Collections.unmodifiableSet(orphans);

    return Collections.unmodifiableSet(new HashSet<>(orphans)); // Need to make a new copy of the set to prevent concurrent modification exception
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addOrphanToAll(HDT_Base orphan)
  {
    Set<RelationSet<? extends HDT_Base, ? extends HDT_Base>> relSets = orphanTypeToRelSets.get(orphan.getType());

    if (relSets == null) return;

    relSets.forEach(relSet-> relSet.addOrphan(orphan));
  }

  @SuppressWarnings("unchecked")
  private void addOrphan(HDT_Base orphan)
  {
    if ((subjToObjList.containsKey(orphan) == false) && (HyperDB.isUnstoredRecord(orphan.getID(), subjType) == false))
      orphans.add((HDT_Subj) orphan);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static RelationType getRelation(HDT_RecordType subjType, HDT_RecordType objType)
  {
    return nullSwitch(typeMappings.get(subjType, objType), RelationType.rtNone);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EnumSet<RelationType> getRelationsForObjType(HDT_RecordType objType)
  {
    Collection<RelationType> relTypes = typeMappings.getColumn(objType);
    return collEmpty(relTypes) ? EnumSet.noneOf(RelationType.class) : EnumSet.copyOf(relTypes);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EnumSet<RelationType> getRelationsForSubjType(HDT_RecordType subjType)
  {
    Collection<RelationType> relTypes = typeMappings.getRow(subjType);
    return collEmpty(relTypes) ? EnumSet.noneOf(RelationType.class) : EnumSet.copyOf(relTypes);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<Tag> getNestedTags()
  {
    HashSet<Tag> set = new HashSet<>();

    if (tagToSchema != null)
      set.addAll(tagToSchema.keySet());

    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings({ "unchecked" })
  public void saveNestedValuesToOfflineMap(HDT_Subj subj, HDT_Obj obj, Map<Tag, HDI_OfflineBase> tagToNestedItem, HDT_RecordState recordState)
  {
    LinkedHashMap<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>> items = objectGroups.get(subj, obj);
    if (items == null) return;

    items.forEach((tag, onlineItem) ->
    {
      HDI_OfflineBase offlineItem = null;
      HDI_Schema schema = getSchema(tag);

      switch (schema.getCategory())
      {
        case hdcString :        if (NestedValue.isEmpty(HDI_OnlineString.class.cast(onlineItem).get()) == false)
                                  offlineItem = new HDI_OfflineString(schema, recordState);
          break;

        case hdcBoolean :       if (NestedValue.isEmpty(HDI_OnlineBoolean.class.cast(onlineItem).get()) == false)
                                  offlineItem = new HDI_OfflineBoolean(schema, recordState);
          break;

        case hdcTernary :       if (NestedValue.isEmpty(HDI_OnlineTernary.class.cast(onlineItem).get()) == false)
                                  offlineItem = new HDI_OfflineTernary(schema, recordState);
          break;

        case hdcNestedPointer : if (HDT_Record.isEmpty(HDI_OnlineNestedPointer.class.cast(onlineItem).get()) == false)
                                  offlineItem = new HDI_OfflineNestedPointer(schema, recordState);
          break;

        default : break;
      }

      if (offlineItem == null) return;

      HDI_OnlineBase.class.cast(onlineItem).getToOfflineValue(offlineItem, tag);
      tagToNestedItem.put(tag, offlineItem);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public <HDI_Offline extends HDI_OfflineBase> void setNestedItemFromOfflineValue(HDT_Subj subj, HDT_Obj obj, Tag tag, HDI_Offline value) throws RelationCycleException
  {
    if (hasNestedItems == false) { falseWithErrorMessage("Internal error #49221"); return; }

    boolean isEmpty;

    switch (value.getCategory())
    {
      case hdcBoolean       : isEmpty = NestedValue.isEmpty(HDI_OfflineBoolean      .class.cast(value).get     ()); break;
      case hdcTernary       : isEmpty = NestedValue.isEmpty(HDI_OfflineTernary      .class.cast(value).get     ()); break;
      case hdcString        : isEmpty = NestedValue.isEmpty(HDI_OfflineString       .class.cast(value).get     ()); break;
      case hdcNestedPointer : isEmpty = NestedValue.isEmpty(HDI_OfflineNestedPointer.class.cast(value).getObjID()); break;
      default : return;
    }

    HDI_OnlineBase<HDI_Offline> item = getNestedItem(subj, obj, tag, isEmpty);
    if (item != null) item.setFromOfflineValue(value, tag);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Returns true if changed

  public boolean setNestedString(HDT_Subj subj, HDT_Obj obj, Tag tag, String str)
  {
    if (hasNestedItems == false) return falseWithErrorMessage("Internal error #49225");

    str = safeStr(str);
    boolean isEmpty = NestedValue.isEmpty(str);
    if (isEmpty == false) addObjAndMod(subj, obj);

    HDI_OnlineString item = getNestedItem(subj, obj, tag, isEmpty);

    if ((item == null) || item.get().equals(str)) return false;

    item.set(str);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Returns true if changed

  public boolean setNestedBoolean(HDT_Subj subj, HDT_Obj obj, Tag tag, boolean bool)
  {
    if (hasNestedItems == false) return falseWithErrorMessage("Internal error #49224");

    boolean isEmpty = NestedValue.isEmpty(bool);
    if (isEmpty == false) addObjAndMod(subj, obj);

    HDI_OnlineBoolean item = getNestedItem(subj, obj, tag, isEmpty);

    if ((item == null) || (item.get() == bool)) return false;

    item.set(bool);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Returns true if changed

  public boolean setNestedTernary(HDT_Subj subj, HDT_Obj obj, Tag tag, Ternary ternary)
  {
    if (hasNestedItems == false) return falseWithErrorMessage("Internal error #49224");

    boolean isEmpty = NestedValue.isEmpty(ternary);
    if (isEmpty == false) addObjAndMod(subj, obj);

    HDI_OnlineTernary item = getNestedItem(subj, obj, tag, isEmpty);

    if ((item == null) || (item.get() == ternary)) return false;

    item.set(ternary);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Returns true if changed

  public boolean setNestedPointer(HDT_Subj subj, HDT_Obj obj, Tag tag, HDT_Base target)
  {
    if (hasNestedItems == false) return falseWithErrorMessage("Internal error #49223");

    boolean isEmpty = HDT_Record.isEmpty(target);
    if (isEmpty == false) addObjAndMod(subj, obj);

    HDI_OnlineNestedPointer item = getNestedItem(subj, obj, tag, isEmpty);

    if ((item == null) || (item.get() == target)) return false;

    item.set(target);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<ObjectGroup> getObjectGroupList(HDT_Subj subj, Collection<Tag> tags)
  {
    ArrayList<ObjectGroup> list = new ArrayList<>();
    HashSet<HDT_Obj> objSet = new LinkedHashSet<>();

    if (subjToObjList.containsKey(subj))
      objSet.addAll(subjToObjList.get(subj));

    Map<HDT_Obj, LinkedHashMap<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> objToObjItems = objectGroups.row(subj);

    objToObjItems.forEach((primary, items) ->
    {
      ObjectGroup group = new ObjectGroup(primary);
      objSet.remove(primary);

      items.forEach((tag, value) ->
      {
        if (tags.contains(tag))
          group.addNestedEntry(tag, new NestedValue(value));
      });

      list.add(group);
    });

    // Now make sure there is an empty object group for all remaining objects, and keep the list in the same order as the objects

    objSet.forEach(obj -> list.add(new ObjectGroup(obj)));

    List<HDT_Obj> origList = subjToObjList.get(subj);

    list.sort((og1, og2) -> origList.indexOf(og1.getPrimary()) - origList.indexOf(og2.getPrimary()));

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private <HDI_Offline extends HDI_OfflineBase, HDI_Online extends HDI_OnlineBase<HDI_Offline>>
                HDI_Online getNestedItem(HDT_Subj subj, HDT_Obj obj, Tag tag, boolean noCreate)
  {
    LinkedHashMap<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>> items = objectGroups.get(subj, obj);

    if (items == null)
    {
      if (noCreate) return null;
      items = new LinkedHashMap<>();
      objectGroups.put(subj, obj, items);
    }

    if (items.containsKey(tag)) return (HDI_Online) items.get(tag);
    if (noCreate) return null;

    HDI_OnlineBase<? extends HDI_OfflineBase> item;

    switch (getSchema(tag).getCategory())
    {
      case hdcBoolean       : item = new HDI_OnlineBoolean      (getSchema(tag), subj); break;
      case hdcTernary       : item = new HDI_OnlineTernary      (getSchema(tag), subj); break;
      case hdcString        : item = new HDI_OnlineString       (getSchema(tag), subj); break;
      case hdcNestedPointer : item = new HDI_OnlineNestedPointer(getSchema(tag), subj); break;
      default : return null;
    }

    items.put(tag, item);
    return (HDI_Online) item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getNestedString(HDT_Subj subj, HDT_Obj obj, Tag tag)
  {
    if (hasNestedItems == false) { falseWithErrorMessage("Internal error #49226"); return ""; }

    return nullSwitch(getNestedItem(subj, obj, tag, true), "", HDI_OnlineString::get);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean getNestedBoolean(HDT_Subj subj, HDT_Obj obj, Tag tag)
  {
    if (hasNestedItems == false) return falseWithErrorMessage("Internal error #49227");

    return nullSwitch(getNestedItem(subj, obj, tag, true), false, HDI_OnlineBoolean::get);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Ternary getNestedTernary(HDT_Subj subj, HDT_Obj obj, Tag tag)
  {
    if (hasNestedItems == false) { falseWithErrorMessage("Internal error #49227"); return Ternary.Unset; }

    return nullSwitch(getNestedItem(subj, obj, tag, true), Ternary.Unset, HDI_OnlineTernary::get);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Base getNestedPointer(HDT_Subj subj, HDT_Obj obj, Tag tag)
  {
    if (hasNestedItems == false) { falseWithErrorMessage("Internal error #49228"); return null; }

    return nullSwitch(getNestedItem(subj, obj, tag, true), null, HDI_OnlineNestedPointer::get);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void updateObjectGroups(HDT_Subj subj, List<ObjectGroup> groups)
  {
    HyperObjList<HDT_Subj, HDT_Obj> list = new HyperObjList<>(this, subj, true);

    list.clear();

    groups.forEach(group ->
    {
      HDT_Obj obj = group.getPrimary();

      if (obj == null) return;

      if (list.add(obj) == false)
      {
        try                              { list.throwLastException(); }
        catch (RelationCycleException e) { messageDialog(e.getMessage(), mtError); }
        return;
      }

      tagToSchema.keySet().forEach(tag ->
      {
        NestedValue value = group.getValue(tag);
        if (value == null) return;

        switch (value.hdc)
        {
          case hdcString        : setNestedString (subj, obj, tag, value.str    ); break;
          case hdcBoolean       : setNestedBoolean(subj, obj, tag, value.bool   ); break;
          case hdcTernary       : setNestedTernary(subj, obj, tag, value.ternary); break;
          case hdcNestedPointer : setNestedPointer(subj, obj, tag, value.target ); break;
          default : break;
        }
      });
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  void setObject(HDT_Subj subj, HDT_Obj obj, int ndx, boolean affirm) throws RelationCycleException
  {
    if ((subj == null) || (obj == null))
    {
      messageDialog("Internal error #30299", mtError);
      throw new NullPointerException();
    }

    List<HDT_Obj> objList = subjToObjList.get(subj);

    if (affirm)
    {
      if (obj.getType() == subj.getType())
      {
        if (subj.getID() == obj.getID())
          throw new RelationCycleException(subj.getID(), subj.getType(), obj.getID(), obj.getType());
        cycleCheck(subj, (HDT_Subj) obj, obj);
      }

      // Add the object to the object list if not already there
      if (objList.contains(obj)) return;

      if (ndx == -1) objList.add(obj);
      else           objList.add(ndx, obj);

      objToSubjList.put(obj, subj);

      orphans.remove(subj);

      Platform.runLater(() -> changeHandlers.forEach(handler -> handler.handle(subj, obj, true)));

      return;
    }

    // Remove the object from the object list if it was there;
    if (objList.contains(obj))
    {
      if (ndx == -1) objList.remove(obj); // removes first occurrence
      else           objList.remove(ndx);

      if (objList.contains(obj) == false)
      {
        objToSubjList.remove(obj, subj);

        if (HDT_Record.isEmpty(subj) == false) // skip if record is in the process of being deleted
        {
          if (trackOrphans && objList.isEmpty() && (HyperDB.isUnstoredRecord(subj.getID(), subjType) == false))
            orphans.add(subj);

          if ((HDT_Record.isEmpty(subj) == false) && (HDT_Record.isEmpty(obj) == false))  // Only run change handlers if the record is not in the process of being deleted
            Platform.runLater(() -> changeHandlers.forEach(handler -> handler.handle(subj, obj, false)));
        }
      }
    }

    if (hasNestedItems)
      objectGroups.remove(subj, obj);

    if (type == RelationType.rtWorkFileOfWork)
      if (getSubjectCount(obj) == 0)
        if (obj.isExpired() == false) // The obj record may have just been deleted, and the pointers are still being resolved
          db.deleteRecord(hdtWorkFile, obj.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private void cycleCheck(HDT_Subj subj, HDT_Subj obj, HDT_Obj origObj) throws RelationCycleException
  {
    for (HDT_Obj nextObj : subjToObjList.get(obj))
    {
      if (nextObj == subj)
        throw new RelationCycleException(subj.getID(), subj.getType(), origObj.getID(), obj.getType());

      cycleCheck(subj, (HDT_Subj) nextObj, origObj);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearObjects(HDT_Subj subj)
  {
    while (getObjectCount(subj) > 0)
    {
      HDT_Obj obj = getObject(subj, 0);
      try { setObject(subj, obj, 0, false); } catch (RelationCycleException e) { noOp(); }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <HDT_Key extends HDT_Base, HDT_Value extends HDT_Base> ArrayListMultimap<HDT_Key, HDT_Value> rebuildMultimap(ArrayListMultimap<HDT_Key, HDT_Value> oldMap) throws HDB_InternalError
  {
    ArrayListMultimap<HDT_Key, HDT_Value> newMap = ArrayListMultimap.create();

    for (Entry<HDT_Key, HDT_Value> entry : oldMap.entries())
    {
      HDT_Key key = entry.getKey();

      if (HDT_Record.isEmptyThrowsException(key) == false)
      {
        HDT_Value value = entry.getValue();
        if (HDT_Record.isEmptyThrowsException(value) == false)
          newMap.put(key, value);
      }
    }

    return newMap;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void cleanup() throws HDB_InternalError
  {
    subjToObjList = rebuildMultimap(subjToObjList);
    objToSubjList = rebuildMultimap(objToSubjList);

    Iterator<HDT_Subj> orphanIt = orphans.iterator();
    while (orphanIt.hasNext())
    {
      if (HDT_Record.isEmptyThrowsException(orphanIt.next())) orphanIt.remove();
    }

    if (hasNestedItems == false) return;

    Iterator<Cell<HDT_Subj, HDT_Obj, LinkedHashMap<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>>> cellIt = objectGroups.cellSet().iterator();

    while (cellIt.hasNext())
    {
      Cell<HDT_Subj, HDT_Obj, LinkedHashMap<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> cell = cellIt.next();

      if      (HDT_Record.isEmptyThrowsException(cell.getRowKey()))    cellIt.remove();
      else if (HDT_Record.isEmptyThrowsException(cell.getColumnKey())) cellIt.remove();
      else
      {
        Iterator<Entry<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> targetIt = cell.getValue().entrySet().iterator();

        while (targetIt.hasNext())
        {
          HDI_OnlineBase<? extends HDI_OfflineBase> item = targetIt.next().getValue();

          if (item.getCategory() == hdcNestedPointer)
            if (HDT_Record.isEmptyThrowsException(HDI_OnlineNestedPointer.class.cast(item).get())) targetIt.remove();
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void resolvePointers(HDT_Subj subj) throws HDB_InternalError
  {
    List<HDT_Obj> list = subjToObjList.get(subj);

    for (int ndx = 0; ndx < getObjectCount(subj); ndx++)
    {
      HDT_Obj obj = list.get(ndx);

      if (HDT_Record.isEmptyThrowsException(obj))
      {
        try { setObject(subj, obj, ndx, false); } catch (RelationCycleException e) { noOp(); }
        ndx--;
      }
      else if (hasNestedItems)
      {
        LinkedHashMap<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>> nestedItemMap = objectGroups.get(subj, obj);
        if (nonNull(nestedItemMap))
        {
          for (HDI_OnlineBase<? extends HDI_OfflineBase> nestedItem : nestedItemMap.values())
            nestedItem.resolvePointers();
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Numeric code associated with each enum value should NEVER be changed.

  public static enum RelationType
  {
 // rtObjectOfSubject

    rtNone                    ( 1, ""),

    rtParentWorkOfWork        ( 2, "Work(s) under this parent work"),
    rtTypeOfWork              ( 3, "Work(s) of this type"),
    rtTypeOfFile              ( 4, "File(s) of this type"),
    rtAuthorOfWork            ( 5, "Work(s) by this author"),
    rtAuthorOfFile            ( 6, "File(s) by this author"),
    rtWorkOfArgument          ( 7, "Argument(s) having this work as source"),
    rtParentLabelOfLabel      (11, "Label(s) under this parent label"),
    rtLabelOfWork             (12, "Work(s) having this label"),
    rtLabelOfFile             (13, "File(s) having this label"),
    rtCounterOfArgument       (14, "Argument(s) with this counterargument"),
    rtStatusOfPerson          (15, "Person(s) having this status"),
    rtFieldOfPerson           (16, "Person(s) having this field"),
    rtSubfieldOfPerson        (17, "Person(s) having this subfield"),
    rtFieldOfSubfield         (18, "Subfield(s) under this field"),
    rtRankOfPerson            (19, "Person(s) having this rank"),
    rtParentDebateOfDebate    (20, "Debate(s) under this larger debate"),
    rtParentNoteOfNote        (21, "Note(s) under this parent note"),
    rtFolderOfNote            (22, "Folder of this note"),
    rtDebateOfPosition        (23, "Position(s) under this debate"),
    rtParentPosOfPos          (24, "Position(s) under this parent position"),
    rtPositionOfArgument      (25, "Argument(s) concerning this position"),
    rtInvestigationOfWork     (26, "Work(s) in this investigation"),
    rtPersonOfInv             (27, "Investigation(s) by this person"),
    rtStateOfInst             (30, "Institution(s) in this state"),
    rtCountryOfInst           (31, "Institution(s) in this country"),
    rtParentGroupOfGroup      (32, "Group(s) under this parent group"),
    rtWorkOfMiscFile          (33, "Work of this miscellaneous file"),
    rtWorkFileOfWork          (34, "File(s) of this work"),
    rtFolderOfWorkFile        (35, "Folder of this work file"),
    rtFolderOfMiscFile        (36, "Folder of this misc. file"),
    rtParentFolderOfFolder    (37, "Parent folder of this folder"),
    rtUnited                  (38, ""),
    rtTypeOfInst              (39, "Institution(s) of this type"),
    rtParentInstOfInst        (40, "Institution(s) under this larger institution"),
    rtInstOfPerson            (41, "Person(s) in this institution"),
    rtGlossaryOfConcept       (43, "Concept(s) in this glossary"),
    rtParentGlossaryOfGlossary(44, "Parent glossary of this glossary"),
    rtConceptOfTerm           (45, "Term(s) associated with this concept");

    private final int code;
    private final String title;
    private static final HashMap<Integer, RelationType> codeToVal;

    static
    {
      codeToVal = new HashMap<>();
      EnumSet.allOf(RelationType.class).forEach(val -> codeToVal.put(val.getCode(), val));
    }

    private RelationType(int code, String title)  { this.code = code; this.title = title; }
    public static RelationType codeToVal(int num) { return codeToVal.get(num); }
    public final int getCode()                    { return code; }
    public final String getTitle()                { return title; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addNestedItem(HyperDataCategory dataCat, Tag tag) throws HDB_InternalError
  {
    addNestedItem(dataCat, tag, hdtNone);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addNestedItem(HyperDataCategory dataCat, Tag tag, HDT_RecordType targetType) throws HDB_InternalError
  {
    HDI_Schema schema = new HDI_Schema(dataCat, type, tag);

    if (tagToSchema.containsKey(tag))
      throw new HDB_InternalError(98925);

    tagToSchema.put(tag, schema);

    if (dataCat == hdcNestedPointer)
      tagToTargetType.put(tag, targetType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reorderObjects (HDT_Subj subj, ArrayList<HDT_Obj>  newObjList)  { reorderList(subj, newObjList,  subjToObjList); }
  void reorderSubjects(HDT_Obj   obj, ArrayList<HDT_Subj> newSubjList) { reorderList(obj,  newSubjList, objToSubjList); }

  private <HDT_Key extends HDT_Base, HDT_Value extends HDT_Base> void reorderList(HDT_Key key, ArrayList<HDT_Value> newValueList, ArrayListMultimap<HDT_Key, HDT_Value> map)
  {
    if (key == null) throw new NullPointerException();

    if (map.containsKey(key) == false) return;

    List<HDT_Value> existingValueList = map.get(key);

    if (existingValueList.size() != newValueList.size()) return;

    for (HDT_Value value : newValueList)
      if (existingValueList.contains(value) == false) return;

    for (HDT_Value value : existingValueList)
      if (newValueList.contains(value) == false) return;

    for (int ndx = 0; ndx < existingValueList.size(); ndx++)
      existingValueList.set(ndx, newValueList.get(ndx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static RelationSet<? extends HDT_Base, ? extends HDT_Base> createSet(RelationType relType) throws HDB_InternalError
  {
    switch (relType)
    {
      case rtAuthorOfFile:             return new RelationSet<HDT_MiscFile,      HDT_Work>           (relType);
      case rtAuthorOfWork:             return new RelationSet<HDT_Work,          HDT_Person>         (relType);
      case rtConceptOfTerm:            return new RelationSet<HDT_Term,          HDT_Concept>        (relType);
      case rtCounterOfArgument:        return new RelationSet<HDT_Argument,      HDT_Argument>       (relType);
      case rtCountryOfInst:            return new RelationSet<HDT_Institution,   HDT_Country>        (relType);
      case rtDebateOfPosition:         return new RelationSet<HDT_Position,      HDT_Debate>         (relType);
      case rtFieldOfPerson:            return new RelationSet<HDT_Person,        HDT_Field>          (relType);
      case rtFieldOfSubfield:          return new RelationSet<HDT_Subfield,      HDT_Field>          (relType);
      case rtFolderOfMiscFile:         return new RelationSet<HDT_MiscFile,      HDT_Folder>         (relType);
      case rtFolderOfNote:             return new RelationSet<HDT_Note,          HDT_Folder>         (relType);
      case rtFolderOfWorkFile:         return new RelationSet<HDT_WorkFile,      HDT_Folder>         (relType);
      case rtGlossaryOfConcept:        return new RelationSet<HDT_Concept,       HDT_Glossary>       (relType);
      case rtInstOfPerson:             return new RelationSet<HDT_Person,        HDT_Institution>    (relType);
      case rtInvestigationOfWork:      return new RelationSet<HDT_Work,          HDT_Investigation>  (relType);
      case rtLabelOfFile:              return new RelationSet<HDT_MiscFile,      HDT_WorkLabel>      (relType);
      case rtLabelOfWork:              return new RelationSet<HDT_Work,          HDT_WorkLabel>      (relType);
      case rtParentDebateOfDebate:     return new RelationSet<HDT_Debate,        HDT_Debate>         (relType);
      case rtParentFolderOfFolder:     return new RelationSet<HDT_Folder,        HDT_Folder>         (relType);
      case rtParentGlossaryOfGlossary: return new RelationSet<HDT_Glossary,      HDT_Glossary>       (relType);
      case rtParentGroupOfGroup:       return new RelationSet<HDT_PersonGroup,   HDT_PersonGroup>    (relType);
      case rtParentInstOfInst:         return new RelationSet<HDT_Institution,   HDT_Institution>    (relType);
      case rtParentLabelOfLabel:       return new RelationSet<HDT_WorkLabel,     HDT_WorkLabel>      (relType);
      case rtParentNoteOfNote:         return new RelationSet<HDT_Note,          HDT_Note>           (relType);
      case rtParentPosOfPos:           return new RelationSet<HDT_Position,      HDT_Position>       (relType);
      case rtParentWorkOfWork:         return new RelationSet<HDT_Work,          HDT_Work>           (relType);
      case rtPersonOfInv:              return new RelationSet<HDT_Investigation, HDT_Person>         (relType);
      case rtPositionOfArgument:       return new RelationSet<HDT_Argument,      HDT_Position>       (relType);
      case rtRankOfPerson:             return new RelationSet<HDT_Person,        HDT_Rank>           (relType);
      case rtStateOfInst:              return new RelationSet<HDT_Institution,   HDT_State>          (relType);
      case rtStatusOfPerson:           return new RelationSet<HDT_Person,        HDT_PersonStatus>   (relType);
      case rtSubfieldOfPerson:         return new RelationSet<HDT_Person,        HDT_Subfield>       (relType);
      case rtTypeOfFile:               return new RelationSet<HDT_MiscFile,      HDT_FileType>       (relType);
      case rtTypeOfInst:               return new RelationSet<HDT_Institution,   HDT_InstitutionType>(relType);
      case rtTypeOfWork:               return new RelationSet<HDT_Work,          HDT_WorkType>       (relType);
      case rtWorkFileOfWork:           return new RelationSet<HDT_Work,          HDT_WorkFile>       (relType);
      case rtWorkOfArgument:           return new RelationSet<HDT_Argument,      HDT_Work>           (relType);
      case rtWorkOfMiscFile:           return new RelationSet<HDT_MiscFile,      HDT_Work>           (relType);
      default:                         return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
