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

package org.hypernomicon.model.relations;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_RecordBase.*;
import org.hypernomicon.util.EnumBasedTable;

import com.google.common.collect.ArrayListMultimap;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
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
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Table.Cell;

import javafx.application.Platform;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

//---------------------------------------------------------------------------

public final class RelationSet<HDT_Subj extends HDT_Record, HDT_Obj extends HDT_Record>
{
  private final Set<HDT_Subj> orphans = new HashSet<>();
  private ArrayListMultimap<HDT_Obj, HDT_Subj> objToSubjList = ArrayListMultimap.create();
  private ArrayListMultimap<HDT_Subj, HDT_Obj> subjToObjList = ArrayListMultimap.create();
  private final HashBasedTable<HDT_Subj, HDT_Obj, Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> objectGroups = HashBasedTable.create();
  private final Map<Tag, HDI_Schema> tagToSchema = new LinkedHashMap<>();
  private final Map<Tag, RecordType> tagToTargetType = new EnumMap<>(Tag.class);
  private final List<RelationChangeHandler> changeHandlers = new ArrayList<>();
  private final Set<RelationType> cycleGroup;

  private static final EnumMap<RecordType, Set<RelationSet<? extends HDT_Record, ? extends HDT_Record>>> orphanTypeToRelSets = new EnumMap<>(RecordType.class);
  private static final EnumMap<RelationType, RelationSet<? extends HDT_Record, ? extends HDT_Record>> relationSets = new EnumMap<>(RelationType.class);
  private static final EnumBasedTable<RecordType, RecordType, RelationType> typeMappings = new EnumBasedTable<>(RecordType.class, RecordType.class);

  private final RelationType type;
  private final RecordType objType, subjType;
  private final boolean hasNestedItems, trackOrphans;

  public RecordType getObjType()                          { return objType; }
  public RecordType getSubjType()                         { return subjType; }
  public HDI_Schema getSchema(Tag tag)                    { return tagToSchema.get(tag); }
  public Collection<HDI_Schema> getSchemas()              { return tagToSchema.values(); }
  public RecordType getTargetType(Tag tag)                { return tagToTargetType.get(tag); }
  public boolean getHasNestedItems()                      { return hasNestedItems; }
  public Set<Tag> getNestedTags()                         { return new HashSet<>(tagToSchema.keySet()); }
  public void addChangeHandler(RelationChangeHandler rch) { changeHandlers.add(rch); }
  public Set<HDT_Subj> getOrphans()                       { return ImmutableSet.copyOf(orphans); } // Make a new copy of the set to prevent concurrent modification exception
  private void addObjAndMod(HDT_Subj subj, HDT_Obj obj)   { new HyperObjList<>(this, subj, true).add(obj); }
  List<HDT_Obj> getUnmodifiableObjectList(HDT_Subj subj)  { return Collections.unmodifiableList(subjToObjList.get(subj)); }
  List<HDT_Subj> getUnmodifiableSubjectList(HDT_Obj obj)  { return Collections.unmodifiableList(objToSubjList.get(obj)); }
  int getSubjectCount(HDT_Obj obj)                        { return objToSubjList.get(obj).size(); }
  int getObjectCount(HDT_Subj subj)                       { return subjToObjList.get(subj).size(); }
  HDT_Subj getSubject(HDT_Obj obj, int ndx)               { return objToSubjList.get(obj).get(ndx); }
  int getSubjectNdx(HDT_Obj obj, HDT_Subj subj)           { return objToSubjList.get(obj).indexOf(subj); }
  int getObjectNdx(HDT_Subj subj, HDT_Obj obj)            { return subjToObjList.get(subj).indexOf(obj); }
  HDT_Obj getObject(HDT_Subj subj, int ndx)               { return subjToObjList.get(subj).get(ndx); }
  boolean alreadyHasAsObject(HDT_Subj subj, HDT_Obj obj)  { return subjToObjList.containsEntry(subj, obj); }
  boolean alreadyHasAsSubject(HDT_Obj obj, HDT_Subj subj) { return objToSubjList.containsEntry(obj, subj); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Set<Set<RelationType>> cycleGroups;

  static
  {
    cycleGroups = ImmutableSet.<Set<RelationType>>builder()
      .add(EnumSet.of(rtParentDebateOfPos, rtParentPosOfDebate, rtParentDebateOfDebate, rtParentPosOfPos))
      .build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void initCycleGroup()
  {
    if (cycleGroup != null)
      cycleGroup.removeIf(relType -> relationSets.get(relType).subjType != objType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private RelationSet(RelationType newType) throws HDB_InternalError
  {
    boolean trackOrphans = false;
    type = newType;

    relationSets.put(type, this);
    cycleGroup = cycleGroups.stream().filter(cycleGroup -> cycleGroup.contains(type)).findAny().map(EnumSet::copyOf).orElse(null);

    switch (type)
    {
      case rtParentWorkOfWork         : hasNestedItems = false; subjType = hdtWork;          objType = hdtWork;            break;
      case rtParentGroupOfGroup       : hasNestedItems = false; subjType = hdtPersonGroup;   objType = hdtPersonGroup;

        trackOrphans = true; break;

      case rtParentLabelOfLabel       : hasNestedItems = false; subjType = hdtWorkLabel;     objType = hdtWorkLabel;

        trackOrphans = true; break;

      case rtCounterOfArgument        : hasNestedItems = true;  subjType = hdtArgument;      objType = hdtArgument;

        addNestedItem(hdcNestedPointer, tagArgumentVerdict, hdtArgumentVerdict);                   break;

      case rtParentDebateOfDebate     : hasNestedItems = false; subjType = hdtDebate;        objType = hdtDebate;

        trackOrphans = true; break;

      case rtParentNoteOfNote         : hasNestedItems = false; subjType = hdtNote;          objType = hdtNote;

        trackOrphans = true; break;

      case rtParentPosOfPos           : hasNestedItems = false; subjType = hdtPosition;      objType = hdtPosition;

        trackOrphans = true; break;

      case rtWorkOfArgument           : hasNestedItems = false; subjType = hdtArgument;      objType = hdtWork;            break;
      case rtParentDebateOfPos        : hasNestedItems = false; subjType = hdtPosition;      objType = hdtDebate;

        trackOrphans = true; break;

      case rtParentPosOfDebate        : hasNestedItems = false; subjType = hdtDebate;        objType = hdtPosition;

        trackOrphans = true; break;

      case rtPositionOfArgument       : hasNestedItems = true;  subjType = hdtArgument;      objType = hdtPosition;

        addNestedItem(hdcNestedPointer, tagPositionVerdict, hdtPositionVerdict);                   break;

      case rtAuthorOfWork             : hasNestedItems = true;  subjType = hdtWork;          objType = hdtPerson;

        addNestedItem(hdcTernary, tagInFileName);
        addNestedItem(hdcBoolean, tagEditor);
        addNestedItem(hdcBoolean, tagTranslator);                                                  break;

      case rtAuthorOfFile             : hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtPerson;          break;
      case rtStatusOfPerson           : hasNestedItems = false; subjType = hdtPerson;        objType = hdtPersonStatus;    break;
      case rtFieldOfPerson            : hasNestedItems = false; subjType = hdtPerson;        objType = hdtField;           break;
      case rtSubfieldOfPerson         : hasNestedItems = false; subjType = hdtPerson;        objType = hdtSubfield;        break;
      case rtFieldOfSubfield          : hasNestedItems = false; subjType = hdtSubfield;      objType = hdtField;           break;
      case rtRankOfPerson             : hasNestedItems = false; subjType = hdtPerson;        objType = hdtRank;            break;
      case rtPersonOfInv              : hasNestedItems = false; subjType = hdtInvestigation; objType = hdtPerson;          break;
      case rtInstOfPerson             : hasNestedItems = true;  subjType = hdtPerson;        objType = hdtInstitution;

        addNestedItem(hdcBoolean, tagPast);                                                        break;

      case rtTypeOfInst               : hasNestedItems = false; subjType = hdtInstitution;   objType = hdtInstitutionType; break;
      case rtParentInstOfInst         : hasNestedItems = false; subjType = hdtInstitution;   objType = hdtInstitution;     break;
      case rtCountryOfRegion          : hasNestedItems = false; subjType = hdtRegion;        objType = hdtCountry;         break;
      case rtRegionOfInst             : hasNestedItems = false; subjType = hdtInstitution;   objType = hdtRegion;          break;
      case rtCountryOfInst            : hasNestedItems = false; subjType = hdtInstitution;   objType = hdtCountry;         break;
      case rtTypeOfWork               : hasNestedItems = false; subjType = hdtWork;          objType = hdtWorkType;        break;
      case rtTypeOfFile               : hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtFileType;        break;
      case rtConceptOfTerm            : hasNestedItems = false; subjType = hdtTerm;          objType = hdtConcept;         break;
      case rtGlossaryOfConcept        : hasNestedItems = false; subjType = hdtConcept;       objType = hdtGlossary;        break;
      case rtParentGlossaryOfGlossary : hasNestedItems = false; subjType = hdtGlossary;      objType = hdtGlossary;

        trackOrphans = true; break;

      case rtLabelOfWork              : hasNestedItems = false; subjType = hdtWork;          objType = hdtWorkLabel;       break;
      case rtLabelOfFile              : hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtWorkLabel;       break;
      case rtWorkOfMiscFile           : hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtWork;            break;
      case rtWorkFileOfWork           : hasNestedItems = true;  subjType = hdtWork;          objType = hdtWorkFile;

        addNestedItem(hdcString, tagStartPageNum);
        addNestedItem(hdcString, tagEndPageNum);                                                   break;

      case rtFolderOfWorkFile         : hasNestedItems = false; subjType = hdtWorkFile;      objType = hdtFolder;          break;
      case rtFolderOfMiscFile         : hasNestedItems = false; subjType = hdtMiscFile;      objType = hdtFolder;          break;
      case rtParentFolderOfFolder     : hasNestedItems = false; subjType = hdtFolder;        objType = hdtFolder;          break;
      case rtFolderOfNote             : hasNestedItems = false; subjType = hdtNote;          objType = hdtFolder;          break;
      case rtPictureFolderOfPerson    : hasNestedItems = false; subjType = hdtPerson;        objType = hdtFolder;          break;

      default                         : hasNestedItems = false; subjType = hdtNone;          objType = hdtNone;

        messageDialog("Internal error #84723", mtError);                                           break;

    }

    typeMappings.put(subjType, objType, type);
    this.trackOrphans = trackOrphans;

    if (trackOrphans)
    {
      Set<RelationSet<? extends HDT_Record, ? extends HDT_Record>> relSets = orphanTypeToRelSets.computeIfAbsent(subjType, k -> new HashSet<>());

      relSets.add(this);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static RelationSet<HDT_Record, HDT_Record> createSet(RelationType relType) throws HDB_InternalError
  {
    return (relType == rtNone) || (relType == rtUnited) ? null : new RelationSet<>(relType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addOrphanToAll(HDT_Record orphan)
  {
    Set<RelationSet<? extends HDT_Record, ? extends HDT_Record>> relSets = orphanTypeToRelSets.get(orphan.getType());

    if (relSets != null)
      relSets.forEach(relSet-> relSet.addOrphan(orphan));
  }

  @SuppressWarnings("unchecked")
  private void addOrphan(HDT_Record orphan)
  {
    if ((subjToObjList.containsKey(orphan) == false) && (isUnstoredRecord(orphan.getID(), subjType) == false))
      orphans.add((HDT_Subj) orphan);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static RelationType getRelation(RecordType subjType, RecordType objType)
  {
    return nullSwitch(typeMappings.get(subjType, objType), rtNone);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EnumSet<RelationType> getRelationsForObjType(RecordType objType)
  {
    Collection<RelationType> relTypes = typeMappings.getColumn(objType);
    return collEmpty(relTypes) ? EnumSet.noneOf(RelationType.class) : EnumSet.copyOf(relTypes);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EnumSet<RelationType> getRelationsForSubjType(RecordType subjType)
  {
    Collection<RelationType> relTypes = typeMappings.getRow(subjType);
    return collEmpty(relTypes) ? EnumSet.noneOf(RelationType.class) : EnumSet.copyOf(relTypes);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings({ "unchecked" })
  public void saveNestedValuesToOfflineMap(HDT_Subj subj, HDT_Obj obj, Map<Tag, HDI_OfflineBase> tagToNestedItem, RecordState recordState)
  {
    Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>> items = objectGroups.get(subj, obj);
    if (items == null) return;

    items.forEach((tag, onlineItem) ->
    {
      HDI_OfflineBase offlineItem = null;
      HDI_Schema schema = getSchema(tag);

      switch (schema.getCategory())
      {
        case hdcString :        if (NestedValue.isEmpty(((HDI_OnlineString)onlineItem).get()) == false)
                                  offlineItem = new HDI_OfflineString(schema, recordState);
          break;

        case hdcBoolean :       if (NestedValue.isEmpty(((HDI_OnlineBoolean)onlineItem).get()) == false)
                                  offlineItem = new HDI_OfflineBoolean(schema, recordState);
          break;

        case hdcTernary :       if (NestedValue.isEmpty(((HDI_OnlineTernary)onlineItem).get()) == false)
                                  offlineItem = new HDI_OfflineTernary(schema, recordState);
          break;

        case hdcNestedPointer : if (HDT_Record.isEmpty(((HDI_OnlineNestedPointer)onlineItem).get()) == false)
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
      case hdcBoolean       : isEmpty = NestedValue.isEmpty(((HDI_OfflineBoolean      )value).get     ()); break;
      case hdcTernary       : isEmpty = NestedValue.isEmpty(((HDI_OfflineTernary      )value).get     ()); break;
      case hdcString        : isEmpty = NestedValue.isEmpty(((HDI_OfflineString       )value).get     ()); break;
      case hdcNestedPointer : isEmpty = NestedValue.isEmpty(((HDI_OfflineNestedPointer)value).getObjID()); break;
      default               : return;
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

  public boolean setNestedPointer(HDT_Subj subj, HDT_Obj obj, Tag tag, HDT_Record target)
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
    List<ObjectGroup> list = new ArrayList<>();
    Set<HDT_Obj> objSet = new LinkedHashSet<>();

    if (subjToObjList.containsKey(subj))
      objSet.addAll(subjToObjList.get(subj));

    Map<HDT_Obj, Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> objToObjItems = objectGroups.row(subj);

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

    list.sort(Comparator.comparing(og -> origList.indexOf(og.getPrimary())));

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private <HDI_Offline extends HDI_OfflineBase, HDI_Online extends HDI_OnlineBase<HDI_Offline>>
                HDI_Online getNestedItem(HDT_Subj subj, HDT_Obj obj, Tag tag, boolean noCreate)
  {
    Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>> items = objectGroups.get(subj, obj);

    if (items == null)
    {
      if (noCreate) return null;
      objectGroups.put(subj, obj, items = new LinkedHashMap<>());
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
      default               : return null;
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

  public HDT_Record getNestedPointer(HDT_Subj subj, HDT_Obj obj, Tag tag)
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
          default               : break;
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
      // Add the object to the object list if not already there
      if (objList.contains(obj)) return;

      if (cycleGroup != null)
        groupCycleCheck(subj, obj, obj);
      else if (obj.getType() == subj.getType())
        cycleCheck(subj, (HDT_Subj) obj, obj);

      if (ndx == -1) objList.add(obj);
      else           objList.add(ndx, obj);

      objToSubjList.put(obj, subj);

      orphans.remove(subj);

      Platform.runLater(() -> changeHandlers.forEach(handler -> handler.handle(subj, obj, true)));

      return;
    }

    // Remove the object from the object list if it was there
    if (objList.contains(obj))
    {
      if (ndx == -1) objList.remove(obj); // removes first occurrence
      else           objList.remove(ndx);

      if (objList.contains(obj) == false)
      {
        objToSubjList.remove(obj, subj);

        if (HDT_Record.isEmpty(subj) == false) // skip if record is in the process of being deleted
        {
          if (trackOrphans && objList.isEmpty() && (isUnstoredRecord(subj.getID(), subjType) == false))
            orphans.add(subj);

          if ((HDT_Record.isEmpty(subj) == false) && (HDT_Record.isEmpty(obj) == false))  // Only run change handlers if the record is not in the process of being deleted
            Platform.runLater(() -> changeHandlers.forEach(handler -> handler.handle(subj, obj, false)));
        }
      }
    }

    if (hasNestedItems)
      objectGroups.remove(subj, obj);

    if ((type == rtWorkFileOfWork) && (getSubjectCount(obj) == 0))
      if (obj.isExpired() == false) // The obj record may have just been deleted, and the pointers are still being resolved
        db.deleteRecord(obj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private void cycleCheck(HDT_Subj origSubj, HDT_Subj curSubj, HDT_Obj origObj) throws RelationCycleException
  {
    if (origSubj == curSubj)
      throw new RelationCycleException(origSubj, origObj);

    for (HDT_Obj nextObj : subjToObjList.get(curSubj))
      cycleCheck(origSubj, (HDT_Subj) nextObj, origObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private void groupCycleCheck(HDT_Record origSubj, HDT_Obj curSubj, HDT_Record origObj) throws RelationCycleException
  {
    if (origSubj == curSubj)
      throw new RelationCycleException(origSubj, origObj);

    for (RelationType relType : cycleGroup)
    {
      RelationSet<HDT_Obj, ? extends HDT_Record> relSet = (RelationSet<HDT_Obj, ? extends HDT_Record>) relationSets.get(relType);
      relSet.groupParentCycleCheck(origSubj, curSubj, origObj);
    }
  }

  private void groupParentCycleCheck(HDT_Record origSubj, HDT_Subj curSubj, HDT_Record origObj) throws RelationCycleException
  {
    for (HDT_Obj nextObj : subjToObjList.get(curSubj))
      groupCycleCheck(origSubj, nextObj, origObj);
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

  private <HDT_Key extends HDT_Record, HDT_Value extends HDT_Record> ArrayListMultimap<HDT_Key, HDT_Value> rebuildMultimap(ArrayListMultimap<HDT_Key, HDT_Value> oldMap) throws HDB_InternalError
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

    Iterator<Cell<HDT_Subj, HDT_Obj, Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>>> cellIt = objectGroups.cellSet().iterator();

    while (cellIt.hasNext())
    {
      Cell<HDT_Subj, HDT_Obj, Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> cell = cellIt.next();

      if      (HDT_Record.isEmptyThrowsException(cell.getRowKey()))    cellIt.remove();
      else if (HDT_Record.isEmptyThrowsException(cell.getColumnKey())) cellIt.remove();
      else
      {
        Iterator<Entry<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> targetIt = cell.getValue().entrySet().iterator();

        while (targetIt.hasNext())
        {
          HDI_OnlineBase<? extends HDI_OfflineBase> item = targetIt.next().getValue();

          if (item.getCategory() == hdcNestedPointer)
            if (HDT_Record.isEmptyThrowsException(((HDI_OnlineNestedPointer)item).get())) targetIt.remove();
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
        Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>> nestedItemMap = objectGroups.get(subj, obj);
        if (nestedItemMap != null)
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
  // Doing so could cause query favorites to get corrupted.

  public enum RelationType
  {
 // rtObjectOfSubject

    rtNone                    ( 1, ""                     , ""),

    rtParentWorkOfWork        ( 2, "Child Work(s)"        , "Work(s) under this parent work"),
    rtTypeOfWork              ( 3, tagWork                , "Work(s) of this type"),
    rtTypeOfFile              ( 4, tagMiscFile            , "File(s) of this type"),
    rtAuthorOfWork            ( 5, tagWork                , "Work(s) by this author"),
    rtAuthorOfFile            ( 6, tagMiscFile            , "Misc. File(s) by this author"),
    rtWorkOfArgument          ( 7, tagArgument            , "Argument(s) having this work as source"),

    rtParentLabelOfLabel      (11, "Child Label(s)"       , "Label(s) under this parent label"),
    rtLabelOfWork             (12, tagWork                , "Work(s) having this label"),
    rtLabelOfFile             (13, tagMiscFile            , "Misc. File(s) having this label"),
    rtCounterOfArgument       (14, "Countered Argument(s)", "Argument(s) countered by this argument"),
    rtStatusOfPerson          (15, tagPerson              , "Person(s) having this status"),
    rtFieldOfPerson           (16, tagPerson              , "Person(s) having this field"),
    rtSubfieldOfPerson        (17, tagPerson              , "Person(s) having this subfield"),
    rtFieldOfSubfield         (18, tagSubfield            , "Subfield(s) under this field"),
    rtRankOfPerson            (19, tagPerson              , "Person(s) having this rank"),
    rtParentDebateOfDebate    (20, "Sub-Debate(s)"        , "Debate(s) under this larger debate"),
    rtParentNoteOfNote        (21, "Sub-Note(s)"          , "Note(s) under this parent note"),
    rtFolderOfNote            (22, tagNote                , "Note(s) associated with this Folder"),
    rtParentDebateOfPos       (23, tagPosition            , "Position(s) under this debate"),
    rtParentPosOfPos          (24, "Sub-Position(s)"      , "Position(s) under this parent position"),
    rtPositionOfArgument      (25, tagArgument            , "Argument(s) concerning this position"),
    rtParentPosOfDebate       (46, tagDebate              , "Debate(s) under this position"),

    rtPersonOfInv             (27, tagInvestigation       , "Investigation(s) by this person"),
    rtPictureFolderOfPerson   (28, tagPerson              , "Person(s) with pictures in this folder"),
    rtCountryOfRegion         (29, tagRegion              , "States/regions in this country"),
    rtRegionOfInst            (30, tagInstitution         , "Institution(s) in this state/region"),
    rtCountryOfInst           (31, tagInstitution         , "Institution(s) in this country"),
    rtParentGroupOfGroup      (32, "Sub-Group(s)"         , "Group(s) under this parent group"),
    rtWorkOfMiscFile          (33, tagMiscFile            , "Misc. files of this work"),
    rtWorkFileOfWork          (34, tagWork                , "Work(s) having this work file"),
    rtFolderOfWorkFile        (35, tagWorkFile            , "Work file(s) in this folder"),
    rtFolderOfMiscFile        (36, tagMiscFile            , "Misc. file(s) in this folder"),
    rtParentFolderOfFolder    (37, "Subfolder(s)"         , "Subfolder(s) of this folder"),
    rtUnited                  (38, ""                     , ""),

    rtTypeOfInst              (39, tagInstitution         , "Institution(s) of this type"),
    rtParentInstOfInst        (40, "Division(s)"          , "Division(s) of this institution"),
    rtInstOfPerson            (41, tagPerson              , "Person(s) in this institution"),
    rtGlossaryOfConcept       (43, tagConcept             , "Concept(s) in this glossary"),
    rtParentGlossaryOfGlossary(44, "Sub-Glossary(ies)"    , "Sub-glossaries under this glossary"),
    rtConceptOfTerm           (45, tagTerm                , "Term(s) associated with this concept");

    private final int code;
    private final String title, subjTitle;
    private final Tag subjTag;
    private final static Map<Integer, RelationType> codeToVal;

  //---------------------------------------------------------------------------

    static
    {
      codeToVal = new HashMap<>();
      EnumSet.allOf(RelationType.class).forEach(val -> codeToVal.put(val.getCode(), val));
    }

  //---------------------------------------------------------------------------

    RelationType(int code, Tag subjTag, String title)
    {
      this.code = code;
      this.subjTag = subjTag;
      this.subjTitle = "";
      this.title = title;
    }

  //---------------------------------------------------------------------------

    RelationType(int code, String subjTitle, String title)
    {
      this.code = code;
      this.subjTag = tagNone;
      this.subjTitle = subjTitle;
      this.title = title;
    }

  //---------------------------------------------------------------------------

    public static RelationType codeToVal(int num) { return codeToVal.get(num); }
    public int getCode()                          { return code; }
    public String getTitle()                      { return title; }
    public Tag getSubjTag()                       { return subjTag; }
    public String getSubjTitle()                  { return subjTag == tagNone ? subjTitle : db.getTagHeader(subjTag); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addNestedItem(HyperDataCategory dataCat, Tag tag) throws HDB_InternalError
  {
    addNestedItem(dataCat, tag, hdtNone);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addNestedItem(HyperDataCategory dataCat, Tag tag, RecordType targetType) throws HDB_InternalError
  {
    if (tagToSchema.containsKey(tag))
      throw new HDB_InternalError(98925);

    tagToSchema.put(tag, new HDI_Schema(dataCat, type, tag));

    if (dataCat == hdcNestedPointer)
      tagToTargetType.put(tag, targetType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reorderObjects (HDT_Subj subj, List<HDT_Obj>  newObjList)  { reorderList(subj, newObjList,  subjToObjList); }
  void reorderSubjects(HDT_Obj   obj, List<HDT_Subj> newSubjList) { reorderList(obj,  newSubjList, objToSubjList); }

  private <HDT_Key extends HDT_Record, HDT_Value extends HDT_Record> void reorderList(HDT_Key key, List<HDT_Value> newValueList, ArrayListMultimap<HDT_Key, HDT_Value> map)
  {
    if (key == null) throw new NullPointerException();

    if (map.containsKey(key) == false) return;

    List<HDT_Value> existingValueList = map.get(key);

    if (existingValueList.size() != newValueList.size()) return;

    if ((existingValueList.containsAll(newValueList     ) == false) ||
        (newValueList     .containsAll(existingValueList) == false))
      return;

    for (int ndx = 0; ndx < existingValueList.size(); ndx++)
      existingValueList.set(ndx, newValueList.get(ndx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
