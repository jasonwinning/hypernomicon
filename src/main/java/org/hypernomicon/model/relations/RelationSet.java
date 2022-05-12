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
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText;
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
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

//---------------------------------------------------------------------------

public final class RelationSet<HDT_Subj extends HDT_Record, HDT_Obj extends HDT_Record>
{
  private final Set<HDT_Subj> orphans = new HashSet<>();
  private ArrayListMultimap<HDT_Obj, HDT_Subj> objToSubjList = ArrayListMultimap.create();
  private ArrayListMultimap<HDT_Subj, HDT_Obj> subjToObjList = ArrayListMultimap.create();
  private final HashBasedTable<HDT_Subj, HDT_Obj, Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> objectGroups = HashBasedTable.create();
  private final Map<Tag, HDI_Schema> tagToSchema = new LinkedHashMap<>();
  private final List<RelationChangeHandler> changeHandlers = new ArrayList<>();
  private final Set<RelationType> cycleGroup;

  private static final EnumMap<RecordType, Set<RelationSet<? extends HDT_Record, ? extends HDT_Record>>> orphanTypeToRelSets = new EnumMap<>(RecordType.class);
  private static final EnumMap<RelationType, RelationSet<? extends HDT_Record, ? extends HDT_Record>> relationSets = new EnumMap<>(RelationType.class);
  private static final EnumBasedTable<RecordType, RecordType, RelationType> typeMappings            = new EnumBasedTable<>(RecordType.class, RecordType.class),
                                                                            typeMappingsWithKeyWork = new EnumBasedTable<>(RecordType.class, RecordType.class);

  private final RelationType type;
  private final RecordType objType, subjType;
  private final boolean hasNestedItems, trackOrphans;

  public RecordType getObjType()                          { return objType; }
  public RecordType getSubjType()                         { return subjType; }
  public HDI_Schema getSchema(Tag tag)                    { return tagToSchema.get(tag); }
  public Collection<HDI_Schema> getSchemas()              { return tagToSchema.values(); }
  public boolean getHasNestedItems()                      { return hasNestedItems; }
  public Set<Tag> getNestedTags()                         { return EnumSet.copyOf(tagToSchema.keySet()); }
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

  private RelationSet(RelationType newType, Class<HDT_Subj> subjClass, Class<HDT_Obj> objClass) throws HDB_InternalError
  {
    this(newType, subjClass, objClass, false);
  }

  private RelationSet(RelationType newType, Class<HDT_Subj> subjClass, Class<HDT_Obj> objClass, HDI_Schema... nestedSchemas) throws HDB_InternalError
  {
    this(newType, subjClass, objClass, false, nestedSchemas);
  }

  private RelationSet(RelationType newType, Class<HDT_Subj> subjClass, Class<HDT_Obj> objClass, boolean trackOrphans, HDI_Schema... nestedSchemas) throws HDB_InternalError
  {
    type = newType;

    cycleGroup = cycleGroups.stream().filter(cGroup -> cGroup.contains(type)).findAny().map(EnumSet::copyOf).orElse(null);

    this.subjType = typeByRecordClass(subjClass);
    this.objType = typeByRecordClass(objClass);
    this.hasNestedItems = (nestedSchemas.length > 0);
    this.trackOrphans = trackOrphans;

    typeMappings           .put(subjType, objType, type);
    typeMappingsWithKeyWork.put(subjType, objType, type);

    if (trackOrphans)
      orphanTypeToRelSets.computeIfAbsent(subjType, k -> new HashSet<>()).add(this);

    for (HDI_Schema nestedSchema : nestedSchemas)
    {
      Tag tag = nestedSchema.getTags().get(0);

      if (tagToSchema.containsKey(tag))
        throw new HDB_InternalError(98925);

      tagToSchema.put(tag, nestedSchema);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static RelationSet<? extends HDT_Record, ? extends HDT_Record> createSet(RelationType relType) throws HDB_InternalError
  {
    switch (relType)
    {
      case rtParentWorkOfWork         : return new RelationSet<>(relType, HDT_Work         .class, HDT_Work           .class);
      case rtParentGroupOfGroup       : return new RelationSet<>(relType, HDT_PersonGroup  .class, HDT_PersonGroup    .class, true );
      case rtParentLabelOfLabel       : return new RelationSet<>(relType, HDT_WorkLabel    .class, HDT_WorkLabel      .class, true );
      case rtCounterOfArgument        : return new RelationSet<>(relType, HDT_Argument     .class, HDT_Argument       .class,

        new HDI_Schema(hdcNestedPointer, relType, hdtArgumentVerdict, tagArgumentVerdict));

      case rtParentDebateOfDebate     : return new RelationSet<>(relType, HDT_Debate       .class, HDT_Debate         .class, true );
      case rtParentNoteOfNote         : return new RelationSet<>(relType, HDT_Note         .class, HDT_Note           .class, true );
      case rtParentPosOfPos           : return new RelationSet<>(relType, HDT_Position     .class, HDT_Position       .class, true );
      case rtWorkOfArgument           : return new RelationSet<>(relType, HDT_Argument     .class, HDT_Work           .class);
      case rtParentDebateOfPos        : return new RelationSet<>(relType, HDT_Position     .class, HDT_Debate         .class, true );
      case rtParentPosOfDebate        : return new RelationSet<>(relType, HDT_Debate       .class, HDT_Position       .class, true );
      case rtPositionOfArgument       : return new RelationSet<>(relType, HDT_Argument     .class, HDT_Position       .class,

        new HDI_Schema(hdcNestedPointer, relType, hdtPositionVerdict, tagPositionVerdict));

      case rtAuthorOfWork             : return new RelationSet<>(relType, HDT_Work         .class, HDT_Person         .class,

        new HDI_Schema(hdcTernary, relType, tagInFileName),
        new HDI_Schema(hdcBoolean, relType, tagEditor),
        new HDI_Schema(hdcBoolean, relType, tagTranslator));

      case rtAuthorOfFile             : return new RelationSet<>(relType, HDT_MiscFile     .class, HDT_Person         .class);
      case rtStatusOfPerson           : return new RelationSet<>(relType, HDT_Person       .class, HDT_PersonStatus   .class);
      case rtFieldOfPerson            : return new RelationSet<>(relType, HDT_Person       .class, HDT_Field          .class);
      case rtSubfieldOfPerson         : return new RelationSet<>(relType, HDT_Person       .class, HDT_Subfield       .class);
      case rtFieldOfSubfield          : return new RelationSet<>(relType, HDT_Subfield     .class, HDT_Field          .class);
      case rtRankOfPerson             : return new RelationSet<>(relType, HDT_Person       .class, HDT_Rank           .class);
      case rtPersonOfInv              : return new RelationSet<>(relType, HDT_Investigation.class, HDT_Person         .class);
      case rtInstOfPerson             : return new RelationSet<>(relType, HDT_Person       .class, HDT_Institution    .class,

        new HDI_Schema(hdcBoolean, relType, tagPast));

      case rtTypeOfInst               : return new RelationSet<>(relType, HDT_Institution  .class, HDT_InstitutionType.class);
      case rtParentInstOfInst         : return new RelationSet<>(relType, HDT_Institution  .class, HDT_Institution    .class);
      case rtCountryOfRegion          : return new RelationSet<>(relType, HDT_Region       .class, HDT_Country        .class);
      case rtRegionOfInst             : return new RelationSet<>(relType, HDT_Institution  .class, HDT_Region         .class);
      case rtCountryOfInst            : return new RelationSet<>(relType, HDT_Institution  .class, HDT_Country        .class);
      case rtTypeOfWork               : return new RelationSet<>(relType, HDT_Work         .class, HDT_WorkType       .class);
      case rtTypeOfFile               : return new RelationSet<>(relType, HDT_MiscFile     .class, HDT_FileType       .class);
      case rtConceptOfTerm            : return new RelationSet<>(relType, HDT_Term         .class, HDT_Concept        .class);
      case rtGlossaryOfConcept        : return new RelationSet<>(relType, HDT_Concept      .class, HDT_Glossary       .class);
      case rtParentGlossaryOfGlossary : return new RelationSet<>(relType, HDT_Glossary     .class, HDT_Glossary       .class, true);
      case rtParentConceptOfConcept   : return new RelationSet<>(relType, HDT_Concept      .class, HDT_Concept        .class);
      case rtSenseOfConcept           : return new RelationSet<>(relType, HDT_Concept      .class, HDT_ConceptSense   .class);
      case rtWorkOfMiscFile           : return new RelationSet<>(relType, HDT_MiscFile     .class, HDT_Work           .class);
      case rtWorkFileOfWork           : return new RelationSet<>(relType, HDT_Work         .class, HDT_WorkFile       .class,

        new HDI_Schema(hdcString, relType, tagStartPageNum),
        new HDI_Schema(hdcString, relType, tagEndPageNum));

      case rtFolderOfWorkFile         : return new RelationSet<>(relType, HDT_WorkFile     .class, HDT_Folder         .class);
      case rtFolderOfMiscFile         : return new RelationSet<>(relType, HDT_MiscFile     .class, HDT_Folder         .class);
      case rtParentFolderOfFolder     : return new RelationSet<>(relType, HDT_Folder       .class, HDT_Folder         .class);
      case rtFolderOfNote             : return new RelationSet<>(relType, HDT_Note         .class, HDT_Folder         .class);
      case rtPictureFolderOfPerson    : return new RelationSet<>(relType, HDT_Person       .class, HDT_Folder         .class);

      default                         : throw new HDB_InternalError(84723);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void init(Map<RelationType, RelationSet<? extends HDT_Record, ? extends HDT_Record>> dbRelationSets) throws HDB_InternalError
  {
    for (RelationType relType : RelationType.values())
      if ((relType != rtUnited) && (relType != rtKeyWork) && (relType != rtNone))
        dbRelationSets.put(relType, createSet(relType));

    relationSets.putAll(dbRelationSets);
    relationSets.values().forEach(RelationSet::initCycleGroup);

    for (RecordType objType : RecordType.values())
    {
      Class<? extends HDT_Record> recordClass = objType.getRecordClass();
      if (HDT_RecordWithMainText.class.isAssignableFrom(recordClass) && (objType != hdtHub) && MainText.typeHasKeyWorks(objType))
      {
        if (typeMappingsWithKeyWork.get(hdtWork, objType) == null)
          typeMappingsWithKeyWork.put(hdtWork, objType, rtKeyWork);
        if (typeMappingsWithKeyWork.get(hdtMiscFile, objType) == null)
          typeMappingsWithKeyWork.put(hdtMiscFile, objType, rtKeyWork);
      }
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private void initCycleGroup()
    {
      if (cycleGroup != null)
        cycleGroup.removeIf(relType -> relationSets.get(relType).subjType != objType);
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

  public static RelationType getRelation(RecordType subjType, RecordType objType, boolean includeKeyWork)
  {
    return nullSwitch((includeKeyWork ? typeMappingsWithKeyWork : typeMappings).get(subjType, objType), rtNone);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EnumSet<RelationType> getRelationsForObjType(RecordType objType, boolean includeKeyWork)
  {
    Collection<RelationType> relTypes = (includeKeyWork ? typeMappingsWithKeyWork : typeMappings).getColumn(objType);
    return collEmpty(relTypes) ? EnumSet.noneOf(RelationType.class) : EnumSet.copyOf(relTypes);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EnumSet<RelationType> getRelationsForSubjType(RecordType subjType, boolean includeKeyWork)
  {
    Collection<RelationType> relTypes = (includeKeyWork ? typeMappingsWithKeyWork : typeMappings).getRow(subjType);
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

      ((HDI_OnlineBase<HDI_OfflineBase>) onlineItem).getToOfflineValue(offlineItem, tag);
      tagToNestedItem.put(tag, offlineItem);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public <HDI_Offline extends HDI_OfflineBase> void setNestedItemFromOfflineValue(HDT_Subj subj, HDT_Obj obj, Tag tag, HDI_Offline value) throws RelationCycleException, HDB_InternalError
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
    Set<HDT_Obj> objSet = new LinkedHashSet<>();

    if (subjToObjList.containsKey(subj))
      objSet.addAll(subjToObjList.get(subj));

    Map<HDT_Obj, Map<Tag, HDI_OnlineBase<? extends HDI_OfflineBase>>> objToObjItems = objectGroups.row(subj);
    List<ObjectGroup> list = new ArrayList<>();

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

  void setObject(HDT_Subj subj, HDT_Obj obj, int ndx, boolean affirm) throws RelationCycleException
  {
    setObject(subj, obj, ndx, -1, affirm);
  }

  @SuppressWarnings("unchecked")
  void setObject(HDT_Subj subj, HDT_Obj obj, int ndx, int subjOrd, boolean affirm) throws RelationCycleException
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

      if (subjOrd > -1)
        initOrderedSubject(obj, subj, subjOrd);
      else
        objToSubjList.put(obj, subj);

      orphans.remove(subj);

      Platform.runLater(() -> changeHandlers.forEach(handler -> handler.handle(subj, obj, true)));

      return;
    }

    // Remove the object from the object list if it was there
    if (objList.contains(obj))
    {
      if (ndx == -1) objList.remove(obj);
      else           objList.remove(ndx);

      if (objList.contains(obj) == false)
      {
        objToSubjList.remove(obj, subj);
        subjOrdMap.remove(subj); // This map is only used for ordered pointer-single items so the subject will no longer have any objects

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

    if (((type == rtSenseOfConcept) || (type == rtWorkFileOfWork)) && (getSubjectCount(obj) == 0))
      if (obj.isExpired() == false) // The obj record may have just been deleted, and the pointers are still being resolved
        db.deleteRecord(obj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Map<HDT_Subj, Integer> subjOrdMap = new HashMap<>();

  // This should only get called while database is first loading; that is the only time subject order values are valid.

  private void initOrderedSubject(HDT_Obj obj, HDT_Subj subj, int subjOrd)
  {
    subjOrdMap.put(subj, subjOrd);
    addToSortedList(objToSubjList.get(obj), subj, Comparator.comparing(subjOrdMap::get));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int getSubjectOrd(HDT_Obj obj, HDT_Subj subj)
  {
    return subjOrdMap.containsKey(subj) ? (getSubjectNdx(obj, subj) + 1) : -1;
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

  private static <HDT_Key extends HDT_Record, HDT_Value extends HDT_Record> ArrayListMultimap<HDT_Key, HDT_Value> rebuildMultimap(ArrayListMultimap<HDT_Key, HDT_Value> oldMap) throws HDB_InternalError
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

    Iterator<Entry<HDT_Subj, Integer>> subjOrdMapIt = subjOrdMap.entrySet().iterator();
    while (subjOrdMapIt.hasNext())
    {
      if (HDT_Record.isEmptyThrowsException(subjOrdMapIt.next().getKey())) subjOrdMapIt.remove();
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
    rtConceptOfTerm           (45, tagTerm                , "Term(s) associated with this concept"),
    rtParentPosOfDebate       (46, tagDebate              , "Debate(s) under this position"),

    rtKeyWork                 (47, ""                     , ""), // Like rtUnited, this is not a real relation type having its own RelationSet object.

    rtParentConceptOfConcept  (48, "Child Concept(s)"     , "Child concept(s) under this concept"),
    rtSenseOfConcept          (49, tagConcept             , "Concept(s) having this kind of sense");

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
    public String getSubjTitle()                  { return subjTag == tagNone ? subjTitle : subjTag.header; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reorderObjects (HDT_Subj subj, List<HDT_Obj>  newObjList) { reorderList(subj, newObjList, subjToObjList); }

  void reorderSubjects(HDT_Obj obj, List<HDT_Subj> newSubjList)
  {
    if (reorderList(obj, newSubjList, objToSubjList) == false)
      return;

    subjOrdMap.clear();
    for (int ndx = 0; ndx < newSubjList.size(); ndx++)
      subjOrdMap.put(newSubjList.get(ndx), ndx + 1);
  }

  // Returns true if changed

  private static <HDT_Key extends HDT_Record, HDT_Value extends HDT_Record> boolean reorderList(HDT_Key key, List<HDT_Value> newValueList, ArrayListMultimap<HDT_Key, HDT_Value> map)
  {
    if (key == null) throw new NullPointerException();

    if (map.containsKey(key) == false) return false;

    List<HDT_Value> existingValueList = map.get(key);

    if (existingValueList.size() != newValueList.size()) return false;

    if ((existingValueList.containsAll(newValueList     ) == false) ||
        (newValueList     .containsAll(existingValueList) == false))
      return false;

    boolean changed = false;

    for (int ndx = 0; ndx < existingValueList.size(); ndx++)
    {
      HDT_Value newValue = newValueList.get(ndx);
      if (newValue != existingValueList.set(ndx, newValue))
        changed = true;
    }

    return changed;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
