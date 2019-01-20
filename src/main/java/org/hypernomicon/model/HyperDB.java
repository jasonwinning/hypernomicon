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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_Record.HyperDataCategory.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.relations.RelationSet.*;

import static java.util.Objects.*;
import static java.util.Collections.*;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.security.InvalidAlgorithmParameterException;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.prefs.BackingStoreException;
import java.util.prefs.InvalidPreferencesFormatException;
import java.util.prefs.Preferences;

import javax.crypto.BadPaddingException;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.events.Attribute;
import javax.xml.stream.events.Characters;
import javax.xml.stream.events.StartElement;
import javax.xml.stream.events.XMLEvent;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.json.simple.parser.ParseException;

import com.google.common.collect.EnumBiMap;
import com.google.common.collect.EnumHashBiMap;
import com.google.common.collect.Sets;

import org.hypernomicon.FolderTreeWatcher;
import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.lib.BibCollection;
import org.hypernomicon.bib.lib.BibEntry;
import org.hypernomicon.bib.lib.LibraryWrapper;
import org.hypernomicon.bib.lib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_Record.HyperDataCategory;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.*;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.util.BidiOneToManyMainTextMap;
import org.hypernomicon.util.CryptoUtil;
import org.hypernomicon.util.FilenameMap;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperFavorites;

//---------------------------------------------------------------------------

public final class HyperDB
{
  public static final HyperDB db = new HyperDB();

  final private EnumMap<HDT_RecordType, HyperDataset<? extends HDT_Base>> datasets = new EnumMap<>(HDT_RecordType.class);
  final private EnumMap<HDT_RecordType, HyperDataset<? extends HDT_Base>.CoreAccessor> accessors = new EnumMap<>(HDT_RecordType.class);
  final private EnumMap<RelationType, RelationSet<? extends HDT_Base, ? extends HDT_Base>> relationSets = new EnumMap<>(RelationType.class);
  final private EnumMap<RelationType, Boolean> relTypeToIsMulti = new EnumMap<>(RelationType.class);
  final private EnumMap<Tag, HDT_RecordType> tagToObjType = new EnumMap<>(Tag.class);
  final private EnumMap<Tag, EnumSet<HDT_RecordType>> tagToSubjType = new EnumMap<>(Tag.class);
  final private EnumMap<Tag, String> tagToHeader = new EnumMap<>(Tag.class);

  final private EnumBiMap<HDT_RecordType, Tag> typeToTag = EnumBiMap.create(HDT_RecordType.class, Tag.class);
  final private EnumHashBiMap<Tag, String> tagToStr = EnumHashBiMap.create(Tag.class);
  final private EnumHashBiMap<HDT_RecordType, String> typeToTagStr = EnumHashBiMap.create(HDT_RecordType.class);
  final private SearchKeys searchKeys = new SearchKeys();
  final private ArrayList<RecordDeleteHandler> recordDeleteHandlers          = new ArrayList<>();
  final private ArrayList<DatabaseEvent>       dbCloseHandlers               = new ArrayList<>(),
                                               dbPreChangeHandlers           = new ArrayList<>(),
                                               dbMentionsNdxCompleteHandlers = new ArrayList<>(),
                                               bibChangedHandlers            = new ArrayList<>();
  final private MentionsIndex mentionsIndex = new MentionsIndex(dbMentionsNdxCompleteHandlers);
  final private ArrayList<HDT_Base> initialNavList = new ArrayList<>();
  final private EnumMap<HDT_RecordType, RelationChangeHandler> keyWorkHandlers = new EnumMap<>(HDT_RecordType.class);
  public Preferences prefs;
  private Preferences appPrefs;
  private LibraryWrapper<? extends BibEntry, ? extends BibCollection> bibLibrary = null;
  private FolderTreeWatcher folderTreeWatcher;
  private FilePath lockFilePath = null;
  public FilenameMap<Set<HyperPath>> filenameMap;
  private HashMap<HDT_RecordWithPath, Set<HDT_RecordWithConnector>> keyWorkIndex;
  private BidiOneToManyMainTextMap displayedAtIndex;
  private Map<String, HDT_Work> bibEntryKeyToWork;
  private DialogResult deleteFileAnswer;
  HyperTask task;
  long totalTaskCount, curTaskCount;
  private FilePath rootFilePath, prefsFilePath;
  private Instant dbCreationDate;

  private boolean loaded = false, deletionInProgress = false, pointerResolutionInProgress = false, resolveAgain = false,
                  unableToLoad = false, initialized = false;

  public boolean runningConversion = false,     // suppresses "modified date" updating
                 viewTestingInProgress = false; // suppresses "view date" updating

//---------------------------------------------------------------------------
  @FunctionalInterface public interface RelationChangeHandler { public abstract void handle(HDT_Base subject, HDT_Base object, boolean affirm); }
  @FunctionalInterface public interface RecordDeleteHandler   { public abstract void handle(HDT_Base record); }
  @FunctionalInterface public interface RecordAddHandler      { public abstract void handle(HDT_Base record); }
  @FunctionalInterface public interface DatabaseEvent         { public abstract void handle(); }
//---------------------------------------------------------------------------

  public boolean isDeletionInProgress()                       { return deletionInProgress; }
  public boolean resolvingPointers()                          { return pointerResolutionInProgress; }
  public FilePath getRootFilePath()                           { return rootFilePath; }
  public int getNextID(HDT_RecordType type)                   { return datasets.get(type).getNextID(); }
  public boolean idAvailable(HDT_RecordType type, int id)     { return datasets.get(type).idAvailable(id); }
  public String getTypeTagStr(HDT_RecordType type)            { return typeToTagStr.get(type); }
  public HDT_RecordType parseTypeTagStr(String tag)           { return typeToTagStr.inverse().getOrDefault(tag, hdtNone); }
  public boolean isLoaded()                                   { return loaded; }
  public boolean bibLibraryIsLinked()                         { return nonNull(bibLibrary); }
  public Instant getCreationDate()                            { return dbCreationDate; }
  public HDT_RecordType getSubjType(RelationType relType)     { return relationSets.get(relType).getSubjType(); }
  public HDT_RecordType getObjType(RelationType relType)      { return relationSets.get(relType).getObjType(); }
  public boolean relationIsMulti(RelationType relType)        { return relTypeToIsMulti.get(relType).booleanValue(); }
  public String getTagStr(Tag tag)                            { return tagToStr.get(tag); }
  public String getTagHeader(Tag tag)                         { return tagToHeader.getOrDefault(tag, ""); }
  public Tag getMainTextTag(HDT_RecordType type)              { return datasets.get(type).getMainTextTag(); }
  public List<HDT_Base> getInitialNavList()                   { return unmodifiableList(initialNavList); }
  public String getSearchKey(HDT_Base record)                 { return searchKeys.getStringForRecord(record); }
  public SearchKeyword getKeyByKeyword(String keyword)        { return searchKeys.getKeywordObjByKeywordStr(keyword); }
  public String getFirstActiveKeyWord(HDT_Base record)        { return searchKeys.getFirstActiveKeyword(record); }
  public List<SearchKeyword> getKeysByPrefix(String prefix)   { return searchKeys.getKeywordsByPrefix(prefix); }
  public List<SearchKeyword> getKeysByRecord(HDT_Base record) { return searchKeys.getKeysByRecord(record); }
  public HDT_Work getWorkByBibEntryKey(String key)            { return bibEntryKeyToWork.get(key); }
  public boolean reindexingMentioners()                       { return mentionsIndex.isRebuilding(); }
  public BibEntry getBibEntryByKey(String key)                { return bibLibrary.getEntryByKey(key); }

  public void setSearchKey(HDT_Base record, String newKey, boolean noMod) throws SearchKeyException { searchKeys.setSearchKey(record, newKey, noMod); }

  public LibraryWrapper<? extends BibEntry, ? extends BibCollection> getBibLibrary()        { return bibLibrary; }
  public List<RecordDeleteHandler> getRecordDeleteHandlers()                                { return unmodifiableList(recordDeleteHandlers); }
  public void addRelationChangeHandler(RelationType relType, RelationChangeHandler handler) { relationSets.get(relType).addChangeHandler(handler); }
  public void addKeyWorkHandler(HDT_RecordType recordType, RelationChangeHandler handler)   { keyWorkHandlers.put(recordType, handler); }
  public void addCloseDBHandler(DatabaseEvent handler)                                      { dbCloseHandlers.add(handler); }
  public void addPreDBChangeHandler(DatabaseEvent handler)                                  { dbPreChangeHandlers.add(handler); }
  public void addMentionsNdxCompleteHandler(DatabaseEvent handler)                          { dbMentionsNdxCompleteHandlers.add(handler); }
  public void addBibChangedHandler(DatabaseEvent handler)                                   { bibChangedHandlers.add(handler); }
  public void addDeleteHandler(RecordDeleteHandler handler)                                 { recordDeleteHandlers.add(handler); }
  public void rebuildMentions()                                                             { if (loaded) mentionsIndex.startRebuild(); }
  public void stopIndexingMentions()                                                        { mentionsIndex.stopRebuild(); }
  public void updateMentioner(HDT_Base record)                                              { if (loaded) mentionsIndex.updateMentioner(record); }
  public boolean waitUntilRebuildIsDone()                                                   { return mentionsIndex.waitUntilRebuildIsDone(); }

  public boolean firstMentionsSecond(HDT_Base mentioner, HDT_Base target, boolean descOnly, MutableBoolean choseNotToWait) {
    return mentionsIndex.firstMentionsSecond(mentioner, target, descOnly, choseNotToWait); }
  public Set<HDT_Base> getMentionerSet(HDT_Base target, boolean descOnly, MutableBoolean choseNotToWait) {
    return mentionsIndex.getMentionerSet(target, descOnly, choseNotToWait); }
  public Set<HDT_Base> getMentionedSet(HDT_Base mentioner, boolean descOnly, MutableBoolean choseNotToWait) {
    return mentionsIndex.getMentionedSet(mentioner, descOnly, choseNotToWait); }

  public boolean firstMentionsSecond(HDT_Base mentioner, HDT_Base target, boolean descOnly) {
    return mentionsIndex.firstMentionsSecond(mentioner, target, descOnly); }
  public Set<HDT_Base> getMentionerSet(HDT_Base target, boolean descOnly) {
    return mentionsIndex.getMentionerSet(target, descOnly); }
  public Set<HDT_Base> getMentionedSet(HDT_Base mentioner, boolean descOnly) {
    return mentionsIndex.getMentionedSet(mentioner, descOnly); }

//---------------------------------------------------------------------------

  public String getNestedString(HDT_Base subj, HDT_Base obj, Tag tag)    { return getRelSet(subj, obj).getNestedString(subj, obj, tag);  }
  public boolean getNestedBoolean(HDT_Base subj, HDT_Base obj, Tag tag)  { return getRelSet(subj, obj).getNestedBoolean(subj, obj, tag); }
  public Ternary getNestedTernary(HDT_Base subj, HDT_Base obj, Tag tag)  { return getRelSet(subj, obj).getNestedTernary(subj, obj, tag); }
  public HDT_Base getNestedPointer(HDT_Base subj, HDT_Base obj, Tag tag) { return getRelSet(subj, obj).getNestedPointer(subj, obj, tag); }
  public boolean relationHasNestedValues(RelationType relType)           { return relationSets.get(relType).getHasNestedItems(); }
  public HDI_Schema getNestedSchema(RelationType relType, Tag tag)       { return relationSets.get(relType).getSchema(tag); }
  public Set<Tag> getNestedTags(RelationType relType)                    { return relationSets.get(relType).getNestedTags(); }

  @SuppressWarnings("unchecked")
  private <HDT_SubjType extends HDT_Base, HDT_ObjType extends HDT_Base> RelationSet<HDT_SubjType, HDT_ObjType> getRelSet(HDT_SubjType subj, HDT_ObjType obj)
  { return (RelationSet<HDT_SubjType, HDT_ObjType>) relationSets.get(getRelation(subj.getType(), obj.getType())); }

  @SuppressWarnings("unchecked")
  private <HDT_SubjType extends HDT_Base, HDT_ObjType extends HDT_Base> RelationSet<HDT_SubjType, HDT_ObjType> getRelSet(RelationType relType)
  { return (RelationSet<HDT_SubjType, HDT_ObjType>) relationSets.get(relType); }

  public void setNestedItemFromOfflineValue(HDT_Base subj, HDT_Base obj, Tag tag, HDI_OfflineBase value) throws RelationCycleException
  { getRelSet(subj, obj).setNestedItemFromOfflineValue(subj, obj, tag, value); }

  public void saveNestedValuesToOfflineMap(HDT_Base subj, HDT_Base obj, Map<Tag, HDI_OfflineBase> tagToNestedItem, HDT_RecordState recordState)
  { getRelSet(subj, obj).saveNestedValuesToOfflineMap(subj, obj, tagToNestedItem, recordState); }

  public <HDT_ObjType extends HDT_Base, HDT_SubjType extends HDT_Base> HyperObjList<HDT_SubjType, HDT_ObjType> getObjectList(RelationType relType, HDT_SubjType subj, boolean modTracking)
  { return new HyperObjList<>(getRelSet(relType), subj, modTracking); }

  public <HDT_ObjType extends HDT_Base, HDT_SubjType extends HDT_Base> HyperSubjList<HDT_SubjType, HDT_ObjType> getSubjectList(RelationType relType, HDT_ObjType obj)
  { return new HyperSubjList<>(getRelSet(relType), obj); }

  public <HDT_ObjType extends HDT_Base, HDT_SubjType extends HDT_Base> HyperObjPointer<HDT_SubjType, HDT_ObjType> getObjPointer(RelationType relType, HDT_SubjType subj)
  { return new HyperObjPointer<>(getRelSet(relType), subj, true); }

  public <HDT_ObjType extends HDT_Base, HDT_SubjType extends HDT_Base> HyperSubjPointer<HDT_SubjType, HDT_ObjType> getSubjPointer(RelationType relType, HDT_ObjType obj)
  { return new HyperSubjPointer<>(getRelSet(relType), obj); }

  public HDT_RecordType getNestedTargetType(RelationType relType, Tag mainTag)
  { return relationSets.get(relType).getTargetType(mainTag); }

  public <HDT_SubjType extends HDT_Base> List<ObjectGroup> getObjectGroupList(RelationType relType, HDT_SubjType subj, Collection<Tag> tags)
  { return getRelSet(relType).getObjectGroupList(subj, tags); }

  public <HDT_SubjType extends HDT_Base> void updateObjectGroups(RelationType relType, HDT_SubjType subj, List<ObjectGroup> groups)
  { getRelSet(relType).updateObjectGroups(subj, groups); subj.modifyNow(); }

  public void updateNestedString(HDT_Base subj, HDT_Base obj, Tag tag, String str)
  { if (getRelSet(subj, obj).setNestedString(subj, obj, tag, str)) subj.modifyNow(); }

  public void updateNestedBoolean(HDT_Base subj, HDT_Base obj, Tag tag, boolean bool)
  { if (getRelSet(subj, obj).setNestedBoolean(subj, obj, tag, bool)) subj.modifyNow(); }

  public void updateNestedTernary(HDT_Base subj, HDT_Base obj, Tag tag, Ternary ternary)
  { if (getRelSet(subj, obj).setNestedTernary(subj, obj, tag, ternary)) subj.modifyNow(); }

  public void updateNestedPointer(HDT_Base subj, HDT_Base obj, Tag tag, HDT_Base target)
  { if (getRelSet(subj, obj).setNestedPointer(subj, obj, tag, target)) subj.modifyNow(); }

  public <HDT_SubjType extends HDT_Base> void resolvePointersByRelation(RelationType relType, HDT_SubjType subj) throws HDB_InternalError
  { getRelSet(relType).resolvePointers(subj); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings({ "unused", "unchecked" })
  public <HDT_T extends HDT_Base> Set<HDT_T> getOrphans(RelationType relType, Class<HDT_T> klazz)
  {
    return (Set<HDT_T>) relationSets.get(relType).getOrphans();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setResolvePointersAgain()
  {
    if ((deletionInProgress == false) && (pointerResolutionInProgress == false))
      messageDialog("Internal error #44928", mtError);

    this.resolveAgain = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cleanupRelations() throws HDB_InternalError
  {
    for (RelationSet<? extends HDT_Base, ? extends HDT_Base> relationSet : relationSets.values())
      relationSet.cleanup();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void fileNoLongerInUse(FilePath filePath)
  {
    if (deletionInProgress == false)
    {
      if (confirmDialog("No more records will be assigned to the file: \"" + filePath + "\". Should the file be deleted?"))
      {
        filePath.deletePromptOnFail(false);
        unmapFilePath(filePath);
      }

      return;
    }

    if (deleteFileAnswer == mrNone)
    {
      DialogResult result = seriesConfirmDialog("No more records will be assigned to the file: \"" + filePath + "\". Should the file be deleted?");

      switch (result)
      {
        case mrYes :      break;
        case mrNoToAll :  deleteFileAnswer = mrNoToAll;  break;
        case mrYesToAll : deleteFileAnswer = mrYesToAll; break;
        default : return;
      }
    }

    if (deleteFileAnswer == mrNoToAll) return;

    if (folderTreeWatcher.isDisabled() == false)
    {
      folderTreeWatcher.stop();
      folderTreeWatcher.disable();
    }

    filePath.deletePromptOnFail(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperDataset<? extends HDT_Base>.CoreAccessor records(HDT_RecordType type)
  {
    HyperDataset<? extends HDT_Base>.CoreAccessor accessor = accessors.get(type);

    if (accessor == null)
    {
      messageDialog("Internal error: null dataset", mtError);
      return null;
    }

    return accessor;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath getPath(String prefKey, FilePath relativePath)
  {
    String pathStr = appPrefs.get(PREF_KEY_SOURCE_PATH, "");
    if (pathStr.equals("")) return null;
    FilePath sourcePath = new FilePath(pathStr);

    if (prefKey.length() == 0)
      return sourcePath.resolve(relativePath);

    String path2 = prefs.get(prefKey, "");
    if (path2.equals("")) return null;

    if (FilePath.isEmpty(relativePath)) return sourcePath.resolve(new FilePath(path2));

    return sourcePath.resolve(new FilePath(path2))
                     .resolve(relativePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void finalizeXMLFile(ArrayList<StringBuilder> xmlList, ArrayList<String> filenameList, String fileName)
  {
    xmlList.get(xmlList.size() - 1).append(System.lineSeparator() + "</records>");
    filenameList.add(fileName);
    xmlList.add(new StringBuilder());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void writeDatasetToXML(ArrayList<StringBuilder> xmlList, HDT_RecordType type) throws HDB_InternalError, TerminateTaskException
  {
    StringBuilder xml = xmlList.get(xmlList.size() - 1);

    String recordsXmlVersion = RECORDS_XML_VERSION.toString();

    if (xml.length() == 0)
    {
      xml.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>" + System.lineSeparator() + System.lineSeparator());
      xml.append("<records version=\"" + recordsXmlVersion + "\" xmlns=\"org.hypernomicon\"");

      //xml.append(" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"org.hypernomicon http://hypernomicon.org/records.xsd\"");

      xml.append(">" + System.lineSeparator() + System.lineSeparator());
    }

    datasets.get(type).writeToXML(xml);

    switch (type)
    {
      case hdtDebate : case hdtNote : case hdtPersonGroup : case hdtWorkLabel : case hdtGlossary :
        curTaskCount += records(type).size() - 1;
        break;

      default :
        curTaskCount += records(type).size();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean saveAllToDisk(HyperFavorites favorites)
  {
    if (loaded == false) return false;

    ArrayList<String> filenameList = new ArrayList<>();
    ArrayList<StringBuilder> xmlList = new ArrayList<>();
    xmlList.add(new StringBuilder());

    task = new HyperTask() { @Override protected Boolean call() throws Exception
    {
      updateMessage("Saving to XML files...");

      curTaskCount = 0; totalTaskCount = 0;
      accessors.forEach((type, accessor) ->
      {
        switch (type)
        {
          case hdtNone :
            break;

          case hdtDebate : case hdtNote : case hdtPersonGroup : case hdtWorkLabel : case hdtGlossary :
            totalTaskCount += accessor.size() - 1;
            break;

          default :
            totalTaskCount += accessor.size();
            break;
        }
      });

      try
      {
        writeDatasetToXML(xmlList, hdtPersonStatus);    writeDatasetToXML(xmlList, hdtRank);            writeDatasetToXML(xmlList, hdtField);
        writeDatasetToXML(xmlList, hdtSubfield);        writeDatasetToXML(xmlList, hdtWorkType);        writeDatasetToXML(xmlList, hdtFileType);
        writeDatasetToXML(xmlList, hdtCountry);         writeDatasetToXML(xmlList, hdtState);           writeDatasetToXML(xmlList, hdtPositionVerdict);
        writeDatasetToXML(xmlList, hdtArgumentVerdict); writeDatasetToXML(xmlList, hdtInstitutionType); writeDatasetToXML(xmlList, hdtPersonGroup);

                                                        finalizeXMLFile(xmlList, filenameList, OTHER_FILE_NAME);

        writeDatasetToXML(xmlList, hdtPerson);          finalizeXMLFile(xmlList, filenameList, PERSON_FILE_NAME);
        writeDatasetToXML(xmlList, hdtInstitution);     finalizeXMLFile(xmlList, filenameList, INSTITUTION_FILE_NAME);
        writeDatasetToXML(xmlList, hdtInvestigation);   finalizeXMLFile(xmlList, filenameList, INVESTIGATION_FILE_NAME);
        writeDatasetToXML(xmlList, hdtDebate);          finalizeXMLFile(xmlList, filenameList, DEBATE_FILE_NAME);
        writeDatasetToXML(xmlList, hdtArgument);        finalizeXMLFile(xmlList, filenameList, ARGUMENT_FILE_NAME);
        writeDatasetToXML(xmlList, hdtPosition);        finalizeXMLFile(xmlList, filenameList, POSITION_FILE_NAME);
        writeDatasetToXML(xmlList, hdtGlossary);
        writeDatasetToXML(xmlList, hdtTerm);
        writeDatasetToXML(xmlList, hdtConcept);         finalizeXMLFile(xmlList, filenameList, TERM_FILE_NAME);
        writeDatasetToXML(xmlList, hdtFolder);
        writeDatasetToXML(xmlList, hdtMiscFile);
        writeDatasetToXML(xmlList, hdtWorkFile);        finalizeXMLFile(xmlList, filenameList, FILE_FILE_NAME);
        writeDatasetToXML(xmlList, hdtWorkLabel);
        writeDatasetToXML(xmlList, hdtWork);            finalizeXMLFile(xmlList, filenameList, WORK_FILE_NAME);
        writeDatasetToXML(xmlList, hdtNote);            finalizeXMLFile(xmlList, filenameList, NOTE_FILE_NAME);
        writeDatasetToXML(xmlList, hdtHub);             finalizeXMLFile(xmlList, filenameList, HUB_FILE_NAME);

        for (int ndx = 0; ndx < filenameList.size(); ndx++)
          saveStringBuilderToFile(xmlList.get(ndx), rootFilePath.resolve(new FilePath(filenameList.get(ndx))));
      }
      catch (IOException | HDB_InternalError e)
      {
        throw new HyperDataException("An error occurred while saving to XML files. " + e.getMessage(), e);
      }

      succeeded();
      return true;
    }};

    if (!HyperTask.performTaskWithProgressDialog(task)) return false;

    try (FileOutputStream out = new FileOutputStream(prefsFilePath.toFile()))
    {
      favorites.saveToPrefNode();

      if (prefs.get(PREF_KEY_SETTINGS_VERSION, "").length() == 0)
        prefs.put(PREF_KEY_SETTINGS_VERSION, HDB_SETTINGS_VERSION.toString());

      prefs.put(PREF_KEY_DB_CREATION_DATE, dateTimeToIso8601offset(dbCreationDate));

      prefs.exportSubtree(out);
    }
    catch (IOException | BackingStoreException e)
    {
      messageDialog("An error occurred while attempting to save database options to HDB file. Record data has been saved to XML files, however." + System.lineSeparator() + e.getMessage(), mtError);
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean loadAllFromDisk(HyperFavorites favorites) throws HDB_InternalError
  {
    if ((initialized == false) || unableToLoad)
      return false;

    if (nonNull(getLockOwner()))
      return false;

    FilePath newRootFilePath = new FilePath(appPrefs.get(PREF_KEY_SOURCE_PATH, System.getProperty("user.dir")));
    boolean dbChanged = false;

    if (FilePath.isEmpty(rootFilePath))
      dbChanged = true;
    else if (rootFilePath.equals(newRootFilePath) == false)
      dbChanged = true;

    close(null);

    rootFilePath = newRootFilePath;
    prefsFilePath = rootFilePath.resolve(new FilePath(appPrefs.get(PREF_KEY_SOURCE_FILENAME, PREFS_DEFAULT_FILENAME)));

    if (dbChanged)
      dbPreChangeHandlers.forEach(DatabaseEvent::handle);

    final ArrayList<FilePath> xmlFileList = new ArrayList<>();

    for (String fileName : new String[]{ OTHER_FILE_NAME,  PERSON_FILE_NAME,   INSTITUTION_FILE_NAME, INVESTIGATION_FILE_NAME,
                                         DEBATE_FILE_NAME, ARGUMENT_FILE_NAME, POSITION_FILE_NAME,    WORK_FILE_NAME,
                                         TERM_FILE_NAME,   FILE_FILE_NAME,     NOTE_FILE_NAME,        HUB_FILE_NAME })
    {
      FilePath filePath = rootFilePath.resolve(new FilePath(fileName));

      if (filePath.exists() == false)
        return falseWithErrorMessage("Unable to load database. Reason: File does not exist: " + filePath);

      xmlFileList.add(filePath);
    }

    task = new HyperTask() { @Override protected Boolean call() throws Exception
    {
      updateMessage("Loading database from folder " + rootFilePath + "...");
      updateProgress(0, 1);

      totalTaskCount = 0; curTaskCount = 0;

      for (FilePath filePath : xmlFileList) totalTaskCount += filePath.size();

      for (FilePath filePath : xmlFileList) loadFromXML(filePath);

      return true;
    }};

    if (!HyperTask.performTaskWithProgressDialog(task))
    {
      close(null);
      return false;
    }

    totalTaskCount = 0;

    accessors.values().forEach(coreAccessor -> totalTaskCount += coreAccessor.size());

    task = new HyperTask() { @Override protected Boolean call() throws Exception
    {
      updateMessage("Starting database session...");
      updateProgress(0, 1);

      curTaskCount = 0;

      for (HyperDataset<? extends HDT_Base> dataset : datasets.values())
        dataset.assignIDs();

      bringAllRecordsOnline();

      return true;
    }};

    if (!HyperTask.performTaskWithProgressDialog(task))
    {
      close(null);
      return false;
    }

    try { resolvePointers(); }
    catch (HDB_InternalError e) { return falseWithErrorMessage(e.getMessage()); }

    dbCreationDate = APP_GENESIS_INSTANT;

    try
    {
      try (InputStream is = new FileInputStream(rootFilePath.resolve(prefsFilePath).toFile()))
      {
        prefs = Preferences.userNodeForPackage(getClass());
        prefs.node("favorites").removeNode();
        prefs.clear();
        prefs.flush();
        Preferences.importPreferences(is);
        favorites.loadFromPrefNode();

        if ((db.getPath(PREF_KEY_PICTURES_PATH  , null) == null) ||
            (db.getPath(PREF_KEY_BOOKS_PATH     , null) == null) ||
            (db.getPath(PREF_KEY_PAPERS_PATH    , null) == null) ||
            (db.getPath(PREF_KEY_RESULTS_PATH   , null) == null) ||
            (db.getPath(PREF_KEY_UNENTERED_PATH , null) == null) ||
            (db.getPath(PREF_KEY_MISC_FILES_PATH, null) == null) ||
            (db.getPath(PREF_KEY_TOPICAL_PATH   , null) == null))
        {
          throw new HyperDataException("Unable to load information about paths from database options HDB file");
        }

        String dbCreationDateStr = prefs.get(PREF_KEY_DB_CREATION_DATE, "");
        if (safeStr(dbCreationDateStr).length() > 0)
          dbCreationDate = parseIso8601offset(dbCreationDateStr);

        String bibEncApiKey = prefs.get(PREF_KEY_BIB_API_KEY, ""),
               bibUserID = prefs.get(PREF_KEY_BIB_USER_ID, ""),
               bibTypeDescriptor = prefs.get(PREF_KEY_BIB_LIBRARY_TYPE, "");

        if ((bibEncApiKey.length() > 0) && (bibUserID.length() > 0) && (bibTypeDescriptor.length() > 0))
        {
          LibraryType libType = LibraryType.getByDescriptor(bibTypeDescriptor);

          try
          {
            loadBibLibrary(libType, bibEncApiKey, bibUserID);
          }
          catch (Exception e)
          {
            throw new HyperDataException("Unable to initialize link to " + libType.getUserReadableName() + ": " + e.getMessage(), e);
          }
        }
      }
      catch (IOException | InvalidPreferencesFormatException | BackingStoreException e)
      {
        throw new HyperDataException("An error occurred while attempting to read database options from HDB file: " + e.getMessage(), e);
      }
    }
    catch (HyperDataException e)
    {
      messageDialog(e.getMessage(), mtError);

      close(null);
      return false;
    }

    ArrayList<HDT_Work> worksToUnlink = new ArrayList<>();
    bibEntryKeyToWork.forEach((bibEntryKey, work) ->
    {
      if ((bibLibrary == null) || (bibLibrary.getEntryByKey(bibEntryKey) == null))
        worksToUnlink.add(work);
    });

    worksToUnlink.forEach(work -> work.setBibEntryKey(""));

    HDT_Record.setRootRecordDates();

    folders.getByID(ROOT_FOLDER_ID).checkExists();

    loaded = true;
    rebuildMentions();

    lock();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void unlinkBibLibrary()
  {
    if (bibLibrary == null) return;

    boolean startWatcher = folderTreeWatcher.stop();

    bibLibrary = null;

    getRootFilePath().resolve(new FilePath(BIB_FILE_NAME)).deletePromptOnFail(true);

    prefs.remove(PREF_KEY_BIB_API_KEY);
    prefs.remove(PREF_KEY_BIB_USER_ID);
    prefs.remove(PREF_KEY_BIB_LIBRARY_VERSION);
    prefs.remove(PREF_KEY_BIB_LIBRARY_TYPE);

    works.forEach(work -> work.setBibEntryKey(""));

    bibChangedHandlers.forEach(DatabaseEvent::handle);

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void linkBibLibrary(LibraryType libType, String bibEncApiKey, String bibUserID) throws InvalidKeyException, NoSuchAlgorithmException, InvalidKeySpecException, NoSuchPaddingException, InvalidAlgorithmParameterException, UnsupportedEncodingException, IllegalBlockSizeException, BadPaddingException, IOException, ParseException, HDB_InternalError
  {
    if (bibLibrary != null)
      throw new HDB_InternalError(21174);

    FilePath bibJsonFilePath = getRootFilePath().resolve(new FilePath(BIB_FILE_NAME));

    if (bibJsonFilePath.exists())
    {
      boolean startWatcher = folderTreeWatcher.stop();

      bibJsonFilePath.deletePromptOnFail(true);

      if (startWatcher)
        folderTreeWatcher.createNewWatcherAndStart();
    }

    loadBibLibrary(libType, bibEncApiKey, bibUserID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadBibLibrary(LibraryType libType, String bibEncApiKey, String bibUserID) throws InvalidKeyException, NoSuchAlgorithmException, InvalidKeySpecException, NoSuchPaddingException, InvalidAlgorithmParameterException, UnsupportedEncodingException, IllegalBlockSizeException, BadPaddingException, IOException, ParseException, HDB_InternalError
  {
    if (bibLibrary != null)
      throw new HDB_InternalError(21173);

    String bibApiKey = CryptoUtil.decrypt("", bibEncApiKey);
    LibraryWrapper<? extends BibEntry, ? extends BibCollection> bLibrary;

    switch (libType)
    {
      case ltZotero :

        bLibrary = new ZoteroWrapper(bibApiKey, bibUserID);
        break;

      default :

        throw new HDB_InternalError(21175);
    }

    bLibrary.loadFromDisk(getRootFilePath().resolve(new FilePath(BIB_FILE_NAME)));

    bibLibrary = bLibrary;

    prefs.put(PREF_KEY_BIB_API_KEY, bibEncApiKey);
    prefs.put(PREF_KEY_BIB_USER_ID, bibUserID);
    prefs.put(PREF_KEY_BIB_LIBRARY_TYPE, libType.getDescriptor());

    bibChangedHandlers.forEach(DatabaseEvent::handle);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isProtectedRecord(int id, HDT_RecordType type)
  {
    if (isUnstoredRecord(id, type)) return true;

    if (type == hdtFolder)
      return id < FIRST_USER_FOLDER_ID;

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isUnstoredRecord(int id, HDT_RecordType type)
  {
    switch (type)
    {
      case hdtNote : case hdtDebate : case hdtWorkLabel : case hdtPersonGroup : case hdtFolder : case hdtGlossary :
        return id == 1;
      default:
        return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void deleteRecord(HDT_RecordType type, int id)
  {
    boolean doMentionsRebuild = false;

    if (isProtectedRecord(id, type))
    {
      messageDialog("Unable to delete record.", mtError);
      return;
    }

    HDT_Base record = records(type).getByID(id);

    if (record == null)
    {
      messageDialog("Unable to delete record.", mtError);
      return;
    }

    if (record.isExpired())
    {
      messageDialog("The record has already been deleted.", mtError);
      return;
    }

    if (record.isDummy() == false)
    {
      if (mentionsIndex.isRebuilding())
      {
        doMentionsRebuild = true;
        mentionsIndex.stopRebuild();
      }
      else
        mentionsIndex.removeRecord(record);

      try { record.setSearchKey(""); } catch (SearchKeyException e) { noOp(); }
    }

    if (deletionInProgress)
    {
      record.expire();
      return;
    }

    deletionInProgress = true;
    deleteFileAnswer = mrNone;

    record.expire();

    try
    {
      resolvePointers();
      cleanupRelations();
    }
    catch (HDB_InternalError e)
    {
      messageDialog(e.getMessage(), mtError);
    }

    deletionInProgress = false;
    deleteFileAnswer = mrNone;

    if (folderTreeWatcher.isDisabled())
    {
      folderTreeWatcher.enable();
      folderTreeWatcher.createNewWatcherAndStart();
    }

    if (doMentionsRebuild)
      rebuildMentions();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void resolvePointers() throws HDB_InternalError
  {
    if (pointerResolutionInProgress)
      throw new HDB_InternalError(78382);

    pointerResolutionInProgress = true;

    do
    {
      resolveAgain = false;
      for (HyperDataset<? extends HDT_Base> dataset : datasets.values())
        dataset.resolvePointers();
    } while (resolveAgain);

    pointerResolutionInProgress = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void bringAllRecordsOnline() throws HyperDataException, TerminateTaskException
  {
    try
    {
      for (HyperDataset<? extends HDT_Base> dataset : datasets.values()) // Folders must be brought online first. See HyperPath.assignNameInternal
        dataset.bringAllRecordsOnline();

      addRootFolder();
    }
    catch (RelationCycleException | HDB_InternalError | SearchKeyException e)
    {
      throw new HyperDataException(e);
    }
    catch (HubChangedException e)
    {
      String msg = "Internal error #42837";
      throw new HyperDataException(msg, e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addRootFolder()
  {
    Set<HyperPath> set = filenameMap.get(rootFilePath.getNameOnly().toString());
    if (set == null)
    {
      set = Sets.newConcurrentHashSet();
      filenameMap.put(rootFilePath.getNameOnly().toString(), set);
    }

    set.add(folders.getByID(HyperDB.ROOT_FOLDER_ID).getPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_RecordState getNextRecordFromXML(XMLEventReader eventReader) throws XMLStreamException
  {
    while (eventReader.hasNext())
    {
      XMLEvent event = eventReader.nextEvent();

      if (event.isStartElement() == false)
        continue;

      StartElement startElement = event.asStartElement();
      if (startElement.getName().getLocalPart().equals(tagToStr.get(tagRecord)) == false)
        continue;

      int id = -1;
      HDT_RecordType type = hdtNone;
      String sortKeyAttr = "", listName = "", searchKey = "";

      @SuppressWarnings("unchecked")
      Iterator<Attribute> attributes = startElement.getAttributes();

      while (attributes.hasNext())
      {
        Attribute attribute = attributes.next();
        Tag tag = tagToStr.inverse().get(attribute.getName().toString());

        switch (tag)
        {
          case tagID :        id = parseInt(attribute.getValue(), -1); break;
          case tagType :      type = typeToTagStr.inverse().getOrDefault(attribute.getValue(), hdtNone); break;
          case tagSortKey :   sortKeyAttr = attribute.getValue(); break;
          case tagSearchKey : searchKey = attribute.getValue(); break;
          case tagListName :  listName = attribute.getValue(); break;
          default : break;
        }
      }

      return new HDT_RecordState(type, id, sortKeyAttr, "", searchKey, listName);
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public <T extends HDT_Record> T createNewRecordFromState(HDT_RecordState recordState, boolean bringOnline) throws DuplicateRecordException, RelationCycleException, HDB_InternalError, SearchKeyException, HubChangedException
  {
    return (T) datasets.get(recordState.type).createNewRecord(recordState, bringOnline);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public <T extends HDT_Record> T createNewBlankRecord(HDT_RecordType type)
  {
    HDT_RecordState recordState = new HDT_RecordState(type, -1, "", "", "", "");

    try
    {
      return (T) datasets.get(type).createNewRecord(recordState, true);
    }
    catch (HDB_InternalError e)
    {
      messageDialog(e.getMessage(), mtError);
    }
    catch (DuplicateRecordException | RelationCycleException | SearchKeyException | HubChangedException e) { noOp(); }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public class HDX_Element
  {
    public Tag tag;
    public int objID;
    public HDT_RecordType objType;

  //---------------------------------------------------------------------------

    @SuppressWarnings("unchecked")
    public HDX_Element(StartElement startElement, HDT_RecordState xmlRecord) throws InvalidItemException
    {
      tag = tagToStr.inverse().getOrDefault(startElement.getName().getLocalPart(), tagNone);

      if (tag == tagNone)
        throw new InvalidItemException(xmlRecord.id, xmlRecord.type, startElement.getName().getLocalPart());

      objType = tagToObjType.getOrDefault(tag, hdtNone);
      objID = -1;

      ((Iterator<Attribute>)startElement.getAttributes()).forEachRemaining(attribute ->
      {
        switch (attribute.getName().toString())
        {
          case "id" :
            if (objType != hdtNone)
              objID = parseInt(attribute.getValue(), -1);
            break;

          case "type" :
            if (objType == hdtAuxiliary) // this represents that the object type is not given away by the
                                         // tag name, and should be obtained from the "type" attribute
              objType = typeToTagStr.inverse().getOrDefault(attribute.getValue(), hdtNone);
            break;

          default:
            break;
        }
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadFromXML(FilePath filePath) throws HyperDataException, TerminateTaskException
  {
    try (InputStream in = new FileInputStream(filePath.toFile()))
    {
      XMLEventReader eventReader = XMLInputFactory.newInstance().createXMLEventReader(in);
      HDT_RecordState xmlRecord = getNextRecordFromXML(eventReader);

      while (nonNull(xmlRecord))
      {
        boolean notDoneReadingRecord = eventReader.hasNext(), noInnerTags = true, wasAlreadyInStartTag = false;
        LinkedHashMap<Tag, HDI_OfflineBase> nestedItems = null;
        HDT_RecordType objType = hdtNone;
        XMLEvent event = null;
        String nodeText = "";
        Tag tag = tagNone;
        int objID = -1;

        while (notDoneReadingRecord)
        {
          if (task.isCancelled()) throw new TerminateTaskException();

          event = eventReader.nextEvent();
          switch (event.getEventType())
          {
            case XMLStreamConstants.START_ELEMENT :

              HDX_Element hdxElement = new HDX_Element(event.asStartElement(), xmlRecord);

              if (wasAlreadyInStartTag)
              {
                if (nestedItems == null)
                  nestedItems = new LinkedHashMap<>();

                readNestedItem(xmlRecord, nestedItems, getRelation(xmlRecord.type, objType), hdxElement, eventReader);
              }
              else
              {
                objID = hdxElement.objID;
                objType = hdxElement.objType;
                tag = hdxElement.tag;
                nodeText = "";
                noInnerTags = false;
                nestedItems = null;
              }

              wasAlreadyInStartTag = true;
              break;

            case XMLStreamConstants.END_ELEMENT :

              wasAlreadyInStartTag = false;
              if (event.asEndElement().getName().getLocalPart().equals("record"))
                notDoneReadingRecord = false;
              else
              {
                try
                {
                  switch (tag)
                  {
                    case tagCreationDate : xmlRecord.creationDate = parseIso8601offset(nodeText); break;
                    case tagModifiedDate : xmlRecord.modifiedDate = parseIso8601offset(nodeText); break;
                    case tagViewDate :     xmlRecord.viewDate =     parseIso8601offset(nodeText); break;

                    default :              xmlRecord.loadItemFromXML(tag, nodeText, objType, objID, nestedItems);
                  }
                }
                catch (DateTimeParseException e)
                {
                  throw new HyperDataException(e);
                }

                tag = tagNone;
                nodeText = "";
                objType = hdtNone;
                objID = -1;
              }

              break;

            case XMLStreamConstants.CHARACTERS   : nodeText = nodeText + event.asCharacters().getData(); break;
            case XMLStreamConstants.END_DOCUMENT : notDoneReadingRecord = false; break;
            default                              : break;
          }
        }

        if (noInnerTags)
          xmlRecord.loadItemFromXML(tagNone, nodeText, hdtNone, -1, null);

        try
        {
          if (isUnstoredRecord(xmlRecord.id, xmlRecord.type) == false)
            createNewRecordFromState(xmlRecord, false);
        }
        catch (RelationCycleException | HDB_InternalError | SearchKeyException e) { noOp(); }

        if (nonNull(event))
          task.updateProgress(curTaskCount + event.getLocation().getCharacterOffset(), totalTaskCount);

        xmlRecord = getNextRecordFromXML(eventReader);
      }

      curTaskCount += filePath.size();
    }
    catch (IOException | DuplicateRecordException | InvalidItemException e)
    {
      throw new HyperDataException(e);
    }
    catch (XMLStreamException e)
    {
      String msg = "File: " + filePath + System.lineSeparator() + e.getMessage();
      throw new HyperDataException(msg, e);
    }
    catch (HubChangedException e)
    {
      String msg = "Internal error #42837";
      throw new HyperDataException(msg, e);
    }

    return;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initNestedItems(HDT_RecordState xmlRecord, LinkedHashMap<Tag, HDI_OfflineBase> nestedItems, RelationType relation)
  {
    HDI_OfflineBase item = null;

    Collection<HDI_Schema> schemas = relationSets.get(relation).getSchemas();

    if (schemas == null) return;

    for (HDI_Schema schema : schemas)
    {
      switch (schema.getCategory())
      {
        case hdcBoolean:       item = new HDI_OfflineBoolean(schema, xmlRecord); break;
        case hdcTernary:       item = new HDI_OfflineTernary(schema, xmlRecord); break;
        case hdcString:        item = new HDI_OfflineString(schema, xmlRecord); break;
        case hdcNestedPointer: item = new HDI_OfflineNestedPointer(schema, xmlRecord); break;

        default :
          messageDialog("Internal error #78936", mtError);
          return;
      }

      for (Tag tag : schema.getTags())
        nestedItems.put(tag, item);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void readNestedItem(HDT_RecordState xmlRecord, LinkedHashMap<Tag, HDI_OfflineBase> nestedItems, RelationType relationType, HDX_Element hdxElement, XMLEventReader eventReader) throws XMLStreamException, HyperDataException, InvalidItemException
  {
    boolean notDone = eventReader.hasNext();
    String nodeText = "";
    Characters chars;

    if (nestedItems.isEmpty())
      initNestedItems(xmlRecord, nestedItems, relationType);

    while (notDone)
    {
      XMLEvent event = eventReader.nextEvent();

      switch (event.getEventType())
      {
        case XMLStreamConstants.START_ELEMENT :

          String msg = "Too many nested elements in XML";
          throw new HyperDataException(msg);

        case XMLStreamConstants.END_ELEMENT :

          notDone = false;
          break;

        case XMLStreamConstants.CHARACTERS :

          chars = event.asCharacters();
          nodeText = nodeText + chars.getData();
          break;

        case XMLStreamConstants.END_DOCUMENT :

          notDone = false;
          break;

        default :
          break;
      }
    }

    HDI_OfflineBase item = nestedItems.get(hdxElement.tag);

    if (item == null) throw new InvalidItemException(xmlRecord.id, xmlRecord.type, "(nested) " + getTagStr(hdxElement.tag));

    item.setFromXml(hdxElement.tag, nodeText, hdxElement.objType, hdxElement.objID, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addToInitialNavList(HDT_Base record)
  {
    if (isUnstoredRecord(record.getID(), record.getType())) return;

    switch (record.getType())
    {
      case hdtArgument:     case hdtDebate:   case hdtPosition: case hdtInvestigation:
      case hdtPersonGroup:  case hdtMiscFile: case hdtNote:     case hdtPerson:
      case hdtInstitution:  case hdtConcept:  case hdtWork:     case hdtWorkLabel:
        break;
      default:
        return;
    }

    int ndx = binarySearch(initialNavList, record, (record1, record2) -> record1.getViewDate().compareTo(record2.getViewDate()));

    if (ndx >= 0) ndx++;
    else          ndx = (ndx + 1) * -1;

    initialNavList.add(ndx, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath getRequestMessageFilePath()
  { return new FilePath(appPrefs.get(PREF_KEY_SOURCE_PATH, "")).resolve(new FilePath(REQUEST_MSG_FILE_NAME)); }

  public FilePath getResponseMessageFilePath()
  { return new FilePath(appPrefs.get(PREF_KEY_SOURCE_PATH, "")).resolve(new FilePath(RESPONSE_MSG_FILE_NAME)); }

  public FilePath getLockFilePath()
  { return new FilePath(appPrefs.get(PREF_KEY_SOURCE_PATH, "")).resolve(new FilePath(LOCK_FILE_NAME)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static enum HDB_MessageType
  {
    hmtNone,
    hmtEchoRequest,
    hmtEchoReply,
    hmtUnlockRequest,
    hmtUnlockComplete
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getLockOwner()
  {
    List<String> s = new ArrayList<>();
    FilePath filePath = getLockFilePath();
    StringBuilder errorSB = new StringBuilder("");

    if (filePath.exists())
    {
      try { s = FileUtils.readLines(filePath.toFile(), "UTF-8"); }
      catch (IOException e) { return "whatevervolleyball"; }

      if (s.get(0).equals(getComputerName()) == false)
        return s.get(0);
      else
        filePath.deleteReturnsBoolean(true, errorSB);
    }

    getRequestMessageFilePath().deletePromptOnFail(true);
    getResponseMessageFilePath().deletePromptOnFail(true);

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean lock()
  {
    lockFilePath = getLockFilePath();

    try { FileUtils.writeLines(lockFilePath.toFile(), singletonList(getComputerName())); }
    catch (IOException e) { return false; }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void close(EnumSet<HDT_RecordType> datasetsToKeep) throws HDB_InternalError
  {
    boolean bringOnline = nonNull(datasetsToKeep); // Datasets remain online through process of creating a new database

    folderTreeWatcher.stop();

    if (FilePath.isEmpty(lockFilePath) == false)
    {
      lockFilePath.deletePromptOnFail(true);
      lockFilePath = null;
    }

    mentionsIndex.stopRebuild();
    loaded = false;
    clearAllDataSets(datasetsToKeep);
    cleanupRelations();

    initialNavList.clear();
    filenameMap = new FilenameMap<>();
    keyWorkIndex = new HashMap<>();
    displayedAtIndex = new BidiOneToManyMainTextMap();
    bibEntryKeyToWork = new HashMap<>();

    if (bibLibrary != null)
    {
      bibLibrary = null;
      bibChangedHandlers.forEach(DatabaseEvent::handle);

      prefs.remove(PREF_KEY_BIB_API_KEY);
      prefs.remove(PREF_KEY_BIB_USER_ID);
      prefs.remove(PREF_KEY_BIB_LIBRARY_VERSION);
      prefs.remove(PREF_KEY_BIB_LIBRARY_TYPE);
    }

    try
    {
      HDT_RecordState recordState;

      recordState = new HDT_RecordState(hdtFolder, ROOT_FOLDER_ID, "", "", "", "");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new HDT_RecordState(hdtDebate, 1, "", "", "", "");
      HDI_OfflineString.class.cast(recordState.items.get(tagName)).set("All debates");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new HDT_RecordState(hdtNote, 1, "", "", "", "");
      HDI_OfflineString.class.cast(recordState.items.get(tagName)).set("All notes");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new HDT_RecordState(hdtWorkLabel, 1, "", "", "", "");
      HDI_OfflineString.class.cast(recordState.items.get(tagText)).set("All labels");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new HDT_RecordState(hdtPersonGroup, 1, "", "", "", "");
      HDI_OfflineString.class.cast(recordState.items.get(tagName)).set("All groups");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new HDT_RecordState(hdtGlossary, 1, "", "", "", "");
      HDI_OfflineString.class.cast(recordState.items.get(tagName)).set("General");
      HDI_OfflineBoolean.class.cast(recordState.items.get(tagActive)).set(true);
      createNewRecordFromState(recordState, bringOnline);

      dbCloseHandlers.forEach(DatabaseEvent::handle);

    } catch (DuplicateRecordException | RelationCycleException | SearchKeyException | HubChangedException e) { noOp(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearAllDataSets(EnumSet<HDT_RecordType> datasetsToKeep)
  {
    boolean leaveOnline = false;

    if (nonNull(datasetsToKeep)) // It should only be non-null when a new database is being created
    {
      datasetsToKeep.add(hdtWorkType);
      datasetsToKeep.add(hdtPositionVerdict);
      datasetsToKeep.add(hdtArgumentVerdict);
      datasetsToKeep.add(hdtInstitutionType);

      leaveOnline = true; // Datasets remain online through process of creating a new database
    }
    else
      datasetsToKeep = EnumSet.noneOf(HDT_RecordType.class);

    for (Entry<HDT_RecordType, HyperDataset<? extends HDT_Base>> entry : datasets.entrySet())
      if (datasetsToKeep.contains(entry.getKey()) == false)
        entry.getValue().removeAll(leaveOnline);

    searchKeys.removeAll();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean newDB(String newPath, EnumSet<HDT_RecordType> datasetsToKeep, HashMap<String, String> folders) throws HDB_InternalError
  {
    if (loaded == false) return false;

    if (datasetsToKeep == null)
      datasetsToKeep = EnumSet.noneOf(HDT_RecordType.class);

    close(datasetsToKeep);

    dbCreationDate = Instant.now();
    prefs.put(PREF_KEY_DB_CREATION_DATE, dateTimeToIso8601offset(dbCreationDate));

    appPrefs.put(PREF_KEY_SOURCE_PATH, newPath);
    rootFilePath = new FilePath(newPath);
    prefsFilePath = rootFilePath.resolve(new FilePath(appPrefs.get(PREF_KEY_SOURCE_FILENAME, PREFS_DEFAULT_FILENAME)));

    folders.forEach(prefs::put);

    addRootFolder();

    HDT_Record.setRootRecordDates();

    try
    {
      HDT_RecordState recordState;

      recordState = new HDT_RecordState(hdtFolder, PAPERS_FOLDER_ID, "", "", "", "");
      HDI_OfflinePath.class.cast(recordState.items.get(tagFileName)).setFileName(folders.get(PREF_KEY_PAPERS_PATH));
      HDI_OfflinePath.class.cast(recordState.items.get(tagParentFolder)).setFolderID(ROOT_FOLDER_ID);
      createNewRecordFromState(recordState, true);

      recordState = new HDT_RecordState(hdtFolder, BOOKS_FOLDER_ID, "", "", "", "");
      HDI_OfflinePath.class.cast(recordState.items.get(tagFileName)).setFileName(folders.get(PREF_KEY_BOOKS_PATH));
      HDI_OfflinePath.class.cast(recordState.items.get(tagParentFolder)).setFolderID(ROOT_FOLDER_ID);
      createNewRecordFromState(recordState, true);

      recordState = new HDT_RecordState(hdtFolder, MISC_FOLDER_ID, "", "", "", "");
      HDI_OfflinePath.class.cast(recordState.items.get(tagFileName)).setFileName(folders.get(PREF_KEY_MISC_FILES_PATH));
      HDI_OfflinePath.class.cast(recordState.items.get(tagParentFolder)).setFolderID(ROOT_FOLDER_ID);
      createNewRecordFromState(recordState, true);

      recordState = new HDT_RecordState(hdtFolder, PICTURES_FOLDER_ID, "", "", "", "");
      HDI_OfflinePath.class.cast(recordState.items.get(tagFileName)).setFileName(folders.get(PREF_KEY_PICTURES_PATH));
      HDI_OfflinePath.class.cast(recordState.items.get(tagParentFolder)).setFolderID(ROOT_FOLDER_ID);
      createNewRecordFromState(recordState, true);

      recordState = new HDT_RecordState(hdtFolder, TOPICAL_FOLDER_ID, "", "", "", "");
      HDI_OfflinePath.class.cast(recordState.items.get(tagFileName)).setFileName(folders.get(PREF_KEY_TOPICAL_PATH));
      HDI_OfflinePath.class.cast(recordState.items.get(tagParentFolder)).setFolderID(ROOT_FOLDER_ID);
      createNewRecordFromState(recordState, true);

      recordState = new HDT_RecordState(hdtFolder, UNENTERED_FOLDER_ID, "", "", "", "");
      HDI_OfflinePath.class.cast(recordState.items.get(tagFileName)).setFileName(folders.get(PREF_KEY_UNENTERED_PATH));
      HDI_OfflinePath.class.cast(recordState.items.get(tagParentFolder)).setFolderID(ROOT_FOLDER_ID);
      createNewRecordFromState(recordState, true);

      recordState = new HDT_RecordState(hdtFolder, RESULTS_FOLDER_ID, "", "", "", "");
      HDI_OfflinePath.class.cast(recordState.items.get(tagFileName)).setFileName(folders.get(PREF_KEY_RESULTS_PATH));
      HDI_OfflinePath.class.cast(recordState.items.get(tagParentFolder)).setFolderID(ROOT_FOLDER_ID);
      createNewRecordFromState(recordState, true);
    }
    catch(RelationCycleException | DuplicateRecordException | HDB_InternalError | SearchKeyException | HubChangedException e)
    {
      messageDialog("Unable to create folder records for new database.", mtError);
      return false;
    }

    try
    {
      resolvePointers();
    }
    catch (HDB_InternalError e)
    {
      messageDialog(e.getMessage(), mtError);
      return false;
    }

    loaded = true;

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<Tag> getTagsByRecordType(HDT_RecordType recordType)
  {
    EnumSet<Tag> tags = EnumSet.noneOf(Tag.class);

    if (datasets.containsKey(recordType))
      tags.addAll(datasets.get(recordType).getTags());
    else
      datasets.values().forEach(dataset -> tags.addAll(dataset.getTags()));

    tags.remove(tagHub);

    return tags;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Collection<HDI_Schema> getSchemasByRecordType(HDT_RecordType type)
  {
    return nullSwitch(datasets.get(type), null, HyperDataset::getSchemas);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDI_Schema> getSchemasByTag(Tag tag)
  {
    Set<HDI_Schema> schemas = new HashSet<>();

    datasets.values().forEach(dataset ->
    {
      HDI_Schema schema = dataset.getSchema(tag);

      if (nonNull(schema))
        schemas.add(schema);
    });

    return schemas;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperDataset<HDT_Person         >.CoreAccessor persons;
  public HyperDataset<HDT_PersonStatus   >.CoreAccessor personStatuses;
  public HyperDataset<HDT_Institution    >.CoreAccessor institutions;
  public HyperDataset<HDT_InstitutionType>.CoreAccessor institutionTypes;
  public HyperDataset<HDT_State          >.CoreAccessor states;
  public HyperDataset<HDT_Country        >.CoreAccessor countries;
  public HyperDataset<HDT_Rank           >.CoreAccessor ranks;
  public HyperDataset<HDT_Investigation  >.CoreAccessor investigations;
  public HyperDataset<HDT_Debate         >.CoreAccessor debates;
  public HyperDataset<HDT_Argument       >.CoreAccessor arguments;
  public HyperDataset<HDT_Position       >.CoreAccessor positions;
  public HyperDataset<HDT_Field          >.CoreAccessor fields;
  public HyperDataset<HDT_Subfield       >.CoreAccessor subfields;
  public HyperDataset<HDT_Term           >.CoreAccessor terms;
  public HyperDataset<HDT_Concept        >.CoreAccessor concepts;
  public HyperDataset<HDT_Work           >.CoreAccessor works;
  public HyperDataset<HDT_WorkType       >.CoreAccessor workTypes;
  public HyperDataset<HDT_WorkLabel      >.CoreAccessor workLabels;
  public HyperDataset<HDT_PositionVerdict>.CoreAccessor positionVerdicts;
  public HyperDataset<HDT_ArgumentVerdict>.CoreAccessor argumentVerdicts;
  public HyperDataset<HDT_MiscFile       >.CoreAccessor miscFiles;
  public HyperDataset<HDT_WorkFile       >.CoreAccessor workFiles;
  public HyperDataset<HDT_Folder         >.CoreAccessor folders;
  public HyperDataset<HDT_Note           >.CoreAccessor notes;
  public HyperDataset<HDT_Glossary       >.CoreAccessor glossaries;
  public HyperDataset<HDT_Hub            >.CoreAccessor hubs;
  public HyperDataset<HDT_PersonGroup    >.CoreAccessor personGroups;
  public HyperDataset<HDT_FileType       >.CoreAccessor fileTypes;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addTag(String tagString, Tag tag, String tagHeader) throws HDB_InternalError
  {
    addTag(tagString, tag, tagHeader, null);
  }

  private <HDT_T extends HDT_Base> HyperDataset<HDT_T>.CoreAccessor addTag(String tagStr, Tag tag, String tagHeader, Class<HDT_T> klass) throws HDB_InternalError
  {
    int hashCode = Math.abs(stringHash(tagStr));
    if (tagToNum.containsValue(hashCode))
      throw new HDB_InternalError(99215, "Duplicate tag hash codes.");

    tagToStr.put(tag, tagStr);
    tagToNum.put(tag, hashCode);
    tagToHeader.put(tag, tagHeader);

    if (klass == null) return null;

    HDT_RecordType type = HDT_RecordType.typeByRecordClass(klass);

    typeToTagStr.put(type, tagStr);
    typeToTag.put(type, tag);

    HyperDataset<HDT_T> dataset = new HyperDataset<>(type);
    HyperDataset<HDT_T>.CoreAccessor accessor = dataset.getAccessor();
    datasets.put(type, dataset);
    accessors.put(type, accessor);

    return accessor;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void init(Preferences appPrefs, FolderTreeWatcher folderTreeWatcher)
  {
    if (initialized || unableToLoad) return;

    this.appPrefs = appPrefs;
    this.folderTreeWatcher = folderTreeWatcher;

    try
    {
      persons          = addTag("person",             tagPerson,          "Person",                    HDT_Person         .class);
      personStatuses   = addTag("person_status",      tagPersonStatus,    "Status",                    HDT_PersonStatus   .class);
      institutions     = addTag("institution",        tagInstitution,     "Institution",               HDT_Institution    .class);
      institutionTypes = addTag("institution_type",   tagInstitutionType, "Institution Type",          HDT_InstitutionType.class);
      states           = addTag("state",              tagState,           "State",                     HDT_State          .class);
      countries        = addTag("country",            tagCountry,         "Country",                   HDT_Country        .class);
      ranks            = addTag("rank",               tagRank,            "Rank",                      HDT_Rank           .class);
      investigations   = addTag("investigation",      tagInvestigation,   "Investigation",             HDT_Investigation  .class);
      debates          = addTag("debate",             tagDebate,          "Problem/Debate",            HDT_Debate         .class);
      arguments        = addTag("argument",           tagArgument,        "Argument",                  HDT_Argument       .class);
      terms            = addTag("term",               tagTerm,            "Term",                      HDT_Term           .class);
      concepts         = addTag("concept",            tagConcept,         "Concept",                   HDT_Concept        .class);
      works            = addTag("work",               tagWork,            "Work",                      HDT_Work           .class);
      workTypes        = addTag("work_type",          tagWorkType,        "Type of Work",              HDT_WorkType       .class);
      workLabels       = addTag("work_label",         tagWorkLabel,       "Work Label",                HDT_WorkLabel      .class);
      fields           = addTag("field",              tagField,           "Field",                     HDT_Field          .class);
      subfields        = addTag("subfield",           tagSubfield,        "Subfield",                  HDT_Subfield       .class);
      positions        = addTag("position",           tagPosition,        "Position",                  HDT_Position       .class);
      positionVerdicts = addTag("position_verdict",   tagPositionVerdict, "Conclusion about Position", HDT_PositionVerdict.class);
      argumentVerdicts = addTag("argument_verdict",   tagArgumentVerdict, "Conclusion about Argument", HDT_ArgumentVerdict.class);
      miscFiles        = addTag("misc_file",          tagMiscFile,        "Misc. File",                HDT_MiscFile       .class);
      workFiles        = addTag("work_file",          tagWorkFile,        "Work File",                 HDT_WorkFile       .class);
      folders          = addTag("folder",             tagFolder,          "Folder",                    HDT_Folder         .class);
      notes            = addTag("note",               tagNote,            "Note",                      HDT_Note           .class);
      glossaries       = addTag("glossary",           tagGlossary,        "Glossary",                  HDT_Glossary       .class);
      hubs             = addTag("hub",                tagHub,             "Record Hub",                HDT_Hub            .class);
      personGroups     = addTag("person_group",       tagPersonGroup,     "Person Group",              HDT_PersonGroup    .class);
      fileTypes        = addTag("file_type",          tagFileType,        "File Type",                 HDT_FileType       .class);

                         addTag("id",                 tagID,              "Record ID");
                         addTag("type",               tagType,            "Record Type");
                         addTag("sort_key",           tagSortKey,         "Sort Key");
                         addTag("search_key",         tagSearchKey,       "Search Key");
                         addTag("record",             tagRecord,          "Record");
                         addTag("list_name",          tagListName,        "List Name");
                         addTag("first_name",         tagFirstName,       "First Name");
                         addTag("last_name",          tagLastName,        "Last Name");
                         addTag("link",               tagWebLink,         "Link");
                         addTag("orcid",              tagORCID,           "ORCID");
                         addTag("picture",            tagPicture,         "Picture");
                         addTag("picture_crop",       tagPictureCrop,     "Picture Crop");
                         addTag("why_famous",         tagWhyFamous,       "Description");
                         addTag("name",               tagName,            "Name");
                         addTag("city",               tagCity,            "City");
                         addTag("description",        tagDescription,     "Description");
                         addTag("title",              tagTitle,           "Title");
                         addTag("file_name",          tagFileName,        "Filename");
                         addTag("year",               tagYear,            "Year");
                         addTag("bib_entry_key",      tagBibEntryKey,     "Bibliography Entry Key");
                         addTag("misc_bib",           tagMiscBib,         "Misc. Bib. Info");
                         addTag("doi",                tagDOI,             "DOI");
                         addTag("isbn",               tagISBN,            "ISBN");
                         addTag("author",             tagAuthor,          "Author");
                         addTag("in_filename",        tagInFileName,      "Included in File Name");
                         addTag("editor",             tagEditor,          "Editor");
                         addTag("translator",         tagTranslator,      "Translator");
                         addTag("start_page",         tagStartPageNum,    "Starting Page Number");
                         addTag("end_page",           tagEndPageNum,      "Ending Page Number");
                         addTag("annotated",          tagAnnotated,       "Annotated");
                         addTag("comments",           tagComments,        "Description");
                         addTag("larger_debate",      tagLargerDebate,    "Larger Debate");
                         addTag("counterargument",    tagCounterargument, "Counterargument");
                         addTag("definition",         tagDefinition,      "Definition");
                         addTag("text",               tagText,            "Text");
                         addTag("active",             tagActive,          "Active");
                         addTag("larger_position",    tagLargerPosition,  "Larger Position");
                         addTag("parent_institution", tagParentInst,      "Parent Institution");
                         addTag("parent_glossary",    tagParentGlossary,  "Parent Glossary");
                         addTag("parent_note",        tagParentNote,      "Parent Note");
                         addTag("parent_folder",      tagParentFolder,    "Parent Folder");
                         addTag("linked_record",      tagLinkedRecord,    "Linked Record");
                         addTag("display_item",       tagDisplayRecord,   "Relevant Records");
                         addTag("key_work",           tagKeyWork,         "Key Works");
                         addTag("larger_work",        tagLargerWork,      "Larger Work");
                         addTag("parent_label",       tagParentLabel,     "Parent Label");
                         addTag("parent_group",       tagParentGroup,     "Parent Group");
                         addTag("creation_date",      tagCreationDate,    "Date Created");
                         addTag("modified_date",      tagModifiedDate,    "Date Modified");
                         addTag("view_date",          tagViewDate,        "Date Last Viewed");

      typeToTag.keySet().forEach(type -> tagToObjType.put(typeToTag.get(type), type));

      for (RelationType relType : RelationType.values())
        if ((relType != rtUnited) && (relType != rtNone))
          relationSets.put(relType, RelationSet.createSet(relType));

      MainText.init(this);

      tagToObjType.put(tagAuthor, hdtPerson);
      tagToObjType.put(tagLargerDebate, hdtDebate);
      tagToObjType.put(tagLargerPosition, hdtPosition);
      tagToObjType.put(tagParentNote, hdtNote);
      tagToObjType.put(tagParentGlossary, hdtGlossary);
      tagToObjType.put(tagLinkedRecord, hdtAuxiliary);
      tagToObjType.put(tagKeyWork, hdtAuxiliary);
      tagToObjType.put(tagDisplayRecord, hdtAuxiliary);
      tagToObjType.put(tagLargerWork, hdtWork);
      tagToObjType.put(tagParentLabel, hdtWorkLabel);
      tagToObjType.put(tagParentGroup, hdtPersonGroup);
      tagToObjType.put(tagCounterargument, hdtArgument);
      tagToObjType.put(tagParentFolder, hdtFolder);
      tagToObjType.put(tagParentInst, hdtInstitution);

  /*****************************************************************************
  * ************************************************************************** *
  * *                                                                        * *
  * *                        Type Definitions                                * *
  * *                                                                        * *
  * ************************************************************************** *
  *****************************************************************************/

      // Nested items are defined in RelationSet.initTypes()

      addStringItem(hdtArgument, tagName);
      addPointerMulti(hdtArgument, rtWorkOfArgument, tagWork);
      addPointerMulti(hdtArgument, rtPositionOfArgument, tagPosition);
      addPointerMulti(hdtArgument, rtCounterOfArgument, tagCounterargument);
      addConnectorItem(hdtArgument, tagDescription, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtDebate, tagName);
      addPointerMulti(hdtDebate, rtParentDebateOfDebate, tagLargerDebate);
      addConnectorItem(hdtDebate, tagHub, tagDescription, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtMiscFile, tagName);
      addPointerSingle(hdtMiscFile, rtTypeOfFile, tagFileType);
      addPointerSingle(hdtMiscFile, rtWorkOfMiscFile, tagWork);
      addPointerMulti(hdtMiscFile, rtLabelOfFile, tagWorkLabel);
      addPathItem(hdtMiscFile, rtFolderOfMiscFile, tagFolder, tagFileName);
      addPointerMulti(hdtMiscFile, rtAuthorOfFile, tagAuthor);
      addBooleanItem(hdtMiscFile, tagAnnotated);
      addConnectorItem(hdtMiscFile, tagDescription, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtWorkFile, tagName);
      addPathItem(hdtWorkFile, rtFolderOfWorkFile, tagFolder, tagFileName);
      addBooleanItem(hdtWorkFile, tagAnnotated);

      addStringItem(hdtFolder, tagName);
      addPathItem(hdtFolder, rtParentFolderOfFolder, tagParentFolder, tagFileName);

      addStringItem(hdtInstitution, tagName);
      addPointerSingle(hdtInstitution, rtTypeOfInst, tagInstitutionType);
      addPointerSingle(hdtInstitution, rtParentInstOfInst, tagParentInst);
      addPointerSingle(hdtInstitution, rtStateOfInst, tagState);
      addPointerSingle(hdtInstitution, rtCountryOfInst, tagCountry);
      addStringItem(hdtInstitution, tagWebLink);
      addStringItem(hdtInstitution, tagCity);

      addStringItem(hdtInvestigation, tagName);
      addPointerSingle(hdtInvestigation, rtPersonOfInv, tagPerson);
      addConnectorItem(hdtInvestigation, tagDescription, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtNote, tagName);
      addPointerMulti(hdtNote, rtParentNoteOfNote, tagParentNote);
      addPointerSingle(hdtNote, rtFolderOfNote, tagFolder);
      addConnectorItem(hdtNote, tagHub, tagText, tagDisplayRecord, tagKeyWork);

      addPersonNameItem();
      addPointerSingle(hdtPerson, rtStatusOfPerson, tagPersonStatus);
      addPointerMulti(hdtPerson, rtInstOfPerson, tagInstitution);
      addPointerSingle(hdtPerson, rtRankOfPerson, tagRank);
      addPointerSingle(hdtPerson, rtFieldOfPerson, tagField);
      addPointerSingle(hdtPerson, rtSubfieldOfPerson, tagSubfield);
      addStringItem(hdtPerson, tagWebLink);
      addStringItem(hdtPerson, tagORCID);
      addPathItem(hdtPerson, rtNone, tagPicture);
      addStringItem(hdtPerson, tagPictureCrop);
      addConnectorItem(hdtPerson, tagWhyFamous, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtPersonGroup, tagName);
      addPointerMulti(hdtPersonGroup, rtParentGroupOfGroup, tagParentGroup);

      addStringItem(hdtPosition, tagName);
      addPointerMulti(hdtPosition, rtDebateOfPosition, tagDebate);
      addPointerMulti(hdtPosition, rtParentPosOfPos, tagLargerPosition);
      addConnectorItem(hdtPosition, tagHub, tagDescription, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtSubfield, tagName);
      addPointerSingle(hdtSubfield, rtFieldOfSubfield, tagField);

      addStringItem(hdtPositionVerdict, tagListName);
      addStringItem(hdtArgumentVerdict, tagListName);

      addStringItem(hdtTerm, tagTerm);
      addPointerMulti(hdtTerm, rtConceptOfTerm, tagConcept);

      addStringItem(hdtConcept, tagName);
      addPointerSingle(hdtConcept, rtGlossaryOfConcept, tagGlossary);
      addConnectorItem(hdtConcept, tagHub, tagDefinition, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtGlossary, tagName);
      addBooleanItem(hdtGlossary, tagActive);
      addPointerMulti(hdtGlossary, rtParentGlossaryOfGlossary, tagParentGlossary);

      addStringItem(hdtWork, tagTitle);
      addPointerSingle(hdtWork, rtTypeOfWork, tagWorkType);
      addPointerSingle(hdtWork, rtParentWorkOfWork, tagLargerWork);
      addPointerMulti(hdtWork, rtWorkFileOfWork, tagWorkFile);
      addAuthorsItem();
      addPointerMulti(hdtWork, rtInvestigationOfWork, tagInvestigation);
      addPointerMulti(hdtWork, rtLabelOfWork, tagWorkLabel);
      addStringItem(hdtWork, tagWebLink);
      addStringItem(hdtWork, tagYear);
      addBibEntryKeyItem();
      addStringItem(hdtWork, tagMiscBib);
      addStringItem(hdtWork, tagDOI);
      addStringItem(hdtWork, tagISBN);
      addConnectorItem(hdtWork, tagComments, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtWorkLabel, tagText);
      addPointerMulti(hdtWorkLabel, rtParentLabelOfLabel, tagParentLabel);
      addConnectorItem(hdtWorkLabel, tagHub, tagDescription, tagDisplayRecord, tagKeyWork);

      addStringItem(hdtHub, tagName);
      addHubSpokesItem();
      addConnectorItem(hdtHub, tagDescription, tagDisplayRecord, tagKeyWork);

      initialized = true;
    }
    catch (HDB_InternalError e)
    {
      messageDialog(e.getMessage(), mtError);
      unableToLoad = true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addItem(HDT_RecordType recordType, HyperDataCategory dataCat, RelationType relType, Tag... tags) throws HDB_InternalError
  {
    HDI_Schema schema;
    EnumSet<HDT_RecordType> types = tagToSubjType.get(tags[0]);

    if (types == null)
    {
      types = EnumSet.noneOf(HDT_RecordType.class);

      for (Tag tag : tags)
        tagToSubjType.put(tag, types);
    }

    if (types.contains(recordType))
    {
      schema = datasets.get(recordType).getSchema(tags[0]);
      if (schema.getCategory() != dataCat)
        throw new HDB_InternalError(78129);
    }
    else
    {
      switch (dataCat)
      {
        case hdcPointerMulti : case hdcPointerSingle : case hdcPath : case hdcAuthors :

          schema = new HDI_Schema(dataCat, relType, tags);

          if (relType != rtNone)
            relTypeToIsMulti.put(relType, Boolean.valueOf((dataCat == hdcPointerMulti) || (dataCat == hdcAuthors)));

          break;

        default : schema = new HDI_Schema(dataCat, tags); break;
      }

      types.add(recordType);
      datasets.get(recordType).addSchema(schema, tags);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  private void addTernaryItem    (HDT_RecordType type,                  Tag... tags) throws HDB_InternalError { addItem(type, hdcTernary      , rtNone, tags); }

  private void addBooleanItem    (HDT_RecordType type,                  Tag... tags) throws HDB_InternalError { addItem(type, hdcBoolean      , rtNone, tags); }
  private void addPointerMulti   (HDT_RecordType type, RelationType rt, Tag... tags) throws HDB_InternalError { addItem(type, hdcPointerMulti , rt    , tags); }
  private void addPointerSingle  (HDT_RecordType type, RelationType rt, Tag... tags) throws HDB_InternalError { addItem(type, hdcPointerSingle, rt    , tags); }
  private void addStringItem     (HDT_RecordType type,                  Tag... tags) throws HDB_InternalError { addItem(type, hdcString       , rtNone, tags); }
  private void addPathItem       (HDT_RecordType type, RelationType rt, Tag... tags) throws HDB_InternalError { addItem(type, hdcPath         , rt    , tags); }
  private void addConnectorItem  (HDT_RecordType type,                  Tag... tags) throws HDB_InternalError { addItem(type, hdcConnector    , rtNone, tags); }

  private void addBibEntryKeyItem() throws HDB_InternalError { addItem(hdtWork,   hdcBibEntryKey, rtNone,         tagBibEntryKey); }
  private void addAuthorsItem    () throws HDB_InternalError { addItem(hdtWork,   hdcAuthors,     rtAuthorOfWork, tagAuthor); }
  private void addPersonNameItem () throws HDB_InternalError { addItem(hdtPerson, hdcPersonName,  rtNone,         tagFirstName, tagLastName); }
  private void addHubSpokesItem  () throws HDB_InternalError { addItem(hdtHub,    hdcHubSpokes,   rtNone,         tagLinkedRecord); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final String

    LOCK_FILE_NAME = "dblock.dat",
    REQUEST_MSG_FILE_NAME = "request_message.dat",
    RESPONSE_MSG_FILE_NAME = "response_message.dat",

    BIB_FILE_NAME = "Bib.json",
    ZOTERO_TEMPLATE_FILE_NAME = "ZoteroTemplates.json",
    ZOTERO_CREATOR_TYPES_FILE_NAME = "ZoteroCreatorTypes.json",

    DEFAULT_PICTURES_PATH = "Pictures",
    DEFAULT_BOOKS_PATH = "Books",
    DEFAULT_PAPERS_PATH = "Papers",
    DEFAULT_UNENTERED_PATH = "Works not entered yet",
    DEFAULT_MISC_FILES_PATH = "Misc",
    DEFAULT_RESULTS_PATH = "Search results",
    DEFAULT_TOPICAL_PATH = "Topical";

  public static final int

    ROOT_FOLDER_ID = 1,
    PAPERS_FOLDER_ID = 2,
    BOOKS_FOLDER_ID = 3,
    MISC_FOLDER_ID = 4,
    PICTURES_FOLDER_ID = 5,
    TOPICAL_FOLDER_ID = 6,
    UNENTERED_FOLDER_ID = 7,
    RESULTS_FOLDER_ID = 8,
    FIRST_USER_FOLDER_ID = 9;

  private static final String

    OTHER_FILE_NAME = "Other.xml",
    PERSON_FILE_NAME = "People.xml",
    INSTITUTION_FILE_NAME = "Institutions.xml",
    INVESTIGATION_FILE_NAME = "Investigations.xml",
    DEBATE_FILE_NAME = "Debates.xml",
    ARGUMENT_FILE_NAME = "Arguments.xml",
    POSITION_FILE_NAME = "Positions.xml",
    WORK_FILE_NAME = "Works.xml",
    TERM_FILE_NAME = "Terms.xml",
    FILE_FILE_NAME = "Files.xml",
    NOTE_FILE_NAME = "Notes.xml",
    HUB_FILE_NAME = "Hubs.xml";

  public static enum Tag
  {
    tagNone,          tagPerson,       tagPersonStatus,  tagInstitution,     tagInstitutionType, tagState,        tagCountry,      tagRank,
    tagInvestigation, tagDebate,       tagArgument,      tagTerm,            tagConcept,         tagWork,         tagWorkType,     tagWorkLabel,
    tagField,         tagSubfield,     tagPosition,      tagPositionVerdict, tagArgumentVerdict, tagMiscFile,     tagWorkFile,     tagNote,
    tagGlossary,      tagPersonGroup,  tagFileType,      tagID,              tagType,            tagSortKey,      tagDOI,          tagISBN,
    tagSearchKey,     tagRecord,       tagFirstName,     tagLastName,        tagWebLink,         tagORCID,        tagPicture,      tagPictureCrop,
    tagWhyFamous,     tagName,         tagCity,          tagDescription,     tagTitle,           tagFileName,     tagYear,         tagMiscBib,
    tagAuthor,        tagInFileName,   tagEditor,        tagTranslator,      tagAnnotated,       tagStartPageNum, tagEndPageNum,   tagBibEntryKey,
    tagComments,      tagLargerDebate, tagListName,      tagCounterargument, tagDefinition,      tagText,         tagActive,       tagLargerPosition,
    tagParentNote,    tagFolder,       tagLargerWork,    tagParentLabel,     tagParentGlossary,  tagParentGroup,  tagParentFolder, tagCreationDate,
    tagModifiedDate,  tagViewDate,     tagDisplayRecord, tagKeyWork,         tagLinkedRecord,    tagParentInst,   tagHub;

    static EnumHashBiMap<Tag, Integer> tagToNum = EnumHashBiMap.create(Tag.class);

    public int getNum()                    { return tagToNum.get(this); }
    public static Tag getTagByNum(int num) { return tagToNum.inverse().get(num); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getTypeName(HDT_RecordType type)
  {
    Tag tag = typeToTag.get(type);
    if (nonNull(tag))
      return tagToHeader.get(tag);

    switch (type)
    {
      case hdtNone : return "All";
      default: return "Unknown";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isProtectedFile(FilePath filePath)
  {
    if (filePath.equals(rootFilePath)) return true;
    if (filePath.getParent().equals(rootFilePath) == false) return false;

    HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath, false);

    if (nonNull(folder))
      if (folder.getPath().getFilePath().equals(filePath))
        if (isProtectedRecord(folder.getID(), folder.getType()))
          return true;

    if (filePath.equals(prefsFilePath) ||
        filePath.equals(getRequestMessageFilePath()) ||
        filePath.equals(getResponseMessageFilePath()) ||
        filePath.equals(getLockFilePath()) ||
        filePath.equals(rootFilePath.resolve(new FilePath(PERSON_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(PERSON_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(OTHER_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(INSTITUTION_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(INVESTIGATION_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(DEBATE_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(ARGUMENT_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(POSITION_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(WORK_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(TERM_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(FILE_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(NOTE_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(HUB_FILE_NAME))) ||
        filePath.equals(rootFilePath.resolve(new FilePath(BIB_FILE_NAME))))
      return true;

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean getRelatives(HDT_Base record, Set<HDT_Base> set, int max)
  {
    set.clear();
    boolean hasMore = false;

    for (RelationType relType : getRelationsForSubjType(record.getType()))
    {
      switch (relType)
      {
        case rtParentFolderOfFolder: break;
        default:
          HyperObjList<HDT_Base, HDT_Base> list = getObjectList(relType, record, false);

          if ((max > 0) && (list.size() > max))
          {
            for (int ndx = 0; ndx < max; ndx++)
              set.add(list.get(ndx));

            hasMore = true;
          }
          else
            set.addAll(getObjectList(relType, record, false));
      }
    }

    for (RelationType relType : getRelationsForObjType(record.getType()))
    {
      switch (relType)
      {
        case rtParentFolderOfFolder: break;
        default:

          HyperSubjList<HDT_Base, HDT_Base> list = getSubjectList(relType, record);
          if ((max > 0) && (list.size() > max))
          {
            for (int ndx = 0; ndx < max; ndx++)
              set.add(list.get(ndx));

            hasMore = true;
          }
          else
            set.addAll(getSubjectList(relType, record));
      }
    }

    return hasMore;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void handleBibEntryKeyAssocation(String key, HDT_Work work, boolean affirm)
  {
    if (affirm)
      bibEntryKeyToWork.put(key, work);
    else
      bibEntryKeyToWork.remove(key);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void handleKeyWork(HDT_RecordWithConnector record, HDT_RecordWithPath keyWorkRecord, boolean affirm)
  {
    if (record.getType() != hdtWorkLabel)
    {
      Set<HDT_RecordWithConnector> set = keyWorkIndex.get(keyWorkRecord);

      if (affirm)
      {
        if (isNull(set))
        {
          set = new HashSet<>();
          keyWorkIndex.put(keyWorkRecord, set);
        }

        set.add(record);
      }
      else if (nonNull(set))
        set.remove(record);
    }

    RelationChangeHandler handler = keyWorkHandlers.get(record.getType());

    if (handler == null) return;

    runInFXThread(() -> handler.handle(keyWorkRecord, record, affirm));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void handleDisplayRecord(MainText displayer, MainText displayed, boolean affirm)
  {
    if (affirm)
      displayedAtIndex.addForward(displayed, displayer);
    else
      displayedAtIndex.removeForward(displayed, displayer);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<MainText> getDisplayers(MainText displayed)
  {
    Set<MainText> set = displayedAtIndex.getForwardSet(displayed);

    set.removeIf(mainText ->
    {
      HDT_RecordWithConnector record = mainText.getRecord();
      return HDT_Record.isEmpty(record) || (record.getMainText() != mainText);
    });

    return unmodifiableSet(set);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void replaceMainText(MainText oldMT, MainText newMT)
  {
    displayedAtIndex.replaceItem(oldMT, newMT);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDT_RecordWithConnector> getKeyWorkMentioners(HDT_RecordWithPath record)
  {
    if (keyWorkIndex.get(record) == null) return null;

    HashSet<HDT_RecordWithConnector> set = new HashSet<>();

    keyWorkIndex.get(record).forEach(mentioner ->
    {
      boolean added = false;

      if (mentioner.isLinked())
      {
        set.add(mentioner.getHub());
        added = true;
      }

      if (added == false)
        set.add(mentioner);
    });

    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void unmapFilePath(FilePath filePath)
  {
    String name = filePath.getNameOnly().toString();
    Set<HyperPath> paths = filenameMap.get(name);

    if (paths == null) return;

    paths.removeIf(path -> path.getFilePath().equals(filePath));

    if (paths.isEmpty())
      filenameMap.remove(name);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}