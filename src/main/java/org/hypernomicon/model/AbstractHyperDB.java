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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HDI_Schema.HyperDataCategory.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.relations.RelationSet.*;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.security.*;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.prefs.*;
import java.util.stream.Stream;

import static java.util.stream.Collectors.*;

import javax.xml.stream.*;
import javax.xml.stream.events.*;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.compare.ComparableUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;

import org.json.simple.parser.ParseException;

import com.google.common.collect.*;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.concurrent.Worker.State;

import org.hypernomicon.FolderTreeWatcher;
import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.*;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.auth.BibAuthKeys;
import org.hypernomicon.bib.mendeley.auth.MendeleyAuthKeys;
import org.hypernomicon.bib.mendeley.MendeleyWrapper;
import org.hypernomicon.bib.zotero.auth.ZoteroAuthKeys;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HDI_Schema.HyperDataCategory;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.data.HyperDataset;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.*;
import org.hypernomicon.model.unities.*;
import org.hypernomicon.util.*;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.prefs.XmlSupport;
import org.hypernomicon.view.HyperFavorites;

//---------------------------------------------------------------------------

public abstract class AbstractHyperDB
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract FilePath getRequestMessageFilePath(boolean useAppPrefs);
  public abstract FilePath getResponseMessageFilePath(boolean useAppPrefs);
  public abstract FilePath getLockFilePath(boolean useAppPrefs);

  /**
   * Retrieves the owner of the lock from the lock file.
   * <p>
   * This method attempts to read the first line of the lock file, which is
   * presumed to contain the owner's information. If the lock file does not
   * exist or an IOException occurs during the reading process, it returns
   * "[Unknown]". If the lock file is empty, it returns {@code null}.
   * </p>
   *
   * @return The owner of the lock if available; "[Unknown]" if an IOException occurs;
   *         {@code null} if the file does not exist or is empty.
   */
  public abstract String getLockOwner();

  /**
   * Retrieves the owner of the lock from the lock file, provided it's not the current computer.
   * <p>
   * This method first calls {@link #getLockOwner()} to obtain the lock owner's name.
   * If the owner is not {@code null} and it is not the current computer, it returns the owner's name.
   * If the owner is {@code null} or the current computer, it deletes the lock file and message files.
   * </p>
   *
   * @return The lock owner's name if it is not the current computer; {@code null} otherwise.
   */
  public abstract String getOtherLockOwner();
  public abstract FilePath extPath();
  public abstract void updateMainTextTemplate(RecordType recordType, String html) throws IOException;
  public abstract String getMainTextTemplate(RecordType recordType);

  protected abstract void lock() throws IOException;
  protected abstract void unlock();

  abstract MentionsIndex createMentionsIndex(List<Runnable> completeHandlers);

  /**
   * Updates the temp file with information about running instances of Hypernomicon
   * on this computer
   * @param newRootFilePath Currently open database folder for this instance
   */
  protected abstract void updateRunningInstancesFile(FilePath newRootFilePath);

  protected abstract void saveSourcePathToSystemSettings(String newPathStr);
  protected abstract FolderTreeWatcher getFolderTreeWatcher();

  /**
   * Traverse through all folder records and show a popup warning for any
   * folder that no longer exists on the file system.<br>
   * This is intended to be called during database load.
   */
  protected abstract void checkWhetherFoldersExist();
  protected abstract void loadMainTextTemplates();
  protected abstract void warningMessage(String msg);
  protected abstract void errorMessage(String msg);

  protected abstract void loadSettings(boolean creatingNew, HyperFavorites favorites) throws HyperDataException;
  protected abstract boolean loadFromXMLFiles(List<FilePath> xmlFileList, boolean creatingNew, EnumMap<RecordType, VersionNumber> recordTypeToDataVersion, SetMultimap<Integer, Integer> workIDtoInvIDs);
  protected abstract boolean bringAllRecordsOnline();
  protected abstract boolean checkChecksums();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface
  public interface ChangeRecordIDHandler { void changeRecordID(HDT_Record record, int newID); }

  private final EnumMap<RecordType, HyperDataset<? extends HDT_Record>> datasets = new EnumMap<>(RecordType.class);
  private final EnumMap<RecordType, DatasetAccessor<? extends HDT_Record>> accessors = new EnumMap<>(RecordType.class);

  private final EnumMap<RelationType, RelationSet<? extends HDT_Record, ? extends HDT_Record>> relationSets = new EnumMap<>(RelationType.class);
  private final EnumMap<RelationType, Boolean> relTypeToIsMulti = new EnumMap<>(RelationType.class);
  private final EnumMap<Tag, EnumSet<RecordType>> tagToSubjType = new EnumMap<>(Tag.class);

  private final List<Consumer<HDT_Record>> recordDeleteHandlers          = new ArrayList<>();
  private final List<Runnable>             dbCloseHandlers               = new ArrayList<>(),
                                           dbLoadedHandlers              = new ArrayList<>(),
                                           dbPreChangeHandlers           = new ArrayList<>(),
                                           dbMentionsNdxCompleteHandlers = new ArrayList<>(),
                                           bibChangedHandlers            = new ArrayList<>();

  private final List<ChangeRecordIDHandler> changeRecordIDHandlers = new ArrayList<>();

  protected final EnumMap<RecordType, String> mainTextTemplates = new EnumMap<>(RecordType.class);
  protected final Map<String, String> xmlChecksums = new HashMap<>();

  private final SearchKeys searchKeys = new SearchKeys();
  private final MentionsIndex mentionsIndex = createMentionsIndex(dbMentionsNdxCompleteHandlers);
  private final Map<HDT_RecordWithAuthors<? extends Authors>, Set<HDT_RecordWithMainText>> keyWorkIndex = new HashMap<>();
  private final BidiOneToManyMap<MainText> displayedAtIndex = new BidiOneToManyMap<>();
  private final Map<String, HDT_Work> bibEntryKeyToWork = new HashMap<>();
  private final List<HDT_Record> initialNavList = new ArrayList<>();
  private final EnumMap<RecordType, RelationChangeHandler> keyWorkHandlers = new EnumMap<>(RecordType.class);

  public final FilenameMap<Set<HyperPath>> filenameMap = new FilenameMap<>();

  public Preferences prefs = null;
  private LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> bibLibrary = null;

  private Instant dbCreationDate;

  protected DialogResult deleteFileAnswer;

  protected FilePath rootFilePath;
  private FilePath hdbFilePath;

  private boolean loaded       = false, pointerResolutionInProgress     = false, deletionInProgress      = false, resolveAgain = false,
                  initialized  = false, startMentionsRebuildAfterDelete = false, alreadyShowedUpgradeMsg = false;

  public boolean runningConversion     = false, // suppresses "modified date" updating
                 viewTestingInProgress = false; // suppresses "view date" updating

//---------------------------------------------------------------------------
  @FunctionalInterface public interface RelationChangeHandler { void handle(HDT_Record subject, HDT_Record object, boolean affirm); }
//---------------------------------------------------------------------------

  public boolean isDeletionInProgress()                             { return deletionInProgress; }
  public boolean resolvingPointers()                                { return pointerResolutionInProgress; }
  public int getNextID(RecordType type)                             { return datasets.get(type).getNextID(); }
  public boolean idAvailable(RecordType type, int id)               { return datasets.get(type).idAvailable(id); }
  public Tag mainTextTagForRecordType(RecordType type)              { return nullSwitch(datasets.get(type), null, HyperDataset::getMainTextTag); }
  public boolean isLoaded()                                         { return loaded; }
  public long totalRecordCount()                                    { return accessors.values().stream().mapToLong(Collection::size).sum(); }
  public boolean bibLibraryIsLinked()                               { return bibLibrary != null; }
  public String bibLibraryUserFriendlyName()                        { return bibLibraryIsLinked() ? bibLibrary.getUserFriendlyName() : ""; }
  public Instant getCreationDate()                                  { return dbCreationDate; }
  public RecordType getSubjType(RelationType relType)               { return relationSets.get(relType).getSubjType(); }
  public RecordType getObjType(RelationType relType)                { return relationSets.get(relType).getObjType(); }
  public boolean relationIsMulti(RelationType relType)              { return relTypeToIsMulti.get(relType); }
  public List<HDT_Record> initialNavHistory()                       { return Collections.unmodifiableList(initialNavList); }
  public String getSearchKey(HDT_Record record)                     { return searchKeys.getStringForRecord(record); }
  public SearchKeyword getKeyByKeyword(String keyword)              { return searchKeys.getKeywordObjByKeywordStr(keyword); }
  public String firstActiveKeyWord(HDT_Record record)               { return searchKeys.firstActiveKeyword(record); }
  public Iterable<SearchKeyword> getKeysByPrefix(String prefix)     { return searchKeys.getKeywordsByPrefix(prefix); }
  public Iterable<SearchKeyword> getKeysByRecord(HDT_Record record) { return searchKeys.getKeysByRecord(record); }
  public HDT_Work getWorkByBibEntryKey(String key)                  { return bibEntryKeyToWork.get(key); }
  public boolean reindexingMentioners()                             { return mentionsIndex.isRebuilding(); }
  public BibEntry<?, ?> getBibEntryByKey(String key)                { return bibLibrary.getEntryByKey(key); }

  public void setSearchKey(HDT_Record record, String newKey, boolean noMod, boolean rebuildMentions) throws DuplicateSearchKeyException, SearchKeyTooShortException
  { searchKeys.setSearchKey(record, newKey, noMod, rebuildMentions); }

  public LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> getBibLibrary()  { return bibLibrary; }
  public Stream<Consumer<HDT_Record>> getRecordDeleteHandlers()                             { return recordDeleteHandlers.stream(); }
  public void addRelationChangeHandler(RelationType relType, RelationChangeHandler handler) { relationSets.get(relType).addChangeHandler(handler); }
  public void addKeyWorkHandler(RecordType recordType, RelationChangeHandler handler)       { keyWorkHandlers.put(recordType, handler); }
  public void addCloseDBHandler(Runnable handler)                                           { dbCloseHandlers.add(handler); }
  public void addPreDBChangeHandler(Runnable handler)                                       { dbPreChangeHandlers.add(handler); }
  public void addDBLoadedHandler(Runnable handler)                                          { dbLoadedHandlers.add(handler); }
  public void addMentionsNdxCompleteHandler(Runnable handler)                               { dbMentionsNdxCompleteHandlers.add(handler); }
  public void addBibChangedHandler(Runnable handler)                                        { bibChangedHandlers.add(handler); }
  public void addDeleteHandler(Consumer<HDT_Record> handler)                                { recordDeleteHandlers.add(handler); }
  public void addChangeRecordIDHandler(ChangeRecordIDHandler handler)                       { changeRecordIDHandlers.add(handler); }

  /**
   * This updates the displayedAt index when the MainText object for a record is being replaced.
   * This does not assume that the record's MainText reference has been changed yet, and does
   * not change the record's MainText reference; the caller usually does that right after calling
   * this function.
   * @param oldMT The previous MainText object the record was pointing to
   * @param newMT The new MainText object the record will be pointing to
   */
  public void replaceMainText(MainText oldMT, MainText newMT)                               { displayedAtIndex.replaceItem(oldMT, newMT); }

  public void rebuildMentions()                                                             { if (loaded) mentionsIndex.startRebuild(); }
  public void updateMentioner(HDT_Record record)                                            { if (loaded) mentionsIndex.updateMentioner(record); }
  public boolean waitUntilRebuildIsDone()                                                   { return mentionsIndex.waitUntilRebuildIsDone(); }

  public boolean firstMentionsSecond(HDT_Record mentioner, HDT_Record target, boolean descOnly, MutableBoolean choseNotToWait) {
    return mentionsIndex.firstMentionsSecond(mentioner, target, descOnly, choseNotToWait); }
  public Set<HDT_Record> getMentionerSet(HDT_Record target, boolean descOnly, MutableBoolean choseNotToWait) {
    return mentionsIndex.getMentionerSet(target, descOnly, choseNotToWait); }
  public Set<HDT_Record> getMentionerSet(HDT_Record target, boolean descOnly) {
    return mentionsIndex.getMentionerSet(target, descOnly); }

//---------------------------------------------------------------------------

  public String     getNestedString (HDT_Record subj, HDT_Record obj, Tag tag) { return relSet(subj, obj).getNestedString (subj, obj, tag); }
  public boolean    getNestedBoolean(HDT_Record subj, HDT_Record obj, Tag tag) { return relSet(subj, obj).getNestedBoolean(subj, obj, tag); }
  public Ternary    getNestedTernary(HDT_Record subj, HDT_Record obj, Tag tag) { return relSet(subj, obj).getNestedTernary(subj, obj, tag); }
  public HDT_Record getNestedPointer(HDT_Record subj, HDT_Record obj, Tag tag) { return relSet(subj, obj).getNestedPointer(subj, obj, tag); }
  public boolean    relationHasNestedValues(RelationType relType)              { return relationSets.get(relType).getHasNestedItems(); }
  public HDI_Schema getNestedSchema(RelationType relType, Tag tag)             { return relationSets.get(relType).getSchema(tag); }
  public Set<Tag>   getNestedTags(RelationType relType)                        { return relationSets.get(relType).getNestedTags(); }

  @SuppressWarnings("unchecked")
  private <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> RelationSet<HDT_SubjType, HDT_ObjType> relSet(HDT_SubjType subj, HDT_ObjType obj)
  { return (RelationSet<HDT_SubjType, HDT_ObjType>) relationSets.get(getRelation(subj.getType(), obj.getType(), false)); }

  @SuppressWarnings("unchecked")
  private <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> RelationSet<HDT_SubjType, HDT_ObjType> relSet(RelationType relType)
  { return (RelationSet<HDT_SubjType, HDT_ObjType>) relationSets.get(relType); }

  public void setNestedItemFromOfflineValue(HDT_Record subj, HDT_Record obj, Tag tag, HDI_OfflineBase value) throws RelationCycleException, HDB_InternalError
  { relSet(subj, obj).setNestedItemFromOfflineValue(subj, obj, tag, value); }

  public void saveNestedValuesToOfflineMap(HDT_Record subj, HDT_Record obj, Map<Tag, HDI_OfflineBase> tagToNestedItem, RecordState recordState)
  { relSet(subj, obj).saveNestedValuesToOfflineMap(subj, obj, tagToNestedItem, recordState); }

  public <HDT_ObjType extends HDT_Record, HDT_SubjType extends HDT_Record> HyperObjList<HDT_SubjType, HDT_ObjType> getObjectList(RelationType relType, HDT_SubjType subj, boolean modTracking)
  { return new HyperObjList<>(relSet(relType), subj, modTracking); }

  public <HDT_ObjType extends HDT_Record, HDT_SubjType extends HDT_Record> HyperSubjList<HDT_SubjType, HDT_ObjType> getSubjectList(RelationType relType, HDT_ObjType obj)
  { return new HyperSubjList<>(relSet(relType), obj); }

  public <HDT_ObjType extends HDT_Record, HDT_SubjType extends HDT_Record> HyperObjPointer<HDT_SubjType, HDT_ObjType> getObjPointer(RelationType relType, HDT_SubjType subj)
  { return new HyperObjPointer<>(relSet(relType), subj, true); }

  public <HDT_ObjType extends HDT_Record, HDT_SubjType extends HDT_Record> HyperSubjPointer<HDT_SubjType, HDT_ObjType> getSubjPointer(RelationType relType, HDT_ObjType obj)
  { return new HyperSubjPointer<>(relSet(relType), obj); }

  @SuppressWarnings("unchecked")
  public <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> List<ObjectGroup> getObjectGroupList(RelationType relType, HDT_SubjType subj, Collection<Tag> tags)
  { return ((RelationSet<HDT_SubjType, HDT_ObjType>)relationSets.get(relType)).getObjectGroupList(subj, tags); }

  @SuppressWarnings("unchecked")
  public <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> void updateObjectGroups(RelationType relType, HDT_SubjType subj, List<ObjectGroup> groups) throws RelationCycleException
  { ((RelationSet<HDT_SubjType, HDT_ObjType>)relationSets.get(relType)).updateObjectGroups(subj, groups); subj.modifyNow(); }

  public void updateNestedString(HDT_Record subj, HDT_Record obj, Tag tag, String str)
  { if (relSet(subj, obj).setNestedString(subj, obj, tag, str)) subj.modifyNow(); }

  public void updateNestedBoolean(HDT_Record subj, HDT_Record obj, Tag tag, boolean bool)
  { if (relSet(subj, obj).setNestedBoolean(subj, obj, tag, bool)) subj.modifyNow(); }

  public void updateNestedTernary(HDT_Record subj, HDT_Record obj, Tag tag, Ternary ternary)
  { if (relSet(subj, obj).setNestedTernary(subj, obj, tag, ternary)) subj.modifyNow(); }

  public void updateNestedPointer(HDT_Record subj, HDT_Record obj, Tag tag, HDT_Record target)
  { if (relSet(subj, obj).setNestedPointer(subj, obj, tag, target)) subj.modifyNow(); }

  @SuppressWarnings("unchecked")
  public <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> void resolvePointersByRelation(RelationType relType, HDT_SubjType subj) throws HDB_InternalError
  { ((RelationSet<HDT_SubjType, HDT_ObjType>)relationSets.get(relType)).resolvePointers(subj); }

  private HDT_Folder xmlFolder, booksFolder, papersFolder, miscFilesFolder, picturesFolder, resultsFolder, unenteredFolder, topicalFolder;

  public HDT_Folder getRootFolder     () { return folders.getByID(ROOT_FOLDER_ID); }
  public HDT_Folder getXmlFolder      () { return xmlFolder      ; }
  public HDT_Folder getBooksFolder    () { return booksFolder    ; }
  public HDT_Folder getPapersFolder   () { return papersFolder   ; }
  public HDT_Folder getMiscFilesFolder() { return miscFilesFolder; }
  public HDT_Folder getPicturesFolder () { return picturesFolder ; }
  public HDT_Folder getResultsFolder  () { return resultsFolder  ; }
  public HDT_Folder getUnenteredFolder() { return unenteredFolder; }
  public HDT_Folder getTopicalFolder  () { return topicalFolder  ; }

  public HDT_Folder getSpecialFolder(String prefKey)
  {
    return switch (prefKey)
    {
      case FolderIDPrefKey.PICTURES   -> picturesFolder;
      case FolderIDPrefKey.BOOKS      -> booksFolder;
      case FolderIDPrefKey.PAPERS     -> papersFolder;
      case FolderIDPrefKey.RESULTS    -> resultsFolder;
      case FolderIDPrefKey.UNENTERED  -> unenteredFolder;
      case FolderIDPrefKey.MISC_FILES -> miscFilesFolder;
      case FolderIDPrefKey.TOPICAL    -> topicalFolder;
      case FolderIDPrefKey.XML        -> xmlFolder;
      default                         -> null;
    };
  }

  public FilePath getHdbPath   () { return hdbFilePath; }
  public FilePath getRootPath  () { return rootFilePath; }
  public FilePath xmlPath      () { return rootFilePath.resolve(DEFAULT_XML_PATH); }

  public FilePath booksPath    () { return booksFolder    .filePath(); }
  public FilePath papersPath   () { return papersFolder   .filePath(); }
  public FilePath miscFilesPath() { return miscFilesFolder.filePath(); }
  public FilePath picturesPath () { return picturesFolder .filePath(); }
  public FilePath resultsPath  () { return resultsFolder  .filePath(); }
  public FilePath unenteredPath() { return unenteredFolder.filePath(); }
  public FilePath topicalPath  () { return topicalFolder  .filePath(); }

  public FilePath getRootPath  (String fileNameStr) { return rootFilePath.resolve(fileNameStr); }
  public FilePath xmlPath      (String fileNameStr) { return rootFilePath.resolve(DEFAULT_XML_PATH).resolve(fileNameStr); }

  public FilePath booksPath    (String fileNameStr) { return booksFolder    .filePath().resolve(fileNameStr); }
  public FilePath papersPath   (String fileNameStr) { return papersFolder   .filePath().resolve(fileNameStr); }
  public FilePath miscFilesPath(String fileNameStr) { return miscFilesFolder.filePath().resolve(fileNameStr); }
  public FilePath picturesPath (String fileNameStr) { return picturesFolder .filePath().resolve(fileNameStr); }
  public FilePath resultsPath  (String fileNameStr) { return resultsFolder  .filePath().resolve(fileNameStr); }
  public FilePath unenteredPath(String fileNameStr) { return unenteredFolder.filePath().resolve(fileNameStr); }
  public FilePath topicalPath  (String fileNameStr) { return topicalFolder  .filePath().resolve(fileNameStr); }

//---------------------------------------------------------------------------

  public final DatasetAccessor<HDT_Person         > persons;
  public final DatasetAccessor<HDT_PersonStatus   > personStatuses;
  public final DatasetAccessor<HDT_Institution    > institutions;
  public final DatasetAccessor<HDT_InstitutionType> institutionTypes;
  public final DatasetAccessor<HDT_Region         > regions;
  public final DatasetAccessor<HDT_Country        > countries;
  public final DatasetAccessor<HDT_Rank           > ranks;
  public final DatasetAccessor<HDT_Investigation  > investigations;
  public final DatasetAccessor<HDT_Debate         > debates;
  public final DatasetAccessor<HDT_Argument       > arguments;
  public final DatasetAccessor<HDT_Position       > positions;
  public final DatasetAccessor<HDT_Field          > fields;
  public final DatasetAccessor<HDT_Subfield       > subfields;
  public final DatasetAccessor<HDT_Term           > terms;
  public final DatasetAccessor<HDT_Concept        > concepts;
  public final DatasetAccessor<HDT_ConceptSense   > conceptSenses;
  public final DatasetAccessor<HDT_Work           > works;
  public final DatasetAccessor<HDT_WorkType       > workTypes;
  public final DatasetAccessor<HDT_WorkLabel      > workLabels;
  public final DatasetAccessor<HDT_PositionVerdict> positionVerdicts;
  public final DatasetAccessor<HDT_ArgumentVerdict> argumentVerdicts;
  public final DatasetAccessor<HDT_MiscFile       > miscFiles;
  public final DatasetAccessor<HDT_WorkFile       > workFiles;
  public final DatasetAccessor<HDT_Folder         > folders;
  public final DatasetAccessor<HDT_Note           > notes;
  public final DatasetAccessor<HDT_Glossary       > glossaries;
  public final DatasetAccessor<HDT_Hub            > hubs;
  public final DatasetAccessor<HDT_PersonGroup    > personGroups;
  public final DatasetAccessor<HDT_FileType       > fileTypes;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected AbstractHyperDB()
  {
    persons          = getAccessor(HDT_Person         .class);
    personStatuses   = getAccessor(HDT_PersonStatus   .class);
    institutions     = getAccessor(HDT_Institution    .class);
    institutionTypes = getAccessor(HDT_InstitutionType.class);
    regions          = getAccessor(HDT_Region         .class);
    countries        = getAccessor(HDT_Country        .class);
    ranks            = getAccessor(HDT_Rank           .class);
    investigations   = getAccessor(HDT_Investigation  .class);
    debates          = getAccessor(HDT_Debate         .class);
    arguments        = getAccessor(HDT_Argument       .class);
    terms            = getAccessor(HDT_Term           .class);
    concepts         = getAccessor(HDT_Concept        .class);
    conceptSenses    = getAccessor(HDT_ConceptSense   .class);
    works            = getAccessor(HDT_Work           .class);
    workTypes        = getAccessor(HDT_WorkType       .class);
    workLabels       = getAccessor(HDT_WorkLabel      .class);
    fields           = getAccessor(HDT_Field          .class);
    subfields        = getAccessor(HDT_Subfield       .class);
    positions        = getAccessor(HDT_Position       .class);
    positionVerdicts = getAccessor(HDT_PositionVerdict.class);
    argumentVerdicts = getAccessor(HDT_ArgumentVerdict.class);
    miscFiles        = getAccessor(HDT_MiscFile       .class);
    workFiles        = getAccessor(HDT_WorkFile       .class);
    folders          = getAccessor(HDT_Folder         .class);
    notes            = getAccessor(HDT_Note           .class);
    glossaries       = getAccessor(HDT_Glossary       .class);
    hubs             = getAccessor(HDT_Hub            .class);
    personGroups     = getAccessor(HDT_PersonGroup    .class);
    fileTypes        = getAccessor(HDT_FileType       .class);

    RelationSet.init(relationSets);

  /*****************************************************************************
  * ************************************************************************** *
  * *                                                                        * *
  * *                        Record Type Definitions                         * *
  * *                                                                        * *
  * ************************************************************************** *
  *****************************************************************************/

    // Nested items are defined in RelationSet.createSet()

    addStringItem  (hdtArgument                      , tagName);
    addPointerMulti(hdtArgument, rtWorkOfArgument    , tagWork);
    addPointerMulti(hdtArgument, rtPositionOfArgument, tagPosition);
    addPointerMulti(hdtArgument, rtTargetArgOfArg    , tagTargetArgument);
    addMainTextItem(hdtArgument                      , tagDescription, tagDisplayRecord, tagKeyWork);

    addStringItem  (hdtDebate                        , tagName);
    addPointerMulti(hdtDebate, rtParentDebateOfDebate, tagLargerDebate);
    addPointerMulti(hdtDebate, rtParentPosOfDebate   , tagLargerPosition);
    addMainTextItem(hdtDebate                        , tagHub, tagDescription, tagDisplayRecord, tagKeyWork);

    addStringItem   (hdtMiscFile                    , tagName);
    addPointerSingle(hdtMiscFile, rtTypeOfFile      , tagFileType);
    addPointerSingle(hdtMiscFile, rtWorkOfMiscFile  , tagWork);
    addPathItem     (hdtMiscFile, rtFolderOfMiscFile, tagFolder, tagFileName);
    addAuthorsItem  (hdtMiscFile, rtAuthorOfFile);
    addBooleanItem  (hdtMiscFile                    , tagAnnotated);
    addMainTextItem (hdtMiscFile                    , tagDescription, tagDisplayRecord, tagKeyWork);

    addStringItem (hdtWorkFile                    , tagName);
    addPathItem   (hdtWorkFile, rtFolderOfWorkFile, tagFolder, tagFileName);
    addBooleanItem(hdtWorkFile                    , tagAnnotated);

    addStringItem(hdtFolder                        , tagName);
    addPathItem  (hdtFolder, rtParentFolderOfFolder, tagParentFolder, tagFileName);

    addStringItem   (hdtInstitution                    , tagName);
    addPointerSingle(hdtInstitution, rtTypeOfInst      , tagInstitutionType);
    addPointerSingle(hdtInstitution, rtParentInstOfInst, tagParentInst);
    addPointerSingle(hdtInstitution, rtRegionOfInst    , tagRegion);
    addPointerSingle(hdtInstitution, rtCountryOfInst   , tagCountry);
    addStringItem   (hdtInstitution                    , tagWebURL);
    addStringItem   (hdtInstitution                    , tagCity);

    addStringItem   (hdtInvestigation               , tagName);
    addPointerSingle(hdtInvestigation, rtPersonOfInv, tagPerson);
    addMainTextItem (hdtInvestigation               , tagDescription, tagDisplayRecord, tagKeyWork);

    addStringItem   (hdtNote                    , tagName);
    addPointerMulti (hdtNote, rtParentNoteOfNote, tagParentNote);
    addPointerSingle(hdtNote, rtFolderOfNote    , tagFolder);
    addMainTextItem (hdtNote                    , tagHub, tagText, tagDisplayRecord, tagKeyWork);

    addPersonNameItem();
    addPointerSingle (hdtPerson, rtStatusOfPerson       , tagPersonStatus);
    addPointerMulti  (hdtPerson, rtInstOfPerson         , tagInstitution);
    addPointerSingle (hdtPerson, rtRankOfPerson         , tagRank);
    addPointerSingle (hdtPerson, rtFieldOfPerson        , tagField);
    addPointerSingle (hdtPerson, rtSubfieldOfPerson     , tagSubfield);
    addStringItem    (hdtPerson                         , tagWebURL);
    addStringItem    (hdtPerson                         , tagORCID);
    addPathItem      (hdtPerson, rtPictureFolderOfPerson, tagPictureFolder, tagPicture);
    addStringItem    (hdtPerson                         , tagPictureCrop);
    addMainTextItem  (hdtPerson                         , tagWhyFamous, tagDisplayRecord, tagKeyWork);

    addStringItem  (hdtPersonGroup                      , tagName);
    addPointerMulti(hdtPersonGroup, rtParentGroupOfGroup, tagParentGroup);

    addStringItem  (hdtPosition                     , tagName);
    addPointerMulti(hdtPosition, rtParentDebateOfPos, tagDebate);
    addPointerMulti(hdtPosition, rtParentPosOfPos   , tagLargerPosition);
    addMainTextItem(hdtPosition                     , tagHub, tagDescription, tagDisplayRecord, tagKeyWork);

    addStringItem   (hdtRegion                   , tagName);
    addStringItem   (hdtRegion                   , tagAbbreviation);
    addPointerSingle(hdtRegion, rtCountryOfRegion, tagCountry);

    addStringItem   (hdtSubfield                   , tagName);
    addPointerSingle(hdtSubfield, rtFieldOfSubfield, tagField);

    addStringItem(hdtPositionVerdict, tagListName);
    addStringItem(hdtArgumentVerdict, tagListName);

    addStringItem  (hdtTerm                 , tagTerm);
    addPointerMulti(hdtTerm, rtConceptOfTerm, tagConcept);

    addStringItem   (hdtConcept                          , tagName);
    addPointerSingle(hdtConcept, rtGlossaryOfConcept     , tagGlossary);
    addPointerSingle(hdtConcept, rtSenseOfConcept        , tagSense);
    addPointerMulti (hdtConcept, rtParentConceptOfConcept, tagParentConcept);
    addMainTextItem (hdtConcept                          , tagHub, tagDefinition, tagDisplayRecord, tagKeyWork);

    addStringItem  (hdtGlossary                            , tagName);
    addBooleanItem (hdtGlossary                            , tagActive);
    addPointerMulti(hdtGlossary, rtParentGlossaryOfGlossary, tagParentGlossary);

    addStringItem     (hdtWork                    , tagTitle);
    addPointerSingle  (hdtWork, rtTypeOfWork      , tagWorkType);
    addPointerSingle  (hdtWork, rtParentWorkOfWork, tagLargerWork);
    addPointerMulti   (hdtWork, rtWorkFileOfWork  , tagWorkFile);
    addAuthorsItem    (hdtWork, rtAuthorOfWork);
    addStringItem     (hdtWork                    , tagWebURL);
    addStringItem     (hdtWork                    , tagStartPageNum);  // These are only used for works with an external file path. Otherwise, nested page
    addStringItem     (hdtWork                    , tagEndPageNum);    // number items in rtWorkFileOfWork relation are used.
    addBibDateItem    (hdtWork                    , tagBibDate);
    addBibEntryKeyItem();
    addStringItem     (hdtWork                    , tagMiscBib);
    addStringItem     (hdtWork                    , tagDOI);
    addStringItem     (hdtWork                    , tagISBN);
    addMainTextItem   (hdtWork                    , tagComments, tagDisplayRecord, tagKeyWork);

    addStringItem  (hdtWorkLabel                      , tagText);
    addPointerMulti(hdtWorkLabel, rtParentLabelOfLabel, tagParentLabel);
    addMainTextItem(hdtWorkLabel                      , tagHub, tagDescription, tagDisplayRecord, tagKeyWork);

    addStringItem   (hdtHub, tagName);
    addHubSpokesItem();
    addMainTextItem (hdtHub, tagDescription, tagDisplayRecord, tagKeyWork);

    initialized = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public DatasetAccessor<? extends HDT_Record> records(RecordType type)
  {
    DatasetAccessor<? extends HDT_Record> accessor = accessors.get(type);

    if (accessor == null) throw new NoSuchElementException("Internal error: null dataset");

    return accessor;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean creatingDataset = false;

  public static boolean isCreatingDataset() { return creatingDataset; }

  private <HDT_T extends HDT_Record> DatasetAccessor<HDT_T> getAccessor(Class<HDT_T> klass)
  {
    RecordType type = typeByRecordClass(klass);

    creatingDataset = true;
    HyperDataset<HDT_T> dataset = HyperDataset.create(type);
    creatingDataset = false;

    DatasetAccessor<HDT_T> accessor = dataset.getAccessor();
    datasets.put(type, dataset);
    accessors.put(type, accessor);

    return accessor;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final boolean falseWithErrorMessage(String msg)
  {
    errorMessage(msg);
    return false;
  }

  protected final void errorMessage(Throwable e)
  {
    errorMessage(getThrowableMessage(e));
  }

  protected final void internalErrorMessage(int num)
  {
    errorMessage(new HDB_InternalError(num));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addItem(RecordType recordType, HyperDataCategory dataCat, RelationType relType, Tag... tags)
  {
    HDI_Schema schema;
    EnumSet<RecordType> types = tagToSubjType.get(tags[0]);

    if (types == null)
    {
      types = EnumSet.noneOf(RecordType.class);

      for (Tag tag : tags)
        tagToSubjType.put(tag, types);
    }

    if (types.contains(recordType))
    {
      schema = datasets.get(recordType).getSchema(tags[0]);
      assert(schema.category() == dataCat);
    }
    else
    {
      switch (dataCat)
      {
        case hdcPointerMulti : case hdcPointerSingle : case hdcPath : case hdcAuthors :

          schema = new HDI_Schema(dataCat, relType, tags);

          if (relType != rtNone)
            relTypeToIsMulti.put(relType, (dataCat == hdcPointerMulti) || (dataCat == hdcAuthors));

          break;

        default : schema = new HDI_Schema(dataCat, tags); break;
      }

      types.add(recordType);
      datasets.get(recordType).addSchema(schema);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  private void addTernaryItem  (RecordType type,                  Tag... tags) { addItem(type, hdcTernary       , rtNone, tags); }

  private void addBooleanItem  (RecordType type,                  Tag... tags) { addItem(type, hdcBoolean       , rtNone, tags); }
  private void addPointerMulti (RecordType type, RelationType rt, Tag... tags) { addItem(type, hdcPointerMulti  , rt    , tags); }
  private void addPointerSingle(RecordType type, RelationType rt, Tag... tags) { addItem(type, hdcPointerSingle , rt    , tags); }
  private void addStringItem   (RecordType type,                  Tag... tags) { addItem(type, hdcString        , rtNone, tags); }
  private void addBibDateItem  (RecordType type,                  Tag... tags) { addItem(type, hdcBibDate       , rtNone, tags); }
  private void addPathItem     (RecordType type, RelationType rt, Tag... tags) { addItem(type, hdcPath          , rt    , tags); }
  private void addMainTextItem (RecordType type,                  Tag... tags) { addItem(type, hdcMainTextAndHub, rtNone, tags); }
  private void addAuthorsItem  (RecordType type, RelationType rt             ) { addItem(type, hdcAuthors       , rt    , tagAuthor); }

  private void addBibEntryKeyItem() { addItem(hdtWork,   hdcBibEntryKey, rtNone, tagBibEntryKey           ); }

  private void addPersonNameItem () { addItem(hdtPerson, hdcPersonName,  rtNone, tagFirstName, tagLastName); }
  private void addHubSpokesItem  () { addItem(hdtHub,    hdcHubSpokes,   rtNone, tagLinkedRecord          ); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings({ "unused", "unchecked" })
  private <HDT_T extends HDT_Record> Set<HDT_T> getOrphans(RelationType relType, Class<HDT_T> klazz)
  {
    return (Set<HDT_T>) relationSets.get(relType).getOrphans();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void attachOrphansToRoots()
  {
    Set<HDT_Position> posOrphans = getOrphans(rtParentPosOfPos, HDT_Position.class);

    getOrphans(rtParentDebateOfPos, HDT_Position.class).forEach(position ->
    {
      if (posOrphans.contains(position))
        position.largerDebates.add(debates.getByID(1));
    });

    Set<HDT_Debate> debateOrphans = getOrphans(rtParentDebateOfDebate, HDT_Debate.class);

    getOrphans(rtParentPosOfDebate, HDT_Debate.class).forEach(debate ->
    {
      if (debateOrphans.contains(debate))
        debate.largerDebates.add(debates.getByID(1));
    });

    getOrphans(rtParentNoteOfNote        , HDT_Note       .class).forEach(note     -> note    .parentNotes     .add(notes       .getByID(1)));
    getOrphans(rtParentLabelOfLabel      , HDT_WorkLabel  .class).forEach(label    -> label   .parentLabels    .add(workLabels  .getByID(1)));
    getOrphans(rtParentGroupOfGroup      , HDT_PersonGroup.class).forEach(group    -> group   .parentGroups    .add(personGroups.getByID(1)));
    getOrphans(rtParentGlossaryOfGlossary, HDT_Glossary   .class).forEach(glossary -> glossary.parentGlossaries.add(glossaries  .getByID(1)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setResolvePointersAgain()
  {
    if ((deletionInProgress == false) && (pointerResolutionInProgress == false))
      internalErrorMessage(44928);

    resolveAgain = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cleanupRelations() throws HDB_InternalError
  {
    globalLock.lock();

    try
    {
      for (RelationSet<? extends HDT_Record, ? extends HDT_Record> relationSet : relationSets.values())
        relationSet.cleanup();
    }
    finally
    {
      globalLock.unlock();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract void fileNoLongerInUse(FilePath filePath);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void finalizeXMLFile(List<StringBuilder> xmlList, List<String> filenameList, String fileName)
  {
    xmlList.get(xmlList.size() - 1).append(System.lineSeparator()).append("</records>");
    filenameList.add(fileName);
    xmlList.add(new StringBuilder());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void writeDatasetToXML(List<StringBuilder> xmlList, HyperTask task, RecordType type) throws HDB_InternalError, CancelledTaskException
  {
    StringBuilder xml = xmlList.get(xmlList.size() - 1);

    if (xml.isEmpty())
    {
      xml.append("<?xml version=\"1.0\" encoding=\"" + XML_FILES_CHARSET.name() + "\"?>").append(System.lineSeparator()).append(System.lineSeparator())
         .append("<records version=\"").append(getVersionNumberSavingAs(appVersionToMaxRecordsXMLVersion)).append("\" xmlns=\"org.hypernomicon\"")

      //   .append(" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:schemaLocation=\"org.hypernomicon http://hypernomicon.org/records.xsd\"")

         .append('>').append(System.lineSeparator()).append(System.lineSeparator());
    }

    datasets.get(type).writeToXML(xml, task);

    if (task != null)
    {
      task.completedCount += records(type).size();

      if (EnumSet.of(hdtDebate, hdtNote, hdtPersonGroup, hdtWorkLabel, hdtGlossary).contains(type))
        task.completedCount--;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean saveAllToPersistentStorage(HyperFavorites favorites)
  {
    if (loaded == false) return false;

    if (checkChecksums() == false) return false;

    if (bibLibraryIsLinked())
      bibLibrary.saveAllToPersistentStorage();

    HyperTask task = new HyperTask("SaveAllToXML", "Saving to XML files...") { @Override protected void call() throws CancelledTaskException, HyperDataException
    {
      totalCount = 0;
      accessors.forEach((type, accessor) ->
      {
        switch (type)
        {
          case hdtNone :
            break;

          case hdtDebate : case hdtNote : case hdtPersonGroup : case hdtWorkLabel : case hdtGlossary :
            totalCount += accessor.size() - 1;
            break;

          default :
            totalCount += accessor.size();
            break;
        }
      });

      try
      {
        List<String> filenameList = new ArrayList<>();
        List<StringBuilder> xmlList = Lists.newArrayList(new StringBuilder());

        writeDatasetToXML(xmlList, this, hdtPersonStatus);    writeDatasetToXML(xmlList, this, hdtRank);            writeDatasetToXML(xmlList, this, hdtField);
        writeDatasetToXML(xmlList, this, hdtSubfield);        writeDatasetToXML(xmlList, this, hdtWorkType);        writeDatasetToXML(xmlList, this, hdtFileType);
        writeDatasetToXML(xmlList, this, hdtCountry);         writeDatasetToXML(xmlList, this, hdtRegion);          writeDatasetToXML(xmlList, this, hdtPositionVerdict);
        writeDatasetToXML(xmlList, this, hdtArgumentVerdict); writeDatasetToXML(xmlList, this, hdtInstitutionType); writeDatasetToXML(xmlList, this, hdtPersonGroup);

                                                              finalizeXMLFile(xmlList, filenameList, OTHER_FILE_NAME);

        writeDatasetToXML(xmlList, this, hdtPerson);          finalizeXMLFile(xmlList, filenameList, PERSON_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtInstitution);     finalizeXMLFile(xmlList, filenameList, INSTITUTION_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtInvestigation);   finalizeXMLFile(xmlList, filenameList, INVESTIGATION_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtDebate);          finalizeXMLFile(xmlList, filenameList, DEBATE_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtArgument);        finalizeXMLFile(xmlList, filenameList, ARGUMENT_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtPosition);        finalizeXMLFile(xmlList, filenameList, POSITION_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtGlossary);
        writeDatasetToXML(xmlList, this, hdtConceptSense);
        writeDatasetToXML(xmlList, this, hdtTerm);
        writeDatasetToXML(xmlList, this, hdtConcept);         finalizeXMLFile(xmlList, filenameList, TERM_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtFolder);
        writeDatasetToXML(xmlList, this, hdtMiscFile);
        writeDatasetToXML(xmlList, this, hdtWorkFile);        finalizeXMLFile(xmlList, filenameList, FILE_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtWorkLabel);
        writeDatasetToXML(xmlList, this, hdtWork);            finalizeXMLFile(xmlList, filenameList, WORK_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtNote);            finalizeXMLFile(xmlList, filenameList, NOTE_FILE_NAME);
        writeDatasetToXML(xmlList, this, hdtHub);             finalizeXMLFile(xmlList, filenameList, HUB_FILE_NAME);

        for (int ndx = 0; ndx < filenameList.size(); ndx++)
          xmlChecksums.put(filenameList.get(ndx), saveStringBuilderToFile(xmlList.get(ndx), xmlPath(filenameList.get(ndx)), XML_FILES_CHARSET));
      }
      catch (IOException | HDB_InternalError e)
      {
        throw new HyperDataException("An error occurred while saving to XML files. " + getThrowableMessage(e), e);
      }
    }};

    if (task.runWithProgressDialog() != State.SUCCEEDED) return false;

    MessageDigest md = newMessageDigest();

    try (OutputStream os = Files.newOutputStream(xmlPath(SETTINGS_FILE_NAME).toPath());
         DigestOutputStream dos = new DigestOutputStream(os, md))
    {
      favorites.saveToPrefNode();

      prefs.put(PrefKey.SETTINGS_VERSION, getVersionNumberSavingAs(appVersionToMaxSettingsXMLVersion).toString());

      prefs.put(PrefKey.DB_CREATION_DATE, dateTimeToIso8601offset(dbCreationDate));

      prefs.exportSubtree(dos);  // Hardcoded to export in UTF-8
    }
    catch (IOException | BackingStoreException e)
    {
      errorMessage("An error occurred while attempting to save database options to " + SETTINGS_FILE_NAME +
                   ". Record data has been saved to XML files, however." + System.lineSeparator() + getThrowableMessage(e));

      return true;
    }

    xmlChecksums.put(SETTINGS_FILE_NAME, digestHexStr(md));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static VersionNumber getVersionNumberSavingAs(Map<VersionNumber, VersionNumber> appVersionToMaxVersion)
  {
    VersionNumber versionNumber = new VersionNumber(0);

    for (Entry<VersionNumber, VersionNumber> entry : appVersionToMaxVersion.entrySet())
    {
      if (ComparableUtils.is(entry.getKey()).lessThanOrEqualTo(appVersion))
      {
        VersionNumber maxVersion = entry.getValue();

        if (ComparableUtils.is(maxVersion).greaterThan(versionNumber))
          versionNumber = maxVersion;
      }
    }

    return versionNumber;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean loadAllFromPersistentStorage(boolean creatingNew, HyperFavorites favorites, FilePath newRootFilePath, String hdbFileName) throws HDB_UnrecoverableInternalError
  {
    if (initialized == false)
      return false;

    if (getOtherLockOwner() != null)
      return false;

    if (FilePath.isEmpty(newRootFilePath) || strNullOrBlank(hdbFileName))
      return false;

    boolean dbChanged = FilePath.isEmpty(rootFilePath) || (rootFilePath.equals(newRootFilePath) == false);

    close(null);

    updateRunningInstancesFile(newRootFilePath);

    rootFilePath = newRootFilePath;
    hdbFilePath = rootFilePath.resolve(hdbFileName);

    if (dbChanged)
      dbPreChangeHandlers.forEach(Runnable::run);

    alreadyShowedUpgradeMsg = false;
    EnumMap<RecordType, VersionNumber> recordTypeToDataVersion = new EnumMap<>(RecordType.class);
    SetMultimap<Integer, Integer> workIDtoInvIDs = LinkedHashMultimap.create(); // For backwards compatibility with records XML version 1.4

    final List<FilePath> xmlFileList = Stream.of(OTHER_FILE_NAME,  PERSON_FILE_NAME,   INSTITUTION_FILE_NAME, INVESTIGATION_FILE_NAME,
                                                 DEBATE_FILE_NAME, ARGUMENT_FILE_NAME, POSITION_FILE_NAME,    WORK_FILE_NAME,
                                                 TERM_FILE_NAME,   FILE_FILE_NAME,     NOTE_FILE_NAME,        HUB_FILE_NAME ).map(this::xmlPath).toList();

    if (loadFromXMLFiles(xmlFileList, creatingNew, recordTypeToDataVersion, workIDtoInvIDs) == false)
    {
      close(null);
      return false;
    }

    try
    {
      for (HyperDataset<? extends HDT_Record> dataset : datasets.values())
        dataset.assignIDs();
    }
    catch (HDB_InternalError e)
    {
      errorMessage(e);

      close(null);
      return false;
    }

    if (bringAllRecordsOnline() == false)
    {
      close(null);
      return false;
    }

    dbCreationDate = APP_GENESIS_INSTANT;

    try
    {
      loadSettings(creatingNew, favorites);

      resolvePointers();

      initBibLibraryLinkFromDBSettings();
    }
    catch (HyperDataException e)
    {
      errorMessage(e);

      close(null);
      return false;
    }

    // Backwards compatibility with records XML version 1.4
    if (workIDtoInvIDs.isEmpty() == false)
      doInvestigationConversion(workIDtoInvIDs);

    // Backwards compatibility with records XML version 1.3
    if (ComparableUtils.is(recordTypeToDataVersion.getOrDefault(hdtWorkType, new VersionNumber(1))).lessThanOrEqualTo(new VersionNumber(1, 3)))
      doConversionForThesisWorkType();

    loadMainTextTemplates();

    List<HDT_Work> worksToUnlink = new ArrayList<>();
    bibEntryKeyToWork.forEach((bibEntryKey, work) ->
    {
      // Unlink works from reference manager entries that aren't present to avoid data integrity errors
      if ((bibLibrary == null) || (bibLibrary.getEntryByKey(bibEntryKey) == null))
        worksToUnlink.add(work);
    });

    worksToUnlink.forEach(work -> work.setBibEntryKey(""));

    HDT_RecordBase.setRootRecordDates();

    // Backwards compatibility with records XML version 1.7
    if (bibLibraryIsLinked() && ComparableUtils.is(recordTypeToDataVersion.getOrDefault(hdtWork, new VersionNumber(1))).lessThan(new VersionNumber(1, 8)))
      doBibDateConversion();

    checkWhetherFoldersExist();

    loaded = true;
    dbLoadedHandlers.forEach(Runnable::run);

    rebuildMentions();

    try
    {
      lock();
    }
    catch (IOException e)
    {
      warningMessage("An error occurred while writing lock file: " + getThrowableMessage(e));
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Backwards compatibility with records XML version 1.7

  private void doBibDateConversion()
  {
    boolean wasRunningConversion = runningConversion;
    runningConversion = true;

    bibEntryKeyToWork.forEach((bibEntryKey, work) ->
    {
      BibliographicDate date = bibLibrary.getEntryByKey(bibEntryKey).getDateFromJson();

      if ((date.hasDay() || date.hasMonth()) && Objects.equals(date.year, work.getBibDate().year))
        work.setBibDate(date);
    });

    runningConversion = wasRunningConversion;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Backwards compatibility with records XML version 1.4

  private void doInvestigationConversion(SetMultimap<Integer, Integer> workIDtoInvIDs)
  {
    boolean wasRunningConversion = runningConversion;
    runningConversion = true;

    for (Entry<Integer, Collection<Integer>> entry : workIDtoInvIDs.asMap().entrySet())
    {
      List<HDT_Investigation> invList = entry.getValue().stream().map(investigations::getByID).toList();

      invList.forEach(inv -> inv.getMainText().addKeyworksIfNotPresent());

      MainText.setKeyWorkMentioners(works.getByID(entry.getKey()), invList, HDT_Investigation.class);
    }

    runningConversion = wasRunningConversion;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Backwards compatibility with records XML version 1.3

  private void doConversionForThesisWorkType()
  {
    int thesisID = HDT_WorkType.getIDbyEnum(wtThesis);

    HDT_WorkType thesisWorkType = workTypes.getByID(thesisID);

    try
    {
      if (thesisWorkType != null)
        changeRecordID(thesisWorkType, datasets.get(hdtWorkType).getNextID());

      createNewRecordFromState(new RecordState(hdtWorkType, thesisID, "Thesis", "Thesis", "", ""), true);
    }
    catch (HyperDataException e)
    {
      throw new AssertionError("Internal error while creating thesis work type record: " + getThrowableMessage(e), e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void changeRecordID(HDT_Record record, int newID) throws HyperDataException
  {
    int oldID = record.getID();

    if (isProtectedRecord(oldID, record.getType(), false))
      throw new HyperDataException("That record's ID cannot be changed.");

    if (idAvailable(record.getType(), newID) == false)
      throw new HDB_InternalError(42973);

    changeRecordIDHandlers.forEach(handler -> handler.changeRecordID(record, newID));

    datasets.get(record.getType()).changeRecordID(oldID, newID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void unlinkBibLibrary()
  {
    if (bibLibrary == null) return;

    boolean startWatcher = nullSwitch(getFolderTreeWatcher(), false, FolderTreeWatcher::stop);

    clearBibLinkData();

    xmlPath(BIB_FILE_NAME).deletePromptOnFail(true);

    if (startWatcher)
      getFolderTreeWatcher().createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Clears all bibliographic link data associated with the current session.
   * This method resets stored library information, removes associated user preferences,
   * and triggers any registered change handlers.
   *
   * <p>Actions performed by this method:</p>
   * <ul>
   *   <li>Sets <code>bibLibrary</code> to null</li>
   *   <li>Clears bibliographic entry keys for all works</li>
   *   <li>Triggers registered handlers for bibliographic data changes</li>
   *   <li>Removes stored user identifiers and synchronization metadata from DB settings</li>
   *   <li>Deletes authentication-related credentials from DB settings (API key, access token, refresh token)</li>
   * </ul>
   * <p>This method does NOT delete the Bib.json file that has the reference manager entries.</p>
   *
   * @see #bibLibrary
   */
  private void clearBibLinkData()
  {
    bibLibrary = null;

    works.forEach(work -> work.setBibEntryKey(""));

    bibChangedHandlers.forEach(Runnable::run);

    prefs.remove(PrefKey.BIB_USER_ID);
    prefs.remove(PrefKey.BIB_USER_NAME);
    prefs.remove(PrefKey.BIB_LIBRARY_VERSION);
    prefs.remove(PrefKey.BIB_LAST_SYNC_TIME);
    prefs.remove(PrefKey.BIB_LIBRARY_TYPE);

    prefs.remove(PrefKey.BIB_API_KEY);
    prefs.remove(PrefKey.BIB_ACCESS_TOKEN);
    prefs.remove(PrefKey.BIB_REFRESH_TOKEN);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void linkBibLibrary(LibraryType libType, BibAuthKeys authKeys, String userID, String userName) throws IOException, ParseException, HyperDataException
  {
    if (bibLibrary != null)
      throw new HDB_InternalError(21174);

    FilePath bibJsonFilePath = xmlPath(BIB_FILE_NAME);

    if (bibJsonFilePath.exists())
    {
      boolean startWatcher = nullSwitch(getFolderTreeWatcher(), false, FolderTreeWatcher::stop);

      bibJsonFilePath.deletePromptOnFail(true);

      if (startWatcher)
        getFolderTreeWatcher().createNewWatcherAndStart();
    }

    loadBibLibrary(libType, authKeys, userID, userName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadBibLibrary(LibraryType libType, BibAuthKeys authKeys, String userID, String userName) throws IOException, ParseException, HyperDataException
  {
    if (bibLibrary != null)
      throw new HDB_InternalError(21173);

    bibLibrary = switch (libType)
    {
      case ltZotero   -> ZoteroWrapper  .create((ZoteroAuthKeys  ) authKeys, userID, userName, xmlPath(BIB_FILE_NAME));
      case ltMendeley -> MendeleyWrapper.create((MendeleyAuthKeys) authKeys, userID, userName, xmlPath(BIB_FILE_NAME));
    };

    bibChangedHandlers.forEach(Runnable::run);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isProtectedRecord(HDT_Record record, boolean checkSubfolders)
  {
    return isProtectedRecord(record.getID(), record.getType(), checkSubfolders);
  }

  public boolean isProtectedRecord(int id, RecordType type, boolean checkSubfolders)
  {
    return isUnstoredRecord(id, type) || ((type == hdtFolder) && isSpecialFolder(id, checkSubfolders));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isSpecialFolder(int id, boolean checkSubfolders)
  {
    if (id < 1) return false;

    if ((id == ROOT_FOLDER_ID         ) ||
        (id == xmlFolder      .getID()) ||
        (id == booksFolder    .getID()) ||
        (id == papersFolder   .getID()) ||
        (id == miscFilesFolder.getID()) ||
        (id == picturesFolder .getID()) ||
        (id == resultsFolder  .getID()) ||
        (id == unenteredFolder.getID()) ||
        (id == topicalFolder  .getID()))
      return true;

    return checkSubfolders && folders.getByID(id).childFolders.stream().anyMatch(folder -> folder.isSpecial(true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isUnstoredRecord(HDT_Record record)
  {
    return isUnstoredRecord(record.getID(), record.getType());
  }

  public static boolean isUnstoredRecord(int id, RecordType type)
  {
    return switch (type)
    {
      case hdtNote, hdtDebate, hdtWorkLabel, hdtPersonGroup, hdtFolder, hdtGlossary -> id == 1;
      default -> false;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void deleteRecord(HDT_Record record)
  {
    Objects.requireNonNull(record, "Record to delete is null");

    if (deletionInProgress == false)
      startMentionsRebuildAfterDelete = false;

    if (record.isExpired())
    {
      errorMessage("The record has already been deleted.");
      return;
    }

    if (HDT_Record.isEmpty(record, false) || isProtectedRecord(record, true))
    {
      errorMessage("Unable to delete record.");
      return;
    }

    if (record.isDummy() == false)
    {
      if (mentionsIndex.isRebuilding())
      {
        startMentionsRebuildAfterDelete = true;
        mentionsIndex.stopRebuild();
      }
      else
        mentionsIndex.removeRecord(record);

      if (record.getType() != hdtConcept)
        try { record.setSearchKey(""); } catch (SearchKeyException e) { throw newAssertionError(e); }
    }

    if (deletionInProgress)
    {
      record.expire();
      return;
    }

    deletionInProgress = true;
    deleteFileAnswer = mrNone;

    globalLock.lock();

    try
    {
      record.expire();
    }
    finally
    {
      globalLock.unlock();
    }

    try
    {
      resolvePointers();  // This is where the record actually gets deleted (removed from its HyperCore)
      cleanupRelations();
    }
    catch (HDB_InternalError e)
    {
      errorMessage(e);
    }

    deletionInProgress = false;
    deleteFileAnswer = mrNone;

    FolderTreeWatcher watcher = getFolderTreeWatcher();

    if ((watcher != null) && watcher.isDisabled())
    {
      watcher.enable();
      watcher.createNewWatcherAndStart();
    }

    if (startMentionsRebuildAfterDelete == false)
      return;

    rebuildMentions();
    startMentionsRebuildAfterDelete = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * For each record in the database, calls its resolvePointers method (which may cause the
   * record to become expired), and removes the record from its HyperCore ("deletes" it) if it
   * became expired.<br>
   * HDT_Record.expire is where delete handlers get called.
   * @throws HDB_InternalError If the record somehow entered the inconsistent
   * state of its ID being -1 but the expired flag was not set.
   */
  public void resolvePointers() throws HDB_InternalError
  {
    if (pointerResolutionInProgress)
      throw new HDB_InternalError(78382);

    boolean startMentionsRebuild = false;

    if (mentionsIndex.isRebuilding())
    {
      startMentionsRebuild = true;
      mentionsIndex.stopRebuild();
    }

    pointerResolutionInProgress = true;

    globalLock.lock();

    try
    {
      do
      {
        resolveAgain = false;

        for (HyperDataset<? extends HDT_Record> dataset : datasets.values())
          dataset.resolvePointers();

      } while (resolveAgain);
    }
    finally
    {
      globalLock.unlock();

      pointerResolutionInProgress = false;

      if (startMentionsRebuild)
        rebuildMentions();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void bringAllDatasetsOnline(HyperTask task) throws HyperDataException, CancelledTaskException
  {
    try
    {
      for (Entry<RecordType, HyperDataset<? extends HDT_Record>> entry : datasets.entrySet()) // Folders must be brought online first. See HyperPath.assignNameInternal
      {
        HyperDataset<? extends HDT_Record> dataset = entry.getValue();

        if (dataset.online)
          throw new HDB_InternalError(89842);

        for (HDT_Record record : accessors.get(entry.getKey()))
        {
          record.bringStoredCopyOnline(false);
          addToInitialNavList(record);

          if (task != null)
            task.incrementAndUpdateProgress(50);
        }

        dataset.online = true;
      }

      addRootFolder();
    }
    catch (RestoreException e)
    {
      throw new HDB_InternalError(42837, e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void loadSettingsFromStream(InputStream is, boolean creatingNew, HyperFavorites favorites) throws HyperDataException
  {
    MessageDigest md = newMessageDigest();

    try (DigestInputStream dis = new DigestInputStream(is, md))
    {
      prefs = XmlSupport.importPreferences(dis, XML_FILES_CHARSET).node("org").node("hypernomicon").node("model");

      xmlChecksums.put(SETTINGS_FILE_NAME, digestHexStr(md));

      if (favorites != null)
        favorites.loadFromPrefNode();

      String versionStr = prefs.get(PrefKey.SETTINGS_VERSION, "");
      if (versionStr.isBlank())
        throw new HyperDataException("XML settings data version number not found.");

      VersionNumber settingsVersion = new VersionNumber(versionStr);
      checkVersion(creatingNew, settingsVersion, "the Settings XML file", appVersionToMinSettingsXMLVersion, appVersionToMaxSettingsXMLVersion);

      boolean writeFolderIDs = false;

      if (ComparableUtils.is(settingsVersion).lessThanOrEqualTo(new VersionNumber(1)))  // Backwards compatibility with settings version 1.0
      {
        papersFolder    = folders.getByID(2);
        booksFolder     = folders.getByID(3);
        miscFilesFolder = folders.getByID(4);
        picturesFolder  = folders.getByID(5);
        topicalFolder   = folders.getByID(6);
        unenteredFolder = folders.getByID(7);
        resultsFolder   = folders.getByID(8);
        xmlFolder       = HyperPath.getFolderFromFilePath(getRootPath(DEFAULT_XML_PATH), true);

        writeFolderIDs = true;
      }
      else
      {
        xmlFolder       = folders.getByID(prefs.getInt(FolderIDPrefKey.XML       , -1));
        picturesFolder  = folders.getByID(prefs.getInt(FolderIDPrefKey.PICTURES  , -1));
        booksFolder     = folders.getByID(prefs.getInt(FolderIDPrefKey.BOOKS     , -1));
        papersFolder    = folders.getByID(prefs.getInt(FolderIDPrefKey.PAPERS    , -1));
        resultsFolder   = folders.getByID(prefs.getInt(FolderIDPrefKey.RESULTS   , -1));
        unenteredFolder = folders.getByID(prefs.getInt(FolderIDPrefKey.UNENTERED , -1));
        miscFilesFolder = folders.getByID(prefs.getInt(FolderIDPrefKey.MISC_FILES, -1));
        topicalFolder   = folders.getByID(prefs.getInt(FolderIDPrefKey.TOPICAL   , -1));
      }

      if (HDT_Record.isEmpty(picturesFolder , false) ||
          HDT_Record.isEmpty(booksFolder    , false) ||
          HDT_Record.isEmpty(papersFolder   , false) ||
          HDT_Record.isEmpty(resultsFolder  , false) ||
          HDT_Record.isEmpty(unenteredFolder, false) ||
          HDT_Record.isEmpty(miscFilesFolder, false) ||
          HDT_Record.isEmpty(xmlFolder      , false) ||
          HDT_Record.isEmpty(topicalFolder  , false))
      {
        throw new HyperDataException("Unable to load information about paths from database settings file.");
      }

      if (writeFolderIDs)  // Backwards compatibility with settings version 1.0
      {
        prefs.putInt(FolderIDPrefKey.XML       , xmlFolder      .getID());
        prefs.putInt(FolderIDPrefKey.PICTURES  , picturesFolder .getID()); prefs.remove("picturesPath" );
        prefs.putInt(FolderIDPrefKey.BOOKS     , booksFolder    .getID()); prefs.remove("booksPath"    );
        prefs.putInt(FolderIDPrefKey.PAPERS    , papersFolder   .getID()); prefs.remove("papersPath"   );
        prefs.putInt(FolderIDPrefKey.RESULTS   , resultsFolder  .getID()); prefs.remove("resultsPath"  );
        prefs.putInt(FolderIDPrefKey.UNENTERED , unenteredFolder.getID()); prefs.remove("unenteredPath"); prefs.remove("unenteredPat");
        prefs.putInt(FolderIDPrefKey.MISC_FILES, miscFilesFolder.getID()); prefs.remove("suppFilesPath");
        prefs.putInt(FolderIDPrefKey.TOPICAL   , topicalFolder  .getID()); prefs.remove("topicsPath"   );
      }

      String dbCreationDateStr = prefs.get(PrefKey.DB_CREATION_DATE, "");
      if (strNotNullOrEmpty(dbCreationDateStr))
      {
        dbCreationDate = parseIso8601offset(dbCreationDateStr);

        if (dbCreationDate.isAfter(Instant.now())) // Creation date in template is year 9999 so it will be set
          dbCreationDate = Instant.now();          // to the current date when loaded for the first time
      }
    }
    catch (IOException | InvalidPreferencesFormatException e)
    {
      throw new HyperDataException("An error occurred while attempting to read database settings: " + getThrowableMessage(e), e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("deprecation")
  private void initBibLibraryLinkFromDBSettings() throws HyperDataException
  {
    String bibUserID          = prefs.get(PrefKey.BIB_USER_ID     , ""),
           bibUserName        = prefs.get(PrefKey.BIB_USER_NAME   , ""),
           bibTypeDescriptor  = prefs.get(PrefKey.BIB_LIBRARY_TYPE, "");

    LibraryType libType = LibraryType.getByDescriptor(bibTypeDescriptor);

    if ((libType != LibraryType.ltMendeley) && ((libType != LibraryType.ltZotero) || bibUserID.isBlank()))
    {
      clearBibLinkData();
      return;
    }

    try
    {
      loadBibLibrary(libType, BibAuthKeys.loadFromDBSettings(libType), bibUserID, bibUserName);
    }
    catch (Exception e)
    {
      throw new HyperDataException("Unable to initialize link to " + libType.userFriendlyName + ": " + getThrowableMessage(e), e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addRootFolder()
  {
    filenameMap.computeIfAbsent(rootFilePath.getNameOnly().toString(), rootFolderName -> ConcurrentHashMap.newKeySet()).add(getRootFolder().getPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String recordsTagName = "records", versionAttr = "version";

  public static VersionNumber getVersionNumberFromXML(XMLEventReader eventReader) throws XMLStreamException
  {
    while (eventReader.hasNext())
    {
      XMLEvent event = eventReader.nextEvent();

      if (event.isStartElement() == false)
        continue;

      StartElement startElement = event.asStartElement();

      if (startElement.getName().getLocalPart().equals(tagRecord.name))
        return null;

      if (startElement.getName().getLocalPart().equals(recordsTagName) == false)
        continue;

      Iterator<Attribute> attributes = startElement.getAttributes();

      while (attributes.hasNext())
      {
        Attribute attribute = attributes.next();

        if (attribute.getName().toString().equals(versionAttr))
          return new VersionNumber(attribute.getValue());
      }
    }

    return new VersionNumber(1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Find the beginning of the next record element

  /**
   * Parses XML starting from the current cursor, skipping anything that is not the beginning of a record tag.<br>
   * Then parses the open tag, using its attributes to create a new RecordState (with no item values populated yet)
   * which is then returned. The cursor is left immediately after the end of the open tag for the record element.
   * @param eventReader Iterator for XML events
   * @param fileDescription How to describe the current file for purposes of error messages
   * @return The new RecordState object
   * @throws XMLStreamException if there is an error with the underlying XML.
   * @throws HyperDataException if there is no record type or an invalid record type in the record tag.
   */
  private static RecordState getNextRecordFromXML(XMLEventReader eventReader, String fileDescription) throws XMLStreamException, HyperDataException
  {
    while (eventReader.hasNext())
    {
      XMLEvent event = eventReader.nextEvent();

      if (event.isStartElement() == false)
        continue;

      StartElement startElement = event.asStartElement();
      if (startElement.getName().getLocalPart().equals(tagRecord.name) == false)
        continue;

      int id = -1;
      RecordType type = hdtNone;
      String sortKeyAttr = "", listName = "", searchKey = "";

      Iterator<Attribute> attributes = startElement.getAttributes();

      while (attributes.hasNext())
      {
        Attribute attribute = attributes.next();
        Tag tag = getTag(attribute.getName().toString());

        switch (tag)
        {
          case tagID        : id = parseInt(attribute.getValue(), -1); break;
          case tagType      :

            type = parseTypeTagStr(attribute.getValue());
            if (type == hdtNone)
              throw new HyperDataException("Invalid record type: " + attribute.getValue() + (id > 0 ? (" ID: " + id) : "") + " File: " + fileDescription);

            break;

          case tagSortKey   : sortKeyAttr = attribute.getValue(); break;
          case tagSearchKey : searchKey   = attribute.getValue(); break;
          case tagListName  : listName    = attribute.getValue(); break;
          default           : break;
        }
      }

      if (type == hdtNone)
        throw new HyperDataException("Record with no type found." + (id > 0 ? (" ID: " + id) : "") + " File: " + fileDescription);

      RecordState xmlRecord = new RecordState(type, id, sortKeyAttr, "", searchKey, listName);
      xmlRecord.stored = true;
      return xmlRecord;
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a new Hypernomicon record and initialize its data with information from
   * the passed-in record state
   *
   * @param <T> The record type
   * @param recordState The record state object containing information with which to initialize the record's fields
   *                    such as record type, ID, etc.
   * @param bringOnline Whether the new record should be brought online (included in indexes, etc.)
   * @return The new record
   * @throws DuplicateRecordException If a record already exists with the supplied type and ID
   * @throws RelationCycleException Only thrown if bringing online; should never happen because a record cannot already
   *                                be in relation with another record before it has been created
   * @throws HDB_InternalError If bringOnline is true but the database is not online, or some other internal error
   *                           happens while bringing record online
   * @throws SearchKeyException Only thrown if bringing online; if the recordState's search key was too short or is a duplicate of an existing search key
   * @throws RestoreException Only thrown if bringing online; should never happen because it only gets thrown when existing
   *                          data in the record (like the record's hub, or a term record's concept records) conflicts with
   *                          data in the recordState
   */
  @SuppressWarnings("unchecked")
  public <T extends HDT_RecordBase> T createNewRecordFromState(RecordState recordState, boolean bringOnline) throws DuplicateRecordException, RelationCycleException, HDB_InternalError, SearchKeyException, RestoreException
  {
    return (T) datasets.get(recordState.type).createNewRecord(recordState, bringOnline);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public <T extends HDT_RecordBase> T createNewBlankRecord(RecordType type)
  {
    try
    {
      return (T) datasets.get(type).createNewRecord(new RecordState(type), true);
    }
    catch (HDB_InternalError e)
    {
      errorMessage(e);
    }
    catch (DuplicateRecordException | RelationCycleException | SearchKeyException | RestoreException e) { throw newAssertionError(e); }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void checkVersion(boolean creatingNew, VersionNumber versionNumber, String dataName,
                            Map<VersionNumber, VersionNumber> appVersionToMinVersion,
                            Map<VersionNumber, VersionNumber> appVersionToMaxVersion) throws HyperDataException
  {
    VersionNumber newestTooOldAppVersion = new VersionNumber(0),
                  oldestTooNewAppVersion = new VersionNumber(Integer.MAX_VALUE);

    for (Entry<VersionNumber, VersionNumber> entry : appVersionToMinVersion.entrySet())
    {
      if (ComparableUtils.is(entry.getValue()).greaterThan(versionNumber))        // Too new
        if (ComparableUtils.is(entry.getKey()).lessThan(oldestTooNewAppVersion))  // Older than other too new ones
          oldestTooNewAppVersion = entry.getKey();
    }

    if (ComparableUtils.is(oldestTooNewAppVersion).lessThanOrEqualTo(appVersion))
      throw new HyperDataException("A version of Hypernomicon older than v" + oldestTooNewAppVersion + " is required to load " + dataName + '.');

    for (Entry<VersionNumber, VersionNumber> entry : appVersionToMaxVersion.entrySet())
    {
      if (ComparableUtils.is(entry.getValue()).lessThan(versionNumber))              // Too old
        if (ComparableUtils.is(entry.getKey()).greaterThan(newestTooOldAppVersion))  // Newer than other too old ones
          newestTooOldAppVersion = entry.getKey();
    }

    if (ComparableUtils.is(newestTooOldAppVersion).greaterThanOrEqualTo(appVersion))
      throw new HyperDataException("A version of Hypernomicon newer than v" + newestTooOldAppVersion + " is required to load " + dataName + '.');

    VersionNumber savingAs = getVersionNumberSavingAs(appVersionToMaxVersion);

    if (creatingNew || (ComparableUtils.is(versionNumber).lessThan(savingAs) == false))
      return;

    if (appVersionToMinVersion == appVersionToMinRecordsXMLVersion)
    {
      if (alreadyShowedUpgradeMsg) return;
      alreadyShowedUpgradeMsg = true;
    }

    newestTooOldAppVersion = new VersionNumber(0);

    for (Entry<VersionNumber, VersionNumber> entry : appVersionToMaxVersion.entrySet())
    {
      if (ComparableUtils.is(entry.getValue()).lessThan(savingAs))                   // Too old
        if (ComparableUtils.is(entry.getKey()).greaterThan(newestTooOldAppVersion))  // Newer than other too old ones
          newestTooOldAppVersion = entry.getKey();
    }

    warningMessage("When you save changes, " + dataName + " will be upgraded and will no longer be compatible with Hypernomicon v" + newestTooOldAppVersion + " or older.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void loadFromXMLStream(boolean creatingNew, InputStream is, EnumMap<RecordType, VersionNumber> recordTypeToDataVersion, SetMultimap<Integer, Integer> workIDtoInvIDs,
                                   String fileDescription, String fileName, long fileSize, HyperTask task) throws HyperDataException, CancelledTaskException
  {
    MessageDigest md = newMessageDigest();

    try (DigestInputStream dis = new DigestInputStream(is, md))
    {
      XMLEventReader eventReader = XMLInputFactory.newInstance().createXMLEventReader(dis, XML_FILES_CHARSET.name());

      VersionNumber dataVersion = getVersionNumberFromXML(eventReader);

      if (dataVersion == null)
        throw new HyperDataException("XML record data version number not found. File: " + fileDescription);

      checkVersion(creatingNew, dataVersion, "this XML record data", appVersionToMinRecordsXMLVersion, appVersionToMaxRecordsXMLVersion);

//---------------------------------------------------------------------------

      // Main loop for single XML file
      // -----------------------------

      while (true)
      {
        RecordState xmlRecord = getNextRecordFromXML(eventReader, fileDescription);
        if (xmlRecord == null) break;

        boolean notDoneReadingRecord = eventReader.hasNext(), noItemTags = true;
        Map<Tag, HDI_OfflineBase> nestedItems = null;
        XMLEvent event = null;
        String nodeText = "";
        HDX_Element topLevelItemElement = null;

        // Loop for the elements in a single record
        // ----------------------------------------

        while (notDoneReadingRecord)
        {
          HyperTask.throwExceptionIfCancelled(task);

          event = eventReader.nextEvent();

          switch (event.getEventType())
          {

//---------------------------------------------------------------------------

            case XMLStreamConstants.START_ELEMENT :
            {
              HDX_Element hdxElement = HDX_Element.create(event.asStartElement(), xmlRecord, dataVersion);

              if (topLevelItemElement == null)
              {
                topLevelItemElement = hdxElement;
                nodeText = "";
                noItemTags = false;
                nestedItems = null;
              }
              else
              {
                if (nestedItems == null)
                  nestedItems = new LinkedHashMap<>();

                readNestedItem(xmlRecord, nestedItems, getRelation(xmlRecord.type, topLevelItemElement.getObjType(), false), hdxElement, eventReader);
              }

              break;
            }

//---------------------------------------------------------------------------

            case XMLStreamConstants.END_ELEMENT :
            {
              if ("record".equals(event.asEndElement().getName().getLocalPart()))
                notDoneReadingRecord = false;
              else
              {
                try
                {
                  switch (topLevelItemElement.getTag())
                  {
                    case tagCreationDate : xmlRecord.creationDate = parseIso8601offset(nodeText); break;
                    case tagModifiedDate : xmlRecord.modifiedDate = parseIso8601offset(nodeText); break;
                    case tagViewDate     : xmlRecord.viewDate =     parseIso8601offset(nodeText); break;

                    default              :

                      if ((topLevelItemElement.getTag() == tagInvestigation) && (xmlRecord.type == hdtWork))
                        workIDtoInvIDs.put(xmlRecord.id, topLevelItemElement.getObjID());
                      else
                      {
                        // Backwards compatibility for record data version 1.7 or lower
                        if ((topLevelItemElement instanceof HDX_BibDateElement) && strNotNullOrBlank(nodeText) && ComparableUtils.is(dataVersion).greaterThan(new VersionNumber(1, 7)))
                          throw new HyperDataException(tagBibDate.name + " XML tags can only have attributes, not text. Found text: " + nodeText);

                        xmlRecord.setItemFromXML(topLevelItemElement, nodeText, nestedItems);
                      }
                  }
                }
                catch (DateTimeParseException e)
                {
                  throw new HyperDataException(e);
                }

                topLevelItemElement = null;
                nodeText = "";
              }

              break;
            }

//---------------------------------------------------------------------------

            case XMLStreamConstants.CHARACTERS   :
            {
              nodeText = nodeText + event.asCharacters().getData();
              break;
            }

//---------------------------------------------------------------------------

            case XMLStreamConstants.END_DOCUMENT :
            {
              notDoneReadingRecord = false;
              break;
            }

//---------------------------------------------------------------------------

            default :
            {
              break;
            }
          }

        }  // End of loop for the elements in a single record

//---------------------------------------------------------------------------

        if (noItemTags)
          xmlRecord.setItemFromXML(null, nodeText, null);

        if (isUnstoredRecord(xmlRecord.id, xmlRecord.type) == false)
        {
          try
          {
            createNewRecordFromState(xmlRecord, false);
          }
          catch (RelationCycleException | SearchKeyException e) { throw newAssertionError(e); }

          VersionNumber previousDataVersion = recordTypeToDataVersion.get(xmlRecord.type);

          if (previousDataVersion == null)
            recordTypeToDataVersion.put(xmlRecord.type, dataVersion);
          else if (previousDataVersion.equals(dataVersion) == false)
            throw new HyperDataException("Multiple " + HyperDB.getTypeName(xmlRecord.type) + " records found with incompatible XML record data version numbers. ID: " + xmlRecord.id + " File: " + fileDescription);
        }

        if ((task != null) && (event != null))
          task.updateProgress(task.completedCount + event.getLocation().getCharacterOffset(), task.totalCount);

      }  // End of main loop for single XML file

      if (task != null)
        task.completedCount += fileSize;
    }
    catch (IOException | DuplicateRecordException | InvalidItemException e)
    {
      throw new HyperDataException(e);
    }
    catch (XMLStreamException e)
    {
      throw new HyperDataException("File: " + fileDescription + System.lineSeparator() + getThrowableMessage(e), e);
    }
    catch (RestoreException e)
    {
      throw new HDB_InternalError(42838, e);
    }

    xmlChecksums.put(fileName, digestHexStr(md));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initNestedItems(RecordState xmlRecord, Map<Tag, HDI_OfflineBase> nestedItems, RelationType relation) throws HDB_InternalError
  {
    Property<HDI_OfflineBase> item = new SimpleObjectProperty<>();

    for (HDI_Schema schema : relationSets.get(relation).getSchemas())
    {
      item.setValue(switch (schema.category())
      {
        case hdcBoolean       -> new HDI_OfflineBoolean      (schema, xmlRecord);
        case hdcTernary       -> new HDI_OfflineTernary      (schema, xmlRecord);
        case hdcString        -> new HDI_OfflineString       (schema, xmlRecord);
        case hdcNestedPointer -> new HDI_OfflineNestedPointer(schema, xmlRecord);

        default               -> throw new HDB_InternalError(78936);
      });

      schema.tags().forEach(tag -> nestedItems.put(tag, item.getValue()));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void readNestedItem(RecordState xmlRecord, Map<Tag, HDI_OfflineBase> nestedItems, RelationType relationType, HDX_Element hdxElement, XMLEventReader eventReader) throws XMLStreamException, HyperDataException
  {
    boolean notDone = eventReader.hasNext();
    StringBuilder nodeText = new StringBuilder();

    if (nestedItems.isEmpty())
      initNestedItems(xmlRecord, nestedItems, relationType);

    while (notDone)
    {
      XMLEvent event = eventReader.nextEvent();

      switch (event.getEventType())
      {
        case XMLStreamConstants.START_ELEMENT :

          throw new HyperDataException("Too many nested elements in XML");

        case XMLStreamConstants.CHARACTERS :

          nodeText.append(event.asCharacters().getData());
          break;

        case XMLStreamConstants.END_ELEMENT  : // fall through
        case XMLStreamConstants.END_DOCUMENT :

          notDone = false;
          break;

        default :
          break;
      }
    }

    HDI_OfflineBase item = nestedItems.get(hdxElement.getTag());

    if (item == null) throw new InvalidItemException(xmlRecord.id, xmlRecord.type, "(nested) " + hdxElement.getTag().name);

    item.setFromXml(hdxElement, nodeText.toString(), null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Adds a record to the initial navigation list if it meets specific criteria.
   * Ensures that the list maintains a history of recently viewed records in past
   * sessions, of length no larger than INITIAL_NAV_LIST_SIZE.
   *
   * @param record the record to be added to the initial navigation list.
   */
  private void addToInitialNavList(HDT_Record record)
  {
    if (isUnstoredRecord(record)) return;

    switch (record.getType())
    {
      case hdtArgument    : case hdtDebate   : case hdtPosition : case hdtInvestigation :
      case hdtPersonGroup : case hdtMiscFile : case hdtNote     : case hdtPerson        :
      case hdtInstitution : case hdtConcept  : case hdtWork     : case hdtWorkLabel     :
        break;
      default:
        return;
    }

    int ndx = addToSortedList(initialNavList, record, Comparator.comparing(HDT_Record::getViewDate));

    if ((record.getType() == hdtInvestigation) || (record.getType() == hdtPerson))
      if (removePersonIfAdjacentToInvestigation(record, ndx, ndx - 1) == false)
          removePersonIfAdjacentToInvestigation(record, ndx, ndx + 1);

    while (initialNavList.size() > INITIAL_NAV_LIST_SIZE)
      initialNavList.remove(0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Potentially removes a person record from the initial navigation list if it is
   * adjacent to an investigation record for that person viewed at close to the same
   * time, since they represent the same tab.
   *
   * @param record the record being checked.
   * @param ndx the index of the current record in the navigation list.
   * @param otherNdx the index of the adjacent record being checked for removal.
   * @return true if a person record was successfully removed; false otherwise.
   */
  private boolean removePersonIfAdjacentToInvestigation(HDT_Record record, int ndx, int otherNdx)
  {
    if ((otherNdx < 0) || (otherNdx == initialNavList.size()))
      return false;

    HDT_Record otherRecord = initialNavList.get(otherNdx);

    if (milliDiff(record.getViewDate(), otherRecord.getViewDate()) > 200L)
      return false;

    if ((record.getType() == hdtInvestigation) && (otherRecord.getType() == hdtPerson))
    {
      if (otherRecord == ((HDT_Investigation)record).person.get())
      {
        initialNavList.remove(otherNdx);
        return true;
      }
    }
    else if ((record.getType() == hdtPerson) && (otherRecord.getType() == hdtInvestigation))
    {
      if (record == ((HDT_Investigation)otherRecord).person.get())
      {
        initialNavList.remove(ndx);
        return true;
      }
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void close(Set<RecordType> datasetsToKeep) throws HDB_UnrecoverableInternalError
  {
    boolean bringOnline = datasetsToKeep != null; // Datasets remain online through process of creating a new database

    nullSwitch(getFolderTreeWatcher(), FolderTreeWatcher::stop);

    unlock();

    mentionsIndex.stopRebuild();
    mentionsIndex.clear();

    loaded = false;
    updateRunningInstancesFile(new FilePath(""));
    clearAllDataSets(datasetsToKeep);

    try
    {
      cleanupRelations();
    }
    catch (HDB_InternalError e)
    {
      throw new HDB_UnrecoverableInternalError(e);
    }

    initialNavList   .clear();
    filenameMap      .clear();
    mainTextTemplates.clear();
    keyWorkIndex     .clear();
    displayedAtIndex .clear();
    bibEntryKeyToWork.clear();
    xmlChecksums     .clear();

    if (bibLibrary != null)
      clearBibLinkData();

    if (datasetsToKeep == null)
      prefs = null;

    try
    {
      RecordState recordState = new RecordState(hdtFolder, ROOT_FOLDER_ID);
      createNewRecordFromState(recordState, bringOnline);

      recordState = new RecordState(hdtDebate, 1);
      ((HDI_OfflineString) recordState.items.get(tagName)).set("All debates");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new RecordState(hdtNote, 1);
      ((HDI_OfflineString) recordState.items.get(tagName)).set("All notes");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new RecordState(hdtWorkLabel, 1);
      ((HDI_OfflineString) recordState.items.get(tagText)).set("All labels");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new RecordState(hdtPersonGroup, 1);
      ((HDI_OfflineString) recordState.items.get(tagName)).set("All groups");
      createNewRecordFromState(recordState, bringOnline);

      recordState = new RecordState(hdtGlossary, 1);
      ((HDI_OfflineString) recordState.items.get(tagName)).set("General");
      ((HDI_OfflineBoolean) recordState.items.get(tagActive)).set(true);
      createNewRecordFromState(recordState, bringOnline);

      dbCloseHandlers.forEach(Runnable::run);
    }
    catch (DuplicateRecordException | RelationCycleException | SearchKeyException | RestoreException e)
    {
      throw newAssertionError(e);
    }
    catch (HDB_InternalError e)
    {
      throw new HDB_UnrecoverableInternalError(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearAllDataSets(Set<RecordType> datasetsToKeep)
  {
    if (datasetsToKeep != null) // It should only be non-null when a new database is being created
      datasetsToKeep.addAll(EnumSet.of(hdtWorkType, hdtPositionVerdict, hdtArgumentVerdict, hdtInstitutionType));

    datasets.forEach((type, dataset) -> {
      if ((datasetsToKeep == null) || (datasetsToKeep.contains(type) == false))
        dataset.removeAll(datasetsToKeep != null); }); // Datasets remain online through process of creating a new database

    searchKeys.removeAll();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean newDB(FilePath newPath, String hdbFileName, Set<RecordType> datasetsToKeep, Map<String, String> folderMap) throws HDB_InternalError
  {
    if (loaded == false) return false;

    if (datasetsToKeep == null)
      datasetsToKeep = EnumSet.noneOf(RecordType.class);

    close(datasetsToKeep);

    dbCreationDate = Instant.now();
    prefs.put(PrefKey.DB_CREATION_DATE, dateTimeToIso8601offset(dbCreationDate));

    rootFilePath = newPath;
    saveSourcePathToSystemSettings(newPath.toString());
    hdbFilePath = rootFilePath.resolve(hdbFileName);

    addRootFolder();

    HDT_RecordBase.setRootRecordDates();

    try
    {
      createSpecialFolderRecord(DEFAULT_XML_FOLDER_ID    , DEFAULT_XML_PATH, FolderIDPrefKey.XML       );
      createSpecialFolderRecord(DEFAULT_XML_FOLDER_ID + 1, folderMap       , FolderIDPrefKey.PAPERS    );
      createSpecialFolderRecord(DEFAULT_XML_FOLDER_ID + 2, folderMap       , FolderIDPrefKey.BOOKS     );
      createSpecialFolderRecord(DEFAULT_XML_FOLDER_ID + 3, folderMap       , FolderIDPrefKey.MISC_FILES);
      createSpecialFolderRecord(DEFAULT_XML_FOLDER_ID + 4, folderMap       , FolderIDPrefKey.PICTURES  );
      createSpecialFolderRecord(DEFAULT_XML_FOLDER_ID + 5, folderMap       , FolderIDPrefKey.TOPICAL   );
      createSpecialFolderRecord(DEFAULT_XML_FOLDER_ID + 6, folderMap       , FolderIDPrefKey.UNENTERED );
      createSpecialFolderRecord(DEFAULT_XML_FOLDER_ID + 7, folderMap       , FolderIDPrefKey.RESULTS   );
    }
    catch (RelationCycleException | DuplicateRecordException | SearchKeyException | RestoreException e)
    {
      throw new HDB_InternalError(64642);
    }

    resolvePointers();

    loaded = true;
    updateRunningInstancesFile(rootFilePath);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createSpecialFolderRecord(int id, Map<String, String> folderMap, String prefKey) throws DuplicateRecordException, RelationCycleException, HDB_InternalError, SearchKeyException, RestoreException
  {
    createSpecialFolderRecord(id, folderMap.get(prefKey), prefKey);
  }

  private void createSpecialFolderRecord(int id, String name, String prefKey) throws DuplicateRecordException, RelationCycleException, HDB_InternalError, SearchKeyException, RestoreException
  {
    RecordState recordState = new RecordState(hdtFolder, id);
    ((HDI_OfflinePath) recordState.items.get(tagFileName)).setFileName(name);
    ((HDI_OfflinePath) recordState.items.get(tagParentFolder)).setFolderID(ROOT_FOLDER_ID);
    createNewRecordFromState(recordState, true);
    prefs.putInt(prefKey, recordState.id);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<Tag> getTagsByRecordType(RecordType recordType, boolean substituteMainText)
  {
    EnumSet<Tag> tags = EnumSet.noneOf(Tag.class);

    if (datasets.containsKey(recordType))
      tags.addAll(datasets.get(recordType).getTags(false, substituteMainText));
    else
      tags = datasets.values().stream().flatMap(dataset -> dataset.getTags(false, substituteMainText).stream())
                                       .collect(toCollection(() -> EnumSet.noneOf(Tag.class)));
    return tags;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDI_Schema getSchema(RecordType type, Tag tag)
  {
    return nullSwitch(datasets.get(type), null, dataset -> dataset.getSchema(tag));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Collection<HDI_Schema> getSchemasByRecordType(RecordType type)
  {
    return nullSwitch(datasets.get(type), null, HyperDataset::getSchemas);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDI_Schema> getSchemasByTag(Tag tag)
  {
    return datasets.values().stream().map(dataset -> dataset.getSchema(tag)).filter(Objects::nonNull).collect(toSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int DEFAULT_XML_FOLDER_ID = 2; // 2 is the default for new databases. Old ones may have 9 as the XML folder ID.

  public static final int ROOT_FOLDER_ID = 1;

  public static final Charset XML_FILES_CHARSET = StandardCharsets.UTF_8;

  public static final String

    BIB_FILE_NAME = "Bib.json",
    ZOTERO_TEMPLATE_FILE_NAME = "ZoteroTemplates.json",
    ZOTERO_CREATOR_TYPES_FILE_NAME = "ZoteroCreatorTypes.json",
    DESC_TEMPLATE_FOLDER_NAME = "Description templates",
    DEFAULT_XML_PATH = "XML",
    DEFAULT_PICTURES_PATH = "Pictures",
    DEFAULT_BOOKS_PATH = "Books",
    DEFAULT_PAPERS_PATH = "Papers",
    DEFAULT_UNENTERED_PATH = "Works not entered yet",
    DEFAULT_MISC_FILES_PATH = "Misc",
    DEFAULT_RESULTS_PATH = "Search results",
    DEFAULT_TOPICAL_PATH = "Topical",
    SETTINGS_FILE_NAME = "Settings.xml";

  protected static final String

    LOCK_FILE_NAME = "dblock.dat",
    REQUEST_MSG_FILE_NAME = "request_message.dat",
    RESPONSE_MSG_FILE_NAME = "response_message.dat";

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

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isProtectedFile(FilePath filePath, boolean checkSubfolders)
  {
    if (filePath.equals(rootFilePath)) return true;

    HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath, false);

    if ((folder != null) && folder.filePath().equals(filePath) && isProtectedRecord(folder, checkSubfolders))
      return true;

    FilePath xmlPath = xmlPath();

    if ((filePath.getParent().equals(rootFilePath) == false) &&
        (filePath.getParent().equals(xmlPath     ) == false))   return false;

    return filePath.equals(hdbFilePath) ||
           filePath.equals(getRequestMessageFilePath(false)) ||
           filePath.equals(getResponseMessageFilePath(false)) ||
           filePath.equals(getLockFilePath(false)) ||
           filePath.equals(xmlPath.resolve(SETTINGS_FILE_NAME     )) ||
           filePath.equals(xmlPath.resolve(PERSON_FILE_NAME       )) ||
           filePath.equals(xmlPath.resolve(PERSON_FILE_NAME       )) ||
           filePath.equals(xmlPath.resolve(OTHER_FILE_NAME        )) ||
           filePath.equals(xmlPath.resolve(INSTITUTION_FILE_NAME  )) ||
           filePath.equals(xmlPath.resolve(INVESTIGATION_FILE_NAME)) ||
           filePath.equals(xmlPath.resolve(DEBATE_FILE_NAME       )) ||
           filePath.equals(xmlPath.resolve(ARGUMENT_FILE_NAME     )) ||
           filePath.equals(xmlPath.resolve(POSITION_FILE_NAME     )) ||
           filePath.equals(xmlPath.resolve(WORK_FILE_NAME         )) ||
           filePath.equals(xmlPath.resolve(TERM_FILE_NAME         )) ||
           filePath.equals(xmlPath.resolve(FILE_FILE_NAME         )) ||
           filePath.equals(xmlPath.resolve(NOTE_FILE_NAME         )) ||
           filePath.equals(xmlPath.resolve(HUB_FILE_NAME          )) ||
           filePath.equals(xmlPath.resolve(BIB_FILE_NAME          ));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean getRelatives(HDT_RecordWithPath record, LinkedHashSet<HDT_Record> set, int max, boolean gettingNotesForFile)
  {
    set.clear();

    if (gettingNotesForFile)
    {
      // If gettingNotesForFile is true, then there are two scenarios.
      //   1. If "record" is a folder, then we are getting the "relatives" for a file with no associated record, i.e., any note record associated with the parent folder.
      //   2. If "record" is NOT a folder, then we are getting relatives of a file with an associated record in the File Manager and want to include any notes associated with the parent folder,
      //      as well as the record's other relatives.

      if (record.getType() == hdtFolder)
      {
        HDT_Folder folder = (HDT_Folder)record;

        for (HDT_Note note : folder.notes)
        {
          if (set.size() == max)
            return true;

          set.add(note);
        }

        return false; // We are getting "relatives" of a file row with no associated record; only the parent folder's notes should be returned in this case.
      }

      HDT_Folder folder = record.parentFolder();
      if ((folder != null) && (folder.notes.size() > 0))
      {
        for (HDT_Note note : folder.notes)
        {
          if (set.size() == max)
            return true;

          set.add(note);
        }
      }
    }

    for (RelationType relType : getRelationsForSubjType(record.getType(), false))
    {
      if (relType != rtParentFolderOfFolder)
      {
        HyperObjList<HDT_Record, HDT_Record> list = getObjectList(relType, record, false);

        for (HDT_Record obj : list)
        {
          if (set.size() == max)
            return true;

          set.add(obj);
        }
      }
    }

    for (RelationType relType : getRelationsForObjType(record.getType(), false))
    {
      if (relType != rtParentFolderOfFolder)
      {
        HyperSubjList<HDT_Record, HDT_Record> list = getSubjectList(relType, record);

        for (HDT_Record subj : list)
        {
          if (set.size() == max)
            return true;

          set.add(subj);
        }
      }
    }

    return false;
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

  public void handleKeyWork(HDT_RecordWithMainText record, HDT_RecordWithAuthors<? extends Authors> keyWorkRecord, boolean affirm)
  {
    if (affirm)
      keyWorkIndex.computeIfAbsent(keyWorkRecord, _keyWorkRecord -> new HashSet<>()).add(record);
    else
      nullSwitch(keyWorkIndex.get(keyWorkRecord), set -> set.remove(record));

    nullSwitch(keyWorkHandlers.get(record.getType()), handler -> runInFXThread(() -> handler.handle(keyWorkRecord, record, affirm)));
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

  public Stream<HDT_RecordWithMainText> displayerStream(HDT_RecordWithMainText displayed)
  {
    return displayedAtIndex.getForwardStream(displayed.getMainText()).filter(Predicate.not(mainText ->
    {
      HDT_RecordWithMainText record = mainText.getRecord();
      return HDT_Record.isEmpty(record, false) || (record.getMainText() != mainText);

    })).map(displayerText -> displayerText.getRecord().mainSpoke());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a stream of the {@link HDT_RecordWithMainText} records having the provided record as a key work.
   *
   * @param record Key work record
   * @param mainSpokes a boolean flag indicating whether to return all key work mentioners or just main spokes
   * @return a stream of {@link HDT_RecordWithMainText} objects; an empty stream if the input record is not found in the keyWorkIndex
   */
  public Stream<HDT_RecordWithMainText> keyWorkMentionerStream(HDT_RecordWithAuthors<? extends Authors> record, boolean mainSpokes)
  {
    Set<HDT_RecordWithMainText> set = keyWorkIndex.get(record);

    if (set == null) return Stream.empty();

    Stream<HDT_RecordWithMainText> stream = set.stream();

    return mainSpokes ?
      stream.map(HDT_RecordWithMainText::mainSpoke).distinct()
    :
      stream;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public <HDT_MT extends HDT_RecordWithMainText> Stream<HDT_MT> keyWorkMentionerStream(HDT_RecordWithAuthors<? extends Authors> record, Class<HDT_MT> klazz)
  {
    return nullSwitch(keyWorkIndex.get(record),
                      Stream.empty(),
                      set -> set.stream().filter(recordWMT -> recordWMT.getClass() == klazz).map(klazz::cast));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<HDT_RecordWithMainText> keyWorkMentionerStream(HDT_RecordWithAuthors<? extends Authors> record, RecordType mentionerType)
  {
    return nullSwitch(keyWorkIndex.get(record),
                      Stream.empty(),
                      set -> set.stream().filter(recordWMT -> recordWMT.getType() == mentionerType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void unmapFilePath(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    String name = filePath.getNameOnly().toString();
    Set<HyperPath> paths = filenameMap.get(name);

    if (paths == null) return;

    paths.removeIf(path -> filePath.equals(path.filePath()));

    if (paths.isEmpty())
      filenameMap.remove(name);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Folder getImportFolderForWorkType(WorkTypeEnum workTypeEnum)
  {
    return switch (workTypeEnum)
    {
      case wtBook,
           wtChapter -> booksFolder;
      case wtPaper   -> papersFolder;
      case wtThesis  -> prefs.getBoolean(PrefKey.THESIS_FOLDER_IS_BOOKS, false) ? booksFolder : papersFolder;
      default        -> miscFilesFolder;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setSpecialFolder(String prefKey, HDT_Folder folder)
  {
    prefs.putInt(prefKey, folder.getID());

    switch (prefKey)
    {
      case FolderIDPrefKey.PICTURES   : picturesFolder  = folder; break;
      case FolderIDPrefKey.BOOKS      : booksFolder     = folder; break;
      case FolderIDPrefKey.PAPERS     : papersFolder    = folder; break;
      case FolderIDPrefKey.RESULTS    : resultsFolder   = folder; break;
      case FolderIDPrefKey.UNENTERED  : unenteredFolder = folder; break;
      case FolderIDPrefKey.MISC_FILES : miscFilesFolder = folder; break;
      case FolderIDPrefKey.TOPICAL    : topicalFolder   = folder; break;

      default                         : throw newAssertionError(new HDB_InternalError(59294));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath resolveExtFilePath(String url)
  {
    return (url != null) && url.startsWith(EXT_1) && (extPath() != null) ?
      extPath().resolve(FilenameUtils.separatorsToSystem(url.substring(7)))
    :
      null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
