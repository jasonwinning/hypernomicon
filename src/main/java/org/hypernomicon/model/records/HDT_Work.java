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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.WorkBibData;
import org.hypernomicon.dialogs.UpdateISBNsDlgCtrlr;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.WorkAuthors;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjList;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.settings.WorkSearchKeySettings;
import org.hypernomicon.settings.WorkSearchKeySettings.WorkSearchKeyConfig;
import org.hypernomicon.util.SplitString;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.tabs.WorkTabCtrlr;

public class HDT_Work extends HDT_RecordWithMainText implements HDT_RecordWithPath, HDT_RecordWithAuthors<WorkAuthors>, Comparable<HDT_Work>
{
  private final WorkAuthors authors;

  public final List<HDT_Person> authorRecords;
  public final List<HDT_WorkFile> workFiles;

  public final HyperSubjList<HDT_Work    , HDT_Work> subWorks;
  public final HyperSubjList<HDT_MiscFile, HDT_Work> miscFiles;
  public final HyperSubjList<HDT_Argument, HDT_Work> arguments;

  public final HyperObjPointer<HDT_Work, HDT_WorkType> workType;
  public final HyperObjPointer<HDT_Work, HDT_Work> largerWork;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Work(RecordState xmlState, HyperDataset<HDT_Work> dataset)
  {
    super(xmlState, dataset, tagTitle);

    authors = new WorkAuthors(getObjList(rtAuthorOfWork), this);

    authorRecords = Collections.unmodifiableList(getObjList(rtAuthorOfWork));
    workFiles     = Collections.unmodifiableList(getObjList(rtWorkFileOfWork));

    subWorks  = getSubjList(rtParentWorkOfWork);
    miscFiles = getSubjList(rtWorkOfMiscFile);
    arguments = getSubjList(rtWorkOfArgument);

    workType   = getObjPointer(rtTypeOfWork);
    largerWork = getObjPointer(rtParentWorkOfWork);
  }

//---------------------------------------------------------------------------

  public WorkTypeEnum getWorkTypeEnum()             { return HDT_WorkType.workTypeIDToEnumVal(workType.getID()); }
  public String getYear()                           { return getBibDateInternal().getYearStr(); }
  public BibliographicDate getBibDate()             { return getBibDateInternal(); }
  public String getBibEntryKey()                    { return getBibEntryKeyInternal(); }
  public String getMiscBib()                        { return getTagString(tagMiscBib); }
  public String getDOI()                            { return getTagString(tagDOI); }
  public List<String> getISBNs()                    { return matchISBN(getTagString(tagISBN)); }
  public String getURL()                            { return getTagString(tagWebURL); }
  public boolean canLaunch()                        { return ! (getPath().isEmpty() && getURL().isEmpty()); }

  public void setWorkType(WorkTypeEnum val)         { workType.set(HDT_WorkType.get(val)); }
  public void setBibDate(BibliographicDate bibDate) { updateBibDate(bibDate); }
  public void setYear(String str)                   { updateBibDate(getBibDateInternal().setYear(str)); }
  public void setBibEntryKey(String str)            { updateBibEntryKey(str); }
  public void setMiscBib(String str)                { updateTagString(tagMiscBib, str); }
  public void setDOI(String str)                    { updateTagString(tagDOI, matchDOI(str)); }
  public void setURL(String str)                    { updateTagString(tagWebURL, str); }

  @Override public String listName()                { return name(); }
  @Override public HyperPath getPath()              { return workFiles.isEmpty() ? HyperPath.EmptyPath : workFiles.get(0).getPath(); }
  @Override public WorkAuthors getAuthors()         { return authors; }

//---------------------------------------------------------------------------

  public Set<HDT_Investigation> investigationSet()       { return investigationStream().collect(Collectors.toSet()); }

  public Stream<HDT_Investigation> investigationStream() { return db.keyWorkMentionerStream(this, HDT_Investigation.class); }
  public Stream<HDT_WorkLabel    > labelStream        () { return db.keyWorkMentionerStream(this, HDT_WorkLabel    .class); }

//---------------------------------------------------------------------------

  public Ternary personIsInFileName(HDT_Person person)                       { return db.getNestedTernary(this, person, tagInFileName); }
  public void setPersonIsInFileName(HDT_Person person, Ternary inFileName)   { db.updateNestedTernary(this, person, tagInFileName, inFileName); }
  public boolean personIsEditor(HDT_Person person)                           { return db.getNestedBoolean(this, person, tagEditor); }
  public void setPersonIsEditor(HDT_Person person, boolean isEditor)         { db.updateNestedBoolean(this, person, tagEditor, isEditor); }
  public boolean personIsTranslator(HDT_Person person)                       { return db.getNestedBoolean(this, person, tagTranslator); }
  public void setPersonIsTranslator(HDT_Person person, boolean isTranslator) { db.updateNestedBoolean(this, person, tagTranslator, isTranslator); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // pass -1 to clear the value

  public void setStartPageNum(HDT_WorkFile workFile, int val)
  {
    if (workFile == null)
      updateTagString(tagStartPageNum, val < 0 ? "" : String.valueOf(val));
    else
      db.updateNestedString(this, workFile, tagStartPageNum, val < 0 ? "" : String.valueOf(val));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // pass -1 to clear the value

  public void setEndPageNum(HDT_WorkFile workFile, int val)
  {
    if (workFile == null)
      updateTagString(tagEndPageNum, val < 0 ? "" : String.valueOf(val));
    else
      db.updateNestedString(this, workFile, tagEndPageNum, val < 0 ? "" : String.valueOf(val));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getStartPageNumStr(HDT_WorkFile workFile)
  {
    return workFile == null ? getStartPageNumStr() : db.getNestedString(this, workFile, tagStartPageNum);
  }

  private String getEndPageNumStr(HDT_WorkFile workFile)
  {
    return workFile == null ? getEndPageNumStr() : db.getNestedString(this, workFile, tagEndPageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getStartPageNumStr()
  {
    if (workFiles.isEmpty() == false)
      return getStartPageNumStr(workFiles.get(0));

    if (getURL().startsWith(EXT_1))
      return getTagString(tagStartPageNum);

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getEndPageNumStr()
  {
    if (workFiles.isEmpty() == false)
      return getEndPageNumStr(workFiles.get(0));

    if (getURL().startsWith(EXT_1))
      return getTagString(tagEndPageNum);

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getStartPageNum(HDT_WorkFile workFile) { return parseInt(getStartPageNumStr(workFile), -1); }
  public int getEndPageNum  (HDT_WorkFile workFile) { return parseInt(getEndPageNumStr  (workFile), -1); }
  public int getStartPageNum(                     ) { return parseInt(getStartPageNumStr(        ), -1); }
  public int getEndPageNum  (                     ) { return parseInt(getEndPageNumStr  (        ), -1); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String resultTextForTag(Tag tag)
  {
    return switch (tag)
    {
      case tagStartPageNum -> getStartPageNumStr();
      case tagEndPageNum   -> getEndPageNumStr();
      default              -> super.resultTextForTag(tag);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String fixCase(String title)
  {
    return db.prefs.getBoolean(PREF_KEY_SENTENCE_CASE, false) ? sentenceCase(title) : titleCase(title);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isUnenteredSet(HDT_Work work)
  {
    return (work != null) && (work.getWorkTypeEnum() == WorkTypeEnum.wtUnenteredSet);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setAuthors(List<ObjectGroup> newGroups)
  {
    boolean theSame = true;

    if (newGroups.size() != authors.size())
      theSame = false;
    else
    {
      for (int ndx = 0; ndx < newGroups.size(); ndx++)
        if (authors.get(ndx).equalsObjGroup(newGroups.get(ndx)) == false)
          theSame = false;
    }

    if (theSame) return;

    authors.update(newGroups);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getCBText()
  {
    String authorStr = getShortAuthorsStr(false),
           yearStr = getYear(),
           titleStr = name(),
           cbStr = "";

    if (authorStr.length() > 0)
      cbStr = authorStr + ' ';

    if (yearStr.length() > 0)
      cbStr += '(' + yearStr + ") ";

    if (titleStr.length() > 0)
      cbStr += titleStr;

    return cbStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setLargerWork(int newID, boolean noIsbnUpdate)
  {
    if (largerWork.getID() == newID) return;

    largerWork.setID(newID);

    if (newID < 1) return;
    HDT_Work largerWorkRec = db.works.getByID(newID);

    /***********************************************/
    /*          Update ISBNs                       */
    /***********************************************/

    if (noIsbnUpdate == false)
    {
      List<String> ISBNs = getISBNs(), lwISBNs = largerWorkRec.getISBNs();

      boolean notAllInLW = ISBNs  .stream().anyMatch(Predicate.not(lwISBNs::contains)),
              notAllInSW = lwISBNs.stream().anyMatch(Predicate.not(ISBNs  ::contains));

      if ((notAllInLW == false) && notAllInSW)
      {
        nonInteractivelyUpdateISBNsForThisWorkAndDescendants(largerWorkRec.getTagString(tagISBN));
      }
      else if (notAllInLW)
      {
        interactivelyUpdateISBNsForThisWorkAndDescendants(largerWorkRec);
      }
    }

    if (largerWorkRec.workFiles.isEmpty()) return;

    /***********************************************/
    /*          Update work files                  */
    /***********************************************/

    if (workFiles.isEmpty())
    {
      largerWorkRec.workFiles.forEach(workFile -> addWorkFile(workFile.getID()));
      return;
    }

    if ((workFiles.size() != largerWorkRec.workFiles.size()) || (largerWorkRec.workFiles.containsAll(workFiles) == false))
    {
      String msg = workFiles.size() == 1 ? " file is " : " files are ";
      if (confirmDialog("Currently, " + workFiles.size() + msg + "attached to the child work. Replace with parent work file(s)?"))
      {
        getObjList(rtWorkFileOfWork).clear();
        largerWorkRec.workFiles.forEach(workFile -> addWorkFile(workFile.getID()));
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void replaceWorkFile(HDT_WorkFile oldWorkFile, HDT_WorkFile workFile)
  {
    HyperObjList<HDT_Work, HDT_WorkFile> workFileList = getObjList(rtWorkFileOfWork);

    int ndx = workFileList.indexOf(oldWorkFile);

    if (ndx > -1)
      workFileList.set(ndx, workFile);

    subWorks.forEach(childWork -> childWork.replaceWorkFile(oldWorkFile, workFile));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_Work sourceUnenteredWork = null;

  public void addWorkFile(int newID)
  {
    HDT_WorkFile workFile = db.workFiles.getByID(newID);

    getObjList(rtWorkFileOfWork).add(workFile);
    clearExtFilePageNums();

    workFile.works.forEach(work ->
    {
      if ((work.getID() != getID()) && isUnenteredSet(work) &&
        ((sourceUnenteredWork == work) || confirmDialog("Okay to remove the file from the the unentered set of work files: \"" + work.name() + "\"?")))
          db.getObjectList(rtWorkFileOfWork, work, true).remove(workFile);
    });

    subWorks.stream().filter(childWork -> childWork.workFiles.isEmpty())
                     .forEachOrdered(childWork -> childWork.addWorkFile(newID));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clearExtFilePageNums()
  {
    updateTagString(tagStartPageNum, "");
    updateTagString(tagEndPageNum  , "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void launch(int pageNum)
  {
    if (workFiles.isEmpty() && getURL().isBlank()) return;

    viewNow();

    if (pageNum < 1) pageNum = getStartPageNum();

    if (getPath().isEmpty() == false)
    {
      launchWorkFile(filePath(), pageNum);
      return;
    }

    String url = getURL();

    if (url.startsWith(EXT_1) && (extPath() == null))
    {
      warningPopup(WorkTabCtrlr.NO_EXT_PATH_MESSAGE);
      return;
    }

    FilePath filePath = resolveExtFilePath(url);

    if (FilePath.isEmpty(filePath))
      openWebLink(url);
    else
      launchWorkFile(filePath, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean hasLaunchableWork(List<HDT_Work> works)
  {
    return works.stream().anyMatch(HDT_Work::canLaunch);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_Work getLaunchableWork(Iterable<HDT_Work> works)
  {
    HDT_Work work = findFirst(works, HDT_Work::pathNotEmpty);
    if (work == null)
      work = findFirst(works, wrk -> wrk.getURL().isEmpty() == false);

    return work;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String addFileIndicator(String str, HDT_RecordWithPath record)
  {
    if (record == null) return str;

    String indicator = getFileIndicator(record);

    return indicator.isBlank() ? str : (str + " (" + indicator + ')').trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getFileIndicator(HDT_RecordWithPath record)
  {
    if (record == null) return "";

    FilePath filePath = record.filePath();
    if (FilePath.isEmpty(filePath) == false)
      return filePath.getExtensionOnly().toLowerCase();

    if (record.getType() == hdtWork)
    {
      HDT_Work work = (HDT_Work)record;

      if (work.getURL().isBlank() == false)
        return nullSwitch(resolveExtFilePath(work.getURL()), "web", filePath2 -> filePath2.getExtensionOnly().toLowerCase());
    }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void interactivelyUpdateISBNsForThisWorkAndDescendants(HDT_Work orig)
  {
    List<HDT_Work> list = new ArrayList<>();

    addThisWorkAndDescendantsToList(list);

    list.remove(orig);

    if (list.isEmpty()) return;

    UpdateISBNsDlgCtrlr dlg = new UpdateISBNsDlgCtrlr(orig, list);

    if (dlg.showModal() == false) return;

    String isbnStr = orig.getTagString(tagISBN);

    dlg.worksToUpdate().forEach(work -> work.updateTagString(tagISBN, isbnStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addThisWorkAndDescendantsToList(List<HDT_Work> list)
  {
    list.add(this);

    for (HDT_Work subWork : subWorks)
      subWork.addThisWorkAndDescendantsToList(list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void nonInteractivelyUpdateISBNsForThisWorkAndDescendants(String newISBNs)
  {
    updateTagString(tagISBN, newISBNs);

    subWorks.forEach(child -> child.nonInteractivelyUpdateISBNsForThisWorkAndDescendants(newISBNs));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setISBNs(List<String> list)
  {
    List<String> allIsbns = new ArrayList<>();

    list.forEach(isbn -> matchISBN(isbn, allIsbns));

    List<String> curISBNs = getISBNs();

    if (curISBNs.containsAll(allIsbns) && allIsbns.containsAll(curISBNs))
      return;

    String isbnStr = allIsbns.stream().reduce((s1, s2) -> s1 + "; " + s2).orElse("");

    updateTagString(tagISBN, isbnStr);

    HDT_Work ancestor = this;

    while (ancestor.largerWork.isNotNull())
      ancestor = ancestor.largerWork.get();

    ancestor.interactivelyUpdateISBNsForThisWorkAndDescendants(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibData getBibData()
  {
    String entryKey = getBibEntryKey();

    if (entryKey.length() > 0)
    {
      BibData bibData = db.getBibEntryByKey(entryKey);
      if (bibData != null) return bibData;
    }

    return new WorkBibData(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean canPreview()
  {
    return FilePath.isEmpty(filePathIncludeExt()) == false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath filePathIncludeExt()
  {
    return workFiles.isEmpty() ? resolveExtFilePath(getURL()) : filePath();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // This function is similar to BibEntry.comparator()

  @Override public int compareTo(HDT_Work otherWork)
  {
    if (otherWork == null) return 1;

    int cResult, numAuthors = Math.max(getAuthors().size(), otherWork.getAuthors().size());

    for (int ndx = 0; ndx < numAuthors; ndx++)
    {
      if ((ndx >= getAuthors().size()) || (ndx >= otherWork.getAuthors().size()))
        return getAuthors().size() - otherWork.getAuthors().size();

      cResult = getAuthors().get(ndx).compareTo(otherWork.getAuthors().get(ndx));
      if (cResult != 0) return cResult;
    }

    cResult = compareYears(getYear(), otherWork.getYear());
    if (cResult != 0) return cResult;

    return getSortKey().compareTo(otherWork.getSortKey());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Author getSingleAuthorForSearchKey(List<Author> authors)
  {
    for (Author author : authors) if ((author.getIsEditor() == false) && (author.getIsTrans() == false) && (author.getPerson() != null)) return author;
    for (Author author : authors) if (                                   (author.getIsTrans() == false) && (author.getPerson() != null)) return author;
    for (Author author : authors) if ((author.getIsEditor() == false) && (author.getIsTrans() == false) && (author.getPerson() == null)) return author;
    for (Author author : authors) if (                                   (author.getIsTrans() == false) && (author.getPerson() == null)) return author;
    for (Author author : authors) if                                                                       (author.getPerson() != null)  return author;
    for (Author author : authors) if                                                                       (author.getPerson() == null)  return author;

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String makeWorkSearchKey(boolean addLetter, boolean keyWorkLink)
  {
    return makeWorkSearchKey(getAuthors().asList(), getYear(), addLetter, keyWorkLink);
  }

  public String makeWorkSearchKey(List<Author> authorsToUse, String yearToUse, boolean addLetter, boolean keyWorkLink)
  {
    if ((authorsToUse == null) || authorsToUse.isEmpty() || safeStr(yearToUse).isBlank())
      return "";

    WorkSearchKeySettings settings = WorkSearchKeySettings.loadFromPrefNode();
    String singleAuthorName = getSingleAuthorForSearchKey(authorsToUse).singleName();

    return settings.format(authorsToUse.stream().map(Author::singleName).toList(), singleAuthorName, yearToUse, addLetter, this, keyWorkLink);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String makeKeyWorkSearchKey()
  {
    String searchKey = makeWorkSearchKey(true, true);

    if (searchKey.isEmpty() == false)
      return searchKey;

    String title = ultraTrim(new SplitString(name(), ':').next());
    if (title.isBlank())
      return "Work ID=" + getID();

    if ((getYear().isBlank() == false) || largerWork.isNull())
      return makeKeyFromYearAndTitle();

    String lwTitle = ultraTrim(new SplitString(largerWork.get().name(), ':').next());
    if (lwTitle.isBlank())
      return makeKeyFromYearAndTitle();

    String lwSearchKey = largerWork.get().makeWorkSearchKey(true, true);

    if (lwSearchKey.isEmpty())
      lwSearchKey = new SplitString(largerWork.get().getSearchKey(), ';').next();

    if (lwSearchKey.isEmpty())
      lwSearchKey = largerWork.get().makeKeyFromYearAndTitle();

    return title + " in " + lwSearchKey;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String makeKeyFromYearAndTitle()
  {
    String title = ultraTrim(new SplitString(name(), ':').next());

    if (title.isBlank())
      return "";

    if (getYear().isBlank())
      return title;

    WorkSearchKeySettings settings = WorkSearchKeySettings.loadFromPrefNode();
    WorkSearchKeyConfig configToUse = settings.stream().filter(keyConfig -> keyConfig.multipleAuthors == false).findFirst().orElse(settings.get(0));

    return switch (configToUse.parentheses)
    {
      case aroundAll  -> '(' + getYear() + ' '  + title + ')';
      case aroundYear -> '(' + getYear() + ") " + title;
      default         -> getYear() + ' ' + title;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
