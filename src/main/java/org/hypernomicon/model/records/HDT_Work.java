/*
 * Copyright 2015-2021 Jason Winning
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
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;

import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.WorkBibData;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.WorkAuthors;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithAuthors;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjList;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.util.filePath.FilePath;

public class HDT_Work extends HDT_RecordWithConnector implements HDT_RecordWithPath, HDT_RecordWithAuthors<WorkAuthors>
{
  private final WorkAuthors authors;

  public final List<HDT_Person> authorRecords;
  public final HyperObjList<HDT_Work, HDT_Investigation> investigations;
  public final HyperObjList<HDT_Work, HDT_WorkLabel> labels;
  public final List<HDT_WorkFile> workFiles;
  public final HyperSubjList<HDT_Work, HDT_Work> subWorks;
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
    investigations = getObjList(rtInvestigationOfWork);
    labels = getObjList(rtLabelOfWork);
    workFiles = Collections.unmodifiableList(getObjList(rtWorkFileOfWork));

    subWorks = getSubjList(rtParentWorkOfWork);
    miscFiles = getSubjList(rtWorkOfMiscFile);
    arguments = getSubjList(rtWorkOfArgument);

    workType = getObjPointer(rtTypeOfWork);
    largerWork = getObjPointer(rtParentWorkOfWork);
  }

  public void setInvestigations(List<HDT_Investigation> list) { updateObjectsFromList(rtInvestigationOfWork, list); }
  public void setWorkLabels(List<HDT_WorkLabel> list)         { updateObjectsFromList(rtLabelOfWork, list); }

  public WorkTypeEnum getWorkTypeEnum()     { return HDT_WorkType.workTypeIDToEnumVal(workType.getID()); }
  public void setWorkType(WorkTypeEnum val) { workType.set(HDT_WorkType.get(val)); }

  @Override public String listName()        { return name(); }
  @Override public HyperPath getPath()      { return workFiles.isEmpty() ? HyperPath.EmptyPath : workFiles.get(0).getPath(); }
  @Override public WorkAuthors getAuthors() { return authors; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getYear()        { return getTagString(tagYear); }
  public String getBibEntryKey() { return getBibEntryKeyString(); }
  public String getMiscBib()     { return getTagString(tagMiscBib); }
  public String getDOI()         { return getTagString(tagDOI); }
  public List<String> getISBNs() { return matchISBN(getTagString(tagISBN)); }
  public String getURL()         { return getTagString(tagWebURL); }
  public boolean canLaunch()     { return ! (getPath().isEmpty() && getURL().isEmpty()); }

  public void setYear(String str)        { updateTagString(tagYear, str); }
  public void setBibEntryKey(String str) { updateBibEntryKey(str); }
  public void setMiscBib(String str)     { updateTagString(tagMiscBib, str); }
  public void setDOI(String str)         { updateTagString(tagDOI, matchDOI(str)); }
  public void setURL(String str)         { updateTagString(tagWebURL, str); }

//---------------------------------------------------------------------------
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

  public int getStartPageNum(HDT_WorkFile workFile)
  {
    return  workFile == null ? getStartPageNum() : parseInt(db.getNestedString(this, workFile, tagStartPageNum), -1);
  }

  public int getEndPageNum(HDT_WorkFile workFile)
  {
    return workFile == null ? getEndPageNum() : parseInt(db.getNestedString(this, workFile, tagEndPageNum), -1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getStartPageNum()
  {
    if (workFiles.isEmpty() == false)
      return getStartPageNum(workFiles.get(0));

    if (getURL().startsWith(EXT_1))
      return parseInt(getTagString(tagStartPageNum), -1);

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getEndPageNum()
  {
    if (workFiles.isEmpty() == false)
      return getEndPageNum(workFiles.get(0));

    if (getURL().startsWith(EXT_1))
      return parseInt(getTagString(tagEndPageNum), -1);

    return -1;
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
    return work == null ? false : work.getWorkTypeEnum() == WorkTypeEnum.wtUnenteredSet;
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
      cbStr = authorStr + " ";

    if (yearStr.length() > 0)
      cbStr += "(" + yearStr + ") ";

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
    HDT_Work largerWork = db.works.getByID(newID);

    /***********************************************/
    /*          Update ISBNs                       */
    /***********************************************/

    if (noIsbnUpdate == false)
    {
      List<String> ISBNs = getISBNs(), lwISBNs = largerWork.getISBNs();

      boolean notAllInLW = false, notAllInSW = false;

      for (String isbn : ISBNs)
        if (lwISBNs.contains(isbn) == false)
          notAllInLW = true;

      for (String isbn : lwISBNs)
        if (ISBNs.contains(isbn) == false)
          notAllInSW = true;

      if ((notAllInLW == false) && notAllInSW)
      {
        updateISBNstrRecursively(largerWork.getTagString(tagISBN));
      }
      else if (notAllInLW)
      {
        if (confirmDialog("Recursively update ISBN(s) for contained/container works?"))
        {
          String isbnStr = lwISBNs.stream().filter(Predicate.not(ISBNs::contains)).reduce((s1, s2) -> s1 + "; " + s2).orElse("");

          HDT_Work ancestor = this;
          while (ancestor.largerWork.isNotNull())
            ancestor = ancestor.largerWork.get();

          ancestor.updateISBNstrRecursively(isbnStr);
        }
      }
    }

    if (largerWork.workFiles.isEmpty()) return;

    /***********************************************/
    /*          Update work files                  */
    /***********************************************/

    if (workFiles.isEmpty())
    {
      largerWork.workFiles.forEach(workFile -> addWorkFile(workFile.getID()));
      return;
    }

    if ((workFiles.size() != largerWork.workFiles.size()) || (workFiles.stream().allMatch(largerWork.workFiles::contains) == false))
    {
      String msg = workFiles.size() == 1 ? " file is " : " files are ";
      if (confirmDialog("Currently, " + workFiles.size() + msg + "attached to the child work. Replace with parent work file(s)?"))
      {
        getObjList(rtWorkFileOfWork).clear();
        largerWork.workFiles.forEach(workFile -> addWorkFile(workFile.getID()));
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
      if ((work.getID() != getID()) && HDT_Work.isUnenteredSet(work) &&
        ((sourceUnenteredWork == work) || confirmDialog("Okay to remove the file from the the unentered set of work files: \"" + work.name() + "\"?")))
          db.getObjectList(rtWorkFileOfWork, work, true).remove(workFile);
    });

    subWorks.stream().filter(childWork -> childWork.workFiles.isEmpty())
                     .forEach(childWork -> childWork.addWorkFile(newID));
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

    if (getPath().isEmpty())
    {
      String url = getURL();

      if (url.startsWith(EXT_1))
        launchWorkFile(db.resolveExtFilePath(url), pageNum);
      else
        openWebLink(url);
    }

    launchWorkFile(filePath(), pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean hasLaunchableWork(List<HDT_Work> works)
  {
    return findFirst(works, HDT_Work::canLaunch) != null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_Work getLaunchableWork(List<HDT_Work> works)
  {
    HDT_Work work = findFirst(works, HDT_Work::pathNotEmpty);
    if (work == null)
      work = findFirst(works, wrk -> wrk.getURL().isEmpty() == false);

    return work;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getInvText(HDT_Person person)
  {
    return investigations.stream().filter(inv -> inv.person.get() == person)
                                  .map(HDT_Investigation::listName)
                                  .reduce((s1, s2) -> s1 + ", " + s2).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String addFileIndicator(String str, HDT_Work work)
  {
    if (work == null) return str;

    String indicator = "";

    if (work.workFiles.isEmpty() == false)
      indicator = work.workFiles.get(0).filePath().getExtensionOnly().toLowerCase();
    else if (safeStr(work.getURL()).length() > 0)
      indicator = "web";

    return indicator.isEmpty() ? str : new String(str + " (" + indicator + ")").trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateISBNstrRecursively(String newISBNs)
  {
    updateTagString(tagISBN, newISBNs);

    subWorks.forEach(child -> child.updateISBNstrRecursively(newISBNs));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setISBNs(List<String> list)
  {
    List<String> allIsbns = new ArrayList<>();

    list.forEach(isbn -> matchISBN(isbn, allIsbns));

    List<String> curISBNs = getISBNs();

    if (allIsbns.stream().allMatch(curISBNs::contains) &&
        curISBNs.stream().allMatch(allIsbns::contains))
      return;

    String isbnStr = allIsbns.stream().reduce((s1, s2) -> s1 + "; " + s2).orElse("");

    if (largerWork.isNotNull())
    {
      if (confirmDialog("Recursively update ISBN(s) for contained/container works?"))
      {
        HDT_Work ancestor = this;
        while (ancestor.largerWork.isNotNull())
          ancestor = ancestor.largerWork.get();

        ancestor.updateISBNstrRecursively(isbnStr);
        return;
      }
    }
    else if (subWorks.isEmpty() == false)
    {
      if (confirmDialog("Recursively update ISBN(s) for contained/container works?"))
      {
        updateISBNstrRecursively(isbnStr);
        return;
      }
    }

    updateTagString(tagISBN, isbnStr);
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
    return FilePath.isEmpty(previewFilePath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath previewFilePath()
  {
    if (workFiles.isEmpty() == false)
      return filePath();

    String url = getURL();
    if (url.startsWith(EXT_1))
      return db.resolveExtFilePath(url);

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
