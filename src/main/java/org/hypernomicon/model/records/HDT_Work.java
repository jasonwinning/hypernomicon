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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.hypernomicon.bib.BibData;
import org.hypernomicon.bib.BibUtils;
import org.hypernomicon.bib.WorkBibData;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjList;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.util.filePath.FilePath;

public class HDT_Work extends HDT_RecordWithConnector implements HDT_RecordWithPath
{
  private final Authors authors;

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

  public HDT_Work(HDT_RecordState xmlState, HyperDataset<HDT_Work> dataset)
  {
    super(xmlState, dataset, tagTitle);

    if (dataset != null)
    {
      authors = new Authors(getObjList(rtAuthorOfWork), this);

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
    else
    {
      authors  = null; authorRecords = null; investigations = null; labels   = null; workFiles  = null;
      subWorks = null; miscFiles     = null; arguments      = null; workType = null; largerWork = null;
    }
  }

  public void setInvestigations(List<HDT_Investigation> list) { updateObjectsFromList(rtInvestigationOfWork, list); }
  public void setWorkLabels(List<HDT_WorkLabel> list)         { updateObjectsFromList(rtLabelOfWork, list); }

  public WorkTypeEnum getWorkTypeValue()    { return HDT_WorkType.workTypeIDToEnumVal(workType.getID()); }
  public void setWorkType(WorkTypeEnum val) { workType.set(HDT_WorkType.get(val)); }

  @Override public String listName()        { return name(); }
  @Override public HDT_RecordType getType() { return hdtWork; }
  @Override public HyperPath getPath()      { return workFiles.isEmpty() ? HyperPath.EmptyPath : workFiles.get(0).getPath(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getYear()        { return getTagString(tagYear); }
  public String getBibEntryKey() { return getBibEntryKeyString(); }
  public String getMiscBib()     { return getTagString(tagMiscBib); }
  public String getDOI()         { return getTagString(tagDOI); }
  public List<String> getISBNs() { return BibUtils.matchISBN(getTagString(tagISBN)); }
  public String getWebLink()     { return getTagString(tagWebLink); }
  public Authors getAuthors()    { return authors; }
  public int getStartPageNum()   { return workFiles.isEmpty() ? -1 : getStartPageNum(workFiles.get(0)); }
  public int getEndPageNum()     { return workFiles.isEmpty() ? -1 : getEndPageNum(workFiles.get(0)); }
  public boolean canLaunch()     { return ! (getPath().isEmpty() && getWebLink().isEmpty()); }

  public void setYear(String str)        { updateTagString(tagYear, str); }
  public void setBibEntryKey(String str) { updateBibEntryKey(str); }
  public void setMiscBib(String str)     { updateTagString(tagMiscBib, str); }
  public void setDOI(String str)         { updateTagString(tagDOI, BibUtils.matchDOI(str)); }
  public void setWebLink(String str)     { updateTagString(tagWebLink, str); }

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

  public void setStartPageNum(HDT_WorkFile workFile, int val) { db.updateNestedString(this, workFile, tagStartPageNum, val < 0 ? "" : String.valueOf(val)); }
  public int getStartPageNum(HDT_WorkFile workFile)           { return parseInt(db.getNestedString(this, workFile, tagStartPageNum), -1); }
  public void setEndPageNum(HDT_WorkFile workFile, int val)   { db.updateNestedString(this, workFile, tagEndPageNum, val < 0 ? "" : String.valueOf(val)); }
  public int getEndPageNum(HDT_WorkFile workFile)             { return parseInt(db.getNestedString(this, workFile, tagEndPageNum), -1); }

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

  public String getShortAuthorsStr(boolean fullNameIfSingleton)
  {
    return Authors.getShortAuthorsStr(getAuthors().asCollection(), false, fullNameIfSingleton);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getLongAuthorsStr(boolean fullNameIfSingleton)
  {
    return Authors.getLongAuthorsStr(getAuthors().asCollection(), fullNameIfSingleton);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getCBText()
  {
    String authorStr = getShortAuthorsStr(false);
    String yearStr = getYear();
    String titleStr = name();

    String cbStr = "";

    if (authorStr.length() > 0)
      cbStr = authorStr + " ";

    if (yearStr.length() > 0)
      cbStr = cbStr + "(" + yearStr + ") ";

    if (titleStr.length() > 0)
      cbStr = cbStr + titleStr;

    return cbStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setLargerWork(int newID, boolean noIsbnUpdate)
  {
    boolean ask = false;

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

      if ((notAllInLW == false) && (notAllInSW == true))
      {
        updateISBNstrRecursively(largerWork.getTagString(tagISBN));
      }
      else if (notAllInLW)
      {
        if (confirmDialog("Recursively update ISBN(s) for contained/container works?"))
        {
          String isbnStr = lwISBNs.stream().filter(isbn -> ISBNs.contains(isbn) == false).reduce((s1, s2) -> s1 + "; " + s2).orElse("");

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
      largerWork.workFiles.forEach(workFile -> addWorkFile(workFile.getID(), true, true));
      return;
    }

    if (workFiles.size() != largerWork.workFiles.size())
      ask = true;
    else
      ask = workFiles.stream().allMatch(workFile -> largerWork.workFiles.contains(workFile)) == false;

    if (ask)
    {
      String msg = workFiles.size() == 1 ? " file is " : " files are ";
      if (confirmDialog("Currently, " + workFiles.size() + msg + "attached to the child work. Replace with parent work file(s)?"))
      {
        getObjList(rtWorkFileOfWork).clear();
        largerWork.workFiles.forEach(workFile -> addWorkFile(workFile.getID(), true, true));
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

  public void addWorkFile(int newID, boolean alsoAddToEmptySubworks, boolean confirmToRemoveFromUnenteredSet)
  {
    HDT_WorkFile workFile = db.workFiles.getByID(newID);

    getObjList(rtWorkFileOfWork).add(workFile);

    workFile.works.forEach(work ->
    {
      if ((work.getID() != getID()) && (work.getWorkTypeValue() == WorkTypeEnum.wtUnenteredSet))
      {
        boolean okToRemoveFromUnenteredSet;

        if (confirmToRemoveFromUnenteredSet)
          okToRemoveFromUnenteredSet = confirmDialog("Okay to remove the file from the the unentered set of work files: \"" + work.name() + "\"?");
        else
          okToRemoveFromUnenteredSet = true;

        if (okToRemoveFromUnenteredSet)
          db.getObjectList(rtWorkFileOfWork, work, true).remove(workFile);
      }
    });

    if (alsoAddToEmptySubworks == false) return;

    subWorks.forEach(childWork -> {
      if (childWork.workFiles.isEmpty()) childWork.addWorkFile(newID, true, confirmToRemoveFromUnenteredSet); });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FilePath getBasePathForWorkTypeID(int workTypeID)
  {
    switch (HDT_WorkType.workTypeIDToEnumVal(workTypeID))
    {
      case wtBook:    return db.getPath(PREF_KEY_BOOKS_PATH);
      case wtChapter: return db.getPath(PREF_KEY_BOOKS_PATH);
      case wtPaper:   return db.getPath(PREF_KEY_PAPERS_PATH);
      default:        return db.getPath(PREF_KEY_MISC_FILES_PATH);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void launch(int pageNum)
  {
    if (workFiles.isEmpty() && getWebLink().isEmpty()) return;

    viewNow();

    if (getPath().isEmpty())
    {
      openWebLink(getWebLink());
      return;
    }

    if (pageNum < 1) pageNum = getStartPageNum();
    launchWorkFile(getPath().getFilePath(), pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getInvText(HDT_Person person)
  {
    return person.investigations.stream().map(HDT_Investigation::listName)
                                         .reduce((s1, s2) -> s1 + ", " + s2).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String addFileIndicator(String str, HDT_Work work)
  {
    if (work == null) return str;

    String indicator = "";

    if (work.workFiles.isEmpty() == false)
      indicator = work.workFiles.get(0).getPath().getFilePath().getExtensionOnly();
    else if (safeStr(work.getWebLink()).length() > 0)
      indicator = "web";

    return indicator.length() == 0 ? str : new String(str + " (" + indicator + ")").trim();
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

    list.forEach(isbn -> BibUtils.matchISBN(isbn, allIsbns));

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

}