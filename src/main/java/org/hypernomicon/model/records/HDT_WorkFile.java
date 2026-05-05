/*
 * Copyright 2015-2026 Jason Winning
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
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import org.hypernomicon.Const.FileNamePrefKey;
import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.settings.WorkFileNamingSettingsCtrlr.WorkFileNameComponent;
import org.hypernomicon.settings.WorkFileNamingSettingsCtrlr.WorkFileNameComponentType;
import org.hypernomicon.util.file.FilePath;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.stream.IntStream;

//---------------------------------------------------------------------------

public class HDT_WorkFile extends HDT_RecordBase implements HDT_RecordWithPath
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperPath path;
  public final List<HDT_Work> works;

//---------------------------------------------------------------------------

  public HDT_WorkFile(RecordState xmlState, DatasetAccessor<HDT_WorkFile> dataset)
  {
    super(xmlState, dataset);

    works = getSubjList(rtWorkFileOfWork);
    path = new HyperPath(getObjPointer(rtFolderOfWorkFile), this);
  }

//---------------------------------------------------------------------------

  /* ************************************************************* */
  /*                                                               */
  /*   The name item for HDT_WorkFile consists only of the         */
  /*   user-entered description from the work files table in the   */
  /*   Works tab. Addtional text functions add the actual          */
  /*   file name.                                                  */
  /*                                                               */
  /* ************************************************************* */

 @Override public HyperPath getPath()                 { return path; }
 @Override public String getXMLObjectName()           { return defaultCellText(); }

 /**
  * {@inheritDoc}
  */
 @Override public String defaultChoiceText()          { return defaultCellText(); }

 /**
  * {@inheritDoc}
  */
 @Override public String defaultCellText()            { return strNotNullOrEmpty(name()) ? (path.getNameStr() + " (" + name() + ')') : path.getNameStr(); }
 @Override protected String makeSortKeyTypeSpecific() { return strNotNullOrEmpty(name()) ? (path.getNameStr() + '\u0000' + name()) : path.getNameStr(); }

  public boolean getAnnotated()         { return getTagBoolean(tagAnnotated); }
  public void setAnnotated(boolean val) { updateTagBoolean(tagAnnotated, val); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class FileNameAuthor
  {
    private final String name;
    private final boolean isEditor, isTrans;

    public FileNameAuthor(String name, boolean isEditor, boolean isTrans)
    {
      this.name = name;
      this.isEditor = isEditor;
      this.isTrans = isTrans;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String makeFileName(List<FileNameAuthor> authors, HDT_WorkType workType, String year, String title, String container, String publisher, CharSequence ext)
  {
    return makeFileName(authors, workType, year, title, container, publisher, ext, null);
  }

  public static String makeFileName(List<FileNameAuthor> authors, HDT_WorkType workType, String year, String title, String container, String publisher, CharSequence ext, List<WorkFileNameComponent> components)
  {
    if (components == null)
      components = WorkFileNameComponent.loadFromPrefs();

    WorkFileNameComponent authComponent = findFirst(components, component -> component.type == WorkFileNameComponentType.fncAuthorLastNames);
    String fileName = "";

    EnumSet<WorkFileNameComponentType> usedComponentTypes = EnumSet.noneOf(WorkFileNameComponentType.class);

    for (WorkFileNameComponent component : components)
    {
      if ((component.type == WorkFileNameComponentType.fncBlank) || (component.type == null) || usedComponentTypes.contains(component.type) || ((workType != null) && component.excludedWorkTypes.contains(workType)))
        continue;

      fileName = fileName + getFNComponent(component, authComponent, authors, year, title, container, publisher);
      usedComponentTypes.add(component.type);
    }

    fileName = fileName.strip();

    if (db.prefs.getBoolean(FileNamePrefKey.POSIX, false))
    {
      fileName = convertToEnglishChars(fileName);

      String newName = "";

      for (int ndx = 0; ndx < fileName.length(); ndx++)
      {
        char ch = fileName.charAt(ndx);

        if (((ch >= 'A') && (ch <= 'Z')) ||
            ((ch >= 'a') && (ch <= 'z')) ||
            ((ch >= '0') && (ch <= '9')) ||
            (ch == '-') ||
            (ch == '_'))
          newName = newName + ch;
      }

      // Strip leading dashes after filtering: removing other characters can expose a dash that
      // was not at the start before, and a leading '-' is treated as an option flag by many
      // command-line tools.

      while (newName.startsWith("-"))
        newName = newName.substring(1);

      fileName = newName;
    }

    if (db.prefs.getBoolean(FileNamePrefKey.LOWERCASE, false))
      fileName = fileName.toLowerCase();

    fileName = FilePath.removeInvalidFileNameChars(fileName);

    int maxLen = db.prefs.getInt(FileNamePrefKey.MAX_CHAR, 255);

    if (ext.length() > 0)
    {
      int extLen = ext.length() + FilenameUtils.EXTENSION_SEPARATOR_STR.length();

      if ((fileName.length() + extLen) > maxLen)
        fileName = safeSubstring(fileName, 0, maxLen - extLen);

      return fileName.strip() + FilenameUtils.EXTENSION_SEPARATOR_STR + ext;
    }

    if (fileName.length() > maxLen)
      fileName = safeSubstring(fileName, 0, maxLen);

    return fileName.strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getAuthorStr(Iterable<FileNameAuthor> authors, boolean isEditor, boolean isTrans)
  {
    String comp = "";

    for (FileNameAuthor author : authors)
    {
      if (((author.isEditor == false) && (author.isTrans == false) && (isEditor == false) && (isTrans == false)) ||
          (author.isEditor && isEditor) ||
          (author.isTrans && isTrans))
      {
        String authorStr = author.name;
        int pos = authorStr.indexOf(',');

        if (pos >= 0)
          authorStr = authorStr.substring(0, pos);

        comp = comp.isEmpty() ? authorStr : (comp + ' ' + authorStr);
      }
    }

    return comp;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getFNComponent(WorkFileNameComponent component,  WorkFileNameComponent authComponent, List<FileNameAuthor> authors, String year, String title, String container, String publisher)
  {
    String compStr = "";

    switch (component.type)
    {
      case fncAuthorLastNames :

        compStr = getAuthorStr(authors, false, false);
        break;

      case fncEditors :

        if (db.prefs.getBoolean(FileNamePrefKey.TREAT_ED_AS_AUTHOR, true) && (authComponent != null) &&
            authors.stream().allMatch(author -> author.isEditor || author.isTrans))
          component = authComponent;

        compStr = getAuthorStr(authors, true, false);
        break;

      case fncTranslators :

        compStr = getAuthorStr(authors, false, true);
        break;

      case fncTitleNoSub :

        compStr = stripSubtitle(title);
        break;

      case fncContainerNoSub :

        compStr = stripSubtitle(container);
        break;

      case fncYear :

        compStr = year;
        break;

      case fncPublisher:

        compStr = publisher;
        break;

      default :

        break;
    }

    compStr = compStr.strip().replace(" ", component.withinSep);

    if (compStr.length() > 0)
      compStr = component.beforeSep + compStr + component.afterSep;

    return compStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Derives the file-name form of a work title or container title by dropping any subtitle. A slash
   * is meaningful punctuation and is converted to a hyphen; the first subtitle or separator
   * delimiter (a colon, question mark, or vertical bar) marks the start of the subtitle, and
   * everything from there on is dropped. Other characters that are illegal in file names are left in
   * place to be removed afterward by {@link FilePath#removeInvalidFileNameChars(CharSequence)}. The
   * result is stripped of surrounding whitespace.
   */
  public static String stripSubtitle(String title)
  {
    String str = safeStr(title).replace('/', '-');
    int ndx = StringUtils.indexOfAny(str, ":?|");

    return ((ndx >= 0) ? str.substring(0, ndx) : str).strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A work's page range within a particular work file. {@code startPage} is
   * always {@code > 0}; {@code endPage <= 0} indicates the range extends to
   * the end of the file.
   *
   * @see #getBoundariesForFile(FilePath)
   */
  public record WorkBoundary(int startPage, int endPage, HDT_Work work) {}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Collects the page-range boundaries for every work associated with the given
   * file via its work file record(s). Works without an explicit start page are
   * omitted. Results are sorted by start page ascending.
   *
   * @param filePath a file path (typically under the database root)
   * @return an ordered list of work boundaries; empty if no {@code HDT_WorkFile}
   *         is associated with the file, or if no associated work has a start page
   */
  public static List<WorkBoundary> getBoundariesForFile(FilePath filePath)
  {
    Set<HyperPath> hyperPaths = HyperPath.getHyperPathSetForFilePath(filePath);
    if (collEmpty(hyperPaths)) return List.of();

    List<WorkBoundary> boundaries = new ArrayList<>();

    for (HyperPath hp : hyperPaths)
    {
      if (hp.getRecord() instanceof HDT_WorkFile workFile)
      {
        for (HDT_Work work : workFile.works)
        {
          int startPage = work.getStartPageNum(workFile),
              endPage   = work.getEndPageNum  (workFile);

          if (startPage > 0)
            boundaries.add(new WorkBoundary(startPage, endPage, work));
        }
      }
    }

    boundaries.sort(Comparator.comparingInt(WorkBoundary::startPage));
    return boundaries;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Among all works of all {@link HDT_WorkFile} records associated with {@code hyperPaths},
   * returns the one whose explicit page range most tightly covers {@code [minPage, maxPage]}
   * (smallest span wins). A work with no explicit page range, and any work when
   * {@code minPage <= 0} (no page constraint), is used only as a last resort. Page values
   * {@code <= 0} are treated as "not set" (matching {@code HDT_Work}'s {@code -1} sentinel
   * from {@code parseInt}, and guarding against a stray {@code 0}).
   *
   * @return the most specific covering work, or {@code null} if no associated work qualifies
   */
  public static HDT_Work smallestCoveringWork(Set<HyperPath> hyperPaths, int minPage, int maxPage)
  {
    HDT_Work bestWork = null;
    int bestSpan = Integer.MAX_VALUE;

    for (HyperPath hyperPath : hyperPaths)
    {
      if (hyperPath.getRecord() instanceof HDT_WorkFile workFile)
      {
        for (HDT_Work work : workFile.works)
        {
          int startPage = work.getStartPageNum(workFile),
              endPage   = work.getEndPageNum  (workFile);

          // No explicit range, or no page constraint: last-resort candidate only

          if (((startPage <= 0) && (endPage <= 0)) || (minPage <= 0))
          {
            if (bestWork == null)
              bestWork = work;

            continue;
          }

          // Does this work's range contain every page in [minPage, maxPage]?

          boolean containsAll = ((startPage <= 0) || (minPage >= startPage)) &&
                                ((endPage   <= 0) || (maxPage <= endPage));

          if (containsAll == false) continue;

          int span = ((startPage > 0) && (endPage > 0)) ? (endPage - startPage) : (Integer.MAX_VALUE - 1);

          if (span < bestSpan)
          {
            bestSpan = span;
            bestWork = work;
          }
        }
      }
    }

    return bestWork;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Finds the most specific work (smallest explicit page range) whose range
   * contains every page in {@code pages}. Works with no explicit page range
   * are treated as covering the entire file and used only as a last resort.
   * Non-positive page numbers in the stream are ignored.
   *
   * @param filePath     a file path (typically under the database root)
   * @param pageNumbers  the page numbers that must all be covered
   * @param fallback     the record to return if no single covering work is found
   *                     (including when {@code pages} reduces to empty, or when
   *                     no {@code HDT_WorkFile} is associated with the file)
   * @return the most specific covering work, or {@code fallback}
   */
  public static HDT_RecordWithPath resolveRecordForPages(FilePath filePath, IntStream pageNumbers, HDT_RecordWithPath fallback)
  {
    IntSummaryStatistics pageNumberStats = pageNumbers.filter(pageNumber -> pageNumber > 0).summaryStatistics();

    if (pageNumberStats.getCount() == 0) return fallback;

    Set<HyperPath> hyperPaths = HyperPath.getHyperPathSetForFilePath(filePath);

    return collEmpty(hyperPaths) ?
      fallback
    :
      nullSwitch(smallestCoveringWork(hyperPaths, pageNumberStats.getMin(), pageNumberStats.getMax()), fallback);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
