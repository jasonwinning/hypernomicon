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

package org.hypernomicon.query.ui;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.*;

import org.apache.commons.io.FilenameUtils;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.interactive.annotation.PDAnnotation;

import org.hypernomicon.HyperTask;
import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.SearchResult;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.FilePathSet;

//---------------------------------------------------------------------------

class SearchResultFileList
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class SearchResultFile
  {

//---------------------------------------------------------------------------

    private final FilePath filePath;
    private final int startPage;
    private int endPage;

//---------------------------------------------------------------------------

    private SearchResultFile(FilePath filePath, int startPage, int endPage)
    {
      this.filePath = filePath;

      if (startPage < 1) startPage = 1;
      if (endPage < 1)   endPage = Integer.MAX_VALUE;

      if (startPage <= endPage)
      {
        this.startPage = startPage;
        this.endPage = endPage;
      }
      else
      {
        this.startPage = endPage;
        this.endPage = startPage;
      }
    }

//---------------------------------------------------------------------------

    private boolean hasPageRestriction()             { return (startPage > 1) || (endPage < Integer.MAX_VALUE); }
    private boolean overlaps(SearchResultFile other) { return filePath.equals(other.filePath) && (endPage >= other.startPage) && (other.endPage >= startPage); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private boolean contains(SearchResultFile other)
    {
      return filePath.equals(other.filePath) &&
        (((startPage <= other.startPage) && (endPage >= other.endPage)) ||
         ((other.startPage <= startPage) && (other.endPage >= endPage)));
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private SearchResultFile createCombined(SearchResultFile other)
    {
      int newStartPage = Math.min(startPage, other.startPage),
          newEndPage   = Math.max(endPage, other.endPage);

      return new SearchResultFile(filePath, newStartPage, newEndPage);
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private static boolean hasAnnotations(PDDocument pdf) throws IOException
    {
      for (int numPages = pdf.getNumberOfPages(), curPageNdx = 0; curPageNdx < numPages; curPageNdx++)
      {
        PDPage page = pdf.getPage(curPageNdx);

        if (page.getAnnotations().stream().anyMatch(an -> ("Link".equals(an.getSubtype()) == false) && ("Widget".equals(an.getSubtype()) == false)))
          return true;
      }

      return false;
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private static FilePath getDestPath(FilePath filePath)
    {
      FilePath destFilePath = db.resultsPath(filePath.getNameOnly().toString());
      String destStr = destFilePath.toString(),
             baseStr = FilenameUtils.removeExtension(destStr),
             ext = FilenameUtils.EXTENSION_SEPARATOR_STR + filePath.getExtensionOnly();

      int num = 1001;

      while (destFilePath.exists())
      {
        destStr = baseStr + '_' + String.valueOf(num++).substring(1) + ext;
        destFilePath = FilePath.of(destStr);
      }

      return destFilePath;
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private void copyToResultsFolder(boolean excludeAnnots, List<String> errList)
    {
      try
      {
        FilePath destFilePath = getDestPath(filePath);

        if (getMediaType(filePath).toString().contains("pdf") == false)
        {
          filePath.copyTo(destFilePath, false);
        }
        else if ((startPage == 1) && (endPage == Integer.MAX_VALUE) && (excludeAnnots == false))
        {
          filePath.copyTo(destFilePath, false);
        }
        else try (PDDocument srcPdf = Loader.loadPDF(filePath.toFile()))
        {
          int numPages = srcPdf.getNumberOfPages();

          if (numPages > 0)
          {
            if (endPage > numPages)
              endPage = numPages;

            if (excludeAnnots && (startPage == 1) && (numPages == endPage))
              excludeAnnots = hasAnnotations(srcPdf);

            if ((startPage == 1) && (numPages == endPage) && !excludeAnnots)
            {
              filePath.copyTo(destFilePath, false);
            }
            else try (PDDocument destPdf = new PDDocument())
            {
              for (int curPageNdx = startPage - 1; curPageNdx < endPage; curPageNdx++)
              {
                // Make a copy of the page and add it to the new document

                PDPage newPage = destPdf.importPage(srcPdf.getPage(curPageNdx));

                // Remove annotations

                if (excludeAnnots)
                {
                  List<PDAnnotation> annotations = newPage.getAnnotations();

                  // The reason for iterating over index numbers and calling the list remove(ndx) method
                  // is that other methods for removing from a collection don't seem to be implemented
                  // correctly.

                  for (int ndx = annotations.size() - 1; ndx >= 0; ndx--)
                  {
                    String subtype = annotations.get(ndx).getSubtype();

                    if (("Link".equals(subtype) == false) && ("Widget".equals(subtype) == false))
                      annotations.remove(ndx);
                  }
                }
              }

              destPdf.save(destFilePath.toString());
            }
          }
        }
      }
      catch (Throwable e)
      {
        errList.add("Error: Unable to copy \"" + filePath + "\". Reason: " + getThrowableMessage(e));
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<SearchResultFile> list = new ArrayList<>();
  private final List<String> errList = new ArrayList<>();
  private final boolean copyingEntirePDFs, includeEdited;
  private final FilePathSet filePathSet = new FilePathSet();

//---------------------------------------------------------------------------

  SearchResultFileList(boolean copyingEntirePDFs, boolean includeEdited)
  {
    this.copyingEntirePDFs = copyingEntirePDFs;
    this.includeEdited = includeEdited;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addRecord(HDT_RecordWithPath record)
  {
    switch (record.getType())
    {
      case hdtMiscFile : case hdtWorkFile :

        addFile(record.filePath(), -1, -1);
        break;

      case hdtWork :

        HDT_Work work = (HDT_Work)record;

        if ((includeEdited == false) && (work.getAuthors().isEmpty() == false) && work.getAuthors().stream().allMatch(author -> author.getIsEditor() || author.getIsTrans()))
          return;

        if (work.workFiles.isEmpty())
        {
          FilePath filePath = work.filePathIncludeExt();
          if (FilePath.isEmpty(filePath) == false)
            addFile(filePath, work.getStartPageNum(), work.getEndPageNum());
        }
        else for (HDT_WorkFile workFile : work.workFiles)
        {
          int startPage = work.getStartPageNum(workFile),
              endPage   = work.getEndPageNum  (workFile);

          if (copyingEntirePDFs)
          {
            if (filePathSet.contains(workFile.filePath()))
              continue;

            filePathSet.add(workFile.filePath());
            startPage = -1;
            endPage = -1;
          }

          if (((startPage < 1) && (endPage > 0)) ||
              ((endPage < 1) && (startPage > 0)))
            errList.add("Warning: Work \"" + work.name() + "\", ID " + work.getID() + " is missing a start or end page number.");

          addFile(workFile.filePath(), startPage, endPage);
        }

        work.subWorks .forEach(this::addRecord);
        work.miscFiles.forEach(this::addRecord);

        break;

      default :
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addFile(FilePath filePath, int startPage, int endPage)
  {
    if (filePath.exists() == false) return;

    SearchResultFile otherFile = new SearchResultFile(filePath, startPage, endPage);

    for (int ndx = 0; ndx < list.size(); ndx++)
    {
      SearchResultFile resultFile = list.get(ndx);

      if (resultFile.overlaps(otherFile))
      {
        if (resultFile.contains(otherFile) == false)
          list.set(ndx, resultFile.createCombined(otherFile));

        return;
      }
    }

    list.add(otherFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void showErrors()
  {
    String errors = strListToStr(errList, false);

    if (strNotNullOrBlank(convertToSingleLine(errors)))
      errorPopup(errors);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperTask newCopyAllTask(boolean excludeAnnots) { return new HyperTask("CopyingFiles", "Copying files...") { @Override protected void call() throws CancelledTaskException
  {
    totalCount = list.size();

    for (SearchResultFile resultFile : list)
    {
      resultFile.copyToResultsFolder(excludeAnnots, errList);

      incrementAndUpdateProgress();
    }
  }}; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the set of relative paths (forward-slash separated, relative to DB root)
   * suitable for use as a Lucene TermInSetQuery filter.
   */
  Set<String> getPathScope()
  {
    Set<String> paths = new HashSet<>();

    for (SearchResultFile resultFile : list)
    {
      String ext = resultFile.filePath.getExtensionOnly();
      if (FullTextIndexer.isIndexableExtension(ext) == false) continue;

      FilePath relPath = db.getRootPath().relativize(resultFile.filePath);
      if (relPath != null)
        paths.add(relPath.toString().replace('\\', '/'));
    }

    return paths;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a summary string for the scope label (e.g., "47 files (3 with page restrictions)").
   */
  String getSummary()
  {
    Set<FilePath> uniqueFiles = new HashSet<>();
    int restrictedCount = 0;

    for (SearchResultFile rf : list)
    {
      uniqueFiles.add(rf.filePath);

      if (rf.hasPageRestriction())
        restrictedCount++;
    }

    String summary = uniqueFiles.size() + " file" + (uniqueFiles.size() == 1 ? "" : "s");

    if (restrictedCount > 0)
      summary += " (" + restrictedCount + " with page restrictions)";

    return summary;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Filters FTS search results by page range. Returns a new list containing only results
   * whose page matches fall within the scoped page ranges for the corresponding file.
   */
  List<SearchResult> filterResults(List<SearchResult> results)
  {
    List<SearchResult> filtered = new ArrayList<>();

    for (SearchResult result : results)
    {
      List<PageMatch> filteredMatches = filterPageMatches(result.path(), result.pageMatches());

      if (collEmpty(filteredMatches)) continue;

      filtered.add(new SearchResult(result.path(), result.score(), filteredMatches, result.scoreDoc()));
    }

    return filtered;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<PageMatch> filterPageMatches(String relPath, List<PageMatch> matches)
  {
    if (collEmpty(matches)) return matches;

    FilePath absPath = db.getRootPath(relPath);

    // Find all page range entries for this file

    List<SearchResultFile> entries = list.stream().filter(rf -> rf.filePath.equals(absPath)).toList();
    if (entries.isEmpty()) return null;  // file not in scope

    // If any entry covers the entire file, no filtering needed

    for (SearchResultFile entry : entries)
      if (entry.hasPageRestriction() == false)
        return matches;

    // Filter page matches to those within at least one allowed range

    List<PageMatch> filtered = new ArrayList<>();

    for (PageMatch pm : matches)
    {
      if (pm.pageNumber() == 0)  // non-PDF or unknown page; include unconditionally
      {
        filtered.add(pm);
        continue;
      }

      for (SearchResultFile entry : entries)
      {
        if ((pm.pageNumber() >= entry.startPage) && (pm.pageNumber() <= entry.endPage))
        {
          filtered.add(pm);
          break;
        }
      }
    }

    return filtered;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a sorted list of scope descriptions for display in a popup
   * (e.g., "Papers/anthology.pdf  [pp. 50–75]").
   */
  List<String> getScopeDescription()
  {
    List<String> desc = new ArrayList<>();

    for (SearchResultFile rf : list)
    {
      FilePath relPath = db.getRootPath().relativize(rf.filePath);
      String pathStr = relPath != null ? relPath.toString().replace('\\', '/') : rf.filePath.toString();

      if (rf.hasPageRestriction())
        desc.add(pathStr + "  [" + formatPageRange(rf.startPage, rf.endPage == Integer.MAX_VALUE ? 0 : rf.endPage) + ']');
      else
        desc.add(pathStr);
    }

    desc.sort(String.CASE_INSENSITIVE_ORDER);
    return desc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
