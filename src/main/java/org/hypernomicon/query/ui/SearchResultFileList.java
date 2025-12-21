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
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.interactive.annotation.PDAnnotation;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;

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
//---------------------------------------------------------------------------

    private boolean overlaps(SearchResultFile other)
    {
      return filePath.equals(other.filePath) && (endPage >= other.startPage) && (other.endPage >= startPage);
    }

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
        destFilePath = new FilePath(destStr);
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
        else if ((startPage == 1) && (endPage == Integer.MAX_VALUE) && !excludeAnnots)
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

        if ((includeEdited == false) && work.getAuthors().stream().allMatch(author -> author.getIsEditor() || author.getIsTrans()))
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

}
