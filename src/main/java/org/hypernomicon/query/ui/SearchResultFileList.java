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

package org.hypernomicon.query.ui;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.cos.COSArray;
import org.apache.pdfbox.cos.COSDictionary;
import org.apache.pdfbox.cos.COSName;
import org.apache.pdfbox.multipdf.PDFCloneUtility;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.HDT_RecordWithPath;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;

import com.google.common.collect.Iterators;

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

    // This class is necessary because the PDFBox developers decided to reduce
    // the visibility of the PDFCloneUtility constructor without providing an
    // alternate way to clone a PDF page in memory.

    private static final class PDFCloneUtility2 extends PDFCloneUtility
    {
      private PDFCloneUtility2(PDDocument dest) { super(dest); }  // Make callable from outer class
    }

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
          }

          if (numPages > 0)
          {
            if ((startPage == 1) && (numPages == endPage) && !excludeAnnots)
            {
              filePath.copyTo(destFilePath, false);
            }
            else try (PDDocument destPdf = new PDDocument())
            {
              PDFCloneUtility cloneUtil = new PDFCloneUtility2(destPdf);

              for (int curPageNdx = startPage - 1; curPageNdx < endPage; curPageNdx++)
              {
                COSDictionary dict = cloneUtil.cloneForNewDocument(srcPdf.getPage(curPageNdx).getCOSObject());

                if (excludeAnnots)
                {
                  COSArray annots = (COSArray) dict.getItem(COSName.ANNOTS);

                  if (annots != null)
                  {
                    Iterators.removeIf(annots.iterator(), annot ->
                    {
                      String subtype = ((COSName) ((COSDictionary) annot).getItem(COSName.SUBTYPE)).getName();

                      return ("Link".equals(subtype) == false) && ("Widget".equals(subtype) == false);
                    });
                  }
                }

                destPdf.addPage(new PDPage(dict));
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
        boolean isAuthored = work.getAuthors().stream().allMatch(author -> author.getIsEditor() || author.getIsTrans()) == false;

        if ((isAuthored == false) && (includeEdited == false)) return;

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

    if (ultraTrim(convertToSingleLine(errors)).length() > 0)
      errorPopup(errors);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void copyAll(boolean excludeAnnots, HyperTask task) throws CancelledTaskException
  {
    int ndx = 0; for (SearchResultFile resultFile : list)
    {
      resultFile.copyToResultsFolder(excludeAnnots, errList);
      task.updateProgress(ndx++, list.size());

      if (task.isCancelled())
        throw new CancelledTaskException();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
