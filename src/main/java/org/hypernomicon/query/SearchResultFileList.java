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

package org.hypernomicon.query;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.util.MediaUtil.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.pdfbox.cos.COSArray;
import org.apache.pdfbox.cos.COSDictionary;
import org.apache.pdfbox.cos.COSName;
import org.apache.pdfbox.multipdf.PDFCloneUtility;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;

import com.google.common.collect.Iterators;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class SearchResultFileList
{
  private static class SearchResultFile
  {
    private final FilePath filePath;
    private int startPage, endPage;

  //---------------------------------------------------------------------------
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
      return filePath.equals(other.filePath) == false ?
        false
      :
        ((endPage >= other.startPage) && (other.endPage >= startPage));
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private boolean contains(SearchResultFile other)
    {
      return filePath.equals(other.filePath) == false ?
        false
      :
        (((startPage <= other.startPage) && (endPage >= other.endPage))  ||
         ((other.startPage <= startPage) && (other.endPage >= endPage)));
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private SearchResultFile createCombined(SearchResultFile other)
    {
      int newStartPage = startPage < other.startPage ? startPage : other.startPage,
          newEndPage   = endPage   > other.endPage   ? endPage   : other.endPage;

      return new SearchResultFile(filePath, newStartPage, newEndPage);
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private static boolean hasAnnotations(PDDocument pdf) throws IOException
    {
      int numPages = pdf.getNumberOfPages();
      for (int curPageNdx = 0; curPageNdx < numPages; curPageNdx++)
      {
        PDPage page = pdf.getPage(curPageNdx);

        if (page.getAnnotations().stream().anyMatch(an -> (an.getSubtype().equals("Link") == false) && (an.getSubtype().equals("Widget") == false)))
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
        destStr = baseStr + "_" + String.valueOf(num++).substring(1) + ext;
        destFilePath = new FilePath(destStr);
      }

      return destFilePath;
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void copyToResultsFolder(boolean excludeAnnots, List<String> errList)
    {
      PDFCloneUtility cloneUtil = null;

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
        else try (PDDocument srcPdf = PDDocument.load(filePath.toFile()))
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
              cloneUtil = new PDFCloneUtility(destPdf);

              for (int curPageNdx = startPage - 1; curPageNdx < endPage; curPageNdx++)
              {
                COSDictionary dict = (COSDictionary) cloneUtil.cloneForNewDocument(srcPdf.getPage(curPageNdx));
                if (excludeAnnots)
                {
                  COSArray annots = (COSArray) dict.getItem(COSName.ANNOTS);

                  if (annots != null)
                  {
                    Iterators.removeIf(annots.iterator(), annot ->
                    {
                      String subtype = COSName.class.cast(COSDictionary.class.cast(annot).getItem(COSName.SUBTYPE)).getName();

                      return (subtype.equals("Link") == false) && (subtype.equals("Widget") == false);
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
        String msg = e.getMessage();
        if (String.valueOf(msg).equals("null"))
          msg = e.getClass().getName();

        errList.add("Error: Unable to copy \"" + filePath + "\". Reason: " + msg);
      }
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final List<SearchResultFile> list = new ArrayList<>();
  private final List<String> errList = new ArrayList<>();
  private final boolean copyingEntirePDFs;
  private final FilePathSet filePathSet = new FilePathSet();

  SearchResultFileList(boolean copyingEntirePDFs)
  {
    this.copyingEntirePDFs = copyingEntirePDFs;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void addRecord(HDT_RecordWithPath record, boolean includeEdited)
  {
    switch (record.getType())
    {
      case hdtMiscFile : case hdtWorkFile :

        addFile(record.filePath(), -1, -1);
        break;

      case hdtWork :

        HDT_Work work = (HDT_Work)record;
        boolean isAuthored = work.getAuthors().stream().allMatch(author -> author.getIsEditor() || author.getIsTrans()) == false;

        if (!isAuthored && !includeEdited) return;

        for (HDT_WorkFile workFile : work.workFiles)
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

        work.subWorks .forEach(subWork  -> addRecord(subWork , includeEdited));
        work.miscFiles.forEach(miscFile -> addRecord(miscFile, includeEdited));

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
      messageDialog(errors, mtError);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void copyAll(boolean excludeAnnots, HyperTask task) throws TerminateTaskException
  {
    int ndx = 0; for (SearchResultFile resultFile : list)
    {
      resultFile.copyToResultsFolder(excludeAnnots, errList);
      task.updateProgress(ndx++, list.size());

      if (task.isCancelled())
        throw new TerminateTaskException();
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
