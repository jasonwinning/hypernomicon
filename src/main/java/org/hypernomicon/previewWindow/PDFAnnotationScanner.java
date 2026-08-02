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

package org.hypernomicon.previewWindow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.pdmodel.interactive.annotation.PDAnnotation;

import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Finds the pages of a PDF that bear user-visible annotations (anything other
 * than links and form widgets), by reading the document's object structure
 * directly with PDFBox. Collecting this through the viewer would force every
 * page dictionary through the pdf.js worker's single thread, where the walk
 * competes with page rendering. Reading the page tree directly touches only the
 * document's cross-reference table, page dictionaries, and annotation arrays,
 * which takes seconds even on multi-hundred-MB files and contends with
 * nothing.
 */
final class PDFAnnotationScanner
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private PDFAnnotationScanner() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the 1-based page numbers, in ascending order, of pages having at
   * least one annotation other than a link or form widget. Returns an empty
   * list if the document cannot be parsed; the preview then simply shows no
   * annotation markers.
   */
  static List<Integer> scan(FilePath filePath)
  {
    List<Integer> annotPages = new ArrayList<>();

    long startTime = System.currentTimeMillis();

    int pageNum = 0;

    try (PDDocument document = Loader.loadPDF(filePath.toFile()))
    {
      for (PDPage page : document.getPages())
      {
        pageNum++;

        try
        {
          // One qualifying annotation marks the page

          if (page.getAnnotations().stream().map(PDAnnotation::getSubtype)
                                            .anyMatch(subtype -> ("Link".equals(subtype) == false) && ("Widget".equals(subtype) == false)))
            annotPages.add(pageNum);
        }
        catch (IOException e)
        {
          // A page whose annotations cannot be parsed just goes unmarked
        }
      }
    }
    catch (IOException e)
    {
      System.out.println("PDFAnnotationScanner: unable to scan " + filePath + ": " + getThrowableMessage(e));
    }

    if (app.debugging)
      System.out.println("PDFAnnotationScanner: " + annotPages.size() + " annotated of " + pageNum + " pages in " + (System.currentTimeMillis() - startTime) + " ms: " + filePath.getNameOnly());

    return annotPages;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
