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

package org.hypernomicon.testTools;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.testTools.TestConsoleDlgCtrlr.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.stream.IntStream;

import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;

import org.hypernomicon.fts.PDFJSTextExtractor;
import org.hypernomicon.testTools.TestConsoleDlgCtrlr.TestConsoleTab;
import org.hypernomicon.util.StopWatch;
import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------

/**
 * The Test Console's PDF Extraction tab: extracts one page of a PDF through
 * pdf.js or PDFBox and shows it, and runs up to three pdf.js extractions
 * concurrently to exercise multi-instance extraction.
 */
public class PdfExtractionTestCtrlr implements TestConsoleTab
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnPdfExtract, btnPdfExtract2, btnPdfExtract3;
  @FXML private CheckBox chkPdfDebug;
  @FXML private Label lblPdfTime;
  @FXML private RadioButton rbPdfJS, rbPDFBox;
  @FXML private TextArea taPdfResult;
  @FXML private TextField tfPdfPath, tfPdfPath2, tfPdfPath3, tfPdfPage;

  private List<String> cachedPdfJSPages = null, cachedPDFBoxPages = null;

  // Per-slot caches for multi-instance pdf.js testing
  private final String[] cachedPdfJSPaths = new String[3];
  private final List<?>[] cachedPdfJSSlotPages = new List<?>[3];

  // Tracks number of concurrent extractions; overall stopwatch runs while > 0
  private int activeExtractions = 0;

  // Times the overall extraction shown in lblPdfTime. Deliberately not one of Util's
  // shared stopWatch1-6, which are reserved for temporary debugging; a lasting use of
  // one would collide with whatever a debugging session assigns it to.
  private final StopWatch extractionStopWatch = new StopWatch();

//---------------------------------------------------------------------------

  @Override public void init(TestConsoleDlgCtrlr console)
  {
    initTextField(app.prefs, tfPdfPath , PrefKey.PDF_EXTRACTION_TEST_PATH  , "", null);
    initTextField(app.prefs, tfPdfPath2, PrefKey.PDF_EXTRACTION_TEST_PATH_2, "", null);
    initTextField(app.prefs, tfPdfPath3, PrefKey.PDF_EXTRACTION_TEST_PATH_3, "", null);

    if (jxBrowserDisabled)
    {
      // pdf.js extraction runs in the browser engine; without it (missing JxBrowser license
      // key or engine startup failure), these controls could only produce null results.
      // The PDFBox side needs no engine and stays usable.

      disableAll(rbPdfJS, btnPdfExtract2, btnPdfExtract3);

      rbPDFBox.setSelected(true);
    }
  }

//---------------------------------------------------------------------------

  @FXML private void btnPdfBrowseClick()  { browseForPdf(tfPdfPath);  }
  @FXML private void btnPdfBrowse2Click() { browseForPdf(tfPdfPath2); }
  @FXML private void btnPdfBrowse3Click() { browseForPdf(tfPdfPath3); }

  @FXML private void btnPdfExtract2Click() { extractSlot(1, tfPdfPath2, btnPdfExtract2); }
  @FXML private void btnPdfExtract3Click() { extractSlot(2, tfPdfPath3, btnPdfExtract3); }

  @FXML private void btnPdfShowClick () { showSlotPage(0); }
  @FXML private void btnPdfShow2Click() { showSlotPage(1); }
  @FXML private void btnPdfShow3Click() { showSlotPage(2); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void browseForPdf(TextField tf)
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("PDF files (*.pdf)", "*.pdf"));
    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    FilePath curPath = FilePath.of(tf.getText());

    if (FilePath.isEmpty(curPath) == false)
    {
      FilePath parentDir = curPath.getParent();

      if ((parentDir != null) && parentDir.exists())
        fileChooser.setInitialDirectory(parentDir.toFile());
    }

    fileChooser.setTitle("Select PDF file");

    FilePath filePath = showOpenDialog(fileChooser);

    if (FilePath.isEmpty(filePath) == false)
      tf.setText(filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnPdfExtractClick()
  {
    String pathStr = tfPdfPath.getText();

    if (stripSafe(pathStr).isEmpty())
    {
      falseWithErrorPopup("Please select a PDF file.", tfPdfPath);
      return;
    }

    FilePath filePath = FilePath.of(pathStr);

    if (filePath.exists() == false)
    {
      falseWithErrorPopup("File not found: " + pathStr, tfPdfPath);
      return;
    }

    int page = parseInt(tfPdfPage.getText().trim(), -1);

    if (page < 1)
    {
      falseWithErrorPopup("Page number must be a number greater than zero.", tfPdfPage);
      return;
    }

    taPdfResult.clear();
    btnPdfExtract.setDisable(true);

    extractionStarted();

    if (rbPdfJS.isSelected())
      extractViaPdfJS(filePath, page);
    else
      extractViaPDFBox(filePath, page);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private void showSlotPage(int slot)
  {
    int page = parsePage();
    if (page < 1) return;

    List<String> pages;

    if (slot == 0)
    {
      // Slot 0 uses the existing cache (which may be pdf.js or PDFBox)
      pages = rbPdfJS.isSelected() ? cachedPdfJSPages : cachedPDFBoxPages;
    }
    else
    {
      TextField pathField = (slot == 1) ? tfPdfPath2 : tfPdfPath3;

      // Only reuse the slot's cached pages if its file path is unchanged since extraction;
      // otherwise treat as no cache so a stale extraction isn't shown for a different file.

      pages = Objects.equals(pathField.getText(), cachedPdfJSPaths[slot]) ? (List<String>) cachedPdfJSSlotPages[slot] : null;
    }

    if (pages == null)
    {
      taPdfResult.setText("(no cached extraction for file " + (slot + 1) + ')');
      return;
    }

    taPdfResult.clear();
    showCachedPage(pages, page, "(file " + (slot + 1) + ", cached) ");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractSlot(int slot, TextField tf, Button btn)
  {
    String pathStr = tf.getText();

    if (stripSafe(pathStr).isEmpty())
    {
      falseWithErrorPopup("Please select a PDF file.", tf);
      return;
    }

    FilePath filePath = FilePath.of(pathStr);

    if (filePath.exists() == false)
    {
      falseWithErrorPopup("File not found: " + pathStr, tf);
      return;
    }

    taPdfResult.clear();
    btn.setDisable(true);

    extractionStarted();

    int slotNum = slot + 1;

    System.out.println("Slot " + slotNum + ": starting extraction of " + filePath.getNameOnly());

    runOutsideFXThread(() ->
    {
      PDFJSTextExtractor extractor = new PDFJSTextExtractor();
      long startTime = System.nanoTime();

      try
      {
        extractor.initialize();

        System.out.println("Slot " + slotNum + ": browser initialized, extracting...");

        PDFJSTextExtractor.ExtractionResult result = extractor.extractText(filePath);

        long elapsed = System.nanoTime() - startTime;
        double seconds = elapsed / 1_000_000_000.0;

        System.out.println("Slot " + slotNum + ": extraction complete in " + String.format("%.2f", seconds) + 's'
          + (result != null ? " (" + result.pageCount() + " pages)" : " (null result)"));

        Platform.runLater(() ->
        {
          if (result != null && result.pageOffsets() != null)
          {
            cachedPdfJSPaths[slot] = pathStr;
            cachedPdfJSSlotPages[slot] = splitIntoPages(result);
          }

          btn.setDisable(false);
          extractionFinished();
        });
      }
      catch (Exception e)
      {
        System.out.println("Slot " + slotNum + ": extraction failed: " + getThrowableMessage(e));

        Platform.runLater(() ->
        {
          btn.setDisable(false);
          extractionFinished();
        });
      }
      finally
      {
        extractor.dispose();
        System.out.println("Slot " + slotNum + ": browser disposed");
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractViaPdfJS(FilePath filePath, int page)
  {
    boolean debug = chkPdfDebug.isSelected();

    runOutsideFXThread(() ->
    {
      PDFJSTextExtractor extractor = new PDFJSTextExtractor();

      try
      {
        extractor.initialize();

        PDFJSTextExtractor.ExtractionResult result = debug
          ? extractor.extractText(filePath, true, page)
          : extractor.extractText(filePath);

        Platform.runLater(() ->
        {
          if (result == null)
          {
            taPdfResult.setText("(extraction returned null)");
          }
          else if (debug)
          {
            taPdfResult.setText(result.text());
          }
          else if (result.pageOffsets() == null)
          {
            taPdfResult.setText("(no page offsets available)");
          }
          else
          {
            cachedPdfJSPages = splitIntoPages(result);
            showCachedPage(cachedPdfJSPages, page, "");
          }

          btnPdfExtract.setDisable(false);
          extractionFinished();
        });
      }
      catch (Exception e)
      {
        Platform.runLater(() ->
        {
          taPdfResult.setText("Error: " + getThrowableMessage(e));
          btnPdfExtract.setDisable(false);
          extractionFinished();
        });
      }
      finally
      {
        extractor.dispose();
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractViaPDFBox(FilePath filePath, int page)
  {
    runOutsideFXThread(() ->
    {
      try (PDDocument doc = Loader.loadPDF(filePath.toFile()))
      {
        int pageCount = doc.getNumberOfPages();
        List<String> pages = new ArrayList<>(pageCount);

        PDFTextStripper stripper = new PDFTextStripper();
        stripper.setSortByPosition(true);
        stripper.setLineSeparator("\n");

        for (int pageNum = 1; pageNum <= pageCount; pageNum++)
        {
          stripper.setStartPage(pageNum);
          stripper.setEndPage(pageNum);
          pages.add(stripper.getText(doc));
        }

        Platform.runLater(() ->
        {
          cachedPDFBoxPages = pages;

          showCachedPage(pages, page, "");
          btnPdfExtract.setDisable(false);
          extractionFinished();
        });
      }
      catch (Exception e)
      {
        Platform.runLater(() ->
        {
          taPdfResult.setText("Error: " + getThrowableMessage(e));
          btnPdfExtract.setDisable(false);
          extractionFinished();
        });
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractionStarted()
  {
    activeExtractions++;

    if (activeExtractions == 1)
    {
      extractionStopWatch.resetAndStart();
      lblPdfTime.setText("...");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractionFinished()
  {
    activeExtractions--;

    if (activeExtractions == 0)
    {
      extractionStopWatch.stop();
      lblPdfTime.setText("Overall: " + extractionStopWatch.elapsedStr());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int parsePage()
  {
    int page = parseInt(tfPdfPage.getText().trim(), -1);
    if (page < 1)
    {
      falseWithErrorPopup("Page number must be a number greater than zero.", tfPdfPage);
      return -1;
    }
    return page;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showCachedPage(List<String> pages, int page, String timePrefix)
  {
    if (page > pages.size())
    {
      taPdfResult.setText("Page " + page + " is out of range (document has " + pages.size() + " pages).");
    }
    else
    {
      lblPdfTime.setText(timePrefix + extractionStopWatch.elapsedStr());
      taPdfResult.setText(pages.get(page - 1));
    }

    btnPdfExtract.setDisable(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<String> splitIntoPages(PDFJSTextExtractor.ExtractionResult result)
  {
    int[] offsets = result.pageOffsets();
    String text = result.text();

    // Defensive against inconsistent ExtractionResults where pageOffsets claim positions
    // past text.length() (e.g., extractor returned a tiny text snippet but reported
    // full-document page boundaries). safeSubstring clamps, surfacing the inconsistency
    // in the UI rather than crashing.

    return IntStream.range(0, result.pageCount())
      .mapToObj(ndx -> safeSubstring(text, offsets[ndx], offsets[ndx + 1]))
      .toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
