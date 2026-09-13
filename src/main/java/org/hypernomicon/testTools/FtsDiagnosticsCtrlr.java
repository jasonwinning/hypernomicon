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
import static org.hypernomicon.fts.FTSUtil.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.apache.lucene.search.Query;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.previewWindow.BrowserEngine;
import org.hypernomicon.testTools.TestConsoleDlgCtrlr.TestConsoleTab;
import org.hypernomicon.util.file.FilePath;

import org.jodconverter.core.office.OfficeUtils;
import org.jodconverter.local.LocalConverter;
import org.jodconverter.local.office.LocalOfficeManager;

import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------

/**
 * The Test Console's FTS Diagnostics tab: runs a query against a file's Tika
 * extraction (from the index) and its pdf.js extraction (of the file, or of
 * an office conversion of it) side by side, and maps each Tika match to its
 * pdf.js position the way passage-click navigation does.
 */
public class FtsDiagnosticsCtrlr implements TestConsoleTab
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record FtsDiagMatch(int ndx, int tikaOffset, int tikaNormPos, String tikaSnippet,
                              int pdfPage, int pdfNormPos, String pdfSnippet) {}

//---------------------------------------------------------------------------

  @FXML private Button btnFtsDiagConvert, btnFtsDiagExtract;
  @FXML private Label lblFtsDiagConvertedPath, lblFtsDiagStatus;
  @FXML private TableColumn<FtsDiagMatch, String> colFtsDiagNdx, colFtsDiagTikaOffset, colFtsDiagTikaNormOffset, colFtsDiagTikaSnippet,
                                                  colFtsDiagPdfPage, colFtsDiagPdfNormOffset, colFtsDiagPdfSnippet;
  @FXML private TableView<FtsDiagMatch> tvFtsDiagMatches;
  @FXML private TextArea taFtsDiagTika, taFtsDiagPdfJS;
  @FXML private TextField tfFtsDiagPath, tfFtsDiagQuery;

  private FilePath ftsDiagConvertedPath;

//---------------------------------------------------------------------------

  @Override public void init(TestConsoleDlgCtrlr console)
  {
    if (jxBrowserDisabled)
    {
      // pdf.js extraction runs in the browser engine; without it (missing JxBrowser license
      // key or engine startup failure), the extraction here would stall waiting for an
      // extractor pool that can never be populated.

      disableAll(btnFtsDiagConvert, btnFtsDiagExtract);

      lblFtsDiagStatus.setText("Extraction unavailable: browser engine not initialized" +
        (BrowserEngine.licenseKeyIsMissing() ? " (no JxBrowser license key)" : "") + '.');
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagBrowseClick()
  {
    FileChooser fileChooser = new FileChooser();
    fileChooser.setTitle("Select file for FTS diagnostics");

    if (db.isLoaded())
      fileChooser.setInitialDirectory(db.getRootPath().toFile());

    FilePath filePath = showOpenDialog(fileChooser);

    if (FilePath.isEmpty(filePath)) return;

    tfFtsDiagPath.setText(filePath.toString());
    lblFtsDiagConvertedPath.setText("");
    ftsDiagConvertedPath = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagConvertClick()
  {
    String pathStr = tfFtsDiagPath.getText();

    if (strNullOrBlank(pathStr))
    {
      falseWithErrorPopup("Please select a file.", tfFtsDiagPath);
      return;
    }

    FilePath filePath = FilePath.of(pathStr);

    if (filePath.exists() == false)
    {
      falseWithErrorPopup("File not found: " + pathStr, tfFtsDiagPath);
      return;
    }

    lblFtsDiagStatus.setText("Converting...");

    // Convert to PDF using JodConverter (same as OfficePreviewer)

    Thread convertThread = new HyperThread("FtsDiagConvert", () ->
    {
      try
      {
        FilePath tempDirFilePath = tempDir().resolve("hnFtsDiag_" + System.currentTimeMillis());
        Files.createDirectories(tempDirFilePath.toPath());

        FilePath outputFilePath = tempDirFilePath.resolve("converted.pdf");

        String officePath = getOfficeHome();

        if (officePath.isBlank())
        {
          Platform.runLater(() -> lblFtsDiagStatus.setText("No office installation path configured in settings."));
          return;
        }

        List<Integer> ports = new ArrayList<>();
        findAvailablePorts(1, ports);

        LocalOfficeManager officeManager = LocalOfficeManager.builder().officeHome(officePath).portNumbers(ports.getFirst()).build();

        officeManager.start();

        try
        {
          LocalConverter.make(officeManager).convert(filePath.toFile()).to(outputFilePath.toFile()).execute();
        }
        finally
        {
          OfficeUtils.stopQuietly(officeManager);
        }

        Platform.runLater(() ->
        {
          ftsDiagConvertedPath = outputFilePath;
          lblFtsDiagConvertedPath.setText(outputFilePath.toString());
          lblFtsDiagStatus.setText("Conversion complete.");
        });
      }
      catch (Exception e)
      {
        Platform.runLater(() -> lblFtsDiagStatus.setText("Conversion failed: " + getThrowableMessage(e)));
      }
    });

    convertThread.setDaemon(true);
    convertThread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagShowConvertedClick()
  {
    if (ftsDiagConvertedPath != null)
      highlightFileInExplorer(ftsDiagConvertedPath);
    else
      infoPopup("No converted file available. Click Convert first.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagExportClick()
  {
    try
    {
      FilePath exportDir = testDir().resolve("fts-diag");
      exportDir.createDirectories();

      String tikaText = taFtsDiagTika.getText(),
             pdfText  = taFtsDiagPdfJS.getText();

      if (strNullOrBlank(tikaText) && strNullOrBlank(pdfText))
      {
        infoPopup("No extraction data to export. Run Extract & Search first.");
        return;
      }

      if (strNullOrBlank(tikaText) == false)
        Files.writeString(exportDir.resolve("tika-normalized.txt").toPath(), tikaText);

      if (strNullOrBlank(pdfText) == false)
        Files.writeString(exportDir.resolve("pdfjs-normalized.txt").toPath(), pdfText);

      // Export match table as TSV

      StringBuilder tsv = new StringBuilder();
      tsv.append("#\tTika Offset\tTika Norm Pos\tTika Snippet\tPDF Page\tPDF Norm Pos\tPDF Snippet\n");

      for (FtsDiagMatch m : tvFtsDiagMatches.getItems())
      {
        tsv.append(m.ndx        ()).append('\t')
           .append(m.tikaOffset ()).append('\t')
           .append(m.tikaNormPos()).append('\t')
           .append(m.tikaSnippet()).append('\t')
           .append(m.pdfPage    ()).append('\t')
           .append(m.pdfNormPos ()).append('\t')
           .append(m.pdfSnippet ()).append('\n');
      }

      Files.writeString(exportDir.resolve("matches.tsv").toPath(), tsv.toString());

      highlightFileInExplorer(exportDir.resolve("matches.tsv"));
    }
    catch (IOException e)
    {
      errorPopup("Export failed: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagExtractClick()
  {
    String pathStr  = tfFtsDiagPath .getText(),
           queryStr = tfFtsDiagQuery.getText();

    if (strNullOrBlank(pathStr))
    {
      falseWithErrorPopup("Please select a file.", tfFtsDiagPath);
      return;
    }

    if (strNullOrBlank(queryStr))
    {
      falseWithErrorPopup("Please enter a query.", tfFtsDiagQuery);
      return;
    }

    FilePath filePath = FilePath.of(pathStr);

    if (filePath.exists() == false)
    {
      falseWithErrorPopup("File not found: " + pathStr, tfFtsDiagPath);
      return;
    }

    FullTextIndexer indexer = db.getFullTextIndexer();
    if (indexer == null)
    {
      errorPopup("Full-text indexer is not running.");
      return;
    }

    lblFtsDiagStatus.setText("Extracting...");

    Thread extractThread = new HyperThread("FtsDiagExtract", () ->
    {
      try
      {
        // Get Tika extraction from the Lucene index

        String dbRelPath = null;

        if (db.isLoaded())
        {
          try { dbRelPath = db.getRootPath().relativize(filePath).toString().replace('\\', '/'); }
          catch (Exception e) { /* not under DB root */ }
        }

        String tikaText = (dbRelPath != null) ? indexer.getStoredContent(dbRelPath) : null;

        // Get pdf.js extraction

        FilePath pdfPath = ftsDiagConvertedPath != null ? ftsDiagConvertedPath : filePath;
        FullTextIndexer.ExtractionResult pdfExtraction = null;

        String mime = getMediaType(filePath).toString();

        if (mime.contains("pdf") || (ftsDiagConvertedPath != null))
          pdfExtraction = indexer.extractPdfText(pdfPath);

        // Normalize both texts

        ArrayList<Integer> tikaPosMap = new ArrayList<>(),
                           pdfPosMap  = new ArrayList<>();

        String normTika   = (tikaText != null) ? normalizeForMatching(tikaText, tikaPosMap) : "",
               pdfRawText = ((pdfExtraction != null) && (pdfExtraction.text() != null)) ? pdfExtraction.text() : null;

        int[] pdfPageOffsets = (pdfExtraction != null) ? pdfExtraction.pageOffsets().clone() : null;

        if ((pdfRawText != null) && db.isLoaded())
          pdfRawText = stripConvertedPdfHeaders(pdfRawText, db.getRootPath().toString().replace('/', '\\'), pdfPageOffsets);

        String normPdf = (pdfRawText != null) ? normalizeForMatching(pdfRawText, pdfPosMap) : "";

        int[] tikaRevMap = (tikaText != null) ? buildReversePositionMap(tikaPosMap, tikaText.length()) : new int[0];

        // Parse and run the query against both texts using temporary indexes

        Query query;

        try (var analyzer = FullTextIndexer.createAnalyzer())
        {
          query = FullTextIndexer.createQueryParser(analyzer).parse(queryStr);
        }
        catch (Exception e)
        {
          Platform.runLater(() -> lblFtsDiagStatus.setText("Query parse error: " + e.getMessage()));
          return;
        }

        // Search Tika text

        List<FullTextIndexer.SearchResult.PageMatch> tikaMatches =
          (tikaText != null) ? FullTextIndexer.searchExtractedText(tikaText, null, query) : List.of();

        // Search pdf.js text

        List<FullTextIndexer.SearchResult.PageMatch> pdfMatches =
          ((pdfExtraction != null) && (pdfExtraction.text() != null))
            ? FullTextIndexer.searchExtractedText(pdfExtraction.text(), pdfPageOffsets, query) : List.of();

        // Build diagnostic match rows by mapping Tika matches to pdf.js positions

        List<FtsDiagMatch> diagMatches = new ArrayList<>();

        for (int ndx = 0; ndx < tikaMatches.size(); ndx++)
        {
          FullTextIndexer.SearchResult.PageMatch tm = tikaMatches.get(ndx);

          int tikaAbsOffset = tm.startOffset();
          if ((tm.hitRanges() != null) && (tm.hitRanges().isEmpty() == false))
            tikaAbsOffset += tm.hitRanges().getFirst().start();

          int tikaNormPos = ((tikaAbsOffset >= 0) && (tikaAbsOffset < tikaRevMap.length)) ? tikaRevMap[tikaAbsOffset] : -1;

          String tikaSnip = (tm.snippet() != null) ? tm.snippet().replaceAll("\\s+", " ").strip() : "";
          tikaSnip = safeSubstring(tikaSnip, 0, 60);

          // Align via the same helper production passage-click navigation uses, so the
          // diagnostics reflect what it actually computes (progressive context windows,
          // mappable-position requirement) rather than a separate approximation.

          int pdfNormPos = ((tikaNormPos >= 0) && (normPdf.isEmpty() == false)) ? findPdfNormPos   (tikaNormPos, normTika, normPdf, pdfPosMap.size()) : -1,
              pdfPage    = ((pdfNormPos  >= 0) && (pdfPageOffsets    != null )) ? pageForPdfNormPos(pdfNormPos, pdfPosMap, pdfPageOffsets) : -1;

          String pdfSnip = (pdfNormPos >= 0) ? safeSubstring(normPdf, pdfNormPos - 20, pdfNormPos + 40) : "";

          diagMatches.add(new FtsDiagMatch(ndx, tikaAbsOffset, tikaNormPos, tikaSnip, pdfPage, pdfNormPos, pdfSnip));
        }

        int tikaMatchCount = tikaMatches.size(), pdfMatchCount = pdfMatches.size();

        Platform.runLater(() ->
        {
          taFtsDiagTika.setText(normTika);
          taFtsDiagPdfJS.setText(normPdf);

          colFtsDiagNdx           .setCellValueFactory(cd -> new SimpleStringProperty(String.valueOf(cd.getValue().ndx())));
          colFtsDiagTikaOffset    .setCellValueFactory(cd -> new SimpleStringProperty(String.valueOf(cd.getValue().tikaOffset())));
          colFtsDiagTikaNormOffset.setCellValueFactory(cd -> new SimpleStringProperty(String.valueOf(cd.getValue().tikaNormPos())));
          colFtsDiagTikaSnippet   .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().tikaSnippet()));
          colFtsDiagPdfPage       .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().pdfPage() > 0 ? String.valueOf(cd.getValue().pdfPage()) : "?"));
          colFtsDiagPdfNormOffset .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().pdfNormPos() >= 0 ? String.valueOf(cd.getValue().pdfNormPos()) : "?"));
          colFtsDiagPdfSnippet    .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().pdfSnippet()));

          tvFtsDiagMatches.getItems().setAll(diagMatches);

          // Click a row to scroll both text areas to the match position

          tvFtsDiagMatches.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) ->
          {
            if (nv == null) return;

            if (nv.tikaNormPos() >= 0)
              taFtsDiagTika.positionCaret(nv.tikaNormPos());

            if (nv.pdfNormPos() >= 0)
              taFtsDiagPdfJS.positionCaret(nv.pdfNormPos());
          });

          lblFtsDiagStatus.setText("Tika: " + tikaMatchCount + " matches, pdf.js: " + pdfMatchCount + " matches, " +
            diagMatches.size() + " mapped");
        });
      }
      catch (Exception e)
      {
        Platform.runLater(() -> lblFtsDiagStatus.setText("Error: " + getThrowableMessage(e)));
        e.printStackTrace();
      }
    });

    extractThread.setDaemon(true);
    extractThread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
