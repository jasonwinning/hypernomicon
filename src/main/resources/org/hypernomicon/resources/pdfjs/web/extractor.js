/*
 * Copyright 2026 Jason Winning
 *
 * This file is new with Hypernomicon and is not part of PDF.js
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

// Assemble one page's text from its pdf.js textContent items, applying the column-aware spacing and
// dehyphenation rules. This depends only on the page's own items, not on how or when the page was
// fetched, so sequential and concurrent extraction produce identical per-page text.

function extractPageText(textContent) {
  var text = '',
      items = textContent.items;

  for (var ndx = 0; ndx < items.length; ndx++) {
    var item = items[ndx],
        t = item.transform;

    if (ndx > 0 && text.length > 0 && item.str.length > 0) {
      var lastChar = text.charAt(text.length - 1);

      if (lastChar !== ' ') {
        var prev = items[ndx - 1],
            pt = prev.transform;

        // Different line (different ty) or gap between items on the same line.
        // Threshold scales with font size to handle OCR'd text where glyph
        // positioning is less precise (e.g. small-caps in scanned documents).

        var fontSize = Math.abs(t[0]) || Math.abs(t[3]) || 10,
            threshold = fontSize * 0.27;

        if (Math.abs(t[5] - pt[5]) > threshold || (t[4] - (pt[4] + prev.width)) > threshold) {

          // Dehyphenate: if text ends with a hyphen at a line break,
          // remove the hyphen and don't add a space (rejoin the word)

          if (lastChar === '-' && Math.abs(t[5] - pt[5]) > threshold) {
            text = text.substring(0, text.length - 1);
          } else {
            text += ' ';
          }
        }
      }
    }

    text += item.str;
  }

  return text.replace(/\s+/g, ' ').trim();
}

function extractDebug(requestID, fileUrl, pageNum) {
  pdfjsLib.GlobalWorkerOptions.workerSrc = '../build/pdf.worker.js';

  pdfjsLib.getDocument(fileUrl).then(function (pdf) {
    pdf.getPage(pageNum).then(function (page) {
      return page.getTextContent().then(function (textContent) {
        var items = textContent.items,
            lines = [];

        for (var ndx = 0; ndx < items.length; ndx++) {
          var item = items[ndx],
              t = item.transform;

          lines.push(
            'ndx=' + ndx +
            ' str=' + JSON.stringify(item.str) +
            ' hasEOL=' + (item.hasEOL || false) +
            ' w=' + (item.width || 0).toFixed(2) +
            ' tx=' + (t ? t[4].toFixed(2) : '?') +
            ' ty=' + (t ? t[5].toFixed(2) : '?') +
            ' fs=' + (t ? t[0].toFixed(2) : '?')
          );
        }

        pdf.destroy();
        javaApp.extractionDone(requestID, lines.join('\n'), '[]');
      });
    }).catch(function (error) {
      // A failure after getDocument resolved (e.g. a corrupt page) won't reach the getDocument
      // reject handler below, so destroy the document and report here to avoid leaking it and
      // hanging the Java-side worker until its timeout.
      pdf.destroy();
      javaApp.extractionFailed(requestID, (error && error.message) ? error.message : String(error));
    });
  }, function (error) {
    javaApp.extractionFailed(requestID, (error && error.message) ? error.message : String(error));
  });
}

function extractText(requestID, fileUrl) {
  pdfjsLib.GlobalWorkerOptions.workerSrc = '../build/pdf.worker.js';

  pdfjsLib.getDocument(fileUrl).then(function (pdf) {
    var pageCount = pdf.numPages,
        pageTexts = new Array(pageCount);

    if (pageCount === 0) {
      pdf.destroy();
      javaApp.extractionDone(requestID, '', '[]');
      return;
    }

    // Assemble the final document text and page offsets once every page has been processed. Pages are
    // concatenated in page order, so this output is independent of the order in which pages were fetched.

    function finish() {
      var fullText = '',
          offsets = [];

      for (var ndx = 0; ndx < pageCount; ndx++) {
        offsets.push(fullText.length);
        fullText += pageTexts[ndx];

        if (ndx < pageCount - 1) {
          fullText += ' ';
        }
      }

      offsets.push(fullText.length);  // trailing sentinel

      // Substitute NUL chars with a literal '?'. PDFs with broken
      // ToUnicode CMaps yield NUL for unmapped glyphs (commonly
      // ligature glyphs like Th/fi/ft/tt/cr that the font's subset
      // doesn't expose mappings for). The JxBrowser JS-to-Java string
      // bridge terminates at NUL (C-string convention), so leaving
      // NUL in the assembled text truncates everything past the first
      // unmapped glyph.

      fullText = fullText.replace(/\x00/g, '?');

      pdf.destroy();
      javaApp.extractionDone(requestID, fullText, JSON.stringify(offsets));
    }

    function fail(error) {
      // Release the document so a mid-extraction failure can't leak it or hang the Java-side worker
      // until its timeout, then report the failure.
      pdf.destroy();
      javaApp.extractionFailed(requestID, (error && error.message) ? error.message : String(error));
    }

    // Process one page at a time, holding only a single page's textContent in memory at once instead of
    // every page's concurrently. This bounds peak memory, which is what lets very large PDFs (e.g.
    // multi-thousand-page books) extract without exhausting RAM. Recursing through promise callbacks runs
    // each step as a microtask, so the call stack does not grow with page count.

    function processPage(pageNum) {
      if (pageNum > pageCount) {
        finish();
        return;
      }

      pdf.getPage(pageNum).then(function (page) {
        return page.getTextContent().then(function (textContent) {
          pageTexts[pageNum - 1] = extractPageText(textContent);
          processPage(pageNum + 1);
        });
      }).catch(fail);
    }

    processPage(1);
  }, function (error) {
    javaApp.extractionFailed(requestID, (error && error.message) ? error.message : String(error));
  });
}
