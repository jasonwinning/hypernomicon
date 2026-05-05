/*
 * Copyright 2017-2026 Jason Winning
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

// Stored hit data for all pages, keyed by 1-based page number.
// Set by setAllHits(), consumed by applyHitsToPage() which is called from
// TextLayerBuilder._finishRendering when a page's text layer becomes ready.

var pendingHits = null;  // { "1": [[s,e],...], "3": [[s,e],...], ... }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Store all hit data for the current file. The viewer applies highlights
 * lazily as each page's text layer finishes rendering.
 *
 * @param hitsJson  JSON object mapping 1-based page numbers to arrays of
 *                  [startOffset, endOffset] pairs (page-relative offsets)
 */
function setAllHits(hitsJson) {
  clearHighlights();
  pendingHits = JSON.parse(hitsJson);
  applyPendingHitsToRenderedPages();
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Apply pending hits to any pages whose text layer has already finished
 * rendering. Called after setAllHits() to catch pages that rendered before
 * the hits arrived. Pages that haven't rendered yet will be picked up by the
 * _finishRendering hook in viewer.js when they do; no retry needed here.
 */
function applyPendingHitsToRenderedPages() {
  if (pendingHits == null) return;
  if (typeof PDFViewerApplication === 'undefined') return;

  var pdfViewer = PDFViewerApplication.pdfViewer;
  if (pdfViewer == null) return;

  for (var pageNumStr in pendingHits) {
    var pageNum = parseInt(pageNumStr);
    var pageView = pdfViewer.getPageView(pageNum - 1);

    if ((pageView != null) && (pageView.textLayer != null) && pageView.textLayer.renderingDone) {
      applyHitsToPage(pageNum);
    }
  }
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Apply pending hits for a single page, assuming the text layer is ready.
 * Called from the _finishRendering hook in viewer.js (push) and from
 * applyPendingHitsToRenderedPages after a pre-checked readiness test (pull).
 * Highlighting only; all navigation is handled by Java via goToPage.
 *
 * Re-runs the same concatenation logic as extractor.js to translate Lucene
 * post-processed character offsets back to the raw text items, then maps item
 * indices to textDiv elements and adds the .fts-highlight CSS class. The
 * .fts-active class on the text layer overrides its default opacity: 0.2 so
 * highlights are visible.
 */
function applyHitsToPage(pageNum) {
  if (pendingHits == null) return;

  var hitRanges = pendingHits[String(pageNum)];
  if ((hitRanges == null) || (hitRanges.length === 0)) return;

  var pageView = PDFViewerApplication.pdfViewer.getPageView(pageNum - 1);
  if ((pageView == null) || (pageView.textLayer == null) || (pageView.textLayer.renderingDone == false)) {
    // Not ready. The _finishRendering hook will re-call us when it is.
    return;
  }

  var pdfPage = pageView.pdfPage;
  if (pdfPage == null) return;

  pdfPage.getTextContent().then(function (textContent) {
    var items = textContent.items;

    // Re-run the same concatenation logic as extractor.js to build an offset map.
    // Each entry maps a character offset in the post-processed text to
    // { itemNdx, charNdx } in the raw text items.

    var text = '',
        offsetMap = [];  // offsetMap[textPos] = { itemNdx, charNdx }

    for (var ndx = 0; ndx < items.length; ndx++) {
      var item = items[ndx],
          t = item.transform;

      if (ndx > 0 && text.length > 0 && item.str.length > 0) {
        var lastChar = text.charAt(text.length - 1);

        if (lastChar !== ' ') {
          var prev = items[ndx - 1],
              pt = prev.transform;

          var fontSize = Math.abs(t[0]) || Math.abs(t[3]) || 10,
              threshold = fontSize * 0.27;

          if (Math.abs(t[5] - pt[5]) > threshold || (t[4] - (pt[4] + prev.width)) > threshold) {
            if (lastChar === '-' && Math.abs(t[5] - pt[5]) > threshold) {
              // Dehyphenate: remove the hyphen (don't map it)
              text = text.substring(0, text.length - 1);
              offsetMap.length = text.length;
            } else {
              // Insert space (no item mapping for inserted spaces)
              offsetMap.push(null);
              text += ' ';
            }
          }
        }
      }

      for (var c = 0; c < item.str.length; c++) {
        offsetMap.push({ itemNdx: ndx, charNdx: c });
        text += item.str.charAt(c);
      }
    }

    // Collapse whitespace: replicate the \s+ replacement

    var collapsedMap = [];
    var inWhitespace = false;

    for (var pos = 0; pos < text.length; pos++) {
      var ch = text.charAt(pos);

      if (/\s/.test(ch)) {
        if (inWhitespace == false) {
          collapsedMap.push(offsetMap[pos]);
          inWhitespace = true;
        }
      } else {
        collapsedMap.push(offsetMap[pos]);
        inWhitespace = false;
      }
    }

    // Trim leading/trailing space mappings

    var trimStart = 0, trimEnd = collapsedMap.length;
    while (trimStart < trimEnd && collapsedMap[trimStart] == null) trimStart++;
    while (trimEnd > trimStart && collapsedMap[trimEnd - 1] == null) trimEnd--;
    collapsedMap = collapsedMap.slice(trimStart, trimEnd);

    // Now collapsedMap[i] maps post-processed character i to { itemNdx, charNdx }.
    // Find which text layer divs to highlight for each hit range.

    var textLayerDiv = pageView.textLayer.textLayerDiv;
    if (textLayerDiv == null) return;

    // pdf.js v2.0.943 stores text layer elements in textDivs (div elements, not spans)
    var textDivs = pageView.textLayer.textDivs;
    if ((textDivs == null) || (textDivs.length === 0)) return;

    for (var h = 0; h < hitRanges.length; h++) {
      var hitStart = hitRanges[h][0],
          hitEnd   = hitRanges[h][1];

      if (hitStart < 0) hitStart = 0;
      if (hitEnd > collapsedMap.length) hitEnd = collapsedMap.length;

      var itemsInHit = new Set();

      for (var p = hitStart; p < hitEnd; p++) {
        var mapping = collapsedMap[p];
        if (mapping != null) itemsInHit.add(mapping.itemNdx);
      }

      itemsInHit.forEach(function (itemNdx) {
        if (itemNdx < textDivs.length) {
          textDivs[itemNdx].classList.add('fts-highlight');
        }
      });
    }

    // Raise the text layer opacity so highlights are visible.
    textLayerDiv.classList.add('fts-active');
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Clear all stored hit data and remove all highlights.
 */
function clearAllHits() {
  pendingHits = null;
  clearHighlights();
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function openPdfFile(fileStr, pageNum, sidebarView) {

  if (PDFViewerApplication.initialized == false) {
    window.setTimeout(openPdfFile, 50, fileStr, pageNum, sidebarView);
    return;
  }

  PDFViewerApplicationOptions.set('initialPage', pageNum);
  PDFViewerApplicationOptions.set('sidebarViewOnLoad', sidebarView);
  PDFViewerApplicationOptions.set('disablePageMode', true);
  PDFViewerApplicationOptions.set('showPreviousViewOnLoad', false);

  PDFViewerApplication.pdfViewer.eventBus.on('pagechange', function (e) { javaApp.pageChange(e.pageNumber); });
  PDFViewerApplication.pdfViewer.eventBus.on('sidebarviewchanged', function (e) { javaApp.sidebarChange(e.view); });

  PDFViewerApplication.open(fileStr).then(function() {
    javaApp.openDone(true, { });
  }, function (error) {
    javaApp.openDone(false, error);
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function closePdfFile() {
	PDFViewerApplication.close().then(function () {
	  javaApp.closeDone(true, { });
	}, function (error) {
	  javaApp.closeDone(false, error);
	});
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function clearHighlights() {
  var highlighted = document.querySelectorAll('.fts-highlight');

  for (var ndx = 0; ndx < highlighted.length; ndx++) {
    highlighted[ndx].classList.remove('fts-highlight');
  }

  // Restore normal text layer opacity
  var activeLayers = document.querySelectorAll('.fts-active');

  for (var ndx = 0; ndx < activeLayers.length; ndx++) {
    activeLayers[ndx].classList.remove('fts-active');
  }
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function getPdfData() {
  var pagesCount = PDFViewerApplication.pagesCount;
  var pagesLeft = pagesCount;
  var params = { intent: 'display' };
  var annotPages = [];
  var pdfDocument = PDFViewerApplication.pdfDocument;

  pdfDocument.getPageLabels().then(function (pageLabels) {
    for (var pageNum = 1; pageNum <= pagesCount; ++pageNum) {
      pdfDocument.getPage(pageNum).then(function (pageNum, pdfPage) {
        pdfPage.getAnnotations(params).then(function (pageNum, annotations) {
          for (var ndx = 0; ndx < annotations.length; ndx++) {
            var subtype = annotations[ndx].subtype;
            if ((subtype !== "Link") && (subtype !== "Widget")) {
              if (annotPages.indexOf(pageNum) === -1) {
                annotPages.push(pageNum);
              }
            }
          }
          pagesLeft--;
          if (pagesLeft === 0) {
            javaApp.setData({ annotPages, pageLabels });
          }
        }.bind(null, pageNum));
      }.bind(null, pageNum));
    }
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
