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
var pendingHitsScrolled = false;  // true after first scroll to a highlight

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
  console.log('FTS-PDF: setAllHits called, json length=' + hitsJson.length);
  clearHighlights();
  pendingHits = JSON.parse(hitsJson);
  pendingHitsScrolled = false;
  var pageCount = Object.keys(pendingHits).length;
  console.log('FTS-PDF: stored hits for ' + pageCount + ' pages');
  applyPendingHitsToRenderedPages();
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Apply pending hits to any pages that have already finished rendering.
 * Called after setAllHits() and also from _finishRendering hook in viewer.js.
 */
function applyPendingHitsToRenderedPages() {
  if (pendingHits == null) return;
  if (typeof PDFViewerApplication === 'undefined') return;

  var pdfViewer = PDFViewerApplication.pdfViewer;
  if (pdfViewer == null) return;

  for (var pageNumStr in pendingHits) {
    var pageNum = parseInt(pageNumStr);
    applyHitsToPage(pageNum);
  }

  // Scroll to the first highlight if one was applied
  var firstHighlight = document.querySelector('.fts-highlight');
  if (firstHighlight != null) {
    firstHighlight.scrollIntoView({ behavior: 'smooth', block: 'center' });
  }
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Apply pending hits for a single page, if the text layer is ready.
 * Called from _finishRendering hook and from applyPendingHitsToRenderedPages.
 */
function applyHitsToPage(pageNum) {
  if (pendingHits == null) return;

  var ranges = pendingHits[String(pageNum)];
  if ((ranges == null) || (ranges.length === 0)) return;

  var pageView = PDFViewerApplication.pdfViewer.getPageView(pageNum - 1);
  if ((pageView == null) || (pageView.textLayer == null) || (pageView.textLayer.renderingDone == false)) {
    console.log('FTS-PDF: applyHitsToPage(' + pageNum + '): text layer not ready');
    return;
  }

  console.log('FTS-PDF: applyHitsToPage(' + pageNum + '): applying ' + ranges.length + ' ranges');
  doHighlightHits(pageNum, ranges, false, function() {
    // Scroll to the first highlight once (when the first page with hits renders)
    if (pendingHitsScrolled == false) {
      var firstHighlight = document.querySelector('.fts-highlight');
      console.log('FTS-PDF: scroll check (onDone): firstHighlight=' + (firstHighlight != null ? 'yes' : 'null') + ' page=' + pageNum);
      if (firstHighlight != null) {
        firstHighlight.scrollIntoView({ behavior: 'smooth', block: 'center' });
        pendingHitsScrolled = true;
        console.log('FTS-PDF: scrolled to first highlight on page ' + pageNum);
      }
    }
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Clear all stored hit data and remove all highlights.
 */
function clearAllHits() {
  pendingHits = null;
  pendingHitsScrolled = false;
  clearHighlights();
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function openPdfFile(fileStr, pageNum, sidebarView) {

  if (PDFViewerApplication.initialized == false) {
    window.setTimeout(openPdfFile, 50, fileStr, pageNum, sidebarView);
    return;
  }

  // Opening a new document into this reused viewer must not inherit the prior
  // document's hit data. Drop stored hits (and any existing highlight spans)
  // before the swap; the new file's hits, if any, are pushed by Java after the
  // open completes. This makes a new document open clear prior hits regardless
  // of whether the Java caller remembered to clear them.

  clearAllHits();

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

/**
 * Highlight text ranges on a specific page. Called from Java when displaying
 * FTS search results. The hitRangesJson parameter is a JSON array of
 * [startOffset, endOffset] pairs, where offsets are relative to the page's
 * post-processed text (same concatenation logic as extractor.js).
 *
 * @param pageNum     1-based page number
 * @param hitRangesJson  JSON string: [[start1, end1], [start2, end2], ...]
 */
function highlightHits(pageNum, hitRangesJson) {
  clearHighlights();

  var hitRanges = JSON.parse(hitRangesJson);
  if (hitRanges.length === 0) return;

  // Wait for the text layer to be rendered before highlighting.
  // pdf.js renders text layer spans lazily when the page is visible.
  waitForTextLayer(pageNum, function () {
    doHighlightHits(pageNum, hitRanges, true);
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function waitForTextLayer(pageNum, callback) {
  var attempts = 0, maxAttempts = 40;  // 40 * 100ms = 4 seconds max

  function check() {
    var pageView = PDFViewerApplication.pdfViewer.getPageView(pageNum - 1);

    if (pageView != null && pageView.textLayer != null && pageView.textLayer.renderingDone) {
      callback();
      return;
    }

    attempts++;

    if (attempts < maxAttempts) {
      setTimeout(check, 100);
    }
  }

  check();
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function doHighlightHits(pageNum, hitRanges, shouldScroll, onDone) {
  var pageView = PDFViewerApplication.pdfViewer.getPageView(pageNum - 1);
  if (pageView == null) return;

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
    var collapsedText = '',
        collapsedMap = [];

    var inWhitespace = false;

    for (var pos = 0; pos < text.length; pos++) {
      var ch = text.charAt(pos);

      if (/\s/.test(ch)) {
        if (inWhitespace == false) {
          collapsedMap.push(offsetMap[pos]);
          collapsedText += ' ';
          inWhitespace = true;
        }
      } else {
        collapsedMap.push(offsetMap[pos]);
        collapsedText += ch;
        inWhitespace = false;
      }
    }

    // Trim leading/trailing spaces
    var trimStart = 0, trimEnd = collapsedText.length;

    while (trimStart < trimEnd && collapsedText.charAt(trimStart) === ' ') trimStart++;
    while (trimEnd > trimStart && collapsedText.charAt(trimEnd - 1) === ' ') trimEnd--;

    collapsedMap = collapsedMap.slice(trimStart, trimEnd);

    // Now collapsedMap[i] maps post-processed character i to { itemNdx, charNdx }
    // Find which text layer spans to highlight for each hit range

    var textLayerDiv = pageView.textLayer ? pageView.textLayer.textLayerDiv : null;

    if (textLayerDiv == null) return;

    // pdf.js v2.0.943 stores text layer elements in textDivs array (div elements, not spans)
    var textDivs = pageView.textLayer.textDivs;

    if ((textDivs == null) || (textDivs.length === 0)) return;

    // The page container holds both the canvas and the text layer.
    // We add highlight overlays as children of the page container so they're
    // outside the text layer's opacity: 0.2 compositing group.
    var pageContainer = textLayerDiv.parentNode;

    if (pageContainer == null) return;

    for (var h = 0; h < hitRanges.length; h++) {
      var hitStart = hitRanges[h][0],
          hitEnd   = hitRanges[h][1];

      // Clamp to valid range
      if (hitStart < 0) hitStart = 0;
      if (hitEnd > collapsedMap.length) hitEnd = collapsedMap.length;

      // Collect the set of item indices that are part of this hit
      var itemsInHit = new Set();

      for (var pos = hitStart; pos < hitEnd; pos++) {
        var mapping = collapsedMap[pos];
        if (mapping != null) {
          itemsInHit.add(mapping.itemNdx);
        }
      }

      // Highlight text divs
      itemsInHit.forEach(function (itemNdx) {
        if (itemNdx < textDivs.length) {
          textDivs[itemNdx].classList.add('fts-highlight');
        }
      });
    }

    // Raise the text layer opacity so highlights are visible
    var hlCount = textLayerDiv.querySelectorAll('.fts-highlight').length;
    console.log('FTS-PDF: doHighlightHits page=' + pageNum + ' hlCount=' + hlCount + ' textDivs=' + textDivs.length + ' collapsedMapLen=' + collapsedMap.length);
    textLayerDiv.classList.add('fts-active');

    if (hlCount > 0) {
      var firstHl = textLayerDiv.querySelector('.fts-highlight');
      var tlClasses = textLayerDiv.className;
      var tlStyle = window.getComputedStyle(textLayerDiv);
      var hlStyle = window.getComputedStyle(firstHl);
      console.log('FTS-PDF: textLayerDiv classes=' + tlClasses + ' opacity=' + tlStyle.opacity + ' display=' + tlStyle.display);
      console.log('FTS-PDF: highlight opacity=' + hlStyle.opacity + ' bgColor=' + hlStyle.backgroundColor + ' display=' + hlStyle.display + ' text=' + firstHl.textContent.substring(0, 20));
      console.log('FTS-PDF: textLayerDiv parent=' + (textLayerDiv.parentNode ? textLayerDiv.parentNode.className : 'null'));
    }

    // Scroll to the first highlight only when explicitly requested (e.g., clicking
    // a specific match in the context pane). When applying hits from setAllHits/
    // applyHitsToPage, we don't scroll because the user already navigated to the
    // desired page.
    if (shouldScroll) {
      var firstHighlight = textLayerDiv.querySelector('.fts-highlight');

      if (firstHighlight != null) {
        firstHighlight.scrollIntoView({ behavior: 'smooth', block: 'center' });
      }
    }

    if (typeof onDone === 'function') onDone();
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
