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

// Glue between Hypernomicon's Java side (window.javaApp, injected by JxBrowser
// before page scripts run) and the stock pdf.js 6 viewer. Unlike the pdf.js
// 2.0.943 era, the viewer files themselves are unpatched: everything the old
// inline patches did is done here via the viewer's public objects and eventBus.
//
// Hit highlighting strategy (validated against pdf.js 6.1.200): Hypernomicon's
// Lucene-derived hit offsets are converted from Hypernomicon's extracted-text
// space to the find controller's raw page-content space, injected into
// PDFFindController's match arrays (sorted ascending; out-of-order entries are
// silently dropped by the highlighter), and rendered by pdf.js's native
// highlight machinery via the 'updatetextlayermatches' event. This gives exact
// sub-span highlights (better than the whole-text-div CSS approach the 2.0.943
// integration used).

'use strict';

// Opening a document on top of one that still has queued page work (render
// queue, text layers, annotation layers) makes every still-pending operation for
// the old document fail; one rapid supersession of a large PDF can log hundreds
// of rejections. They come in two shapes: operations holding the old document's
// destroyed worker transport reject with "Transport destroyed", and viewer
// internals that re-read their nulled document field die on null.getPage. Both
// are the expected byproduct of superseding a document mid-work (the promised
// work is moot), so swallow exactly those two and let every other rejection
// surface.

window.addEventListener('unhandledrejection', function(event) {
  var reason = event.reason;
  var message = (reason && reason.message) ? reason.message : String(reason);

  if ((message === 'Transport destroyed') ||
      (message === "Cannot read properties of null (reading 'getPage')"))
    event.preventDefault();
});

var listenersRegistered = false;

// Stored hit data for all pages, keyed by 1-based page number:
// { "1": [[s,e],...], "3": [[s,e],...] } with offsets in Hypernomicon's
// extracted-text space (column-aware spacing, dehyphenation, collapsed
// whitespace); see extractor.js for the algorithm that defines that space.
var pendingHits = null;

// Per-page converted matches in find-controller space, built asynchronously by
// applyAllHits: convertedMatches[pageNdx] = { starts: [...], lens: [...] }.
var convertedMatches = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// Hide viewer chrome Hypernomicon doesn't want (see hypernomicon.css). The
// rules are a same-origin stylesheet rather than injected inline styles, which
// the viewer page's CSP (style-src 'self') forbids.

(function () {
  var link = document.createElement('link');
  link.rel = 'stylesheet';
  link.href = 'hypernomicon.css';
  document.head.appendChild(link);
})();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// Status overlay: an opaque in-page panel covering the whole viewer (chrome
// included), through which the Java side displays conversion progress and
// notice states (unable to preview, office installation missing, idle). It
// replaces the JavaFX panel that was swapped over the BrowserView: the view is
// a native hardware surface that ignores JavaFX visibility and z-order
// (observed as a window-scale black rectangle on Linux), so status must render
// inside the page. Styles live in hypernomicon.css.
//
// The Java side is the only writer; these functions hold display state only.
// showStatusOverlay is idempotent: an identical kind and message leave the DOM
// alone, so repeated updates never restart the progress animation.

var statusOverlayDiv = null, statusOverlayShownSpec = null;

function buildStatusOverlay() {
  if (statusOverlayDiv !== null) return;

  statusOverlayDiv = document.createElement('div');
  statusOverlayDiv.id = 'hnStatusOverlay';
  statusOverlayDiv.innerHTML =
    '<div id="hnStatusBox">' +
      '<div id="hnStatusGlyph">&#9432;</div>' +
      '<div id="hnStatusMessage"></div>' +
      '<div id="hnStatusProgress"><div id="hnStatusProgressBar"></div></div>' +
    '</div>';
  statusOverlayDiv.style.display = 'none';
  document.body.appendChild(statusOverlayDiv);

  // While a status shows, the live viewer underneath must not react to the
  // keyboard; pointer-events blocks only the mouse. Capture phase, so the
  // viewer's own window-level handlers never see the event.

  ['keydown', 'keyup', 'keypress'].forEach(function (type) {
    window.addEventListener(type, function (event) {
      if (statusOverlayDiv.style.display !== 'none') {
        event.stopImmediatePropagation();
        event.preventDefault();
      }
    }, true);
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Shows the status overlay. spec = { kind: 'progress'|'notice', message: s }:
 * progress adds an indeterminate animation, notice an info glyph. An empty
 * message shows a bare neutral panel: the idle state of a warmed, empty pane
 * (never the viewer's own chrome with no document).
 */
function showStatusOverlay(spec) {

  // This script runs from <head>, so a call buffered before parsing (or made
  // between parse and DOMContentLoaded) can arrive before body exists;
  // re-buffer and the DOMContentLoaded drain below replays it.

  if (document.body === null) {
    window.__hnPendingStatus = spec;
    return;
  }

  buildStatusOverlay();

  // Font size follows the application preference (see execStatusOverlay);
  // applied before the idempotence check so a preference change takes effect
  // even when the status text itself is unchanged.

  statusOverlayDiv.style.fontSize =
    ((typeof spec.fontSize === 'number') && (spec.fontSize >= 1)) ? spec.fontSize + 'px' : '';

  if ((statusOverlayShownSpec !== null) &&
      (statusOverlayShownSpec.kind === spec.kind) && (statusOverlayShownSpec.message === spec.message))
    return;

  statusOverlayShownSpec = spec;

  var blank = spec.message === '';

  document.getElementById('hnStatusMessage').textContent = spec.message;
  document.getElementById('hnStatusGlyph').style.display = ((blank === false) && (spec.kind === 'notice')) ? '' : 'none';
  document.getElementById('hnStatusProgress').style.display = ((blank === false) && (spec.kind === 'progress')) ? '' : 'none';

  statusOverlayDiv.style.display = '';
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function hideStatusOverlay() {
  window.__hnPendingStatus = null;  // also cancels a buffered show that never displayed

  if (statusOverlayDiv === null) return;

  statusOverlayShownSpec = null;
  statusOverlayDiv.style.display = 'none';
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function drainPendingStatus() {
  if (window.__hnPendingStatus) {
    var pendingStatus = window.__hnPendingStatus;
    window.__hnPendingStatus = null;
    showStatusOverlay(pendingStatus);
  }
}

document.addEventListener('DOMContentLoaded', drainPendingStatus);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// Init-time viewer options must be set on the 'webviewerloaded' event: it fires
// after the viewer module evaluates (so PDFViewerApplicationOptions exists) but
// before PDFViewerApplication initializes its components and consumes them.
// Setting them per-open is too late for options read at initialization.
//
// annotationEditorMode -1 disables the annotation editor subsystem entirely.
// Hiding the editor toolbar buttons (hypernomicon.css) is not enough: gestures
// like double-clicking an existing highlight annotation also enter editing
// mode, and with the buttons hidden there is no way back out. Edits could
// never be saved anyway (downloads are rejected); annotations belong in the
// PDF, made by real PDF tools, not in the preview pane.

document.addEventListener('webviewerloaded', function () {
  PDFViewerApplicationOptions.set('annotationEditorMode', -1);
  PDFViewerApplicationOptions.set('viewOnLoad', 1);

  // The document is served locally with range support, so every chunk the
  // viewer needs is a fast on-demand read. Prefetching the entire file in the
  // background (the default) gains nothing here, and on very large documents
  // it monopolizes the disk and the transport channel for tens of seconds
  // (competing with page fetches and the Java-side annotation scan) while
  // accumulating the whole file in worker memory.

  PDFViewerApplicationOptions.set('disableAutoFetch', true);
});

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function viewerReady() {
  return (typeof PDFViewerApplication !== 'undefined') && PDFViewerApplication.initialized;
}

function registerListeners() {
  if (listenersRegistered) return;
  listenersRegistered = true;

  var eventBus = PDFViewerApplication.eventBus;

  eventBus.on('pagechanging',       function (e) { javaApp.pageChange(e.pageNumber); });
  eventBus.on('sidebarviewchanged', function (e) { javaApp.sidebarChange(e.view); });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Opens a PDF (served over the hnres: scheme) at the given 1-based page, with
 * the given sidebar view (0 = none; values match pdf.js SidebarView).
 * Retries while the viewer finishes initializing (the Java side never polls),
 * but not forever: a stalled initialization is reported as a failed open.
 */
function openPdfFile(fileUrl, pageNum, sidebarView) {

  if (viewerReady() === false) {
    if (window.__hnRetryCount == null)
      window.__hnRetryCount = 0;

    // Retrying is normal for a moment after the viewer page loads; a long wait
    // means the viewer never finished initializing and is worth a log entry.
    // Give up after ~30 seconds: retrying forever would leave the Java open
    // coordinator waiting on an openDone that never comes, silently wedging
    // every later open behind it, so convert the stall into a failed open and
    // let the normal failure handling take over.

    if (++window.__hnRetryCount >= 600) {
      window.__hnRetryCount = null;
      javaApp.openDone(false, 0, 'The viewer never finished initializing');
      return;
    }

    if ((window.__hnRetryCount % 100) === 0)
      console.log('openPdfFile still waiting for viewer init after ' + window.__hnRetryCount + ' retries');

    window.setTimeout(openPdfFile, 50, fileUrl, pageNum, sidebarView);
    return;
  }

  window.__hnRetryCount = null;

  registerListeners();

  clearAllHits();

  // The sidebar view goes through the viewer's own stock option
  // (sidebarViewOnLoad), applied before open(). (Touching viewer components like
  // pdfSidebar directly during initialization can abort the document load;
  // learned the hard way.) viewOnLoad is set to 1 (INITIAL, ignoring stored
  // per-document view history): Hypernomicon's caller decides the starting page,
  // and neither the previous document's scroll position nor the viewer's own
  // history may override it. The pdf.js 2.0.943 integration got the same effect
  // from showPreviousViewOnLoad=false plus localStorage-disabling patches;
  // viewOnLoad is that option's modern name.
  //
  // The requested page is applied EXPLICITLY once pages are loaded, as a physical
  // page number (Hypernomicon page numbers are physical). The stock alternative,
  // the initialBookmark 'page=' mechanism the viewer uses for #page=N URL hashes,
  // proved unreliable (a fresh open landed on an unrelated page; open() appears
  // to reset the bookmark internally), and setting the page after 'pagesloaded'
  // overrides both the viewer's default and any stored view history.

  var pagesEventBus = PDFViewerApplication.eventBus;

  function onPagesLoaded() {
    pagesEventBus.off('pagesloaded', onPagesLoaded);

    if (pageNum >= 1)
      PDFViewerApplication.pdfViewer.currentPageNumber = pageNum;
  }

  pagesEventBus.on('pagesloaded', onPagesLoaded);

  if (typeof PDFViewerApplicationOptions !== 'undefined') {
    PDFViewerApplicationOptions.set('sidebarViewOnLoad', sidebarView);
    PDFViewerApplicationOptions.set('viewOnLoad', 1);
  }

  PDFViewerApplication.open({ url: fileUrl }).then(function () {
    javaApp.openDone(true, PDFViewerApplication.pdfDocument.numPages, '');
  }, function (error) {
    javaApp.openDone(false, 0, (error && error.message) ? error.message : String(error));
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

function closePdfFile() {
  PDFViewerApplication.close().then(function () {
    javaApp.closeDone(true, '');
  }, function (error) {
    javaApp.closeDone(false, (error && error.message) ? error.message : String(error));
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Reports the document's page labels to Java as JSON. Annotated pages are
 * deliberately not collected here: doing it through the viewer forces every
 * page dictionary through the worker's single thread, where the walk competes
 * with page rendering (over a minute on a 10,000-page document, even batched).
 * The Java side reads them straight from the file instead
 * (PDFAnnotationScanner), which takes seconds regardless of document size.
 */
function getPdfData() {
  var pdfDocument = PDFViewerApplication.pdfDocument;
  if (pdfDocument == null) return;

  pdfDocument.getPageLabels().then(function (pageLabels) {
    javaApp.setData(JSON.stringify({ pageLabels: pageLabels }));
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Rebuilds Hypernomicon's extraction-space offset map for one page and returns
 * match positions for the page's hit ranges in the space pdf.js's text
 * highlighter consumes: the plain concatenation of the page's item strings.
 * This reruns the same concatenation algorithm as extractor.js to map
 * extraction-space offsets back to (item, char) coordinates, then converts
 * those to offsets in that concatenation (see the itemStart comment below for
 * why the highlighter's space, not the find controller's, is the target).
 */
function convertPageHits(pdfPage, hitRanges) {
  // disableNormalization matters three ways: (1) the find controller builds its
  // raw page content from unnormalized items, so itemStart must be computed over
  // the same strings; (2) extractor.js extracts from unnormalized items, so the
  // offset-map re-run must too; (3) pdf.js 2.0.943 (which built existing indexes)
  // had no normalization at all. All three getTextContent call sites (here and
  // two in extractor.js) must stay in lockstep.
  return pdfPage.getTextContent({ disableNormalization: true }).then(function (textContent) {
    var items = textContent.items;

    // Cumulative offset of each item's first character in the TEXT HIGHLIGHTER's
    // space: the concatenation of item.str values only. Note this deliberately
    // differs from the find controller's page-content space, which inserts "\n"
    // per hasEOL item (viewer.mjs PDFFindController): the highlighter's
    // textContentItemsStr gets only item.str (the hasEOL newline becomes a <br>
    // element with no string entry), and injected match values are consumed by
    // the highlighter, so its space is the one that must be matched. Counting
    // the hasEOL newlines here displaced highlights by one character per line
    // of preceding text on hasEOL-heavy documents.

    var itemStart = new Array(items.length);
    var pos = 0;

    for (var ndx = 0; ndx < items.length; ndx++) {
      itemStart[ndx] = pos;
      pos += items[ndx].str.length;
    }

    // Rerun the extraction concatenation to map extraction-space offsets to
    // (itemNdx, charNdx); see extractPageText in extractor.js.

    var text = '',
        offsetMap = [],  // offsetMap[extractionPos] = { itemNdx: n, charNdx: c } or null for inserted spaces
        prevTextNdx = -1;  // last item with a non-empty str; see extractPageText in extractor.js

    for (var ndx2 = 0; ndx2 < items.length; ndx2++) {
      var item = items[ndx2],
          t = item.transform;

      if (prevTextNdx >= 0 && text.length > 0 && item.str.length > 0) {
        var lastChar = text.charAt(text.length - 1);

        if (lastChar !== ' ') {
          var prev = items[prevTextNdx],
              pt = prev.transform;

          var fontSize = Math.abs(t[0]) || Math.abs(t[3]) || 10,
              threshold = fontSize * 0.27;

          if (Math.abs(t[5] - pt[5]) > threshold || (t[4] - (pt[4] + prev.width)) > threshold) {
            if (lastChar === '-' && Math.abs(t[5] - pt[5]) > threshold) {
              text = text.substring(0, text.length - 1);
              offsetMap.length = text.length;
            } else {
              offsetMap.push(null);
              text += ' ';
            }
          }
        }
      }

      if (item.str.length > 0)
        prevTextNdx = ndx2;

      for (var c = 0; c < item.str.length; c++) {
        offsetMap.push({ itemNdx: ndx2, charNdx: c });
        text += item.str.charAt(c);
      }
    }

    // Collapse whitespace and trim, mirroring the extractor's final replace/trim.
    // The extractor's trim() removes real leading/trailing whitespace characters,
    // not only inserted spaces, so the trim here must strip edge entries that map
    // to whitespace too; keeping them would shift every offset on a page whose
    // item stream begins (or ends) with whitespace.

    var collapsedMap = [],
        collapsedChars = [],
        inWhitespace = false;

    for (var p = 0; p < text.length; p++) {
      var ch = text.charAt(p);

      if (/\s/.test(ch)) {
        if (inWhitespace === false) {
          collapsedMap.push(offsetMap[p]);
          collapsedChars.push(' ');
          inWhitespace = true;
        }
      } else {
        collapsedMap.push(offsetMap[p]);
        collapsedChars.push(ch);
        inWhitespace = false;
      }
    }

    var trimStart = 0, trimEnd = collapsedMap.length;
    while (trimStart < trimEnd && (collapsedMap[trimStart] == null || collapsedChars[trimStart] === ' ')) trimStart++;
    while (trimEnd > trimStart && (collapsedMap[trimEnd - 1] == null || collapsedChars[trimEnd - 1] === ' ')) trimEnd--;
    collapsedMap = collapsedMap.slice(trimStart, trimEnd);

    // Convert each hit range to a find-space (start, length) pair. A range maps
    // to the span from its first to its last mapped character; inserted spaces
    // (null entries) at the edges are skipped.

    var starts = [], lens = [];

    for (var h = 0; h < hitRanges.length; h++) {
      var s = hitRanges[h][0], e = hitRanges[h][1];

      if (s < 0) s = 0;

      if (e > collapsedMap.length) {
        console.log('convertPageHits: range [' + s + ',' + e + '] exceeds page text length ' +
          collapsedMap.length + ' on page ' + pdfPage.pageNumber + ' (offset drift?)');
        e = collapsedMap.length;
      }

      var first = null, last = null;

      for (var q = s; q < e; q++) {
        var mapping = collapsedMap[q];
        if (mapping != null) {
          if (first == null) first = mapping;
          last = mapping;
        }
      }

      if (first == null) {
        console.log('convertPageHits: DROPPED range [' + hitRanges[h][0] + ',' + hitRanges[h][1] +
          '] on page ' + pdfPage.pageNumber + ' (no mappable characters)');
        continue;
      }

      var startPos = itemStart[first.itemNdx] + first.charNdx,
          endPos   = itemStart[last.itemNdx] + last.charNdx + 1;

      starts.push(startPos);
      lens.push(endPos - startPos);
    }

    return { starts: starts, lens: lens };
  });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Store all hit data for the current file and inject it into the find
 * controller's match arrays so pdf.js's native highlighter renders it.
 *
 * @param hitsJson JSON object mapping 1-based page numbers to arrays of
 *                 [startOffset, endOffset] pairs (extraction-space offsets).
 *                 Example: {"1":[[10,20],[50,60]],"3":[[5,15]]}
 */
function setAllHits(hitsJson) {
  if (viewerReady() === false || PDFViewerApplication.pdfDocument == null) {
    console.log('setAllHits DROPPED: viewerReady=' + viewerReady() + ' docOpen='
      + ((typeof PDFViewerApplication !== 'undefined') && (PDFViewerApplication.pdfDocument != null)));
    return;
  }

  pendingHits = JSON.parse(hitsJson);

  // Identity of this call's hit set: the guards below compare against
  // pendingHits by identity (not just null), so async work still in flight
  // from a superseded setAllHits call can neither inject nor abort on behalf
  // of this one.

  var hits = pendingHits;

  var pdfDocument = PDFViewerApplication.pdfDocument,
      fc = PDFViewerApplication.findController,
      eventBus = PDFViewerApplication.eventBus,
      pagesCount = pdfDocument.numPages,
      pageNums = Object.keys(hits);

  convertedMatches = new Array(pagesCount);

  // No find is dispatched: the TextHighlighter is enabled unconditionally
  // whenever a page's text layer renders, and it renders whatever the find
  // controller's match arrays hold as long as the controller's highlight gate
  // (_highlightMatches) is on. Injection sets the gate, fills the arrays, and
  // pokes rendered pages; pages rendered later pull the arrays automatically.
  //
  // Injection must not start until the find controller has been given the
  // current document: openDone (and therefore this call) can arrive BEFORE the
  // viewer's internal document setup, whose findController.setDocument does a
  // reset that clears the gate and replaces the match arrays, wiping anything
  // injected too early. If the controller doesn't have this document yet, wait
  // for 'documentloaded'. The gate is also re-asserted in every page callback,
  // since those complete asynchronously.

  function startInjection() {
    if ((pendingHits !== hits) || (PDFViewerApplication.pdfDocument !== pdfDocument)) {
      console.log('javaapp.startInjection: aborted (hits cleared or superseded, or document changed)');
      return;
    }

    // The text-layer render path reads findController.state.highlightAll (via the
    // public state getter), and state is null unless a real find has run; the null
    // read throws inside rendering, which is what silently killed all highlight
    // painting. Shadow the getter with an own property carrying the one flag the
    // render path needs. (Side effect, accepted: a user-initiated Ctrl+F find in
    // the pane will read this shadow and behave as highlight-all.)

    if (fc.state == null) {
      Object.defineProperty(fc, 'state', {
        value: { query: '', type: '', highlightAll: true, caseSensitive: false, entireWord: false, matchDiacritics: false, findPrevious: false },
        writable: true,
        configurable: true
      });
    }

    fc._highlightMatches = true;

    // Replace the match arrays wholesale rather than merging into whatever a
    // previous query left in them: entries persist per page, so a page outside
    // this query's hit set would otherwise keep the previous query's matches
    // and highlight them whenever its text layer renders later (observed as the
    // old search term lighting up on pages scrolled to after a new search). The
    // all-pages poke then repaints every currently-rendered page from the now
    // empty arrays; per-page pokes follow as this query's conversions land.

    fc._pageMatches = [];
    fc._pageMatchesLength = [];

    eventBus.dispatch('updatetextlayermatches', { source: fc, pageIndex: -1 });

    pageNums.forEach(function (pageNumStr) {
      var pageNum = parseInt(pageNumStr, 10),
          hitRanges = hits[pageNumStr];

      if ((hitRanges == null) || (hitRanges.length === 0)) return;

      pdfDocument.getPage(pageNum).then(function (pdfPage) {
        return convertPageHits(pdfPage, hitRanges);
      }).then(function (converted) {
        if (pendingHits !== hits) return;

        // Matches must be sorted ascending; the highlighter silently drops
        // out-of-order entries.

        var order = converted.starts.map(function (_, ndx) { return ndx; })
                                    .sort(function (a, b) { return converted.starts[a] - converted.starts[b]; });

        var pageNdx = pageNum - 1;
        convertedMatches[pageNdx] = {
          starts: order.map(function (ndx) { return converted.starts[ndx]; }),
          lens:   order.map(function (ndx) { return converted.lens[ndx]; })
        };

        fc._highlightMatches = true;
        fc._pageMatches[pageNdx] = convertedMatches[pageNdx].starts;
        fc._pageMatchesLength[pageNdx] = convertedMatches[pageNdx].lens;

        eventBus.dispatch('updatetextlayermatches', { source: fc, pageIndex: pageNdx });
      });
    });
  }

  // The find controller receives the document asynchronously, later than the
  // eventBus 'documentloaded' event: when this body runs in the gap (document
  // loaded, controller assignment still pending), a 'documentloaded' listener
  // would wait forever because the event already fired. No event marks the
  // controller assignment itself, so poll for it; the same supersession checks
  // that guard injection bound the polling (hits cleared or document changed).

  if (fc._pdfDocument === pdfDocument) {
    startInjection();
  } else {
    var pollForFindController = function () {
      if ((pendingHits !== hits) || (PDFViewerApplication.pdfDocument !== pdfDocument))
        return;  // superseded; a newer hit set (if any) polls on its own

      if (fc._pdfDocument === pdfDocument) {
        startInjection();
        return;
      }

      setTimeout(pollForFindController, 50);
    };

    setTimeout(pollForFindController, 50);
  }
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Clear all stored hit data and remove all highlights.
 */
function clearAllHits() {
  pendingHits = null;
  convertedMatches = null;

  if (viewerReady() === false) return;

  // Empty the injected match arrays, not just the highlight state: findbarclose
  // clears the highlight gate and repaints rendered pages, but the arrays
  // persist, and a later injection re-enabling the gate would resurrect their
  // entries on any page the new query doesn't cover.

  var fc = PDFViewerApplication.findController;

  fc._pageMatches = [];
  fc._pageMatchesLength = [];

  PDFViewerApplication.eventBus.dispatch('findbarclose', { source: window });
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// If the Java side tried to open a file before this script had parsed (see the
// typeof guard in PDFJSWrapper.issueOpen), the arguments were buffered; open now.
// (Function declarations are hoisted, so calling openPdfFile here is safe.)

// A status buffered before parse is drained first (its overlay covers the open
// that may be about to start); if body does not exist yet, showStatusOverlay
// re-buffers and the DOMContentLoaded drain replays it.

drainPendingStatus();

// (Bracket access because the property is created by the Java side, never
// assigned in this file, so the editor's JS type inference does not know it.)

if (window['__hnPendingOpen']) {
  var pendingOpen = window['__hnPendingOpen'];
  delete window['__hnPendingOpen'];
  openPdfFile(pendingOpen[0], pendingOpen[1], pendingOpen[2]);
}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Scroll to the nth match on the given 1-based page. A match that spans
 * multiple text divs renders as several spans (highlight begin/middle/end),
 * so match starts are the highlight spans that are neither middle nor end.
 */
function scrollToMatchOnPage(pageNum, ndxOnPage) {
  if (viewerReady() === false) return;

  PDFViewerApplication.pdfViewer.currentPageNumber = pageNum;

  var attempts = 0;

  function tryScroll() {
    var pageDiv = document.querySelector('.page[data-page-number="' + pageNum + '"]');
    var starts = pageDiv ? pageDiv.querySelectorAll('.textLayer .highlight:not(.middle):not(.end)') : [];

    if (starts.length > ndxOnPage) {
      starts[ndxOnPage].scrollIntoView({ behavior: 'smooth', block: 'center' });
      return;
    }

    if (++attempts < 20)
      window.setTimeout(tryScroll, 100);
    else
      console.log('scrollToMatchOnPage: gave up; page ' + pageNum + ' ndx ' + ndxOnPage + ' starts=' + starts.length);
  }

  tryScroll();
}
