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

// Applies FTS hit highlights to the DOM of rendered "direct content" (non-PDF
// preview pages like HTML notes, Markdown output, etc.).
//
// Invoked as a function expression: PDFJSWrapper wraps this body in "(...)(json);"
// so `data` receives a parsed object of the form:
//   { "matches": [ { "ctx": "<context>", "s": <start>, "e": <end> }, ... ] }

function (data) {
  console.log('FTS-DOM: starting applyDirectContentHits');

  var entries = data.matches;
  if (!entries || entries.length === 0) { console.log('FTS-DOM: no entries'); return; }
  console.log('FTS-DOM: ' + entries.length + ' match entries');

  // Inject CSS

  if (!document.getElementById('fts-hl-style')) {
    var style = document.createElement('style');
    style.id = 'fts-hl-style';
    style.textContent = '.fts-highlight { background-color: rgba(255, 165, 0, 0.35); border-radius: 2px; }';
    var target = document.head || document.body || document.documentElement;
    if (!target) { console.log('FTS-DOM: no target for CSS'); return; }
    target.appendChild(style);
  }

  if (!document.body) { console.log('FTS-DOM: no document.body'); return; }

  // Build full text from all text nodes

  var walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, null, false);
  var nodes = [], nodeStarts = [];
  var fullText = '';
  var n;
  while ((n = walker.nextNode()) != null) {
    nodes.push(n);
    nodeStarts.push(fullText.length);
    fullText += n.textContent;
  }
  console.log('FTS-DOM: ' + nodes.length + ' text nodes, fullText length=' + fullText.length);

  if (fullText.length === 0) { console.log('FTS-DOM: empty fullText'); return; }

  // Normalize Unicode to ASCII-ish for matching: NFKC decomposition + manual
  // replacements for characters NFKC doesn't simplify.
  // Remove characters that may differ between Tika extraction and DOM rendering
  // (curly quotes, em/en dashes, replacement chars from encoding errors).

  function normStr(s) {
    return s.normalize('NFKC')
      .replace(/[\u2018\u2019\u201C\u201D\u2014\u2013\uFFFD]/g, '')
      .replace(/\u00A0/g, ' ');
  }

  // Build normalized text for searching and a mapping back to original positions.
  // Process each original character: normalize it, then add to normText with
  // whitespace collapsing. normToOrig[i] maps normText position i to the original
  // fullText position.

  var normText = '', normToOrig = [];
  var inWs = false;

  for (var oi = 0; oi < fullText.length; oi++) {
    var ch = normStr(fullText.charAt(oi));  // may be empty, single char, or multi char
    for (var ci = 0; ci < ch.length; ci++) {
      if (/\s/.test(ch.charAt(ci))) {
        if (!inWs) { normToOrig.push(oi); normText += ' '; inWs = true; }
      } else {
        normToOrig.push(oi); normText += ch.charAt(ci); inWs = false;
      }
    }
  }
  var normTextLower = normText.toLowerCase();
  console.log('FTS-DOM: normText length=' + normText.length);

  // Phase 1: Find positions by searching for context strings

  var allPositions = [];  // [origDomStart, origDomEnd, matchIndex] of the matched word
  var found = 0, notFound = 0, duplicate = 0;

  for (var m = 0; m < entries.length; m++) {
    var ctx = entries[m].ctx.toLowerCase();
    var pos = normTextLower.indexOf(ctx);
    if (pos < 0) {
      notFound++;
      if (notFound <= 3) {
        console.log('FTS-DOM: not found ctx: [' + ctx.substring(0, 50) + ']');
        var codes = '';
        for (var cj = 0; cj < Math.min(ctx.length, 30); cj++) codes += ctx.charCodeAt(cj).toString(16) + ' ';
        console.log('FTS-DOM: not found hex: ' + codes);
        // Search for a shorter substring to see if part of it exists
        var partial = ctx.substring(10, 30);
        var partialPos = normTextLower.indexOf(partial);
        console.log('FTS-DOM: partial [' + partial + '] found at ' + partialPos);
        if (partialPos >= 0) {
          var domSlice = normTextLower.substring(Math.max(0, partialPos - 15), partialPos + 40);
          console.log('FTS-DOM: dom around partial: [' + domSlice + ']');
          var hexSlice = '';
          for (var hi = Math.max(0, partialPos - 15); hi < Math.min(normTextLower.length, partialPos + 5); hi++)
            hexSlice += normTextLower.charCodeAt(hi).toString(16) + ' ';
          console.log('FTS-DOM: dom hex around partial: ' + hexSlice);
        } else {
          // Try searching for just the word 'because' near start of context
          var keyword = ctx.indexOf('because');
          if (keyword >= 0) {
            var before = ctx.substring(Math.max(0, keyword - 5), keyword);
            var bPos = normTextLower.indexOf(before + 'because');
            console.log('FTS-DOM: keyword search [' + before + 'because] at ' + bPos);
            if (bPos >= 0) {
              var bSlice = normTextLower.substring(Math.max(0, bPos - 20), bPos + 30);
              console.log('FTS-DOM: dom around keyword: [' + bSlice + ']');
              var bhex = '';
              for (var bhi = Math.max(0, bPos - 20); bhi < Math.min(normTextLower.length, bPos + 5); bhi++)
                bhex += normTextLower.charCodeAt(bhi).toString(16) + ' ';
              console.log('FTS-DOM: dom hex around keyword: ' + bhex);
            }
          }
        }
      }
      continue;
    }

    // Check uniqueness

    var pos2 = normTextLower.indexOf(ctx, pos + 1);
    if (pos2 >= 0) duplicate++;

    // Map normalized positions back to original positions

    var matchStart = normToOrig[pos + entries[m].s];
    var matchEnd = normToOrig[Math.min(pos + entries[m].e, normToOrig.length - 1)];

    // Check overlap with already-found positions

    var overlap = false;
    for (var h = 0; h < allPositions.length; h++) {
      if (matchStart < allPositions[h][1] && matchEnd > allPositions[h][0]) { overlap = true; break; }
    }
    if (!overlap) { allPositions.push([matchStart, matchEnd, m]); found++; }
  }
  console.log('FTS-DOM: found=' + found + ' notFound=' + notFound + ' duplicate=' + duplicate + ' total positions=' + allPositions.length);

  if (allPositions.length === 0) return;

  // Sort in reverse order for back-to-front DOM mutation

  allPositions.sort(function(a, b) { return b[0] - a[0]; });

  // Phase 2: Apply highlights in reverse order

  var applied = 0;
  for (var r = 0; r < allPositions.length; r++) {
    var start = allPositions[r][0], end = allPositions[r][1], mNdx = allPositions[r][2];
    for (var ni = 0; ni < nodes.length; ni++) {
      var nStart = nodeStarts[ni], nEnd = nStart + nodes[ni].textContent.length;
      if (start < nStart || start >= nEnd) continue;
      var localStart = start - nStart;
      var localEnd = Math.min(nodes[ni].textContent.length, end - nStart);
      if (localEnd <= localStart) continue;
      var textNode = nodes[ni];
      if (localEnd < textNode.textContent.length) textNode.splitText(localEnd);
      if (localStart > 0) textNode = textNode.splitText(localStart);
      var span = document.createElement('span');
      span.className = 'fts-highlight';
      span.setAttribute('data-match-ndx', mNdx);
      textNode.parentNode.replaceChild(span, textNode);
      span.appendChild(textNode);
      applied++;
      break;
    }
  }
  console.log('FTS-DOM: applied ' + applied + ' highlights');

  // Scroll to first highlight

  var first = document.querySelector('.fts-highlight');
  if (first) first.scrollIntoView({ behavior: 'smooth', block: 'center' });
}
