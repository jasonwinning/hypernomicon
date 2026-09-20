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
// Written as a parenthesized function expression, so the file parses on its own
// and PDFJSWrapper invokes it by appending "(json);". `data` receives a parsed
// object of the form:
//   { "matches": [ { "ctx": "<context>", "s": <start>, "e": <end> }, ... ] }

(function (data) {
  const entries = data.matches;
  if ((entries == null) || (entries.length === 0)) return;

  // Inject CSS. The color matches pdf.js's stock match highlight
  // (viewer.css --highlight-bg-color), so hits look the same in every
  // preview content kind.

  if (document.getElementById('fts-hl-style') == null) {
    const style = document.createElement('style');
    style.id = 'fts-hl-style';
    style.textContent = '.fts-highlight { background-color: rgba(180, 0, 170, 0.25); border-radius: 2px; }';
    const target = document.head || document.body || document.documentElement;
    if (target == null) return;
    target.appendChild(style);
  }

  if (document.body == null) return;

  // Build full text from all text nodes

  const walker = document.createTreeWalker(document.body, NodeFilter.SHOW_TEXT, null);
  const nodes = [], nodeStarts = [];
  let fullText = '';
  let n;
  while ((n = walker.nextNode()) != null) {
    nodes.push(n);
    nodeStarts.push(fullText.length);
    fullText += n.textContent;
  }

  if (fullText.length === 0) return;

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

  const normToOrig = [];
  let normText = '', inWs = false;

  for (let oi = 0; oi < fullText.length; oi++) {
    const ch = normStr(fullText.charAt(oi));  // may be empty, single char, or multi char
    for (let ci = 0; ci < ch.length; ci++) {
      if (/\s/.test(ch.charAt(ci))) {
        if (inWs === false) { normToOrig.push(oi); normText += ' '; inWs = true; }
      } else {
        normToOrig.push(oi); normText += ch.charAt(ci); inWs = false;
      }
    }
  }
  const normTextLower = normText.toLowerCase();

  // Phase 1: Find positions by searching for context strings

  const allPositions = [];  // [origDomStart, origDomEnd, matchIndex] of the matched word

  // Anomaly counts: a context string absent from the page text (the indexed text and
  // the rendered text disagree), or present more than once (placement is ambiguous)

  let notFound = 0, duplicate = 0;

  for (let m = 0; m < entries.length; m++) {
    const ctx = entries[m].ctx.toLowerCase();
    const pos = normTextLower.indexOf(ctx);
    if (pos < 0) { notFound++; continue; }
    if (normTextLower.indexOf(ctx, pos + 1) >= 0) duplicate++;

    // Map normalized positions back to original positions

    const matchStart = normToOrig[pos + entries[m].s];
    const matchEnd = normToOrig[Math.min(pos + entries[m].e, normToOrig.length - 1)];

    // Check overlap with already-found positions

    let overlap = false;
    for (let h = 0; h < allPositions.length; h++) {
      if (matchStart < allPositions[h][1] && matchEnd > allPositions[h][0]) { overlap = true; break; }
    }
    if (overlap === false) allPositions.push([matchStart, matchEnd, m]);
  }

  // Anomalies only; a clean run stays silent. Reaches the application log through
  // the console-message handler, which is registered only when debugging is on.

  if ((notFound > 0) || (duplicate > 0))
    console.log('FTS-DOM: ' + entries.length + ' match entries; ' + notFound + ' not found in page text, '
      + duplicate + ' ambiguous (context occurs more than once), ' + allPositions.length + ' placed');

  if (allPositions.length === 0) return;

  // Sort in reverse order for back-to-front DOM mutation

  allPositions.sort(function(a, b) { return b[0] - a[0]; });

  // Phase 2: Apply highlights in reverse order

  let applied = 0;

  for (let r = 0; r < allPositions.length; r++) {
    const start = allPositions[r][0], end = allPositions[r][1], mNdx = allPositions[r][2];
    for (let ni = 0; ni < nodes.length; ni++) {
      const nStart = nodeStarts[ni], nEnd = nStart + nodes[ni].textContent.length;
      if (start < nStart || start >= nEnd) continue;
      const localStart = start - nStart;
      const localEnd = Math.min(nodes[ni].textContent.length, end - nStart);
      if (localEnd <= localStart) continue;
      let textNode = nodes[ni];
      if (localEnd < textNode.textContent.length) textNode.splitText(localEnd);
      if (localStart > 0) textNode = textNode.splitText(localStart);
      const span = document.createElement('span');
      span.className = 'fts-highlight';
      span.setAttribute('data-match-ndx', mNdx);
      textNode.parentNode.replaceChild(span, textNode);
      span.appendChild(textNode);
      applied++;
      break;
    }
  }

  if (applied < allPositions.length)
    console.log('FTS-DOM: ' + (allPositions.length - applied) + ' of ' + allPositions.length + ' placed highlights matched no text node');

  // Scroll to first highlight

  const first = document.querySelector('.fts-highlight');
  if (first) first.scrollIntoView({ behavior: 'smooth', block: 'center' });
})
