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

import static org.hypernomicon.util.Util.*;

import java.util.Map;

import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * The typed command surface a {@link PreviewPane} drives: the three alt
 * displays plus the document commands of the viewer protocol. Every document
 * command carries the pane's document generation; the viewer side drops
 * commands whose generation has been superseded, mirroring how the pane drops
 * stale viewer events. Commands carry their full target sub-state, so
 * re-issuing any of them is always safe.
 * <p>
 * <b>Liveness.</b> Every issued document command reaches a terminal report
 * in bounded time: a load confirmation or a viewer error. The adapter owns
 * converting silence into failure (a dispatch the viewer never received, an
 * open that stops reporting progress, a navigation that commits the browser's
 * error page in place of the content), so the pane never waits on a report
 * that is not coming; see {@code OpenCoordinator}.
 * <p>
 * Production implementations adapt {@code PDFJSWrapper} (documents and the
 * in-viewer status overlay alike); contract tests substitute a scripted fake
 * that records the command stream.
 */
interface ViewerPort
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Metadata delivered with a load confirmation: what the window chrome is
   * built on. The page labels ride the confirmation itself (the viewer resolves
   * them before reporting the open), so the document's description arrives in
   * one event under one identity rather than through a second round trip with
   * its own attribution. Direct content is {@link #PAGELESS}: one notional
   * page, no labels. Annotated pages are not part of this channel; they come
   * from a Java-side scan of the file that runs alongside the load.
   */
  record ViewerMeta(int pageCount, Map<String, Integer> labelToPage, Map<Integer, String> pageToLabel)
  {
    static final ViewerMeta PAGELESS = withPageCount(1);

    static ViewerMeta withPageCount(int pageCount) { return new ViewerMeta(pageCount, Map.of(), Map.of()); }

    /** The page a label names, or the label read as a page number when the
     *  document has no labels; -1 if neither. */
    int pageForLabel(String label) { return labelToPage.isEmpty() ? parseInt(label, -1) : labelToPage.getOrDefault(label, -1); }

    /** A page's label, or the page number itself when the document has no
     *  labels; empty if the document has labels but none for this page. */
    String labelForPage(int page) { return pageToLabel.isEmpty() ? String.valueOf(page) : pageToLabel.getOrDefault(page, ""); }
  }

//---------------------------------------------------------------------------

  void showEmpty();

  void showProgress(FilePath sourceFile, ProgressVariant variant);

  void showUnable(FilePath sourceFile);

  /** Loads a document in paged (pdf.js) mode at the given 1-based page,
   *  establishing a new document generation. */
  void showDocument(long gen, FilePath documentPath, int pageNum);

  /** Loads a file as direct browser content, establishing a new document generation. */
  void showContent(long gen, FilePath contentPath);

  void setHits(long gen, String hitsJson);

  void clearHits(long gen);

  void goToPage(long gen, int pageNum);

  /** Scrolls to a match. Paged mode addresses by (page, index within that
   *  page); direct mode addresses by the global match index. Carried in the
   *  intent as a {@link ScrollTarget}; the reconciler forwards it once per
   *  target, after the generation's load is confirmed and its hits have been
   *  issued. */
  void scrollToMatch(long gen, int matchNdx, int pageNum, int ndxOnPage);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
