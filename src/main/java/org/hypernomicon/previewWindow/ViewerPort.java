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
 * Production implementations adapt {@code PDFJSWrapper} and the alt-display
 * controller; contract tests substitute a scripted fake that records the
 * command stream.
 */
interface ViewerPort
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Metadata delivered with a load confirmation; built out as the protocol
   *  grows (page labels, annotation pages). */
  record ViewerMeta(int pageCount) { }

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

  /** One-shot: scrolls to a match. Paged mode addresses by (page, index
   *  within that page); direct mode addresses by the global match index.
   *  Never part of desired state; forwarded only against a confirmed
   *  generation. */
  void scrollToMatch(long gen, int matchNdx, int pageNum, int ndxOnPage);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
