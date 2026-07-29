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

import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * The display a preview pane should be presenting, as a value:
 * {@code PreviewPane.reconcile} computes one from (intent, pipeline state,
 * viewer state) as a total function, then diffs it against the last issued
 * view to decide which viewer commands to send. Every pipeline state maps to
 * exactly one of these, which is what makes stranded UI unrepresentable.
 * <p>
 * The two document shapes carry the content-kind axis: {@code PagedDoc} is
 * pdf.js mode (native and converted PDFs), {@code DirectDoc} is direct
 * browser content (HTML, text, images, media), pageless.
 */
sealed interface DesiredView
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  enum ProgressVariant { STARTING_CONVERTER, GENERATING }

//---------------------------------------------------------------------------

  record Empty() implements DesiredView { }

  record Progress(FilePath sourceFile, ProgressVariant variant) implements DesiredView { }

  record Unable(FilePath sourceFile, String cause) implements DesiredView { }

  /**
   * @param sourceFile  the file the intent asked for (identity for read-back and escalation)
   * @param displayPath the file the viewer actually loads (the source itself, or a converted artifact)
   * @param pageNum     1-based page the viewer should be on
   * @param hitsJson    per-page hit JSON to apply, or {@code null} for no highlights
   */
  record PagedDoc(FilePath sourceFile, FilePath displayPath, int pageNum, String hitsJson) implements DesiredView { }

  /**
   * @param sourceFile  the file the intent asked for
   * @param displayPath the file the viewer loads as direct content
   * @param hitsJson    context-window hit JSON to apply, or {@code null} for no highlights
   */
  record DirectDoc(FilePath sourceFile, FilePath displayPath, String hitsJson) implements DesiredView { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
