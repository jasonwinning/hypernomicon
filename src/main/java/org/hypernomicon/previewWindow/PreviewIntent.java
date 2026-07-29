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
 * What one preview pane should be showing: the single source of truth that
 * initiators (record navigation, FTS row selection, passage clicks, File
 * Manager, dialogs) write and {@link PreviewPane} derives its display from.
 * Immutable; setting a pane's intent is the only sanctioned display-mutation
 * path.
 *
 * @param sourceFile      the file the user asked to preview
 * @param kind            whether the viewer shows it as paged (pdf.js) or direct browser content
 * @param pageNum         1-based explicit page request, or -1 to derive the page
 *                        from the hit set (the FTS first-match-page rule)
 * @param wantsHighlights whether a hit set should be computed and applied for this view
 */
record PreviewIntent(FilePath sourceFile, ContentKind kind, int pageNum, boolean wantsHighlights)
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  enum ContentKind { PAGED, DIRECT }

//---------------------------------------------------------------------------

  /** Whether the page should be derived from the hit set rather than honored as given. */
  boolean derivesPage() { return pageNum < 1; }

  /** Copy with a different page; used by the viewer-scroll intent back-edge. */
  PreviewIntent withPage(int newPageNum)
  {
    return new PreviewIntent(sourceFile, kind, newPageNum, wantsHighlights);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
