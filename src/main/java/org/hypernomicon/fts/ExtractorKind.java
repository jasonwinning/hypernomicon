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

package org.hypernomicon.fts;

import org.apache.commons.io.FilenameUtils;

//---------------------------------------------------------------------------

/**
 * Which text extractor handled a file. Recorded with each metadata entry so that
 * an upgrade of one extractor marks only the entries that extractor produced as
 * due for re-extraction.
 * <p>
 * The indexer picks the extractor from the file's detected media type (content
 * magic, with the filename only as a hint), so the extension does not reliably
 * say which one ran; that is why the kind is recorded instead of derived.
 */
enum ExtractorKind
{
  /** pdf.js running in an off-screen browser; handles every file detected as a PDF. */
  PDFJS("pdfjs"),

  /** Apache Tika; handles everything else. */
  TIKA("tika");

//---------------------------------------------------------------------------

  private final String jsonName;

  ExtractorKind(String jsonName) { this.jsonName = jsonName; }

  String jsonName()              { return jsonName; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Returns the kind with the given name in the metadata snapshot, or {@code null} if there is none. */
  static ExtractorKind fromJsonName(String jsonName)
  {
    for (ExtractorKind kind : values())
      if (kind.jsonName.equals(jsonName))
        return kind;

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Best guess for an entry from a snapshot written before the kind was recorded.
   * It is wrong only for a file whose extension misstates its content; such an
   * entry is corrected the next time the file is extracted.
   */
  static ExtractorKind inferFromPath(String relPath)
  {
    return "pdf".equalsIgnoreCase(FilenameUtils.getExtension(relPath)) ? PDFJS : TIKA;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
