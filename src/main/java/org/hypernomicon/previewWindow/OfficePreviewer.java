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
 * Thin adapter over {@link DocumentArtifactService}: mimetype dispatch for
 * convertible office formats, plus session creation, enqueueing, and shutdown
 * forwarding. UI feedback happens entirely through the callers' display
 * subscriptions (the preview pane and dialog hosts).
 */
final class OfficePreviewer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private OfficePreviewer() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether the mimetype is an office format JodConverter converts for
   * preview (to PDF for documents/presentations, to HTML for spreadsheets).
   */
  static boolean isOfficeConvertible(String mimetypeStr)
  {
    return mimetypeStr.contains("openxmlformats-officedocument") ||  // docx (Microsoft Word XML), xlsx, pptx
           "application/msword".equalsIgnoreCase(mimetypeStr)    ||  // doc  (Microsoft Word)
           "application/rtf".equalsIgnoreCase(mimetypeStr)       ||  // rtf  (Rich Text format)
           mimetypeStr.contains("opendocument.text")             ||  // odt  (OpenDocument text), ott (OpenDocument text template)
           mimetypeStr.contains("sun.xml.writer")                ||  // sxw  (OpenOffice.org 1.0 text)
           mimetypeStr.contains("ms-powerpoint")                 ||  // ppt  (Microsoft PowerPoint)
           mimetypeStr.contains("opendocument.presentation")     ||  // odp  (OpenDocument presentation), otp (OpenDocument presentation template)
           mimetypeStr.contains("sun.xml.impress")               ||  // sxi  (OpenOffice.org 1.0 presentation)
           mimetypeStr.contains("vnd.wordperfect")               ||  // wpd  (WordPerfect)
           mimetypeStr.contains("ms-excel")                      ||  // xls  (Microsoft Excel)
           "text/csv".equalsIgnoreCase(mimetypeStr)              ||  // csv  (Comma-separated values)
           mimetypeStr.contains("tab-separated-values")          ||  // tsv  (Tab-separated values)
           mimetypeStr.contains("opendocument.spreadsheet")      ||  // ods  (OpenDocument spreadsheet), ots (OpenDocument spreadsheet template)
           mimetypeStr.contains("sun.xml.calc");                     // sxc  (OpenOffice.org 1.0 spreadsheet)
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Find or create the {@link ConversionSession} for the given document.
   * Sessions are content-keyed, so every pane and dialog requesting the same
   * (unmodified) document shares one conversion and one cached artifact.
   */
  static ConversionSession getOrCreateSession(FilePath filePath, String mimetypeStr)
  {
    return DocumentArtifactService.getOrCreateSession(filePath, mimetypeStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Queue a session for conversion. All UI feedback happens through the
   * callers' display subscriptions; this method has no UI side effects.
   */
  static void enqueueForConversion(ConversionSession session)
  {
    DocumentArtifactService.enqueue(session);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void cleanup()
  {
    DocumentArtifactService.shutdown();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
