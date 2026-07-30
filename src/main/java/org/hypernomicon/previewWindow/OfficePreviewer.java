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

import java.io.IOException;

import org.hypernomicon.previewWindow.ConversionSession.NoOfficeInstallationException;
import org.hypernomicon.previewWindow.DocumentArtifactService.ConverterState;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * UI adapter over {@link DocumentArtifactService}: builds the display callbacks
 * that translate conversion-session state changes into wrapper/viewer UI
 * (progress alt-displays, loading the finished artifact, failure indicators),
 * and forwards session creation, enqueueing, and shutdown to the service.
 */
final class OfficePreviewer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private OfficePreviewer() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

  /** Whether the next conversion is the converter process's first (drives the
   *  "starting converter" vs "generating preview" progress message). */
  static boolean getFirstConversion() { return DocumentArtifactService.converterState() != ConverterState.RUNNING; }

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

  /**
   * Builds the DisplayCallback used for previewing an office document.
   * <ul>
   *   <li>PENDING/CONVERTING: shows the progress alt-display; the
   *       starting-converter variant is chosen when the converter process is
   *       not yet running ({@link DocumentArtifactService#converterState()}).</li>
   *   <li>COMPLETED: loads the PDF (or HTML for spreadsheets) into the viewer.</li>
   *   <li>FAILED: shows the unable-to-preview indicator, or the no-office
   *       message for {@link NoOfficeInstallationException}.</li>
   *   <li>CANCELLED: does nothing; cancellation means either supersession by a
   *       newer request or the user navigated away, in which case the wrapper's
   *       UI has already moved on.</li>
   * </ul>
   */
  static ConversionSession.DisplayCallback displayCallbackForPreview(ConversionSession session, FilePath filePath, PreviewWrapper previewWrapper, boolean convertToHtml, int pageNum)
  {
    return (state, convertedPath, failure) ->
    {
      switch (state)
      {
        case PENDING, CONVERTING ->
        {
          if (DocumentArtifactService.converterState() != ConverterState.RUNNING)
            previewWrapper.setStartingConverter();
          else
          {
            // dontRestartProgressIfSamePreview once the conversion is actually
            // under way, so the CONVERTING notification doesn't restart the
            // progress animation the PENDING notification started.

            previewWrapper.setGenerating(filePath, state == ConversionSession.ConversionState.CONVERTING);
          }
        }

        case COMPLETED ->
        {
          // Lease the artifact for as long as this consumer displays it, so cache
          // eviction cannot delete the file out from under the viewer. The holder
          // releases its previous lease, if any.

          previewWrapper.leaseArtifact(session);

          if (convertToHtml)
          {
            try
            {
              previewWrapper.loadConvertedHtml(convertedPath);
            }
            catch (IOException e)
            {
              previewWrapper.setUnable(filePath);
            }
          }
          else
          {
            previewWrapper.loadConvertedPdfBytes(convertedPath, pageNum);
          }
        }

        case FAILED ->
        {
          if (failure instanceof NoOfficeInstallationException)
            previewWrapper.setNoOfficeInstallation();
          else
            previewWrapper.setUnable(filePath);
        }

        default -> { /* CANCELLED: no action */ }
      }
    };
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
