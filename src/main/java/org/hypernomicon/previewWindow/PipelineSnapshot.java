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

import org.hypernomicon.previewWindow.DocumentArtifactService.ConverterState;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * An immutable snapshot of the pipeline state a preview pane derives its
 * display from: the artifact side (is the file, or its converted form, ready
 * to load?) and the hit-set side (are highlights computed?). Adapters observe
 * the services and hand {@link PreviewPane} a fresh snapshot on every change;
 * the reconciler never talks to the services directly, which is what makes it
 * a synchronous function testable without them.
 *
 * @param sourceFile     the file this snapshot describes; the reconciler treats a
 *                       snapshot for a different file than the current intent's as
 *                       absent (the snapshot analogue of dropping stale-generation
 *                       viewer events), so an intent change can never combine the
 *                       new source with a leftover artifact
 * @param artifact       status of the displayable file; for natively-viewable
 *                       files the adapter passes {@code Ready(sourceFile)} directly
 * @param converterState the converter lifecycle, for deriving the starting-vs-generating
 *                       progress variant (converter state is not request state)
 * @param hits           status of the hit-set computation; only consulted when the
 *                       intent wants highlights
 */
record PipelineSnapshot(FilePath sourceFile, ArtifactStatus artifact, ConverterState converterState, HitsStatus hits)
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  sealed interface ArtifactStatus
  {
    record Queued() implements ArtifactStatus { }

    record Converting() implements ArtifactStatus { }

    /** @param displayPath the file the viewer should load (source file or converted artifact) */
    record Ready(FilePath displayPath) implements ArtifactStatus { }

    record Failed(String cause) implements ArtifactStatus { }
  }

//---------------------------------------------------------------------------

  sealed interface HitsStatus
  {
    record Pending() implements HitsStatus { }

    /** @param hitsJson     per-page hit JSON, or {@code null} when the computation found no hits
     *  @param firstMatchPage 1-based page the view should open to, or -1 when the caller decides */
    record ReadyPaged(String hitsJson, int firstMatchPage) implements HitsStatus { }

    /** @param hitsJson context-window hit JSON, or {@code null} when the computation found no hits */
    record ReadyDirect(String hitsJson) implements HitsStatus { }

    record Failed() implements HitsStatus { }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
