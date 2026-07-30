/*
 * Copyright 2026 Jason Winning
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

import static org.hypernomicon.util.MediaUtil.*;

import java.util.function.Consumer;

import org.hypernomicon.previewWindow.ConversionSession.ConversionState;
import org.hypernomicon.previewWindow.ConversionSession.NoOfficeInstallationException;
import org.hypernomicon.previewWindow.PipelineSnapshot.ArtifactStatus;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * The artifact side of a preview host, shared by {@link PreviewPaneHost} and
 * {@link DialogPreviewHost}: turns the host's intent file into the
 * {@link ArtifactStatus} its reconciler derives from. For office documents it
 * starts (or joins) the content-keyed conversion and follows the session's
 * display notifications; for natively-viewable files the artifact is the
 * source file itself.
 * <p>
 * All calls and callbacks are on the FX thread (session display callbacks
 * marshal there).
 */
final class ArtifactTracker
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Consumer<ConversionSession> artifactLeaser;
  private final Runnable changeListener;

  private ConversionSession session = null;
  private ConversionSession.Subscription subscription = null;
  private ArtifactStatus status = null;
  private boolean noOfficeFailure = false;

//---------------------------------------------------------------------------

  /**
   * @param artifactLeaser leases a completed conversion's artifact against
   *                       cache eviction for as long as this consumer displays it
   * @param changeListener notified after each status change; the host pushes a
   *                       fresh pipeline snapshot to its reconciler
   */
  ArtifactTracker(Consumer<ConversionSession> artifactLeaser, Runnable changeListener)
  {
    this.artifactLeaser = artifactLeaser;
    this.changeListener = changeListener;
  }

//---------------------------------------------------------------------------

  /** The tracked artifact's status, or {@code null} when nothing is tracked. */
  ArtifactStatus status()  { return status; }

  /** Whether the tracked conversion failed because no office installation is configured. */
  boolean noOfficeInstallation()  { return noOfficeFailure; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Starts tracking the artifact for a newly-intended file, replacing whatever
   * was tracked before; call only when the intent file actually changed.
   *
   * @param filePath    the intended source file
   * @param consumerKey this consumer's display-slot key on the session (see
   *                    {@link ConversionSession#subscribeDisplay})
   */
  void trackNewFile(FilePath filePath, Object consumerKey)
  {
    drop();

    String mimetypeStr = getMediaType(filePath).toString();

    if (OfficePreviewer.isOfficeConvertible(mimetypeStr))
    {
      status = ArtifactStatus.QUEUED;

      ConversionSession newSession = OfficePreviewer.getOrCreateSession(filePath, mimetypeStr);

      session = newSession;
      subscription = newSession.subscribeDisplay(consumerKey,
        (state, convertedPath, failure) -> onArtifactState(newSession, state, convertedPath, failure));

      OfficePreviewer.enqueueForConversion(newSession);
    }
    else
    {
      status = new ArtifactStatus.Ready(filePath);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Unsubscribes and forgets the tracked artifact (intent cleared or host torn down). */
  void drop()
  {
    if (subscription != null)
    {
      subscription.unsubscribe();
      subscription = null;
    }

    session = null;
    status = null;
    noOfficeFailure = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void onArtifactState(ConversionSession notifyingSession, ConversionState state, FilePath convertedPath, Throwable failure)
  {
    if (notifyingSession != session) return;  // superseded subscription

    switch (state)
    {
      case PENDING    -> status = ArtifactStatus.QUEUED;
      case CONVERTING -> status = ArtifactStatus.CONVERTING;

      case COMPLETED  ->
      {
        artifactLeaser.accept(notifyingSession);
        status = new ArtifactStatus.Ready(convertedPath);
      }

      case FAILED     ->
      {
        noOfficeFailure = failure instanceof NoOfficeInstallationException;
        status = new ArtifactStatus.Failed((failure == null) || (failure.getMessage() == null) ? "Conversion failed" : failure.getMessage());
      }

      case CANCELLED  ->
      {
        // Supersession or teardown; a new intent (or cleanup) is imminent
      }
    }

    changeListener.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
