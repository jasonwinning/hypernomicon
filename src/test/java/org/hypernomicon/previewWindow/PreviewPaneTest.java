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

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.previewWindow.DocumentArtifactService.ConverterState;
import org.hypernomicon.previewWindow.PipelineSnapshot.ArtifactStatus;
import org.hypernomicon.previewWindow.PipelineSnapshot.HitsStatus;
import org.hypernomicon.previewWindow.PreviewIntent.ContentKind;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Contract tests for {@link PreviewPane}: every derivation and diffing rule
 * of the reconciliation design, run against a {@link FakeViewer} on a direct
 * executor (all mutation synchronous, no JavaFX). These are the executable
 * boundary contracts; a change to any reconciler invariant, protocol message,
 * or the intent back-edge updates these tests in the same commit.
 */
class PreviewPaneTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final FilePath DOC      = FilePath.of("chapter.docx"),
                                ARTIFACT = FilePath.of("artifact/preview.pdf"),
                                PDF      = FilePath.of("paper.pdf"),
                                HTML     = FilePath.of("notes.html");

  private static final String HITS   = "{\"2\":[[4,11]]}",
                              HITS_B = "{\"5\":[[0,7]]}";

  private final FakeViewer viewer = new FakeViewer();
  private final PreviewPane pane = new PreviewPane(viewer, Runnable::run);

//---------------------------------------------------------------------------

  private static PreviewIntent ftsIntent(FilePath filePath)
  {
    return new PreviewIntent(filePath, ContentKind.PAGED, -1, true);
  }

  private static PipelineSnapshot readySnapshot(FilePath sourceFile, FilePath displayPath, HitsStatus hits)
  {
    return new PipelineSnapshot(sourceFile, new ArtifactStatus.Ready(displayPath), ConverterState.RUNNING, hits);
  }

  private void confirmLoad()
  {
    pane.onDocumentLoaded(viewer.lastShownGen(), new ViewerMeta(10));
  }

  private long countCalls(String method)
  {
    return viewer.methods().stream().filter(method::equals).count();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void noIntentShowsEmpty()
  {
    pane.setIntent(null);

    assertEquals(List.of("showEmpty"), viewer.methods());
  }

//---------------------------------------------------------------------------

  @Test void clearedIntentShowsEmpty()
  {
    pane.setIntent(ftsIntent(PDF));
    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS, 2)));
    viewer.clear();

    pane.setIntent(null);

    assertEquals(List.of("showEmpty"), viewer.methods());
  }

//---------------------------------------------------------------------------

  @Test void queuedConversionShowsStartingConverterWhileConverterIsNotRunning()
  {
    pane.setIntent(ftsIntent(DOC));
    pane.updatePipeline(new PipelineSnapshot(DOC, new ArtifactStatus.Queued(), ConverterState.STOPPED, new HitsStatus.Pending()));

    assertEquals(ProgressVariant.STARTING_CONVERTER, viewer.last("showProgress").variant());
  }

//---------------------------------------------------------------------------

  /** The progress variant is part of the desired view, so a converter-state
   *  change alone issues a display update. */
  @Test void progressVariantChangesToGeneratingOnceConverterRuns()
  {
    pane.setIntent(ftsIntent(DOC));
    pane.updatePipeline(new PipelineSnapshot(DOC, new ArtifactStatus.Converting(), ConverterState.STOPPED, new HitsStatus.Pending()));
    pane.updatePipeline(new PipelineSnapshot(DOC, new ArtifactStatus.Converting(), ConverterState.RUNNING, new HitsStatus.Pending()));

    assertEquals(List.of("showProgress", "showProgress"), viewer.methods());
    assertEquals(ProgressVariant.GENERATING, viewer.last("showProgress").variant());
  }

//---------------------------------------------------------------------------

  @Test void unchangedDesiredViewIssuesNoDuplicateCommands()
  {
    PipelineSnapshot snapshot = new PipelineSnapshot(DOC, new ArtifactStatus.Converting(), ConverterState.RUNNING, new HitsStatus.Pending());

    pane.setIntent(ftsIntent(DOC));
    pane.updatePipeline(snapshot);
    viewer.clear();

    pane.updatePipeline(snapshot);
    pane.updatePipeline(new PipelineSnapshot(DOC, new ArtifactStatus.Converting(), ConverterState.RUNNING, new HitsStatus.Pending()));

    assertEquals(List.of(), viewer.methods());
  }

//---------------------------------------------------------------------------

  @Test void conversionFailureShowsUnable()
  {
    pane.setIntent(ftsIntent(DOC));
    viewer.clear();

    pane.updatePipeline(new PipelineSnapshot(DOC, new ArtifactStatus.Failed("LibreOffice exited"), ConverterState.STOPPED, new HitsStatus.Pending()));

    assertEquals(List.of("showUnable"), viewer.methods());
    assertEquals(DOC, viewer.last("showUnable").filePath());
  }

//---------------------------------------------------------------------------

  /** The FTS withhold rule: with a derived page, the display waits in
   *  Progress until the hit set determines the first-match page. */
  @Test void derivedPageWithheldUntilHitsReadyThenShowsAtFirstMatchPage()
  {
    pane.setIntent(ftsIntent(DOC));
    viewer.clear();

    pane.updatePipeline(readySnapshot(DOC, ARTIFACT, new HitsStatus.Pending()));

    assertEquals(List.of("showProgress"), viewer.methods());
    assertEquals(ProgressVariant.GENERATING, viewer.last("showProgress").variant());

    pane.updatePipeline(readySnapshot(DOC, ARTIFACT, new HitsStatus.ReadyPaged(HITS, 5)));

    assertEquals(List.of("showProgress", "showDocument"), viewer.methods());
    assertEquals(ARTIFACT, viewer.last("showDocument").filePath());
    assertEquals(5, viewer.last("showDocument").pageNum());
  }

//---------------------------------------------------------------------------

  /** Degraded display beats withheld display: a failed hit computation after
   *  a successful conversion shows the document at page 1 with no highlights,
   *  never a permanent Progress. */
  @Test void failedHitsFallBackToPageOneWithoutHighlights()
  {
    pane.setIntent(ftsIntent(DOC));
    viewer.clear();

    pane.updatePipeline(readySnapshot(DOC, ARTIFACT, new HitsStatus.Failed()));

    assertEquals(List.of("showDocument"), viewer.methods());
    assertEquals(1, viewer.last("showDocument").pageNum());

    confirmLoad();

    assertNull(viewer.last("setHits"), "no hits should ever be pushed for a failed hit computation");
  }

//---------------------------------------------------------------------------

  @Test void hitsAreNotPushedBeforeLoadConfirmsAndArePushedOnConfirmation()
  {
    pane.setIntent(ftsIntent(DOC));
    pane.updatePipeline(readySnapshot(DOC, ARTIFACT, new HitsStatus.ReadyPaged(HITS, 2)));

    assertNull(viewer.last("setHits"), "hits must wait for the load confirmation");

    confirmLoad();

    assertEquals(HITS, viewer.last("setHits").hitsJson());
    assertEquals(viewer.lastShownGen(), viewer.last("setHits").gen());
  }

//---------------------------------------------------------------------------

  @Test void explicitPageShowsImmediatelyAndAppliesHitsWhenTheyArrive()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 7, true));
    viewer.clear();

    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.Pending()));

    assertEquals(List.of("showDocument"), viewer.methods());
    assertEquals(7, viewer.last("showDocument").pageNum());

    confirmLoad();
    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS, 3)));

    // Hits ship, but the explicit page is honored: no goToPage to the first-match page

    assertEquals(HITS, viewer.last("setHits").hitsJson());
    assertNull(viewer.last("goToPage"));
  }

//---------------------------------------------------------------------------

  @Test void pageChangeOnSameDocumentIssuesGoToPageOnly()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 7, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));
    confirmLoad();
    viewer.clear();

    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 9, false));

    assertEquals(List.of("goToPage"), viewer.methods());
    assertEquals(9, viewer.last("goToPage").pageNum());
  }

//---------------------------------------------------------------------------

  /** Chrome-driven page navigation: setIntentPage re-sets the intent's page,
   *  producing the same-document goToPage diff. */
  @Test void setIntentPageIssuesGoToPageOnCurrentDocument()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 7, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));
    confirmLoad();
    viewer.clear();

    pane.setIntentPage(9);

    assertEquals(List.of("goToPage"), viewer.methods());
    assertEquals(9, viewer.last("goToPage").pageNum());
  }

//---------------------------------------------------------------------------

  /** A derived page (the FTS first-match rule) becomes explicit when chrome
   *  navigates: the pane jumps to the chosen page and later hit updates no
   *  longer re-derive the page. */
  @Test void setIntentPageMakesDerivedPageExplicit()
  {
    pane.setIntent(ftsIntent(PDF));
    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS, 5)));
    confirmLoad();
    viewer.clear();

    pane.setIntentPage(9);

    assertEquals(9, viewer.last("goToPage").pageNum());

    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS, 5)));

    assertEquals(1, countCalls("goToPage"), "the explicit page must not be re-derived back to the first-match page");
  }

//---------------------------------------------------------------------------

  @Test void setIntentPageWithoutIntentIsNoOp()
  {
    pane.setIntentPage(3);

    assertEquals(List.of(), viewer.methods());
  }

//---------------------------------------------------------------------------

  @Test void hitsChangeOnSameDocumentIssuesSetHitsOnly()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 7, true));
    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS, 3)));
    confirmLoad();
    viewer.clear();

    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS_B, 3)));

    assertEquals(List.of("setHits"), viewer.methods());
    assertEquals(HITS_B, viewer.last("setHits").hitsJson());
  }

//---------------------------------------------------------------------------

  @Test void documentChangeBumpsGenerationAndDropsStaleEvents()
  {
    pane.setIntent(ftsIntent(DOC));
    pane.updatePipeline(readySnapshot(DOC, ARTIFACT, new HitsStatus.ReadyPaged(HITS, 2)));

    long firstGen = viewer.lastShownGen();

    pane.setIntent(ftsIntent(PDF));
    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS_B, 4)));

    long secondGen = viewer.lastShownGen();
    assertEquals(firstGen + 1, secondGen);

    // A late confirmation for the superseded document must not unlock hit delivery

    pane.onDocumentLoaded(firstGen, new ViewerMeta(10));

    assertNull(viewer.last("setHits"), "stale-generation confirmation must be dropped");

    pane.onDocumentLoaded(secondGen, new ViewerMeta(10));

    assertEquals(HITS_B, viewer.last("setHits").hitsJson());
  }

//---------------------------------------------------------------------------

  @Test void pagedToDirectModeSwitchIssuesShowContent()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 1, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));
    confirmLoad();
    viewer.clear();

    pane.setIntent(new PreviewIntent(HTML, ContentKind.DIRECT, -1, false));
    pane.updatePipeline(readySnapshot(HTML, HTML, null));

    // The stale paged snapshot maps to Progress until the new file's pipeline
    // reports; then the mode switch is a full showContent

    assertEquals(List.of("showProgress", "showContent"), viewer.methods());
    assertEquals(HTML, viewer.last("showContent").filePath());
  }

//---------------------------------------------------------------------------

  @Test void directContentAppliesDirectHitsAfterLoad()
  {
    pane.setIntent(new PreviewIntent(HTML, ContentKind.DIRECT, -1, true));
    pane.updatePipeline(readySnapshot(HTML, HTML, new HitsStatus.ReadyDirect(HITS)));

    assertNull(viewer.last("setHits"));

    confirmLoad();

    assertEquals(HITS, viewer.last("setHits").hitsJson());
  }

//---------------------------------------------------------------------------

  /** The intent back-edge: user scrolling updates intent and the issued view,
   *  so reconcile never fights the user with a goToPage to the stale page. */
  @Test void userScrollFoldsBackIntoIntentWithoutCounterCommands()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 7, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));
    confirmLoad();

    long gen = viewer.lastShownGen();
    viewer.clear();

    pane.onPageChanged(gen, 12);

    assertEquals(List.of(), viewer.methods(), "a viewer-originated page change must not be fought");
    assertEquals(12, pane.currentPage());

    // A later unrelated reconcile still issues nothing for the page

    pane.updatePipeline(readySnapshot(PDF, PDF, null));

    assertNull(viewer.last("goToPage"));
  }

//---------------------------------------------------------------------------

  /** The back-edge folds only pages of the intended document: during a document
   *  switch, the outgoing document's page events are still current by generation
   *  (the new document has not issued, so the generation has not moved), and
   *  folding one in would make the old document's page the new intent's explicit
   *  page, skipping the withhold-until-hits derivation and landing the new
   *  document on the previous document's page instead of the first-match page. */
  @Test void stalePageEventFromOutgoingDocumentDoesNotBecomeExplicitIntentPage()
  {
    pane.setIntent(ftsIntent(PDF));
    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS, 2)));
    confirmLoad();

    long gen = viewer.lastShownGen();

    // New intent for a different file; its hits are pending, so the display is
    // withheld and no new document has issued (the generation has not moved)

    pane.setIntent(ftsIntent(DOC));
    pane.updatePipeline(readySnapshot(DOC, ARTIFACT, new HitsStatus.Pending()));
    viewer.clear();

    // A trailing page event from the outgoing document arrives; by generation
    // it is current, but it does not describe the intended document

    pane.onPageChanged(gen, 8);

    pane.updatePipeline(readySnapshot(DOC, ARTIFACT, new HitsStatus.ReadyPaged(HITS_B, 10)));

    assertEquals(ARTIFACT, viewer.last("showDocument").filePath());
    assertEquals(10, viewer.last("showDocument").pageNum(), "the first-match page must win over the outgoing document's page");
  }

//---------------------------------------------------------------------------

  @Test void readBackReflectsConfirmedState()
  {
    assertNull(pane.currentFile());

    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 7, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));

    assertNull(pane.currentFile(), "read-back reflects confirmed state, not issued commands");

    confirmLoad();

    assertEquals(PDF, pane.currentFile());
    assertEquals(7, pane.currentPage());
  }

//---------------------------------------------------------------------------

  @Test void viewerErrorReissuesBoundedThenEscalatesToUnable()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 1, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));

    // Initial attempt plus MAX_VIEWER_RETRIES re-issues, each with a fresh generation

    for (int ndx = 0; ndx < PreviewPane.MAX_VIEWER_RETRIES; ndx++)
      pane.onViewerError(viewer.lastShownGen(), "render failure");

    long shownCount = viewer.methods().stream().filter("showDocument"::equals).count();
    assertEquals(1 + PreviewPane.MAX_VIEWER_RETRIES, shownCount);
    assertNull(viewer.last("showUnable"));

    // The next error exhausts the budget: terminal Unable

    pane.onViewerError(viewer.lastShownGen(), "render failure");

    assertNotNull(viewer.last("showUnable"));
    assertEquals(PDF, viewer.last("showUnable").filePath());

    // Further pipeline updates stay in Unable; no reissue spin

    viewer.clear();
    pane.updatePipeline(readySnapshot(PDF, PDF, null));

    assertEquals(List.of(), viewer.methods());

    // An intent change is the way out of the terminal state

    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 1, false));

    assertEquals(List.of("showDocument"), viewer.methods());
  }

//---------------------------------------------------------------------------

  @Test void staleErrorEventsAreDropped()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 1, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));

    long oldGen = viewer.lastShownGen();

    pane.setIntent(ftsIntent(DOC));
    pane.updatePipeline(readySnapshot(DOC, ARTIFACT, new HitsStatus.ReadyPaged(HITS, 2)));
    viewer.clear();

    pane.onViewerError(oldGen, "late failure from the superseded document");

    assertEquals(List.of(), viewer.methods());
  }

//---------------------------------------------------------------------------

  /** The hybrid escape hatch: yielding to an external writer issues nothing
   *  and leaves the pane cleanly re-enterable. */
  @Test void yieldDisplayIssuesNothingAndNextIntentStartsFresh()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 3, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));
    confirmLoad();
    viewer.clear();

    pane.yieldDisplay();

    assertEquals(List.of(), viewer.methods(), "yielding must not disturb the external writer's display");
    assertNull(pane.currentFile());

    // The next intent re-issues a full load even for the same document,
    // because the pane no longer trusts what the viewer shows

    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 3, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));

    assertTrue(viewer.methods().contains("showDocument"));
  }

//---------------------------------------------------------------------------

  /** User-driven refresh re-issues the full display with a fresh generation;
   *  hits re-ship once the reloaded document confirms. */
  @Test void refreshDisplayReissuesTheFullDisplayWithAFreshGeneration()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 3, true));
    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS, 3)));
    confirmLoad();

    long firstGen = viewer.lastShownGen();
    viewer.clear();

    pane.refreshDisplay();

    assertEquals(List.of("showDocument"), viewer.methods());
    assertEquals(firstGen + 1, viewer.lastShownGen());

    confirmLoad();

    assertEquals(HITS, viewer.last("setHits").hitsJson());
  }

//---------------------------------------------------------------------------

  /** Refresh is the second exit (besides an intent change) from the terminal
   *  viewer-failure display. */
  @Test void refreshDisplayResetsViewerFailureEscalation()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 1, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));

    for (int ndx = 0; ndx <= PreviewPane.MAX_VIEWER_RETRIES; ndx++)
      pane.onViewerError(viewer.lastShownGen(), "render failure");

    assertNotNull(viewer.last("showUnable"));
    viewer.clear();

    pane.refreshDisplay();

    assertEquals(List.of("showDocument"), viewer.methods());
  }

//---------------------------------------------------------------------------

  @Test void scrollToMatchForwardedOnlyAgainstConfirmedLoad()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 1, true));
    pane.updatePipeline(readySnapshot(PDF, PDF, new HitsStatus.ReadyPaged(HITS, 2)));

    pane.scrollToMatch(0, 2, 0);

    assertNull(viewer.last("scrollToMatch"), "one-shots against an unconfirmed load are dropped");

    confirmLoad();
    pane.scrollToMatch(1, 2, 1);

    assertEquals(2, viewer.last("scrollToMatch").pageNum());
    assertEquals(1, viewer.last("scrollToMatch").ndxOnPage());
    assertEquals(viewer.lastShownGen(), viewer.last("scrollToMatch").gen());
  }

//---------------------------------------------------------------------------

  /** The flash fix: setting a new intent together with an already-ready snapshot
   *  goes straight to the document, with no intermediate Progress on the switch. */
  @Test void setIntentAndPipelineSwitchesModeWithoutProgressFlash()
  {
    pane.setIntent(new PreviewIntent(PDF, ContentKind.PAGED, 1, false));
    pane.updatePipeline(readySnapshot(PDF, PDF, null));
    confirmLoad();
    viewer.clear();

    pane.setIntentAndPipeline(
      new PreviewIntent(HTML, ContentKind.DIRECT, -1, false),
      readySnapshot(HTML, HTML, null));

    assertEquals(List.of("showContent"), viewer.methods());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
