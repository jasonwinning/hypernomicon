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

import java.util.*;
import java.util.concurrent.Executor;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.previewWindow.PreviewPaneHost.PaneViewer;
import org.hypernomicon.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.previewWindow.PreviewWrapper.PaneEventSink;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.RequestGate;
import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Contract tests for {@link PreviewPaneHost}: the layer between the window's
 * initiators and the {@link PreviewPane} reconciler, run with a held gate, a
 * direct executor, and a recording viewer (no JavaFX, no browser). They pin
 * the rules that sit above the pane's own tests: the early-versus-stale
 * treatment of hits that arrive while an intent is gated, confirmation by
 * document identity, the atomic intent-plus-snapshot issue, and what the
 * chrome's refresh, page navigation, and clear do to the viewer. Files here
 * do not exist on disk, so every one is a natively viewable artifact (no
 * conversion service involved); paged versus direct is chosen explicitly.
 */
class PreviewPaneHostTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Holds every request while {@link #holding}; {@link #release} runs the latest. */
  private static final class HeldGate implements RequestGate
  {
    boolean holding = false;
    private Runnable held = null;

    @Override public void request(Runnable action)
    {
      if (holding)
        held = action;
      else
        action.run();
    }

    @Override public void cancel() { held = null; }

    void release()
    {
      Runnable action = held;
      held = null;

      if (action != null)
        action.run();
    }
  }

//---------------------------------------------------------------------------

  /** Records every command; the tests confirm loads through the sink it is given. */
  private static final class RecordingViewer implements PaneViewer
  {
    final List<String> calls = new ArrayList<>();
    PaneEventSink sink = null;
    boolean directShowable = true;

    @Override public boolean ensureInitialized() { return true; }
    @Override public void setPaneEventSink(PaneEventSink sink) { this.sink = sink; }

    @Override public void clearPreview() { calls.add("clear"); }
    @Override public void paneShowProgress(FilePath sourceFile, HDT_Record record, ProgressVariant variant) { calls.add("progress:" + sourceFile.getNameOnly()); }
    @Override public void paneShowUnable(FilePath sourceFile, HDT_Record record, boolean noOfficeInstallation) { calls.add("unable:" + sourceFile.getNameOnly()); }
    @Override public void paneShowPaged(FilePath sourceFile, FilePath displayPath, int pageNum, HDT_Record record) { calls.add("paged:" + displayPath.getNameOnly() + '@' + pageNum); }

    @Override public boolean paneShowDirect(FilePath sourceFile, FilePath displayPath, HDT_Record record)
    {
      calls.add("direct:" + displayPath.getNameOnly());
      return directShowable;
    }

    @Override public void paneGoToPage(int pageNum) { calls.add("goToPage:" + pageNum); }
    @Override public void setAllHits(String hitsJson) { calls.add("hits:" + hitsJson); }
    @Override public void clearAllHits() { calls.add("clearHits"); }
    @Override public void scrollToHighlight(int matchNdx, int pageNum, int ndxOnPage) { calls.add("scroll:" + pageNum + '/' + ndxOnPage); }
    @Override public void leaseArtifact(ConversionSession session) { }
    @Override public void reloadViewer(Runnable done) { calls.add("reload"); done.run(); }

    /** The viewer confirms the load of the given document. */
    void confirm(FilePath displayPath) { sink.onOpened(displayPath, true, ViewerMeta.withPageCount(10)); }
  }

//---------------------------------------------------------------------------

  /**
   * Runs tasks in FIFO order, synchronously, but never nested: a task
   * submitted while one runs waits its turn, as it would behind
   * {@code Platform.runLater}. A direct executor would instead re-enter the
   * reconciler from inside a viewer command (a load that fails synchronously
   * reports its error from within the command that issued it).
   */
  private static final class TrampolineExecutor implements Executor
  {
    private final Deque<Runnable> queue = new ArrayDeque<>();
    private boolean running = false;

    @Override public void execute(Runnable task)
    {
      queue.add(task);

      if (running) return;

      running = true;

      try
      {
        while (queue.isEmpty() == false)
          queue.poll().run();
      }
      finally
      {
        running = false;
      }
    }
  }

//---------------------------------------------------------------------------

  private static final FilePath A = FilePath.of("a.pdf"),
                                B = FilePath.of("b.pdf");

  private static final String HITS = "{\"2\":[[4,11]]}";

  private final HeldGate gate = new HeldGate();
  private final RecordingViewer viewer = new RecordingViewer();
  private final PreviewPaneHost host = new PreviewPaneHost(PreviewSource.pvsQueriesTab, gate, new TrampolineExecutor(), () -> viewer);

//---------------------------------------------------------------------------

  private List<String> calls() { return viewer.calls; }

  private long count(String call) { return viewer.calls.stream().filter(call::equals).count(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void readyFileIssuesItsDocumentOnceWithNoProgressFlash()
  {
    host.setPreview(A, null, true, 3, false, null);

    assertEquals(List.of("paged:a.pdf@3"), calls());
    assertNull(host.confirmedFile(), "nothing is confirmed until the viewer reports");
  }

//---------------------------------------------------------------------------

  @Test void confirmationIsByDocumentIdentityNotArrivalOrder()
  {
    host.setPreview(A, null, true, 1, false, null);

    viewer.confirm(B);  // a superseded document's late report

    assertNull(host.confirmedFile());

    viewer.confirm(A);

    assertEquals(A, host.confirmedFile());
  }

//---------------------------------------------------------------------------

  @Test void hitsArrivingWhileTheIntentIsGatedAreEarlyNotStale()
  {
    gate.holding = true;

    host.setPreview(A, null, true, -1, true, null);
    host.updateHitsPaged(A, HITS, 2);  // the FTS initiator pushes hits right behind its request

    assertTrue(calls().isEmpty(), "nothing issued while the intent waits in the gate");

    gate.release();

    assertEquals(List.of("paged:a.pdf@2"), calls(), "the stashed hits let the derived page issue at the first match");

    viewer.confirm(A);

    assertEquals(List.of("paged:a.pdf@2", "hits:" + HITS), calls());
  }

//---------------------------------------------------------------------------

  @Test void hitsForAFileThatIsNeitherIntendedNorRequestedAreDropped()
  {
    host.setPreview(A, null, true, 1, true, null);
    host.updateHitsPaged(B, HITS, 2);

    viewer.confirm(A);

    assertEquals(List.of("paged:a.pdf@1"), calls(), "no hits for A were ever supplied");

    host.updateHitsPaged(A, HITS, 2);

    assertEquals(List.of("paged:a.pdf@1", "hits:" + HITS), calls());
  }

//---------------------------------------------------------------------------

  @Test void aRequestForAnotherFileWhileGatedInvalidatesTheStash()
  {
    gate.holding = true;

    host.setPreview(A, null, true, 1, true, null);
    host.updateHitsPaged(A, HITS, 2);
    host.setPreview(B, null, true, 1, false, null);  // latest wins, in the gate and for the stash

    gate.release();
    viewer.confirm(B);

    assertEquals(List.of("paged:b.pdf@1"), calls());
    assertEquals(0, count("hits:" + HITS), "A's hits never apply to B");
  }

//---------------------------------------------------------------------------

  @Test void clearWhileGatedDropsTheRequestAndItsStash()
  {
    gate.holding = true;

    host.setPreview(A, null, true, 1, true, null);
    host.updateHitsPaged(A, HITS, 2);
    host.clear();
    gate.release();

    assertEquals(List.of("clear"), calls(), "the viewer is emptied; the held request never issues");
    assertNull(host.confirmedFile());
  }

//---------------------------------------------------------------------------

  @Test void clearEmptiesTheViewer()
  {
    host.setPreview(A, null, true, 1, false, null);
    viewer.confirm(A);
    host.clear();

    assertEquals(List.of("paged:a.pdf@1", "clear"), calls());
  }

//---------------------------------------------------------------------------

  @Test void pageNavigationOnTheCurrentDocumentIssuesOnlyThePage()
  {
    host.setPreview(A, null, true, 1, false, null);
    viewer.confirm(A);
    host.navigateToPage(4);

    assertEquals(List.of("paged:a.pdf@1", "goToPage:4"), calls());
    assertEquals(1, count("paged:a.pdf@1"), "the document is not re-issued");
  }

//---------------------------------------------------------------------------

  @Test void navigationBeforeAnyIntentIsANoOp()
  {
    host.navigateToPage(4);
    host.refresh();

    assertTrue(calls().isEmpty());
  }

//---------------------------------------------------------------------------

  @Test void refreshReloadsTheBrowserAndReissuesTheDocument()
  {
    host.setPreview(A, null, true, 5, false, null);
    viewer.confirm(A);
    host.refresh();

    assertEquals(List.of("paged:a.pdf@5", "reload", "paged:a.pdf@5"), calls());
    assertEquals(A, host.confirmedFile(), "the read-back keeps the last confirmed file until the re-issued load confirms");
  }

//---------------------------------------------------------------------------

  @Test void unshowableDirectContentEscalatesToUnableAfterBoundedRetries()
  {
    viewer.directShowable = false;

    host.setPreview(A, null, false, 1, false, null);

    assertEquals(1 + PreviewPane.MAX_VIEWER_RETRIES, count("direct:a.pdf"));
    assertEquals("unable:a.pdf", calls().get(calls().size() - 1));
  }

//---------------------------------------------------------------------------

  @Test void aRequestWithNoViewerYetIsDropped()
  {
    PreviewPaneHost windowless = new PreviewPaneHost(PreviewSource.pvsQueriesTab, gate, new TrampolineExecutor(), () -> null);

    windowless.setPreview(A, null, true, 1, false, null);

    assertNull(windowless.confirmedFile());
    assertTrue(calls().isEmpty());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
