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

import static org.hypernomicon.model.records.RecordType.*;

import java.util.List;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.previewWindow.PreviewNavHistory.Entry;
import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Contract tests for {@link PreviewNavHistory}: the file and page cursors of
 * a preview pane's back/forward navigation. These pin the semantics the
 * Preview Window's buttons and the wrapper's load path rely on, so the
 * history can move behind the pane host without changing what the buttons
 * do. Records are null except where the rule under test is about them.
 */
class PreviewNavHistoryTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final FilePath A = FilePath.of("a.pdf"),
                                B = FilePath.of("b.pdf"),
                                C = FilePath.of("c.pdf");

  private final PreviewNavHistory history = new PreviewNavHistory();

//---------------------------------------------------------------------------

  @AfterAll static void closeTestDB() { TestHyperDB.closeIfOpen(); }

//---------------------------------------------------------------------------

  private List<Integer> pages() { return history.current().pages(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // File entries

  @Test void trackingAFileMakesItCurrentWithNoPagesYet()
  {
    assertNull(history.current());
    assertNull(history.currentFile());

    Entry entry = history.track(A, null);

    assertSame(entry, history.current());
    assertEquals(A, history.currentFile());
    assertNull(history.currentRecord());
    assertFalse(entry.hasPages());
    assertEquals(1, entry.currentPage(), "an unvisited file reopens at page 1");
    assertFalse(history.canStepFile(false));
    assertFalse(history.canStepFile(true));
  }

//---------------------------------------------------------------------------

  @Test void trackingTheCurrentFileAgainKeepsItsEntry()
  {
    Entry entry = history.track(A, null);
    history.recordPage(4);

    assertSame(entry, history.track(A, null));  // a status display preceding its document, or a re-issued document
    assertEquals(List.of(4), pages());
    assertFalse(history.canStepFile(false), "no second entry was appended");
  }

//---------------------------------------------------------------------------

  @Test void trackingAnotherFileAppendsAnEntryBehindTheCurrentOne()
  {
    history.track(A, null);
    history.track(B, null);

    assertEquals(B, history.currentFile());
    assertTrue(history.canStepFile(false));
    assertFalse(history.canStepFile(true));
  }

//---------------------------------------------------------------------------

  @Test void trackingANewFileDiscardsTheForwardFileHistory()
  {
    history.track(A, null);
    history.track(B, null);
    history.track(C, null);
    history.stepFile(false);  // back to B
    history.stepFile(false);  // back to A

    assertTrue(history.canStepFile(true));

    history.track(C, null);   // a new visit from A, not a step forward

    assertFalse(history.canStepFile(true), "B and the old C are gone");
    assertEquals(A, history.stepFile(false).filePath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Page history within a file

  @Test void recordedPagesStepBackAndForward()
  {
    history.track(A, null);
    history.recordPage(1);
    history.recordPage(5);
    history.recordPage(9);

    assertEquals(List.of(1, 5, 9), pages());
    assertTrue(history.canStepPage(false));
    assertFalse(history.canStepPage(true));

    assertEquals(5, history.stepPage(false));
    assertEquals(1, history.stepPage(false));
    assertEquals(-1, history.stepPage(false), "nothing before the first page");
    assertFalse(history.canStepPage(false));

    assertEquals(5, history.stepPage(true));
    assertEquals(9, history.stepPage(true));
    assertEquals(-1, history.stepPage(true));
  }

//---------------------------------------------------------------------------

  @Test void recordingAPageAfterSteppingBackDiscardsTheForwardPages()
  {
    history.track(A, null);
    history.recordPage(1);
    history.recordPage(5);
    history.recordPage(9);
    history.stepPage(false);
    history.stepPage(false);  // on 1
    history.recordPage(3);

    assertEquals(List.of(1, 3), pages());
    assertEquals(1, history.current().pageNdx());
    assertFalse(history.canStepPage(true));
  }

//---------------------------------------------------------------------------

  @Test void adjacentDuplicatePagesCollapse()
  {
    history.track(A, null);
    history.recordPage(2);
    history.recordPage(2);

    assertEquals(List.of(2), pages());
    assertEquals(0, history.current().pageNdx());
    assertFalse(history.canStepPage(false));

    history.recordPage(7);
    history.stepPage(false);  // on 2
    history.recordPage(2);    // same page again from the cursor: nothing new

    assertEquals(List.of(2), pages(), "forward page discarded, duplicate collapsed");
    assertEquals(0, history.current().pageNdx());
  }

//---------------------------------------------------------------------------

  @Test void aHistoryMenuPickMovesTheCursorToThatPage()
  {
    history.track(A, null);
    history.recordPage(1);
    history.recordPage(5);
    history.recordPage(9);

    assertEquals(1, history.selectPage(0));
    assertEquals(0, history.current().pageNdx());
    assertTrue(history.canStepPage(true));
    assertEquals(5, history.stepPage(true));
  }

//---------------------------------------------------------------------------

  @Test void pageOperationsWithoutACurrentEntryAreNoOps()
  {
    history.recordPage(3);

    assertNull(history.current());
    assertFalse(history.canStepPage(false));
    assertEquals(-1, history.stepPage(true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Stepping between files

  @Test void steppingBackMakesTheEarlierEntryCurrentAtItsLastPage()
  {
    history.track(A, null);
    history.recordPage(1);
    history.recordPage(6);
    history.track(B, null);
    history.recordPage(1);

    Entry entry = history.stepFile(false);

    assertEquals(A, entry.filePath());
    assertSame(entry, history.current());
    assertEquals(6, entry.currentPage(), "reopens where the user left it");
    assertTrue(history.canStepFile(true));
  }

//---------------------------------------------------------------------------

  @Test void theLoadAStepAsksForKeepsTheEntryAndTheForwardFileHistory()
  {
    history.track(A, null);
    history.recordPage(6);
    history.track(B, null);

    Entry target = history.stepFile(false);

    assertSame(target, history.track(A, null), "the load tracks the same file and record");
    assertEquals(List.of(6), pages(), "page history survives the return");
    assertTrue(history.canStepFile(true), "B is still ahead");
    assertEquals(B, history.stepFile(true).filePath());
    assertFalse(history.canStepFile(true));
  }

//---------------------------------------------------------------------------

  @Test void steppingWithNothingInThatDirectionReturnsNull()
  {
    history.track(A, null);

    assertNull(history.stepFile(false));
    assertNull(history.stepFile(true));
    assertEquals(A, history.currentFile());
  }

//---------------------------------------------------------------------------

  @Test void fileStepsSkipEntriesForTheCurrentFile()
  {
    TestHyperDB db = TestHyperDB.instance();
    HDT_Work work1 = db.createNewBlankRecord(hdtWork),
             work2 = db.createNewBlankRecord(hdtWork);

    history.track(A, null);
    history.track(B, work1);
    history.track(B, work2);  // another record, same file: its own entry

    assertSame(work2, history.currentRecord());
    assertEquals(A, history.stepFile(false).filePath(), "the other record's entry for B is skipped");

    db.deleteRecord(work1);
    db.deleteRecord(work2);
  }

//---------------------------------------------------------------------------

  @Test void onlyFileOwningRecordTypesAreTracked()
  {
    TestHyperDB db = TestHyperDB.instance();
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    HDT_Position position = db.createNewBlankRecord(hdtPosition);

    assertSame(work, history.track(A, work).record());
    assertNull(history.track(B, position).record(), "a position cannot own a previewed file");

    db.deleteRecord(work);
    db.deleteRecord(position);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Clearing

  @Test void clearingTheCurrentEntryLeavesTheEntriesNavigable()
  {
    history.track(A, null);
    history.track(B, null);
    history.clearCurrent();

    assertNull(history.current());
    assertNull(history.currentFile());
    assertTrue(history.canStepFile(false));
    assertFalse(history.canStepFile(true));
    assertEquals(A, history.stepFile(false).filePath());
  }

//---------------------------------------------------------------------------

  @Test void resetForgetsEverything()
  {
    history.track(A, null);
    history.recordPage(3);
    history.track(B, null);
    history.clear();

    assertNull(history.current());
    assertFalse(history.canStepFile(false));
    assertFalse(history.canStepFile(true));

    history.track(C, null);

    assertFalse(history.canStepFile(false), "C is the first entry of a fresh history");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
