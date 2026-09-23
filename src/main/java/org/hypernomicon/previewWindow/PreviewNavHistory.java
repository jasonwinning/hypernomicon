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

import java.util.*;

import org.hypernomicon.model.records.HDT_RecordWithFilePath;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * One preview pane's navigation history: the files it has shown, in order,
 * each with the pages visited in it. Two cursors move through it: the file
 * cursor (the window's file back/forward buttons) and, within the current
 * file, the page cursor (the page back/forward buttons and their hold menus).
 * <p>
 * The rules, pinned by {@code PreviewNavHistoryTest}:
 * <ul>
 * <li>Tracking a file that is not the current entry's appends an entry and
 *     discards the forward file history. Tracking the current entry's own file
 *     and record again keeps the entry, and with it its page history and the
 *     forward file history: that covers a status display preceding its
 *     document, a re-issued document, and the load a file back/forward step
 *     asks for, which is why stepping needs no separate handoff to the load.</li>
 * <li>A recorded page truncates the current entry's forward page history,
 *     then adjacent duplicate pages collapse into one.</li>
 * <li>File back/forward skips entries with no path and entries for the
 *     current entry's own file (two records sharing one file).</li>
 * <li>Clearing the current entry (the pane shows nothing) leaves the entries
 *     navigable; only a reset empties them.</li>
 * </ul>
 * Pure state with no JavaFX, confined to the FX thread by its owner.
 */
final class PreviewNavHistory
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** One visited file and the pages visited in it. */
  static final class Entry
  {
    private final FilePath filePath;
    private final HDT_RecordWithFilePath record;
    private final List<Integer> pages = new ArrayList<>();
    private int pageNdx = -1;

    private Entry(FilePath filePath, HDT_RecordWithFilePath record)
    {
      this.filePath = filePath;
      this.record = record;
    }

    FilePath filePath()             { return filePath; }
    HDT_RecordWithFilePath record() { return record; }
    boolean hasPages()              { return pages.isEmpty() == false; }
    int pageNdx()                   { return pageNdx; }

    /** The pages visited, oldest first; the page cursor indexes into it. */
    List<Integer> pages()           { return Collections.unmodifiableList(pages); }

    /** The page to reopen this file at: the page the cursor is on, or 1 if none was visited. */
    int currentPage()               { return pageNdx < 0 ? 1 : pages.get(pageNdx); }
  }

//---------------------------------------------------------------------------

  private final List<Entry> entries = new ArrayList<>();
  private int entryNdx = -1;
  private Entry current = null;

//---------------------------------------------------------------------------

  /** The entry the pane is showing, or null when it shows nothing. */
  Entry current()                        { return current; }
  FilePath currentFile()                 { return current == null ? null : current.filePath; }
  HDT_RecordWithFilePath currentRecord() { return current == null ? null : current.record; }

  boolean canStepPage(boolean forward)   { return (current != null) && (forward ? ((current.pageNdx + 1) < current.pages.size()) : (current.pageNdx >= 1)); }
  boolean canStepFile(boolean forward)   { return entryNdxAfterStep(forward) >= 0; }

  /** The pane shows nothing; the entries stay navigable. */
  void clearCurrent()                    { current = null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Makes {@code sourceFile} the current entry and returns it: the current
   * entry itself when it is already that file with that record, otherwise a
   * new entry appended in place of the forward file history.
   */
  Entry track(FilePath sourceFile, HDT_RecordWithFilePath record)
  {
    if ((current != null) && current.filePath.equals(sourceFile) && (current.record == record))
      return current;

    current = new Entry(sourceFile, record);

    entryNdx++;

    while (entries.size() > entryNdx)
      entries.remove(entryNdx);

    entries.add(current);

    return current;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Records a visit to a page of the current file (the user scrolled there, a
   * chrome page jump was issued, or the document's load confirmed on its
   * initial page): the page cursor moves onto it, forward page history is
   * discarded, and adjacent duplicates collapse. No-op with no current entry.
   */
  void recordPage(int page)
  {
    if (current == null) return;

    List<Integer> pages = current.pages;

    current.pageNdx++;

    while (pages.size() > current.pageNdx)
      pages.remove(current.pageNdx);

    pages.add(page);

    Iterator<Integer> it = pages.iterator();
    int ndx = 0, prevPage = -1;

    while (it.hasNext())
    {
      int curPage = it.next();

      if (curPage == prevPage)
      {
        it.remove();

        if (current.pageNdx >= ndx)
          current.pageNdx--;
      }
      else
      {
        ndx++;
        prevPage = curPage;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Moves the page cursor one step and returns the page there; -1 if there is no page in that direction. */
  int stepPage(boolean forward)
  {
    if (canStepPage(forward) == false) return -1;

    current.pageNdx += forward ? 1 : -1;

    return current.pages.get(current.pageNdx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Moves the page cursor to an index in the current entry's page list (a
   *  history-menu pick) and returns the page there. */
  int selectPage(int ndx)
  {
    current.pageNdx = ndx;

    return current.pages.get(ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Moves the file cursor to the nearest usable entry in the direction, makes
   * it current, and returns it; null if there is none. The caller reopens the
   * entry's file at {@link Entry#currentPage()}; the load that follows tracks
   * the same file and record, which keeps the entry.
   */
  Entry stepFile(boolean forward)
  {
    int ndx = entryNdxAfterStep(forward);
    if (ndx < 0) return null;

    entryNdx = ndx;
    current = entries.get(ndx);

    return current;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int entryNdxAfterStep(boolean forward)
  {
    if (forward)
    {
      for (int ndx = entryNdx + 1; ndx < entries.size(); ndx++)
        if (usable(ndx)) return ndx;
    }
    else
    {
      for (int ndx = entryNdx - 1; ndx >= 0; ndx--)
        if (usable(ndx)) return ndx;
    }

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Whether file back/forward may land on an entry: it names a file, and not the current entry's own. */
  private boolean usable(int ndx)
  {
    Entry entry = entries.get(ndx);

    if (FilePath.isEmpty(entry.filePath)) return false;

    if ((current == null) || FilePath.isEmpty(current.filePath)) return true;

    return current.filePath.equals(entry.filePath) == false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Forgets everything. */
  void clear()
  {
    entries.clear();
    entryNdx = -1;
    current = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
