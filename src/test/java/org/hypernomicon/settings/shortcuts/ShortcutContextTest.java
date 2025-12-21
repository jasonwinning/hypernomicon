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

package org.hypernomicon.settings.shortcuts;

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.settings.shortcuts.Shortcut.ShortcutContext.*;

import org.junit.jupiter.api.Test;

import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutContext;

//---------------------------------------------------------------------------

class ShortcutContextTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testOverlapMatrix()
  {
    ShortcutContext[] values =
    {
      MainWindow,
      AllWindows,
      FileManager,
      BibManager,
      RecordDescription,
      RecordDescriptionEditor,
      PreviewWindow,
      ContentsWindow,
      QueriesTab,
      TreeTab,
      PersonsTab,
      InstitutionsTab,
      WorksTab,
      MiscFilesTab,
      DebatesTab,
      PositionsTab,
      ArgumentsTab,
      NotesTab,
      TermsTab
    };

    // Direct-overlap truth table (row-major order).
    // Each row corresponds to "this", each column to "other".
    boolean[][] expected =
    {
      // MainWindow
      { true, true, false, false, true, true, false, false, true, true, true, true, true, true, true, true, true, true, true },
      // AllWindows
      { true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true },
      // FileManager
      { false, true, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false },
      // BibManager
      { false, true, false, true, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false },
      // RecordDescription
      { true, true, true, false, true, true, false, false, true, true, true, false, true, true, true, true, true, true, true },
      // RecordDescriptionEditor
      { true, true, false, false, true, true, false, false, false, false, true, false, true, true, true, true, true, true, true },
      // PreviewWindow
      { false, true, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false, false },
      // ContentsWindow
      { false, true, false, false, false, false, false, true, false, false, false, false, false, false, false, false, false, false, false },
      // QueriesTab
      { true, true, false, false, true, false, false, false, true, false, false, false, false, false, false, false, false, false, false },
      // TreeTab
      { true, true, false, false, true, false, false, false, false, true, false, false, false, false, false, false, false, false, false },
      // PersonsTab
      { true, true, false, false, true, true, false, false, false, false, true, false, false, false, false, false, false, false, false },
      // InstitutionsTab
      { true, true, false, false, false, false, false, false, false, false, false, true, false, false, false, false, false, false, false },
      // WorksTab
      { true, true, false, false, true, true, false, false, false, false, false, false, true, false, false, false, false, false, false },
      // MiscFilesTab
      { true, true, false, false, true, true, false, false, false, false, false, false, false, true, false, false, false, false, false },
      // DebatesTab
      { true, true, false, false, true, true, false, false, false, false, false, false, false, false, true, false, false, false, false },
      // PositionsTab
      { true, true, false, false, true, true, false, false, false, false, false, false, false, false, false, true, false, false, false },
      // ArgumentsTab
      { true, true, false, false, true, true, false, false, false, false, false, false, false, false, false, false, true, false, false },
      // NotesTab
      { true, true, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, true, false },
      // TermsTab
      { true, true, false, false, true, true, false, false, false, false, false, false, false, false, false, false, false, false, true }
    };

    for (int i = 0; i < values.length; i++)
    {
      for (int j = 0; j < values.length; j++)
      {
        final int row = i, col = j;
        boolean exp = expected[row][col];
        assertEquals(exp, values[row].overlaps(values[col]), () -> values[row] + " vs " + values[col] + " expected " + exp);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
