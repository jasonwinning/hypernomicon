/*
 * Copyright 2015-2025 Jason Winning
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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.settings.SettingsDlgCtrlr.*;
import static org.hypernomicon.settings.shortcuts.Shortcut.ShortcutContext.*;
import static org.hypernomicon.settings.shortcuts.Shortcut.ShortcutAction.*;

import java.util.*;
import java.util.stream.Collectors;

import org.hypernomicon.settings.shortcuts.Shortcut.*;
import org.hypernomicon.util.EnumBasedTable;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import com.google.common.collect.Table;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TableView;

//---------------------------------------------------------------------------

public class ShortcutsCtrlr implements SettingsControl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnRevert;
  @FXML private TableView<HyperTableRow> tv;

  private HyperTable hyperTable;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(boolean noDB)
  {
    hyperTable = new HyperTable(tv, 0, false, "");

    hyperTable.addLabelCol(hdtNone);  // Context column
    hyperTable.addLabelCol(hdtNone);  // Action column

    hyperTable.addLabelEditCol((row, colNdx) ->
    {
      Shortcut oldShortcut = ((ShortcutHTC) row.getCell(2)).shortcut;

      Set<Shortcut> existingCombos = hyperTable.dataRowStream()
        .filter(_row -> _row != row)
        .map(_row -> ((ShortcutHTC)_row.getCell(2)).shortcut)
        .filter(shortcut -> shortcut.keyCombo() != null)
        .collect(Collectors.toSet());

      ShortcutEditorDlgCtrlr dlg = new ShortcutEditorDlgCtrlr(oldShortcut, existingCombos);

      if (dlg.showModal() == false)
        return;

      row.setCellValue(2, dlg.getShortcutFromUI().toHTC());
    });

    reloadFromPrefs();

    btnRevert.setOnAction(event -> reloadFromPrefs());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void reloadFromPrefs()
  {
    hyperTable.clear();

    Table<ShortcutContext, ShortcutAction, Shortcut> table = Shortcut.loadFromPrefs();

    addRow(table, AllWindows, PreviewRecord);
    addRow(table, MainWindow, ShowMentions);

    addRow(table, MainWindow, CreateNewRecord);
    addRow(table, MainWindow, CreateNewPerson);
    addRow(table, MainWindow, CreateNewInstitution);
    addRow(table, MainWindow, CreateNewWork);
    addRow(table, MainWindow, CreateNewMiscFile);
    addRow(table, MainWindow, CreateNewDebate);
    addRow(table, MainWindow, CreateNewPosition);
    addRow(table, MainWindow, CreateNewArgument);
    addRow(table, MainWindow, CreateNewNote);
    addRow(table, MainWindow, CreateNewTerm);

    addRow(table, PersonsTab, CreateNewInvestigation);

    addRow(table, AllWindows, GoToMainWindow);
    addRow(table, AllWindows, GoToFileManager);
    addRow(table, AllWindows, GoToBibManager);
    addRow(table, AllWindows, GoToPreviewWindow);

    addRow(table, MainWindow, GoToPersonsTab);
    addRow(table, MainWindow, GoToInstitutionsTab);
    addRow(table, MainWindow, GoToWorksTab);
    addRow(table, MainWindow, GoToMiscFilesTab);
    addRow(table, MainWindow, GoToDebatesTab);
    addRow(table, MainWindow, GoToPositionsTab);
    addRow(table, MainWindow, GoToArgumentsTab);
    addRow(table, MainWindow, GoToNotesTab);
    addRow(table, MainWindow, GoToTermsTab);
    addRow(table, MainWindow, GoToQueriesTab);
    addRow(table, MainWindow, GoToTreeTab);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addRow(Table<ShortcutContext, ShortcutAction, Shortcut> table, ShortcutContext context, ShortcutAction action)
  {
    HyperTableRow row = hyperTable.newDataRow();

    row.setCellValue(0, new GenericNonRecordHTC(context.userReadableName, hdtNone));
    row.setCellValue(1, new GenericNonRecordHTC(action .userReadableName, hdtNone));

    Shortcut shortcut = table.get(context, action);

    if (shortcut == null)
      shortcut = new Shortcut(context, action, null);

    row.setCellValue(2, shortcut.toHTC());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void save(boolean noDB)
  {
    Table<ShortcutContext, ShortcutAction, Shortcut> shortcuts = new EnumBasedTable<>(ShortcutContext.class, ShortcutAction.class);

    hyperTable.dataRows().forEach(row ->
    {
      Shortcut shortcut = ((ShortcutHTC) row.getCell(2)).shortcut;

      if (shortcut.keyCombo() == null) return;

      shortcuts.put(shortcut.context(), shortcut.action(), shortcut);
    });

    Shortcut.saveToPrefs(shortcuts);

    app.shortcuts.setValue(shortcuts);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
