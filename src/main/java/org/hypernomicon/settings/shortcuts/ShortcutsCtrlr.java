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
import javafx.scene.layout.VBox;
import javafx.stage.Window;

//---------------------------------------------------------------------------

public class ShortcutsCtrlr implements SettingsControl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnRevert;
  @FXML private TableView<HyperTableRow> tv;
  @FXML private VBox vbox;

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
      Window owner = ui.windows.getOutermostStage();

      if ((owner != null) && (owner.isShowing() == false))
        owner = null;

      ShortcutHTC cell = (ShortcutHTC)row.getCell(2);
      Shortcut oldShortcut = cell.shortcut;

      Set<KeyCombo> existingCombos = hyperTable.dataRowStream()
        .filter(_row -> _row != row)
        .map(_row -> ((ShortcutHTC)_row.getCell(2)).shortcut.keyCombo)
        .filter(Objects::nonNull)
        .collect(Collectors.toSet());

      ShortcutEditorDlgCtrlr dlg = new ShortcutEditorDlgCtrlr(oldShortcut, existingCombos);

      if (dlg.showModal() == false)
        return;

      row.setCellValue(2, new Shortcut(oldShortcut.context, oldShortcut.action, dlg.getKeyCombo()).toHTC());
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

    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewRecord     , table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewPerson     , table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewInstitution, table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewWork       , table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewMiscFile   , table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewDebate     , table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewPosition   , table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewArgument   , table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewNote       , table);
    addRow(ShortcutContext.MainWindow, ShortcutAction.CreateNewTerm       , table);

    addRow(ShortcutContext.PersonsTab, ShortcutAction.CreateNewInvestigation, table);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addRow(ShortcutContext context, ShortcutAction action, Table<ShortcutContext, ShortcutAction, Shortcut> table)
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

      if (shortcut.keyCombo == null) return;

      shortcuts.put(shortcut.context, shortcut.action, shortcut);
    });

    Shortcut.saveToPrefs(shortcuts);

    app.shortcuts.setValue(shortcuts);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
