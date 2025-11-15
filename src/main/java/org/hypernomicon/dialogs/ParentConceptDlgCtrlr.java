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

package org.hypernomicon.dialogs;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.view.populators.Populator.DisplayKind;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import javafx.application.Platform;
import javafx.collections.ListChangeListener;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TableView;

//---------------------------------------------------------------------------

public class ParentConceptDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnMoveUp, btnMoveDown, btnRemove;
  @FXML private TableView<HyperTableRow> tv;

  private final HyperTable hyperTable;
  private final HDT_Concept childConcept;

//---------------------------------------------------------------------------

  public ParentConceptDlgCtrlr(HDT_Concept childConcept)
  {
    super("ParentConceptDlg", "Select Parent Concept(s)", true);

    this.childConcept = childConcept;

    HDT_Term curTerm = childConcept.term.get();

    hyperTable = new HyperTable(tv, 0, true, "");

    SubjectPopulator pop = new SubjectPopulator(rtGlossaryOfConcept, false, id -> // Populator ID filter
    {
      if ((id < 1) || HDT_Record.isEmpty(curTerm, false)) return false;

      return curTerm.concepts.contains(db.concepts.getByID(id)) == false;
    }, DisplayKind.listName);

    hyperTable.addColAltPopulatorWithUpdateHandler(hdtConcept, ctEditableLimitedDropDown, pop, (row, cellVal, nextColNdx, nextPop) ->
    {
      if (cellVal.isEmpty() == false)
        hyperTable.edit(hyperTable.getRow(hyperTable.dataRowCount()), 0);
    });

    btnMoveUp.setOnAction(event ->
    {
      int selNdx = tv.getSelectionModel().getSelectedIndex();
      hyperTable.swapRows(selNdx, selNdx - 1);
    });

    btnMoveDown.setOnAction(event ->
    {
      int selNdx = tv.getSelectionModel().getSelectedIndex();
      hyperTable.swapRows(selNdx, selNdx + 1);
    });

    btnRemove.setOnAction(event -> hyperTable.removeRow(tv.getSelectionModel().getSelectedItem()));

    setToolTip(btnMoveUp  , "Move selected parent concept up"  );
    setToolTip(btnMoveDown, "Move selected parent concept down");
    setToolTip(btnRemove  , "Remove selected parent concept"   );

    tv.getSelectionModel().selectedItemProperty().addListener((obs, ov, nv) -> updateButtons(nv));
    tv.getItems().addListener((ListChangeListener<HyperTableRow>) change -> updateButtons(tv.getSelectionModel().getSelectedItem()));

    hyperTable.clear();

    pop.setObj(childConcept.glossary.get());

    pop.populate(false);

    hyperTable.buildRows(childConcept.parentConcepts, (row, parentConcept) -> row.setCellValue(0, parentConcept));

    Platform.runLater(() -> updateButtons(tv.getSelectionModel().getSelectedItem()));

    hyperTable.edit(hyperTable.getRow(hyperTable.dataRowCount()), 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateButtons(HyperTableRow selectedRow)
  {
    if ((selectedRow != null) && (selectedRow.getRecord() != null))
    {
      int selNdx = tv.getSelectionModel().getSelectedIndex();

      btnMoveUp.setDisable(tv.getSelectionModel().getSelectedIndex() == 0);

      btnMoveDown.setDisable((tv.getItems().size() <= (selNdx + 1)) || (tv.getItems().get(selNdx + 1).getRecord() == null));

      btnRemove.setDisable(false);

      return;
    }

    disableAll(btnMoveUp, btnMoveDown, btnRemove);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    try
    {
      if (childConcept.setParentConcepts(hyperTable.saveToList(0, hdtConcept)) == false)
        return falseWithErrorPopup("Unable to assign parent concept(s): " + getThrowableMessage(new HDB_InternalError(85342)));
    }
    catch (RelationCycleException e)
    {
      return falseWithErrorPopup("Unable to add parent concept \"" + ((HDT_Concept) e.parent).extendedName(false) + "\" : A cycle would result.");
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
