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

package org.hypernomicon.tree;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.Iterator;
import java.util.Set;

import org.controlsfx.control.BreadCrumbBar;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.records.*;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableView;

public class ChooseParentDlgCtrlr extends ModalDialog
{
  @FXML private BreadCrumbBar<TreeRow> bcbPath;
  @FXML private TreeTableView<TreeRow> ttv;
  @FXML private TreeTableColumn<TreeRow, TreeCellValue> tcName;
  @FXML private TreeTableColumn<TreeRow, TreeRow> tcLinked;
  @FXML private TreeTableColumn<TreeRow, String> tcDesc;

  private final TreeWrapper popupTree;
  private static String title;
  private final Set<RecordType> types;
  private final HDT_Record child;
  private HDT_Record parent;

  HDT_Record getParent() { return parent; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ChooseParentDlgCtrlr(HDT_Record child)
  {
    super("tree/ChooseParentDlg", "Record Select", true, true);

    popupTree = new TreeWrapper(bcbPath, false, new ComboBox<>());
    this.child = child;
    parent = null;

    tcName.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue().getNameCell  ()));
    tcDesc.setCellValueFactory(row -> new SimpleStringProperty  (row.getValue().getValue().getDescString()));

    tcLinked.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue()));
    tcLinked.setCellFactory(row -> TreeRow.typeCellFactory());

    popupTree.reset(ttv, true, true);

    TreeWrapper tree = ui.tree();
    tree.debateTree.copyTo(popupTree.debateTree);
    tree.noteTree  .copyTo(popupTree.noteTree  );
    tree.labelTree .copyTo(popupTree.labelTree );

    popupTree.sort();
    popupTree.expandMainBranches();

    ttv.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) -> popupTree.setBreadCrumb(newValue));

    types = tree.getValidTargetTypes(child.getType());

    title = "Select a ";

    Iterator<RecordType> it = types.iterator();

    for (int ctr = 1; it.hasNext(); ctr++)
    {
      RecordType type = it.next();

      title += getTypeName(type);

      if (ctr < types.size())
      {
        if (types.size() == 2)
          title += " or ";
        else if (ctr == (types.size() - 1))
          title += ", or ";
        else
          title += ", ";
      }
    }

    title = title + " record";
    stage.setTitle(title);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    TreeRow selectedRow = nullSwitch(popupTree.selectedItem(), null, TreeItem::getValue);

    if ((selectedRow == null) || (types.contains(selectedRow.getRecordType()) == false))
      return falseWithErrorPopup("You must " + title.toLowerCase() + '.');

    parent = selectedRow.getRecord();

    RecordTreeEdge edge = new RecordTreeEdge(parent, child);

    return edge.canAttach() &&
           ((edge.edgeToDetach() == null) || falseWithErrorPopup("A " + getTypeName(child.getType()).toLowerCase() + " cannot have more than one parent " +
                                                                 getTypeName(parent.getType()).toLowerCase() + " record."));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
