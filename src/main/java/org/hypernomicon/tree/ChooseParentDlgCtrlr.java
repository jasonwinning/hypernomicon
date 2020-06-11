/*
 * Copyright 2015-2020 Jason Winning
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
import static org.hypernomicon.util.Util.*;

import java.util.Iterator;
import java.util.Set;

import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.model.records.*;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableView;

public class ChooseParentDlgCtrlr extends HyperDlg
{
  @FXML private TreeTableView<TreeRow> ttv;
  @FXML private TextField tfPath;
  @FXML private TreeTableColumn<TreeRow, HyperTreeCellValue> tcName;
  @FXML private TreeTableColumn<TreeRow, String> tcType, tcDesc;

  private TreeWrapper popupTree;
  private static String title;
  private Set<RecordType> types;
  private HDT_Record parent, child;

  HDT_Record getParent() { return parent; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ChooseParentDlgCtrlr build(HDT_Record child)
  {
    return ((ChooseParentDlgCtrlr) createUsingFullPath("tree/ChooseParentDlg", "Record Select", true)).init(child);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ChooseParentDlgCtrlr init(HDT_Record child)
  {
    popupTree = new TreeWrapper(ttv, false, new ComboBox<TreeRow>(), true);
    this.child = child;
    parent = null;

    tcName.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue().getNameCell()));
    tcType.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getTypeString()));
    tcDesc.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getDescString()));

    popupTree.reset();

    TreeWrapper tree = ui.getTree();
    tree.debateTree.copyTo(popupTree.debateTree);
    tree.noteTree  .copyTo(popupTree.noteTree  );
    tree.labelTree .copyTo(popupTree.labelTree );

    popupTree.sort();
    popupTree.expandMainBranches();

    ttv.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        tfPath.setText(TreeTabCtrlr.getTreePath(ttv, newValue));
      else
        tfPath.clear();
    });

    types = tree.getValidTargetTypes(child.getType());

    title = "Select a ";

    Iterator<RecordType> it = types.iterator();

    for (int ctr = 1; it.hasNext(); ctr++)
    {
      RecordType type = it.next();

      title += db.getTypeName(type);

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
    getStage().setTitle(title);

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (types.contains(popupTree.selectedItem().getValue().getRecordType()) == false)
      return falseWithErrorMessage("You must " + title.toLowerCase() + ".");

    parent = popupTree.selectedItem().getValue().getRecord();

    RecordTreeEdge edge = new RecordTreeEdge(parent, child);

    if (edge.canAttach() == false)
      return false;

    return edge.edgeToDetach() == null ?
      true
    :
      falseWithErrorMessage("A " + db.getTypeName(child .getType()).toLowerCase() + " cannot have more than one parent " +
                                   db.getTypeName(parent.getType()).toLowerCase() + " record.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
