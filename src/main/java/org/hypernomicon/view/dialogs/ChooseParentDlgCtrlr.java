/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.view.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.relations.RelationSet.*;

import java.util.EnumSet;
import java.util.Iterator;

import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.tabs.TreeTabCtrlr;
import org.hypernomicon.view.wrappers.HyperTreeCellValue;
import org.hypernomicon.view.wrappers.TreeRow;
import org.hypernomicon.view.wrappers.TreeWrapper;
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
  private EnumSet<HDT_RecordType> types;
  private HDT_Record parent, child;

  public HDT_Record getParent() { return parent; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ChooseParentDlgCtrlr create(HDT_Record child, EnumSet<HDT_RecordType> types)
  {
    ChooseParentDlgCtrlr cpd = HyperDlg.create("ChooseParentDlg.fxml", "Record Select", true);
    cpd.init(child, types);
    return cpd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_Record child, EnumSet<HDT_RecordType> types)
  {
    popupTree = new TreeWrapper(ttv, false, new ComboBox<TreeRow>(), true);
    this.types = types;
    this.child = child;
    parent = null;

    title = "Select a ";

    Iterator<HDT_RecordType> it = types.iterator();

    for (int ctr = 1; it.hasNext(); ctr++)
    {
      HDT_RecordType type = it.next();

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

    tcName.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue().getNameCell()));
    tcType.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getTypeString()));
    tcDesc.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getDescString()));

    popupTree.reset();

    TreeWrapper tree = ui.getTree();
    tree.debateTree.copyTo(popupTree.debateTree);
    tree.noteTree.copyTo(popupTree.noteTree);
    tree.labelTree.copyTo(popupTree.labelTree);

    popupTree.sort();
    popupTree.expandMainBranches();

    ttv.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue != null)
        tfPath.setText(TreeTabCtrlr.getTreePath(ttv, newValue));
      else
        tfPath.clear();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (types.contains(popupTree.selectedItem().getValue().getRecordType()) == false)
      return falseWithErrorMessage("You must " + title.toLowerCase() + ".");

    parent = popupTree.selectedItem().getValue().getRecord();

    if ((parent.getID() == child.getID()) && (parent.getType() == child.getType()))
      return falseWithErrorMessage("A record cannot be its own parent. Please select another record.");

    RelationType relType = getRelation(child.getType(), parent.getType());

    switch (child.getType())
    {
      case hdtWorkLabel : case hdtNote : case hdtPosition : case hdtDebate : case hdtArgument :

        if (db.getObjectList(relType, child, true).contains(parent))
          return falseWithErrorMessage("The record already has that " + db.getTypeName(parent.getType()).toLowerCase() + " as a parent.");

        break;

      case hdtWork : case hdtMiscFile :

        switch (parent.getType())
        {
          case hdtWorkLabel :

            if (db.getObjectList(relType, child, true).contains(parent))
              return falseWithErrorMessage("The record already has that " + db.getTypeName(parent.getType()).toLowerCase() + " as a parent.");

            if (child.getType() == hdtMiscFile)
              if (HDT_MiscFile.class.cast(child).work.isNotNull())
                return falseWithErrorMessage("A file record's labels cannot be changed while it is attached to a work record.");

            break;

          case hdtWork :

            if (db.getObjectList(relType, child, true).size() > 0)
              return falseWithErrorMessage("A " + db.getTypeName(child.getType()).toLowerCase() + " cannot have more than one parent work record.");

            break;

          case hdtArgument : // the parent can only be a work for this case

            if (db.getObjectList(relType, parent, true).contains(child))
              return falseWithErrorMessage("That work is already listed as a source for that argument.");

            break;

          default : break;
        }

      default : break;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
