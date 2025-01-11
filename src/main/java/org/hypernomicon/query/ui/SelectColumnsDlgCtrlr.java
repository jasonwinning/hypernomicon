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

package org.hypernomicon.query.ui;

import static org.hypernomicon.query.ui.ResultsTable.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.dialogs.HyperDlg;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public class SelectColumnsDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final class TypeCheckBox extends CheckBox
  {
    private TypeCheckBox(String caption) { super(caption); }
    private final List<CheckBox> children = new ArrayList<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class ColumnCheckBox extends CheckBox
  {
    private ColumnCheckBox(String caption) { super(caption); }
    private TypeCheckBox parent;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private CheckBox chkSelectAll, chkSelectNone, chkFirstType, chkFirstField, chkSecondType;
  @FXML private Button btnOk;
  @FXML private AnchorPane innerPane;

//---------------------------------------------------------------------------

  private static boolean noListen = false;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------

  SelectColumnsDlgCtrlr()
  {
    super("query/SelectColumnsDlg", "Select Columns", true, true);

    double typeLeft = chkFirstType.getLayoutX(),
           fieldLeft = chkFirstField.getLayoutX(),
           itemMargin = chkFirstField.getLayoutY() - chkFirstType.getLayoutY(),
           typeMargin = chkSecondType.getLayoutY() - chkFirstField.getLayoutY(),
           posY = chkFirstType.getLayoutY() - typeMargin;

    btnOk.setOnAction(event -> getStage().close());

    innerPane.getChildren().removeAll(chkFirstType, chkFirstField, chkSecondType);

    chkSelectAll.setSelected(true);

    for (AbstractColumnGroup<? extends ColumnGroupItem> group : recordTypeToColumnGroups.values())
    {
      if (group.isEmpty()) continue;

      posY += typeMargin;
      TypeCheckBox chkType = new TypeCheckBox(group.caption);
      group.checkBox = chkType;
      chkType.setLayoutX(typeLeft);
      chkType.setLayoutY(posY);
      chkType.setSelected(false);
      innerPane.getChildren().add(chkType);

//---------------------------------------------------------------------------

      for (ColumnGroupItem item : group)
      {
        if (item.col == null) continue;

        posY += itemMargin;

        addColumnCheckBox(item, chkType, fieldLeft, posY, false);

        if (item.col.countCol != null)
          addColumnCheckBox(item, chkType, fieldLeft, posY, true);
      }

//---------------------------------------------------------------------------

      chkType.selectedProperty().addListener((ob, oldValue, newValue) ->
      {
        if (noListen) return;

        noListen = true;

        if (Boolean.TRUE.equals(newValue))
        {
          chkSelectNone.setSelected(false);
          chkType.children.forEach(chk -> chk.setSelected(true));
        }
        else
        {
          chkSelectAll.setSelected(false);
          chkType.children.forEach(chk -> chk.setSelected(false));
        }

        noListen = false;
      });
    }

//---------------------------------------------------------------------------

    chkSelectAll.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (noListen) return;

      noListen = true;

      if (Boolean.TRUE.equals(newValue))
      {
        chkSelectNone.setSelected(false);

        recordTypeToColumnGroups.values().forEach(grp ->
        {
          if (grp.isEmpty()) return;

          TypeCheckBox tcb = grp.checkBox;
          tcb.setSelected(true);

          tcb.children.forEach(ccb -> ccb.setSelected(true));
        });
      }
      else
        chkSelectAll.setSelected(true);

      noListen = false;
    });

//---------------------------------------------------------------------------

    chkSelectNone.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (noListen) return;

      noListen = true;

      if (Boolean.TRUE.equals(newValue))
      {
        chkSelectAll.setSelected(false);

        recordTypeToColumnGroups.values().forEach(grp ->
        {
          if (grp.isEmpty()) return;

          TypeCheckBox tcb = grp.checkBox;
          tcb.setSelected(false);

          tcb.children.forEach(ccb -> ccb.setSelected(false));
        });
      }
      else
        chkSelectNone.setSelected(true);

      noListen = false;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addColumnCheckBox(ColumnGroupItem item, TypeCheckBox chkType, double fieldLeft, double posY, boolean isCount)
  {
    ColumnCheckBox chkField = new ColumnCheckBox(isCount ? "Count" : item.caption);
    chkType.children.add(chkField);
    chkField.parent = chkType;
    ResultColumn col;

    if (isCount)
    {
      col = item.col.countCol;
      AnchorPane.setRightAnchor(chkField, 3.0);
    }
    else
    {
      col = item.col;
      chkField.setLayoutX(fieldLeft);
    }

    chkField.setLayoutY(posY);
    chkField.selectedProperty().bindBidirectional(col.visibleProperty());

    chkField.setOnAction(event ->
    {
      if (Boolean.TRUE.equals(chkField.isSelected()))
        col.getTableView().scrollToColumn(col);
    });

    if (chkField.isSelected())
      chkType.setSelected(true);

    innerPane.getChildren().add(chkField);

    chkField.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (noListen) return;
      noListen = true;

      TypeCheckBox tcb = chkField.parent;

      if (Boolean.TRUE.equals(newValue))
      {
        chkSelectNone.setSelected(false);
        if (tcb.children.stream().allMatch(CheckBox::isSelected))
          tcb.setSelected(true);
      }
      else
      {
        chkSelectAll.setSelected(false);
        if (tcb.children.stream().noneMatch(CheckBox::isSelected))
          tcb.setSelected(false);
      }

      noListen = false;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
