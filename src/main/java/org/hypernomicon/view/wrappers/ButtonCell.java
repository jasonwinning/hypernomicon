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

package org.hypernomicon.view.wrappers;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableRow;
import javafx.scene.control.Tooltip;
import javafx.scene.image.ImageView;

import static org.hypernomicon.App.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class ButtonCell extends TableCell<HyperTableRow, HyperTableCell>
{
  final Button cellButton;
  final HyperTable ht;
  final int colNdxOfTarget;
  final HyperCtrlType ctrlType;
  final private HyperTableColumn col;
  final private ButtonCellHandler handler;
  final private String btnCaption;

//---------------------------------------------------------------------------

  public static enum ButtonAction { baEdit, baNew, baGo, baLink, baBrowse, baCustom, baNone }

//---------------------------------------------------------------------------

  @FunctionalInterface public static interface ButtonCellHandler { void handle(HyperTableRow row, int colNdx); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ButtonCell(HyperCtrlType ctrlType, HyperTable newHT, HyperTableColumn col, int colNdxOfTarget, ButtonCellHandler handler, String btnCaption)
  {
    ht = newHT;
    this.colNdxOfTarget = colNdxOfTarget;
    this.ctrlType = ctrlType;
    this.col = col;
    this.handler = handler;
    this.btnCaption = safeStr(btnCaption);

    cellButton = HyperTableColumn.makeButton(this);

    switch (ctrlType)
    {
      case ctGoBtn : case ctGoNewBtn : case ctEditNewBtn :

        setAction(ctrlType == ctGoBtn ? ButtonAction.baGo : ButtonAction.baNew); break;

      case ctCustomBtn : setAction(ButtonAction.baCustom); break;
      case ctLinkBtn   : setAction(ButtonAction.baLink  ); break;
      case ctBrowseBtn : setAction(ButtonAction.baBrowse); break;
      default          : break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setOnAction(ButtonCellHandler defHandler)
  {
    cellButton.setOnAction(event ->
    {
      HyperTableRow row = HyperTableRow.class.cast(getTableRow().getItem());

      if (handler != null)
        handler.handle(row, colNdxOfTarget);
      else if (defHandler != null)
        defHandler.handle(row, colNdxOfTarget);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setAction(ButtonAction newAction)
  {
    switch (newAction)
    {
      case baBrowse:

        if (btnCaption.length() > 0)
          cellButton.setText(btnCaption);
        else
          cellButton.setText("...");

        cellButton.setGraphic(null);
        setOnAction(ht::browseClick);

        break;

      case baEdit:

        cellButton.setText("");

        ImageView iv = getImageViewForRelativePath("resources/images/form-pencil.png");
        iv.setFitWidth(16);
        iv.setFitHeight(16);
        cellButton.setGraphic(iv);

        setOnAction(null);

        break;

      case baGo:

        if (btnCaption.length() > 0)
          cellButton.setText(btnCaption);
        else
          cellButton.setText("Go:");

        cellButton.setGraphic(null);

        setOnAction((row, colNdx) -> ui.goToRecord(row.getRecord(colNdx), true));

        break;

      case baLink:

        if (btnCaption.length() > 0)
          cellButton.setText(btnCaption);
        else
          cellButton.setText("Link:");

        cellButton.setGraphic(null);
        setOnAction((row, colNdx) -> openWebLink(row.getText(colNdx)));

        break;

      case baNew:

        if (btnCaption.length() > 0)
          cellButton.setText(btnCaption);
        else
          cellButton.setText("New");

        cellButton.setGraphic(null);

        setOnAction((row, colNdx) -> ui.activeTab().newClick(ht.getTypeByCol(colNdx), row));

        break;

      case baCustom :

        cellButton.setText(btnCaption);
        cellButton.setGraphic(null);
        setOnAction(null);            // There is no default recordHandler
        break;

      default: break;

    }

    cellButton.setTooltip(null);
    if (col.tooltips.containsKey(newAction) == false) return;

    String tooltip = col.tooltips.get(newAction);
    if ((tooltip == null) || (tooltip.length() == 0)) return;

    cellButton.setTooltip(new Tooltip(tooltip));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateItem(HyperTableCell c, boolean empty)
  {
    super.updateItem(c, empty);

    if (empty) return;

    setGraphic(cellButton);
    cellButton.setDisable(false);

    if (colNdxOfTarget < 0) return;

    HyperTableRow row = nullSwitch(getTableRow(), null, TableRow::getItem);
    if (row == null) return;

    if (row.getID(ht.getMainColNdx()) > 0) // That's not a typo, it should be the main column ndx for the table, not the target ndx
    {
      if      (ctrlType == ctGoNewBtn  ) setAction(ButtonAction.baGo);
      else if (ctrlType == ctEditNewBtn) setAction(ButtonAction.baEdit);
    }
    else if (ctrlType != ctBrowseBtn)
    {
      if ((ctrlType != ctGoNewBtn) && (ctrlType != ctEditNewBtn))
        cellButton.setDisable(true);
      else
        setAction(ButtonAction.baNew);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}