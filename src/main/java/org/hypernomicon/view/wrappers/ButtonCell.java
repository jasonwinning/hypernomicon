/*
 * Copyright 2015-2022 Jason Winning
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

import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableRow;
import javafx.scene.image.ImageView;

import static org.hypernomicon.App.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.util.DesktopUtil;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;

//---------------------------------------------------------------------------

public class ButtonCell extends TableCell<HyperTableRow, HyperTableCell>
{
  final Button btn;
  final HyperTable ht;
  final int colNdxOfTarget;
  final HyperCtrlType ctrlType;
  final private HyperTableColumn col;
  final private ButtonCellHandler handler;
  final private String caption;

//---------------------------------------------------------------------------

  public enum ButtonAction { baEdit, baNew, baGo, baWeb, baBrowse, baCustom, baNone }

//---------------------------------------------------------------------------

  @FunctionalInterface public interface ButtonCellHandler { void handle(HyperTableRow row, int colNdx); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ButtonCell(HyperCtrlType ctrlType, HyperTable newHT, HyperTableColumn col, int colNdxOfTarget, ButtonCellHandler handler, String caption)
  {
    ht = newHT;
    this.colNdxOfTarget = colNdxOfTarget;
    this.ctrlType = ctrlType;
    this.col = col;
    this.handler = handler;
    this.caption = safeStr(caption);

    btn = HyperTableColumn.makeButton(this);

    switch (ctrlType)
    {
      case ctGoBtn : case ctGoNewBtn : case ctEditNewBtn :

        setAction(ctrlType == ctGoBtn ? ButtonAction.baGo : ButtonAction.baNew); break;

      case ctCustomBtn : setAction(ButtonAction.baCustom); break;
      case ctUrlBtn    : setAction(ButtonAction.baWeb   ); break;
      case ctBrowseBtn : setAction(ButtonAction.baBrowse); break;
      default          : break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setOnAction(ButtonCellHandler defHandler)
  {
    btn.setOnAction(event ->
    {
      HyperTableRow row = getTableRow().getItem();

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

        btn.setText(caption.length() > 0 ? caption : "...");
        btn.setGraphic(null);
        setOnAction(ht::browseClick);

        break;

      case baEdit:

        btn.setText("");

        ImageView iv = imgViewFromRelPath("resources/images/form-pencil.png");
        iv.setFitWidth(16);
        iv.setFitHeight(16);
        btn.setGraphic(iv);

        setOnAction(null);

        break;

      case baGo:

        btn.setText(caption.length() > 0 ? caption : "Go:");
        btn.setGraphic(null);
        setOnAction((row, colNdx) -> ui.goToRecord(row.getRecord(colNdx), true));

        break;

      case baWeb:

        btn.setText(caption.length() > 0 ? caption : "URL:");
        btn.setGraphic(null);
        setOnAction((row, colNdx) -> DesktopUtil.openWebLink(row.getText(colNdx)));

        break;

      case baNew:

        btn.setText(caption.length() > 0 ? caption : "New");
        btn.setGraphic(null);
        setOnAction((row, colNdx) -> ui.activeTab().newClick(ht.getTypeByCol(colNdx), row));

        break;

      case baCustom :

        btn.setText(caption);
        btn.setGraphic(null);
        setOnAction(null);            // There is no default recordHandler
        break;

      default: break;
    }

    setToolTip(btn, col.tooltips.get(newAction));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateItem(HyperTableCell c, boolean empty)
  {
    super.updateItem(c, empty);

    if (empty)
    {
      setText(null);
      setGraphic(null);
      setTooltip(null);
      return;
    }

    setGraphic(btn);
    btn.setDisable(false);

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
        btn.setDisable(true);
      else
        setAction(ButtonAction.baNew);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
