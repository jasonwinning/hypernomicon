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

package org.hypernomicon.view.tableCells;

import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.HyperTableColumn.CellClickHandler;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;

import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;

import static org.hypernomicon.App.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class ButtonCell extends TableCell<HyperTableRow, HyperTableCell>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Button btn;
  private final HyperTable ht;
  private final int colNdxOfTarget;
  private final HyperCtrlType ctrlType;
  private final HyperTableColumn col;
  private final CellClickHandler clickHandler;
  private final String caption;

  public static final String URL_BUTTON_TOOLTIP = "Search for website (if not entered) or navigate to website (if entered) in browser";

//---------------------------------------------------------------------------

  public enum ButtonAction { baEdit, baLabelEdit, baNew, baGo, baWeb, baBrowse, baCustom, baNone }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ButtonCell(HyperCtrlType ctrlType, HyperTable newHT, HyperTableColumn col, int colNdxOfTarget, CellClickHandler clickHandler, String caption)
  {
    ht = newHT;
    this.colNdxOfTarget = colNdxOfTarget;
    this.ctrlType = ctrlType;
    this.col = col;
    this.clickHandler = clickHandler;
    this.caption = ctrlType == ctCustomBtn ? safeStr(caption) : ""; // Custom caption is only supported for ctCustomBtn

    btn = HyperTableColumn.makeButton(this);

    switch (ctrlType)
    {
      case ctGoBtn : case ctGoNewBtn : case ctEditNewBtn :

        setAction(ctrlType == ctGoBtn ? ButtonAction.baGo : ButtonAction.baNew); break;

      case ctCustomBtn : setAction(ButtonAction.baCustom   ); break;
      case ctLabelEdit : setAction(ButtonAction.baLabelEdit); break;
      case ctUrlBtn    : setAction(ButtonAction.baWeb      ); break;
      case ctBrowseBtn : setAction(ButtonAction.baBrowse   ); break;
      default          : break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setOnAction(CellClickHandler defHandler)
  {
    btn.setOnAction(event ->
    {
      HyperTableRow row = getTableRow().getItem();

      if (clickHandler != null)
        clickHandler.handle(row, colNdxOfTarget);
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
      {
        btn.setText("...");
        btn.setGraphic(null);
        setOnAction(ht::browseClick);

        break;
      }

      case baEdit:
      {
        btn.setText("");

        ImageView iv = imgViewFromRelPath("resources/images/form-pencil.png");

        iv.setFitWidth(16);
        iv.setFitHeight(16);
        btn.setGraphic(iv);

        setOnAction(null);

        break;
      }

      case baLabelEdit :
      {
        btn.setText("");

        ImageView iv = imgViewFromRelPath("resources/images/pencil.png");

        iv.setFitWidth(16);
        iv.setFitHeight(16);
        btn.setGraphic(iv);

        setOnAction(null);

        break;
      }

      case baGo:
      {
        btn.setText("Go:");
        btn.setGraphic(null);
        setOnAction((row, colNdx) -> ui.goToRecord(row.getRecord(colNdx), true));

        break;
      }

      case baWeb:
      {
        btn.setText("URL:");
        btn.setGraphic(null);
        setToolTip(btn, URL_BUTTON_TOOLTIP);
        setOnAction((row, colNdx) -> openWebLink(row.getText(colNdx)));

        break;
      }

      case baNew:
      {
        btn.setText("New");
        btn.setGraphic(null);
        setOnAction((row, colNdx) -> ui.activeTab().newClick(ht.getTypeByCol(colNdx), row));

        break;
      }

      case baCustom :
      {
        btn.setText(caption);
        btn.setGraphic(null);
        setOnAction(null);            // There is no default recordHandler
        break;
      }

      default: break;
    }

    nullSwitch(col.buttonTooltips.get(newAction), tooltip -> setToolTip(btn, () -> tooltip.apply(getTableRow().getItem())));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateItem(HyperTableCell cell, boolean empty)
  {
    super.updateItem(cell, empty);

    if (empty)
    {
      setText(null);
      setGraphic(null);
      setTooltip(null);
      return;
    }

    btn.setDisable(false);

    if (this.ctrlType == ctLabelEdit)
    {
      String text = HyperTableCell.getCellText(cell);

      AnchorPane ap = new AnchorPane();
      ap.setMinWidth(0.0);
      setGraphic(ap);

      Label label = new Label(text);
      label.prefWidthProperty().bind(widthProperty().subtract(btn.widthProperty()).subtract(6.0));

      AnchorPane.setLeftAnchor(label, 0.0);
      AnchorPane.setRightAnchor(btn, 0.0);

      ap.getChildren().add(btn);
      ap.getChildren().add(label);

      setToolTip(this, text);
    }
    else
      setGraphic(btn);

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
