/*
 * Copyright 2015-2026 Jason Winning
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

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.records.*;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.tableCells.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TableView;
import javafx.scene.image.ImageView;

//---------------------------------------------------------------------------

public class MultiLinkActionDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Label lblInfo;
  @FXML private TableView<HyperTableRow> tvRecords;

  //---------------------------------------------------------------------------

  public MultiLinkActionDlgCtrlr(Iterable<HDT_Record> records, String searchKeyText)
  {
    super("MultiLinkActionDlg", "Multiple Matching Records", true);

    lblInfo.setText("Multiple matching records assigned to search key \"" + searchKeyText + "\". Select record/action.");

    HyperTable htRecords = new HyperTable(tvRecords, 1, false, "");

    htRecords.addIconCol();
    htRecords.addLabelCol(hdtNone);
    htRecords.addCustomActionCol(1, "Go", (row, colNdx) -> openRecord(row.getRecord(1)))
      .setButtonTooltip(ButtonAction.baCustom, "Go to this record");

    htRecords.addCustomActionCol(1, (row, btn) ->
    {
      HDT_Record record = row.getRecord(1);
      if (record == null)
      {
        btn.setVisible(false);
        return;
      }

      if (record.getType() == hdtWork)
        btn.setVisible(((HDT_Work) record).canLaunch());
      else if ((record.getType() != hdtPerson) && (record instanceof HDT_RecordWithPath recordWithPath))
        btn.setVisible(recordWithPath.pathNotEmpty());
      else
      {
        btn.setVisible(false);
        return;
      }

      ImageView imageView = imgViewFromRelPath("resources/images/rocket-fly.png");
      imageView.setFitHeight(16);
      imageView.setPreserveRatio(true);
      btn.setGraphic(imageView);
    },
    (row, colNdx) ->
    {
      okClicked = true;
      stage.close();
      Platform.runLater(() -> MainTextUtil.launchRecordLinkAction(row.getRecord(1)));

    }).setButtonTooltip(ButtonAction.baCustom, "Launch this file in system viewer");

    htRecords.addCustomActionCol(1, (row, btn) ->
    {
      HDT_Record record = row.getRecord(1);
      if (record == null)
      {
        btn.setVisible(false);
        return;
      }

      if (record.getType() == hdtWork)
        btn.setVisible(((HDT_Work) record).canPreview());
      else if ((record.getType() == hdtMiscFile) || (record.getType() == hdtWorkFile))
        btn.setVisible(((HDT_RecordWithPath) record).pathNotEmpty());
      else
      {
        btn.setVisible(false);
        return;
      }

      btn.setText("Preview");
    },
    (row, colNdx) ->
    {
      okClicked = true;
      stage.close();
      Platform.runLater(() -> MainTextUtil.previewRecordLinkAction(row.getRecord(1)));

    }).setButtonTooltip(ButtonAction.baCustom, "Show this record in Preview Window");

    htRecords.setDblClickHandler(HDT_Record.class, this::openRecord);

    htRecords.buildRows(records, (row, record) ->
    {
      row.setIconCellValue(0, record);
      row.setCellValue(1, record);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void openRecord(HDT_Record record)
  {
    okClicked = true;
    stage.close();
    Platform.runLater(() -> MainTextUtil.openRecordLinkAction(record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
