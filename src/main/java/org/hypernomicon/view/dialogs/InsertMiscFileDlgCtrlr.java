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

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;

public class InsertMiscFileDlgCtrlr extends HyperDlg
{
  @FXML private Button btnExisting, btnNew;
  @FXML private ComboBox<HyperTableCell> cbExisting;

  private HDT_MiscFile miscFile = null;
  private HyperCB hcbExisting;

  public HDT_MiscFile getMiscFile()     { return miscFile; }

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static InsertMiscFileDlgCtrlr create()
  {
    InsertMiscFileDlgCtrlr imfd = HyperDlg.create("InsertMiscFileDlg.fxml", "Insert Misc. File", true);
    imfd.init();
    return imfd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    hcbExisting = new HyperCB(cbExisting, ctDropDownList, new StandardPopulator(hdtMiscFile));

    btnExisting.setOnAction(event ->
    {
      miscFile = hcbExisting.selectedRecord();

      if (miscFile == null)
        btnCancelClick();

      btnOkClick();
    });

    btnNew.setOnAction(event -> btnOkClick());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
