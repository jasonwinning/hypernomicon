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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TableView;

public class ChooseParentWorkFileDlgCtrlr extends HyperDlg
{
  @FXML private TableView<HyperTableRow> tvFiles;
  @FXML private Button btnOk, btnCancel;

  private HyperTable htFiles;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ChooseParentWorkFileDlgCtrlr create(String title, HDT_Work work)
  {
    ChooseParentWorkFileDlgCtrlr cpw = HyperDlg.create("ChooseParentWorkFileDlg.fxml", title, true);
    cpw.init(work);
    return cpw;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_Work work)
  {
    htFiles = new HyperTable(tvFiles, 0, false, "");
    htFiles.addCol(hdtWorkFile, ctNone);
    htFiles.addCol(hdtWorkFile, ctNone);
    htFiles.setDblClickHandler(HDT_WorkFile.class, workFile -> launchFile(workFile.getPath().getFilePath()));

    work.largerWork.get().workFiles.forEach(workFile ->
    {
      if (work.workFiles.contains(workFile)) return;

      String pathStr = "";

      if (workFile.getPath().isEmpty() == false)
      {
        FilePath filePath = workFile.getPath().getFilePath();
        pathStr = nullSwitch(db.getRootFilePath().relativize(filePath), filePath.getNameOnly().toString(), FilePath::toString);
      }

      HyperTableRow row = htFiles.newDataRow();
      row.setCellValue(0, workFile, pathStr);
      row.setCellValue(1, workFile, workFile.name());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_WorkFile getWorkFile()
  {
    return htFiles.selectedRecord();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (htFiles.selectedRecord() == null)
    {
      messageDialog("Select a file.", mtWarning);
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
