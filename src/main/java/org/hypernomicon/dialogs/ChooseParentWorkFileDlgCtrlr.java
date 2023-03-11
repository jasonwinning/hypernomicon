/*
 * Copyright 2015-2023 Jason Winning
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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;

import java.util.function.Predicate;

import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;
import javafx.fxml.FXML;
import javafx.scene.control.TableView;

public class ChooseParentWorkFileDlgCtrlr extends HyperDlg
{
  @FXML private TableView<HyperTableRow> tvFiles;

  private final HyperTable htFiles;

  public HDT_WorkFile getWorkFile()     { return htFiles.selectedRecord(); }
  @Override protected boolean isValid() { return (htFiles.selectedRecord() != null) || falseWithWarningMessage("Select a file."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ChooseParentWorkFileDlgCtrlr(HDT_Work work)
  {
    super("ChooseParentWorkFileDlg", "Choose Work File", true);

    htFiles = new HyperTable(tvFiles, 0, false, "");
    htFiles.addLabelCol(hdtWorkFile);
    htFiles.addLabelCol(hdtWorkFile);
    htFiles.setDblClickHandler(HDT_WorkFile.class, workFile -> launchFile(workFile.filePath()));

    htFiles.buildRows(work.largerWork.get().workFiles.stream().filter(Predicate.not(work.workFiles::contains)), (row, workFile) ->
    {
      String pathStr = "";

      if (workFile.pathNotEmpty())
      {
        FilePath filePath = workFile.filePath();
        pathStr = nullSwitch(db.getRootPath().relativize(filePath), filePath.getNameOnly().toString(), FilePath::toString);
      }

      row.setCellValue(0, workFile, pathStr);
      row.setCellValue(1, workFile, workFile.name());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
