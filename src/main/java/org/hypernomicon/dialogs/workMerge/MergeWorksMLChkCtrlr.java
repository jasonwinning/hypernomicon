/*
 * Copyright 2015-2024 Jason Winning
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

package org.hypernomicon.dialogs.workMerge;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;

import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

public class MergeWorksMLChkCtrlr extends BibFieldRow
{
  @FXML private CheckBox chk1, chk2, chk3, chk4;
  @FXML private TextArea ta1, ta2, ta3, ta4;
  @FXML private GridPane gp;
  @FXML private Label lbl;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void mergeInto(BibData bd)
  {
    List<String> list = new ArrayList<>();

    if (chk1.isSelected()) list.addAll(convertMultiLineStrToStrList(ta1.getText(), true));
    if (chk2.isSelected()) list.addAll(convertMultiLineStrToStrList(ta2.getText(), true));
    if (chk3.isSelected()) list.addAll(convertMultiLineStrToStrList(ta3.getText(), true));
    if (chk4.isSelected()) list.addAll(convertMultiLineStrToStrList(ta4.getText(), true));

    bd.setMultiStr(bibFieldEnum, list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void init(BibFieldEnum bibFieldEnum, AnchorPane ap, List<BibData> bibDataList)
  {
    this.ap = ap;
    this.bibFieldEnum = bibFieldEnum;

    lbl.setText(bibFieldEnum.getUserFriendlyName());

    if (bibDataList.size() < 4)
    {
      chk4.setSelected(false);
      deleteGridPaneColumn(gp, 3);
    }
    else if (bibDataList.get(3).fieldNotEmpty(bibFieldEnum))
      ta4.setText(strListToStr(bibDataList.get(3).getMultiStr(bibFieldEnum), true));

    if (bibDataList.size() < 3)
    {
      chk3.setSelected(false);
      deleteGridPaneColumn(gp, 2);
    }
    else if (bibDataList.get(2).fieldNotEmpty(bibFieldEnum))
      ta3.setText(strListToStr(bibDataList.get(2).getMultiStr(bibFieldEnum), true));

    if (bibDataList.get(1).fieldNotEmpty(bibFieldEnum))
      ta2.setText(strListToStr(bibDataList.get(1).getMultiStr(bibFieldEnum), true));

    if (bibDataList.get(0).fieldNotEmpty(bibFieldEnum))
      ta1.setText(strListToStr(bibDataList.get(0).getMultiStr(bibFieldEnum), true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
