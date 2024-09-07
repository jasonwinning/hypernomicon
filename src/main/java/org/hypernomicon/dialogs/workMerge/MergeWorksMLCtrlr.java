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

import java.util.List;

import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextArea;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

public class MergeWorksMLCtrlr extends BibFieldRow
{
  @FXML private RadioButton rb1, rb2, rb3, rb4;
  @FXML private TextArea ta1, ta2, ta3, ta4;
  @FXML private GridPane gp;
  @FXML private Label lbl;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void mergeInto(BibData bd)
  {
    String str;

    if      (rb1.isSelected()) str = ta1.getText();
    else if (rb2.isSelected()) str = ta2.getText();
    else if (rb3.isSelected()) str = ta3.getText();
    else                       str = ta4.getText();

    bd.setMultiStr(bibFieldEnum, convertMultiLineStrToStrList(str, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void init(BibFieldEnum bibFieldEnum, AnchorPane ap, List<BibData> bibDataList)
  {
    this.ap = ap;
    this.bibFieldEnum = bibFieldEnum;

    lbl.setText(bibFieldEnum.getUserFriendlyName());

    if (bibDataList.size() < 4)
      deleteGridPaneColumn(gp, 3);
    else if (bibDataList.get(3).fieldNotEmpty(bibFieldEnum))
    {
      ta4.setText(strListToStr(bibDataList.get(3).getMultiStr(bibFieldEnum), true));
      rb4.setSelected(true);
    }

    if (bibDataList.size() < 3)
      deleteGridPaneColumn(gp, 2);
    else if (bibDataList.get(2).fieldNotEmpty(bibFieldEnum))
    {
      ta3.setText(strListToStr(bibDataList.get(2).getMultiStr(bibFieldEnum), true));
      rb3.setSelected(true);
    }

    if (bibDataList.get(1).fieldNotEmpty(bibFieldEnum))
    {
      ta2.setText(strListToStr(bibDataList.get(1).getMultiStr(bibFieldEnum), true));
      rb2.setSelected(true);
    }

    if (bibDataList.get(0).fieldNotEmpty(bibFieldEnum))
    {
      ta1.setText(strListToStr(bibDataList.get(0).getMultiStr(bibFieldEnum), true));
      rb1.setSelected(true);
    }

    ta1.textProperty().addListener((obs, ov, nv) -> rb1.setSelected(true));
    ta2.textProperty().addListener((obs, ov, nv) -> rb2.setSelected(true));
    ta3.textProperty().addListener((obs, ov, nv) -> rb3.setSelected(true));
    ta4.textProperty().addListener((obs, ov, nv) -> rb4.setSelected(true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
