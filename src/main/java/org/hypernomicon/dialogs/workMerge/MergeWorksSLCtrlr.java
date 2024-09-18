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

import java.util.List;

import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

public class MergeWorksSLCtrlr extends BibFieldRow
{
  @FXML private RadioButton rb1, rb2, rb3, rb4;
  @FXML private TextField tf1, tf2, tf3, tf4;
  @FXML private GridPane gp;
  @FXML private Label lbl;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void mergeInto(BibData bd)
  {
    String str;

    if      (rb1.isSelected()) str = tf1.getText();
    else if (rb2.isSelected()) str = tf2.getText();
    else if (rb3.isSelected()) str = tf3.getText();
    else                       str = tf4.getText();

    bd.setStr(bibFieldEnum, str);
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
      deleteGridPaneColumn(gp, 3);
    }
    else if (bibDataList.get(3).fieldNotEmpty(bibFieldEnum))
    {
      tf4.setText(bibDataList.get(3).getStr(bibFieldEnum));
      rb4.setSelected(true);
    }

    if (bibDataList.size() < 3)
    {
      deleteGridPaneColumn(gp, 2);
    }
    else if (bibDataList.get(2).fieldNotEmpty(bibFieldEnum))
    {
      tf3.setText(bibDataList.get(2).getStr(bibFieldEnum));
      rb3.setSelected(true);
    }

    if (bibDataList.get(1).fieldNotEmpty(bibFieldEnum))
    {
      tf2.setText(bibDataList.get(1).getStr(bibFieldEnum));
      rb2.setSelected(true);
    }

    if (bibDataList.get(0).fieldNotEmpty(bibFieldEnum))
    {
      tf1.setText(bibDataList.get(0).getStr(bibFieldEnum));
      rb1.setSelected(true);
    }

    tf1.textProperty().addListener((obs, ov, nv) -> rb1.setSelected(true));
    tf2.textProperty().addListener((obs, ov, nv) -> rb2.setSelected(true));
    tf3.textProperty().addListener((obs, ov, nv) -> rb3.setSelected(true));
    tf4.textProperty().addListener((obs, ov, nv) -> rb4.setSelected(true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
