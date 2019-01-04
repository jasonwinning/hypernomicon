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

package org.hypernomicon.view.workMerge;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.App.*;

import org.hypernomicon.bib.BibData;
import org.hypernomicon.bib.BibData.BibFieldEnum;
import org.hypernomicon.bib.BibData.EntryType;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

public class MergeWorksCBController extends BibFieldRow
{
  @FXML private RadioButton rb1;
  @FXML private RadioButton rb2;
  @FXML private RadioButton rb3;
  @FXML private RadioButton rb4;

  @FXML private ComboBox<EntryType> cb1;
  @FXML private ComboBox<EntryType> cb2;
  @FXML private ComboBox<EntryType> cb3;
  @FXML private ComboBox<EntryType> cb4;

  @FXML private GridPane gp;

  @FXML private Label lbl;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void mergeInto(BibData bd) { return; } // EntryType should have already been set by the time this gets called

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EntryType getEntryType()
  {
    if      (rb1.isSelected()) return cb1.getValue();
    else if (rb2.isSelected()) return cb2.getValue();
    else if (rb3.isSelected()) return cb3.getValue();
    else                       return cb4.getValue();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


  @Override protected void init(BibFieldEnum bibFieldEnum, AnchorPane ap, BibData bd1, BibData bd2, BibData bd3, BibData bd4)
  {
    this.ap = ap;
    this.bibFieldEnum = bibFieldEnum;

    lbl.setText(BibData.getFieldName(bibFieldEnum));

    if (bd4 == null)
    {
      deleteGridPaneColumn(gp, 3);
    }
    else
    {
      bibManagerDlg.initCB(cb4);

      if (bd4.entryTypeNotEmpty())
      {
        cb4.setValue(bd4.getEntryType());
        cb4.getSelectionModel().select(bd4.getEntryType());

        rb4.setSelected(true);
      }
    }

    if (bd3 == null)
    {
      deleteGridPaneColumn(gp, 2);
    }
    else
    {
      bibManagerDlg.initCB(cb3);

      if (bd3.entryTypeNotEmpty())
      {
        cb3.setValue(bd3.getEntryType());
        cb3.getSelectionModel().select(bd3.getEntryType());

        rb3.setSelected(true);
      }
    }

    bibManagerDlg.initCB(cb2);

    if (bd2.entryTypeNotEmpty())
    {
      cb2.setValue(bd2.getEntryType());
      cb2.getSelectionModel().select(bd2.getEntryType());

      rb2.setSelected(true);
    }

    bibManagerDlg.initCB(cb1);

    if (bd1.entryTypeNotEmpty())
    {
      cb1.setValue(bd1.getEntryType());
      cb1.getSelectionModel().select(bd1.getEntryType());

      rb1.setSelected(true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
