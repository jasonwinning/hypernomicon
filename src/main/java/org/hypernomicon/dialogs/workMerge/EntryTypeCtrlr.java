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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;

import java.util.List;

import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.EntryType;

import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

public class EntryTypeCtrlr extends BibFieldRow
{
  @FXML private RadioButton rb1, rb2, rb3, rb4;
  @FXML private ComboBox<EntryType> cb1, cb2, cb3, cb4;
  @FXML private GridPane gp;
  @FXML private Label lbl;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void mergeInto(BibData bd) { return; } // EntryType should have already been set by the time this gets called

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void init(BibFieldEnum bibFieldEnum, AnchorPane ap, List<BibData> bibDataList)
  {
    this.ap = ap;
    this.bibFieldEnum = bibFieldEnum;

    lbl.setText(bibFieldEnum.getUserFriendlyName());

    if (bibDataList.size() < 4)
      deleteGridPaneColumn(gp, 3);
    else
      initOne(cb4, rb4, bibDataList.get(3));

    if (bibDataList.size() < 3)
      deleteGridPaneColumn(gp, 2);
    else
      initOne(cb3, rb3, bibDataList.get(2));

    initOne(cb2, rb2, bibDataList.get(1));
    initOne(cb1, rb1, bibDataList.get(0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initOne(ComboBox<EntryType> cb, RadioButton rb, BibData bd)
  {
    bibManagerDlg.initCB(cb);

    if (bd.entryTypeNotEmpty())
    {
      EntryType entryType = bd.getEntryType();
      if (cb.getItems().contains(entryType) == false)
      {
        warningPopup('"' + entryType.getUserFriendlyName() + "\" is not a valid " + db.getBibLibrary().type().getUserFriendlyName() + " entry type.");
        cb.getSelectionModel().select(null);
      }
      else
      {
        cb.getSelectionModel().select(entryType);
        rb.setSelected(true);
      }
    }

    cb.getSelectionModel().selectedItemProperty().addListener((obs, ov, nv) -> rb.setSelected(true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EntryType getEntryType()
  {
    if (rb1.isSelected()) return cb1.getValue();
    if (rb2.isSelected()) return cb2.getValue();
    if (rb3.isSelected()) return cb3.getValue();
                          return cb4.getValue();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
