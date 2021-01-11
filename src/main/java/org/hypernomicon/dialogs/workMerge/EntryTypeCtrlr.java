/*
 * Copyright 2015-2021 Jason Winning
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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;

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

  @Override void init(BibFieldEnum bibFieldEnum, AnchorPane ap, BibData bd1, BibData bd2, BibData bd3, BibData bd4)
  {
    this.ap = ap;
    this.bibFieldEnum = bibFieldEnum;

    lbl.setText(bibFieldEnum.getUserFriendlyName());

    if (bd4 == null)
      deleteGridPaneColumn(gp, 3);
    else
      initOne(cb4, rb4, bd4);

    if (bd3 == null)
      deleteGridPaneColumn(gp, 2);
    else
      initOne(cb3, rb3, bd3);

    initOne(cb2, rb2, bd2);
    initOne(cb1, rb1, bd1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initOne(ComboBox<EntryType> cb, RadioButton rb, BibData bd)
  {
    bibManagerDlg.initCB(cb);

    if (bd.entryTypeNotEmpty())
    {
      EntryType entryType = bd.getEntryType();
      if (cb.getItems().contains(entryType) == false)
      {
        messageDialog("\"" + entryType.getUserFriendlyName() + "\" is not a valid " +
                      db.getBibLibrary().type().getUserFriendlyName() + " entry type.", mtWarning);
        cb.getSelectionModel().select(null);
      }
      else
      {
        cb.getSelectionModel().select(entryType);
        rb.setSelected(true);
      }
    }
  }

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

}
