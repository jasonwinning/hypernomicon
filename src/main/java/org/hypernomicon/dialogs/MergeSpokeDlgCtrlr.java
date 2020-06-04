/*
 * Copyright 2015-2020 Jason Winning
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
import static org.hypernomicon.view.mainText.MainTextUtil.*;

import org.hypernomicon.model.records.HDT_RecordWithConnector;

import javafx.fxml.FXML;
import javafx.scene.control.RadioButton;
import javafx.scene.web.HTMLEditor;
import javafx.scene.web.WebView;

public class MergeSpokeDlgCtrlr extends HyperDlg
{
  @FXML private RadioButton rbDesc1, rbDesc2;
  @FXML private WebView view1, view2;
  @FXML private HTMLEditor he3;

  private String mainText1, mainText2;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MergeSpokeDlgCtrlr build(HDT_RecordWithConnector record1, HDT_RecordWithConnector record2)
  {
    return ((MergeSpokeDlgCtrlr) create("MergeSpokeDlg", "Select How to Merge Fields", true)).init(record1, record2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private MergeSpokeDlgCtrlr init(HDT_RecordWithConnector record1, HDT_RecordWithConnector record2)
  {
    rbDesc1.setText(db.getTypeName(record1.getType()));
    rbDesc2.setText(db.getTypeName(record2.getType()));

    mainText1 = record1.getMainText().getHtml();
    mainText2 = record2.getMainText().getHtml();

    view1.getEngine().loadContent(makeLinksExternal(prepHtmlForDisplay(mainText1).replace("contenteditable=\"true\"", "contentEditable=\"false\"")));
    view2.getEngine().loadContent(makeLinksExternal(prepHtmlForDisplay(mainText2).replace("contenteditable=\"true\"", "contentEditable=\"false\"")));

    if (extractTextFromHTML(mainText1).trim().isEmpty())
      if (extractTextFromHTML(mainText2).trim().length() > 0)
        rbDesc2.setSelected(true);

    he3.setHtmlText(disableLinks(""));

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getDesc()
  {
    if      (rbDesc1.isSelected()) return mainText1;
    else if (rbDesc2.isSelected()) return mainText2;
    else                           return getHtmlFromEditor(he3.getHtmlText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
