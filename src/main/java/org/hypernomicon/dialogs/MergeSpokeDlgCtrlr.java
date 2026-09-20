/*
 * Copyright 2015-2026 Jason Winning
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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.view.mainText.MainTextWrapper;

import javafx.fxml.FXML;
import javafx.scene.control.RadioButton;
import javafx.scene.web.*;

//---------------------------------------------------------------------------

public class MergeSpokeDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private RadioButton rbDesc1, rbDesc2;
  @FXML private WebView view1, view2;
  @FXML private HTMLEditor he3;

  private final String mainText1, mainText2;

//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------

  public MergeSpokeDlgCtrlr(HDT_RecordWithMainText record1, HDT_RecordWithMainText record2)
  {
    super("MergeSpokeDlg", "Select How to Merge Fields", true);

    rbDesc1.setText(getTypeName(record1.getType()));
    rbDesc2.setText(getTypeName(record2.getType()));

    mainText1 = record1.getMainText().getHtml();
    mainText2 = record2.getMainText().getHtml();

    MainTextWrapper.initDialogDescView(view1, mainText1);
    MainTextWrapper.initDialogDescView(view2, mainText2);

    if (strNullOrBlank(extractTextFromHTML(mainText1)) && strNotNullOrBlank(extractTextFromHTML(mainText2)))
      rbDesc2.setSelected(true);

    WebView editorView = (WebView) he3.lookup(".web-view");

    editorView.getEngine().setUserStyleSheetLocation(cssStrToDataURI(EMPTY_FONT_CSS));
    updateZoomFromPref(editorView, ZoomPrefKey.MAINTEXT);

    he3.setHtmlText(prepHtmlForEditing(""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getDesc()
  {
    if (rbDesc1.isSelected()) return mainText1;
    if (rbDesc2.isSelected()) return mainText2;

    return getHtmlFromEditor(he3.getHtmlText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
