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

package org.hypernomicon.settings;

import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.util.WebButton;
import org.hypernomicon.util.WebButton.UrlPattern;
import org.hypernomicon.util.WebButton.WebButtonField;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.layout.GridPane;

public class EditWebButtonsDlgCtrlr extends HyperDlg
{
  @FXML private GridPane gpMain;
  @FXML private Button btnAdd;

  private final List<EditWebButtonCtrlr> ctrlrList = new ArrayList<>();
  private final String prefKey;

  private boolean unchanged = true;

  public void setChanged()   { unchanged = false; }
  public boolean unchanged() { return unchanged; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  EditWebButtonsDlgCtrlr(WebButton webBtn, String prefKey) throws IOException
  {
    super("settings/EditWebButtonsDlg", "Edit Web Button", true, true);

    this.prefKey = prefKey;

    if (webBtn == null)
      addEmptyPattern();
    else
    {
      for (UrlPattern urlPattern : webBtn.getPatterns())
        addPattern(urlPattern);
    }

    btnAdd.setOnAction(event ->
    {
      try
      {
        addEmptyPattern();
        unchanged = false;
      }
      catch (IOException e)
      {
        showStackTrace(e);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addEmptyPattern() throws IOException
  {
    addPattern(new UrlPattern("", EnumSet.noneOf(WebButtonField.class)));
  }

  private void addPattern(UrlPattern pattern) throws IOException
  {
    ctrlrList.add(new EditWebButtonCtrlr(pattern, prefKey, this, ctrlrList.size(), gpMain));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void up(EditWebButtonCtrlr ctrlr)
  {
    int ndx = ctrlrList.indexOf(ctrlr);
    if (ndx < 1) return;

    UrlPattern pattern1 = ctrlrList.get(ndx - 1).getPattern(),
               pattern2 = ctrlr.getPattern();

    ctrlrList.get(ndx - 1).setPattern(pattern2);
    ctrlr.setPattern(pattern1);

    unchanged = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void down(EditWebButtonCtrlr ctrlr)
  {
    int ndx = ctrlrList.indexOf(ctrlr);
    if (ndx == (ctrlrList.size() - 1)) return;

    UrlPattern pattern1 = ctrlr.getPattern(),
               pattern2 = ctrlrList.get(ndx + 1).getPattern();

    ctrlr.setPattern(pattern2);
    ctrlrList.get(ndx + 1).setPattern(pattern1);

    unchanged = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void delete(EditWebButtonCtrlr ctrlr)
  {
    int ndx = ctrlrList.indexOf(ctrlr);

    gpMain.getChildren().remove(ndx);
    ctrlrList.remove(ndx);

    unchanged = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getPatterns(WebButton btn)
  {
    ctrlrList.forEach(ctrlr -> btn.addPattern(ctrlr.getPattern()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
