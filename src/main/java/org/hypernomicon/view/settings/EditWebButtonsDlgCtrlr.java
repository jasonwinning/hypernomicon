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

package org.hypernomicon.view.settings;

import static org.hypernomicon.util.Util.showStackTrace;

import java.io.IOException;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.hypernomicon.App;
import org.hypernomicon.util.WebButton;
import org.hypernomicon.util.WebButton.UrlPattern;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.view.dialogs.HyperDlg;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.Button;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

public class EditWebButtonsDlgCtrlr extends HyperDlg
{
  @FXML private GridPane gpMain;
  @FXML private Button btnAdd;

  private final List<EditWebButtonCtrlr> ctrlrList = new ArrayList<>();
  private String prefKey = null;
  private boolean unchanged = true;

  public void setChanged()   { unchanged = false; }
  public boolean unchanged() { return unchanged; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static EditWebButtonsDlgCtrlr create(WebButton webBtn, String prefKey) throws IOException
  {
    EditWebButtonsDlgCtrlr dlg = HyperDlg.createUsingFullPath("view/settings/EditWebButtonsDlg.fxml", "Edit Web Button", true);
    dlg.init(webBtn, prefKey);
    return dlg;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(WebButton webBtn, String prefKey) throws IOException
  {
    this.prefKey = prefKey;

    for (UrlPattern urlPattern : webBtn.getPatterns())
      addPattern(urlPattern);

    btnAdd.setOnAction(event ->
    {
      try
      {
        addPattern(new UrlPattern(EnumSet.noneOf(WebButtonField.class), ""));
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

  private void addPattern(UrlPattern urlPattern) throws IOException
  {
    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/settings/EditWebButton.fxml"));
    AnchorPane ap = loader.load();
    EditWebButtonCtrlr ctrlr = loader.getController();

    ctrlr.init(urlPattern, prefKey, this);

    GridPane.setRowIndex(ap, ctrlrList.size());

    gpMain.getChildren().add(ap);

    ctrlrList.add(ctrlr);
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
