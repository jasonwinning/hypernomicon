/*
 * Copyright 2015-2023 Jason Winning
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

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.List;
import java.util.prefs.Preferences;

import org.hypernomicon.util.WebButton;

import javafx.collections.FXCollections;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import javafx.util.StringConverter;

class WebButtonBar extends WebButtonCtrl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final TextField tfCaption;
  private final ComboBox<WebButton> cbPreset;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  WebButtonBar(String prefKey, List<WebButton> webBtnList, TextField tfCaption, ComboBox<WebButton> cbPreset, Button btnAdvanced)
  {
    super(prefKey, webBtnList);

    this.tfCaption = tfCaption;
    this.cbPreset = cbPreset;

    cbPreset.setItems(FXCollections.observableArrayList(webBtnList));

    cbPreset.setConverter(new StringConverter<>()
    {
      @Override public String toString(WebButton btn)
      {
        return btn == null ? "" : btn.getName();
      }

      @Override public WebButton fromString(String str)
      {
        return findFirst(cbPreset.getItems(), btn -> btn.getName().equalsIgnoreCase(str));
      }
    });

    cbPreset.getSelectionModel().selectedItemProperty().addListener((obs, ov, nv) ->
    {
      if (tfCaption == null) return;
      tfCaption.setText(nv == null ? "" : nv.getCaption());
    });

    btnAdvanced.setOnAction(event ->
    {
      try
      {
        EditWebButtonsDlgCtrlr dlg = EditWebButtonsDlgCtrlr.build(getWebButton(), prefKey);

        if ((dlg.showModal() == false) || dlg.unchanged()) return;

        WebButton webBtn = new WebButton(CUSTOM_NAME, tfCaption.getText());

        dlg.getPatterns(webBtn);
        cbPreset.getItems().removeIf(btn -> btn.getName().equals(CUSTOM_NAME));
        cbPreset.getItems().add(webBtn);
        cbPreset.getSelectionModel().select(webBtn);
      }
      catch (IOException e)
      {
        showStackTrace(e);
      }
    });

    cbPreset.getSelectionModel().select(ui.webButtonMap.get(prefKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void saveToPrefNode(Preferences node) { saveToPrefNode(node, prefKey, getWebButton()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private WebButton getWebButton()
  {
    WebButton webBtn = cbPreset.getValue();
    if (tfCaption != null)
      webBtn.setCaption(tfCaption.getText());

    return webBtn;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
