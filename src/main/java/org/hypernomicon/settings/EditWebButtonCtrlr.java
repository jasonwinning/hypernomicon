/*
 * Copyright 2015-2022 Jason Winning
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

import static org.hypernomicon.util.UIUtil.*;

import java.util.EnumMap;
import java.util.EnumSet;

import org.hypernomicon.util.WebButton.UrlPattern;
import org.hypernomicon.util.WebButton.WebButtonField;

import javafx.fxml.FXML;
import javafx.geometry.Insets;
import javafx.geometry.NodeOrientation;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;
import javafx.scene.layout.HBox;

public class EditWebButtonCtrlr
{
  @FXML private HBox hBox;
  @FXML private Button btnUp, btnDown, btnDelete;
  @FXML private TextField tfPattern;

  private final EnumMap<WebButtonField, CheckBox> fieldToChk = new EnumMap<>(WebButtonField.class);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int caretPos = 0;

  public void init(UrlPattern urlPattern, String prefKey, EditWebButtonsDlgCtrlr dlg)
  {
    btnUp    .setOnAction(event -> dlg.up    (this));
    btnDown  .setOnAction(event -> dlg.down  (this));
    btnDelete.setOnAction(event -> dlg.delete(this));

    hBox.getChildren().clear();

    for (WebButtonField field : WebButtonField.getFieldsForPrefKey(prefKey))
    {
      CheckBox chk = new CheckBox(hBox.getChildren().isEmpty() ? "" : "   ");
      chk.setNodeOrientation(NodeOrientation.RIGHT_TO_LEFT);
      HBox.setMargin(chk, new Insets(4.0, 0.0, 0.0, 0.0));
      fieldToChk.put(field, chk);

      Button btn = new Button(field.key);
      btn.setPrefHeight(25.0);
      setToolTip(btn, field.toolTip);
      hBox.getChildren().addAll(chk, btn);

      btn.focusedProperty().addListener((obs, ov, nv) -> caretPos = tfPattern.getCaretPosition());
      btn.setOnAction(event -> tfPattern.insertText(caretPos, field.key));
    }

    setPattern(urlPattern);

    tfPattern.textProperty().addListener((obs, ov, nv) -> dlg.setChanged());

    fieldToChk.values().forEach(chk -> chk.setOnAction(event -> dlg.setChanged()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setPattern(UrlPattern urlPattern)
  {
    tfPattern.setText(urlPattern.str);

    EnumSet<WebButtonField> requiredFields = urlPattern.reqFields();

    fieldToChk.forEach((field, chk) -> chk.setSelected(requiredFields.contains(field)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public UrlPattern getPattern()
  {
    EnumSet<WebButtonField> requiredFields = EnumSet.noneOf(WebButtonField.class);

    fieldToChk.forEach((field, chk) ->
    {
      if (chk.isSelected())
        requiredFields.add(field);
    });

    return new UrlPattern(tfPattern.getText(), requiredFields);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
