/*
 * Copyright 2015-2025 Jason Winning
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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.WebButton.WebButtonField.*;

import java.io.IOException;
import java.util.EnumMap;
import java.util.EnumSet;

import org.hypernomicon.App;
import org.hypernomicon.util.WebButton.UrlPattern;
import org.hypernomicon.util.WebButton.WebButtonField;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Insets;
import javafx.geometry.NodeOrientation;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;
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

  EditWebButtonCtrlr(UrlPattern urlPattern, String prefKey, EditWebButtonsDlgCtrlr dlg, int rowNdx, GridPane gpMain) throws IOException
  {
    FXMLLoader loader = new FXMLLoader(App.class.getResource("settings/EditWebButton.fxml"), null, null, klass -> this);
    AnchorPane ap = loader.load();

    btnUp    .setOnAction(event -> dlg.up    (this));
    btnDown  .setOnAction(event -> dlg.down  (this));
    btnDelete.setOnAction(event -> dlg.delete(this));

    hBox.getChildren().clear();

    for (WebButtonField field : getFieldsForPrefKey(prefKey))
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

      GridPane.setRowIndex(ap, rowNdx);
      addToParent(ap, gpMain);
    }

    setPattern(urlPattern);

    tfPattern.textProperty().addListener((obs, ov, nv) -> dlg.setChanged());

    fieldToChk.values().forEach(chk -> chk.setOnAction(event -> dlg.setChanged()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EnumSet<WebButtonField> getFieldsForPrefKey(String prefKey)
  {
    EnumSet<WebButtonField> fields = EnumSet.noneOf(WebButtonField.class);

    switch (prefKey)
    {
      case PREF_KEY_PERSON_SRCH : case PREF_KEY_PERSON_IMG_SRCH :

        fields.add(FirstName); fields.add(LastName); fields.add(SingleName); fields.add(QueryName); fields.add(Field);
        break;

      case PREF_KEY_INST_SRCH :

        fields.add(Name); fields.add(DivisionName);
        break;

      case PREF_KEY_INST_MAP_SRCH :

        fields.add(Name); fields.add(City); fields.add(Region); fields.add(Country);
        break;

      case PREF_KEY_DOI_SRCH :

        fields.add(doi);
        break;

      case PREF_KEY_ISBN_SRCH :

        fields.add(ISBN);
        break;

      case PREF_KEY_WORK_SRCH :

        fields.add(Title); fields.add(QueryTitle); fields.add(NumericYear); fields.add(SingleName); fields.add(ISBN); fields.add(doi);
        break;

      case PREF_KEY_GEN_SRCH :

        fields.add(Name);
        break;
    }

    return fields;
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
