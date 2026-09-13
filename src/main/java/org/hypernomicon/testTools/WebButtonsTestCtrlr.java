/*
 * Copyright 2026 Jason Winning
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

package org.hypernomicon.testTools;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;

import java.util.*;

import org.hypernomicon.settings.WebButtonSettingsCtrlr;
import org.hypernomicon.testTools.TestConsoleDlgCtrlr.TestConsoleTab;
import org.hypernomicon.util.WebButton;
import org.hypernomicon.util.WebButton.WebButtonField;

import javafx.beans.property.SimpleStringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

//---------------------------------------------------------------------------

/**
 * The Test Console's Web Buttons tab: exercises every button offered in
 * Settings > Web Search Buttons from one place, for the per-release link-rot
 * check. Each opens in the system browser with the sample field values on the
 * right, built the way the record tabs build it (WebButton takes the first
 * pattern whose required fields are all non-blank, so blanking a field here
 * tries the fallback patterns). Rot is the target site changing, which only a
 * person looking at the live result can judge, so this is a launcher, not a
 * test.
 */
public class WebButtonsTestCtrlr implements TestConsoleTab
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A web-button context (a slot in Settings), as listed in the context combo box. */
  private record WebBtnContext(String prefKey, String label)
  {
    @Override public String toString() { return label; }
  }

//---------------------------------------------------------------------------

  private static final List<WebBtnContext> WEB_BTN_CONTEXTS = List.of
  (
    new WebBtnContext(WebButtonContextPrefKey.PERSON    , "Person"),
    new WebBtnContext(WebButtonContextPrefKey.PERSON_IMG, "Person image"),
    new WebBtnContext(WebButtonContextPrefKey.INST      , "Institution"),
    new WebBtnContext(WebButtonContextPrefKey.INST_MAP  , "Institution map"),
    new WebBtnContext(WebButtonContextPrefKey.WORK      , "Work"),
    new WebBtnContext(WebButtonContextPrefKey.DOI       , "DOI"),
    new WebBtnContext(WebButtonContextPrefKey.ISBN      , "ISBN"),
    new WebBtnContext(WebButtonContextPrefKey.GEN       , "Debate, position, argument, term")
  );

  @FXML private ComboBox<WebBtnContext> cbWebBtnContext;
  @FXML private GridPane gpWebBtnFields;
  @FXML private TableColumn<WebButton, String> colWebBtnName, colWebBtnCaption, colWebBtnPatterns;
  @FXML private TableView<WebButton> tvWebBtnPresets;
  @FXML private TextField tfWebBtnLastUrl;

  private final Map<WebButtonField, TextField> webBtnFieldInputs = new EnumMap<>(WebButtonField.class);

//---------------------------------------------------------------------------

  @Override public void init(TestConsoleDlgCtrlr console)
  {
    colWebBtnName    .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().getName()));
    colWebBtnCaption .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().getCaption()));
    colWebBtnPatterns.setCellValueFactory(cd -> new SimpleStringProperty(String.valueOf(cd.getValue().getPatterns().size())));

    cbWebBtnContext.getItems().setAll(WEB_BTN_CONTEXTS);
    cbWebBtnContext.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) -> populateContext(nv));
    cbWebBtnContext.getSelectionModel().selectFirst();
  }

//---------------------------------------------------------------------------

  @FXML private void btnWebBtnOpenAllClick() { tvWebBtnPresets.getItems().forEach(this::openWebBtn); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Sample values that make every preset's most specific pattern applicable
   *  and give a recognizable result page. */
  private static String sampleValue(String contextPrefKey, WebButtonField field)
  {
    return switch (field)
    {
      case Name                 -> WebButtonContextPrefKey.GEN.equals(contextPrefKey) ? "Epistemology" : "University of Pittsburgh";
      case SingleName, LastName -> "Wittgenstein";
      case FirstName, QueryName -> "Ludwig";
      case Field                -> "Philosophy";
      case DivisionName         -> "Department of Philosophy";
      case City                 -> "Pittsburgh";
      case Region               -> "Pennsylvania";
      case Country              -> "United States";
      case Title, QueryTitle    -> "Philosophical Investigations";
      case NumericYear, Year    -> "1953";
      case doi                  -> "10.1093/mind/LIX.236.433";
      case ISBN                 -> "9780631231592";
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateContext(WebBtnContext context)
  {
    List<WebButton> buttons = WebButtonSettingsCtrlr.buttonsFor(context.prefKey());

    tvWebBtnPresets.getItems().setAll(buttons);
    tvWebBtnPresets.getSelectionModel().selectFirst();

    // One input per field that any of the context's patterns requires or substitutes

    EnumSet<WebButtonField> fields = EnumSet.noneOf(WebButtonField.class);

    buttons.forEach(btn -> btn.getPatterns().forEach(pattern ->
    {
      fields.addAll(pattern.reqFields());

      for (WebButtonField field : WebButtonField.values())
        if (pattern.str.contains(field.key))
          fields.add(field);
    }));

    gpWebBtnFields.getChildren().clear();
    webBtnFieldInputs.clear();

    int rowNdx = 0;

    for (WebButtonField field : fields)
    {
      Label label = new Label(field.name());
      setToolTip(label, field.toolTip);

      TextField tf = new TextField(sampleValue(context.prefKey(), field));
      tf.setMaxWidth(Double.MAX_VALUE);
      GridPane.setHgrow(tf, Priority.ALWAYS);

      gpWebBtnFields.addRow(rowNdx++, label, tf);
      webBtnFieldInputs.put(field, tf);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnWebBtnOpenClick()
  {
    WebButton btn = tvWebBtnPresets.getSelectionModel().getSelectedItem();

    if (btn != null)
      openWebBtn(btn);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void openWebBtn(WebButton btn)
  {
    boolean first = true;

    for (Map.Entry<WebButtonField, TextField> entry : webBtnFieldInputs.entrySet())
    {
      if (first) btn.first(entry.getKey(), entry.getValue().getText());
      else       btn.next (entry.getKey(), entry.getValue().getText());

      first = false;
    }

    String url = btn.buildUrl();

    tfWebBtnLastUrl.setText(url != null ? url : "(no pattern of " + btn.getName() + " matched the supplied fields, or the prompt was cancelled)");

    if (url != null)
      openWebLink(url);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
