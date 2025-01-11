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

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.App;
import org.hypernomicon.settings.SettingsDlgCtrlr.SettingsControl;
import org.hypernomicon.settings.WorkSearchKeySettings.WorkSearchKeyConfig;

import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.ListChangeListener.Change;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.TabPane;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public class WorkSearchKeysCtrlr implements SettingsControl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TabPane tpWorkSearchKeys;

  private final StringProperty examplesStr = new SimpleStringProperty();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(boolean noDB)
  {
    if (noDB) return;

    WorkSearchKeySettings settings = WorkSearchKeySettings.loadFromPrefNode();

    while (tpWorkSearchKeys.getTabs().size() > 1)
      tpWorkSearchKeys.getTabs().remove(0);

    tpWorkSearchKeys.getTabs().addListener((Change<?> change) ->
    {
      Platform.runLater(() ->
      {
        for (int ndx = 0; ndx < (tpWorkSearchKeys.getTabs().size() - 1); ndx++)
          tpWorkSearchKeys.getTabs().get(ndx).setText("Key " + (ndx + 1));
      });

      refreshExamples();
    });

    settings.forEach(this::addTab);

    tpWorkSearchKeys.getSelectionModel().selectedIndexProperty().addListener((ob, ov, nv) ->
    {
      if (nv.intValue() == (tpWorkSearchKeys.getTabs().size() - 1))
      {
        WorkSearchKeyCtrlr keyCtrlr = addTab(new WorkSearchKeyConfig());
        Platform.runLater(() -> tpWorkSearchKeys.getSelectionModel().select(keyCtrlr));
      }
    });

    tpWorkSearchKeys.getSelectionModel().select(0);
    Platform.runLater(this::refreshExamples);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private WorkSearchKeyCtrlr addTab(WorkSearchKeyConfig keyConfig)
  {
    try
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource("settings/WorkSearchKey.fxml"));
      AnchorPane ap = loader.load();
      WorkSearchKeyCtrlr keyCtrlr = loader.getController();
      keyCtrlr.setContent(ap);
      if (tpWorkSearchKeys.getTabs().size() == 1) keyCtrlr.setClosable(false);
      keyCtrlr.init(this, keyConfig);
      scaleNodeForDPI(ap);
      tpWorkSearchKeys.getTabs().add(tpWorkSearchKeys.getTabs().size() - 1, keyCtrlr);
      keyCtrlr.taExamples.textProperty().bind(examplesStr);

      return keyCtrlr;
    }
    catch (IOException e)
    {
      e.printStackTrace();
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void save(boolean noDB)
  {
    if (noDB) return;

    getSettingsFromUI().saveToPrefNode();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private WorkSearchKeySettings getSettingsFromUI()
  {
    WorkSearchKeySettings settings = new WorkSearchKeySettings();

    tpWorkSearchKeys.getTabs().subList(0, tpWorkSearchKeys.getTabs().size() - 1).stream().map(WorkSearchKeyCtrlr.class::cast).map(WorkSearchKeyCtrlr::save).forEach(settings::add);

    return settings;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refreshExamples()
  {
    Platform.runLater(() ->
    {
      WorkSearchKeySettings settings = getSettingsFromUI();

      List<String> strList = new ArrayList<>();
      strList.add("1 author: " + settings.format(List.of("Smith"), "1989"));

      if (settings.stream().anyMatch(setting -> setting.multipleAuthors))
      {
        strList.add("2 authors: " + settings.format(List.of("Smith", "Jones"                           ), "1989"));
        strList.add("3 authors: " + settings.format(List.of("Smith", "Jones", "Nguyen"                 ), "1989"));
        strList.add("4 authors: " + settings.format(List.of("Smith", "Jones", "Nguyen", "Garcia"       ), "1989"));
        strList.add("5 authors: " + settings.format(List.of("Smith", "Jones", "Nguyen", "Garcia", "Kim"), "1989"));
      }

      examplesStr.set(strListToStr(strList, false));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
