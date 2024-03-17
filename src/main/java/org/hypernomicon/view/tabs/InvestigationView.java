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

package org.hypernomicon.view.tabs;

import org.hypernomicon.model.records.HDT_Investigation;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextWrapper;

import static org.hypernomicon.App.ui;

import javafx.scene.control.Label;
import javafx.scene.control.Tab;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public final class InvestigationView
{

//---------------------------------------------------------------------------

  public final HDT_Investigation record;
  public final TextField tfName;

  final TextField tfSearchKey;
  final MainTextWrapper textWrapper;
  final Tab tab;

//---------------------------------------------------------------------------

  InvestigationView(HDT_Investigation record)
  {
    this(record, new TextViewInfo(record));
  }

//---------------------------------------------------------------------------

  InvestigationView(HDT_Investigation record, TextViewInfo textViewInfo)
  {
    assert(record == textViewInfo.record);

    this.record = record;

    BorderPane bPane = new BorderPane();
    GridPane gPane = new GridPane();

    tfName = new TextField();

    gPane.add(new Label("Investigation name:"), 0, 0); // column=2 row=1
    bPane.setTop(gPane);
    gPane.add(tfName, 1, 0);

    tfSearchKey = new TextField(record.getSearchKey());
    MainCtrlr.setSearchKeyToolTip(tfSearchKey);

    gPane.add(new Label("Search key:"), 2, 0);
    gPane.add(tfSearchKey, 3, 0);

    ColumnConstraints cc2 = new ColumnConstraints(),
                      cc4 = new ColumnConstraints();

    cc2.setHgrow(Priority.ALWAYS);
    cc4.setHgrow(Priority.ALWAYS);

    gPane.getColumnConstraints().addAll(new ColumnConstraints(), cc2, new ColumnConstraints(), cc4);

    gPane.setHgap(3);

    AnchorPane aPane = new AnchorPane();
    bPane.setCenter(aPane);

    textWrapper = new MainTextWrapper(aPane);
    textWrapper.loadFromRecord(record, false, textViewInfo);

    tab = new Tab();

    tfName.textProperty().addListener((ob, oldText, newText) ->
    {
      tab.setText(newText);
      ui.personHyperTab().updateInvInWorkTable();
    });

    tfName.setText(record.listName());

    tab.setContent(bPane);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
