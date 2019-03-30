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

package org.hypernomicon.view.reports;

import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.view.tabs.QueriesTabCtrlr.QueryView;
import org.hypernomicon.view.wrappers.HyperTableRow;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.scene.control.Label;
import javafx.scene.control.TableView;
import javafx.scene.layout.AnchorPane;

public class ReportTable
{
  private TableView<HyperTableRow> tv;
  private ReportEngine reportEngine = null;
  final private QueryView qv;

  public ReportTable(QueryView queryView)
  {
    tv = new TableView<>();
    qv = queryView;

    AnchorPane.setBottomAnchor(tv, 0.0);
    AnchorPane.setTopAnchor(tv, 0.0);
    AnchorPane.setLeftAnchor(tv, 0.0);
    AnchorPane.setRightAnchor(tv, 0.0);

    scaleNodeForDPI(tv);
    setFontSize(tv);

    tv.setPlaceholder(new Label("There are no query results to display."));

    tv.getSelectionModel().selectedItemProperty().addListener((observable, oldRow, newRow) -> qv.refreshView());
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void clear()
  {
    tv.getColumns().clear();
    tv.getItems().clear();
    tv.setContextMenu(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setParent(AnchorPane apResults)
  {
    apResults.getChildren().add(tv);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void removeFromParent()
  {
    removeFromAnchor(tv);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void inject(ReportEngine reportEngine)
  {
    this.reportEngine = reportEngine;

    Platform.runLater(() ->
    {
      List<HyperTableRow> rows = reportEngine.getRows();
      tv.setItems(FXCollections.observableList(rows));

      if (reportEngine.alwaysShowDescription() && (rows.size() > 0))
        tv.getSelectionModel().selectFirst();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void format(ReportEngine reportEngine)
  {
    clear();
    reportEngine.prepTable(tv);

    tv.setPlaceholder(new Label("There are no query results to display."));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public String getHtmlForCurrentRow()
  {
    return reportEngine == null ? "" : reportEngine.getHtml(tv.getSelectionModel().getSelectedItem());
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
