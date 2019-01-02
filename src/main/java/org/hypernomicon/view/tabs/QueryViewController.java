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

package org.hypernomicon.view.tabs;

import org.controlsfx.control.MasterDetailPane;

import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.ResultsRow;
import javafx.fxml.FXML;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableView;
import javafx.scene.layout.AnchorPane;

public class QueryViewController extends SplitPane
{
  @FXML MasterDetailPane spMain;
  @FXML MasterDetailPane spLower;
  @FXML TableView<HyperTableRow> tvFields;
  @FXML TableView<ResultsRow> tvResults;
  @FXML AnchorPane apDescription;
  @FXML AnchorPane apResults;
}
