/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.tree;

import javafx.fxml.FXML;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableView;

//---------------------------------------------------------------------------

public class TreeCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML TreeTableView<TreeRow> ttv;
  @FXML TreeTableColumn<TreeRow, TreeCellValue> tcName;
  @FXML TreeTableColumn<TreeRow, TreeRow> tcLinked;
  @FXML TreeTableColumn<TreeRow, String> tcDesc;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
