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

package org.hypernomicon.query.ui;

import org.hypernomicon.model.records.HDT_Record;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.TableColumn.CellDataFeatures;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

final class ResultCellValue
{

//---------------------------------------------------------------------------

  final String text;
  final HDT_Record record;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ResultCellValue(String text, HDT_Record record)
  {
    this.text = text;
    this.record = record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString() { return text; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ObservableValue<ResultCellValue> observableCellValue(CellDataFeatures<ResultRow, ResultCellValue> cellData, String str)
  {
    return new SimpleObjectProperty<>(new ResultCellValue(str, cellData.getValue().getRecord()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}