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

import org.hypernomicon.query.ui.SelectColumnsDlgCtrlr.TypeCheckBox;

import com.google.common.collect.ForwardingCollection;

//---------------------------------------------------------------------------

/**
 * <p>A column group is a collection of result columns that can be made visible or invisible all
 * at once using the column display options popup window.
 * <br>
 * <p>A column group has one or more column group items; there is one column group item to a
 * check box in the column display options popup window. Each ResultColumn is associated with
 * one or more column group items. It is associated with more than one column group item when
 * multiple record types have items with the same tag.
 *
 */
abstract class AbstractColumnGroup<GroupItemType extends ColumnGroupItem> extends ForwardingCollection<GroupItemType>
{
  final ResultsTable resultsTable;
  final String caption;

  TypeCheckBox checkBox;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  AbstractColumnGroup(String caption, ResultsTable resultsTable)
  {
    this.caption = caption;
    this.resultsTable = resultsTable;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
