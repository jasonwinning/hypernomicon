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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.HDT_RecordBase.makeSortKeyByType;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.ui.ResultCellValue.*;
import static org.hypernomicon.util.Util.*;

import java.time.Instant;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.function.Function;

import org.apache.commons.lang3.mutable.MutableInt;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.HDI_OnlinePointerMulti;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.scene.control.TableColumn;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

class ResultColumn extends TableColumn<ResultsRow, ResultCellValue>
{

//---------------------------------------------------------------------------

  final EnumMap<RecordType, ColumnGroupItem> map = new EnumMap<>(RecordType.class);

//---------------------------------------------------------------------------

  private ResultColumn(String caption)
  {
    this(caption, false);
  }

//---------------------------------------------------------------------------

  private ResultColumn(String caption, boolean caseSensitive)
  {
    super(caption);

    setComparator((cell1, cell2) ->
    {
      if ((cell1 == null) && (cell2 == null)) return 0;
      if (cell1 == null) return -1;
      if (cell2 == null) return 1;

      if (caseSensitive)
        return safeStr(cell1.text).trim().compareTo(safeStr(cell2.text).trim());

      return safeStr(cell1.text).trim().compareToIgnoreCase(safeStr(cell2.text).trim());
    });
  }

//---------------------------------------------------------------------------

  private ResultColumn(String caption, Comparator<HDT_Record> comparator)
  {
    this(caption, comparator, false);
  }

//---------------------------------------------------------------------------

  private ResultColumn(String caption, Comparator<HDT_Record> comparator, boolean caseSensitive)
  {
    super(caption);

    setComparator((cell1, cell2) ->
    {
      if ((cell1 == null) && (cell2 == null)) return 0;
      if (cell1 == null) return -1;
      if (cell2 == null) return 1;

      if (HDT_Record.isEmpty(cell1.record) || HDT_Record.isEmpty(cell2.record))
      {
        if (caseSensitive)
          return safeStr(cell1.text).trim().compareTo(safeStr(cell2.text).trim());

        return safeStr(cell1.text).trim().compareToIgnoreCase(safeStr(cell2.text).trim());
      }

      return comparator.compare(cell1.record, cell2.record);
    });
  }

//---------------------------------------------------------------------------

  private ResultColumn(String caption, Function<String, String> sortKeyFunction)
  {
    this(caption, sortKeyFunction, false);
  }

//---------------------------------------------------------------------------

  private ResultColumn(String caption, Function<String, String> sortKeyFunction, boolean caseSensitive)
  {
    super(caption);

    setComparator((cell1, cell2) ->
    {
      if ((cell1 == null) && (cell2 == null)) return 0;
      if (cell1 == null) return -1;
      if (cell2 == null) return 1;

      if (caseSensitive)
        return sortKeyFunction.apply(safeStr(cell1.text).trim()).compareTo(sortKeyFunction.apply(safeStr(cell2.text).trim()));

      return sortKeyFunction.apply(safeStr(cell1.text).trim()).compareToIgnoreCase(sortKeyFunction.apply(safeStr(cell2.text).trim()));
    });
  }

//---------------------------------------------------------------------------

  /**
   *
   * @param caption column header name
   * @param comparator how to compare the display text of the cells
   * @param klass is just there to disambiguate from the other constructor that takes a comparator; it doesn't do anything
   */
  private ResultColumn(String caption, Comparator<String> comparator, Class<String> klass)
  {
    super(caption);

    setComparator((cell1, cell2) ->
    {
      if ((cell1 == null) && (cell2 == null)) return 0;
      if (cell1 == null) return -1;
      if (cell2 == null) return 1;

      return comparator.compare(safeStr(cell1.text).trim(), safeStr(cell2.text).trim());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ResultColumn newDateColumn(Tag dateTag)
  {
    Comparator<HDT_Record> comparator = null;

    switch (dateTag)
    {
      case tagCreationDate :

        comparator = (record1, record2) ->
        {
          Instant i1 = nullSwitch(record1.getCreationDate(), Instant.MIN);
          Instant i2 = nullSwitch(record2.getCreationDate(), Instant.MIN);
          return i1.compareTo(i2);
        };

        break;

      case tagModifiedDate :

        comparator = (record1, record2) ->
        {
          Instant i1 = nullSwitch(record1.getModifiedDate(), Instant.MIN);
          Instant i2 = nullSwitch(record2.getModifiedDate(), Instant.MIN);
          return i1.compareTo(i2);
        };

        break;

      case tagViewDate :

        comparator = (record1, record2) ->
        {
          Instant i1 = nullSwitch(record1.getViewDate(), Instant.MIN);
          Instant i2 = nullSwitch(record2.getViewDate(), Instant.MIN);
          return i1.compareTo(i2);
        };

        break;

      default : break;
    }

    ResultColumn col = new ResultColumn(dateTag.header, comparator);

    col.setCellValueFactory(cellData ->
    {
      Instant i = null;
      HDT_Record record = cellData.getValue().getRecord();

      if ((record != null) && (record.getType() != hdtNone)) switch (dateTag)
      {
        case tagCreationDate : i = record.getCreationDate(); break;
        case tagModifiedDate : i = record.getModifiedDate(); break;
        case tagViewDate     : i = record.getViewDate    (); break;
        default              :                               break;
      }

      return observableCellValue(cellData, i == null ? "" : dateTimeToUserReadableStr(i));
    });

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ResultColumn newNonGeneralColumn(ColumnGroupItem firstItem, EnumMap<RecordType, ColumnGroupItem> recordTypeToItem)
  {
    ResultColumn col;

    switch (firstItem.tag)
    {
      case tagTitle :

        col = new ResultColumn(firstItem.caption, str -> makeSortKeyByType(str, hdtWork));
        break;

      case tagYear :

        Comparator<String> strComparator = (str1, str2) ->
        {
          MutableInt result = new MutableInt();

          if (HyperTableCell.compareNumberStrings(str1, str2, result))
            return result.getValue();

          return str1.compareToIgnoreCase(str2);
        };

        col = new ResultColumn(firstItem.caption, strComparator, String.class);
        break;

      default :

        col = new ResultColumn(firstItem.caption);
        break;
    }

    boolean visible = false;
    for (ColumnGroupItem item : recordTypeToItem.values())
      if (item.relType == RelationType.rtNone)
      {
        visible = true;
        break;
      }

    col.setVisible(visible);

    col.setCellValueFactory(cellData ->
    {
      String str = "";
      HDT_Record record = cellData.getValue().getRecord();

      if (record != null)
      {
        ColumnGroupItem item = recordTypeToItem.get(record.getType());

        if (item != null)
        {
          str = item.relType != RelationType.rtNone ?
            HDI_OnlinePointerMulti.recordStreamResultText(db.getObjType(item.relType), db.getSubjectList(item.relType, record).stream())
          :
            record.resultTextForTag(item.tag);
        }
      }

      return observableCellValue(cellData, str);
    });

    col.setMaxWidth(ColumnGroupItem.RESULT_COL_MAX_WIDTH);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ResultColumn newRecordIDColumn()
  {
    ResultColumn col = new ResultColumn("ID", Comparator.comparingInt(HDT_Record::getID));
    col.setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordIDStr()));
    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ResultColumn newRecordNameColumn()
  {
    ResultColumn col = new ResultColumn("Name", str -> makeSortKeyByType(str, hdtWork));
    col.setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordName()));
    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ResultColumn newRecordTypeColumn()
  {
    ResultColumn col = new ResultColumn("Type");
    col.setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordTypeStr()));
    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ResultColumn newSearchKeyColumn()
  {
    ResultColumn col = new ResultColumn("Search Key");
    col.setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getSearchKey()));
    col.setVisible(false);
    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ResultColumn newSortKeyColumn()
  {
    ResultColumn col = new ResultColumn("Sort Key", true);
    col.setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getSortKey()));
    col.setVisible(false);
    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ResultColumn newBibFieldColumn(BibFieldEnum field)
  {
    ResultColumn col = new ResultColumn(field.getUserFriendlyName());

    col.setCellValueFactory(cellData ->
    {
      HDT_Record record = cellData.getValue().getRecord();
      String text = record.getType() == hdtWork ? ((HDT_Work)record).getBibData().getStr(field) : "";
      return observableCellValue(cellData, text);
    });

    col.setVisible(false);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
