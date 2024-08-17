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

import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.items.HDI_OnlinePointerMulti;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.ui.ColumnGroupItem.NonGeneralColumnGroupItem;
import org.hypernomicon.util.Util;

import javafx.scene.control.TableColumn;

//---------------------------------------------------------------------------

class ResultColumn extends TableColumn<ResultRow, ResultCellValue>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ResultColumn(String caption)
  {
    this(caption, false);
  }

//---------------------------------------------------------------------------

  private ResultColumn(String caption, boolean caseSensitive)
  {
    super(caption);

    setSafeComparator((cell1, cell2) ->
    {
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

    setSafeComparator((cell1, cell2) ->
    {
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

    setSafeComparator((cell1, cell2) ->
    {
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

    setSafeComparator((cell1, cell2) -> comparator.compare(safeStr(cell1.text).trim(), safeStr(cell2.text).trim()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setSafeComparator(Comparator<ResultCellValue> comp)
  {
    setComparator((cell1, cell2) ->
    {
      if ((cell1 == null) && (cell2 == null)) return 0;
      if (cell1 == null) return -1;
      if (cell2 == null) return 1;

      return comp.compare(cell1, cell2);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class DateColumn extends ResultColumn { DateColumn(String caption, Function<HDT_Record, Instant> instantFunction)
  {
    super (caption, (record1, record2) ->
    {
      Instant i1 = nullSwitch(instantFunction.apply(record1), Instant.MIN);
      Instant i2 = nullSwitch(instantFunction.apply(record2), Instant.MIN);
      return i1.compareTo(i2);
    });

    setCellValueFactory(cellData ->
    {
      HDT_Record record = cellData.getValue().getRecord();
      Instant i = (record == null) || (record.getType() == hdtNone) ? null : instantFunction.apply(record);

      return observableCellValue(cellData, i == null ? "" : dateTimeToUserReadableStr(i));
    });
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class RecordIDColumn extends ResultColumn { RecordIDColumn()
  {
    super("ID", Comparator.comparingInt(HDT_Record::getID));

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordIDStr()));
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class RecordNameColumn extends ResultColumn { RecordNameColumn()
  {
    super("Name", str -> makeSortKeyByType(str, hdtWork));

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordName()));
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class RecordTypeColumn extends ResultColumn { RecordTypeColumn()
  {
    super("Type");

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordTypeStr()));
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class SearchKeyColumn extends ResultColumn { SearchKeyColumn()
  {
    super("Search Key");

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getSearchKey()));
    setVisible(false);
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class SortKeyColumn extends ResultColumn { SortKeyColumn()
  {
    super("Sort Key", true);

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getSortKey()));
    setVisible(false);
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class BibFieldColumn extends ResultColumn { BibFieldColumn(BibFieldEnum field)
  {
    super(field.getUserFriendlyName());

    setCellValueFactory(cellData ->
    {
      HDT_Record record = cellData.getValue().getRecord();
      String text = record.getType() == hdtWork ? ((HDT_Work)record).getBibData().getStr(field) : "";
      return observableCellValue(cellData, text);
    });

    setVisible(false);
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final class NonGeneralColumn extends ResultColumn
  {
    final EnumMap <RecordType, NonGeneralColumnGroupItem> map = new EnumMap<>(RecordType.class);

//---------------------------------------------------------------------------

    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem                                                    ) { super(captionItem.caption                        ); }
    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem, Function<String, String> sortKeyFunction          ) { super(captionItem.caption, sortKeyFunction       ); }
    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem, Comparator<String> comparator, Class<String> klass) { super(captionItem.caption, comparator     , klass); }

//---------------------------------------------------------------------------

    static NonGeneralColumn create(NonGeneralColumnGroupItem firstItem, EnumMap<RecordType, NonGeneralColumnGroupItem> recordTypeToItem)
    {
      NonGeneralColumn col = switch (firstItem.tag)
      {
        case tagTitle         -> new NonGeneralColumn(firstItem, str -> makeSortKeyByType(str, hdtWork));
        case tagYear          -> new NonGeneralColumn(firstItem, Util::compareYears, String.class);
        case tagStartPageNum,
             tagEndPageNum    -> new NonGeneralColumn(firstItem, Util::compareNumberStrings, String.class);

        default               -> new NonGeneralColumn(firstItem);
      };

      // Only subject columns have a relType set. They are invisible by default.
      col.setVisible(recordTypeToItem.values().stream().anyMatch(item -> item.relType == RelationType.rtNone));

      col.setCellValueFactory(cellData ->
      {
        String str = "";
        HDT_Record record = cellData.getValue().getRecord();

        if (record != null)
        {
          NonGeneralColumnGroupItem item = recordTypeToItem.get(record.getType());

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
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
