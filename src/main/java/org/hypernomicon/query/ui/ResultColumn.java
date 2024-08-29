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
import org.hypernomicon.model.items.BibliographicDate;
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

  @SuppressWarnings({ "unchecked", "rawtypes" })
  private ResultColumn(String caption, Function<ResultCellValue, Comparable<?>> sortFunction)
  {
    super(caption);

    setSafeComparator((cell1, cell2) ->
    {
      Comparable value1 = sortFunction.apply(cell1);
      return value1.compareTo(sortFunction.apply(cell2));
    });
  }

//---------------------------------------------------------------------------

  private ResultColumn(String caption, Comparator<String> comparator)
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
    super(caption, cell -> cell.record == null ? Instant.MIN : nullSwitch(instantFunction.apply(cell.record), Instant.MIN));

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
    super("ID", cell -> cell.record == null ? -1 : cell.record.getID());

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordIDStr()));
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class RecordNameColumn extends ResultColumn { RecordNameColumn()
  {
    super("Name", cell -> makeSortKeyByType(safeStr(cell.text).trim(), hdtWork));

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordName()));
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class RecordTypeColumn extends ResultColumn { RecordTypeColumn()
  {
    super("Type", false);

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordTypeStr()));
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class SearchKeyColumn extends ResultColumn { SearchKeyColumn()
  {
    super("Search Key", false);

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

  static class BibFieldColumn extends ResultColumn { BibFieldColumn(BibFieldEnum field, boolean caseSensitive)
  {
    super(field.getUserFriendlyName(), caseSensitive);

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

    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem, boolean caseSensitive                                ) { super(captionItem.caption, caseSensitive); }
    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem, Comparator<String> comparator                        ) { super(captionItem.caption, comparator   ); }
    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem, Function<ResultCellValue, Comparable<?>> sortFunction) { super(captionItem.caption, sortFunction ); }

//---------------------------------------------------------------------------

    static NonGeneralColumn create(NonGeneralColumnGroupItem firstItem, EnumMap<RecordType, NonGeneralColumnGroupItem> recordTypeToItem)
    {
      NonGeneralColumn col = switch (firstItem.tag)
      {
        case tagTitle         -> new NonGeneralColumn(firstItem, cell -> makeSortKeyByType(safeStr(cell.text).trim(), hdtWork));
        case tagBibDate       -> new NonGeneralColumn(firstItem, cell -> (cell.record == null) || (cell.record.getType() != hdtWork) ? BibliographicDate.EMPTY_DATE : ((HDT_Work)cell.record).getBibDate());
        case tagStartPageNum,
             tagEndPageNum    -> new NonGeneralColumn(firstItem, Util::compareNumberStrings);

        default               -> new NonGeneralColumn(firstItem, false);
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
