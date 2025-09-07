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

package org.hypernomicon.query.ui;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.HDT_RecordBase.makeSortKeyByType;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.query.ui.ResultCellValue.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.time.Instant;
import java.util.*;
import java.util.function.Function;

import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.HDI_OnlinePointerMulti;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.ui.ColumnGroupItem.NonGeneralColumnGroupItem;
import org.hypernomicon.util.Util;

import javafx.scene.control.TableColumn;

//---------------------------------------------------------------------------

class ResultColumn extends TableColumn<ResultRow, ResultCellValue>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final double COL_MAX_WIDTH = 1200.0,
                              COL_DEFAULT_WIDTH = 80.0,
                              ID_COL_WIDTH  = 55.0,
                              RECORD_NAME_COL_WIDTH = 230.0;

  ResultColumn countCol;

  private ResultColumn(String caption, boolean caseSensitive)
  {
    this(caption, (cell1, cell2) ->
      caseSensitive ?
        stripSafe(cell1.text).compareTo          (stripSafe(cell2.text))  :
        stripSafe(cell1.text).compareToIgnoreCase(stripSafe(cell2.text)));
  }

//---------------------------------------------------------------------------

  @SuppressWarnings({ "unchecked", "rawtypes" })
  private ResultColumn(String caption, Function<ResultCellValue, Comparable<?>> sortFunction)
  {
    this(caption, (cell1, cell2) ->
    {
      Comparable value1 = sortFunction.apply(cell1);
      return value1.compareTo(sortFunction.apply(cell2));
    });
  }

//---------------------------------------------------------------------------

  private ResultColumn(String caption, Comparator<String> comparator, Object unused)
  {
    this(caption, (cell1, cell2) -> comparator.compare(stripSafe(cell1.text), stripSafe(cell2.text)));

    // Assert that the unused parameter is the expected constant
    assert unused == UNUSED : "The unused parameter must be the UNUSED constant";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ResultColumn(String caption, Comparator<ResultCellValue> comp)
  {
    super(caption);

    setPrefWidth(scalePropertyValueForDPI(COL_DEFAULT_WIDTH));
    setMaxWidth(COL_MAX_WIDTH);

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

  static class CountColumn extends ResultColumn
  {
    CountColumn(ResultColumn targetCol)
    {
      super(targetCol.getText() + " Count", Util::compareNumberStrings, UNUSED);

      setVisible(false);

      setCellValueFactory(cellData ->
      {
        int count = -1;
        HDT_Record record = cellData.getValue().getRecord();

        if (record != null)
        {
          if (targetCol instanceof NonGeneralColumn ngTargetCol)
          {
            NonGeneralColumnGroupItem item = ngTargetCol.map.get(record.getType());

            if (item != null)
            {
              count = item.relType != rtNone ?
                db.getSubjectList(item.relType, record).size()
              :
                record.resultCount(item.tag);
            }
          }
          else if (targetCol instanceof BibFieldColumn bfTargetCol)
          {
            count = record.getType() == hdtWork ? ((HDT_Work)record).getBibData().getValueCount(bfTargetCol.field) : 0;
          }
        }

        return observableCellValue(cellData, count < 0 ? "" : String.valueOf(count));
      });
    }
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
    super("ID", cell -> HDT_Record.getIDSafe(cell.record));

    setPrefWidth(scalePropertyValueForDPI(ID_COL_WIDTH));

    setCellValueFactory(cellData -> observableCellValue(cellData, cellData.getValue().getRecordIDStr()));
  }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class RecordNameColumn extends ResultColumn { RecordNameColumn()
  {
    super("Name", cell -> makeSortKeyByType(stripSafe(cell.text), hdtWork));

    setPrefWidth(scalePropertyValueForDPI(RECORD_NAME_COL_WIDTH));

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

  static class BibFieldColumn extends ResultColumn
  {
    final BibFieldEnum field;

    BibFieldColumn(BibFieldEnum field, boolean caseSensitive, EnumSet<BibFieldEnum> bibFieldsToShow)
    {
      super(field.getUserFriendlyName(), caseSensitive);

      this.field = field;

      setCellValueFactory(cellData ->
      {
        HDT_Record record = cellData.getValue().getRecord();
        String text = record.getType() == hdtWork ? ((HDT_Work)record).getBibData().getStr(field) : "";
        return observableCellValue(cellData, text);
      });

      setVisible(bibFieldsToShow.contains(field));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final class NonGeneralColumn extends ResultColumn
  {
    final EnumMap <RecordType, NonGeneralColumnGroupItem> map = new EnumMap<>(RecordType.class);

//---------------------------------------------------------------------------

    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem, boolean caseSensitive                                ) { super(captionItem.caption, caseSensitive        ); }
    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem, Comparator<String> comparator                        ) { super(captionItem.caption, comparator   , UNUSED); }
    private NonGeneralColumn(NonGeneralColumnGroupItem captionItem, Function<ResultCellValue, Comparable<?>> sortFunction) { super(captionItem.caption, sortFunction         ); }

//---------------------------------------------------------------------------

    static NonGeneralColumn create(NonGeneralColumnGroupItem firstItem, EnumMap<RecordType, NonGeneralColumnGroupItem> recordTypeToItem, EnumSet<RelationType> relationsToShow)
    {
      NonGeneralColumn col = switch (firstItem.tag)
      {
        case tagTitle         -> new NonGeneralColumn(firstItem, cell -> makeSortKeyByType(stripSafe(cell.text), hdtWork));
        case tagBibDate       -> new NonGeneralColumn(firstItem, cell -> (cell.record == null) || (cell.record.getType() != hdtWork) ? BibliographicDate.EMPTY_DATE : ((HDT_Work)cell.record).getBibDate());
        case tagStartPageNum,
             tagEndPageNum    -> new NonGeneralColumn(firstItem, Util::compareNumberStrings);

        default               -> new NonGeneralColumn(firstItem, false);
      };

      col.map.putAll(recordTypeToItem);

      // Only subject columns have a relType set. They are invisible by default.
      col.setVisible(recordTypeToItem.values().stream().anyMatch(item -> (item.relType == rtNone) || relationsToShow.contains(item.relType)));

      col.setCellValueFactory(cellData ->
      {
        String str = "";
        HDT_Record record = cellData.getValue().getRecord();

        if (record != null)
        {
          NonGeneralColumnGroupItem item = col.map.get(record.getType());

          if (item != null)
          {
            str = item.relType != rtNone ?
              HDI_OnlinePointerMulti.recordStreamResultText(db.getObjType(item.relType), db.getSubjectList(item.relType, record).stream(), true)
            :
              record.resultTextForTag(item.tag, true);
          }
        }

        return observableCellValue(cellData, str);
      });

      return col;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
