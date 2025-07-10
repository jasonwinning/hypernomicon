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

package org.hypernomicon.query.reports;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.util.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.authors.RecordAuthor;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.query.personMatch.PersonForDupCheck;
import org.hypernomicon.query.personMatch.PersonMatcher;
import org.hypernomicon.view.cellValues.*;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import com.google.common.collect.*;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableView;

//---------------------------------------------------------------------------

public class DupAuthorsReportEngine extends ReportEngine
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<HyperTableRow> rows = new ArrayList<>();
  private final Map<HyperTableRow, ImmutableSet<RecordAuthor>> rowToMatch = new HashMap<>();
  private HyperTable ht;

//---------------------------------------------------------------------------

  @Override public List<HyperTableRow> getRows()     { return rows; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void generate(HyperTask task, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws CancelledTaskException
  {
    rows.clear();
    rowToMatch.clear();

    if (db.isLoaded() == false) return;

    LinkedList<PersonForDupCheck> list = PersonMatcher.createListForDupCheck();

    task.totalCount = ((long) list.size()) * ((long) (list.size() + 1)) / 2;

    PersonMatcher matcher = new PersonMatcher();

    while (list.isEmpty() == false)
      matcher.doDupCheck(list.poll(), list, task);

    rows.clear();
    rowToMatch.clear();

    matcher.forEachMatch((personForDupCheck, match) ->
    {
      ObservableList<HyperTableCell> cells = FXCollections.observableArrayList(GenericNonRecordHTC.blankCell);

      RecordAuthor author = personForDupCheck.getAuthor();

      cells.add(author.getPerson() == null ?
        new GenericNonRecordHTC(author.nameLastFirst(false), hdtNone)
      :
        new RecordHTC(author.getPerson(), author.nameLastFirst(false)));

      cells.add(getWorkCell(author));

      cells.add(match.getPerson() == null ?
        new GenericNonRecordHTC(match.nameLastFirst(false), hdtNone)
      :
        new RecordHTC(match.getPerson(), match.nameLastFirst(false)));

      cells.add(getWorkCell(match));

      HyperTableRow row = new HyperTableRow(cells, ht);

      rows.add(row);

      rowToMatch.put(row, ImmutableSet.of(author, match));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HyperTableCell getWorkCell(RecordAuthor author)
  {
    return nullSwitch(author.getWork(), new GenericNonRecordHTC("", hdtWork), work -> new RecordHTC(work, work.getCBText()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override HyperTable prepTable(TableView<HyperTableRow> tv)
  {
    this.tv = tv;

    addCol("", 100).setSortable(false);

    addCol("Name 1", 200);
    addCol("Work 1", 500);
    addCol("Name 2", 200);
    addCol("Work 2", 500);

    ht = new HyperTable(tv, -1, false, "");

    ht.addCustomActionCol(-1, "Merge", (row, colNdx) ->
    {
      ImmutableList<RecordAuthor> pair = rowToMatch.get(row).asList();

      RecordAuthor author1, author2;

      if ((pair.get(0).getPerson() == null) && (pair.get(1).getPerson() != null))
      {
        author1 = pair.get(1);
        author2 = pair.get(0);
      }
      else
      {
        author1 = pair.get(0);
        author2 = pair.get(1);
      }

      if (author1.outOfDate() || author2.outOfDate())
      {
        warningPopup("The data in this row is out of date.");
        return;
      }

      NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(author1.getName(), null, author1, List.of(author2));

      if (npdc.showModal() == false) return;

      if (npdc.updateWithoutCreateWasSelected())
      {
        HDT_Work work = author1.getWork();

        work.getAuthors().update(author1, new RecordAuthor(work, npdc.getName(), author1.getIsEditor(), author1.getIsTrans(), author1.getInFileName()));
      }

      ui.queryHyperTab().btnExecuteClick();
    });

    ht.addLabelCol(hdtNone);
    ht.addLabelCol(hdtWork, smTextSimple);
    ht.addLabelCol(hdtNone);
    ht.addLabelCol(hdtWork, smTextSimple);

    return ht;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
