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

package org.hypernomicon.query.reports;

import static org.hypernomicon.App.*;
import static org.hypernomicon.dialogs.NewPersonDlgCtrlr.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.RecordHTC;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableView;

public class DupAuthorsReportEngine extends ReportEngine
{
  private final List<HyperTableRow> rows = new ArrayList<>();
  private final Map<HyperTableRow, ImmutableSet<Author>> rowToMatch = new HashMap<>();
  private HyperTable ht;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableRow> getRows()     { return rows; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void generate(HyperTask task, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws CancelledTaskException
  {
    rows.clear();
    rowToMatch.clear();

    if (db.isLoaded() == false) return;

    Map<Author, List<Author>> matchMap = new LinkedHashMap<>();
    LinkedList<PersonForDupCheck> list = createListForDupCheck();

    PersonForDupCheck person = list.poll();

    int ndx = 0, total = list.size();
    total = total * (total + 1) / 2;

    while (list.size() > 0)
    {
      List<Author> matchedAuthors = new ArrayList<>();
      doDupCheck(person, list, matchedAuthors, task, ndx, total);

      if (matchedAuthors.size() > 0)
        matchMap.put(person.getAuthor(), matchedAuthors);

      ndx = ndx + list.size();

      person = list.poll();
    }

    rows.clear();
    rowToMatch.clear();

    matchMap.forEach((author, authorList) -> authorList.forEach(match ->
    {
      ObservableList<HyperTableCell> cells = FXCollections.observableArrayList(HyperTableCell.blankCell);

      cells.add(author.getPerson() == null ?
        new RecordHTC(author.getNameLastFirst(false), hdtNone)
      :
        new RecordHTC(author.getPerson(), author.getNameLastFirst(false)));

      cells.add(getWorkCell(author));

      cells.add(match.getPerson() == null ?
        new RecordHTC(match.getNameLastFirst(false), hdtNone)
      :
        new RecordHTC(match.getPerson(), match.getNameLastFirst(false)));

      cells.add(getWorkCell(match));

      HyperTableRow row = new HyperTableRow(cells, ht);

      rows.add(row);

      rowToMatch.put(row, ImmutableSet.of(author, match));
    }));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HyperTableCell getWorkCell(Author author)
  {
    return nullSwitch(author.getWork(), new RecordHTC("", hdtWork), work -> new RecordHTC(work, work.getCBText()));
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
      ImmutableList<Author> pair = rowToMatch.get(row).asList();

      Author author1, author2;

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

      NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(author1.getName(), null, false, author1.getPerson(), author1, Lists.newArrayList(author2));

      if (npdc.showModal() == false) return;

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
