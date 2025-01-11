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

package org.hypernomicon.view.tabs;

import java.io.IOException;
import java.util.List;

import org.hypernomicon.dialogs.NewArgDlgCtrlr;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.HDT_Argument.ArgumentAuthor;
import org.hypernomicon.model.records.HDT_Position.PositionSource;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PositionVerdict;
import org.hypernomicon.view.cellValues.BibDateHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import javafx.scene.control.Hyperlink;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;
import javafx.scene.layout.HBox;

//---------------------------------------------------------------------------

public final class PositionTabCtrlr extends HyperNodeTab<HDT_Position, HDT_Position>
{
  private final HyperTable htParents, htArguments, htRightChildren;

  private HDT_Position curPosition;

//---------------------------------------------------------------------------

  public PositionTabCtrlr(Tab tab) throws IOException
  {
    super(positionTabEnum, tab);

    List<TableColumn<HyperTableRow, ?>> cols = tvLeftChildren.getColumns();

    cols.get(2).setText("Title of Work");
    cols.add(2, new TableColumn<>("Date"));
    cols.add(2, new TableColumn<>("Verdict"));
    cols.add(new TableColumn<>("Arg. Name"));

    spChildren.setDividerPositions(0.6);

    cols = tvRightChildren.getColumns();

    cols.add(1, new TableColumn<>("Sub-Position/Debate Name"));
    cols.get(2).setText("Person");

    htParents = new HyperTable(tvParents, 3, true, PREF_KEY_HT_POS_PARENTS);

    htParents.addActionCol(ctGoBtn, 3);
    htParents.addActionCol(ctBrowseBtn, 3).setTooltip(ButtonAction.baBrowse, "Select parent record from the Tree");

    RecordTypePopulator rtp = new RecordTypePopulator(hdtDebate, hdtPosition);

    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, rtp, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      RecordByTypePopulator rbtp = (RecordByTypePopulator)nextPopulator;

      RecordType parentType = HyperTableCell.getCellType(cellVal);
      rbtp.setRecordType(row, parentType);
      rbtp.setChanged(row);
      row.setCellValue(nextColNdx, "", parentType);
    });

    htParents.addColAltPopulator(hdtNone, ctDropDownList, new RecordByTypePopulator());

    htParents.addRemoveMenuItem();
    htParents.addChangeOrderMenuItem(true);

    htArguments = new HyperTable(tvLeftChildren, 3, true, PREF_KEY_HT_POS_ARG);

    htArguments.addActionCol(ctGoNewBtn, 3);
    htArguments.addLabelCol(hdtPerson);                        // Author(s) of work
    htArguments.addLabelCol(hdtPositionVerdict, smTextSimple); // True, False, etc.
    htArguments.addLabelCol(hdtArgument);                      // Date
    htArguments.addLabelCol(hdtWork, smStandard);              // Title of work
    htArguments.addLabelCol(hdtArgument);                      // Name of argument

    TableColumn<HyperTableRow, HyperTableCell> col = new TableColumn<>();
    tvRightChildren.getColumns().add(1, col);
    col.setMinWidth(25.0);
    col.setPrefWidth(45.0);
    col.setMaxWidth(45.0);

    htRightChildren = new HyperTable(tvRightChildren, 2, true, PREF_KEY_HT_POS_SUB);

    htRightChildren.addActionCol(ctGoBtn, 2);
    htRightChildren.addIconCol();
    htRightChildren.addReadOnlyColWithCustomGraphic(hdtNone, row ->
    {
      Hyperlink hLink1 = new Hyperlink("Add new position");
      hLink1.setVisited(true);
      hLink1.setOnAction(event -> newClick(hdtPosition, row));
      Hyperlink hLink2 = new Hyperlink("Add new debate");
      hLink2.setVisited(true);
      hLink2.setOnAction(event -> newClick(hdtDebate, row));

      return new HBox(hLink1, hLink2);
    });

    htRightChildren.addLabelCol(hdtPerson);

    initArgContextMenu();
    ui.initPositionContextMenu(htRightChildren);
  }

//---------------------------------------------------------------------------

  @Override protected RecordType type()                { return hdtPosition; }
  @Override protected void setRecord(HDT_Position pos) { curPosition = pos; }
  @Override protected HDT_Position getNodeRecord()     { return curPosition; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateFromRecord()
  {
    curPosition.addParentDisplayRecord();

    super.updateFromRecord();

 // Populate parent records
 // -----------------------

    htParents.buildRows(curPosition.largerPositions, (row, otherPos) ->
    {
      row.setCellValue(2, getTypeName(hdtPosition), hdtPosition);
      row.setCellValue(3, otherPos, otherPos.listName());
    });

    htParents.buildRows(curPosition.largerDebates, (row, debate) ->
    {
      row.setCellValue(2, getTypeName(hdtDebate), hdtDebate);
      row.setCellValue(3, debate, debate.listName());
    });

 // Populate arguments
 // ------------------

    htArguments.buildRows(curPosition.arguments, (row, argument) ->
    {
      HDT_Work work = null;

      if (argument.works.size() > 0)
      {
        work = argument.works.get(0);
        if (work.authorRecords.size() > 0)
          row.setCellValue(1, work.authorRecords.get(0), work.getShortAuthorsStr(true));
        else
          row.setCellValue(1, work, work.getShortAuthorsStr(true));
      }

      if (work != null)
      {
        row.setCellValue(3, new BibDateHTC(argument, work.getBibDate()));
        row.setCellValue(4, work, work.name());
      }
      else
        row.setCellValue(3, argument, "");

      HDT_PositionVerdict verdict = argument.getPosVerdict(curPosition);
      if (verdict != null)
        row.setCellValue(2, argument, verdict.listName());

      row.setCellValue(5, argument, argument.listName());
    });

 // Populate sub-positions and sub-debates
 // --------------------------------------

    htRightChildren.buildRows(curPosition.subPositions, (row, subPos) ->
    {
      row.setCellValue(1, subPos, "");
      row.setCellValue(2, subPos, subPos.getCBText());

      String authStr = Authors.getShortAuthorsStr(subPos.getPeople().stream().map(ArgumentAuthor::getAuthObj), true, true, false);
      PositionSource ps = subPos.getWorkWithAuthor();
      if (ps != null)
        row.setCellValue(3, ps.author, authStr);
      else
        row.setCellValue(3, authStr, hdtPerson);
    });

    htRightChildren.buildRows(curPosition.subDebates, (row, subDebate) ->
    {
      row.setCellValue(1, subDebate, "");
      row.setCellValue(2, subDebate, subDebate.getCBText());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initArgContextMenu()
  {
    htArguments.addDefaultMenuItems();

    htArguments.addContextMenuItem("Go to work record", HDT_Work.class,
      work -> ui.goToRecord(work, true));

    htArguments.addContextMenuItem("Go to person record", HDT_Person.class,
      person -> ui.goToRecord(person, true));

    htArguments.addContextMenuItem("Go to argument record", HDT_Argument.class,
      arg -> ui.goToRecord(arg, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    super.clear(resetRecord);

    curPosition = resetRecord ? null : HDT_Record.getCurrentInstance(curPosition);

    htParents      .clear();
    htArguments    .clear();
    htRightChildren.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord()
  {
    if (super.saveToRecord() == false)
      return false;

    if ((curPosition.setLargerPositions(htParents.saveToList(3, hdtPosition)) == false) ||
        (curPosition.setLargerDebates  (htParents.saveToList(3, hdtDebate  )) == false))
      return false;

    db.attachOrphansToRoots();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void newClick(RecordType objType, HyperTableRow row)
  {
    if (ui.cantSaveRecord()) return;

    switch (objType)
    {
      case hdtPosition :

        HDT_Position newPos = db.createNewBlankRecord(hdtPosition);
        newPos.largerPositions.add(curPosition);
        ui.goToRecord(newPos, false);
        break;

      case hdtDebate :

        HDT_Debate newDebate = db.createNewBlankRecord(hdtDebate);
        newDebate.largerPositions.add(curPosition);
        ui.goToRecord(newDebate, false);
        break;

      case hdtArgument :

        newArgumentClick(curPosition);
        break;

      default:
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void newArgumentClick(HDT_Position target)
  {
    NewArgDlgCtrlr newArgDialog = new NewArgDlgCtrlr(target);

    if (newArgDialog.showModal())
      ui.goToRecord(newArgDialog.getArgument(), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spMain, PREF_KEY_POS_TOP_VERT, 0);
    setDividerPosition(spMain, PREF_KEY_POS_BOTTOM_VERT, 1);
    setDividerPosition(spChildren, PREF_KEY_POS_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spMain, PREF_KEY_POS_TOP_VERT, 0);
    getDividerPosition(spMain, PREF_KEY_POS_BOTTOM_VERT, 1);
    getDividerPosition(spChildren, PREF_KEY_POS_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
