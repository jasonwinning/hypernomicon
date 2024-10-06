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

package org.hypernomicon.view.tabs;

import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.scene.control.Tab;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;

import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.HDT_Argument.ArgumentAuthor;
import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Position.PositionSource;
import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public final class DebateTabCtrlr extends HyperNodeTab<HDT_Debate, HDT_Debate>
{
  private final HyperTable htParents, htSubdebates, htPositions;

  private HDT_Debate curDebate;

//---------------------------------------------------------------------------

  public DebateTabCtrlr(Tab tab) throws IOException
  {
    super(debateTabEnum, tab);

    htParents = new HyperTable(tvParents, 3, true, PREF_KEY_HT_DEBATE_PARENTS);

    htParents.addActionCol(ctGoBtn, 3);
    htParents.addActionCol(ctBrowseBtn, 3);

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

    htParents.setDefaultValue(2, rtp.getChoiceByType(hdtDebate));

    htPositions = new HyperTable(tvLeftChildren, 2, true, PREF_KEY_HT_DEBATE_POS);

    htPositions.addActionCol(ctGoNewBtn, 2);
    htPositions.addLabelCol(hdtPerson);
    htPositions.addLabelCol(hdtPosition);

    htSubdebates = new HyperTable(tvRightChildren, 1, true, PREF_KEY_HT_DEBATE_SUB);

    htSubdebates.addActionCol(ctGoNewBtn, 1);
    htSubdebates.addLabelCol(hdtDebate);

    ui.initPositionContextMenu(htPositions);
  }

//---------------------------------------------------------------------------

  @Override protected RecordType type()              { return hdtDebate; }
  @Override protected void setRecord(HDT_Debate deb) { curDebate = deb; }
  @Override protected HDT_Debate getNodeRecord()     { return curDebate; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateFromRecord()
  {
    curDebate.addParentDisplayRecord();

    super.updateFromRecord();

 // Populate parent records
 // -----------------------

    htParents.buildRows(curDebate.largerDebates, (row, otherDebate) ->
    {
      row.setCellValue(2, getTypeName(hdtDebate), hdtDebate);
      row.setCellValue(3, otherDebate, otherDebate.listName());
    });

    htParents.buildRows(curDebate.largerPositions, (row, pos) ->
    {
      row.setCellValue(2, getTypeName(hdtPosition), hdtPosition);
      row.setCellValue(3, pos, pos.listName());
    });

 // -----------------------

    htSubdebates.buildRows(curDebate.subDebates, (row, subDebate) -> row.setCellValue(1, subDebate, subDebate.name()));

    htSubdebates.sortAscending(1);

    htPositions.buildRows(curDebate.subPositions, (row, position) ->
    {
      String authStr = Authors.getShortAuthorsStr(position.getPeople().stream().map(ArgumentAuthor::getAuthObj), true, true, false);
      PositionSource ps = position.getWorkWithAuthor();
      if (ps != null)
        row.setCellValue(1, ps.author, authStr);
      else
        row.setCellValue(1, authStr, hdtPerson);

      row.setCellValue(2, position, position.name());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    super.clear(resetRecord);

    curDebate = resetRecord ? null : HDT_Record.getCurrentInstance(curDebate);

    htParents   .clear();
    htPositions .clear();
    htSubdebates.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord()
  {
    if (super.saveToRecord() == false)
      return false;

    if ((curDebate.setLargerPositions(htParents.saveToList(3, hdtPosition)) == false) ||
        (curDebate.setLargerDebates  (htParents.saveToList(3, hdtDebate  )) == false))
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

        HDT_Position position = db.createNewBlankRecord(hdtPosition);
        position.largerDebates.add(curDebate);
        ui.goToRecord(position, false);
        break;

      case hdtDebate :

        HDT_Debate subDebate = db.createNewBlankRecord(hdtDebate);
        subDebate.largerDebates.add(curDebate);
        ui.goToRecord(subDebate, false);
        break;

      default:
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spMain, PREF_KEY_DEBATE_TOP_VERT, 0);
    setDividerPosition(spMain, PREF_KEY_DEBATE_BOTTOM_VERT, 1);
    setDividerPosition(spChildren, PREF_KEY_DEBATE_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spMain, PREF_KEY_DEBATE_TOP_VERT, 0);
    getDividerPosition(spMain, PREF_KEY_DEBATE_BOTTOM_VERT, 1);
    getDividerPosition(spChildren, PREF_KEY_DEBATE_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
