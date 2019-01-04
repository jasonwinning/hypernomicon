/*
 * Copyright 2015-2019 Jason Winning
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

import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Position.PositionSource;

//---------------------------------------------------------------------------

public class DebateTab extends HyperNodeTab<HDT_Debate, HDT_Debate>
{
  private HyperTable htParents, htSubdebates, htPositions;
  private HDT_Debate curDebate;

  @Override public HDT_RecordType getType()         { return hdtDebate; }
  @Override public void enable(boolean enabled)     { ui.tabDebates.getContent().setDisable(enabled == false); }
  @Override public void focusOnSearchKey()          { ctrlr.focusOnSearchKey(); }
  @Override public void findWithinDesc(String text) { ctrlr.hilite(text); }
  @Override public TextViewInfo getMainTextInfo()   { return ctrlr.getMainTextInfo(); }
  @Override public void setRecord(HDT_Debate deb)   { curDebate = deb; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean update()
  {
    curDebate.addParentDisplayRecord();

    ctrlr.update(curDebate);

    htParents.buildRows(curDebate.largerDebates, (row, otherDebate) -> row.setCellValue(2, otherDebate, otherDebate.name()));

    htSubdebates.buildRows(curDebate.subDebates, (row, subDebate) -> row.setCellValue(1, subDebate, subDebate.name()));

    htPositions.buildRows(curDebate.positions, (row, position) ->
    {
      PositionSource ps = position.getWorkWithAuthor();

      if (ps != null)
        row.setCellValue(1, ps.author, Authors.getShortAuthorsStr(position.getPeople(), true, true));
      else
        row.setCellValue(1, -1, Authors.getShortAuthorsStr(position.getPeople(), true, true), hdtPerson);

      row.setCellValue(2, position, position.name());
    });

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void init(TabEnum tabEnum)
  {
    this.tabEnum = tabEnum;
    ctrlr.init(hdtDebate, this);
    ctrlr.tvParents.getColumns().remove(2);
    ctrlr.tvParents.getColumns().get(2).setText("Larger Debate Name");

    htParents = new HyperTable(ctrlr.tvParents, 2, true, PREF_KEY_HT_DEBATE_PARENTS);

    htParents.addActionCol(ctGoBtn, 2);
    htParents.addActionCol(ctBrowseBtn, 2);
    htParents.addCol(hdtDebate, ctDropDownList);

    htParents.addRemoveMenuItem();
    htParents.addChangeOrderMenuItem(true);

    htPositions = new HyperTable(ctrlr.tvLeftChildren, 2, true, PREF_KEY_HT_DEBATE_POS);

    htPositions.addActionCol(ctGoNewBtn, 2);
    htPositions.addCol(hdtPerson, ctNone);
    htPositions.addCol(hdtPosition, ctNone);

    htSubdebates = new HyperTable(ctrlr.tvRightChildren, 1, true, PREF_KEY_HT_DEBATE_SUB);

    htSubdebates.addActionCol(ctGoNewBtn, 1);
    htSubdebates.addCol(hdtDebate, ctNone);

    ui.initPositionContextMenu(htPositions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    ctrlr.clear();

    htParents.clear();
    htPositions.clear();
    htSubdebates.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord(boolean showMessage)
  {
    if (!ctrlr.save(curDebate, showMessage, this)) return false;

    curDebate.setLargerDebates(htParents.saveToList(2, hdtDebate));

    ui.attachOrphansToRoots();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void newClick(HDT_RecordType objType, HyperTableRow row)
  {
    if (ui.cantSaveRecord(true)) return;

    switch (objType)
    {
      case hdtPosition :

        HDT_Position position = db.createNewBlankRecord(hdtPosition);

        position.debates.add(curDebate);

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
    setDividerPosition(ctrlr.spMain, PREF_KEY_DEBATE_TOP_VERT, 0);
    setDividerPosition(ctrlr.spMain, PREF_KEY_DEBATE_BOTTOM_VERT, 1);
    setDividerPosition(ctrlr.spChildren, PREF_KEY_DEBATE_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(ctrlr.spMain, PREF_KEY_DEBATE_TOP_VERT, 0);
    getDividerPosition(ctrlr.spMain, PREF_KEY_DEBATE_BOTTOM_VERT, 1);
    getDividerPosition(ctrlr.spChildren, PREF_KEY_DEBATE_BOTTOM_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
