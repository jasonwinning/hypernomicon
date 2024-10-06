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

package org.hypernomicon.settings;

import java.io.IOException;
import java.util.List;
import java.util.prefs.Preferences;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.util.WebButton;
import org.hypernomicon.view.populators.CustomPopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.RecordHTC;

import javafx.scene.control.TableView;

class WebButtonTable extends WebButtonCtrl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperTable ht;
  private final List<WebButton> defaults;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  WebButtonTable(String prefKey, List<WebButton> webBtnList, List<WebButton> defaults, TableView<HyperTableRow> tv)
  {
    super(prefKey, webBtnList);

    ht = new HyperTable(tv, 0, true, "");
    this.defaults = defaults;

    CustomPopulator pop = new CustomPopulator(cvtSrchBtnPreset, (row, force) -> webBtnList.stream().map(webButton -> new RecordHTC(webButton.getName(), hdtNone)));

    ht.addTextEditCol(hdtNone, true);
    ht.addColAltPopulatorWithUpdateHandler(hdtNone, HyperCtrlType.ctDropDownList, pop,
                                           (row, cellVal, nextColNdx, nextPopulator) ->
                                             row.setCellValue(0, nullSwitch(htcToWebButton(cellVal), "", WebButton::getCaption), hdtNone));

    ht.addCustomActionCol(-1, "Advanced", (row, colNdx) ->
    {
      try
      {
        EditWebButtonsDlgCtrlr dlg = new EditWebButtonsDlgCtrlr(getWebButton(row), prefKey);

        if ((dlg.showModal() == false) || dlg.unchanged()) return;

        int nextCustomNum = 1;

        for (WebButton webButton : webBtnList)
          if (webButton.getName().startsWith(CUSTOM_NAME))
            nextCustomNum = parseInt(webButton.getName().substring(CUSTOM_NAME.length()), 1) + 1;

        WebButton webBtn = new WebButton(CUSTOM_NAME + nextCustomNum, row.getText(0));

        dlg.getPatterns(webBtn);
        webBtnList.add(webBtn);
        ht.getPopulator(1).populate(row, true);
        row.setCellValue(1, webBtn.getName(), hdtNone);
      }
      catch (IOException e)
      {
        showStackTrace(e);
      }
    });

    ht.addRemoveMenuItem(row -> row.getText(1).length() > 0);

    int count = app.prefs.node(PREF_KEY_WEB_BUTTONS).getInt(prefKey + "Count", defaults.size());

    for (int ndx = 1; ndx <= count; ndx++)
    {
      WebButton webBtn = ui.webButtonMap.get(prefKey + ndx);

      ht.newDataRow().setCellValue(1, webBtn.getName(), hdtNone);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private WebButton getWebButton(HyperTableRow row)
  {
    HyperTableCell cell = row.getCell(1);
    if (HyperTableCell.isEmpty(cell))
      return null;

    WebButton webBtn = htcToWebButton(row.getCell(1));
    webBtn.setCaption(row.getText(0));

    return webBtn;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private WebButton htcToWebButton(HyperTableCell cell)
  {
    return cell == null ? null : findFirst(webBtnList, webButton -> HyperTableCell.getCellText(cell).equals(webButton.getName()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void saveToPrefNode(Preferences node)
  {
    int ndx = 0, customNdx = 0;
    for (HyperTableRow row : ht.dataRows())
    {
      WebButton webBtn = getWebButton(row);
      if (webBtn != null)
      {
        if (webBtn.getName().startsWith(CUSTOM_NAME))
          webBtn.setName(CUSTOM_NAME + ++customNdx);

        saveToPrefNode(node, prefKey + ++ndx, webBtn);
      }
    }

    while (ndx < defaults.size())
    {
      saveToPrefNode(node, prefKey + (ndx + 1), defaults.get(ndx));
      ndx++;
    }

    node.putInt(prefKey + "Count", ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
