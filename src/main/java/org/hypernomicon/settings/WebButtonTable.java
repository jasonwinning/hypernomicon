/*
 * Copyright 2015-2020 Jason Winning
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
import java.util.prefs.BackingStoreException;
import java.util.prefs.Preferences;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.apache.commons.lang3.mutable.MutableInt;
import org.hypernomicon.util.WebButton;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import org.hypernomicon.view.wrappers.HyperTableRow;

import com.google.common.collect.Lists;

import javafx.scene.control.TableView;

public class WebButtonTable extends WebButtonCtrl
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

    List<HyperTableCell> presetCells = Lists.transform(webBtnList, webButton -> new HyperTableCell(-1, webButton.getName(), hdtNone));

    ht.addTextEditCol(hdtNone, true, false);
    ht.addColAltPopulatorWithUpdateHandler(hdtNone, HyperCtrlType.ctDropDownList, Populator.create(cvtSrchBtnPreset, presetCells),
                                           (row, cellVal, nextColNdx, nextPopulator) ->
    {
      row.setCellValue(0, -1, nullSwitch(htcToWebButton(cellVal), "", WebButton::getCaption), hdtNone);
    });

    ht.addCustomActionCol(-1, "Advanced", (row, colNdx) ->
    {
      try
      {
        EditWebButtonsDlgCtrlr dlg = EditWebButtonsDlgCtrlr.build(getWebButton(row), prefKey);

        if ((dlg.showModal() == false) || dlg.unchanged()) return;

        int nextCustomNum = 1;

        for (WebButton webButton : webBtnList)
          if (webButton.getName().startsWith(CUSTOM_NAME))
            nextCustomNum = parseInt(webButton.getName().substring(CUSTOM_NAME.length()), 1) + 1;

        WebButton webBtn = new WebButton(CUSTOM_NAME + String.valueOf(nextCustomNum), row.getText(0));

        dlg.getPatterns(webBtn);
        webBtnList.add(webBtn);
        ht.getPopulator(1).populate(row, true);
        row.setCellValue(1, -1, webBtn.getName(), hdtNone);
      }
      catch (IOException e)
      {
        showStackTrace(e);
      }
    });

    ht.addRemoveMenuItem(row -> row.getText(1).length() > 0);

    int count = appPrefs.node(PREF_KEY_WEB_BUTTONS).getInt(prefKey + "Count", defaults.size());

    for (int ndx = 1; ndx <= count; ndx++)
    {
      WebButton webBtn = ui.webButtonMap.get(prefKey + String.valueOf(ndx));

      ht.newDataRow().setCellValue(1, new HyperTableCell(-1, webBtn.getName(), hdtNone));
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
    if (cell == null) return null;

    for (WebButton webButton : webBtnList)
      if (cell.getText().equals(webButton.getName()))
        return webButton;

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void saveToPrefNode(Preferences node)
  {
    int ndx = 0, customNdx = 0;
    for (HyperTableRow row : ht.getDataRows())
    {
      WebButton webBtn = getWebButton(row);
      if (webBtn != null)
      {
        if (webBtn.getName().startsWith(CUSTOM_NAME))
          webBtn.setName(CUSTOM_NAME + String.valueOf(++customNdx));

        saveToPrefNode(node, prefKey + String.valueOf(++ndx), webBtn);
      }
    }

    while (ndx < defaults.size())
    {
      saveToPrefNode(node, prefKey + String.valueOf(ndx + 1), defaults.get(ndx));
      ndx++;
    }

    node.putInt(prefKey + "Count", ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void loadPref(Preferences node, List<WebButton> srchList, String prefKey, List<WebButton> defaults) throws BackingStoreException
  {
    MutableInt numCustom = new MutableInt(0);

    int count = node.getInt(prefKey + "Count", defaults.size());

    for (int ndx = 1; ndx <= defaults.size(); ndx++)
      ui.webButtonMap.put(prefKey + String.valueOf(ndx), defaults.get(ndx - 1));

    for (int ndx = 1; ndx <= count; ndx++)
      WebButtonBar.loadPref(node, srchList, prefKey + String.valueOf(ndx), numCustom);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
