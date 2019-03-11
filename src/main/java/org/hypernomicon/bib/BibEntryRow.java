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

package org.hypernomicon.bib;

import java.util.List;

import org.hypernomicon.bib.BibEntryTable.*;
import org.hypernomicon.bib.lib.BibEntry;
import org.hypernomicon.model.records.HDT_Work;
import javafx.scene.control.ContextMenu;

public class BibEntryRow
{
  private final BibEntry entry;

  public BibEntryRow(BibEntry entry) { this.entry = entry; }
  public BibEntry getEntry()         { return entry; }
  public HDT_Work getWork()          { return entry.getWork(); }
  public String getURL()             { return entry.getEntryURL(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibEntryRowMenuItemSchema addCondContextMenuItem(String caption, CondBibEntryRowHandler condHandler, BibEntryRowHandler handler, List<BibEntryRowMenuItemSchema> contextMenuSchemata)
  {
    BibEntryRowMenuItemSchema mnu = new BibEntryRowMenuItemSchema(caption);
    mnu.condHandler = condHandler;
    mnu.handler = handler;

    contextMenuSchemata.add(mnu);
    return mnu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ContextMenu createContextMenu(BibEntryRow entryRow, List<BibEntryRowMenuItemSchema> contextMenuSchemata)
  {
    boolean noneVisible = true;
    ContextMenu rowMenu = new ContextMenu();

    for (BibEntryRowMenuItemSchema schema : contextMenuSchemata)
    {
      if (schema.condHandler.handle(entryRow))
      {
        BibEntryRowMenuItem newItem = new BibEntryRowMenuItem(schema.caption, schema);

        newItem.setOnAction(event ->
        {
          rowMenu.hide();
          schema.handler.handle(entryRow);
        });

        rowMenu.getItems().add(newItem);
        noneVisible = false;
      }
    }

    rowMenu.setOnShowing(event -> rowMenu.getItems().forEach(menuItem ->
    {
      BibEntryRowMenuItem rowItem = (BibEntryRowMenuItem)menuItem;
      rowItem.setVisible(rowItem.schema.visible);
      rowItem.setDisable(rowItem.schema.disabled);
    }));

    return noneVisible ? null : rowMenu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
