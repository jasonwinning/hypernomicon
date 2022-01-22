/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Predicate;

import org.hypernomicon.model.records.*;
import org.hypernomicon.previewWindow.PreviewWindow.PreviewSource;

import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeItem;

public abstract class HasRightClickableRows<RowType extends AbstractRow<? extends HDT_Record, RowType>>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final private class RowMenuItem extends MenuItem
  {
    RowMenuItem(MenuItemSchema<? extends HDT_Record, RowType> schema)
    {
      super(schema.getCaption());
      this.schema = schema;
    }

    final private MenuItemSchema<? extends HDT_Record, RowType> schema;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final private List<MenuItemSchema<? extends HDT_Record, RowType>> contextMenuSchemata = new ArrayList<>();

  final public List<MenuItemSchema<? extends HDT_Record, RowType>> getContextMenuSchemata() { return Collections.unmodifiableList(contextMenuSchemata); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final protected ContextMenu createContextMenu(RowType row)
  {
    return createContextMenu(row, contextMenuSchemata);
  }

  final public ContextMenu createContextMenu(RowType row, List<MenuItemSchema<? extends HDT_Record, RowType>> schemata)
  {
    if (row == null) return null;

    boolean noneVisible = true;
    ContextMenu rowMenu = new ContextMenu();

    for (MenuItemSchema<? extends HDT_Record, RowType> schema : schemata)
    {
      RowMenuItem newItem = createContextMenuItem(schema, row, rowMenu);
      rowMenu.getItems().add(newItem);

      if (newItem.isVisible()) noneVisible = false;
    }

    TreeItem<RowType> treeItem = row.getTreeItem();

    if ((treeItem != null) && (treeItem.isLeaf() == false))
    {
      noneVisible = false;

      MenuItem newItem = new MenuItem("Expand/Collapse");
      newItem.setOnAction(event -> treeItem.setExpanded(!treeItem.isExpanded()));
      rowMenu.getItems().add(newItem);

      newItem = new MenuItem("Expand All");
      newItem.setOnAction(event -> setAllExpanded(treeItem, true));
      rowMenu.getItems().add(newItem);

      newItem = new MenuItem("Collapse All");
      newItem.setOnAction(event -> setAllExpanded(treeItem, false));
      rowMenu.getItems().add(newItem);
    }

    rowMenu.setOnShowing(event -> rowMenu.getItems().forEach(menuItem -> { if (menuItem instanceof HasRightClickableRows.RowMenuItem)
    {
      @SuppressWarnings("unchecked")
      RowMenuItem rowItem = (RowMenuItem)menuItem;

      rowItem.setDisable(rowItem.schema.disabled);
    }}));

    return noneVisible ? null : rowMenu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final private void setAllExpanded(TreeItem<RowType> item, boolean expanded)
  {
    item.setExpanded(expanded);

    nullSwitch(item.getChildren(), children -> children.forEach(child -> setAllExpanded(child, expanded)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final private <HDT_T extends HDT_Record> RowMenuItem createContextMenuItem(MenuItemSchema<HDT_T, RowType> schema, RowType row, ContextMenu rowMenu)
  {
    RowMenuItem newItem = new RowMenuItem(schema);

    newItem.setOnAction(event ->
    {
      rowMenu.hide();
      schema.doAction(row);
    });

    newItem.setVisible(schema.testWhetherToShow(row));
    return newItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final public <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(String caption, Consumer<RowType> rowHandler)
  { return addSchema(new MenuItemSchema<>(caption, rowHandler)); }

  final public <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(String caption, Predicate<RowType> condRowHandler, Consumer<RowType> rowHandler)
  { return addSchema(new MenuItemSchema<>(caption, condRowHandler, rowHandler)); }

  final public <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(String caption, Class<HDT_T> klass, Consumer<HDT_T> recordHandler)
  { return addSchema(new MenuItemSchema<>(caption, klass, recordHandler)); }

  final public <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(String caption, Class<HDT_T> klass, Predicate<HDT_T> condRecordHandler, Consumer<HDT_T> recordHandler)
  { return addSchema(new MenuItemSchema<>(caption, klass, condRecordHandler, recordHandler)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final private <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addSchema(MenuItemSchema<HDT_T, RowType> schema)
  {
    contextMenuSchemata.add(schema);
    return schema;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final public void addDefaultMenuItems()
  {
    addContextMenuItem("Launch work", HDT_Work.class, HDT_Work::canLaunch, work -> work.launch(-1));

    addContextMenuItem("Show in Preview Window", HDT_Work.class, HDT_Work::canPreview,
                       work ->
                       {
                         PreviewSource src = ui.determinePreviewContext();
                         previewWindow.setPreview(src, work.previewFilePath(), work.getStartPageNum(), work.getEndPageNum(), work);
                         ui.openPreviewWindow(src);
                       });

    addContextMenuItem("Launch", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty, workFile -> launchFile(workFile.filePath()));

    addContextMenuItem("Show in Preview Window", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
                       workFile ->
                       {
                         PreviewSource src = ui.determinePreviewContext();
                         previewWindow.setPreview(src, workFile.filePath(), workFile);
                         ui.openPreviewWindow(src);
                       });

    addContextMenuItem("Launch file", HDT_MiscFile.class, HDT_MiscFile::pathNotEmpty,
                       miscFile ->
                       {
                         miscFile.viewNow();
                         launchFile(miscFile.filePath());
                       });

    addContextMenuItem("Show in Preview Window", HDT_MiscFile.class, HDT_MiscFile::pathNotEmpty,
                       miscFile ->
                       {
                         PreviewSource src = ui.determinePreviewContext();
                         previewWindow.setPreview(src, miscFile.filePath(), miscFile);
                         ui.openPreviewWindow(src);
                       });

    addContextMenuItem("Show in File Manager", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
                       workFile -> ui.goToFileInManager(workFile.filePath()));

    addContextMenuItem("Show in File Manager", HDT_MiscFile.class, HDT_MiscFile::pathNotEmpty,
                       miscFile -> ui.goToFileInManager(miscFile.filePath()));

    addContextMenuItem("Show in File Manager", HDT_Folder.class, HDT_Folder::pathNotEmpty,
                       folder -> ui.goToFileInManager(folder.filePath()));

    addContextMenuItem("Show in system explorer", HDT_MiscFile.class, HDT_MiscFile::pathNotEmpty,
                       miscFile -> highlightFileInExplorer(miscFile.filePath()));

    addContextMenuItem("Show in system explorer", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
                       workFile -> highlightFileInExplorer(workFile.filePath()));

    addContextMenuItem("Show in system explorer", HDT_Folder.class, HDT_Folder::pathNotEmpty,
                       folder -> highlightFileInExplorer(folder.filePath()));

    addContextMenuItem("Show folder in File Manager", HDT_Note.class, HDT_Note::pathNotEmpty,
                       note -> ui.goToFileInManager(note.filePath()));

    addContextMenuItem("Show folder in system explorer", HDT_Note.class, HDT_Note::pathNotEmpty,
                       note -> highlightFileInExplorer(note.filePath()));

    addContextMenuItem("Assign to note record", HDT_Folder.class, HDT_Folder::pathNotEmpty,
                       folder ->
                       {
                         ui.treeSelector.reset(folder, false);
                         ui.treeSelector.addTargetType(hdtNote);
                         ui.goToTreeRecord(folder.closestAncestorNote());
                       });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
