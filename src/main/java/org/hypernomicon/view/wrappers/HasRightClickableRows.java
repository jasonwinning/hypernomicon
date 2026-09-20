/*
 * Copyright 2015-2026 Jason Winning
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

import java.util.*;
import java.util.function.*;

import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.records.*;
import org.hypernomicon.previewWindow.PreviewWindow;

import javafx.scene.control.*;
import javafx.scene.input.ContextMenuEvent;

//---------------------------------------------------------------------------

public abstract class HasRightClickableRows<RowType extends AbstractRow<? extends HDT_Record, RowType>>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<MenuItemSchema<? extends HDT_Record, RowType>> contextMenuSchemata = new ArrayList<>();

  public final List<MenuItemSchema<? extends HDT_Record, RowType>> getContextMenuSchemata() { return Collections.unmodifiableList(contextMenuSchemata); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Makes the cell build its context menu each time one is requested, using this object's menu item schemata.
   * @param cell the table row, tree table row, or tree cell
   * @see #buildContextMenuOnRequest(Cell, Supplier, Iterable)
   */
  public final void buildContextMenuOnRequest(Cell<RowType> cell)
  {
    buildContextMenuOnRequest(cell, cell::getItem, contextMenuSchemata);
  }

  /**
   * Makes the cell build its context menu each time one is requested, so that the conditions for showing
   * each item are tested against the state at that moment, not the state when the row was populated.
   * <p>The menu is set in an event filter because filters run before handlers; the cell's built-in handler
   * then shows the menu that was just set, or nothing if no item applies.</p>
   * <p>The expand and collapse items are added only when the cell belongs to a tree. A row in a table can
   * also refer to a tree item (a folder in the File Manager's file table does), but expanding it from there
   * would be meaningless.</p>
   * @param cell the table row, tree table row, or tree cell
   * @param rowSupplier supplies the row currently shown by the cell; null if there is none
   * @param schemata the menu item schemata to build the menu from
   */
  public final void buildContextMenuOnRequest(Cell<RowType> cell, Supplier<RowType> rowSupplier, Iterable<MenuItemSchema<? extends HDT_Record, RowType>> schemata)
  {
    cell.addEventFilter(ContextMenuEvent.CONTEXT_MENU_REQUESTED, event ->
    {
      TreeItem<RowType> treeItem = null;

      if      (cell instanceof TreeTableRow<RowType> treeTableRow) treeItem = treeTableRow.getTreeItem();
      else if (cell instanceof TreeCell    <RowType> treeCell    ) treeItem = treeCell    .getTreeItem();

      cell.setContextMenu(createContextMenu(rowSupplier.get(), treeItem, schemata));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ContextMenu createContextMenu(RowType row, TreeItem<RowType> treeItem, Iterable<MenuItemSchema<? extends HDT_Record, RowType>> schemata)
  {
    if (row == null) return null;

    boolean noneVisible = true;
    ContextMenu rowMenu = new ContextMenu();

    for (MenuItemSchema<? extends HDT_Record, RowType> schema : schemata)
    {
      MenuItem newItem = createContextMenuItem(schema, row, rowMenu);
      rowMenu.getItems().add(newItem);

      if (newItem.isVisible()) noneVisible = false;
    }

    if ((treeItem != null) && (treeItem.isLeaf() == false))
    {
      noneVisible = false;

      MenuItem newItem = new MenuItem("Expand/Collapse");
      newItem.setOnAction(event -> treeItem.setExpanded(treeItem.isExpanded() == false));
      rowMenu.getItems().add(newItem);

      newItem = new MenuItem("Expand All");
      newItem.setOnAction(event -> setAllExpanded(treeItem, true));
      rowMenu.getItems().add(newItem);

      newItem = new MenuItem("Collapse All");
      newItem.setOnAction(event -> setAllExpanded(treeItem, false));
      rowMenu.getItems().add(newItem);
    }

    return noneVisible ? null : rowMenu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setAllExpanded(TreeItem<RowType> item, boolean expanded)
  {
    item.setExpanded(expanded);

    nullSwitch(item.getChildren(), children -> children.forEach(child -> setAllExpanded(child, expanded)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <HDT_T extends HDT_Record> MenuItem createContextMenuItem(MenuItemSchema<HDT_T, RowType> schema, RowType row, ContextMenu rowMenu)
  {
    MenuItem newItem = new MenuItem(schema.getCaption(row));

    newItem.setOnAction(event ->
    {
      rowMenu.hide();
      schema.doAction(row);
    });

    newItem.setVisible(schema.testWhetherToShow(row));
    newItem.setDisable(schema.disabled);
    return newItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(String captionStr, Consumer<RowType> rowHandler)
  { return addContextMenuItem(row -> captionStr, rowHandler); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(String captionStr, Predicate<RowType> condRowHandler, Consumer<RowType> rowHandler)
  { return addContextMenuItem(row -> captionStr, condRowHandler, rowHandler); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(String captionStr, Class<HDT_T> klass, Consumer<HDT_T> recordHandler)
  { return addContextMenuItem(() -> captionStr, klass, recordHandler); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(String captionStr, Class<HDT_T> klass, Predicate<HDT_T> condRecordHandler, Consumer<HDT_T> recordHandler)
  { return addContextMenuItem(() -> captionStr, klass, condRecordHandler, recordHandler); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(Supplier<String> caption, Consumer<RowType> rowHandler)
  { return addSchema(new MenuItemSchema<>(row -> caption.get(), rowHandler)); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(Supplier<String> caption, Predicate<RowType> condRowHandler, Consumer<RowType> rowHandler)
  { return addSchema(new MenuItemSchema<>(row -> caption.get(), condRowHandler, rowHandler)); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(Function<RowType, String> captionHandler, Consumer<RowType> rowHandler)
  { return addSchema(new MenuItemSchema<>(captionHandler, rowHandler)); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(Function<RowType, String> captionHandler, Predicate<RowType> condRowHandler, Consumer<RowType> rowHandler)
  { return addSchema(new MenuItemSchema<>(captionHandler, condRowHandler, rowHandler)); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(Supplier<String> caption, Class<HDT_T> klass, Consumer<HDT_T> recordHandler)
  { return addSchema(new MenuItemSchema<>(caption, klass, recordHandler)); }

  public final <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addContextMenuItem(Supplier<String> caption, Class<HDT_T> klass, Predicate<HDT_T> condRecordHandler, Consumer<HDT_T> recordHandler)
  { return addSchema(new MenuItemSchema<>(caption, klass, condRecordHandler, recordHandler)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <HDT_T extends HDT_Record> MenuItemSchema<HDT_T, RowType> addSchema(MenuItemSchema<HDT_T, RowType> schema)
  {
    contextMenuSchemata.add(schema);
    return schema;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void addDefaultMenuItems()
  {
    addContextMenuItem("Launch work", HDT_Work.class, HDT_Work::canLaunch, work -> work.launch(-1));

    addContextMenuItem("Show in Preview Window", HDT_Work.class, HDT_Work::canPreview,
                       work -> PreviewWindow.show(ui.determinePreviewContext(), work));

    addContextMenuItem("Launch", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty, workFile -> launchFile(workFile.filePath()));

    addContextMenuItem("Show in Preview Window", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
                       workFile -> PreviewWindow.show(ui.determinePreviewContext(), workFile));

    addContextMenuItem("Launch file", HDT_MiscFile.class, HDT_MiscFile::pathNotEmpty,
                       miscFile ->
                       {
                         miscFile.viewNow();
                         launchFile(miscFile.filePath());
                       });

    addContextMenuItem("Show in Preview Window", HDT_MiscFile.class, HDT_MiscFile::pathNotEmpty,
                       miscFile -> PreviewWindow.show(ui.determinePreviewContext(), miscFile));

    addContextMenuItem("Show in File Manager", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
                       workFile -> FileManager.show(workFile.filePath()));

    addContextMenuItem("Show in File Manager", HDT_MiscFile.class, HDT_MiscFile::pathNotEmpty,
                       miscFile -> FileManager.show(miscFile.filePath()));

    addContextMenuItem("Show in File Manager", HDT_Folder.class, HDT_Folder::pathNotEmpty,
                       folder -> FileManager.show(folder.filePath()));

    addContextMenuItem("Show in system explorer", HDT_MiscFile.class, HDT_MiscFile::pathNotEmpty,
                       miscFile -> highlightFileInExplorer(miscFile.filePath()));

    addContextMenuItem("Show in system explorer", HDT_WorkFile.class, HDT_WorkFile::pathNotEmpty,
                       workFile -> highlightFileInExplorer(workFile.filePath()));

    addContextMenuItem("Show in system explorer", HDT_Folder.class, HDT_Folder::pathNotEmpty,
                       folder -> highlightFileInExplorer(folder.filePath()));

    addContextMenuItem("Show folder in File Manager", HDT_Note.class, HDT_Note::pathNotEmpty,
                       note -> FileManager.show(note.filePath()));

    addContextMenuItem("Show folder in system explorer", HDT_Note.class, HDT_Note::pathNotEmpty,
                       note -> highlightFileInExplorer(note.filePath()));

    addContextMenuItem("Show nearest note ancestor's folder in File Manager", HDT_Note.class, note -> (note.pathNotEmpty() == false) && (note.getAncestorWithFolder() != null),
                       note -> FileManager.show(note.getAncestorWithFolder().filePath()));

    addContextMenuItem("Show nearest note ancestor's folder in system explorer", HDT_Note.class, note -> (note.pathNotEmpty() == false) && (note.getAncestorWithFolder() != null),
                       note -> highlightFileInExplorer(note.getAncestorWithFolder().filePath()));

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
