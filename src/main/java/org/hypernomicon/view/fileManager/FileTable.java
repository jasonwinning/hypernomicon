/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.view.fileManager;

import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.noOp;

import java.io.IOException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.hypernomicon.App;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.fileManager.FileManager.MarkedRowInfo;
import org.hypernomicon.view.wrappers.DragNDropHoverHelper;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.DragNDropHoverHelper.DragNDropContainer;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TreeItem;
import javafx.scene.input.DragEvent;
import javafx.scene.text.Text;

public class FileTable implements DragNDropContainer<FileRow>
{
  @FunctionalInterface public interface FileRowHandler     { public abstract void handle(FileRow fileRow); }
  @FunctionalInterface public interface CondFileRowHandler { public abstract boolean handle(FileRow fileRow); }
  
//---------------------------------------------------------------------------
  
  public static class FileRowMenuItemSchema
  {
    public CondFileRowHandler condHandler;
    public FileRowHandler handler;
    public String caption;
    public boolean visible = true;
    public boolean disabled = false;
    
    public FileRowMenuItemSchema(String caption) { this.caption = caption; }
  }
  
//---------------------------------------------------------------------------
  
  public static class FileRowMenuItem extends MenuItem
  {
    public FileRowMenuItem(String caption, FileRowMenuItemSchema schema)
    {
      super(caption);
      this.schema = schema;
    }

    public FileRowMenuItemSchema schema;
  }
  
//---------------------------------------------------------------------------
  
  public static class FileCellValue<Comp_T extends Comparable<Comp_T>> implements Comparable<FileCellValue<Comp_T>>
  {
    public String text;
    public Comparable<Comp_T> sortVal;
    
    public FileCellValue(String text, Comparable<Comp_T> sortVal)
    {
      this.text = text;
      this.sortVal = sortVal;
    }
    
    @Override public String toString() { return text; }

    @SuppressWarnings("unchecked")
    @Override public int compareTo(FileCellValue<Comp_T> other)
    {
      return sortVal.compareTo((Comp_T) other.sortVal);
    }    
  }
  
//---------------------------------------------------------------------------
  
  private TableView<FileRow> fileTV;
  private ObservableList<FileRow> rows;
  List<FileRowMenuItemSchema> contextMenuSchemata;
  List<MarkedRowInfo> draggingRows;
  private DragNDropHoverHelper<FileRow> ddHoverHelper = new DragNDropHoverHelper<>();

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
  @SuppressWarnings("unchecked")
  public FileTable(TableView<FileRow> fileTV, String prefID)
  {
    this.fileTV = fileTV;
    rows = FXCollections.observableArrayList();
    contextMenuSchemata = new ArrayList<>();
    
    if (prefID.length() > 0)
      HyperTable.registerTable(fileTV, prefID);
    
    fileTV.setItems(rows);
    fileTV.setPlaceholder(new Text("This folder is empty."));
       
    fileTV.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
    
    TableColumn<FileRow, FileRow> nameCol = (TableColumn<FileRow, FileRow>) fileTV.getColumns().get(0);
    TableColumn<FileRow, FileCellValue<Instant>> modDateCol = (TableColumn<FileRow, FileCellValue<Instant>>) fileTV.getColumns().get(1);
    TableColumn<FileRow, String> typeCol = (TableColumn<FileRow, String>) fileTV.getColumns().get(2);
    TableColumn<FileRow, FileCellValue<Long>> sizeCol = (TableColumn<FileRow, FileCellValue<Long>>) fileTV.getColumns().get(3);
    TableColumn<FileRow, String> recordsCol = (TableColumn<FileRow, String>) fileTV.getColumns().get(4);
    
    nameCol.setCellValueFactory(cellData -> new SimpleObjectProperty<FileRow>(cellData.getValue()));
    nameCol.setComparator((v1, v2) -> v1.getFileName().compareToIgnoreCase(v2.getFileName()));
    
    modDateCol.setCellValueFactory(cellData -> new SimpleObjectProperty<FileCellValue<Instant>>(cellData.getValue().getModifiedDateCellValue()));
    
    typeCol.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getTypeString()));
    sizeCol.setCellValueFactory(cellData -> new SimpleObjectProperty<FileCellValue<Long>>(cellData.getValue().getSizeCellValue()));
    sizeCol.setStyle( "-fx-alignment: CENTER-RIGHT;");
    
    recordsCol.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getHyperPath().getRecordsString()));
    
    nameCol.setCellFactory(col ->
    {
      TableCell<FileRow, FileRow> cell = new TableCell<FileRow, FileRow>()
      {
        @Override public void updateItem(FileRow item, boolean empty) 
        {
          super.updateItem(item, empty);
  
          if (empty)
          {
            setText(null);
            setGraphic(null);
          }
          else
          {
            if (item == null)
            {
              setText(null);
              setGraphic(null);             
            }
            else
            {
              setText(item.getFileName());
              setGraphic(item.getGraphic());
            }
          }
        }
      };
         
      return cell;
    });     
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public void clear()
  {
    rows.clear();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void update(HDT_Folder folder, TreeItem<FileRow> parentTreeItem)
  {
    clear();
    int nextDirNdx = 0;
        
    HyperPath hyperPath = null;
    FilePath hFilePath = null;
    Path path = null;
    
    if (folder != null)
    {
      hyperPath = folder.getPath();
      if (hyperPath != null)
      {
        hFilePath = hyperPath.getFilePath();
        if (hFilePath != null)
          path = hFilePath.toPath();
      }
    }
    
    if (path == null)
    {
      System.out.println("***********************************");
      System.out.println("File Manager Directory Stream Error");
      System.out.println("***********************************");
      Thread.dumpStack();
      return;
    }
    
    try (DirectoryStream<Path> stream = Files.newDirectoryStream(path, "**"))
    {
      for (Path entry: stream)
      {
        FilePath filePath = new FilePath(entry);
        
        Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);
        
        if (set.size() > 0)
        {
          FileRow row = new FileRow(set.iterator().next(), null);
                  
          if (filePath.isDirectory())
          {          
            rows.add(nextDirNdx, row);
            
            for (TreeItem<FileRow> childTreeItem : parentTreeItem.getChildren())
            {
              FileRow fileRow = childTreeItem.getValue();
              if (fileRow.getRecord().getID() > 0)  // a deleted folder might still be in the tree at this point; the delete recordHandler gets called
                                                    // in a Platform.runLater call
              {
                FilePath rowPath = fileRow.getFilePath();
                
                if (rowPath.equals(filePath))
                {
                  row.setFolderTreeItem(childTreeItem);
                  break;
                }
              }
            }
            
            nextDirNdx++;
          }
          else
            rows.add(row);
        }
        else
        {
          FileRow row = new FileRow(new HyperPath(filePath), null);
          rows.add(row);
        }
      }
    }
    catch (DirectoryIteratorException | IOException ex) { noOp(); }
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
  
  public FileRowMenuItemSchema addContextMenuItem(String caption, FileRowHandler handler)
  {
    return FileRow.addCondContextMenuItem(caption, fileRow -> true, handler, contextMenuSchemata);
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 

  public FileRowMenuItemSchema addCondContextMenuItem(String caption, CondFileRowHandler condHandler, FileRowHandler handler)
  {
    return FileRow.addCondContextMenuItem(caption, condHandler, handler, contextMenuSchemata);
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 

  public void selectByFileName(FilePath fileName)
  {
    FilePath nameOnly = fileName.getNameOnly();
    
    for (FileRow row : rows)
    {
      if (row.getFilePath().getNameOnly().equals(nameOnly))
      {
        fileTV.getSelectionModel().select(row);
        fileTV.scrollTo(row);
        return;
      }
    }
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 

  @Override public void startDrag(FileRow fileRow)
  {
    draggingRows = App.fileManagerDlg.getMarkedRows(fileRow); 
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public void startDragFromFolderTree(FileRow fileRow)
  {
    draggingRows = new ArrayList<>();
    draggingRows.add(new MarkedRowInfo(fileRow, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  @Override public void dragDone()
  {
    draggingRows = null;
    ddHoverHelper.reset();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  @Override public boolean acceptDrag(FileRow targetRow, DragEvent dragEvent, TreeItem<FileRow> treeItem)
  {
    ddHoverHelper.scroll(dragEvent, fileTV);
    
    if (draggingRows == null) return false;
    if (targetRow == null)
      targetRow = App.fileManagerDlg.getFolderRow();
    else if (targetRow.isDirectory() == false)
      targetRow = App.fileManagerDlg.getFolderRow();
      
    if (targetRow == null) return false;
    
    if (draggingRows.size() == 1)
    {
      FilePath srcPath = draggingRows.get(0).row.getFilePath();
      if (srcPath.equals(targetRow.getFilePath())) return false;
      if (srcPath.getDirOnly().equals(targetRow.getFilePath())) return false;
    }
       
    return true;
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 

  @Override public void dragDroppedOnto(FileRow targetRow)
  {   
    PopupDialog dlg = new PopupDialog("Move or copy?");
    
    dlg.addButton("Move", mrMove);
    dlg.addButton("Copy", mrCopy);
    dlg.addButton("Cancel", mrCancel);
    
    DialogResult result = dlg.showModal();
    
    if (result == mrCancel) return;    

    boolean copying = (result == mrCopy);
    
    if (!App.fileManagerDlg.moveCopy(draggingRows, copying, true)) return;
    
    App.fileManagerDlg.paste(targetRow, copying, true);
  }

  @Override public DragNDropHoverHelper<FileRow> getHelper() { return ddHoverHelper; }
  
//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
  
}
