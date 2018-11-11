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

package org.hypernomicon.view.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.relations.RelationSet.*;

import java.util.EnumSet;
import java.util.Iterator;

import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.tabs.TreeTabController;
import org.hypernomicon.view.wrappers.HyperTreeCellValue;
import org.hypernomicon.view.wrappers.TreeRow;
import org.hypernomicon.view.wrappers.TreeWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableView;

public class ChooseParentDialogController extends HyperDialog
{
  @FXML private TreeTableView<TreeRow> ttv;
  @FXML private TextField tfPath;
  @FXML private Button btnCancel;
  @FXML private Button btnOk;
  @FXML TreeTableColumn<TreeRow, HyperTreeCellValue> tcName;
  @FXML TreeTableColumn<TreeRow, String> tcType;
  @FXML TreeTableColumn<TreeRow, String> tcDesc;
  
  public TreeWrapper popupTree;
  public static String title;
  public EnumSet<HDT_RecordType> types;
  public HDT_Base parent;
  private HDT_Base child;
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  @Override protected boolean isValid()
  {
    RelationType relType;
    
    if (types.contains(popupTree.selectedItem().getValue().getRecordType()) == false)
    {
      messageDialog("You must " + title.toLowerCase() + ".", mtError);
      return false;
    }
    
    TreeRow row = popupTree.selectedItem().getValue();
    parent = row.getRecord();
    
    if (((parent.getID() == child.getID())) && (parent.getType() == child.getType()))
    {
      messageDialog("A record cannot be its own parent. Please select another record.", mtError);
      return false;
    }
    
    relType = getRelation(child.getType(), parent.getType());
    
    switch (child.getType())
    {
      case hdtWorkLabel : case hdtNote : case hdtPosition : case hdtDebate : case hdtArgument : 
        
        if (db.getObjectList(relType, child, true).contains(parent))
        {
          messageDialog("The record already has that " + db.getTypeName(parent.getType()).toLowerCase() + " as a parent.", mtError);
          return false;
        }
        break;       
        
      case hdtWork : case hdtMiscFile :
        
        switch (parent.getType())
        {
          case hdtWorkLabel :
            
            if (db.getObjectList(relType, child, true).contains(parent))
            {
              messageDialog("The record already has that " + db.getTypeName(parent.getType()).toLowerCase() + " as a parent.", mtError);
              return false;
            }
            
            if (child.getType() == hdtMiscFile)
            {
              HDT_MiscFile childFile = (HDT_MiscFile) child;
              if (childFile.work.isNotNull())
              {
                messageDialog("A file record's labels cannot be changed while it is attached to a work record.", mtError);
                return false;
              }
            }
            
            break;
            
          case hdtWork :
            
            if (db.getObjectList(relType, child, true).size() > 0)
            {
              messageDialog("A " + db.getTypeName(child.getType()).toLowerCase() + " cannot have more than one parent work record.", mtError);
              return false;
            }
            break;
            
          case hdtArgument : // the parent can only be a work for this case
            
            relType = getRelation(hdtArgument, hdtWork);
            if (db.getObjectList(relType, parent, true).contains(child))
            {
              messageDialog("That work is already listed as a source for that argument.", mtError);
              return false;
            }
            break;
            
          default : break;            
        }
        
      default : break;
    }
    
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public static ChooseParentDialogController create(String title, HDT_Base child, EnumSet<HDT_RecordType> types)
  {
    ChooseParentDialogController cpd = HyperDialog.create("ChooseParentDialog.fxml", title, true);
    cpd.init(child, types);
    return cpd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_Base child, EnumSet<HDT_RecordType> types)
  {
    popupTree = new TreeWrapper(ttv, false, new ComboBox<TreeRow>());
    this.types = types;
    this.child = child;
    int ctr = 1;
    parent = null;
    
    title = "Select a ";
    
    Iterator<HDT_RecordType> it = types.iterator();
    
    while (it.hasNext())
    {
      HDT_RecordType type = it.next();
      
      title += db.getTypeName(type);
      
      if (ctr < types.size())
      {
        if (types.size() == 2)
          title += " or ";
        else if (ctr == (types.size() - 1))
          title += ", or ";
        else
          title += ", ";
      }
        
      ctr++;
    }
     
    title = title + " record";
    getStage().setTitle(title);
    
    tcName.setCellValueFactory(row -> new SimpleObjectProperty<>(row.getValue().getValue().getNameCell()));
    tcType.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getTypeString()));
    tcDesc.setCellValueFactory(row -> new SimpleStringProperty(row.getValue().getValue().getDescString()));
        
    popupTree.reset();
    
    TreeWrapper tree = ui.getTree();
    tree.debateTree.copyTo(popupTree.debateTree);
    tree.noteTree.copyTo(popupTree.noteTree);
    tree.labelTree.copyTo(popupTree.labelTree);
    
    popupTree.sort();
    popupTree.expandMainBranches();
        
    ttv.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue != null)
        tfPath.setText(TreeTabController.getTreePath(ttv, newValue));
      else
        tfPath.clear();
    });
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
