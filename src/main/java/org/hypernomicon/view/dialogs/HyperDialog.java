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
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import org.hypernomicon.App;

import java.io.IOException;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

//---------------------------------------------------------------------------  

public abstract class HyperDialog
{
  protected boolean okClicked = false;
  protected Stage dialogStage;
  protected AnchorPane mainPane;
  public Runnable onShown = null;
  private double initHeight = -1, initWidth = -1;
  private boolean shownAlready = false;

//---------------------------------------------------------------------------  

  public final Stage getStage()         { return dialogStage; }
  public final boolean isOkClicked()    { return okClicked; }
  public final AnchorPane getMainPane() { return mainPane; }
  public final boolean shownAlready()   { return shownAlready; }
  protected abstract boolean isValid();

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  protected static final <T extends HyperDialog> T create(String loc, String title, boolean resizable)
  {
    return createUsingFullPath("view/dialogs/" + loc, title, resizable, StageStyle.UTILITY, Modality.APPLICATION_MODAL);
  }
  
  protected static final <T extends HyperDialog> T createUsingFullPath(String loc, String title, boolean resizable, StageStyle stageStyle, Modality modality)
  {
    FXMLLoader loader = null;
    AnchorPane mainPane = null;
    Stage dialogStage = null;
    Scene scene = null;
    
    try
    {
      loader = new FXMLLoader(App.class.getResource(loc));      
      mainPane = (AnchorPane) loader.load();

      dialogStage = new Stage();
      dialogStage.setTitle(title);
      dialogStage.initModality(modality);
      dialogStage.setResizable(resizable);
      dialogStage.initStyle(stageStyle);
      
      if (modality == Modality.NONE)
        dialogStage.initOwner(null);
      else
        dialogStage.initOwner(ui.windows.getOutermostStage());
      dialogStage.getIcons().addAll(app.getPrimaryStage().getIcons());
      scene = new Scene(mainPane);
      dialogStage.setScene(scene);
      
      String id = safeStr(mainPane.getId());
      
      if (id.equals("About") == false)
      {        
        String css = App.class.getResource("resources/css.css").toExternalForm();
        scene.getStylesheets().add(css);
      }        
            
      final T dlg = loader.getController();

      dlg.mainPane = mainPane;
      dlg.dialogStage = dialogStage;
       
      dialogStage.setOnShown(event -> dlg.doOnShown());
      
      return dlg;
    }
    catch (IOException e) 
    { 
      messageDialog("Internal error while initializing dialog window", mtError);
    }
       
    return null;
  }
 
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void setInitHeight(String prefKey) 
  { 
    initHeight = appPrefs.getDouble(prefKey, mainPane.getPrefHeight());
    
    if (initHeight < 350)
      initHeight = mainPane.getPrefHeight();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void setInitWidth(String prefKey)
  { 
    initWidth = appPrefs.getDouble(prefKey, mainPane.getPrefWidth());
    
    if (initWidth < 350)
      initWidth = mainPane.getPrefWidth();
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  protected final void doOnShown()
  {
    rescale();
    
    if (onShown != null) onShown.run();
    
    shownAlready = true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final void rescale()
  {
    if (shownAlready == false)
    {
      Parent root = dialogStage.getScene().getRoot();
      
      String id = safeStr(mainPane.getId());
      
      if (id.equals("About") == false)
      {        
        scaleNodeForDPI(root);
        setFontSize(mainPane);
      }        
    }
    
    double diff = dialogStage.getHeight() - mainPane.getHeight();    
    if (diff == 0.0) diff = 30.0;
    
    double val = mainPane.getMaxHeight();
    if (val > 0)      
      dialogStage.setMaxHeight(val + diff);

    val = mainPane.getMinHeight();
    if (val > 0)
      dialogStage.setMinHeight(val + diff);
    
    val = mainPane.getMinWidth();
    if (val > 0)
      dialogStage.setMinWidth(val + diff);
   
    if (shownAlready == false)
    {
      if (initWidth <= 0)
      {
        val = mainPane.getPrefWidth();
        if (val > 0)
          dialogStage.setWidth(val + diff);
      }
      else
        dialogStage.setWidth(initWidth);
      
      if (initHeight <= 0)
      {
        val = mainPane.getPrefHeight();
        if (val > 0)          
          dialogStage.setHeight(val + diff);
      }
      else
        dialogStage.setHeight(initHeight);
    }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public final void showNonmodal()
  {
    dialogStage.show();
    
    ensureVisible(dialogStage, mainPane.getPrefWidth(), mainPane.getPrefHeight());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final boolean showModal()
  {
    ui.windows.push(dialogStage);
    
    dialogStage.showAndWait();
    
    ui.windows.pop();
       
    return isOkClicked();
  }
   
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
 
  @FXML protected void btnOkClick()
  {
    if (isValid() == false) return;
    
    okClicked = true;
    dialogStage.close();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @FXML protected void btnCancelClick()
  {
    okClicked = false;
    dialogStage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
