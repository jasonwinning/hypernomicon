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

package org.hypernomicon.view;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static java.util.Objects.*;

import java.util.HashMap;
import java.util.LinkedList;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.util.Util;

import javafx.scene.control.Alert;
import javafx.scene.control.MenuItem;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class WindowStack
{
  private MainController ui;
  
  public WindowStack(MainController ui) { this.ui = ui; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private static interface WindowWrapper
  {
    public Modality getModality();
    public boolean isStage();
  }
  
  private static class AlertWrapper implements WindowWrapper
  {
    private Alert dlg;
    
    public AlertWrapper(Alert dlg) { this.dlg = dlg; }
    
    @Override public Modality getModality() { return dlg.getModality(); }
    @Override public boolean isStage()      { return false; }
  }
  
  private static class StageWrapper implements WindowWrapper
  {
    private Stage stage;
    
    public StageWrapper(Stage stage) { this.stage = stage; }
    
    public Stage getStage() { return stage; }
    
    @Override public Modality getModality() { return stage.getModality(); }
    @Override public boolean isStage()      { return true; }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private LinkedList<WindowWrapper> windows = new LinkedList<>();
  private HashMap<MenuItem, Boolean> itemsDisabled = new HashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private WindowWrapper peek() 
  { 
    if (windows.isEmpty()) return null;
    return windows.getFirst();
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public void push(Alert dlg)   { push(new AlertWrapper(dlg)); }
  public void push(Stage stage) { push(new StageWrapper(stage)); }
  
  private void push(WindowWrapper window)
  {
    if (windows.isEmpty() == false)
    {
      if ((peek().getModality() != Modality.NONE) && (window.getModality() == Modality.NONE)) // This happens when focus returns from a modal window back to the
        return;                                                                               // non-modal window that created it.
      
      if ((peek().getModality() == Modality.NONE) && (window.getModality() != Modality.NONE))
        disableMainMenu();
    }
    
    while (windows.remove(window));
    windows.addFirst(window);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void disableMainMenu()
  {
    itemsDisabled.clear();
    
    ui.getMenuBar().getMenus().forEach(menu ->
    {
      for (MenuItem item : menu.getItems())
      {  
        itemsDisabled.put(item, Boolean.valueOf(item.isDisable()));
        item.setDisable(true);
      }        
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public Modality getOutermostModality()
  {
    return peek().getModality();
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public Stage getOutermostStage()
  {
    for (WindowWrapper window : windows)
      if (window.isStage())
        return StageWrapper.class.cast(window).getStage();
        
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public void pop()
  {
    if (windows.isEmpty()) return;
    WindowWrapper closingWindow = windows.removeFirst(), focusingWindow = windows.getFirst();
        
    if (nonNull(focusingWindow))
    {
      if ((closingWindow.getModality() != Modality.NONE) && (focusingWindow.getModality() == Modality.NONE))
      {
        itemsDisabled.forEach((menuItem, disabled) -> menuItem.setDisable(disabled));
      }
      
      if (focusingWindow.isStage())
      {
        Stage stage = StageWrapper.class.cast(focusingWindow).getStage();
        
        if ((SystemUtils.IS_OS_LINUX) && (stage == app.getPrimaryStage()))                
        {
          // This is a workaround for:
          // https://bugs.openjdk.java.net/browse/JDK-8140491
        	
          Util.runDelayedInFXThread(1, 300, event -> // 300 ms delay to prevent main window from hiding/showing for multiple dialog boxes
          {
            if (app.getPrimaryStage().isShowing() == false)
              return;
            
            for (WindowWrapper window : windows)
              if (window.getModality() != Modality.NONE)
                return;

            app.getPrimaryStage().hide();
            app.getPrimaryStage().show();
          });
        }
        
    	  focusStage(stage);
      }
    }
       
    return;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

}
