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

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.scene.control.MenuItem;
import javafx.stage.Modality;
import javafx.stage.Stage;

public class WindowStack
{  

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  static interface WindowWrapper
  {
    public Modality getModality();
    public boolean isStage();
  }
  
  private static final class AlertWrapper implements WindowWrapper
  {
    private final Alert dlg;
    
    public AlertWrapper(Alert dlg) { this.dlg = dlg; }
    
    @Override public final Modality getModality() { return dlg.getModality(); }
    @Override public final boolean isStage()      { return false; }
  }
  
  private static final class StageWrapper implements WindowWrapper
  {
    private final Stage stage;
    
    public StageWrapper(Stage stage) { this.stage = stage; }
    
    public final Stage getStage() { return stage; }
    
    @Override public final Modality getModality() { return stage.getModality(); }
    @Override public final boolean isStage()      { return true; }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private final LinkedList<WindowWrapper> windows = new LinkedList<>();
  private final HashMap<MenuItem, Boolean> itemsDisabled = new HashMap<>();
  private boolean cyclingFocus = false;
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public final void push(Alert dlg)            { push(new AlertWrapper(dlg)); }
  public final void push(Stage stage)          { push(new StageWrapper(stage)); }
  public final boolean getCyclingFocus()       { return cyclingFocus; }
  public final Modality getOutermostModality() { return peek().getModality(); }
  private final WindowWrapper peek()           { return windows.isEmpty() ? null : windows.getFirst(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private final void push(WindowWrapper window)
  {
    if (windows.isEmpty() == false)
    {
      Modality oldModality = getOutermostModality(),
               newModality = window.getModality();
      
      if ((oldModality != Modality.NONE) &&    // This happens when focus returns from a modal window 
          (newModality == Modality.NONE))      // back to the non-modal window that created it.
        return;                                                                               
      
      if ((oldModality == Modality.NONE) && 
          (newModality != Modality.NONE))
        disableMainMenu();
    }
    
    while (windows.remove(window));  // semicolon at the end is not a typo
    windows.addFirst(window);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public final void focusStage(Stage stage)
  {
    if (getOutermostModality() != Modality.NONE)
      return;
  
    cyclingFocus = true;
    
    windows.descendingIterator().forEachRemaining(window -> 
    {
      Stage curStage = StageWrapper.class.cast(window).getStage();
      if (stage != curStage)
        curStage.toFront(); 
    });
    
    stage.toFront();
    
    Platform.runLater(() ->
    {
      for (int ndx = 0; ndx < 6; ndx++)
      {
        sleepForMillis(50);
        stage.toFront();
      }
  
      cyclingFocus = false;
    });
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private final void disableMainMenu()
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

  public final Stage getOutermostStage()
  {
    for (WindowWrapper window : windows)
      if (window.isStage())
        return StageWrapper.class.cast(window).getStage();
        
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public final void pop()
  {
    if (windows.isEmpty()) return;
    WindowWrapper closingWindow = windows.removeFirst(), focusingWindow = windows.getFirst();
        
    if (nonNull(focusingWindow))
    {
      if ((closingWindow.getModality() != Modality.NONE) && (focusingWindow.getModality() == Modality.NONE))
        itemsDisabled.forEach((menuItem, disabled) -> menuItem.setDisable(disabled));
      
      if (focusingWindow.isStage())
      {
        Stage stage = StageWrapper.class.cast(focusingWindow).getStage();
        
        if (SystemUtils.IS_OS_LINUX && (stage == app.getPrimaryStage()))                
        {
          // This is a workaround for:
          // https://bugs.openjdk.java.net/browse/JDK-8140491
        	
          runDelayedInFXThread(1, 300, event -> // 300 ms delay to prevent main window from hiding/showing for multiple dialog boxes
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
