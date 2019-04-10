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

package org.hypernomicon.view;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;

import java.util.HashMap;
import java.util.LinkedList;

import org.apache.commons.lang3.SystemUtils;

import javafx.scene.control.Alert;
import javafx.scene.control.MenuItem;
import javafx.stage.Modality;
import javafx.stage.Stage;

public final class WindowStack
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static interface WindowWrapper
  {
    Modality getModality();
    boolean isStage();
  }

  private static final class AlertWrapper implements WindowWrapper
  {
    private final Alert dlg;

    private AlertWrapper(Alert dlg) { this.dlg = dlg; }

    @Override public Modality getModality() { return dlg.getModality(); }
    @Override public boolean isStage()      { return false; }
  }

  private static final class StageWrapper implements WindowWrapper
  {
    private final Stage stage;

    private StageWrapper(Stage stage) { this.stage = stage; }

    @Override public Modality getModality() { return stage.getModality(); }
    @Override public boolean isStage()      { return true; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final LinkedList<WindowWrapper> windows = new LinkedList<>();
  private final HashMap<MenuItem, Boolean> itemsDisabled = new HashMap<>();
  private boolean cyclingFocus = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void push(Alert dlg)            { push(new AlertWrapper(dlg)); }
  public void push(Stage stage)          { push(new StageWrapper(stage)); }
  public boolean getCyclingFocus()       { return cyclingFocus; }
  public Modality getOutermostModality() { return peek().getModality(); }
  private WindowWrapper peek()           { return windows.isEmpty() ? null : windows.getFirst(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void push(WindowWrapper window)
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

  public void focusStage(Stage stage)
  {
    if (getOutermostModality() != Modality.NONE)
      return;

    cyclingFocus = true;

    windows.descendingIterator().forEachRemaining(window ->
    {
      Stage curStage = StageWrapper.class.cast(window).stage;
      if (stage != curStage)
        curStage.toFront();
    });

    stage.toFront();

    cyclingFocus = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void disableMainMenu()
  {
    itemsDisabled.clear();

    ui.getMenuBar().getMenus().forEach(menu -> menu.getItems().forEach(item ->
    {
      itemsDisabled.put(item, Boolean.valueOf(item.isDisable()));
      item.setDisable(true);
    }));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stage getOutermostStage()
  {
    return findFirst(windows, WindowWrapper::isStage, window -> StageWrapper.class.cast(window).stage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void pop()
  {
    if (windows.isEmpty()) return;
    WindowWrapper closingWindow = windows.removeFirst(), focusingWindow = windows.getFirst();

    if (focusingWindow == null) return;

    if ((closingWindow.getModality() != Modality.NONE) && (focusingWindow.getModality() == Modality.NONE))
      itemsDisabled.forEach((menuItem, disabled) -> menuItem.setDisable(disabled));

    if (focusingWindow.isStage() == false) return;

    Stage stage = StageWrapper.class.cast(focusingWindow).stage;

    if (SystemUtils.IS_OS_LINUX && (stage == app.getPrimaryStage()))
    {
      // This is a workaround for:
      // https://bugs.openjdk.java.net/browse/JDK-8140491

      runDelayedInFXThread(1, 300, () -> // 300 ms delay to prevent main window from hiding/showing for multiple dialog boxes
      {
        if (app.getPrimaryStage().isShowing() == false)
          return;

        if (windows.stream().anyMatch(window -> window.getModality() != Modality.NONE))
          return;

        app.getPrimaryStage().hide();
        app.getPrimaryStage().show();
      });
    }

	  focusStage(stage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
