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

package org.hypernomicon.view;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;

import java.io.File;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.util.filePath.FilePath;

import javafx.scene.control.Alert;
import javafx.scene.control.MenuItem;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;
import javafx.stage.Modality;
import javafx.stage.Stage;
import javafx.stage.Window;

public final class WindowStack
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static interface WindowWrapper
  {
    Modality getModality();
    boolean isStage();
    Object getWrappedObj();
  }

//---------------------------------------------------------------------------

  private static final class AlertWrapper implements WindowWrapper
  {
    private final Alert dlg;

    private AlertWrapper(Alert dlg)         { this.dlg = dlg; }

    @Override public Modality getModality() { return dlg.getModality(); }
    @Override public boolean isStage()      { return false; }
    @Override public Object getWrappedObj() { return dlg; }
  }

//---------------------------------------------------------------------------

  private static final class StageWrapper implements WindowWrapper
  {
    private final Stage stage;

    private StageWrapper(Stage stage)       { this.stage = stage; }

    @Override public Modality getModality() { return stage.getModality(); }
    @Override public boolean isStage()      { return true; }
    @Override public Object getWrappedObj() { return stage; }
  }

//---------------------------------------------------------------------------

  private static final class ChooserWrapper implements WindowWrapper
  {
    private final Object chooser;

    private ChooserWrapper(Object chooser)  { this.chooser = chooser; }

    @Override public Modality getModality() { return Modality.APPLICATION_MODAL; }
    @Override public boolean isStage()      { return true; }
    @Override public Object getWrappedObj() { return chooser; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final LinkedList<WindowWrapper> windows = new LinkedList<>();
  private final Map<MenuItem, Boolean> itemsDisabled = new HashMap<>();
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

      windows.removeIf(curW -> curW.getWrappedObj() == window.getWrappedObj());
    }

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
      itemsDisabled.forEach(MenuItem::setDisable);

    if (focusingWindow.isStage() == false) return;

    Stage stage = StageWrapper.class.cast(focusingWindow).stage;

    focusStage(stage);

    if (SystemUtils.IS_OS_LINUX && (stage == app.getPrimaryStage()))
    {
      // This is a workaround for: https://bugs.openjdk.java.net/browse/JDK-8140491
      //
      // and another bug that seems related to this: https://bugs.openjdk.java.net/browse/JDK-8168842

      runDelayedInFXThread(1, 300, () -> // 300 ms delay to prevent main window from hiding/showing for multiple dialog boxes
      {
        if (stage.isShowing() == false)
          return;

        if (windows.stream().anyMatch(window -> window.getModality() != Modality.NONE))
          return;

        if ((stage.isFocused() == false) || appPrefs.getBoolean(PREF_KEY_LINUX_WORKAROUND, false))
        {
          stage.hide();
          stage.show();
        }
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath showDirDialog(DirectoryChooser chooser, Window owner)
  {
    push(new ChooserWrapper(chooser));

    FilePath rv = nullSwitch(chooser.showDialog(owner), null, FilePath::new);

    pop();

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath showOpenDialog(FileChooser chooser, Window owner)
  {
    push(new ChooserWrapper(chooser));

    FilePath rv = nullSwitch(chooser.showOpenDialog(owner), null, FilePath::new);

    pop();

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<File> showOpenMultipleDialog(FileChooser chooser, Window owner)
  {
    push(new ChooserWrapper(chooser));

    List<File> rv = chooser.showOpenMultipleDialog(owner);

    pop();

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
