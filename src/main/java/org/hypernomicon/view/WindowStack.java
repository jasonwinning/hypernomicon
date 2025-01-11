/*
 * Copyright 2015-2025 Jason Winning
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
import java.time.Instant;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.util.filePath.FilePath;

import javafx.application.Platform;
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

  private interface WindowWrapper
  {
    Modality getModality();
    boolean isStage();
    Object getWrappedObj();

    default void saveDimensions   () { }
    default void restoreDimensions() { }
  }

//---------------------------------------------------------------------------

  private record AlertWrapper(Alert dlg) implements WindowWrapper
  {
    @Override public Modality getModality() { return dlg.getModality(); }
    @Override public boolean isStage()      { return false; }
    @Override public Object getWrappedObj() { return dlg; }
  }

//---------------------------------------------------------------------------

  private record ChooserWrapper(Object chooser) implements WindowWrapper
  {
    @Override public Modality getModality() { return Modality.APPLICATION_MODAL; }
    @Override public boolean isStage()      { return false; }
    @Override public Object getWrappedObj() { return chooser; }
  }

//---------------------------------------------------------------------------

  private static final class StageWrapper implements WindowWrapper
  {
    private final Stage stage;
    private double height, width;

    private StageWrapper(Stage stage)       { this.stage = stage; }

    @Override public Modality getModality() { return stage.getModality(); }
    @Override public boolean isStage()      { return true; }
    @Override public Object getWrappedObj() { return stage; }

    @Override public void saveDimensions()
    {
      height = stage.getHeight(); width = stage.getWidth();

      if (SystemUtils.IS_OS_LINUX && (stage == ui.getStage()))   // In some Linux environments, the main window inexplicably gets
        runDelayedInFXThread(3, 100, this::restoreDimensions);   // resized when a window it is the owner of is opened
    }

    @Override public void restoreDimensions()
    {
      if (SystemUtils.IS_OS_LINUX && (stage == ui.getStage()))   // In some Linux environments, the main window inexplicably gets
        stage.setHeight(height); stage.setWidth(width);          // resized when a window it is the owner of is opened
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final LinkedList<WindowWrapper> windows = new LinkedList<>();
  private final Map<MenuItem, Boolean> itemsDisabled = new HashMap<>();
  private boolean cyclingFocus = false;
  private volatile Modality outermostModality;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void push(Alert dlg)            { push(new AlertWrapper(dlg)); }
  public void push(Stage stage)          { push(new StageWrapper(stage)); }
  public boolean getCyclingFocus()       { return cyclingFocus; }
  public Modality getOutermostModality() { return outermostModality; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void push(WindowWrapper window)
  {
    Modality newModality = window.getModality();

    if (windows.isEmpty() == false)
    {
      Modality oldModality = getOutermostModality();

      if ((oldModality != Modality.NONE) &&    // This happens when focus returns from a modal window
          (newModality == Modality.NONE))      // back to the non-modal window that created it.
        return;

      if ((oldModality == Modality.NONE) &&
          (newModality != Modality.NONE))
        disableMainMenu();

      windows.removeIf(curW -> curW.getWrappedObj() == window.getWrappedObj());
    }

    if (windows.isEmpty() == false)
      windows.getFirst().saveDimensions();

    windows.addFirst(window);

    outermostModality = newModality;
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
      Stage curStage = ((StageWrapper) window).stage;
      if (stage != curStage)
        curStage.toFront();
    });

    cyclingFocus = false;

    stage.toFront();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void disableMainMenu()
  {
    itemsDisabled.clear();

    ui.getMenuBar().getMenus().forEach(menu -> menu.getItems().forEach(item ->
    {
      itemsDisabled.put(item, item.isDisable());
      item.setDisable(true);
    }));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stage getOutermostStage()
  {
    return findFirst(windows, WindowWrapper::isStage, window -> ((StageWrapper) window).stage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void pop()
  {
    if (windows.isEmpty())
      return;

    WindowWrapper closingWindow = windows.removeFirst(), focusingWindow = windows.getFirst();

    if (focusingWindow == null)
    {
      outermostModality = null;
      return;
    }

    outermostModality = focusingWindow.getModality();

    if ((closingWindow.getModality() != Modality.NONE) && (focusingWindow.getModality() == Modality.NONE))
    {
      itemsDisabled.forEach(MenuItem::setDisable);
      itemsDisabled.clear();
    }

    if (focusingWindow.isStage() == false)
      return;

    focusingWindow.restoreDimensions();

    Stage stage = ((StageWrapper) focusingWindow).stage;

    focusStage(stage);

    if (SystemUtils.IS_OS_LINUX && (stage == ui.getStage()))
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

        if ((stage.isFocused() == false) || app.prefs.getBoolean(PREF_KEY_LINUX_WORKAROUND, false))
        {
          stage.hide();
          stage.show();
        }
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath showDirDialog(DirectoryChooser chooser)
  {
    Window owner = getOutermostStage();

    push(new ChooserWrapper(chooser));

    FilePath rv = nullSwitch(chooser.showDialog(owner), null, FilePath::new);

    pop();

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath showOpenDialog(FileChooser chooser)
  {
    Window owner = getOutermostStage();

    push(new ChooserWrapper(chooser));

    FilePath rv = nullSwitch(chooser.showOpenDialog(owner), null, FilePath::new);

    pop();

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<File> showOpenMultipleDialog(FileChooser chooser)
  {
    Window owner = getOutermostStage();

    push(new ChooserWrapper(chooser));

    List<File> rv = chooser.showOpenMultipleDialog(owner);

    pop();

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Run code after no modal windows have been open for a certain amount of time
   * @param intervalMS The amount of time that has to go by with no modal popups
   * @param runnable The code to run after the interval
   */
  public void runInFXThreadAfterModalPopups(int intervalMS, Runnable runnable)
  {
    runOutsideFXThread(() ->
    {
      tryNonmodalInterval(intervalMS); // Wait until no modal windows have been open for a certain amount of time
      Platform.runLater(runnable);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void tryNonmodalInterval(int intervalMS)
  {
    Modality tmpOutermostModality;

    do
    {
      while (outermostModality != Modality.NONE)
        sleepForMillis(50);

      long startTime = Instant.now().toEpochMilli();

      tmpOutermostModality = outermostModality; // set local variable for thread safety

      while ((tmpOutermostModality == Modality.NONE) && ((Instant.now().toEpochMilli() - startTime) < intervalMS))
      {
        sleepForMillis(50);
        tmpOutermostModality = outermostModality;
      }

    } while (tmpOutermostModality != Modality.NONE);
  }
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
