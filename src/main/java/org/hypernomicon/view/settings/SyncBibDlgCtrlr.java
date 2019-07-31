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

package org.hypernomicon.view.settings;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import org.hypernomicon.bib.LibraryWrapper.SyncTask;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.view.dialogs.HyperDlg;

import javafx.concurrent.Worker.State;
import javafx.fxml.FXML;
import javafx.scene.control.ProgressBar;

public class SyncBibDlgCtrlr extends HyperDlg
{
  @FXML private ProgressBar progressBar;

  private SyncTask syncTask = null;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static SyncBibDlgCtrlr create()
  {
    return HyperDlg.createUsingFullPath("view/settings/SyncBibDlg.fxml", "Link to External Bibliography Manager", true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean sync()
  {
    onShown = () ->
    {
      syncTask = db.getBibLibrary().createNewSyncTask();

      syncTask.runningProperty().addListener((ob, wasRunning, isRunning) ->
      {
        if (wasRunning && (isRunning == false))
        {
          if ((syncTask.getState() == State.FAILED) || (syncTask.getState() == State.CANCELLED))
          {
            Throwable ex = syncTask.getException();

            if (ex instanceof HyperDataException)
              messageDialog(ex.getMessage(), mtError);
          }

          getStage().close();
        }
      });

      Thread thread = new Thread(syncTask);
      thread.setDaemon(true);
      syncTask.setThread(thread);
      thread.start();
    };

    dialogStage.setOnHiding(event ->
    {
      if ((syncTask != null) && syncTask.isRunning())
        syncTask.cancel();

      db.getBibLibrary().stop();
    });

    showModal();

    return okClicked;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
