/*
 * Copyright 2015-2024 Jason Winning
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

package org.hypernomicon.settings;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.bib.LibraryWrapper.SyncTask;
import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.model.Exceptions.HyperDataException;

import javafx.concurrent.Worker.State;
import javafx.fxml.FXML;
import javafx.scene.control.ProgressBar;

public final class SyncBibDlgCtrlr extends HyperDlg
{
  @FXML private ProgressBar progressBar;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void sync()
  {
    new SyncBibDlgCtrlr().showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private SyncBibDlgCtrlr()
  {
    super("settings/SyncBibDlg", "Link to " + db.getBibLibrary().type().getUserFriendlyName(), true, true);

    onShown = () ->
    {
      bibManagerDlg.clearCollectionTree();

      SyncTask syncTask = db.getBibLibrary().createNewSyncTask();

      syncTask.runningProperty().addListener((ob, wasRunning, isRunning) ->
      {
        if (wasRunning && Boolean.FALSE.equals(isRunning))
        {
          if ((syncTask.getState() == State.FAILED) || (syncTask.getState() == State.CANCELLED))
          {
            Throwable e = syncTask.getException();

            if (e instanceof HyperDataException)
              errorPopup(e);
          }

          bibManagerDlg.rebuildCollectionTree();

          getStage().close();
        }
      });

      syncTask.startWithNewThreadAsDaemon();
    };

    dialogStage.setOnHiding(event -> db.getBibLibrary().stop());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
