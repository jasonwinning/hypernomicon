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

package org.hypernomicon.settings;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.bib.BibManager;
import org.hypernomicon.bib.LibraryWrapper.SyncTask;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.HyperDataException;

import javafx.fxml.FXML;
import javafx.scene.control.ProgressBar;

public final class SyncBibDlgCtrlr extends ModalDialog
{
  @FXML private ProgressBar progressBar;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void sync()
  {
    try
    {
      new SyncBibDlgCtrlr(db.getBibLibrary().createNewSyncTask()).showModal();
    }
    catch (HyperDataException e)
    {
      errorPopup("Unable to download library data: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private SyncBibDlgCtrlr(SyncTask syncTask)
  {
    super("settings/SyncBibDlg", "Link to " + db.bibLibraryUserFriendlyName(), true, true);

    onShown = () ->
    {
      BibManager.instance().clearCollectionTree();

      syncTask.runningProperty().addListener((ob, wasRunning, isRunning) ->
      {
        if (wasRunning && Boolean.FALSE.equals(isRunning))
        {
          BibManager.instance().rebuildCollectionTree();

          stage.close();
        }
      });

      syncTask.startWithNewThreadAsDaemon();
    };

    stage.setOnHiding(event -> db.getBibLibrary().stop());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
