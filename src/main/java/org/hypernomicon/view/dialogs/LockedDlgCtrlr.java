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

package org.hypernomicon.view.dialogs;

import javafx.application.Platform;
import javafx.fxml.FXML;

import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import org.hypernomicon.InterComputerMsg;
import org.hypernomicon.model.HyperDB.HDB_MessageType;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.HDB_MessageType.*;
import static org.hypernomicon.util.Util.*;

import java.time.Instant;

import static org.hypernomicon.FolderTreeWatcher.*;

//---------------------------------------------------------------------------

public class LockedDlgCtrlr extends HyperDlg
{
  private static MessageSenderThread thread = null;
  private String otherCompName;
  private long sentTime;

  @FXML private Button btnTryComm, btnTryTerminate, btnStop, btnOverride, btnCancel;
  @FXML private Label lblSeconds;
  @FXML private TextArea taOutput;

//---------------------------------------------------------------------------

  private class MessageSenderThread extends Thread
  {
    private LockedDlgCtrlr dlg;
    private InterComputerMsg sentMsg;
    private boolean done, gotResponse = false;

    private MessageSenderThread(LockedDlgCtrlr dlg, InterComputerMsg sentMsg)
    {
      super();
      this.dlg = dlg;
      this.sentMsg = sentMsg;
      done = false;
      start();
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    @Override public void run()
    {
      InterComputerMsg receivedMsg;

      while (done == false)
      {
        if (gotResponse == false)
        {
          receivedMsg = InterComputerMsg.checkForMessage(db.getResponseMessageFilePath());

          if (receivedMsg != null)
          {
            if (receivedMsg.getSource().equals(sentMsg.getDest()) && receivedMsg.getDest().equals(sentMsg.getSource()))
            {
              if ((sentMsg.getType() == hmtUnlockRequest) && (receivedMsg.getType() == hmtUnlockComplete))
              {
                gotResponse = true;
                db.getRequestMessageFilePath().deletePromptOnFail(false);
              }
              else if ((sentMsg.getType() == hmtEchoRequest) && (receivedMsg.getType() == hmtEchoReply))
              {
                gotResponse = true;
                db.getRequestMessageFilePath().deletePromptOnFail(false);
              }
            }
          }
        }
        else
        {
          if (db.getResponseMessageFilePath().exists() == false)
          {
            if (sentMsg.getType() == hmtUnlockRequest)
            {
              done = true;
              Platform.runLater(() -> dlg.stopTrying("Unlock was successful. Wait for all files to update before continuing!", true));
            }
            else if (sentMsg.getType() == hmtEchoRequest)
            {
              done = true;
              Platform.runLater(() -> dlg.stopTrying("Ping roundtrip completed in " + String.valueOf((Instant.now().getEpochSecond() - sentMsg.getSentTime())) + " seconds.", false));
            }
          }
        }

        Platform.runLater(() ->
        {
          long sec = Instant.now().getEpochSecond() - sentMsg.getSentTime();
          dlg.lblSeconds.setText("Elapsed: " + sec + " s");
        });

        sleepForMillis(FOLDER_TREE_WATCHER_POLL_TIME_MS);
      }

      sentMsg = null;
    }
  }

//---------------------------------------------------------------------------

  @FXML @Override protected void btnCancelClick()
  {
    if (btnStop.isDisabled() == false)
      btnStopClick();

    super.btnCancelClick();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnTryCommClick()
  {
    sendMessage(hmtEchoRequest, "Trying to communicate with " + otherCompName + "...");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnStopClick()
  {
    stopTrying("Cancelled by user after " + String.valueOf((Instant.now().getEpochSecond() - sentTime)) + " seconds.", false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnTryTerminateClick()
  {
    sendMessage(hmtUnlockRequest, "Trying to save/terminate instance on " + otherCompName + "...");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnOverrideClick()
  {
    if (btnStop.isDisabled() == false)
      stopTrying("Cancelled by user.", false);

    db.getLockFilePath().deletePromptOnFail(true);
    db.getRequestMessageFilePath().deletePromptOnFail(true);
    db.getResponseMessageFilePath().deletePromptOnFail(true);

    okClicked = true;
    dialogStage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void sendMessage(HDB_MessageType newMsgType, String output)
  {
    taOutput.appendText(output + System.lineSeparator());

    btnTryComm.setDisable(true);
    btnTryTerminate.setDisable(true);
    btnStop.setDisable(false);

    InterComputerMsg sentMsg = new InterComputerMsg(getComputerName(), otherCompName, newMsgType);
    sentMsg.writeToDisk();
    sentTime = sentMsg.getSentTime();

    thread = new MessageSenderThread(this, sentMsg);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static LockedDlgCtrlr create(String otherCompName)
  {
    LockedDlgCtrlr ldc = HyperDlg.create("LockedDlg.fxml", "Database is Currently Locked", true);
    ldc.init(otherCompName);
    return ldc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(String otherCompName)
  {
    this.otherCompName = otherCompName;
    taOutput.appendText("Database locked by computer " + otherCompName + System.lineSeparator());

    onShown = () -> disableCache(taOutput);

    db.getRequestMessageFilePath().deletePromptOnFail(true);
    db.getResponseMessageFilePath().deletePromptOnFail(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stopTrying(String output, boolean unlocked)
  {
    taOutput.appendText(output + System.lineSeparator());

    btnOverride.setDisable(false);
    btnStop.setDisable(true);
    btnCancel.setDisable(false);
    db.getRequestMessageFilePath().deletePromptOnFail(true);

    thread.done = true;

    if (unlocked)
    {
      btnOverride.setText("Proceed");
    }
    else
    {
      btnTryComm.setDisable(false);
      btnTryTerminate.setDisable(false);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
