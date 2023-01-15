/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.dialogs;

import javafx.application.Platform;
import javafx.fxml.FXML;

import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.InterComputerMsg;
import org.hypernomicon.util.DesktopUtil;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.HDB_MessageType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.time.Instant;

import static org.hypernomicon.FolderTreeWatcher.*;

//---------------------------------------------------------------------------

public class LockedDlgCtrlr extends HyperDlg
{
  private static MessageSenderThread thread = null;
  private String otherCompName, otherHostName;
  private long sentTime;

  @FXML private Button btnTryComm, btnTryTerminate, btnStop, btnOverride, btnCancel;
  @FXML private Label lblSeconds;
  @FXML private TextArea taOutput;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------

  private static final class MessageSenderThread extends HyperThread
  {
    private final LockedDlgCtrlr dlg;
    private InterComputerMsg sentMsg;
    private volatile boolean done;
    private boolean gotResponse = false;

    private MessageSenderThread(LockedDlgCtrlr dlg, InterComputerMsg sentMsg)
    {
      super("InterComputerMessage");
      this.dlg = dlg;
      this.sentMsg = sentMsg;
      done = false;
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    @Override public void run()
    {
      while (done == false)
      {
        if (gotResponse == false)
        {
          InterComputerMsg receivedMsg = InterComputerMsg.checkForMessage(db.getResponseMessageFilePath(true));

          if (receivedMsg != null)
          {
            if (receivedMsg.getSource().equals(sentMsg.getDest()) && receivedMsg.getDest().equals(sentMsg.getSource()))
            {
              if ((sentMsg.getType() == hmtUnlockRequest) && (receivedMsg.getType() == hmtUnlockComplete))
              {
                gotResponse = true;
                db.getRequestMessageFilePath(true).deletePromptOnFail(false);
              }
              else if ((sentMsg.getType() == hmtEchoRequest) && (receivedMsg.getType() == hmtEchoReply))
              {
                gotResponse = true;
                db.getRequestMessageFilePath(true).deletePromptOnFail(false);
              }
            }
          }
        }
        else
        {
          if (db.getResponseMessageFilePath(true).exists() == false)
          {
            if (sentMsg.getType() == hmtUnlockRequest)
            {
              done = true;
              Platform.runLater(() -> dlg.stopTrying("Unlock was successful. Wait for all files to update before continuing!", true));
            }
            else if (sentMsg.getType() == hmtEchoRequest)
            {
              done = true;
              Platform.runLater(() -> dlg.stopTrying("Ping roundtrip completed in " + (Instant.now().getEpochSecond() - sentMsg.getSentTime()) + " seconds.", false));
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

  public static LockedDlgCtrlr build(String otherCompName)
  {
    return ((LockedDlgCtrlr) create("LockedDlg", "Database is Currently Locked", true)).init(otherCompName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static LockedDlgCtrlr build(String title, Throwable e)
  {
    return ((LockedDlgCtrlr) create("LockedDlg", title, true)).init(e);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private LockedDlgCtrlr init(Throwable e)
  {
    String stacktrace = ExceptionUtils.getStackTrace(e);

    taOutput.setText(stacktrace);

    btnTryTerminate.setText("Copy to Clipboard");
    btnTryTerminate.setOnAction(event -> copyToClipboard(stacktrace));

    setAllVisible(false, btnTryComm, btnOverride, btnStop, lblSeconds);

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private LockedDlgCtrlr init(String otherCompName)
  {
    this.otherCompName = otherCompName;

    if (otherCompName.contains("::::"))
    {
      this.otherHostName = otherCompName.substring(0, otherCompName.indexOf("::::"));

      if (otherHostName.isBlank())
        otherHostName = otherCompName.substring(otherCompName.indexOf("::::") + 4);
    }
    else
      this.otherHostName = otherCompName;

    taOutput.appendText("Database locked by computer " + otherHostName + System.lineSeparator());

    onShown = () -> disableCache(taOutput);

    db.getRequestMessageFilePath(true).deletePromptOnFail(true);
    db.getResponseMessageFilePath(true).deletePromptOnFail(true);

    return this;
  }

//---------------------------------------------------------------------------
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
    sendMessage(hmtEchoRequest, "Trying to communicate with " + otherHostName + "...");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnStopClick()
  {
    stopTrying("Cancelled by user after " + (Instant.now().getEpochSecond() - sentTime) + " seconds.", false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnTryTerminateClick()
  {
    sendMessage(hmtUnlockRequest, "Trying to save/terminate instance on " + otherHostName + "...");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnOverrideClick()
  {
    if (btnStop.isDisabled() == false)
      stopTrying("Cancelled by user.", false);

    db.getLockFilePath           (true).deletePromptOnFail(true);
    db.getRequestMessageFilePath (true).deletePromptOnFail(true);
    db.getResponseMessageFilePath(true).deletePromptOnFail(true);

    okClicked = true;
    dialogStage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void sendMessage(HDB_MessageType newMsgType, String output)
  {
    taOutput.appendText(output + System.lineSeparator());

    btnTryComm     .setDisable(true );
    btnTryTerminate.setDisable(true );
    btnStop        .setDisable(false);

    InterComputerMsg sentMsg = new InterComputerMsg(DesktopUtil.getComputerName(), otherCompName, newMsgType);
    sentMsg.writeToDisk(true);
    sentTime = sentMsg.getSentTime();

    (thread = new MessageSenderThread(this, sentMsg)).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stopTrying(String output, boolean unlocked)
  {
    taOutput.appendText(output + System.lineSeparator());

    btnOverride.setDisable(false);
    btnStop.setDisable(true);
    btnCancel.setDisable(false);
    db.getRequestMessageFilePath(true).deletePromptOnFail(true);

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
