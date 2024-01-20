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

package org.hypernomicon.dialogs;

import org.apache.http.client.HttpResponseException;
import org.hypernomicon.HyperTask;
import org.hypernomicon.util.PopupDialog.DialogResult;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.UIUtil.*;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.UnknownHostException;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public final class InternetCheckDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------

  private Exception lastException = null;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean check()
  {
    DialogResult result = mrRetry;

    while (result == mrRetry)
    {
      InternetCheckDlgCtrlr ctrlr = new InternetCheckDlgCtrlr();

      if (ctrlr.showModal())
        return true;

      String msg = "Warning: Internet connection check failed" + (ctrlr.lastException == null ? '.' : ": " + ctrlr.lastException.getMessage());
      result = abortRetryIgnoreDialog(msg);
    }

    return result == mrIgnore;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private InternetCheckDlgCtrlr()
  {
    super("InternetCheckDlg", appTitle, true);

    HyperTask task = new HyperTask("CheckForInternet") { @Override protected void call()
    {
      try
      {
        HttpURLConnection con = (HttpURLConnection) new URL("https://www.google.com/").openConnection();
        con.connect();

        if (con.getResponseCode() == HttpURLConnection.HTTP_OK)
          okClicked = true;
        else
          lastException = new HttpResponseException(con.getResponseCode(), con.getResponseMessage());
      }
      catch (UnknownHostException e) { noOp();            }
      catch (IOException          e) { lastException = e; }

      runInFXThread(() -> getStage().close());
    }};

    onShown = task::startWithNewThread;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
