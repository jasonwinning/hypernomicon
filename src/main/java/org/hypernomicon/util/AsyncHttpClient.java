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

package org.hypernomicon.util;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.Exceptions.*;

import java.io.IOException;
import java.util.function.Consumer;

import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.impl.client.CloseableHttpClient;

public class AsyncHttpClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public class RequestThread extends Thread
  {
    private ResponseHandler<? extends Boolean> responseHandler;
    private Consumer<Exception> failHndlr;

    public RequestThread(ResponseHandler<? extends Boolean> responseHandler, Consumer<Exception> failHndlr)
    {
      super();

      setDaemon(true);

      this.responseHandler = responseHandler;
      this.failHndlr = failHndlr;

      start();
    }

    @Override public void run()
    {
      cancelledByUser = false;

      try (CloseableHttpClient httpclient = getHTTPClient())
      {
        httpclient.execute(request, responseHandler);
      }
      catch (IOException e)
      {
        if (cancelledByUser)
          runInFXThread(() -> failHndlr.accept(new TerminateTaskException()));
        else
          runInFXThread(() -> failHndlr.accept(e));
      }

      stopped = true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static enum HttpRequestType { get, post, put, patch, delete, head, options, connect, trace }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HttpUriRequest request;
  private boolean stopped = true, cancelledByUser = false;
  private RequestThread requestThread;
  private String lastUrl = "";

  public boolean wasCancelledByUser() { return cancelledByUser; }
  public String lastUrl()             { return lastUrl; }
  public void clearLastUrl()          { lastUrl = ""; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void doRequest(HttpUriRequest request, ResponseHandler<? extends Boolean> responseHandler, Consumer<Exception> failHndlr)
  {
    stop();

    this.request = request;

    try
    {
      lastUrl = request.getURI().toURL().toString();
    }
    catch (Exception e)
    {
      lastUrl = "";
    }

    requestThread = new RequestThread(responseHandler, failHndlr);
    stopped = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isRunning()
  {
    if (stopped == true) return false;
    return nullSwitch(requestThread, false, RequestThread::isAlive);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean stop()
  {
    boolean wasRunning = isRunning();

    if ((requestThread != null) && requestThread.isAlive())
    {
      if (request != null)
      {
        cancelledByUser = true;
        request.abort();
      }

      try { requestThread.join(); } catch (InterruptedException e) { noOp(); }

      request = null;
    }

    requestThread = null;
    stopped = true;

    return wasRunning;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
