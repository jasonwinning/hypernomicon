/*
 * Copyright 2015-2026 Jason Winning
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

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.Exceptions.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.security.*;
import java.util.function.Consumer;

import javax.net.ssl.SSLContext;

import org.apache.http.client.ResponseHandler;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.conn.ssl.DefaultHostnameVerifier;
import org.apache.http.conn.ssl.TrustSelfSignedStrategy;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClientBuilder;
import org.apache.http.ssl.SSLContexts;

//---------------------------------------------------------------------------

public class AsyncHttpClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final class RequestThread extends HyperThread
  {
    private final ResponseHandler<Boolean> responseHandler;
    private final Consumer<Exception> failHndlr;

    private RequestThread(ResponseHandler<Boolean> responseHandler, Consumer<Exception> failHndlr)
    {
      super("HttpRequest");

      setDaemon(true);

      this.responseHandler = responseHandler;
      this.failHndlr = failHndlr;
    }

    @Override public void run()
    {
      cancelledByUser = false;

      try (CloseableHttpClient httpclient = createClient())
      {
        httpclient.execute(request, responseHandler);
      }
      catch (IOException e)
      {
        runInFXThread(() -> failHndlr.accept(cancelledByUser ? new CancelledTaskException() : e));
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum HttpRequestType { get, post, put, patch, delete, head, options, connect, trace }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private volatile HttpUriRequest request;
  private volatile boolean cancelledByUser = false;
  private RequestThread requestThread;
  private String lastUrl = "";

  public boolean wasCancelledByUser()     { return cancelledByUser; }

  public synchronized String lastUrl()    { return lastUrl; }
  public synchronized void clearLastUrl() { lastUrl = ""; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public synchronized void doRequest(HttpUriRequest request, ResponseHandler<Boolean> responseHandler, Consumer<Exception> failHndlr)
  {
    stop();

    this.request = request;

    try
    {
      lastUrl = request.getURI().toURL().toString();
    }
    catch (IllegalArgumentException | MalformedURLException e)
    {
      if (failHndlr != null) failHndlr.accept(e);
      lastUrl = "";
      return;
    }

    (requestThread = new RequestThread(responseHandler, failHndlr)).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public synchronized void stop()
  {
    if (HyperThread.isRunning(requestThread))
    {
      if (request != null)
      {
        cancelledByUser = true;
        request.abort();
      }

      try { requestThread.join(); } catch (InterruptedException e) { Thread.currentThread().interrupt(); }

      request = null;
    }

    requestThread = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static CloseableHttpClient createClient()
  {
    try
    {
      SSLContext sc = SSLContexts.custom()
        .loadTrustMaterial(new TrustSelfSignedStrategy())
        .build();

      return HttpClientBuilder.create()
        .setSSLContext(sc)
        .setSSLHostnameVerifier(new DefaultHostnameVerifier())
        .build();
    }
    catch (NoSuchAlgorithmException | KeyManagementException | KeyStoreException e)
    {
      throw new IllegalStateException("Error while creating HTTP client", e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
