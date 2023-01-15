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

package org.hypernomicon;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import java.io.*;
import java.net.*;

import org.hypernomicon.HyperTask.HyperThread;

class InterProcDaemon extends HyperThread
{

//---------------------------------------------------------------------------

  InterProcDaemon()
  {
    super("InterProc");
    setDaemon(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void run()
  {
    try (ServerSocket serverSocket = new ServerSocket(0, 1))
    {
      InterProcClient.setPortNum(serverSocket.getLocalPort());

      while (true)
      {
        try (Socket clientSocket = serverSocket.accept();  // Wait for a connection
             PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
        {
          String line;

          do { line = in.readLine(); }
          while (line == null);

          if (InterProcClient.UPDATE_CMD.equals(line))
            out.println(InterProcClient.getInstance());
        }
      }
    }
    catch (IOException e)
    {
      messageDialog("InterProcDaemon terminated unexpectedly: " + e.getMessage(), mtError, true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
