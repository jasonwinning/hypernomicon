/*
 * Copyright 2015-2020 Jason Winning
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

import java.io.*;
import java.net.*;
import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.HyperTask.HyperThread;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

class InterProcDaemon extends HyperThread
{
  static final int PORT = 59346;

  InterProcDaemon()
  {
    super("InterProc");
    setDaemon(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void run()
  {
    try (ServerSocket serverSocket = new ServerSocket(PORT, 1))
    {
      while (true)
      {
        try (Socket clientSocket = serverSocket.accept();  // Wait for a connection
             PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
             BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
        {
          String line;

          do { line = in.readLine(); }
          while (line == null);

          int numArgs = parseInt(line, 0);

          if (numArgs > 0)
          {
            List<String> args = new ArrayList<>();

            for (int ndx = 0; ndx < numArgs; ndx++)
            {
              do { line = in.readLine(); }
              while (line == null);

              args.add(line);
            }

            runInFXThread(() -> ui.handleArgs(args));
          }

          out.println("Roger; out.");
        }
      }
    }
    catch (IOException e)
    {
      messageDialog("InterProcDaemon terminated unexpectedly: " + e.getMessage(), mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}