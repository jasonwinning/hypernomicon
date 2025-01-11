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

package org.hypernomicon;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.util.SplitString;
import org.hypernomicon.util.filePath.FilePath;

//---------------------------------------------------------------------------

class AppInstance
{
  private final String id;
  private final int portNum;
  private final FilePath dbPath;

//---------------------------------------------------------------------------

  AppInstance(String id, int portNum, FilePath dbPath)
  {
    this.id = id;
    this.portNum = portNum;
    this.dbPath = FilePath.isEmpty(dbPath) ? new FilePath("") : dbPath;
  }

  String getID()       { return id; }
  int getPortNum()     { return portNum; }
  FilePath getDBPath() { return dbPath; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


  @Override public String toString()
  {
    return String.join(";", id, Integer.toString(portNum), dbPath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static AppInstance fromString(String line)
  {
    SplitString splitStr = new SplitString(line, ';');

    String id = splitStr.next();
    int portNum = parseInt(splitStr.next(), -1);
    FilePath dbPath = new FilePath(splitStr.next());

    return safeStr(id).isBlank() || (portNum < 1) ? null : new AppInstance(id, portNum, dbPath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
