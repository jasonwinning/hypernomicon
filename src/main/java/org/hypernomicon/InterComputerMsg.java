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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.HDB_MessageType.*;
import static org.hypernomicon.util.DesktopUtil.*;

import org.hypernomicon.util.filePath.FilePath;

import com.google.common.collect.Lists;

import java.io.IOException;
import java.time.Instant;
import java.util.List;

import org.apache.commons.io.FileUtils;

//---------------------------------------------------------------------------

public class InterComputerMsg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String source, dest;
  private final HDB_MessageType type;
  private final long sentTime;

//---------------------------------------------------------------------------

  public InterComputerMsg(String source, String dest, HDB_MessageType type)
  {
    this.source = source;
    this.dest = dest;
    this.type = type;

    sentTime = Instant.now().getEpochSecond();
  }

//---------------------------------------------------------------------------

  public String getSource()        { return source; }
  public String getDest()          { return dest; }
  public HDB_MessageType getType() { return type; }

  /**
   * Gets the number of seconds when the message was sent from the Java epoch of 1970-01-01T00:00:00Z.
   * <p>
   * The epoch second count is a simple incrementing count of seconds where
   * second 0 is 1970-01-01T00:00:00Z.
   * @return The epoch seconds since the message was sent
   */
  public long getSentTimeInSec()   { return sentTime; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void writeToFile(boolean getFolderFromAppPrefs) throws IOException
  {
    List<String> lines = Lists.newArrayList(source, dest);

    FilePath filePath = switch (type)
    {
      case hmtEchoRequest    -> { lines.add("echo request"   ); yield db.getRequestMessageFilePath (getFolderFromAppPrefs); }
      case hmtUnlockRequest  -> { lines.add("unlock request" ); yield db.getRequestMessageFilePath (getFolderFromAppPrefs); }
      case hmtEchoReply      -> { lines.add("echo reply"     ); yield db.getResponseMessageFilePath(getFolderFromAppPrefs); }
      case hmtUnlockComplete -> { lines.add("unlock complete"); yield db.getResponseMessageFilePath(getFolderFromAppPrefs); }

      default -> throw new UnsupportedOperationException("Attempt to write inter-computer message of invalid type.");
    };

    FileUtils.writeLines(filePath.toFile(), lines);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static InterComputerMsg checkForMessage(FilePath filePath)
  {
    if (filePath.exists() == false) return null;

    List<String> s;

    try { s = filePath.readToStrList(); }
    catch (IOException e) { return null; }

    return ((s.size() != 3) || s.get(0).equals(getComputerName())) ?
      null
    :
      new InterComputerMsg(s.get(0), s.get(1), switch (s.get(2))
      {
        case "echo request"    -> hmtEchoRequest;
        case "echo reply"      -> hmtEchoReply;
        case "unlock request"  -> hmtUnlockRequest;
        case "unlock complete" -> hmtUnlockComplete;
        default                -> hmtNone;
      });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
