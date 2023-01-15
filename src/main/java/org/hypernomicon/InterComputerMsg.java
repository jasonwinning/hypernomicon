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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.HDB_MessageType.*;

import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;

import com.google.common.collect.Lists;

import java.io.IOException;
import java.time.Instant;
import java.util.List;

import static java.nio.charset.StandardCharsets.*;

import org.apache.commons.io.FileUtils;

public class InterComputerMsg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String source, dest;
  private final HDB_MessageType type;
  private final long sentTime;

  public InterComputerMsg(String source, String dest, HDB_MessageType type)
  {
    this.source = source;
    this.dest = dest;
    this.type = type;
    sentTime = Instant.now().getEpochSecond();
  }

  public String getSource()        { return source; }
  public String getDest()          { return dest; }
  public HDB_MessageType getType() { return type; }
  public long getSentTime()        { return sentTime; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean writeToDisk(boolean getFolderFromAppPrefs)
  {
    FilePath filePath;
    List<String> lines = Lists.newArrayList(source, dest);

    switch (type)
    {
      case hmtEchoRequest    : lines.add("echo request"   ); filePath = db.getRequestMessageFilePath (getFolderFromAppPrefs); break;
      case hmtEchoReply      : lines.add("echo reply"     ); filePath = db.getResponseMessageFilePath(getFolderFromAppPrefs); break;
      case hmtUnlockRequest  : lines.add("unlock request" ); filePath = db.getRequestMessageFilePath (getFolderFromAppPrefs); break;
      case hmtUnlockComplete : lines.add("unlock complete"); filePath = db.getResponseMessageFilePath(getFolderFromAppPrefs); break;
      default                : return false;
    }

    try { FileUtils.writeLines(filePath.toFile(), lines); }
    catch (IOException e) { return false; }
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static InterComputerMsg checkForMessage(FilePath filePath)
  {
    if (filePath.exists() == false) return null;

    List<String> s;

    try { s = FileUtils.readLines(filePath.toFile(), UTF_8); }
    catch (IOException e) { return null; }

    if ((s.size() != 3) || s.get(0).equals(DesktopUtil.getComputerName()))
      return null;

    HDB_MessageType type;

    switch (s.get(2))
    {
      case "echo request"    : type = hmtEchoRequest;    break;
      case "echo reply"      : type = hmtEchoReply;      break;
      case "unlock request"  : type = hmtUnlockRequest;  break;
      case "unlock complete" : type = hmtUnlockComplete; break;
      default                : type = hmtNone;
    }

    return new InterComputerMsg(s.get(0), s.get(1), type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
