/*
 * Copyright 2015-2018 Jason Winning
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
import static org.hypernomicon.util.Util.*;
import org.hypernomicon.model.HyperDB.HDB_MessageType;
import org.hypernomicon.util.filePath.FilePath;

import java.io.IOException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;

public class InterComputerMsg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String source;
  private String dest;
  private HDB_MessageType type;
  private long sentTime;
  
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
  
  public boolean writeToDisk()
  {
    List<String> s;
    FilePath filePath;
  
    s = new ArrayList<String>();
    s.add(source);
    s.add(dest);
  
    switch (type)
    {
      case hmtEchoRequest    : s.add("echo request");    filePath = db.getRequestMessageFilePath();  break;
      case hmtEchoReply      : s.add("echo reply");      filePath = db.getResponseMessageFilePath(); break;
      case hmtUnlockRequest  : s.add("unlock request");  filePath = db.getRequestMessageFilePath();  break;
      case hmtUnlockComplete : s.add("unlock complete"); filePath = db.getResponseMessageFilePath(); break;
      default : return false;
    }
  
    try { FileUtils.writeLines(filePath.toFile(), s); }
    catch (IOException e) { return false; }
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static InterComputerMsg checkForMessage(FilePath filePath)
  {
    List<String> s;
    String source, dest;
    HDB_MessageType type;
     
    if (filePath.exists())
    {
      try { s = FileUtils.readLines(filePath.toFile(), "UTF-8"); } 
      catch (IOException e) { return null; }
      
      if (s.size() != 3) return null;
      
      if (s.get(0).equals(getComputerName()))
        return null;

      source = s.get(0);
      dest = s.get(1);

      switch (s.get(2))
      {
        case "echo request" : type = hmtEchoRequest; break;
        case "echo reply" : type = hmtEchoReply; break;
        case "unlock request" : type = hmtUnlockRequest; break;
        case "unlock complete" : type = hmtUnlockComplete; break;
        default : type = hmtNone;
      }
      
      return new InterComputerMsg(source, dest, type);
    }
  
    return null;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
