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

package org.hypernomicon.model;

import org.hypernomicon.model.records.HDT_RecordType;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

@SuppressWarnings("serial")
public class Exceptions
{
  public static class InvalidItemException extends Exception
  {
    private int recordID;
    private HDT_RecordType recordType;
    private String itemName;

  //---------------------------------------------------------------------------
    
    public int getRecordID()              { return recordID; }
    public HDT_RecordType getRecordType() { return recordType; }
    public String getItemName()           { return itemName; }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    public InvalidItemException(int recordID, HDT_RecordType recordType, String itemName)
    {
      super("Invalid item tag: \"" + itemName + "\". Record type: " + db.getTypeTagStr(recordType) + " ID : " + recordID);
      
      this.recordID = recordID;
      this.recordType = recordType;
      this.itemName = itemName;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class TerminateTaskException extends Exception
  {
    public TerminateTaskException() { super("Task was terminated abnormally."); }
    
    public TerminateTaskException(Throwable cause) { super("Task was terminated abnormally.", cause); }
    
    public TerminateTaskException(String msg) 
    { 
      super(msg);
      messageDialog(msg, mtError);       
    }
    
    public TerminateTaskException(String msg, Throwable cause) 
    { 
      super(msg, cause);
      messageDialog(msg, mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class DuplicateRecordException extends Exception
  {
    private int id;
    private HDT_RecordType type;
    
    public DuplicateRecordException(int id, HDT_RecordType type) 
    {
      super("Duplicate record: type = " + db.getTypeTagStr(type) + ", ID = " + id);
      
      this.id = id; 
      this.type = type; 
    }
    
    public int getID()              { return id; }
    public HDT_RecordType getType() { return type; }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class SearchKeyException extends Exception
  {
    private boolean tooShort;
    private int recordID;
    private HDT_RecordType recordType;
    private String key;
    
    public SearchKeyException(boolean tooShort, int recordID, HDT_RecordType recordType, String key) 
    { 
      super(tooShort ? "Search key: \"" + key + "\" is too short. Record type: " + db.getTypeName(recordType) + " ID : " + recordID :
                       "Duplicate search key: \"" + key + "\". Record type: " + db.getTypeName(recordType) + " ID : " + recordID);
      
      this.tooShort = tooShort;
      this.recordID = recordID;
      this.recordType = recordType;
      this.key = key;
    }    
    public boolean getTooShort()          { return tooShort; }
    public int getRecordID()              { return recordID; }
    public HDT_RecordType getRecordType() { return recordType; }
    public String getKey()                { return key; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class HubChangedException extends Exception
  {
    private int recordID;
    private HDT_RecordType recordType;
    
    public HubChangedException(int recordID, HDT_RecordType recordType, boolean formerlyUnlinked)
    {
      super(formerlyUnlinked ? "The record is now linked to a record that it was not previously linked to." :
                               "The record has been unlinked from a record it was previously linked to.");
      
      this.recordID = recordID;
      this.recordType = recordType;
    }
    
    public int getRecordID()              { return recordID; }
    public HDT_RecordType getRecordType() { return recordType; }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class RelationCycleException extends Exception
  {
    private int childID, parentID;
    private HDT_RecordType childType, parentType;

    public RelationCycleException(int childID, HDT_RecordType childType, int parentID, HDT_RecordType parentType) 
    { 
      super("Unable to assign " + db.getTypeName(childType) + " ID " + childID + " as child of " +
            db.getTypeName(parentType) + " ID " + parentID + ": A cycle would result.");
      
      this.childID = childID;
      this.childType = childType;
      this.parentID = parentID;
      this.parentType = parentType;
    }

    public int getChildID()               { return childID; }
    public int getParentID()              { return parentID; }
    public HDT_RecordType getChildType()  { return childType; }
    public HDT_RecordType getParentType() { return parentType; }
  }
  
//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
  
  public static class HyperDataException extends Exception
  {
    public HyperDataException(Throwable e)             { super(e.getMessage(), e); }
    public HyperDataException()                        { super();                  }        
    public HyperDataException(String msg, Throwable e) { super(msg, e);            }
    public HyperDataException(String msg)              { super(msg);               }    
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 

  public static class HDB_InternalError extends Exception
  {
    private int num;
  
    public HDB_InternalError(int newNum)
    {      
      super("Internal error #" + String.valueOf(newNum));
      num = newNum;      
    }
    
    public HDB_InternalError(int newNum, String msg)
    {
      super("Internal error #" + String.valueOf(newNum) + ": " + msg);
      num = newNum;
    }
    
    public int getNum() { return num; }
  }
  
//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 

}