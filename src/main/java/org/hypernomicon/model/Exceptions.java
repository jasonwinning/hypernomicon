/*
 * Copyright 2015-2022 Jason Winning
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

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

@SuppressWarnings("serial")
public final class Exceptions
{
  private Exceptions() { throw new UnsupportedOperationException(); }

  public static class InvalidItemException extends Exception
  {
    public InvalidItemException(int recordID, RecordType recordType, String itemName)
    {
      super("Invalid item tag: \"" + itemName + "\". Record type: " + db.getTypeTagStr(recordType) + " ID : " + recordID);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class TerminateTaskException extends Exception
  {
    public TerminateTaskException() { super("Task was terminated abnormally."); }

    public TerminateTaskException(String msg)
    {
      super(msg);
      messageDialog(msg, mtError, true);
    }

    public TerminateTaskException(String msg, Throwable cause)
    {
      super(msg, cause);
      messageDialog(msg, mtError, true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class DuplicateRecordException extends Exception
  {
    DuplicateRecordException(int id, RecordType type)
    {
      super("Duplicate record: type = " + db.getTypeTagStr(type) + ", ID = " + id);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class SearchKeyException extends Exception
  {
    private final boolean tooShort;

    SearchKeyException(boolean tooShort, HDT_Record record, String key)
    {
      super(tooShort ? "Search key: \"" + key + "\" is too short. Record type: " + db.getTypeName(record.getType()) + " ID : " + record.getID() :
                       "Duplicate search key: \"" + key + "\". Record type: " + db.getTypeName(record.getType()) + " ID : " + record.getID());

      this.tooShort = tooShort;
    }

    public boolean getTooShort() { return tooShort; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class RestoreException extends Exception { public RestoreException(String msg) { super(msg); } }

  public static class ConceptChangedException extends RestoreException
  {
    public ConceptChangedException()
    {
      super("The set of concept records associated with this term record has changed.");
    }
  }

  public static class HubChangedException extends RestoreException
  {
    public HubChangedException(boolean formerlyUnlinked)
    {
      super(formerlyUnlinked ? "The record is now united with a record that it was not previously united with." :
                               "The record has been disunited from a record it was previously united with.");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class RelationCycleException extends Exception
  {
    public RelationCycleException(HDT_Record child, HDT_Record parent)
    {
      super("Unable to assign " + db.getTypeName(child.getType()) + " ID " + child.getID() + " as child of " +
            db.getTypeName(parent.getType()) + " ID " + parent.getID() + ": A cycle would result.");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class HyperDataException extends Exception
  {
    public HyperDataException(String msg, Throwable e) { super(msg, e);            }
    HyperDataException(Throwable e)                    { super(e.getMessage(), e); }
    HyperDataException(String msg)                     { super(msg);               }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class HDB_InternalError extends Exception
  {
    public HDB_InternalError(int newNum)
    {
      super("Internal error #" + newNum);
    }

    HDB_InternalError(int newNum, String msg)
    {
      super("Internal error #" + newNum + ": " + msg);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
