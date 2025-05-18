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

package org.hypernomicon.model;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

@SuppressWarnings("serial")
public final class Exceptions
{
  private Exceptions() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Logs the stack trace for a throwable, preventing HDB_InternalError
   * or any Throwable having HDB_InternalError in its chain of causes
   * from getting logged twice.
   * @param e The throwable
   */
  public static void log(Throwable e)
  {
    HDB_InternalError internalError = getInternalError(e);

    if ((internalError == null) || internalError.logged.compareAndSet(false, true))
      e.printStackTrace();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDB_InternalError getInternalError(Throwable e)
  {
    Set<Throwable> seen = new HashSet<>();

    while (e != null)
    {
      if (seen.add(e) == false) // If already seen, break to prevent infinite loop
        return null;

      if (e instanceof HDB_InternalError)
        return (HDB_InternalError) e;

      e = e.getCause();
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class CancelledTaskException extends Exception
  {
    public CancelledTaskException() { super("Task was cancelled by user."); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class HyperDataException extends Exception
  {
    public HyperDataException(String msg             ) { super(msg                      ); }
    public HyperDataException(String msg, Throwable e) { super(msg                   , e); }
    public HyperDataException(            Throwable e) { super(getThrowableMessage(e), e); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class InvalidItemException extends HyperDataException
  {
    public InvalidItemException(int recordID, RecordType recordType, String itemName)
    {
      super("Invalid item tag: \"" + itemName + "\". Record type: " + Tag.getTypeTagStr(recordType) + " ID: " + recordID);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class InvalidAttributeException extends HyperDataException
  {
    public InvalidAttributeException(int recordID, RecordType recordType, Tag tag, String attrName)
    {
      super("Invalid XML attribute: \"" + attrName + "\". Item tag: \"" + tag.name + "\". Record type: " + Tag.getTypeTagStr(recordType) + " ID: " + recordID);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class DuplicateRecordException extends HyperDataException
  {
    public DuplicateRecordException(int id, RecordType type)
    {
      super("Duplicate record: type = " + Tag.getTypeTagStr(type) + ", ID = " + id);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static abstract class SearchKeyException extends HyperDataException
  {
    private final String key;

    private SearchKeyException(String msg, String key)
    {
      super(msg);

      this.key = key;
    }

    public String getKey() { return key; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class SearchKeyTooShortException extends SearchKeyException
  {
    public SearchKeyTooShortException(HDT_Record record, String key)
    {
      super("Search key: \"" + key + "\" is too short. Record type: " + getTypeName(record.getType()) + " ID: " + record.getID(), key);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class DuplicateSearchKeyException extends SearchKeyException
  {
    DuplicateSearchKeyException(HDT_Record record, String key)
    {
      super("Duplicate search key: \"" + key + "\". Record type: " + getTypeName(record.getType()) + " ID: " + record.getID(), key);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Exception intended to be thrown in {@link org.hypernomicon.model.records.HDT_Record#restoreTo(RecordState,boolean) HDT_Record.restoreTo},
   * when restoring data to a record from its backup record state.
   * <p>
   * Thrown when existing data in the record (like the record's hub, or a term record's concept records) conflicts with
   * data in the recordState.
   */
  public static class RestoreException extends HyperDataException { RestoreException(String msg) { super(msg); } }

  public static class ConceptChangedException extends RestoreException
  {
    public ConceptChangedException()
    {
      super("The set of concept records associated with this term record has changed.");
    }
  }

  public static class HubChangedException extends RestoreException
  {
    public HubChangedException(boolean formerlyDisunited)
    {
      super(formerlyDisunited ? "The record is now united with a record that it was not previously united with." :
                                "The record has been disunited from a record it was previously united with.");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class RelationCycleException extends HyperDataException
  {
    public RelationCycleException(HDT_Record child, HDT_Record parent)
    {
      super("Unable to assign " + getTypeName(child.getType()) + " ID " + child.getID() + " as child of " +
            getTypeName(parent.getType()) + " ID " + parent.getID() + ": A cycle would result.");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class HDB_InternalError extends HyperDataException
  {
    private final int num;
    private final AtomicBoolean logged = new AtomicBoolean(false);

    public HDB_InternalError(int num)
    {
      this(num, true);
    }

    public HDB_InternalError(int num, Throwable e)
    {
      super(createMessage(num), e);
      this.num = num;
      log(this);
    }

    public HDB_InternalError(int num, boolean immediatelyLog)
    {
      super(createMessage(num));
      this.num = num;

      if (immediatelyLog)
        log(this);
    }

    private static String createMessage(int num)
    {
      return "Internal error #" + String.format("%05d", num);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class HDB_UnrecoverableInternalError extends HDB_InternalError
  {
    public HDB_UnrecoverableInternalError(int num             ) { super(num   ); }
    public HDB_UnrecoverableInternalError(int num, Throwable e) { super(num, e); }

    public HDB_UnrecoverableInternalError(HDB_InternalError e ) { super(e.num, e.getCause()); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
