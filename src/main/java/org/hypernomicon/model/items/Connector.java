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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.records.HDT_Hub;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_RecordWithConnector;

/**
 * Every record that has a main HTML description field has its own object of
 * this class. Some of those record types, but not all, also can be "united"
 * to other records so that they have the same {@link MainText MainText} object.
 * They then still have separate Connector objects but each refers to the same
 * {@link MainText MainText} and {@link StrongLink StrongLink} objects.
 *
 * The reason for not folding this functionality into the
 * {@link org.hypernomicon.model.records.HDT_RecordWithConnector HDT_RecordWithConnector}
 * class is that it has to be in the same package as {@link StrongLink StrongLink},
 * and it is safer for both of those classes to not be in the same package as the
 * record classes.
 *
 * @author  Jason Winning
 * @since   1.0
 */
public final class Connector
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Connector(HDT_RecordWithConnector record)
  {
    this.record = record;
    mainText = new MainText(this);

    if (record.getType() == hdtHub)
      link = new StrongLink((HDT_Hub) record);  // This only gets used if the record will be loaded from a record state; a new StrongLink is created
  }                                             // that overwrites this one in StrongLink.connectRecords

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final HDT_RecordWithConnector record;
  StrongLink link;
  MainText mainText;
  private boolean alreadyModifying = false;

  public RecordType getType()                { return getSpoke().getType(); }
  public boolean isLinked()                  { return link != null; }
  public StrongLink getLink()                { return link; }
  public MainText getMainText()              { return mainText; }
  public HDT_Hub getHub()                    { return link == null ? null : link.getHub(); }
  public HDT_RecordWithConnector getSpoke()  { return record; }
  public String listName()                   { return record == null ? "" : record.listName(); }
  public static boolean isEmpty(Connector c) { return (c == null) || HDT_Record.isEmpty(c.getSpoke()); }

  @Override public int hashCode()            { return record == null ? 0 : (31 * record.hashCode()); }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void modifyNow()
  {
    if (db.runningConversion || alreadyModifying) return;

    alreadyModifying = true;

    if (isLinked()) link.modifyNow();
    record.modifyNow();

    alreadyModifying = false;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------


  void resolvePointers() throws HDB_InternalError
  {
    if (HDT_Record.isEmptyThrowsException(getHub()))
      link = null;

    mainText.resolvePointers();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void expire()
  {
    if (getType() == hdtHub) return;

    if (isLinked())
      link.disconnectRecord(getType(), false);

    mainText.expire();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    return record == ((Connector) obj).record;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void initFromHub(HDT_Hub hub)
  {
    link = hub.getLink();

    db.replaceMainText(mainText, hub.getMainText());

    mainText = hub.getMainText();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
