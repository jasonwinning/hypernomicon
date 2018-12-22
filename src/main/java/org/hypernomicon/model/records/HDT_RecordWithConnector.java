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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.items.MainText.DisplayItemType.*;
import static org.hypernomicon.model.HyperDB.*;

import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.KeyWork;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.items.MainText.DisplayItem;
import org.hypernomicon.model.items.Connector;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;

public abstract class HDT_RecordWithConnector extends HDT_Record implements HDT_RecordWithDescription
{
  protected Connector connector; // If you set it to null here, this line executes immediately AFTER super constructor is called, and problems ensue.
  private boolean alreadyModifying;
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_RecordWithConnector(HDT_RecordState xmlState, HyperDataset<? extends HDT_RecordWithConnector> dataset, Tag nameTag)
  {
    super(xmlState, dataset, nameTag);
    
    if (connector == null)
      connector = new Connector(this);
    
    alreadyModifying = false;
  }
 
//---------------------------------------------------------------------------
  
  @Override public final MainText getDesc() { return connector.getMainText(); }
  public MainText getMainText()             { return connector.getMainText(); }
  public void initConnector()               { if (connector == null) connector = new Connector(this); }
  public StrongLink getLink()     { return connector.getLink(); }
  public HDT_Hub getHub()         { return connector.getHub(); }  
  public boolean isLinked()       { return connector.isLinked(); }
  public Connector getConnector() { return connector; }


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire() 
  {
    boolean expiringHub = false;   
    HDT_Hub hub = null;
    
    if (isLinked())
    {     
      StrongLink link = connector.getLink();
      hub = link.getHub();
      
      int cnt = 0;
      if (link.getDebate() != null) cnt++;
      if (link.getLabel() != null) cnt++;
      if (link.getNote() != null) cnt++;
      if (link.getPosition() != null) cnt++;
      if (link.getConcept() != null) cnt++;
      
      if (cnt == 2) expiringHub = true;
    }
    
    for (KeyWork keyWork : getMainText().getKeyWorks())
    {
      if (expiringHub) db.handleKeyWork(hub, keyWork.getRecord(), false); // hub is also getting deleted after this; go ahead and remove it from index
      db.handleKeyWork(this, keyWork.getRecord(), false);
    }
    
    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final void modifyNow()
  {
    if (alreadyModifying) return;
    
    super.modifyNow();
    
    if (db.runningConversion) return;    

    alreadyModifying = true;
   
    connector.modifyNow();

    alreadyModifying = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addParentDisplayRecord()
  {        
    if (getMainText().getPlain().trim().length() > 0) return;

    HDT_RecordWithConnector parent = null;
    List<DisplayItem> displayItems = getMainText().getDisplayItemsCopy();
    HDT_RecordType type = getType();

    for (DisplayItem displayItem : displayItems)
      if (displayItem.type == diRecord)
        return;
    
    switch (type)
    {
      case hdtPosition:
        
        HDT_Position position = (HDT_Position) this;
        
        if (position.debates.isEmpty() == false)
          parent = position.debates.get(0);
        else if (position.largerPositions.isEmpty() == false)
          parent = position.largerPositions.get(0);
        
        break;
        
      case hdtArgument:
        
        HDT_Argument argument = (HDT_Argument) this;
        
        if (argument.positions.isEmpty() == false)
          parent = argument.positions.get(0);
        else if (argument.counteredArgs.isEmpty() == false)
          parent = argument.counteredArgs.get(0);
        
        break;
        
      case hdtDebate:
        
        HDT_Debate debate = (HDT_Debate) this;
        
        if (debate.largerDebates.isEmpty() == false)
          parent = debate.largerDebates.get(0);
        
        break;
        
      default: break;
    }
    
    if (parent == null) return;
    
    boolean rc = db.runningConversion;
    db.runningConversion = true;
    displayItems.add(new DisplayItem(parent));
    getMainText().setDisplayItemsFromList(displayItems);
    db.runningConversion = rc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
