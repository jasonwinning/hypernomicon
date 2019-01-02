/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.bib;

import org.hypernomicon.bib.CollectionTree.BibCollectionType;
import org.hypernomicon.bib.lib.BibCollection;

import static org.hypernomicon.bib.CollectionTree.BibCollectionType.*;
import javafx.scene.control.TreeItem;

public class BibCollectionRow
{
  private final TreeItem<BibCollectionRow> treeItem;
  private final String key;
  private final BibCollectionType type;
  
  private BibCollection coll = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibCollectionRow(BibCollectionType type) { this(type, null); }
  public BibCollectionRow(BibCollection coll)     { this(bctUser, coll); }
  
  public TreeItem<BibCollectionRow> getTreeItem() { return treeItem; }
  public BibCollectionType getType()              { return type; }
  public BibCollection getCollection()            { return coll; }
  public String getKey()                          { return key; }
  public void updateCollObj(BibCollection coll)   { this.coll = coll; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BibCollectionRow(BibCollectionType type, BibCollection coll)
  {
    treeItem = new TreeItem<>(this);
    this.type = type;
    this.coll = coll;
    
    key = coll == null ? null : coll.getCollectionKey();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getSortKey()
  {
    switch (type)
    {
      case bctAll:      return "\u00001";
      case bctUnsorted: return "\u00002";
      case bctTrash:    return "\uffff";
      default:          return coll.getName();      
    }
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public String getText()
  {
    switch (type)
    {
      case bctAll:      return "All Entries";
      case bctUnsorted: return "Unsorted";
      case bctTrash:    return "Trash";
      default:          return coll.getName();      
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
