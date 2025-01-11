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

package org.hypernomicon.bib;

import org.hypernomicon.bib.CollectionTree.BibCollectionType;

import static org.hypernomicon.bib.CollectionTree.BibCollectionType.*;
import javafx.scene.control.TreeItem;

class BibCollectionRow
{
  private final TreeItem<BibCollectionRow> treeItem;
  private final String key;
  private final BibCollectionType type;

  private BibCollection coll;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  BibCollectionRow(BibCollectionType type) { this(type, null); }
  BibCollectionRow(BibCollection coll)     { this(bctUser, coll); }

  TreeItem<BibCollectionRow> getTreeItem() { return treeItem; }
  BibCollectionType getType()              { return type; }
  String getKey()                          { return key; }
  void updateCollObj(BibCollection coll)   { this.coll = coll; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BibCollectionRow(BibCollectionType type, BibCollection coll)
  {
    treeItem = new TreeItem<>(this);
    this.type = type;
    this.coll = coll;

    key = coll == null ? null : coll.getKey();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getSortKey()
  {
    return switch (type)
    {
      case bctAll           -> "\u00001";
      case bctAllAssigned   -> "\u00002";
      case bctAllUnassigned -> "\u00003";
      case bctUnsorted      -> "\u00004";
      case bctTrash         -> "\uffff";
      default               -> coll.getName();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getText()
  {
    return switch (type)
    {
      case bctAll           -> "All Entries";
      case bctAllAssigned   -> "All Entries Assigned to Work Records";
      case bctAllUnassigned -> "All Entries Not Assigned to Work Records";
      case bctUnsorted      -> "Unsorted";
      case bctTrash         -> "Trash";
      default               -> coll.getName();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
