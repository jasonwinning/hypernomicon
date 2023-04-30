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

package org.hypernomicon.query.ui;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.query.ui.ResultsTable.*;
import static org.hypernomicon.query.ui.ResultColumn.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet;
import org.hypernomicon.query.ui.ColumnGroupItem.NonGeneralColumnGroupItem;

/**
 * <p>A column group is a collection of result columns that can be made visible or invisible all
 * at once using the column display options popup window.
 * <br>
 * <p>A column group has one or more column group items; there is one column group item to a
 * check box in the column display options popup window. Each ResultColumn is associated with
 * one or more column group items. It is associated with more than one column group item when
 * multiple record types have items with the same tag.
 *
 */
class ColumnGroup extends AbstractColumnGroup<ColumnGroupItem>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<ColumnGroupItem> items;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ColumnGroup(String caption, ResultsTable resultsTable)
  {
    super(caption, resultsTable);

    items = new ArrayList<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addColumn(ResultColumn col)
  {
    addColumn(col, false);
  }

  void addColumn(ResultColumn col, boolean addToFront)
  {
    resultsTable.addColumn(col, addToFront);

    add(new ColumnGroupItem(col));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  interface NonGeneralColumnGroup
  {
    void addColumnsToTable();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class RecordTypeColumnGroup extends AbstractColumnGroup<NonGeneralColumnGroupItem> implements NonGeneralColumnGroup
  {
    private final RecordType recordType;
    private final List<NonGeneralColumnGroupItem> items;

    RecordTypeColumnGroup(RecordType recordType, Set<Tag> tags, ResultsTable resultsTable)
    {
      super(getTypeName(recordType), resultsTable);

      this.recordType = recordType;

      items = new ArrayList<>();

      tags.forEach(tag -> add(new NonGeneralColumnGroupItem(db.mainTextTagForRecordType(recordType) == tag ? tagMainText : tag)));

      RelationSet.getRelationsForObjType(recordType, false).forEach(relType -> add(new NonGeneralColumnGroupItem(relType)));
    }

//---------------------------------------------------------------------------

    /**
     * Loops through all items in the column group.
     * <br>For each of those, see if there is already an item in another RecordTypeColumnGroup with
     * <br>the same tag (not tagNone).
     * <br>If so, set this item's column equal to that one.
     * <br>Otherwise, add a new column.
     */
    @Override public void addColumnsToTable()
    {
      for (NonGeneralColumnGroupItem item : this)
      {
        if (item.tag == tagName) continue; // The record name tag is in the General group so we don't add it here.

        NonGeneralColumn col = null;
        EnumMap<RecordType, NonGeneralColumnGroupItem> map = new EnumMap<>(RecordType.class); // Keep track of all the items with the same tag; index by record type
                                                                                              // Indexing by record type is necessary for column cell factory
        map.put(recordType, item);

        if (item.tag != tagNone) // Relation subject columns will sometimes have tagNone
        {
          for (AbstractColumnGroup<? extends ColumnGroupItem> grp : colGroups)
          {
            if (this == grp)
              continue;

            for (ColumnGroupItem oItem : grp)
            {
              if ((oItem instanceof NonGeneralColumnGroupItem) == false)
                continue;

              NonGeneralColumnGroupItem otherItem = (NonGeneralColumnGroupItem)oItem;

              if (item.tag != otherItem.tag)
                continue;

              RecordTypeColumnGroup rtcGroup = (RecordTypeColumnGroup)grp;
              map.put(rtcGroup.recordType, otherItem);

              if (otherItem.col != null)
              {
                col = NonGeneralColumn.class.cast(otherItem.col);

                if (item.relType == rtNone) // Only subject columns have a relType set. They are invisible by default.
                  col.setVisible(true);

                col.map.putAll(map);
                map = col.map;
              }
            }
          }
        }

        if (col == null)
          col = resultsTable.addNonGeneralColumn(map);

        for (ColumnGroupItem otherItem : map.values())
          otherItem.col = col;
      }
    }

//---------------------------------------------------------------------------

    @Override protected Collection<NonGeneralColumnGroupItem> delegate() { return items; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class BibFieldsColumnGroup extends ColumnGroup implements NonGeneralColumnGroup
  {
    BibFieldsColumnGroup(ResultsTable resultsTable)
    {
      super("Reference Manager Fields", resultsTable);
    }

    @Override public void addColumnsToTable()
    {
      List.of(bfEntryType, bfContainerTitle, bfPublisher, bfPubLoc, bfEdition, bfVolume, bfIssue, bfLanguage, bfISSNs, bfPages).forEach(field -> addColumn(new BibFieldColumn(field)));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected Collection<ColumnGroupItem> delegate() { return items; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
