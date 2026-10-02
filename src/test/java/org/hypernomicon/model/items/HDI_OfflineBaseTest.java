/*
 * Copyright 2015-2026 Jason Winning
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

import static org.hypernomicon.model.HDI_Schema.HyperDataCategory.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.items.HDI_OfflineBase.HDX_INDENT;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.RecordState;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Pins the XML written for a pointer tag that carries nested items, in particular that it keeps
 * the subject position ({@code ord}) the way a pointer tag without nested items does. No single
 * pointer relation has nested items today, so the loader's own tests cannot reach this writer.
 */
class HDI_OfflineBaseTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String NL = System.lineSeparator();

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    TestHyperDB.instance();  // Offline items resolve their schema against the database
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDI_OfflineString nestedPages(String pages)
  {
    HDI_OfflineString item = new HDI_OfflineString(new HDI_Schema(hdcString, rtWorkOfArgument, tagPages), new RecordState(hdtArgument));
    item.set(pages);
    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void aPositionIsWrittenBesideTheNestedItems()
  {
    StringBuilder xml = new StringBuilder();

    HDI_OfflineBase.writePointerTagWithNestedPointers(xml, tagWork, 5, 2, "Title", Map.of(tagPages, nestedPages("12-14")));

    assertEquals(HDX_INDENT + "<" + tagWork.name + " id=\"5\" ord=\"2\">Title" + NL +
                 HDX_INDENT + HDX_INDENT + "<" + tagPages.name + ">12-14</" + tagPages.name + ">" + NL +
                 HDX_INDENT + "</" + tagWork.name + ">" + NL, xml.toString());
  }

//---------------------------------------------------------------------------

  @Test
  void aPositionIsWrittenWhenThereTurnOutToBeNoNestedItems()
  {
    StringBuilder xml = new StringBuilder();

    HDI_OfflineBase.writePointerTagWithNestedPointers(xml, tagWork, 5, 2, "Title", Map.of());

    assertEquals(HDX_INDENT + "<" + tagWork.name + " id=\"5\" ord=\"2\">Title</" + tagWork.name + ">" + NL, xml.toString());
  }

//---------------------------------------------------------------------------

  @Test
  void noPositionIsWrittenWithoutOne()
  {
    StringBuilder xml = new StringBuilder();

    HDI_OfflineBase.writePointerTagWithNestedPointers(xml, tagWork, 5, "Title", Map.of(tagPages, nestedPages("12-14")));

    assertEquals(HDX_INDENT + "<" + tagWork.name + " id=\"5\">Title" + NL +
                 HDX_INDENT + HDX_INDENT + "<" + tagPages.name + ">12-14</" + tagPages.name + ">" + NL +
                 HDX_INDENT + "</" + tagWork.name + ">" + NL, xml.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
