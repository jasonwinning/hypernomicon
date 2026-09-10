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

package org.hypernomicon.bib.data;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.BibField.BibFieldType.*;
import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.EnumSet;
import java.util.List;
import java.util.stream.Collectors;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.HDT_Work;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Where a work's bibliographic fields go with and without a reference manager entry. A work
 * record stores some fields itself; the rest can be stored only in an entry, and the
 * classification in {@link BibFieldEnum#requiresBibEntry()} has to agree with what
 * {@link WorkBibData} actually keeps.
 */
class WorkBibDataTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    TestHyperDB.closeIfOpen();  // Start from a known state; an earlier test class may have left a library linked

    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    TestHyperDB.closeIfOpen();  // Leave neither a linked library nor the works created here for the next test class
  }

//---------------------------------------------------------------------------

  private static String sampleStr(BibFieldEnum bibFieldEnum)
  {
    return bibFieldEnum == bfDOI ? "10.1234/abcd" : "x";  // The DOI setter keeps only what looks like a DOI
  }

  private static List<String> sampleList(BibFieldEnum bibFieldEnum)
  {
    return List.of(bibFieldEnum == bfISBNs ? "9780306406157" : "x");  // The ISBN setter keeps only valid ISBNs
  }

//---------------------------------------------------------------------------

  @Test
  void classificationIsExplicit()
  {
    EnumSet<BibFieldEnum> expected = EnumSet.of(bfEntryType, bfContainerTitle, bfEdition, bfISSNs, bfVolume, bfIssue, bfLanguage, bfPages, bfPubLoc, bfPublisher),

                          actual = EnumSet.allOf(BibFieldEnum.class).stream().filter(BibFieldEnum::requiresBibEntry)
                                                                             .collect(Collectors.toCollection(() -> EnumSet.noneOf(BibFieldEnum.class)));

    assertEquals(expected, actual);
  }

//---------------------------------------------------------------------------

  /**
   * With no entry, a write to a field that requires one is dropped, and every other text field round-trips
   */
  @Test
  void workWithoutEntryKeepsOnlyItsOwnFields()
  {
    db.unlinkBibLibrary();  // The tests in this class do not run in a fixed order

    assertFalse(db.bibLibraryIsLinked());

    HDT_Work work = db.createNewBlankRecord(hdtWork);
    BibData bd = work.getBibData();

    assertInstanceOf(WorkBibData.class, bd);

    for (BibFieldEnum bibFieldEnum : BibFieldEnum.values())
    {
      if (bibFieldEnum.getType() == bftString)
        bd.setStr(bibFieldEnum, sampleStr(bibFieldEnum));
      else if (bibFieldEnum.getType() == bftMultiString)
        bd.setMultiStr(bibFieldEnum, sampleList(bibFieldEnum));
      else
        continue;

      assertEquals(bibFieldEnum.requiresBibEntry() == false, bd.fieldNotEmpty(bibFieldEnum), bibFieldEnum.name());
    }
  }

//---------------------------------------------------------------------------

  /**
   * Once an entry is assigned, the work's bibliographic data is that entry, and the same write lands in it
   */
  @Test
  void assignedEntryReceivesTheFields() throws HyperDataException
  {
    for (LibraryType libType : LibraryType.values())
    {
      db.linkBibLibrary(libType, "");

      HDT_Work work = db.createNewBlankRecord(hdtWork);

      BibEntry<?, ?> entry = work.assignNewBibEntry(EntryType.etBook);

      assertEquals(entry.getKey(), work.getBibEntryKey());
      assertSame(entry, work.getBibData());
      assertSame(work, entry.getWork());
      assertEquals(EntryType.etBook, entry.getEntryType());

      work.getBibData().setStr(bfPublisher, "Publisher");

      assertEquals("Publisher", entry.getStr(bfPublisher), libType.userFriendlyName);
      assertEquals("Publisher", work.getBibData().getStr(bfPublisher), libType.userFriendlyName);
    }
  }

//---------------------------------------------------------------------------

  /**
   * While an entry is assigned, the fields a work stores itself live on the work; unassigning
   * copies them back into the entry so that nothing is lost
   */
  @Test
  void unassignedEntryKeepsTheWorkOwnedFields() throws HyperDataException
  {
    for (LibraryType libType : LibraryType.values())
    {
      db.linkBibLibrary(libType, "");

      HDT_Work work = db.createNewBlankRecord(hdtWork);

      BibEntry<?, ?> entry = work.assignNewBibEntry(EntryType.etBook);

      work.setName("Title");
      work.setDOI("10.1234/abcd");
      work.setMiscBib("Note");

      entry.unassignWork();

      assertTrue(work.getBibEntryKey().isEmpty());
      assertNull(entry.getWork());

      assertEquals("Title", entry.getStr(bfTitle), libType.userFriendlyName);
      assertEquals("10.1234/abcd", entry.getStr(bfDOI), libType.userFriendlyName);
      assertEquals(List.of("Note"), entry.getMultiStr(bfMisc), libType.userFriendlyName);
    }
  }

//---------------------------------------------------------------------------

  /**
   * The fields that make a merge ask for an entry follow the same classification
   */
  @Test
  void externalFieldsFollowTheClassification()
  {
    GUIBibData bd = new GUIBibData();

    bd.setTitle("Title");
    bd.setStr(bfDOI, "10.1234/abcd");
    bd.setStr(bfPublisher, "Publisher");
    bd.setMultiStr(bfContainerTitle, List.of("Journal"));

    assertEquals(EnumSet.of(bfContainerTitle, bfPublisher), bd.fieldsWithExternalData());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
