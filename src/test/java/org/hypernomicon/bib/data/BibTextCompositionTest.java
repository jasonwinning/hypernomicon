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
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.records.*;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Bibliographic text is stored in canonical composition (NFC) form wherever it is assigned (see
 * {@link BibData#setStr}), so that the same letter spelled precomposed by one source and decomposed by
 * another never registers as a difference. Each test feeds a store the decomposed spelling and expects the
 * composed one back.
 */
class BibTextCompositionTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

  // The same title with ė as the precomposed letter (U+0117) and as e followed by a combining dot above (U+0307),
  // and the same first name with ĭ as the precomposed letter (U+012D) and as i followed by a combining breve (U+0306)

  private static final String TITLE_NFC = "Problemy poėtiki Dostoevskogo",
                              TITLE_NFD = "Problemy poėtiki Dostoevskogo",
                              FIRST_NFC = "Mikhaĭl",
                              FIRST_NFD = "Mikhaĭl";

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
    TestHyperDB.closeIfOpen();  // Leave none of the records created here for the next test class
  }

//---------------------------------------------------------------------------

  @Test
  void standaloneDataComposesEveryField()
  {
    GUIBibData bd = new GUIBibData();

    bd.setStr(bfPublisher, TITLE_NFD);
    bd.setMultiStr(bfTitle, List.of(TITLE_NFD));
    bd.addStr(bfMisc, TITLE_NFD);
    bd.authors.setOneLiner(FIRST_NFD + " Bakhtin");

    assertEquals(TITLE_NFC, bd.getStr(bfPublisher));
    assertEquals(List.of(TITLE_NFC), bd.getMultiStr(bfTitle));
    assertEquals(List.of(TITLE_NFC), bd.getMultiStr(bfMisc));
    assertEquals(FIRST_NFC + " Bakhtin", bd.getStr(bfAuthors));
    assertEquals(FIRST_NFC, bd.getAuthors().iterator().next().firstName());
  }

//---------------------------------------------------------------------------

  @Test
  void spellingsThatDifferOnlyInCompositionAreEqualFields()
  {
    GUIBibData composed = new GUIBibData(), decomposed = new GUIBibData();

    composed  .setMultiStr(bfTitle, List.of(TITLE_NFC));
    decomposed.setMultiStr(bfTitle, List.of(TITLE_NFD));

    composed  .setStr(bfPublisher, TITLE_NFC);
    decomposed.setStr(bfPublisher, TITLE_NFD);

    assertTrue(composed.fieldsAreEqual(bfTitle    , decomposed, false));
    assertTrue(composed.fieldsAreEqual(bfPublisher, decomposed, false));
  }

//---------------------------------------------------------------------------

  @Test
  void workRecordComposesOnAssignment()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);

    work.setName(TITLE_NFD);
    work.setMiscBib(TITLE_NFD);

    assertEquals(TITLE_NFC, work.name());
    assertEquals(TITLE_NFC, work.getMiscBib());

    GUIBibData composed = new GUIBibData();
    composed.setMultiStr(bfTitle, List.of(TITLE_NFC));

    assertTrue(work.getBibData().fieldsAreEqual(bfTitle, composed, false));

    // Assigning the decomposed spelling of what the record already holds changes nothing, so it is not a modification

    assertFalse(HDI_OnlineString.valueChanged(TITLE_NFC, TITLE_NFD));
  }

//---------------------------------------------------------------------------

  /**
   * Loading passes through the same stores as assignment, so a database holding decomposed text written by an
   * earlier version converges on its next save
   */
  @Test
  void workRecordComposesOnLoad() throws Exception
  {
    RecordState state = new RecordState(hdtWork, db.getNextID(hdtWork));

    ((HDI_OfflineString) state.items.get(tagTitle  )).set(TITLE_NFD);
    ((HDI_OfflineString) state.items.get(tagMiscBib)).set(TITLE_NFD);

    HDT_Work work = db.createNewRecordFromState(state, true);

    assertEquals(TITLE_NFC, work.name());
    assertEquals(TITLE_NFC, work.getMiscBib());
  }

//---------------------------------------------------------------------------

  @Test
  void personNamesCompose()
  {
    assertEquals(FIRST_NFC, new PersonName(FIRST_NFD, "Bakhtin").getFirst());
    assertEquals(new PersonName("Bakhtin, " + FIRST_NFC), new PersonName("Bakhtin, " + FIRST_NFD));

    HDT_Person person = db.createNewBlankRecord(hdtPerson);

    person.setName(new PersonName(FIRST_NFD, "Bakhtin"));

    assertEquals(FIRST_NFC, person.getFirstName());
    assertEquals(FIRST_NFC + " Bakhtin", person.getName().getFull());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
