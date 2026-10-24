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

package org.hypernomicon.fts;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.*;
import java.util.List;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * Tests for {@link DatabaseSketch}: which records a sketch is built from, and when
 * two sketches count as the same database.
 */
class DatabaseSketchTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

  @TempDir
  Path tempDir;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    db.closeAndOpen();  // Leave no records behind for other test classes
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void computeRecordsEachTypesIDsAndLeavesOutTheRootRecords() throws Exception
  {
    db.closeAndOpen();

    HDT_Person person1 = db.createNewBlankRecord(hdtPerson),
               person2 = db.createNewBlankRecord(hdtPerson);

    HDT_Note note = db.createNewBlankRecord(hdtNote);

    DatabaseSketch sketch = DatabaseSketch.compute(db.allRecords());
    FilePath file = FilePath.of(tempDir.resolve("sketch.json"));

    sketch.writeTo(file);

    JsonObj typeToIDs = JsonObj.parseJsonObj(Files.readString(file.toPath())).getObj("typeToIDArray");

    assertEquals(idStrs(person1, person2), idStrsIn(typeToIDs, hdtPerson));
    assertEquals(idStrs(note), idStrsIn(typeToIDs, hdtNote), "The root note is never saved, so the sketch should leave it out.");
    assertNull(typeToIDs.getArray(getTypeTagStr(hdtDebate)), "The root debate is the only debate and is never saved, so the sketch should have no debates.");

    assertTrue(DatabaseSketch.readFrom(file).isVerySimilarTo(sketch), "A sketch read back from its file should match the sketch that wrote it.");
  }

//---------------------------------------------------------------------------

  /**
   * Similarity is the share of all records, across types, that both sketches have.
   * The sketches are read from files in the format sketch.json has always had.
   */
  @Test
  void sketchesAreVerySimilarWhenTheyShareFourFifthsOfTheirRecords() throws IOException
  {
    DatabaseSketch eightPersons      = sketchFromFile("{\"person\": [1, 2, 3, 4, 5, 6, 7, 8]}"),
                   plusTwoWorks      = sketchFromFile("{\"person\": [1, 2, 3, 4, 5, 6, 7, 8], \"work\": [1, 2]}"),
                   plusThreeWorks    = sketchFromFile("{\"person\": [1, 2, 3, 4, 5, 6, 7, 8], \"work\": [1, 2, 3]}"),
                   twoPersonsSwapped = sketchFromFile("{\"person\": [1, 2, 3, 4, 5, 6, 9, 10]}");

    assertTrue (eightPersons.isVerySimilarTo(plusTwoWorks), "8 shared of 10 in all is exactly the threshold, which should count as very similar.");
    assertTrue (plusTwoWorks.isVerySimilarTo(eightPersons), "Similarity should not depend on which sketch is compared with which.");
    assertFalse(eightPersons.isVerySimilarTo(plusThreeWorks), "8 shared of 11 in all is below the threshold.");
    assertFalse(eightPersons.isVerySimilarTo(twoPersonsSwapped), "6 shared of 10 is below the threshold.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private DatabaseSketch sketchFromFile(String typeToIDArrayJson) throws IOException
  {
    Path file = Files.createTempFile(tempDir, "sketch", ".json");

    Files.writeString(file, "{\"version\": 1, \"typeToIDArray\": " + typeToIDArrayJson + '}');

    return DatabaseSketch.readFrom(FilePath.of(file));
  }

//---------------------------------------------------------------------------

  private static List<String> idStrs(HDT_Record... records)
  {
    return Stream.of(records).mapToInt(HDT_Record::getID).sorted().mapToObj(String::valueOf).toList();
  }

//---------------------------------------------------------------------------

  private static List<String> idStrsIn(JsonObj typeToIDs, RecordType type)
  {
    JsonArray idArray = typeToIDs.getArray(getTypeTagStr(type));

    return IntStream.range(0, idArray.size()).mapToObj(idArray::getAsStr).toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
