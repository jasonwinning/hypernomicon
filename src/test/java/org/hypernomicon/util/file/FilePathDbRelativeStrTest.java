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

package org.hypernomicon.util.file;

import org.hypernomicon.model.TestHyperDB;

import org.junit.jupiter.api.*;

import static org.junit.jupiter.api.Assertions.*;

//---------------------------------------------------------------------------

/**
 * Tests for {@link FilePath#toDbRelativeStr()}, the one form in which the application
 * names a file to the user: relative to the database root with forward slashes when
 * the file is under that root, the full native path otherwise. Uses {@link TestHyperDB}
 * for a database with a root folder.
 */
class FilePathDbRelativeStrTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

  /** Reopens the database whether or not the previous test left it open. */
  @BeforeEach
  void resetDB()
  {
    TestHyperDB.closeIfOpen();
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void aFileUnderTheRootIsShownRelativeWithForwardSlashes()
  {
    FilePath filePath = db.getRootPath("Topical").resolve("Notebook.one");

    assertEquals("Topical/Notebook.one", filePath.toDbRelativeStr());
  }

//---------------------------------------------------------------------------

  @Test void aFileOutsideTheRootIsShownInFull()
  {
    FilePath filePath = db.getRootPath().getParent().resolve("elsewhere").resolve("Notebook.one");

    assertEquals(filePath.toString(), filePath.toDbRelativeStr());
  }

//---------------------------------------------------------------------------

  @Test void withNoDatabaseOnlineEveryFileIsShownInFull()
  {
    FilePath filePath = db.getRootPath("Topical").resolve("Notebook.one");

    TestHyperDB.closeIfOpen();

    assertEquals(filePath.toString(), filePath.toDbRelativeStr());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
