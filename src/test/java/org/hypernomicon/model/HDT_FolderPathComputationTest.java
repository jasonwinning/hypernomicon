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

package org.hypernomicon.model;

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Tests for the dynamic {@code filePath()} cascade on {@link HDT_Folder}.
 * <p>
 * {@code filePath()} walks the parent-chain of folder records on every call
 * rather than caching a resolved path. Re-parenting a single folder record
 * therefore immediately updates the computed path of every descendant without
 * any explicit change to those records. These tests encode that architectural
 * invariant.
 * <p>
 * All tests use {@link TestHyperDB} (in-memory only; no real filesystem I/O).
 */
class HDT_FolderPathComputationTest
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

//---------------------------------------------------------------------------

  @BeforeEach
  void resetDB()
  {
    db.closeAndOpen();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void filePath_reflectsParentChain()
  {
    HDT_Folder root  = db.getRootFolder(),
               child = db.createNewBlankRecord(hdtFolder);

    child.getPath().assign(root, FilePath.of("testchild"));

    FilePath childPath = child.filePath();

    assertNotNull(childPath);
    assertEquals(db.getRootPath().resolve("testchild"), childPath);
  }

//---------------------------------------------------------------------------

  @Test
  void reparent_descendantFilePathsUpdateAutomatically()
  {
    HDT_Folder root   = db.getRootFolder(),
               parent = db.createNewBlankRecord(hdtFolder),
               child  = db.createNewBlankRecord(hdtFolder),
               dest   = db.createNewBlankRecord(hdtFolder);

    parent.getPath().assign(root,   FilePath.of("parent"));
    child .getPath().assign(parent, FilePath.of("child"));
    dest  .getPath().assign(root,   FilePath.of("dest"));

    assertEquals(db.getRootPath().resolve("parent", "child"), child.filePath());

    // Re-parent parent under dest; child record is never touched

    parent.getPath().assign(dest, FilePath.of("parent"));

    assertEquals(db.getRootPath().resolve("dest", "parent", "child"), child.filePath());
  }

//---------------------------------------------------------------------------

  @Test
  void reparent_deeplyNestedDescendant_updatesCorrectly()
  {
    HDT_Folder root = db.getRootFolder(),
               lvl1 = db.createNewBlankRecord(hdtFolder),
               lvl2 = db.createNewBlankRecord(hdtFolder),
               lvl3 = db.createNewBlankRecord(hdtFolder),
               dest = db.createNewBlankRecord(hdtFolder);

    lvl1.getPath().assign(root, FilePath.of("lvl1"));
    lvl2.getPath().assign(lvl1, FilePath.of("lvl2"));
    lvl3.getPath().assign(lvl2, FilePath.of("lvl3"));
    dest.getPath().assign(root, FilePath.of("dest"));

    // Move lvl1 under dest; lvl3 is three hops away and is never touched

    lvl1.getPath().assign(dest, FilePath.of("lvl1"));

    assertEquals(db.getRootPath().resolve("dest", "lvl1", "lvl2", "lvl3"), lvl3.filePath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
