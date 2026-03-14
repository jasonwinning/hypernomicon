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

package org.hypernomicon.fileManager;

import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Path;
import java.util.*;

import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.FilePathRegistryTestHelper;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * Tests for {@link FileManager#removeNestedPaths(Set)}, the extracted
 * re-parent-root pruning logic that removes any path from a set whose
 * ancestor is also in the set.
 */
class ReparentRootPruningTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @TempDir
  Path tempDir;

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp()
  {
    FilePathRegistryTestHelper.activateForTesting(tempDir);
  }

//---------------------------------------------------------------------------

  @AfterEach
  void tearDown()
  {
    FilePathRegistryTestHelper.deactivate();
  }

//---------------------------------------------------------------------------

  @Test
  void emptySet_unchanged()
  {
    Set<FilePath> paths = new LinkedHashSet<>();

    FileManager.removeNestedPaths(paths);

    assertTrue(paths.isEmpty());
  }

//---------------------------------------------------------------------------

  @Test
  void singlePath_noPruning()
  {
    Set<FilePath> paths = setOf(p("a"));

    FileManager.removeNestedPaths(paths);

    assertEquals(1, paths.size());
    assertTrue(paths.contains(p("a")));
  }

//---------------------------------------------------------------------------

  @Test
  void parentAndChild_childRemoved()
  {
    Set<FilePath> paths = setOf(p("a"), p("a/b"));

    FileManager.removeNestedPaths(paths);

    assertEquals(Set.of(p("a")), paths);
  }

//---------------------------------------------------------------------------

  @Test
  void parentAndGrandchild_grandchildRemoved()
  {
    Set<FilePath> paths = setOf(p("a"), p("a/b/c"));

    FileManager.removeNestedPaths(paths);

    assertEquals(Set.of(p("a")), paths);
  }

//---------------------------------------------------------------------------

  @Test
  void threeLevelChain_onlyRootSurvives()
  {
    Set<FilePath> paths = setOf(p("a"), p("a/b"), p("a/b/c"));

    FileManager.removeNestedPaths(paths);

    assertEquals(Set.of(p("a")), paths);
  }

//---------------------------------------------------------------------------

  @Test
  void twoIndependentTrees_bothSurvive()
  {
    Set<FilePath> paths = setOf(p("a"), p("b"));

    FileManager.removeNestedPaths(paths);

    assertEquals(Set.of(p("a"), p("b")), paths);
  }

//---------------------------------------------------------------------------

  @Test
  void twoIndependentPlusOneNested_nestedRemoved()
  {
    Set<FilePath> paths = setOf(p("a"), p("a/b"), p("c"));

    FileManager.removeNestedPaths(paths);

    assertEquals(Set.of(p("a"), p("c")), paths);
  }

//---------------------------------------------------------------------------

  @Test
  void sharedPrefix_notParentChild_bothSurvive()
  {
    // "a/bar" is NOT a parent of "a/barbaz" even though they share a prefix

    Set<FilePath> paths = setOf(p("bar"), p("barbaz"));

    FileManager.removeNestedPaths(paths);

    assertEquals(Set.of(p("bar"), p("barbaz")), paths);
  }

//---------------------------------------------------------------------------

  @Test
  void twoSubtreesOfCommonParent_bothSurvive()
  {
    // a/b and a/c are siblings; neither contains the other

    Set<FilePath> paths = setOf(p("a/b"), p("a/c"));

    FileManager.removeNestedPaths(paths);

    assertEquals(Set.of(p("a/b"), p("a/c")), paths);
  }

//---------------------------------------------------------------------------

  @Test
  void diamond_nestedUnderOneRoot_pruned()
  {
    // a/b is root, a/c is root, a/b/d is nested under a/b

    Set<FilePath> paths = setOf(p("a/b"), p("a/c"), p("a/b/d"));

    FileManager.removeNestedPaths(paths);

    assertEquals(Set.of(p("a/b"), p("a/c")), paths);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates a {@link FilePath} under tempDir for the given relative path.
   * Uses the temp directory as base so paths are absolute (required by
   * {@link FilePath#contains}).
   */
  private FilePath p(String relativePath)
  {
    return FilePath.of(tempDir.resolve(relativePath));
  }

//---------------------------------------------------------------------------

  /**
   * Creates a mutable {@link LinkedHashSet} from the given paths.
   * {@code removeNestedPaths} mutates the set, so it must be mutable.
   */
  private static Set<FilePath> setOf(FilePath... paths)
  {
    Set<FilePath> set = new LinkedHashSet<>();

    Collections.addAll(set, paths);

    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
