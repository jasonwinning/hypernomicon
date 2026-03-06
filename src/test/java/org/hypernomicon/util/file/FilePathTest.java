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

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * Edge-case tests for {@link FilePath#contains(FilePath)}.
 * <p>
 * Tests are organized into two groups: those using real filesystem paths
 * (exercising the {@code toRealPath()} branch) and those using non-existent
 * paths (exercising the normalized-absolute-path fallback).
 */
class FilePathContainsTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @TempDir
  Path tempDir;

  private FilePath base;

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp() throws IOException
  {
    // Create a directory tree for tests that need real paths:
    // tempDir/a/b/c/d   (nested dirs)
    // tempDir/bar/       (for shared-prefix test)
    // tempDir/barbaz/    (for shared-prefix test)
    // tempDir/a/x/       (for dot-dot normalization test)

    Files.createDirectories(tempDir.resolve("a").resolve("b").resolve("c").resolve("d"));
    Files.createDirectories(tempDir.resolve("bar"));
    Files.createDirectories(tempDir.resolve("barbaz"));
    Files.createDirectories(tempDir.resolve("a").resolve("x"));

    base = new FilePath(tempDir);
  }

//---------------------------------------------------------------------------
//region Real filesystem paths (toRealPath branch)
//---------------------------------------------------------------------------

  @Test
  void directChild_contained()
  {
    assertTrue(base.resolve("a").contains(base.resolve("a", "b")));
  }

//---------------------------------------------------------------------------

  @Test
  void deepDescendant_contained()
  {
    assertTrue(base.resolve("a").contains(base.resolve("a", "b", "c", "d")));
  }

//---------------------------------------------------------------------------

  @Test
  void selfContainment_returnsTrue()
  {
    FilePath path = base.resolve("a", "b");

    assertTrue(path.contains(path));
  }

//---------------------------------------------------------------------------

  @Test
  void sibling_notContained()
  {
    FilePath a = base.resolve("a"),
             bar = base.resolve("bar");

    assertFalse(a.contains(bar));
    assertFalse(bar.contains(a));
  }

//---------------------------------------------------------------------------

  @Test
  void sharedPrefix_notParentChild()
  {
    FilePath bar    = base.resolve("bar"),
             barbaz = base.resolve("barbaz");

    assertFalse(bar.contains(barbaz), "bar should NOT contain barbaz despite shared prefix");
    assertFalse(barbaz.contains(bar), "barbaz should NOT contain bar");
  }

//---------------------------------------------------------------------------

  @Test
  void reversedRelationship_notContained()
  {
    FilePath parent = base.resolve("a"),
             child  = base.resolve("a", "b");

    assertTrue(parent.contains(child));
    assertFalse(child.contains(parent), "Child should NOT contain parent");
  }

//---------------------------------------------------------------------------

  @Test
  void rootContainsDeepDescendant()
  {
    assertTrue(base.contains(base.resolve("a", "b", "c")));
  }

//---------------------------------------------------------------------------
//endregion
//region Dot and dot-dot normalization
//---------------------------------------------------------------------------

  @Test
  void dotComponent_normalized()
  {
    // tempDir/a/./b should be treated as tempDir/a/b

    FilePath withDot = base.resolve("a", ".", "b");

    assertTrue(base.resolve("a").contains(withDot));
  }

//---------------------------------------------------------------------------

  @Test
  void dotDotComponent_normalized()
  {
    // tempDir/a/x/../b should normalize to tempDir/a/b
    FilePath withDotDot = base.resolve("a", "x", "..", "b");

    assertTrue(base.resolve("a").contains(withDotDot));
  }

//---------------------------------------------------------------------------
//endregion
//region Non-existent paths (normalized-absolute fallback branch)
//---------------------------------------------------------------------------

  @Test
  void nonExistent_parentChild_contained()
  {
    FilePath fakeParent = base.resolve("fake_parent"),
             fakeChild  = base.resolve("fake_parent", "child");

    assertTrue(fakeParent.contains(fakeChild));
  }

//---------------------------------------------------------------------------

  @Test
  void nonExistent_sharedPrefix_notContained()
  {
    FilePath fakeBar    = new FilePath(tempDir.resolve("fake_bar")),
             fakeBarbaz = new FilePath(tempDir.resolve("fake_barbaz"));

    assertFalse(fakeBar.contains(fakeBarbaz));
  }

//---------------------------------------------------------------------------

  @Test
  void nonExistent_sibling_notContained()
  {
    FilePath fakeA = new FilePath(tempDir.resolve("fake_a")),
             fakeB = new FilePath(tempDir.resolve("fake_b"));

    assertFalse(fakeA.contains(fakeB));
    assertFalse(fakeB.contains(fakeA));
  }

//---------------------------------------------------------------------------

  @Test
  void nonExistent_selfContainment_returnsTrue()
  {
    FilePath fake = new FilePath(tempDir.resolve("nonexistent_dir"));

    assertTrue(fake.contains(fake));
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
