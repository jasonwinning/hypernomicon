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

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * Tests for {@link FilePath}: containment checks, copy/move/rename
 * operations, and directory queries.
 */
class FilePathTest
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
    TestHyperDB.closeIfOpen();  // populateForTesting refuses to trample an online session's registry

    FilePathRegistry.instance().populateForTesting(FilePath.of(tempDir));

    // Create a directory tree for contains tests:
    // tempDir/a/b/c/d   (nested dirs)
    // tempDir/bar/       (for shared-prefix test)
    // tempDir/barbaz/    (for shared-prefix test)
    // tempDir/a/x/       (for dot-dot normalization test)

    Files.createDirectories(tempDir.resolve("a").resolve("b").resolve("c").resolve("d"));
    Files.createDirectories(tempDir.resolve("bar"));
    Files.createDirectories(tempDir.resolve("barbaz"));
    Files.createDirectories(tempDir.resolve("a").resolve("x"));

    base = FilePath.of(tempDir);
  }

  @AfterEach
  void tearDown()
  {
    FilePathRegistry.instance().clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Tests for FilePath.contains(FilePath) are organized into two groups:
  // those using real filesystem paths (exercising the toRealPath()) and
  // those using non-existent paths (exercising the normalized-absolute-path
  // fallback).

  // -------------------------------------------------------------------------
  // contains: real filesystem paths (toRealPath branch)
  // -------------------------------------------------------------------------

  @Test
  void contains_directChild_contained()
  {
    assertTrue(base.resolve("a").contains(base.resolve("a", "b")));
  }

//---------------------------------------------------------------------------

  @Test
  void contains_deepDescendant_contained()
  {
    assertTrue(base.resolve("a").contains(base.resolve("a", "b", "c", "d")));
  }

//---------------------------------------------------------------------------

  @Test
  void contains_selfContainment_returnsTrue()
  {
    FilePath path = base.resolve("a", "b");

    assertTrue(path.contains(path));
  }

//---------------------------------------------------------------------------

  @Test
  void contains_sibling_notContained()
  {
    FilePath a = base.resolve("a"),
             bar = base.resolve("bar");

    assertFalse(a.contains(bar));
    assertFalse(bar.contains(a));
  }

//---------------------------------------------------------------------------

  @Test
  void contains_sharedPrefix_notParentChild()
  {
    FilePath bar    = base.resolve("bar"),
             barbaz = base.resolve("barbaz");

    assertFalse(bar.contains(barbaz), "bar should NOT contain barbaz despite shared prefix");
    assertFalse(barbaz.contains(bar), "barbaz should NOT contain bar");
  }

//---------------------------------------------------------------------------

  @Test
  void contains_reversedRelationship_notContained()
  {
    FilePath parent = base.resolve("a"),
             child  = base.resolve("a", "b");

    assertTrue(parent.contains(child));
    assertFalse(child.contains(parent), "Child should NOT contain parent");
  }

//---------------------------------------------------------------------------

  @Test
  void contains_rootContainsDeepDescendant()
  {
    assertTrue(base.contains(base.resolve("a", "b", "c")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // -------------------------------------------------------------------------
  // contains: dot and dot-dot normalization
  // -------------------------------------------------------------------------

  @Test
  void contains_dotComponent_normalized()
  {
    // tempDir/a/./b should be treated as tempDir/a/b

    FilePath withDot = base.resolve("a", ".", "b");

    assertTrue(base.resolve("a").contains(withDot));
  }

//---------------------------------------------------------------------------

  @Test
  void contains_dotDotComponent_normalized()
  {
    // tempDir/a/x/../b should normalize to tempDir/a/b
    FilePath withDotDot = base.resolve("a", "x", "..", "b");

    assertTrue(base.resolve("a").contains(withDotDot));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // -------------------------------------------------------------------------
  // contains: non-existent paths (normalized-absolute fallback branch)
  // -------------------------------------------------------------------------

  @Test
  void contains_nonExistent_parentChild_contained()
  {
    FilePath fakeParent = base.resolve("fake_parent"),
             fakeChild  = base.resolve("fake_parent", "child");

    assertTrue(fakeParent.contains(fakeChild));
  }

//---------------------------------------------------------------------------

  @Test
  void contains_nonExistent_sharedPrefix_notContained()
  {
    FilePath fakeBar    = FilePath.of(tempDir.resolve("fake_bar")),
             fakeBarbaz = FilePath.of(tempDir.resolve("fake_barbaz"));

    assertFalse(fakeBar.contains(fakeBarbaz));
  }

//---------------------------------------------------------------------------

  @Test
  void contains_nonExistent_sibling_notContained()
  {
    FilePath fakeA = FilePath.of(tempDir.resolve("fake_a")),
             fakeB = FilePath.of(tempDir.resolve("fake_b"));

    assertFalse(fakeA.contains(fakeB));
    assertFalse(fakeB.contains(fakeA));
  }

//---------------------------------------------------------------------------

  @Test
  void contains_nonExistent_selfContainment_returnsTrue()
  {
    FilePath fake = FilePath.of(tempDir.resolve("nonexistent_dir"));

    assertTrue(fake.contains(fake));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // -------------------------------------------------------------------------
  // copyTo
  // -------------------------------------------------------------------------

  @Test
  void copyTo_createsFileAtDest() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("dest.txt");

    Files.writeString(src.toPath(), "hello");

    assertTrue(src.copyTo(dest, false));
    assertTrue(dest.exists());
  }

//---------------------------------------------------------------------------

  @Test
  void copyTo_sourceStillExists() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("dest.txt");

    Files.writeString(src.toPath(), "hello");
    src.copyTo(dest, false);

    assertTrue(src.exists());
    assertTrue(dest.exists());
  }

//---------------------------------------------------------------------------

  @Test
  void copyTo_preservesContents() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("dest.txt");

    Files.writeString(src.toPath(), "expected content");
    src.copyTo(dest, false);

    assertEquals("expected content", Files.readString(dest.toPath()));
  }

//---------------------------------------------------------------------------

  @Test
  void copyTo_overwritesExistingDest() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("dest.txt");

    Files.writeString(src.toPath(), "new content");
    Files.writeString(dest.toPath(), "old content");
    src.copyTo(dest, false);

    assertEquals("new content", Files.readString(dest.toPath()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // -------------------------------------------------------------------------
  // moveTo
  // -------------------------------------------------------------------------

  @Test
  void moveTo_createsFileAtDest() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("dest.txt");

    Files.writeString(src.toPath(), "hello");

    assertTrue(src.moveTo(dest, false));
    assertTrue(dest.exists());
  }

//---------------------------------------------------------------------------

  @Test
  void moveTo_removesSource() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("dest.txt");

    Files.writeString(src.toPath(), "hello");
    src.moveTo(dest, false);

    assertFalse(src.exists());
  }

//---------------------------------------------------------------------------

  @Test
  void moveTo_preservesContents() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("dest.txt");

    Files.writeString(src.toPath(), "moved content");
    src.moveTo(dest, false);

    assertEquals("moved content", Files.readString(dest.toPath()));
  }

//---------------------------------------------------------------------------

  @Test
  void moveTo_overwritesExistingDest() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("dest.txt");

    Files.writeString(src.toPath(), "new content");
    Files.writeString(dest.toPath(), "old content");
    src.moveTo(dest, false);

    assertEquals("new content", Files.readString(dest.toPath()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // -------------------------------------------------------------------------
  // renameTo
  // -------------------------------------------------------------------------

  @Test
  void renameTo_renamesFile() throws IOException
  {
    FilePath src = tempFilePath("original.txt");

    Files.writeString(src.toPath(), "data");
    src.renameTo("renamed.txt");

    assertFalse(src.exists());
    assertTrue(tempFilePath("renamed.txt").exists());
  }

//---------------------------------------------------------------------------

  @Test
  void renameTo_preservesContents() throws IOException
  {
    FilePath src = tempFilePath("original.txt");

    Files.writeString(src.toPath(), "file content");
    src.renameTo("renamed.txt");

    assertEquals("file content", Files.readString(tempFilePath("renamed.txt").toPath()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // -------------------------------------------------------------------------
  // dirContainsAnyFiles
  // -------------------------------------------------------------------------

  @Test
  void dirContainsAnyFiles_emptyDir_returnsFalse() throws IOException
  {
    Path emptyDir = Files.createDirectory(tempDir.resolve("emptyForTest"));

    assertFalse(FilePath.of(emptyDir).dirContainsAnyFiles());
  }

//---------------------------------------------------------------------------

  @Test
  void dirContainsAnyFiles_fileInRoot_returnsTrue() throws IOException
  {
    Path dir = Files.createDirectory(tempDir.resolve("dirWithFile"));
    Files.writeString(dir.resolve("file.txt"), "data");

    assertTrue(FilePath.of(dir).dirContainsAnyFiles());
  }

//---------------------------------------------------------------------------

  @Test
  void dirContainsAnyFiles_fileOnlyInSubdir_returnsTrue() throws IOException
  {
    Path dir = Files.createDirectory(tempDir.resolve("dirWithNestedFile")),
         sub = Files.createDirectory(dir.resolve("sub"));

    Files.writeString(sub.resolve("nested.txt"), "data");

    assertTrue(FilePath.of(dir).dirContainsAnyFiles());
  }

//---------------------------------------------------------------------------

  @Test
  void dirContainsAnyFiles_emptySubdirOnly_returnsFalse() throws IOException
  {
    Path dir = Files.createDirectory(tempDir.resolve("dirWithEmptySub"));
    Files.createDirectory(dir.resolve("emptySubdir"));

    assertFalse(FilePath.of(dir).dirContainsAnyFiles());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // -------------------------------------------------------------------------
  // replaceFileAtomically
  // -------------------------------------------------------------------------

  /**
   * An interrupt ends the retry loop at once instead of retrying until the timeout.
   * A directory as the destination makes every attempt fail with
   * {@link AccessDeniedException}, the exception the loop retries on. Windows only:
   * elsewhere the loop has no timeout, and renaming a file onto a directory fails
   * with a different exception.
   */
  @Test
  @EnabledOnOs(OS.WINDOWS)
  void replaceFileAtomically_interrupted_rethrowsWithoutWaitingOutTheTimeout() throws IOException
  {
    FilePath src  = tempFilePath("src.txt"),
             dest = tempFilePath("destDir");

    Files.writeString(src.toPath(), "data");
    Files.createDirectory(dest.toPath());

    AtomicBoolean retried = new AtomicBoolean(false);
    long startNanos = System.nanoTime();

    Thread.currentThread().interrupt();

    try
    {
      assertThrows(AccessDeniedException.class, () -> src.replaceFileAtomically(dest, _ -> retried.set(true)));
      assertTrue(Thread.currentThread().isInterrupted(), "The interrupt status should still be set.");
    }
    finally
    {
      Thread.interrupted();  // Clear the status so later tests on this thread are unaffected
    }

    long elapsedMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos);

    assertTrue(retried.get(), "The rename should have reached its retry loop.");
    assertTrue(elapsedMillis < 5_000, "The interrupt should end the retry loop at once, but it took " + elapsedMillis + " ms.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePath tempFilePath(String name)
  {
    return FilePath.of(tempDir.resolve(name));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
