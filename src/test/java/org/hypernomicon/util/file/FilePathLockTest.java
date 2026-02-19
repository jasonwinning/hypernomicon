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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.DesktopUtil.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;

import java.io.*;
import java.nio.file.*;

import org.hypernomicon.util.PopupRobot;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * Unit tests for {@link FilePath#canObtainLock()} and
 * {@link FilePath#anyOpenFilesInDir()}.
 * <p>
 * Lock checking is Windows-specific; on POSIX systems the methods return
 * early (true / false respectively). OS-specific tests use
 * {@code assumeTrue}/{@code assumeFalse} so they are skipped on the
 * wrong platform rather than failing.
 */
class FilePathLockTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @TempDir
  Path tempDir;

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp()
  {
    PopupRobot.setActive(true);
    PopupRobot.clear();
  }

//---------------------------------------------------------------------------

  @AfterEach
  void tearDown()
  {
    PopupRobot.setActive(false);
    PopupRobot.clear();
  }

//---------------------------------------------------------------------------
//region canObtainLock()
//---------------------------------------------------------------------------

  @Test
  void canObtainLock_nonExistentPath_returnsTrue() throws IOException
  {
    FilePath nonExistent = new FilePath(tempDir.resolve("does_not_exist.txt"));

    assertTrue(nonExistent.canObtainLock());
  }

//---------------------------------------------------------------------------

  @Test
  void canObtainLock_existingUnlockedFile_returnsTrue() throws IOException
  {
    assumeTrue(IS_OS_WINDOWS, "Lock checking only applies on Windows");

    Path file = Files.createFile(tempDir.resolve("unlocked.txt"));

    assertTrue(new FilePath(file).canObtainLock());
  }

//---------------------------------------------------------------------------

  @Test
  void canObtainLock_readOnlyFile_returnsTrue() throws IOException
  {
    assumeTrue(IS_OS_WINDOWS, "Lock checking only applies on Windows");

    Path file = Files.createFile(tempDir.resolve("readonly.txt"));
    File javaFile = file.toFile();

    try
    {
      assertTrue(javaFile.setReadOnly(), "Failed to set file read-only");
      assertTrue(new FilePath(file).canObtainLock());
    }
    finally
    {
      javaFile.setWritable(true);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void canObtainLock_existingUnlockedDirectory_returnsTrue() throws IOException
  {
    assumeTrue(IS_OS_WINDOWS, "Lock checking only applies on Windows");

    Path dir = Files.createDirectory(tempDir.resolve("unlocked_dir"));

    assertTrue(new FilePath(dir).canObtainLock());
  }

//---------------------------------------------------------------------------

  @Test
  void canObtainLock_directoryUnchangedAfterProbeRename() throws IOException
  {
    assumeTrue(IS_OS_WINDOWS, "Lock checking only applies on Windows");

    Path dir = Files.createDirectory(tempDir.resolve("probe_test_dir"));
    FilePath filePath = new FilePath(dir);

    assertTrue(filePath.canObtainLock());

    // Directory should still exist under its original name
    assertTrue(Files.isDirectory(dir), "Directory should still exist after probe-rename round-trip");

    // No leftover .hntmp_ sibling should remain
    try (DirectoryStream<Path> stream = Files.newDirectoryStream(tempDir, '*' + LOCK_PROBE_SUFFIX + '*'))
    {
      assertFalse(stream.iterator().hasNext(), "No lock-probe leftover directories should remain");
    }
  }

//---------------------------------------------------------------------------

  @Test
  void canObtainLock_posix_alwaysReturnsTrue() throws IOException
  {
    assumeFalse(IS_OS_WINDOWS, "Test only runs on POSIX systems");

    Path file = Files.writeString(tempDir.resolve("posix_file.txt"), "data");

    assertTrue(new FilePath(file).canObtainLock());
  }

//---------------------------------------------------------------------------
//endregion
//region anyOpenFilesInDir()
//---------------------------------------------------------------------------

  @Test
  void anyOpenFilesInDir_posix_returnsFalse()
  {
    assumeFalse(IS_OS_WINDOWS, "Test only runs on POSIX systems");

    assertFalse(new FilePath(tempDir).anyOpenFilesInDir());
  }

//---------------------------------------------------------------------------

  @Test
  void anyOpenFilesInDir_emptyDirectory_returnsFalse() throws IOException
  {
    assumeTrue(IS_OS_WINDOWS, "Lock checking only applies on Windows");

    Path dir = Files.createDirectory(tempDir.resolve("empty_dir"));

    assertFalse(new FilePath(dir).anyOpenFilesInDir());
    assertNull(PopupRobot.getLastMessage(), "No popup expected for unlocked directory");
  }

//---------------------------------------------------------------------------

  @Test
  void anyOpenFilesInDir_unlockedFiles_returnsFalse() throws IOException
  {
    assumeTrue(IS_OS_WINDOWS, "Lock checking only applies on Windows");

    Path dir = Files.createDirectory(tempDir.resolve("unlocked_dir"));
    Files.writeString(dir.resolve("a.txt"), "data");
    Files.writeString(dir.resolve("b.txt"), "data");

    assertFalse(new FilePath(dir).anyOpenFilesInDir());
    assertNull(PopupRobot.getLastMessage(), "No popup expected for unlocked files");
  }

//---------------------------------------------------------------------------

  @Test
  void anyOpenFilesInDir_nestedUnlockedSubdirs_returnsFalse() throws IOException
  {
    assumeTrue(IS_OS_WINDOWS, "Lock checking only applies on Windows");

    Path dir = Files.createDirectory(tempDir.resolve("nested_dir")),
         sub = Files.createDirectories(dir.resolve("sub1").resolve("sub2"));
    Files.writeString(sub.resolve("deep.txt"), "data");
    Files.writeString(dir.resolve("top.txt"), "data");

    assertFalse(new FilePath(dir).anyOpenFilesInDir());
    assertNull(PopupRobot.getLastMessage(), "No popup expected for unlocked nested tree");
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
