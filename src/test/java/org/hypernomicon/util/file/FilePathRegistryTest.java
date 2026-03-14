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

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.*;
import java.util.Set;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;

import org.hypernomicon.util.file.deletion.FileDeletion;

//---------------------------------------------------------------------------

/**
 * Unit tests for {@link FilePathRegistry}.
 * <p>
 * These tests exercise the registry's path identity (canonical interning), eviction,
 * temporary file filtering, and lifecycle methods.
 * <p>
 * Many tests use {@code new FilePath(path)} rather than {@code FilePath.of(path)} to create
 * lookup arguments for {@code contains()}, {@code evict()}, and {@code onSubtreeMoved()}.
 * This is intentional: {@code FilePath.of()} would intern the path via {@code getOrCreate()}
 * as a side effect, making negative containment assertions fail and positive ones tautological.
 */
class FilePathRegistryTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @TempDir Path tempDir;

  private FilePathRegistry registry;

  @BeforeEach
  void setUp()
  {
    registry = FilePathRegistry.instance();
    registry.clear();
  }

  @AfterEach
  void tearDown()
  {
    registry.clear();
  }

//---------------------------------------------------------------------------
//region Lifecycle: Populate and Clear
//---------------------------------------------------------------------------

  @Test
  void isActive_beforePopulate_returnsFalse()
  {
    assertFalse(registry.isActive());
  }

  @Test
  void isActive_afterPopulate_returnsTrue() throws IOException
  {
    Files.createFile(tempDir.resolve("test.txt"));

    registry.populate(FilePath.of(tempDir));

    assertTrue(registry.isActive());
    assertTrue(registry.size() > 0);
  }

  @Test
  void clear_afterPopulate_deactivates() throws IOException
  {
    Files.createFile(tempDir.resolve("test.txt"));
    registry.populate(FilePath.of(tempDir));

    registry.clear();

    assertFalse(registry.isActive());
    assertEquals(0, registry.size());
  }

  @Test
  void populate_internsFilesAndDirs() throws IOException
  {
    Path subDir = Files.createDirectory(tempDir.resolve("subdir")),
         file1 = Files.createFile(tempDir.resolve("file1.txt")),
         file2 = Files.createFile(subDir.resolve("file2.txt"));

    registry.populate(FilePath.of(tempDir));

    // Root dir, subdir, file1, file2 should all be interned
    assertTrue(registry.contains(new FilePath(tempDir)));
    assertTrue(registry.contains(new FilePath(subDir)));
    assertTrue(registry.contains(new FilePath(file1)));
    assertTrue(registry.contains(new FilePath(file2)));
  }

  @Test
  void populate_excludesTempFiles() throws IOException
  {
    Files.createFile(tempDir.resolve("real.txt"));
    Files.createFile(tempDir.resolve(".DS_Store"));
    Files.createFile(tempDir.resolve("Thumbs.db"));
    Files.createFile(tempDir.resolve("~$lockfile.doc"));
    Files.createFile(tempDir.resolve("something.tmp"));

    registry.populate(FilePath.of(tempDir));

    assertTrue(registry.contains(new FilePath(tempDir.resolve("real.txt"))));
    assertFalse(registry.contains(new FilePath(tempDir.resolve(".DS_Store"))));
    assertFalse(registry.contains(new FilePath(tempDir.resolve("Thumbs.db"))));
    assertFalse(registry.contains(new FilePath(tempDir.resolve("~$lockfile.doc"))));
    assertFalse(registry.contains(new FilePath(tempDir.resolve("something.tmp"))));
  }

  @Test
  void populate_excludesTempDirectories() throws IOException
  {
    Path dropbox = Files.createDirectory(tempDir.resolve(".dropbox")),
         normalDir = Files.createDirectory(tempDir.resolve("normaldir"));

    Files.createFile(dropbox.resolve("cache.dat"));
    Files.createFile(normalDir.resolve("file.txt"));

    registry.populate(FilePath.of(tempDir));

    assertFalse(registry.contains(new FilePath(dropbox)));
    assertFalse(registry.contains(new FilePath(dropbox.resolve("cache.dat"))));
    assertTrue(registry.contains(new FilePath(normalDir)));
    assertTrue(registry.contains(new FilePath(normalDir.resolve("file.txt"))));
  }

//---------------------------------------------------------------------------
//endregion
//region Path Identity: getOrCreate
//---------------------------------------------------------------------------

  @Test
  void getOrCreate_samePathTwice_returnsSameInstance() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("test.txt"));
    registry.populate(FilePath.of(tempDir));

    FilePath first = registry.getOrCreate(file),
             second = registry.getOrCreate(file);

    assertSame(first, second);
  }

  @Test
  void getOrCreate_pathOutsideRoot_returnsNonCachedInstance()
  {
    registry.populate(FilePath.of(tempDir));

    Path outsidePath = tempDir.getParent().resolve("outside.txt");
    FilePath result = registry.getOrCreate(outsidePath);

    assertNotNull(result);
    assertFalse(registry.contains(result));
  }

  @Test
  void getOrCreate_nonExistentPathUnderRoot_cachesUnderInputKey()
  {
    registry.populate(FilePath.of(tempDir));

    Path nonExistent = tempDir.resolve("does_not_exist.txt");
    FilePath first = registry.getOrCreate(nonExistent),
             second = registry.getOrCreate(nonExistent);

    assertSame(first, second);
    assertTrue(registry.contains(first));
  }

//---------------------------------------------------------------------------
//endregion
//region Eviction
//---------------------------------------------------------------------------

  @Test
  void evict_removesEntry() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("test.txt"));
    registry.populate(FilePath.of(tempDir));

    FilePath filePath = new FilePath(file);
    assertTrue(registry.contains(filePath));

    registry.evict(filePath);

    assertFalse(registry.contains(filePath));
  }

  @Test
  void evict_nullPath_doesNotThrow()
  {
    assertDoesNotThrow(() -> registry.evict(null));
  }

  @Test
  void evictSubtree_removesAllDescendants() throws IOException
  {
    Path subDir = Files.createDirectory(tempDir.resolve("sub")),
         subSubDir = Files.createDirectory(subDir.resolve("subsub"));

    Files.createFile(subDir.resolve("file1.txt"));
    Files.createFile(subSubDir.resolve("file2.txt"));

    registry.populate(FilePath.of(tempDir));

    FilePath subDirPath = new FilePath(subDir);
    assertTrue(registry.contains(subDirPath));
    assertTrue(registry.contains(new FilePath(subDir.resolve("file1.txt"))));
    assertTrue(registry.contains(new FilePath(subSubDir.resolve("file2.txt"))));

    registry.evictSubtree(subDirPath);

    assertFalse(registry.contains(subDirPath));
    assertFalse(registry.contains(new FilePath(subDir.resolve("file1.txt"))));
    assertFalse(registry.contains(new FilePath(subSubDir)));
    assertFalse(registry.contains(new FilePath(subSubDir.resolve("file2.txt"))));

    // Root and sibling files should still be present
    assertTrue(registry.contains(new FilePath(tempDir)));
  }

  @Test
  void evictSubtree_nullDir_doesNotThrow()
  {
    assertDoesNotThrow(() -> registry.evictSubtree(null));
  }

//---------------------------------------------------------------------------
//endregion
//region Temporary File Filtering
//---------------------------------------------------------------------------

  @Test
  void isTemporaryFile_knownExactNames()
  {
    assertTrue(FilePath.isTemporaryFile(".DS_Store"));
    assertTrue(FilePath.isTemporaryFile(".ds_store"));
    assertTrue(FilePath.isTemporaryFile("Thumbs.db"));
    assertTrue(FilePath.isTemporaryFile("THUMBS.DB"));
    assertTrue(FilePath.isTemporaryFile("desktop.ini"));
    assertTrue(FilePath.isTemporaryFile("DESKTOP.INI"));
    assertTrue(FilePath.isTemporaryFile(".dropbox"));
    assertTrue(FilePath.isTemporaryFile(".dropbox.cache"));
  }

  @Test
  void isTemporaryFile_prefixPatterns()
  {
    assertTrue(FilePath.isTemporaryFile("~$Document.docx"));
    assertTrue(FilePath.isTemporaryFile(".~lock.file.ods#"));
    assertTrue(FilePath.isTemporaryFile("._macresource"));
  }

  @Test
  void isTemporaryFile_suffixPatterns()
  {
    assertTrue(FilePath.isTemporaryFile("something.tmp"));
    assertTrue(FilePath.isTemporaryFile("SOMETHING.TMP"));
    assertTrue(FilePath.isTemporaryFile("file.temp"));
    assertTrue(FilePath.isTemporaryFile("FILE.TEMP"));
  }

  @Test
  void isTemporaryFile_normalFiles()
  {
    assertFalse(FilePath.isTemporaryFile("document.txt"));
    assertFalse(FilePath.isTemporaryFile("image.png"));
    assertFalse(FilePath.isTemporaryFile("data.xml"));
    assertFalse(FilePath.isTemporaryFile("README.md"));
  }

  @Test
  void isTemporaryFile_nullOrEmpty()
  {
    assertFalse(FilePath.isTemporaryFile(null));
    assertFalse(FilePath.isTemporaryFile(""));
  }

//---------------------------------------------------------------------------
//endregion
//region Contains
//---------------------------------------------------------------------------

  @Test
  void contains_emptyFilePath_returnsFalse()
  {
    registry.populate(FilePath.of(tempDir));
    assertFalse(registry.contains(null));
  }

  @Test
  void contains_existingPath_returnsTrue() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("exists.txt"));
    registry.populate(FilePath.of(tempDir));

    assertTrue(registry.contains(new FilePath(file)));
  }

  @Test
  void contains_nonExistentPath_returnsFalse()
  {
    registry.populate(FilePath.of(tempDir));

    assertFalse(registry.contains(new FilePath(tempDir.resolve("nope.txt"))));
  }

//---------------------------------------------------------------------------
//endregion
//region Thread Safety
//---------------------------------------------------------------------------

  @Test
  void concurrent_getOrCreate_returnsSameInstance() throws Exception
  {
    Path file = Files.createFile(tempDir.resolve("concurrent.txt"));
    registry.populate(FilePath.of(tempDir));

    int numThreads = 10;
    Set<FilePath> results = ConcurrentHashMap.newKeySet();
    AtomicBoolean identityFailure = new AtomicBoolean(false);

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    try
    {
      CountDownLatch startLatch = new CountDownLatch(1),
                     doneLatch = new CountDownLatch(numThreads);

      FilePath[] firstRef = new FilePath[1];

      for (int ndx = 0; ndx < numThreads; ndx++)
      {
        executor.submit(() ->
        {
          try
          {
            startLatch.await();
            FilePath result = registry.getOrCreate(file);
            results.add(result);

            // Check reference identity
            synchronized (firstRef)
            {
              if (firstRef[0] == null)
                firstRef[0] = result;
              else if (firstRef[0] != result)
                identityFailure.set(true);
            }
          }
          catch (InterruptedException e)
          {
            Thread.currentThread().interrupt();
          }
          finally
          {
            doneLatch.countDown();
          }
        });
      }

      startLatch.countDown();
      assertTrue(doneLatch.await(30, TimeUnit.SECONDS), "Test timed out");
      assertFalse(identityFailure.get(), "Different threads received different FilePath instances for the same path");
    }
    finally
    {
      executor.shutdownNow();
    }
  }

//---------------------------------------------------------------------------
//endregion
//region FilePath.of() Integration
//---------------------------------------------------------------------------

  @Test
  void filePathOf_whenRegistryActive_returnsInterned() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("interned.txt"));
    registry.populate(FilePath.of(tempDir));

    FilePath result1 = FilePath.of(file),
             result2 = FilePath.of(file);

    assertSame(result1, result2);
  }

  @Test
  void filePathOf_whenRegistryInactive_returnsNewInstance()
  {
    // Registry is cleared in setUp(), so it's inactive
    assertFalse(registry.isActive());

    FilePath result1 = FilePath.of(Path.of("/some/path.txt")),
             result2 = FilePath.of(Path.of("/some/path.txt"));

    // Without registry, each call creates a new instance
    assertNotSame(result1, result2);
  }

  @Test
  void filePathOf_relativePath_returnsNewInstance()
  {
    registry.populate(FilePath.of(tempDir));

    FilePath result = FilePath.of(Path.of("relative/path.txt"));

    assertNotNull(result);
    // Relative paths are never interned
  }

//---------------------------------------------------------------------------
//endregion
//region Case Normalization (through registry API)
//---------------------------------------------------------------------------

  /**
   * On case-insensitive filesystems (Windows NTFS, macOS APFS default), requesting a
   * path with different casing should return the same canonical instance that was interned
   * during populate. On case-sensitive filesystems this test is skipped.
   */
  @Test
  void getOrCreate_caseVariant_returnsSameInstanceOnCaseInsensitiveFS() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("CaseTest.txt"));

    // Check if the filesystem is case-insensitive
    Path upperVariant = tempDir.resolve("CASETEST.TXT");
    if (Files.exists(upperVariant) == false)
    {
      // Case-sensitive filesystem; nothing to test
      return;
    }

    registry.populate(FilePath.of(tempDir));

    FilePath original = registry.getOrCreate(file),
             variant = registry.getOrCreate(upperVariant);

    assertSame(original, variant,
        "Case-insensitive FS should return same canonical instance for case variants");
  }

//---------------------------------------------------------------------------

  @Test
  void contains_caseVariant_returnsTrueOnCaseInsensitiveFS() throws IOException
  {
    Files.createFile(tempDir.resolve("ContainsCase.txt"));

    Path upperVariant = tempDir.resolve("CONTAINSCASE.TXT");
    if (Files.exists(upperVariant) == false)
      return;  // Case-sensitive filesystem

    registry.populate(FilePath.of(tempDir));

    assertTrue(registry.contains(new FilePath(upperVariant)),
        "Case-insensitive FS should find case variants via contains()");
  }

//---------------------------------------------------------------------------

  @Test
  void evict_caseVariant_removesEntry() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("EvictCase.txt"));

    Path upperVariant = tempDir.resolve("EVICTCASE.TXT");
    if (Files.exists(upperVariant) == false)
      return;  // Case-sensitive filesystem

    registry.populate(FilePath.of(tempDir));

    assertTrue(registry.contains(new FilePath(file)));

    // Evict using case variant
    registry.evict(new FilePath(upperVariant));

    assertFalse(registry.contains(new FilePath(file)),
        "Evicting a case variant should remove the entry on case-insensitive FS");
  }

//---------------------------------------------------------------------------
//endregion
//region Additional Edge Cases
//---------------------------------------------------------------------------

  @Test
  void getOrCreate_differentFiles_returnsDifferentInstances() throws IOException
  {
    Path file1 = Files.createFile(tempDir.resolve("alpha.txt")),
         file2 = Files.createFile(tempDir.resolve("beta.txt"));

    registry.populate(FilePath.of(tempDir));

    FilePath result1 = registry.getOrCreate(file1),
             result2 = registry.getOrCreate(file2);

    assertNotSame(result1, result2);
    assertNotEquals(result1, result2);
  }

//---------------------------------------------------------------------------

  @Test
  void evict_thenGetOrCreate_returnsNewInstance() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("reenter.txt"));
    registry.populate(FilePath.of(tempDir));

    FilePath original = registry.getOrCreate(file);
    registry.evict(original);
    assertFalse(registry.contains(original));

    // getOrCreate should re-intern a new instance
    FilePath reinterned = registry.getOrCreate(file);
    assertTrue(registry.contains(reinterned));
  }

//---------------------------------------------------------------------------

  @Test
  void size_afterPopulate_countsAllEntries() throws IOException
  {
    Path subDir = Files.createDirectory(tempDir.resolve("sdir"));

    Files.createFile(tempDir.resolve("f1.txt"));
    Files.createFile(tempDir.resolve("f2.txt"));
    Files.createFile(subDir.resolve("f3.txt"));

    registry.populate(FilePath.of(tempDir));

    // tempDir + subDir + f1 + f2 + f3 = 5
    assertEquals(5, registry.size());
  }

//---------------------------------------------------------------------------

  @Test
  void size_afterEviction_decreases() throws IOException
  {
    Files.createFile(tempDir.resolve("one.txt"));
    Files.createFile(tempDir.resolve("two.txt"));

    registry.populate(FilePath.of(tempDir));
    int before = registry.size();

    registry.evict(new FilePath(tempDir.resolve("one.txt")));

    assertEquals(before - 1, registry.size());
  }

//---------------------------------------------------------------------------

  @Test
  void getOrCreate_registryInactive_returnsNonCachedInstance()
  {
    assertFalse(registry.isActive());

    // When inactive, getOrCreate should still return a FilePath (not crash)
    // but it won't be interned
    FilePath result = registry.getOrCreate(tempDir.resolve("inactive.txt"));

    assertNotNull(result);
    assertEquals(0, registry.size());
  }

//---------------------------------------------------------------------------

  @Test
  void evictSubtree_preservesSiblingSubtrees() throws IOException
  {
    Path dirA = Files.createDirectory(tempDir.resolve("dirA")),
         dirB = Files.createDirectory(tempDir.resolve("dirB"));

    Files.createFile(dirA.resolve("a.txt"));
    Files.createFile(dirB.resolve("b.txt"));

    registry.populate(FilePath.of(tempDir));

    registry.evictSubtree(new FilePath(dirA));

    assertFalse(registry.contains(new FilePath(dirA)));
    assertFalse(registry.contains(new FilePath(dirA.resolve("a.txt"))));

    // Sibling subtree unaffected
    assertTrue(registry.contains(new FilePath(dirB)));
    assertTrue(registry.contains(new FilePath(dirB.resolve("b.txt"))));
  }

//---------------------------------------------------------------------------

  @Test
  void populate_calledTwice_reinternsCleanly() throws IOException
  {
    Files.createFile(tempDir.resolve("first.txt"));
    registry.populate(FilePath.of(tempDir));

    int firstSize = registry.size();
    assertTrue(firstSize > 0);

    // Add a new file and re-populate
    Files.createFile(tempDir.resolve("second.txt"));
    registry.clear();
    registry.populate(FilePath.of(tempDir));

    assertEquals(firstSize + 1, registry.size());
  }

//---------------------------------------------------------------------------

  @Test
  void getOrCreate_directoryPath_returnsCanonicalInstance() throws IOException
  {
    Path subDir = Files.createDirectory(tempDir.resolve("mydir"));
    registry.populate(FilePath.of(tempDir));

    FilePath first = registry.getOrCreate(subDir),
             second = registry.getOrCreate(subDir);

    assertSame(first, second);
  }

//---------------------------------------------------------------------------
//endregion
//region onSubtreeMoved
//---------------------------------------------------------------------------

  @Test
  void onSubtreeMoved_evictsOldSubtreeFromPathCache() throws IOException
  {
    Path dirA = Files.createDirectory(tempDir.resolve("dirA")),
         child = Files.createDirectory(dirA.resolve("child"));

    Files.createFile(dirA.resolve("a.txt"));
    Files.createFile(child.resolve("c.txt"));
    Files.createFile(tempDir.resolve("sibling.txt"));

    registry.populate(FilePath.of(tempDir));

    assertTrue(registry.contains(new FilePath(dirA)));
    assertTrue(registry.contains(new FilePath(dirA.resolve("a.txt"))));
    assertTrue(registry.contains(new FilePath(child)));
    assertTrue(registry.contains(new FilePath(child.resolve("c.txt"))));

    registry.onSubtreeMoved(new FilePath(dirA));

    // Old subtree entries evicted
    assertFalse(registry.contains(new FilePath(dirA)));
    assertFalse(registry.contains(new FilePath(dirA.resolve("a.txt"))));
    assertFalse(registry.contains(new FilePath(child)));
    assertFalse(registry.contains(new FilePath(child.resolve("c.txt"))));

    // Sibling and root untouched
    assertTrue(registry.contains(new FilePath(tempDir)));
    assertTrue(registry.contains(new FilePath(tempDir.resolve("sibling.txt"))));
  }

//---------------------------------------------------------------------------

  @Test
  void onSubtreeMoved_nullOrEmpty_doesNotThrow()
  {
    assertDoesNotThrow(() -> registry.onSubtreeMoved(null));
    assertDoesNotThrow(() -> registry.onSubtreeMoved(new FilePath("")));
  }

//---------------------------------------------------------------------------

  @Test
  void onSubtreeMoved_noMatchingEntries_doesNotThrow() throws IOException
  {
    Files.createFile(tempDir.resolve("existing.txt"));
    registry.populate(FilePath.of(tempDir));

    int sizeBefore = registry.size();

    // Move a path that was never in the registry
    registry.onSubtreeMoved(new FilePath(tempDir.resolve("nonexistent_dir")));

    assertEquals(sizeBefore, registry.size());
  }

//---------------------------------------------------------------------------

  @Test
  void onSubtreeMoved_reinternsAfterPhysicalMove() throws IOException
  {
    Path src = Files.createDirectory(tempDir.resolve("src")),
         dest = Files.createDirectory(tempDir.resolve("dest"));

    Path srcFile = Files.createFile(src.resolve("data.txt"));

    registry.populate(FilePath.of(tempDir));

    assertTrue(registry.contains(new FilePath(srcFile)));

    // Physically move the directory
    Path movedDir = dest.resolve("src");
    Files.move(src, movedDir);

    FilePath oldSrc = new FilePath(src);
    registry.onSubtreeMoved(oldSrc);

    // Old path evicted
    assertFalse(registry.contains(new FilePath(src)));
    assertFalse(registry.contains(new FilePath(srcFile)));

    // New location can be re-interned via getOrCreate
    FilePath reinterned = registry.getOrCreate(movedDir.resolve("data.txt"));
    assertNotNull(reinterned);
    assertTrue(registry.contains(reinterned));
  }

//---------------------------------------------------------------------------
//endregion
//region populateForTesting: Controlled Duplicate Handling
//---------------------------------------------------------------------------

  @Test
  void populateForTesting_samePathTwice_countsOnce() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("once.txt"));

    registry.populateForTesting(FilePath.of(tempDir), file, file, file);

    assertEquals(1, registry.size());
  }

//---------------------------------------------------------------------------

  @Test
  void populateForTesting_preInternedPath_returnedByFilePathOf() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("factory.txt"));

    registry.populateForTesting(FilePath.of(tempDir), file);

    FilePath first = FilePath.of(file),
             second = FilePath.of(file);

    assertSame(first, second);
  }

//---------------------------------------------------------------------------

  @Test
  void populateForTesting_filePathOfString_returnsSameAsOfPath() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("strpath.txt"));

    registry.populateForTesting(FilePath.of(tempDir), file);

    FilePath fromPath = FilePath.of(file),
             fromString = FilePath.of(file.toString());

    assertSame(fromPath, fromString);
  }

//---------------------------------------------------------------------------

  @Test
  void populateForTesting_filePathOfFile_returnsSameAsOfPath() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("filepath.txt"));

    registry.populateForTesting(FilePath.of(tempDir), file);

    FilePath fromPath = FilePath.of(file),
             fromFile = FilePath.of(file.toFile());

    assertSame(fromPath, fromFile);
  }

//---------------------------------------------------------------------------

  @Test
  void populateForTesting_multipleDistinctPaths_allRetrievable() throws IOException
  {
    Path dir = Files.createDirectory(tempDir.resolve("sub")),
         file1 = Files.createFile(tempDir.resolve("a.txt")),
         file2 = Files.createFile(dir.resolve("b.txt"));

    registry.populateForTesting(FilePath.of(tempDir), tempDir, dir, file1, file2);

    assertEquals(4, registry.size());
    assertTrue(registry.contains(FilePath.of(tempDir)));
    assertTrue(registry.contains(FilePath.of(dir)));
    assertTrue(registry.contains(FilePath.of(file1)));
    assertTrue(registry.contains(FilePath.of(file2)));
  }

//---------------------------------------------------------------------------

  @Test
  void populateForTesting_caseVariantLookup_returnsSameOnCaseInsensitiveFS() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("CaseTest.txt"));

    Path upperVariant = tempDir.resolve("CASETEST.TXT");
    if (Files.exists(upperVariant) == false)
      return;  // Case-sensitive filesystem

    registry.populateForTesting(FilePath.of(tempDir), file);

    FilePath original = FilePath.of(file),
             variant = FilePath.of(upperVariant);

    assertSame(original, variant);
  }

//---------------------------------------------------------------------------

  @Test
  void populateForTesting_nonInternedPathUnderRoot_createdOnDemand() throws IOException
  {
    Path interned = Files.createFile(tempDir.resolve("interned.txt")),
         lazy = Files.createFile(tempDir.resolve("lazy.txt"));

    // Only intern one file; the other exists on disk but is not pre-interned
    registry.populateForTesting(FilePath.of(tempDir), interned);

    assertEquals(1, registry.size());

    // FilePath.of() for the non-interned path should create it on demand via getOrCreate
    FilePath result = FilePath.of(lazy);

    assertNotNull(result);
    assertEquals(2, registry.size());
    assertTrue(registry.contains(result));

    // And it should be the canonical instance now
    assertSame(result, FilePath.of(lazy));
  }

//---------------------------------------------------------------------------
//endregion
//region Auto-Eviction via Post-Deletion Hook
//---------------------------------------------------------------------------

  @Test
  void postDeletionHook_evictsFileFromRegistry() throws IOException
  {
    Path file = Files.createFile(tempDir.resolve("hookfile.txt"));

    registry.populate(FilePath.of(tempDir));
    assertTrue(registry.contains(new FilePath(file)));

    // Wire the hook the same way FilePathRegistry.populate() does in production
    FileDeletion.setPostDeletionHook(registry::evictSubtree);

    try
    {
      FileDeletion.ofFile(FilePath.of(file)).nonInteractive().execute();

      assertFalse(registry.contains(new FilePath(file)),
          "Deleted file should be evicted from registry via hook");
    }
    finally
    {
      FileDeletion.setPostDeletionHook(null);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void postDeletionHook_evictsDirectorySubtreeFromRegistry() throws IOException
  {
    Path dir = Files.createDirectory(tempDir.resolve("hookdir")),
         child = Files.createFile(dir.resolve("child.txt"));

    registry.populate(FilePath.of(tempDir));
    assertTrue(registry.contains(new FilePath(dir)));
    assertTrue(registry.contains(new FilePath(child)));

    FileDeletion.setPostDeletionHook(registry::evictSubtree);

    try
    {
      FileDeletion.ofDirWithContents(FilePath.of(dir)).nonInteractive().execute();

      assertFalse(registry.contains(new FilePath(dir)),
          "Deleted directory should be evicted from registry");
      assertFalse(registry.contains(new FilePath(child)),
          "Child of deleted directory should be evicted from registry");
    }
    finally
    {
      FileDeletion.setPostDeletionHook(null);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void postDeletionHook_batchDeletion_evictsAllFromRegistry() throws IOException
  {
    Path f1 = Files.createFile(tempDir.resolve("batch1.txt")),
         f2 = Files.createFile(tempDir.resolve("batch2.txt")),
         f3 = Files.createFile(tempDir.resolve("batch3.txt"));

    registry.populate(FilePath.of(tempDir));
    assertTrue(registry.contains(new FilePath(f1)));
    assertTrue(registry.contains(new FilePath(f2)));
    assertTrue(registry.contains(new FilePath(f3)));

    FileDeletion.setPostDeletionHook(registry::evictSubtree);

    try
    {
      FileDeletion.ofFiles(java.util.List.of(FilePath.of(f1), FilePath.of(f2), FilePath.of(f3)))
          .nonInteractive().execute();

      assertFalse(registry.contains(new FilePath(f1)), "batch1 should be evicted");
      assertFalse(registry.contains(new FilePath(f2)), "batch2 should be evicted");
      assertFalse(registry.contains(new FilePath(f3)), "batch3 should be evicted");
    }
    finally
    {
      FileDeletion.setPostDeletionHook(null);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void postDeletionHook_deletionPreservesUnrelatedEntries() throws IOException
  {
    Path target = Files.createFile(tempDir.resolve("target.txt")),
         keeper = Files.createFile(tempDir.resolve("keeper.txt"));

    registry.populate(FilePath.of(tempDir));
    assertTrue(registry.contains(new FilePath(target)));
    assertTrue(registry.contains(new FilePath(keeper)));

    FileDeletion.setPostDeletionHook(registry::evictSubtree);

    try
    {
      FileDeletion.ofFile(FilePath.of(target)).nonInteractive().execute();

      assertFalse(registry.contains(new FilePath(target)), "Deleted file should be evicted");
      assertTrue(registry.contains(new FilePath(keeper)), "Non-deleted file should remain in registry");
    }
    finally
    {
      FileDeletion.setPostDeletionHook(null);
    }
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
