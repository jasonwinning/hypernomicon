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

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;

import java.io.File;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

import org.hypernomicon.TestConfig;

//---------------------------------------------------------------------------

/**
 * Comprehensive unit tests for {@link FilePathSet}.
 * <p>
 * Tests cover basic Set operations, thread safety, and the cleanup mechanism
 * for empty buckets.
 */
class FilePathSetTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePathSet set;

  @BeforeEach
  void setUp()
  {
    set = new FilePathSet();
  }

//---------------------------------------------------------------------------
//region Basic Operations: Add
//---------------------------------------------------------------------------

  @Test
  void add_newPath_returnsTrue()
  {
    FilePath path = new FilePath("/dir/file.txt");

    boolean result = set.add(path);

    assertTrue(result);
  }

  @Test
  void add_duplicatePath_returnsFalse()
  {
    FilePath path = new FilePath("/dir/file.txt");
    set.add(path);

    boolean result = set.add(path);

    assertFalse(result);
  }

  @Test
  void add_nullPath_throwsException()
  {
    assertThrows(IllegalArgumentException.class, () -> set.add(null));
  }

  @Test
  void add_emptyPath_throwsException()
  {
    assertThrows(IllegalArgumentException.class, () -> set.add(new FilePath("")));
  }

  @Test
  void add_sameFilenameDifferentDirectories_addsMultiple()
  {
    FilePath path1 = new FilePath("/dir1/file.txt"),
             path2 = new FilePath("/dir2/file.txt");

    assertTrue(set.add(path1));
    assertTrue(set.add(path2));
    assertEquals(2, set.size());
  }

  @Test
  void add_differentFilenames_addsMultiple()
  {
    FilePath path1 = new FilePath("/dir/file1.txt"),
             path2 = new FilePath("/dir/file2.txt");

    assertTrue(set.add(path1));
    assertTrue(set.add(path2));
    assertEquals(2, set.size());
  }

//---------------------------------------------------------------------------
//endregion
//region Basic Operations: Remove
//---------------------------------------------------------------------------

  @Test
  void remove_existingPath_returnsTrue()
  {
    FilePath path = new FilePath("C:/dir/file.txt");
    set.add(path);

    boolean result = set.remove(path);

    assertTrue(result);
  }

  @Test
  void remove_nonExistentPath_returnsFalse()
  {
    FilePath path = new FilePath("C:/dir/file.txt");

    boolean result = set.remove(path);

    assertFalse(result);
  }

  @Test
  void remove_null_returnsFalse()
  {
    assertFalse(set.remove(null));
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  void remove_wrongType_returnsFalse()
  {
    assertFalse(set.remove("not a FilePath"));
    assertFalse(set.remove(123));
  }

  @Test
  void remove_existingPath_decreasesSize()
  {
    FilePath path = new FilePath("C:/dir/file.txt");
    set.add(path);

    set.remove(path);

    assertEquals(0, set.size());
  }

  @Test
  void remove_oneOfMultipleSameFilename_removesOnlyOne()
  {
    FilePath path1 = new FilePath("C:/dir1/file.txt"),
             path2 = new FilePath("C:/dir2/file.txt");
    set.add(path1);
    set.add(path2);

    set.remove(path1);

    assertEquals(1, set.size());
    assertFalse(set.contains(path1));
    assertTrue(set.contains(path2));
  }

//---------------------------------------------------------------------------
//endregion
//region Basic Operations: Contains
//---------------------------------------------------------------------------

  @Test
  void contains_existingPath_returnsTrue()
  {
    FilePath path = new FilePath("/dir/file.txt");
    set.add(path);

    assertTrue(set.contains(path));
  }

  @Test
  void contains_nonExistentPath_returnsFalse()
  {
    FilePath path = new FilePath("/dir/file.txt");

    assertFalse(set.contains(path));
  }

  @Test
  void contains_null_returnsFalse()
  {
    assertFalse(set.contains(null));
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  void contains_stringPath_works()
  {
    set.add(new FilePath("/dir/file.txt"));

    assertTrue(set.contains("/dir/file.txt"));
    assertFalse(set.contains("/dir/other.txt"));
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  void contains_javaPath_works()
  {
    set.add(new FilePath("/dir/file.txt"));

    assertTrue(set.contains(Path.of("/dir/file.txt")));
    assertFalse(set.contains(Path.of("/dir/other.txt")));
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  void contains_javaFile_works()
  {
    set.add(new FilePath("/dir/file.txt"));

    assertTrue(set.contains(new File("/dir/file.txt")));
    assertFalse(set.contains(new File("/dir/other.txt")));
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  void contains_wrongType_returnsFalse()
  {
    set.add(new FilePath("/dir/file.txt"));

    assertFalse(set.contains(123));
    assertFalse(set.contains(new Object()));
  }

//---------------------------------------------------------------------------
//endregion
//region Basic Operations: Size and isEmpty
//---------------------------------------------------------------------------

  @Test
  void size_emptySet_returnsZero()
  {
    assertEquals(0, set.size());
  }

  @Test
  void size_afterAdds_returnsCorrectCount()
  {
    set.add(new FilePath("/dir/file1.txt"));
    set.add(new FilePath("/dir/file2.txt"));
    set.add(new FilePath("/other/file1.txt"));

    assertEquals(3, set.size());
  }

  @Test
  void isEmpty_emptySet_returnsTrue()
  {
    assertTrue(set.isEmpty());
  }

  @Test
  void isEmpty_nonEmptySet_returnsFalse()
  {
    set.add(new FilePath("/dir/file.txt"));

    assertFalse(set.isEmpty());
  }

  @Test
  void isEmpty_afterRemoveAll_returnsTrue()
  {
    set.add(new FilePath("/dir/file.txt"));
    set.remove(new FilePath("/dir/file.txt"));

    assertTrue(set.isEmpty());
  }

//---------------------------------------------------------------------------
//endregion
//region Basic Operations: Clear
//---------------------------------------------------------------------------

  @Test
  void clear_emptySet_remainsEmpty()
  {
    set.clear();

    assertTrue(set.isEmpty());
  }

  @Test
  void clear_nonEmptySet_becomesEmpty()
  {
    set.add(new FilePath("/dir/file1.txt"));
    set.add(new FilePath("/dir/file2.txt"));

    set.clear();

    assertTrue(set.isEmpty());
    assertEquals(0, set.size());
  }

//---------------------------------------------------------------------------
//endregion
//region Collection Operations: AddAll
//---------------------------------------------------------------------------

  @Test
  void addAll_emptyCollection_returnsFalse()
  {
    boolean result = set.addAll(Collections.emptyList());

    assertFalse(result);
  }

  @Test
  void addAll_newPaths_returnsTrue()
  {
    List<FilePath> paths = List.of
    (
      new FilePath("C:/dir/file1.txt"),
      new FilePath("C:/dir/file2.txt")
    );

    boolean result = set.addAll(paths);

    assertTrue(result);
    assertEquals(2, set.size());
  }

  @Test
  void addAll_allDuplicates_returnsFalse()
  {
    FilePath path = new FilePath("C:/dir/file.txt");
    set.add(path);

    boolean result = set.addAll(List.of(path));

    assertFalse(result);
  }

  @Test
  void addAll_someDuplicates_returnsTrue()
  {
    FilePath path1 = new FilePath("C:/dir/file1.txt"),
             path2 = new FilePath("C:/dir/file2.txt");
    set.add(path1);

    boolean result = set.addAll(List.of(path1, path2));

    assertTrue(result);
    assertEquals(2, set.size());
  }

//---------------------------------------------------------------------------
//endregion
//region Collection Operations: RemoveAll
//---------------------------------------------------------------------------

  @Test
  void removeAll_emptyCollection_returnsFalse()
  {
    set.add(new FilePath("/dir/file.txt"));

    boolean result = set.removeAll(Collections.emptyList());

    assertFalse(result);
  }

  @Test
  void removeAll_existingPaths_returnsTrue()
  {
    FilePath path1 = new FilePath("/dir/file1.txt"),
             path2 = new FilePath("/dir/file2.txt");
    set.add(path1);
    set.add(path2);

    boolean result = set.removeAll(List.of(path1, path2));

    assertTrue(result);
    assertTrue(set.isEmpty());
  }

  @Test
  void removeAll_nonExistentPaths_returnsFalse()
  {
    set.add(new FilePath("/dir/file1.txt"));

    boolean result = set.removeAll(List.of(new FilePath("/dir/other.txt")));

    assertFalse(result);
  }

//---------------------------------------------------------------------------
//endregion
//region Collection Operations: RetainAll
//---------------------------------------------------------------------------

  @Test
  void retainAll_overlapping_keepsOnlyCommon()
  {
    FilePath path1 = new FilePath("C:/dir/file1.txt"),
             path2 = new FilePath("C:/dir/file2.txt"),
             path3 = new FilePath("C:/dir/file3.txt");
    set.add(path1);
    set.add(path2);
    set.add(path3);

    boolean result = set.retainAll(List.of(path1, path3));

    assertTrue(result);
    assertEquals(2, set.size());
    assertTrue(set.contains(path1));
    assertFalse(set.contains(path2));
    assertTrue(set.contains(path3));
  }

  @Test
  void retainAll_noOverlap_clearsSet()
  {
    set.add(new FilePath("C:/dir/file1.txt"));

    boolean result = set.retainAll(List.of(new FilePath("C:/dir/other.txt")));

    assertTrue(result);
    assertTrue(set.isEmpty());
  }

//---------------------------------------------------------------------------
//endregion
//region Collection Operations: RemoveIf
//---------------------------------------------------------------------------

  @Test
  void removeIf_matchingPaths_removesAndReturnsTrue()
  {
    FilePath path1 = new FilePath("C:/dir/file1.txt"),
             path2 = new FilePath("C:/dir/file2.txt"),
             path3 = new FilePath("C:/dir/other.txt");
    set.add(path1);
    set.add(path2);
    set.add(path3);

    boolean result = set.removeIf(p -> p.getNameOnly().toString().startsWith("file"));

    assertTrue(result);
    assertEquals(1, set.size());
    assertTrue(set.contains(path3));
  }

  @Test
  void removeIf_noMatches_returnsFalse()
  {
    set.add(new FilePath("C:/dir/file.txt"));

    boolean result = set.removeIf(p -> p.getNameOnly().toString().startsWith("other"));

    assertFalse(result);
    assertEquals(1, set.size());
  }

  @Test
  void removeIf_allSameFilename_cleansBucket()
  {
    // All paths have same filename; removing all should clean up the bucket
    FilePath path1 = new FilePath("C:/dir1/file.txt"),
             path2 = new FilePath("C:/dir2/file.txt"),
             path3 = new FilePath("C:/dir3/file.txt");
    set.add(path1);
    set.add(path2);
    set.add(path3);

    set.removeIf(p -> "file.txt".equals(p.getNameOnly().toString()));

    assertTrue(set.isEmpty());

    // Adding new path with same filename should work (bucket was cleaned)
    FilePath newPath = new FilePath("C:/dir4/file.txt");
    assertTrue(set.add(newPath));
    assertTrue(set.contains(newPath));
  }

//---------------------------------------------------------------------------
//endregion
//region Collection Operations: ContainsAll
//---------------------------------------------------------------------------

  @Test
  void containsAll_allPresent_returnsTrue()
  {
    FilePath path1 = new FilePath("C:/dir/file1.txt"),
             path2 = new FilePath("C:/dir/file2.txt");
    set.add(path1);
    set.add(path2);

    assertTrue(set.containsAll(List.of(path1, path2)));
  }

  @Test
  void containsAll_someMissing_returnsFalse()
  {
    FilePath path1 = new FilePath("C:/dir/file1.txt"),
             path2 = new FilePath("C:/dir/file2.txt");
    set.add(path1);

    assertFalse(set.containsAll(List.of(path1, path2)));
  }

  @Test
  void containsAll_emptyCollection_returnsTrue()
  {
    assertTrue(set.containsAll(Collections.emptyList()));
  }

//---------------------------------------------------------------------------
//endregion
//region Collection Operations: ToArray
//---------------------------------------------------------------------------

  @Test
  void toArray_emptySet_returnsEmptyArray()
  {
    Object[] array = set.toArray();

    assertEquals(0, array.length);
  }

  @Test
  void toArray_nonEmptySet_containsAllElements()
  {
    FilePath path1 = new FilePath("C:/dir/file1.txt"),
             path2 = new FilePath("C:/dir/file2.txt");
    set.add(path1);
    set.add(path2);

    Object[] array = set.toArray();

    assertEquals(2, array.length);
    Set<Object> arraySet = Set.of(array);
    assertTrue(arraySet.contains(path1));
    assertTrue(arraySet.contains(path2));
  }

  @Test
  void toArrayTyped_emptySet_returnsEmptyArray()
  {
    FilePath[] array = set.toArray(new FilePath[0]);

    assertEquals(0, array.length);
  }

  @Test
  void toArrayTyped_arrayTooSmall_createsNewArray()
  {
    set.add(new FilePath("C:/dir/file1.txt"));
    set.add(new FilePath("C:/dir/file2.txt"));

    FilePath[] array = set.toArray(new FilePath[0]);

    assertEquals(2, array.length);
  }

  @Test
  void toArrayTyped_arrayTooLarge_setsNullTerminator()
  {
    set.add(new FilePath("C:/dir/file.txt"));

    FilePath[] array = new FilePath[5];
    Arrays.fill(array, new FilePath("C:/dummy.txt"));

    FilePath[] result = set.toArray(array);

    assertSame(array, result);
    assertNotNull(result[0]);
    assertNull(result[1]);  // Null terminator per Collection.toArray contract
  }

//---------------------------------------------------------------------------
//endregion
//region Iterator
//---------------------------------------------------------------------------

  @Test
  void iterator_emptySet_hasNoElements()
  {
    Iterator<FilePath> iter = set.iterator();

    assertFalse(iter.hasNext());
  }

  @Test
  void iterator_nonEmptySet_iteratesAllElements()
  {
    FilePath path1 = new FilePath("/dir1/file.txt"),
             path2 = new FilePath("/dir2/file.txt"),
             path3 = new FilePath("/dir/other.txt");
    set.add(path1);
    set.add(path2);
    set.add(path3);

    Set<FilePath> iterated = new HashSet<>();
    for (FilePath path : set)
      iterated.add(path);

    assertEquals(3, iterated.size());
    assertTrue(iterated.contains(path1));
    assertTrue(iterated.contains(path2));
    assertTrue(iterated.contains(path3));
  }

  @Test
  void iterator_remove_throwsUnsupportedOperationException()
  {
    FilePath path = new FilePath("/dir/file.txt");
    set.add(path);

    Iterator<FilePath> iter = set.iterator();
    iter.next();

    // Stream-based iterator doesn't support remove(); use removeIf() instead
    assertThrows(UnsupportedOperationException.class, iter::remove);
  }

//---------------------------------------------------------------------------
//endregion
//region Constructor With Collection
//---------------------------------------------------------------------------

  @Test
  void constructorWithCollection_copiesAllElements()
  {
    List<FilePath> paths = List.of
    (
      new FilePath("/dir/file1.txt"),
      new FilePath("/dir/file2.txt")
    );

    FilePathSet newSet = new FilePathSet(paths);

    assertEquals(2, newSet.size());
    assertTrue(newSet.containsAll(paths));
  }

  @Test
  void constructorWithCollection_emptyCollection_createsEmptySet()
  {
    FilePathSet newSet = new FilePathSet(Collections.emptyList());

    assertTrue(newSet.isEmpty());
  }

//---------------------------------------------------------------------------
//endregion
//region Bucket Cleanup: Memory Leak Prevention
//---------------------------------------------------------------------------

  @Test
  void remove_lastItemInBucket_cleansBucket()
  {
    // Add files with same name in different directories
    FilePath path1 = new FilePath("C:/dir1/file.txt"),
             path2 = new FilePath("C:/dir2/file.txt");
    set.add(path1);
    set.add(path2);

    // Remove both; bucket should be cleaned up
    set.remove(path1);
    set.remove(path2);

    assertTrue(set.isEmpty());

    // Add new file with same name; should work correctly
    FilePath path3 = new FilePath("C:/dir3/file.txt");
    assertTrue(set.add(path3));
    assertTrue(set.contains(path3));
  }

  @Test
  void remove_manyFilesWithSameNameThenAll_sizeIsZero()
  {
    List<FilePath> paths = new ArrayList<>();
    for (int ndx = 0; ndx < 100; ndx++)
    {
      FilePath path = new FilePath("/dir" + ndx + "/file.txt");
      paths.add(path);
      set.add(path);
    }

    assertEquals(100, set.size());

    for (FilePath path : paths)
      set.remove(path);

    assertEquals(0, set.size());
    assertTrue(set.isEmpty());
  }

//---------------------------------------------------------------------------
//endregion
//region Concurrency: Basic Thread Safety
//---------------------------------------------------------------------------

  @Test
  void concurrent_multipleAdds_allSucceed() throws Exception
  {
    int numThreads = 10,
        pathsPerThread = 100;

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    try
    {
      CountDownLatch latch = new CountDownLatch(numThreads);

      for (int t = 0; t < numThreads; t++)
      {
        final int threadId = t;
        executor.submit(() ->
        {
          try
          {
            for (int ndx = 0; ndx < pathsPerThread; ndx++)
            {
              FilePath path = new FilePath("/thread" + threadId + "/file" + ndx + ".txt");
              set.add(path);
            }
          }
          finally
          {
            latch.countDown();
          }
        });
      }

      assertTrue(latch.await(30, TimeUnit.SECONDS), "Test timed out");
      assertEquals(numThreads * pathsPerThread, set.size());
    }
    finally
    {
      executor.shutdownNow();
    }
  }

  @Test
  void concurrent_addsAndRemoves_noExceptions() throws Exception
  {
    int numThreads = 10,
        opsPerThread = 500;
    AtomicBoolean failed = new AtomicBoolean(false);

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    try
    {
      CountDownLatch latch = new CountDownLatch(numThreads);

      for (int t = 0; t < numThreads; t++)
      {
        executor.submit(() ->
        {
          try
          {
            for (int ndx = 0; ndx < opsPerThread; ndx++)
            {
              FilePath path = new FilePath("C:/dir/file" + (ndx % 50) + ".txt");
              if ((ndx % 2) == 0)
                set.add(path);
              else
                set.remove(path);
            }
          }
          catch (Exception e)
          {
            failed.set(true);
            e.printStackTrace();
          }
          finally
          {
            latch.countDown();
          }
        });
      }

      assertTrue(latch.await(30, TimeUnit.SECONDS), "Test timed out");
      assertFalse(failed.get(), "Concurrent operations threw exception");
    }
    finally
    {
      executor.shutdownNow();
    }
  }

  @Test
  void concurrent_mixedOperations_noExceptions() throws Exception
  {
    int numThreads = 20,
        opsPerThread = 200;
    AtomicBoolean failed = new AtomicBoolean(false);

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    try
    {
      CountDownLatch latch = new CountDownLatch(numThreads);

      // Pre-populate some data
      for (int ndx = 0; ndx < 50; ndx++)
        set.add(new FilePath("/dir/file" + ndx + ".txt"));

      for (int t = 0; t < numThreads; t++)
      {
        executor.submit(() ->
        {
          try
          {
            Random random = new Random();
            for (int ndx = 0; ndx < opsPerThread; ndx++)
            {
              FilePath path = new FilePath("/dir/file" + random.nextInt(100) + ".txt");
              switch (ndx % 4)
              {
                case 0 -> set.add(path);
                case 1 -> set.remove(path);
                case 2 -> set.contains(path);
                case 3 -> set.size();
              }
            }
          }
          catch (Exception e)
          {
            failed.set(true);
            e.printStackTrace();
          }
          finally
          {
            latch.countDown();
          }
        });
      }

      assertTrue(latch.await(30, TimeUnit.SECONDS), "Test timed out");
      assertFalse(failed.get(), "Concurrent operations threw exception");
    }
    finally
    {
      executor.shutdownNow();
    }
  }

//---------------------------------------------------------------------------
//endregion
//region Concurrency: Add Retry Mechanism
//---------------------------------------------------------------------------

  @Test
  void concurrent_addAndRemoveSameFilename_noLostUpdates() throws Exception
  {
    // This tests the retry mechanism in add() that handles the race condition
    // when a remove() cleans up the bucket during an add() operation.

    int iterations = 100,
        numThreads = 4;

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    try
    {
      for (int iter = 0; iter < iterations; iter++)
      {
        set.clear();
        CountDownLatch startLatch = new CountDownLatch(1),
                       doneLatch = new CountDownLatch(numThreads);

        // Two threads add different paths with same filename
        // Two threads remove them
        FilePath path1 = new FilePath("/dir1/file.txt"),
                 path2 = new FilePath("/dir2/file.txt");

        executor.submit(() -> awaitAndRun(startLatch, doneLatch, () -> set.add(path1)));
        executor.submit(() -> awaitAndRun(startLatch, doneLatch, () -> set.add(path2)));
        executor.submit(() -> awaitAndRun(startLatch, doneLatch, () -> set.remove(path1)));
        executor.submit(() -> awaitAndRun(startLatch, doneLatch, () -> set.remove(path2)));

        startLatch.countDown();
        assertTrue(doneLatch.await(5, TimeUnit.SECONDS), "Iteration " + iter + " timed out");

        // The set should be consistent: either empty or containing paths that are actually present
        for (FilePath path : set)
          assertTrue(set.contains(path), "Set contains path but contains() returns false");
      }
    }
    finally
    {
      executor.shutdownNow();
    }
  }

  private static void awaitAndRun(CountDownLatch startLatch, CountDownLatch doneLatch, Runnable action)
  {
    try
    {
      startLatch.await();
      action.run();
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
    }
    finally
    {
      doneLatch.countDown();
    }
  }

//---------------------------------------------------------------------------
//endregion
//region Concurrency: Clear Under Contention
//---------------------------------------------------------------------------

  @Test
  void concurrent_clearDuringOperations_noExceptions() throws Exception
  {
    int numWriters = 5,
        numReaders = 5,
        opsPerThread = 500;
    AtomicBoolean failed = new AtomicBoolean(false);
    AtomicReference<Throwable> caughtException = new AtomicReference<>();

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numWriters + numReaders + 1);

    try
    {
      CountDownLatch latch = new CountDownLatch(numWriters + numReaders + 1);

      // Writers
      for (int t = 0; t < numWriters; t++)
      {
        final int threadId = t;
        executor.submit(() ->
        {
          try
          {
            for (int ndx = 0; ndx < opsPerThread; ndx++)
            {
              set.add(new FilePath("C:/writer" + threadId + "/file" + ndx + ".txt"));
            }
          }
          catch (Exception e)
          {
            caughtException.compareAndSet(null, e);
            failed.set(true);
          }
          finally
          {
            latch.countDown();
          }
        });
      }

      // Readers
      for (int t = 0; t < numReaders; t++)
      {
        executor.submit(() ->
        {
          try
          {
            for (int ndx = 0; ndx < opsPerThread; ndx++)
              set.contains(new FilePath("C:/writer0/file" + (ndx % 100) + ".txt"));
          }
          catch (Exception e)
          {
            caughtException.compareAndSet(null, e);
            failed.set(true);
          }
          finally
          {
            latch.countDown();
          }
        });
      }

      // Clearer
      executor.submit(() ->
      {
        try
        {
          for (int ndx = 0; ndx < 10; ndx++)
          {
            Thread.sleep(5);
            set.clear();
          }
        }
        catch (Exception e)
        {
          caughtException.compareAndSet(null, e);
          failed.set(true);
        }
        finally
        {
          latch.countDown();
        }
      });

      assertTrue(latch.await(30, TimeUnit.SECONDS), "Test timed out");
      if (failed.get())
      {
        Throwable ex = caughtException.get();
        fail("Exception thrown during concurrent operations: " + (ex == null ? "unknown" : ex.toString()), ex);
      }
    }
    finally
    {
      executor.shutdownNow();
    }
  }

//---------------------------------------------------------------------------
//endregion
//region Stress Tests
//---------------------------------------------------------------------------

  @Test
  void stress_manyFilesWithSameFilename_handledCorrectly()
  {
    assumeTrue(TestConfig.runLongTests());

    int numPaths = 10000;

    for (int ndx = 0; ndx < numPaths; ndx++)
      set.add(new FilePath("/dir" + ndx + "/samename.txt"));

    assertEquals(numPaths, set.size());

    // Remove half
    for (int ndx = 0; ndx < numPaths / 2; ndx++)
      set.remove(new FilePath("/dir" + ndx + "/samename.txt"));

    assertEquals(numPaths / 2, set.size());
  }

  @Test
  void stress_manyDifferentFilenames_handledCorrectly()
  {
    assumeTrue(TestConfig.runLongTests());

    int numPaths = 10000;

    for (int ndx = 0; ndx < numPaths; ndx++)
      set.add(new FilePath("C:/dir/file" + ndx + ".txt"));

    assertEquals(numPaths, set.size());

    // Check all present
    for (int ndx = 0; ndx < numPaths; ndx++)
      assertTrue(set.contains(new FilePath("C:/dir/file" + ndx + ".txt")));
  }

  @Test
  void stress_addRemoveCycle_noMemoryLeak()
  {
    assumeTrue(TestConfig.runLongTests());

    // Add and remove many items with same filename to test bucket cleanup
    for (int cycle = 0; cycle < 100; cycle++)
    {
      for (int ndx = 0; ndx < 100; ndx++)
        set.add(new FilePath("/dir" + ndx + "/file.txt"));

      for (int ndx = 0; ndx < 100; ndx++)
        set.remove(new FilePath("/dir" + ndx + "/file.txt"));

      assertTrue(set.isEmpty(), "Set should be empty after cycle " + cycle);
    }
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
