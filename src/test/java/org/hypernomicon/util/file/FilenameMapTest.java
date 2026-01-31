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

import static org.hypernomicon.util.file.FilenameRules.*;

import java.io.IOException;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;

import org.hypernomicon.util.file.FilenameRules.CaseFoldingMode;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

//---------------------------------------------------------------------------

/**
 * Unit tests for {@link FilenameMap}.
 * <p>
 * These tests focus on FilenameMap-specific behavior: key normalization,
 * case sensitivity rules, snapshot views, and concurrency with case variants.
 * Basic Map operations are delegated to ConcurrentHashMap and not tested here.
 */
class FilenameMapTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Creates a FilenameMap by probing the filesystem at the given path. */
  private static <T> FilenameMap<T> probed(Path path) throws IOException
  {
    return new FilenameMap<>(detect(path));
  }

//---------------------------------------------------------------------------
//region Key Normalization: Case Insensitive
//---------------------------------------------------------------------------

  @Test
  void caseInsensitive_put_caseVariantReturnsOldValue()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value1");

    String result = map.put("FILE.TXT", "value2");

    assertEquals("value1", result);
    assertEquals(1, map.size());
  }

  @Test
  void caseInsensitive_get_caseVariantReturnsValue()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertEquals("value", map.get("FILE.TXT"));
    assertEquals("value", map.get("File.Txt"));
    assertEquals("value", map.get("FILE.txt"));
  }

  @Test
  void caseInsensitive_remove_caseVariantRemovesEntry()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    String result = map.remove("FILE.TXT");

    assertEquals("value", result);
    assertEquals(0, map.size());
  }

  @Test
  void caseInsensitive_containsKey_caseVariantReturnsTrue()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertTrue(map.containsKey("FILE.TXT"));
    assertTrue(map.containsKey("File.Txt"));
  }

  @Test
  void caseInsensitive_putIfAbsent_caseVariantReturnsExisting()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "first");

    String result = map.putIfAbsent("FILE.TXT", "second");

    assertEquals("first", result);
    assertEquals("first", map.get("file.txt"));
  }

  @Test
  void caseInsensitive_computeIfAbsent_caseVariantReturnsExisting()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "existing");

    String result = map.computeIfAbsent("FILE.TXT", k -> "computed");

    assertEquals("existing", result);
  }

  @Test
  void caseInsensitive_computeIfPresent_caseVariantUpdates()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "old");

    String result = map.computeIfPresent("FILE.TXT", (k, v) -> v + "-updated");

    assertEquals("old-updated", result);
    assertEquals("old-updated", map.get("file.txt"));
  }

  @Test
  void caseInsensitive_compute_caseVariantUpdates()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "old");

    String result = map.compute("FILE.TXT", (k, v) -> v + "-updated");

    assertEquals("old-updated", result);
    assertEquals(1, map.size());
  }

  @Test
  void caseInsensitive_merge_caseVariantMerges()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "old");

    String result = map.merge("FILE.TXT", "new", (old, v) -> old + '+' + v);

    assertEquals("old+new", result);
  }

  @Test
  void caseInsensitive_replace_caseVariantReplaces()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "old");

    String result = map.replace("FILE.TXT", "new");

    assertEquals("old", result);
    assertEquals("new", map.get("file.txt"));
  }

  @Test
  void caseInsensitive_replaceConditional_caseVariantReplaces()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "old");

    boolean result = map.replace("FILE.TXT", "old", "new");

    assertTrue(result);
    assertEquals("new", map.get("file.txt"));
  }

  @Test
  void caseInsensitive_removeConditional_caseVariantRemoves()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    boolean result = map.remove("FILE.TXT", "value");

    assertTrue(result);
    assertEquals(0, map.size());
  }

//---------------------------------------------------------------------------
//endregion
//region Key Normalization: Case Sensitive
//---------------------------------------------------------------------------

  @Test
  void caseSensitive_differentCases_distinctEntries()
  {
    FilenameMap<String> map = new FilenameMap<>(LINUX_HEURISTIC);

    map.put("file.txt", "lower");
    map.put("FILE.TXT", "upper");
    map.put("File.Txt", "mixed");

    assertEquals(3, map.size());
    assertEquals("lower", map.get("file.txt"));
    assertEquals("upper", map.get("FILE.TXT"));
    assertEquals("mixed", map.get("File.Txt"));
  }

  @Test
  void caseSensitive_caseVariant_doesNotMatch()
  {
    FilenameMap<String> map = new FilenameMap<>(LINUX_HEURISTIC);
    map.put("file.txt", "value");

    assertNull(map.get("FILE.TXT"));
    assertFalse(map.containsKey("FILE.TXT"));
  }

//---------------------------------------------------------------------------
//endregion
//region Normalized Key Storage
//---------------------------------------------------------------------------

  @Test
  void keySet_returnsNormalizedKeys()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("MixedCase.TXT", "value");

    Set<String> keys = map.keySet();

    assertTrue(keys.contains("mixedcase.txt"));
    assertEquals(1, keys.size());
  }

  @Test
  void entrySet_containsNormalizedKeys()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("MixedCase.TXT", "value");

    var entries = map.entrySet();
    var entry = entries.iterator().next();

    assertEquals("mixedcase.txt", entry.getKey());
  }

  @Test
  void computeIfAbsent_functionReceivesNormalizedKey()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    AtomicReference<String> receivedKey = new AtomicReference<>();

    map.computeIfAbsent("MixedCase.TXT", k ->
    {
      receivedKey.set(k);
      return "value";
    });

    assertEquals("mixedcase.txt", receivedKey.get());
  }

  @Test
  void compute_functionReceivesNormalizedKey()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "old");
    AtomicReference<String> receivedKey = new AtomicReference<>();

    map.compute("FILE.TXT", (k, v) ->
    {
      receivedKey.set(k);
      return v;
    });

    assertEquals("file.txt", receivedKey.get());
  }

//---------------------------------------------------------------------------
//endregion
//region Snapshot Views
//---------------------------------------------------------------------------

  @Test
  void keySet_returnsSnapshot_notLiveView()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("a.txt", "1");

    Set<String> keys = map.keySet();
    map.put("b.txt", "2");

    assertFalse(keys.contains("b.txt"));
    assertEquals(1, keys.size());
  }

  @Test
  void values_returnsSnapshot_notLiveView()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("a.txt", "1");

    Collection<String> values = map.values();
    map.put("b.txt", "2");

    assertFalse(values.contains("2"));
    assertEquals(1, values.size());
  }

  @Test
  void entrySet_returnsSnapshot_notLiveView()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("a.txt", "1");

    var entries = map.entrySet();
    map.put("b.txt", "2");

    assertEquals(1, entries.size());
  }

  @Test
  void keySet_isUnmodifiable()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("a.txt", "1");

    Set<String> keys = map.keySet();

    assertThrows(UnsupportedOperationException.class, () -> keys.add("b.txt"));
    assertThrows(UnsupportedOperationException.class, () -> keys.remove("a.txt"));
    assertThrows(UnsupportedOperationException.class, keys::clear);
  }

  @Test
  void values_isUnmodifiable()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("a.txt", "1");

    Collection<String> values = map.values();

    assertThrows(UnsupportedOperationException.class, () -> values.add("2"));
    assertThrows(UnsupportedOperationException.class, () -> values.remove("1"));
    assertThrows(UnsupportedOperationException.class, values::clear);
  }

  @Test
  void entrySet_isUnmodifiable()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("a.txt", "1");

    var entries = map.entrySet();

    assertThrows(UnsupportedOperationException.class, () -> entries.add(Map.entry("b.txt", "2")));
    assertThrows(UnsupportedOperationException.class, entries::clear);
  }

  @Test
  void entrySet_setValueDoesNotModifyMap()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("a.txt", "original");

    var entries = map.entrySet();
    var entry = entries.iterator().next();

    // Map.entry() returns immutable entries, so setValue throws UnsupportedOperationException
    assertThrows(UnsupportedOperationException.class, () -> entry.setValue("modified"));

    // Verify map was not modified
    assertEquals("original", map.get("a.txt"));
  }

//---------------------------------------------------------------------------
//endregion
//region Custom Rules
//---------------------------------------------------------------------------

  @Test
  void rulesConstructor_caseInsensitiveRules()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    FilenameMap<String> map = new FilenameMap<>(rules);

    map.put("File.txt", "value");

    assertEquals("value", map.get("FILE.TXT"));
    assertEquals(1, map.size());
  }

  @Test
  void rulesConstructor_caseSensitiveRules()
  {
    // caseInsensitive=false means case-sensitive; caseFoldingMode is ignored
    FilenameRules rules = new FilenameRules(false, false, false, false, CaseFoldingMode.SIMPLE);
    FilenameMap<String> map = new FilenameMap<>(rules);

    map.put("File.txt", "value");

    assertNull(map.get("FILE.TXT"));
    assertEquals("value", map.get("File.txt"));
  }

  @Test
  void rulesConstructor_unicodeNormalization()
  {
    FilenameRules rules = new FilenameRules(false, true, false, false, CaseFoldingMode.SIMPLE);
    FilenameMap<String> map = new FilenameMap<>(rules);

    // NFC form (single codepoint) and NFD form (base + combining) should match
    String nfc = "\u00E9",  // é as single codepoint
           nfd = "e\u0301"; // e + combining acute

    map.put(nfc, "value");

    assertEquals("value", map.get(nfd));
  }

  @Test
  void rulesConstructor_combinedRules()
  {
    FilenameRules rules = new FilenameRules(true, true, true, true, CaseFoldingMode.FULL);
    FilenameMap<String> map = new FilenameMap<>(rules);

    map.put("File.txt", "value");

    // Case insensitive
    assertEquals("value", map.get("FILE.TXT"));
  }

//---------------------------------------------------------------------------
//endregion
//region probed() Factory Method
//---------------------------------------------------------------------------

  @Test
  void probedFactoryMethod_withPath_createsWorkingMap(@TempDir java.nio.file.Path tempDir) throws Exception
  {
    FilenameMap<String> map = probed(tempDir);

    map.put("test.txt", "value");
    assertEquals("value", map.get("test.txt"));
  }

  @Test
  void probedFactoryMethod_agreesWithFilesystem(@TempDir java.nio.file.Path tempDir) throws Exception
  {
    // Create a file and test if the filesystem is case-sensitive
    java.nio.file.Path file = tempDir.resolve("testprobe.txt");
    java.nio.file.Files.createFile(file);

    try
    {
      boolean filesystemCaseInsensitive = java.nio.file.Files.exists(tempDir.resolve("TESTPROBE.TXT"));
      FilenameMap<String> map = probed(tempDir);

      map.put("testprobe.txt", "value");
      boolean mapEquivalent = map.containsKey("TESTPROBE.TXT");

      assertEquals(filesystemCaseInsensitive, mapEquivalent,
          "Map should agree with filesystem on case sensitivity");
    }
    finally
    {
      java.nio.file.Files.deleteIfExists(file);
    }
  }

//---------------------------------------------------------------------------
//endregion
//region Locale Independence
//---------------------------------------------------------------------------

  @Test
  void localeIndependence_turkishI_usesRootLocale()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    // Turkish locale would lowercase I to dotless ı, but we use Locale.ROOT
    map.put("FILE", "value");

    // With Locale.ROOT, "file" matches "FILE"
    assertEquals("value", map.get("file"));
  }

  @Test
  void localeIndependence_allAsciiLetters_matchCaseInsensitively()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    String lower = "abcdefghijklmnopqrstuvwxyz.txt",
           upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ.txt";

    map.put(lower, "value");

    assertEquals("value", map.get(upper));
  }

//---------------------------------------------------------------------------
//endregion
//region Edge Cases
//---------------------------------------------------------------------------

  @Test
  void emptyKey_isAccepted()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    map.put("", "value");

    assertEquals("value", map.get(""));
    assertEquals(1, map.size());
  }

  @Test
  void unicodeFilenames_work()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    map.put("日本語ファイル.txt", "japanese");
    map.put("файл.txt", "russian");
    map.put("αρχείο.txt", "greek");

    assertEquals("japanese", map.get("日本語ファイル.txt"));
    assertEquals("russian", map.get("файл.txt"));
    assertEquals("greek", map.get("αρχείο.txt"));
  }

  @Test
  void longFilename_works()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    String longName = "a".repeat(255) + ".txt";
    map.put(longName, "value");

    assertEquals("value", map.get(longName));
    assertEquals("value", map.get(longName.toUpperCase()));
  }

//---------------------------------------------------------------------------
//endregion
//region Concurrency
//---------------------------------------------------------------------------

  @Test
  void concurrent_putsWithCaseVariants_onlyOneEntry() throws Exception
  {
    FilenameMap<Integer> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    int numThreads = 10;

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    try
    {
      String[] caseVariants = {"file.txt", "FILE.TXT", "File.Txt", "FILE.txt", "file.TXT"};
      CountDownLatch startLatch = new CountDownLatch(1),
                     doneLatch = new CountDownLatch(numThreads);

      for (int ndx = 0; ndx < numThreads; ndx++)
      {
        final int threadId = ndx;
        executor.submit(() ->
        {
          try
          {
            startLatch.await();
            String variant = caseVariants[threadId % caseVariants.length];
            map.put(variant, threadId);
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
      doneLatch.await(5, TimeUnit.SECONDS);

      // All case variants should map to a single entry
      assertEquals(1, map.size());
    }
    finally
    {
      executor.shutdownNow();
    }
  }

  @Test
  void concurrent_computeIfAbsent_functionCalledOnce() throws Exception
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    AtomicInteger callCount = new AtomicInteger(0);
    int numThreads = 20;

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);

    try
    {
      CountDownLatch startLatch = new CountDownLatch(1),
                     doneLatch = new CountDownLatch(numThreads);
      String[] caseVariants = {"file.txt", "FILE.TXT", "File.Txt"};

      for (int ndx = 0; ndx < numThreads; ndx++)
      {
        final int threadId = ndx;
        executor.submit(() ->
        {
          try
          {
            startLatch.await();
            String variant = caseVariants[threadId % caseVariants.length];
            map.computeIfAbsent(variant, k ->
            {
              callCount.incrementAndGet();
              try { Thread.sleep(10); } catch (InterruptedException ignored) {}
              return "computed";
            });
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
      doneLatch.await(5, TimeUnit.SECONDS);

      // Function should only be called once despite concurrent access
      assertEquals(1, callCount.get());
      assertEquals(1, map.size());
    }
    finally
    {
      executor.shutdownNow();
    }
  }

  @Test
  void concurrent_mixedOperations_noExceptions() throws Exception
  {
    FilenameMap<Integer> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    int numThreads = 8,
        opsPerThread = 500;

    @SuppressWarnings("resource")
    ExecutorService executor = Executors.newFixedThreadPool(numThreads);
    AtomicBoolean failed = new AtomicBoolean(false);

    try
    {
      CountDownLatch doneLatch = new CountDownLatch(numThreads);

      for (int ndx = 0; ndx < numThreads; ndx++)
      {
        final int threadId = ndx;
        executor.submit(() ->
        {
          try
          {
            Random random = new Random(threadId);
            for (int opNdx = 0; opNdx < opsPerThread; opNdx++)
            {
              String key = "file" + random.nextInt(20) + ".txt";
              if (random.nextBoolean())
                key = key.toUpperCase();

              final int opValue = opNdx;
              switch (random.nextInt(5))
              {
                case 0 -> map.put(key, opValue);
                case 1 -> map.get(key);
                case 2 -> map.remove(key);
                case 3 -> map.containsKey(key);
                case 4 -> map.computeIfAbsent(key, k -> opValue);
              }
            }
          }
          catch (Exception e)
          {
            failed.set(true);
          }
          finally
          {
            doneLatch.countDown();
          }
        });
      }

      doneLatch.await(30, TimeUnit.SECONDS);
      assertFalse(failed.get(), "Concurrent operations should not throw exceptions");
    }
    finally
    {
      executor.shutdownNow();
    }
  }

//---------------------------------------------------------------------------
//endregion
//region Fuzzer Tests
//---------------------------------------------------------------------------

  /**
   * Reference implementation for comparison: a simple case-insensitive map
   * using TreeMap with case-insensitive comparator.
   */
  private static class ReferenceMap<T>
  {
    private final Map<String, T> map = new TreeMap<>(String.CASE_INSENSITIVE_ORDER);

    private T put(String key, T value)
    {
      if ((key == null) || (value == null)) return null;
      return map.put(key, value);
    }

    private T get(String key)
    {
      if (key == null) return null;
      return map.get(key);
    }

    private T remove(String key)
    {
      if (key == null) return null;
      return map.remove(key);
    }

    private boolean containsKey(String key)
    {
      if (key == null) return false;
      return map.containsKey(key);
    }

    private int size() { return map.size(); }

    private void clear() { map.clear(); }
  }

  @RepeatedTest(5)
  void fuzzer_randomOperations_matchReferenceModel()
  {
    FilenameMap<Integer> testMap = new FilenameMap<>(WINDOWS_HEURISTIC);
    ReferenceMap<Integer> refMap = new ReferenceMap<>();
    Random random = new Random();

    String[] keyPool = new String[50];
    for (int ndx = 0; ndx < keyPool.length; ndx++)
    {
      keyPool[ndx] = "file" + ndx + ".txt";
    }

    int operations = 1000;

    for (int op = 0; op < operations; op++)
    {
      String key = keyPool[random.nextInt(keyPool.length)];
      // Randomly vary case
      if (random.nextBoolean())
        key = key.toUpperCase();
      else if (random.nextBoolean())
        key = key.substring(0, 1).toUpperCase() + key.substring(1);

      int value = random.nextInt(1000),
          operation = random.nextInt(6);

      switch (operation)
      {
        case 0 -> // put
        {
          Integer testResult = testMap.put(key, value),
                  refResult = refMap.put(key, value);
          assertEquals(refResult, testResult, "put mismatch for key: " + key);
        }
        case 1 -> // get
        {
          Integer testResult = testMap.get(key),
                  refResult = refMap.get(key);
          assertEquals(refResult, testResult, "get mismatch for key: " + key);
        }
        case 2 -> // remove
        {
          Integer testResult = testMap.remove(key),
                  refResult = refMap.remove(key);
          assertEquals(refResult, testResult, "remove mismatch for key: " + key);
        }
        case 3 -> // containsKey
        {
          boolean testResult = testMap.containsKey(key),
                  refResult = refMap.containsKey(key);
          assertEquals(refResult, testResult, "containsKey mismatch for key: " + key);
        }
        case 4 -> // size check
        {
          assertEquals(refMap.size(), testMap.size(), "size mismatch");
        }
        case 5 -> // clear (rarely)
        {
          if (random.nextInt(100) < 5)  // 5% chance
          {
            testMap.clear();
            refMap.clear();
            assertEquals(0, testMap.size(), "clear didn't empty map");
          }
        }
      }
    }

    assertEquals(refMap.size(), testMap.size(), "Final size mismatch");
  }

//---------------------------------------------------------------------------
//endregion
//region Default Constructor
//---------------------------------------------------------------------------

  @Test
  void defaultConstructor_works()
  {
    FilenameMap<String> map = new FilenameMap<>();

    map.put("test.txt", "value");
    assertEquals("value", map.get("test.txt"));
  }

//---------------------------------------------------------------------------
//endregion
//region Null Key Handling
//---------------------------------------------------------------------------

  @Test
  void put_nullKey_throwsNPE()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(NullPointerException.class, () -> map.put(null, "value"));
  }

  @Test
  void putIfAbsent_nullKey_throwsNPE()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(NullPointerException.class, () -> map.putIfAbsent(null, "value"));
  }

  @Test
  void computeIfAbsent_nullKey_throwsNPE()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(NullPointerException.class, () -> map.computeIfAbsent(null, k -> "value"));
  }

  @Test
  void compute_nullKey_throwsNPE()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(NullPointerException.class, () -> map.compute(null, (k, v) -> "value"));
  }

  @Test
  void merge_nullKey_throwsNPE()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(NullPointerException.class, () -> map.merge(null, "value", (a, b) -> a + b));
  }

  @Test
  void replace_nullKey_throwsNPE()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(NullPointerException.class, () -> map.replace(null, "value"));
  }

  @Test
  void replaceConditional_nullKey_throwsNPE()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(NullPointerException.class, () -> map.replace(null, "old", "new"));
  }

  @Test
  void computeIfPresent_nullKey_throwsNPE()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(NullPointerException.class, () -> map.computeIfPresent(null, (k, v) -> v));
  }

  @Test
  void get_nullKey_returnsNull()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertNull(map.get(null));
  }

  @Test
  void containsKey_nullKey_returnsFalse()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertFalse(map.containsKey(null));
  }

  @Test
  void remove_nullKey_returnsNull()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertNull(map.remove(null));
    assertEquals(1, map.size());
  }

  @Test
  void removeConditional_nullKey_returnsFalse()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertFalse(map.remove(null, "value"));
    assertEquals(1, map.size());
  }

  @Test
  void getOrDefault_nullKey_returnsDefault()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertEquals("default", map.getOrDefault(null, "default"));
  }

//---------------------------------------------------------------------------
//endregion
//region Non-String Key Handling
//---------------------------------------------------------------------------

  @SuppressWarnings("unlikely-arg-type")
  @Test
  void get_nonStringKey_returnsNull()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertNull(map.get(42));
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  void containsKey_nonStringKey_returnsFalse()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertFalse(map.containsKey(42));
  }

  @SuppressWarnings("unlikely-arg-type")
  @Test
  void remove_nonStringKey_returnsNull()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertNull(map.remove(42));
    assertEquals(1, map.size());
  }

  @Test
  void getOrDefault_nonStringKey_returnsDefault()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertEquals("default", map.getOrDefault(42, "default"));
  }

//---------------------------------------------------------------------------
//endregion
//region Additional Method Coverage
//---------------------------------------------------------------------------

  @Test
  void getOrDefault_existingKey_returnsValue()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertEquals("value", map.getOrDefault("FILE.TXT", "default"));
  }

  @Test
  void getOrDefault_missingKey_returnsDefault()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertEquals("default", map.getOrDefault("missing.txt", "default"));
  }

  @Test
  void containsValue_existingValue_returnsTrue()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertTrue(map.containsValue("value"));
  }

  @Test
  void containsValue_missingValue_returnsFalse()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertFalse(map.containsValue("other"));
  }

  @Test
  void containsValue_nullValue_returnsFalse()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertFalse(map.containsValue(null));
  }

  @Test
  void isEmpty_emptyMap_returnsTrue()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertTrue(map.isEmpty());
  }

  @Test
  void isEmpty_nonEmptyMap_returnsFalse()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    assertFalse(map.isEmpty());
  }

  @Test
  void clear_removesAllEntries()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("a.txt", "1");
    map.put("b.txt", "2");
    map.put("c.txt", "3");

    map.clear();

    assertTrue(map.isEmpty());
    assertEquals(0, map.size());
  }

  @Test
  void putAll_addsAllEntries()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    Map<String, String> source = new HashMap<>();
    source.put("a.txt", "1");
    source.put("b.txt", "2");

    map.putAll(source);

    assertEquals(2, map.size());
    assertEquals("1", map.get("A.TXT"));
    assertEquals("2", map.get("B.TXT"));
  }

  @Test
  void putAll_withCaseVariants_normalizesKeys()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    Map<String, String> source = new LinkedHashMap<>();
    source.put("file.txt", "lower");
    source.put("FILE.TXT", "upper");

    map.putAll(source);

    // Second put should overwrite first due to case normalization
    assertEquals(1, map.size());
    assertEquals("upper", map.get("file.txt"));
  }

//---------------------------------------------------------------------------
//endregion
//region Compute Edge Cases
//---------------------------------------------------------------------------

  @Test
  void computeIfPresent_missingKey_returnsNull()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    String result = map.computeIfPresent("missing.txt", (k, v) -> v + "-updated");

    assertNull(result);
    assertEquals(0, map.size());
  }

  @Test
  void compute_returnsNull_removesEntry()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    String result = map.compute("FILE.TXT", (k, v) -> null);

    assertNull(result);
    assertEquals(0, map.size());
  }

  @Test
  void compute_missingKey_createsEntry()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    String result = map.compute("file.txt", (k, v) -> (v == null) ? "new" : v);

    assertEquals("new", result);
    assertEquals(1, map.size());
  }

  @Test
  void merge_newKey_usesProvidedValue()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    String result = map.merge("file.txt", "initial", (old, v) -> old + '+' + v);

    assertEquals("initial", result);
    assertEquals("initial", map.get("file.txt"));
  }

  @Test
  void merge_existingKey_usesMergeFunction()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "existing");

    String result = map.merge("FILE.TXT", "new", (old, v) -> old + '+' + v);

    assertEquals("existing+new", result);
    assertEquals(1, map.size());
  }

  @Test
  void merge_functionReturnsNull_removesEntry()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);
    map.put("file.txt", "value");

    String result = map.merge("FILE.TXT", "ignored", (old, v) -> null);

    assertNull(result);
    assertEquals(0, map.size());
  }

//---------------------------------------------------------------------------
//endregion
//region Invalid Filename Handling
//---------------------------------------------------------------------------

  @Test
  void put_filenameWithSlash_throwsIllegalArgumentException()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(IllegalArgumentException.class, () -> map.put("path/file.txt", "value"));
  }

  @Test
  void put_dotFilename_throwsIllegalArgumentException()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(IllegalArgumentException.class, () -> map.put(".", "value"));
  }

  @Test
  void put_doubleDotFilename_throwsIllegalArgumentException()
  {
    FilenameMap<String> map = new FilenameMap<>(WINDOWS_HEURISTIC);

    assertThrows(IllegalArgumentException.class, () -> map.put("..", "value"));
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
