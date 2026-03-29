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

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.concurrent.*;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.AbstractHyperDB;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.util.file.deletion.FileDeletion;

//---------------------------------------------------------------------------

/**
 * Canonical registry for {@link FilePath} instances under the database root folder.
 * <p>
 * Provides two services:
 * <ol>
 *   <li><b>Path identity</b>: ensures one canonical {@code FilePath} per real filesystem path,
 *       eliminating identity inconsistencies and redundant {@code toRealPath()} I/O.</li>
 *   <li><b>Record association</b>: maps filesystem paths to the {@link HyperPath} instances that
 *       reference them, providing direct full-path-keyed O(1) lookup.</li>
 * </ol>
 * <p>
 * The registry is dormant (empty, {@link #isActive()} returns {@code false}) until
 * {@link #populate(FilePath)} is called during database load. It is cleared on database close
 * via {@link #clear()}.
 *
 * <h2>Key Normalization</h2>
 * Cache keys are full absolute paths normalized via {@link FilenameRules#normalizePath(java.nio.file.Path)},
 * which handles case folding, Unicode composition, trailing-dot stripping, etc.,
 * matching the actual filesystem's equivalence rules.
 *
 * <h2>Two-Tier Lookup</h2>
 * {@link #getOrCreate(Path)} first tries the normalized input path (fast, no I/O). On a cache
 * miss, it falls back to {@link Path#toRealPath(java.nio.file.LinkOption...) toRealPath()} to handle symlinks, 8.3 short names, and
 * junction points. Filesystem-sourced paths (from {@code walkFileTree}, {@code WatchService})
 * almost always hit on tier 1.
 *
 * <h2>Thread Safety</h2>
 * All maps are {@link ConcurrentHashMap}-backed. Safe for concurrent access from the JavaFX
 * thread, the {@code FolderTreeWatcher} thread, and full-text-indexing threads.
 */
public final class FilePathRegistry implements RegistryAccessor
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class NormalizedAncestor
  {
    private final String ancestorKey, prefix;

    private NormalizedAncestor(FilePath filePath)
    {
      ancestorKey = FilenameRules.current().normalizeFilePath(filePath);
      prefix = ancestorKey + File.separatorChar;
    }

    private boolean contains(String key) { return key.equals(ancestorKey) || key.startsWith(prefix); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final FilePathRegistry INSTANCE = new FilePathRegistry();

  private final ConcurrentHashMap<String, FilePath>       pathCache  = new ConcurrentHashMap<>();
  private final ConcurrentHashMap<String, Set<HyperPath>> hyperPaths = new ConcurrentHashMap<>();

  private volatile NormalizedAncestor normalizedRoot;

  private ScheduledExecutorService cleanupExecutor;

//---------------------------------------------------------------------------

  private FilePathRegistry() { }

  static FilePathRegistry instance() { return INSTANCE; }

  /**
   * Obtain the {@link RegistryAccessor} for this registry. The accessor is the narrow
   * public interface through which authorized components interact with the registry.
   * <p>
   * This method is gated: it can only be called during database session start
   * (when {@link AbstractHyperDB#isStartingDBSession()} returns {@code true}) or from
   * a unit-test thread.
   *
   * @return the singleton registry typed as {@code RegistryAccessor}
   * @throws IllegalStateException if called outside the allowed window
   */
  public static RegistryAccessor getAccessor()
  {
    if ((AbstractHyperDB.isStartingDBSession() || isUnitTestThread()) == false)
      throw new IllegalStateException("RegistryAccessor can only be obtained during DB initialization or in a unit test.");

    return INSTANCE;
  }

  @Override public boolean isActive() { return normalizedRoot != null; }

  int size() { return pathCache.size(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Check whether a normalized key is under the database root.
   */
  private boolean isUnderRoot(String normalizedKey)
  {
    return (normalizedRoot != null) && normalizedRoot.contains(normalizedKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Return the canonical {@link FilePath} for the given absolute path, creating and caching it
   * if necessary. Only paths under the database root are interned; paths outside the root get
   * a fresh (non-cached) instance.
   * <p>
   * Uses a two-tier lookup: first by normalized path (no I/O), then by {@code toRealPath()}
   * on cache miss to handle symlinks, 8.3 short names, and junction points.
   *
   * @param absPath an absolute filesystem path
   * @return the canonical {@code FilePath} for this path
   */
  @Override public FilePath getOrCreate(Path absPath)
  {
    String key = FilenameRules.current().normalizePath(absPath);

    if (isUnderRoot(key) == false)
      return new FilePath(absPath);

    // Tier 1: fast lookup by normalized input path (no I/O)

    FilePath cached = pathCache.get(key);
    if (cached != null)
      return cached;

    // Tier 2: resolve real path to handle symlinks, 8.3 names, junction points

    try
    {
      Path realPath = absPath.toRealPath();
      String realKey = FilenameRules.current().normalizePath(realPath);

      if (realKey.equals(key) == false)
      {
        cached = pathCache.get(realKey);
        if (cached != null)
          return cached;
      }

      return pathCache.computeIfAbsent(realKey, _ -> new FilePath(realPath, realPath));
    }
    catch (IOException e)
    {
      // File does not exist; create entry under normalized input key

      return pathCache.computeIfAbsent(key, _ -> new FilePath(absPath));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Intern a path known to come from a filesystem API (walkFileTree, WatchService). Skips
   * the {@code toRealPath()} fallback since filesystem-sourced paths are already canonical.
   */
  private FilePath intern(Path absPath)
  {
    String key = FilenameRules.current().normalizePath(absPath);
    return pathCache.computeIfAbsent(key, _ -> new FilePath(absPath, absPath));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Remove a single path from the registry (both path cache and HyperPath associations).
   */
  void evict(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    String key = FilenameRules.current().normalizeFilePath(filePath);
    pathCache .remove(key);
    hyperPaths.remove(key);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Remove a directory and all paths under it from the registry.
   */
  void evictSubtree(FilePath dir)
  {
    if (FilePath.isEmpty(dir)) return;

    NormalizedAncestor normalizedAncestor = new NormalizedAncestor(dir);

    pathCache .keySet().removeIf(normalizedAncestor::contains);
    hyperPaths.keySet().removeIf(normalizedAncestor::contains);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Re-register all {@link HyperPath} associations under a moved directory. After a physical
   * directory move and folder re-parent, descendant HyperPaths resolve correct paths via
   * {@code filePath()} (dynamic parent chain walking), but their registry keys are stale.
   * <p>
   * This method evicts old {@code pathCache} entries for the old subtree (files no longer at
   * old paths), collects and removes old {@code hyperPaths} entries, then re-registers the
   * collected HyperPaths under their current {@code filePath()}.
   *
   * @param oldDir the directory path BEFORE the move (captured before re-parenting)
   */
  @Override public void onSubtreeMoved(FilePath oldDir)
  {
    if (FilePath.isEmpty(oldDir)) return;

    NormalizedAncestor normalizedAncestor = new NormalizedAncestor(oldDir);

    // Evict stale pathCache entries (physical files no longer at old paths)

    pathCache.keySet().removeIf(normalizedAncestor::contains);

    // Collect and remove old hyperPaths entries

    List<Set<HyperPath>> collectedSets = new ArrayList<>();

    hyperPaths.entrySet().removeIf(entry ->
    {
      if (normalizedAncestor.contains(entry.getKey()))
      {
        collectedSets.add(entry.getValue());
        return true;
      }

      return false;
    });

    // Re-register under current filePath() (which now resolves to new location
    // because the folder record's parent pointer has already been updated)

    for (Set<HyperPath> set : collectedSets)
      for (HyperPath hyperPath : set)
      {
        FilePath filePath = hyperPath.filePath();
        if (FilePath.isEmpty(filePath) == false)
          addHyperPath(filePath, hyperPath);
      }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Check whether the registry contains an entry for this path.
   */
  boolean contains(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return false;

    return pathCache.containsKey(FilenameRules.current().normalizeFilePath(filePath));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // ---- Record associations (HyperPath) ------------------------------------

  /**
   * Return the set of {@link HyperPath} instances associated with the given path.
   * Returns a defensive copy; modifications do not affect the registry.
   *
   * @param filePath the filesystem path to look up
   * @return a set of matching {@code HyperPath} instances, or an empty set if none match
   */
  @Override public Set<HyperPath> getHyperPaths(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return new HashSet<>();

    String key = FilenameRules.current().normalizeFilePath(filePath);
    Set<HyperPath> set = hyperPaths.get(key);

    return (set != null) ? new HashSet<>(set) : new HashSet<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Register a {@link HyperPath} association for the given path.
   */
  @Override public void addHyperPath(FilePath filePath, HyperPath hyperPath)
  {
    if (FilePath.isEmpty(filePath) || (hyperPath == null)) return;

    String key = FilenameRules.current().normalizeFilePath(filePath);
    hyperPaths.computeIfAbsent(key, _ -> ConcurrentHashMap.newKeySet()).add(hyperPath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Remove a {@link HyperPath} association for the given path. Cleans up empty sets.
   */
  @Override public void removeHyperPath(FilePath filePath, HyperPath hyperPath)
  {
    if (FilePath.isEmpty(filePath) || (hyperPath == null)) return;

    String key = FilenameRules.current().normalizeFilePath(filePath);
    Set<HyperPath> set = hyperPaths.get(key);

    if (set == null) return;

    set.remove(hyperPath);

    if (set.isEmpty())
      hyperPaths.remove(key);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // ---- Lifecycle -----------------------------------------------------------

  /**
   * Populate the registry by walking the filesystem under {@code rootPath}, interning all
   * files and directories (excluding temporary files). After the walk, the registry is
   * active ({@link #isActive()} returns {@code true}).
   * <p>
   * HyperPath associations are NOT populated by this method; the caller is responsible for
   * iterating database records and calling {@link #addHyperPath(FilePath, HyperPath)} after
   * this method returns.
   *
   * @param rootPath the database root folder
   */
  @Override public void populate(FilePath rootPath)
  {
    this.normalizedRoot = new NormalizedAncestor(rootPath);

    try
    {
      Files.walkFileTree(rootPath.toPath(), new SimpleFileVisitor<>()
      {
        @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
        {
          String filename = file.getFileName().toString();

          if (FilePath.isTemporaryFile(filename) == false)
            intern(file);

          return FileVisitResult.CONTINUE;
        }

        @Override public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
        {
          if (dir.equals(rootPath.toPath()) == false)
          {
            String dirname = dir.getFileName().toString();

            if (FilePath.isTemporaryFile(dirname))
              return FileVisitResult.SKIP_SUBTREE;
          }

          intern(dir);
          return FileVisitResult.CONTINUE;
        }

        @Override public FileVisitResult visitFileFailed(Path file, IOException e)
        {
          return FileVisitResult.SKIP_SUBTREE;
        }
      });
    }
    catch (IOException e)
    {
      logThrowable(e);
    }

    startCleanupThread();

    FileDeletion.addPostDeletionHook(INSTANCE::evictSubtree);
    nullSwitch(folderTreeWatcher, ftw -> ftw.setEvictionHook(INSTANCE::evictSubtree));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Activate the registry for unit testing without walking the filesystem or starting the
   * cleanup thread. Sets the root and optionally pre-interns the given paths.
   *
   * @param rootPath the database root folder
   * @param paths    paths to intern (typically created under rootPath in a temp directory)
   */
  @Override public void populateForTesting(FilePath rootPath, Path... paths)
  {
    if (isUnitTestThread() == false)
      throw new IllegalStateException("populateForTesting can only be called from a unit test thread.");

    this.normalizedRoot = new NormalizedAncestor(rootPath);

    for (Path path : paths)
      intern(path);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Clear all entries, stop the cleanup thread, and deactivate the registry.
   */
  @Override public void clear()
  {
    if (cleanupExecutor != null)
    {
      cleanupExecutor.shutdownNow();
      cleanupExecutor = null;
    }

    FileDeletion.clearPostDeletionHooks();
    nullSwitch(folderTreeWatcher, ftw -> ftw.setEvictionHook(null));

    pathCache .clear();
    hyperPaths.clear();
    normalizedRoot = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // ---- Cleanup thread ------------------------------------------------------

  private static final long CLEANUP_INTERVAL_MINUTES = 30;

  private void startCleanupThread()
  {
    if (cleanupExecutor != null)
      cleanupExecutor.shutdownNow();

    cleanupExecutor = Executors.newSingleThreadScheduledExecutor(runnable ->
    {
      Thread cleanupThread = new HyperThread("FilePathRegistryCleanup", runnable);
      cleanupThread.setDaemon(true);
      return cleanupThread;
    });

    cleanupExecutor.scheduleAtFixedRate(this::cleanup, CLEANUP_INTERVAL_MINUTES, CLEANUP_INTERVAL_MINUTES, TimeUnit.MINUTES);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cleanup()
  {
    pathCache.entrySet().removeIf(entry ->
    {
      if (entry.getValue().exists())
        return false;

      hyperPaths.remove(entry.getKey());
      return true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
