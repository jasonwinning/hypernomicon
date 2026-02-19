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

package org.hypernomicon.util.file.deletion;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionType.*;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Stream;

import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionType;

//---------------------------------------------------------------------------

/**
 * Represents a single deletion operation. Knows how to delete one thing once.
 * Used by both Builder and BatchBuilder for actual filesystem operations.
 */
class DeletionTask
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final FilePath filePath;
  private final DeletionType type;
  private IOException lastError;

//---------------------------------------------------------------------------

  DeletionTask(FilePath filePath, DeletionType type)
  {
    this.filePath = filePath;
    this.type = type;
  }

//---------------------------------------------------------------------------

  FilePath getFilePath()     { return filePath; }
  IOException getLastError() { return lastError; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Attempt deletion once. Returns true if successful, false if failed.
   * Does not prompt. Directory tree walks may perform multiple internal passes
   * to handle cloud-storage sync interference, but this counts as one attempt
   * from the retry engine's perspective.
   */
  boolean attemptOnce()
  {
    lastError = null;

    try
    {
      switch (effectiveType())
      {
        case FILE_ONLY                 : Files.deleteIfExists   (filePath.toPath()       ); break;

        case DIR_WITH_CONTENTS         : deleteDirectoryTreeWalk(filePath.toPath(), true ); break;
        case DIR_CONTENTS_ONLY         : deleteDirectoryTreeWalk(filePath.toPath(), false); break;

        case FILE_OR_DIR_WITH_CONTENTS : throw newAssertionError(53716);   // Should not reach here; effectiveType() resolves this
      }

      return true;
    }
    catch (IOException e)
    {
      // Check if the target is actually gone despite the error

      if (filePath.exists() == false)
        return true;

      lastError = e;
      return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Attempt deletion once, suppressing errors. Uses deleteIfExists for files.
   * Returns true if the target no longer exists.
   */
  boolean attemptOnceSilent()
  {
    try
    {
      return switch (effectiveType())
      {
        case FILE_ONLY                 -> { Files.deleteIfExists(filePath.toPath()); yield Files.exists(filePath.toPath()) == false; }

        case DIR_WITH_CONTENTS         -> deleteDirectorySilentOnce(filePath.toPath(), true);
        case DIR_CONTENTS_ONLY         -> deleteDirectorySilentOnce(filePath.toPath(), false);

        case FILE_OR_DIR_WITH_CONTENTS -> throw newAssertionError(53717);   // Should not reach here; effectiveType() resolves this
      };
    }
    catch (Exception e)
    {
      // Silently ignore
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Resolves FILE_OR_DIR_WITH_CONTENTS to the actual type based on the filePath.
   */
  private DeletionType effectiveType()
  {
    return type == FILE_OR_DIR_WITH_CONTENTS ?
      (filePath.isDirectory() ? DIR_WITH_CONTENTS : FILE_ONLY)
    :
      type;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Directory deletion using Java tree walk. Walks the tree, closes the stream,
   * then deletes bottom-up. Throws IOException if the goal is not achieved.
   * <p>
   * Each attempt makes maximum progress by continuing past individual file failures,
   * which works well with the retry engine when Windows file handles linger.
   * <p>
   * Up to three walk passes are performed. Extra passes handle cloud-storage filesystems
   * (e.g. Dropbox on macOS) where a sync daemon may create metadata files between the
   * initial enumeration and the actual deletion, leaving directories non-empty even
   * after the first pass deletes everything it found.
   */
  private static void deleteDirectoryTreeWalk(Path root, boolean deleteRoot) throws IOException
  {
    IOException lastError = null;

    for (int pass = 0; pass < 3; pass++)
    {
      List<Path> paths;

      try (Stream<Path> walk = Files.walk(root))
      {
        paths = walk.sorted(Comparator.reverseOrder()).toList();
      }

      // Delete after closing the stream to avoid Windows file handle issues

      for (Path path : paths)
      {
        if ((deleteRoot == false) && path.equals(root))
          continue;

        try
        {
          Files.deleteIfExists(path);
        }
        catch (IOException e)
        {
          lastError = e;
        }
      }

      // Check if goal was achieved despite errors

      if (deleteRoot)
      {
        if (Files.exists(root) == false)
          return;
      }
      else
      {
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(root))
        {
          if (stream.iterator().hasNext() == false)
            return;
        }
      }
    }

    // Goal not achieved after all passes

    if (deleteRoot)
    {
      if (lastError != null)
        throw lastError;

      throw new IOException("Failed to delete directory: " + root);
    }
    else
    {
      try (DirectoryStream<Path> stream = Files.newDirectoryStream(root))
      {
        if (stream.iterator().hasNext())
          throw lastError != null ? lastError : new IOException("Failed to clean directory contents: " + root);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Failure-OK directory deletion. Delegates to {@link #deleteDirectoryTreeWalk} and
   * translates exceptions to boolean. Returns true if the directory (or its contents)
   * was successfully deleted or didn't exist.
   */
  private static boolean deleteDirectorySilentOnce(Path root, boolean deleteRoot)
  {
    if (Files.exists(root) == false)
      return true;

    try
    {
      deleteDirectoryTreeWalk(root, deleteRoot);
      return true;
    }
    catch (NoSuchFileException e)
    {
      // Root disappeared; goal achieved

      return true;
    }
    catch (IOException e)
    {
      return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
