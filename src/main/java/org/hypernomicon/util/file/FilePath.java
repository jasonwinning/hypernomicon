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

import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.util.file.deletion.FileDeletion;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult.*;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.charset.Charset;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.DosFileAttributeView;
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.commons.io.*;
import org.apache.commons.lang3.mutable.MutableBoolean;

//---------------------------------------------------------------------------

public class FilePath implements Comparable<FilePath>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final InnerFilePath innerVal;

//---------------------------------------------------------------------------

  public FilePath(File file)      { innerVal = new InnerFilePath(file); }
  public FilePath(Path path)      { innerVal = new InnerFilePath(path); }
  public FilePath(String pathStr) { innerVal = new InnerFilePath(pathStr); }

//---------------------------------------------------------------------------

  public File toFile()                  { return innerVal.getFile(); }
  public Path toPath()                  { return innerVal.getPath(); }
  public URI toURI()                    { return nullSwitch(toFile(), null, File::toURI); }
  public boolean exists()               { return toFile().exists(); }
  public long size() throws IOException { return Files.size(toPath()); }
  public boolean isFile()               { return toFile().isFile();  }
  public boolean isDirectory()          { return toFile().isDirectory(); }
  public FilePath getParent()           { return nullSwitch(nullSwitch(toPath(), null, Path::getParent), null, FilePath::new); }
  public Instant lastModified()         { return Instant.ofEpochMilli(toFile().lastModified()); }

  /**
   * @see File#deleteOnExit()
   */
  public void deleteOnExit()            { toFile().deleteOnExit(); }

  /**
   * Gets the extension of a filename.
   * <p>
   * This method returns the textual part of the filename after the last dot.
   * There must be no directory separator after the dot.
   * <pre>
   * foo.txt      --&gt; "txt"
   * a/b/c.jpg    --&gt; "jpg"
   * a/b.txt/c    --&gt; ""
   * a/b/c        --&gt; ""
   * </pre>
   * <p>
   * The output will be the same irrespective of the machine that the code is running on.
   *
   * @return the extension of the file or an empty string if none exists or {@code null}
   * if the filename is {@code null}.
   */
  public String getExtensionOnly()      { return FilenameUtils.getExtension(toString()); }

  public boolean copyTo(FilePath destFilePath, boolean confirmOverwrite) throws IOException { return moveOrCopy(destFilePath, confirmOverwrite, false); }
  public boolean moveTo(FilePath destFilePath, boolean confirmOverwrite) throws IOException { return moveOrCopy(destFilePath, confirmOverwrite, true); }

  public boolean renameTo(String newNameStr) throws IOException { return moveOrCopy(getDirOnly().resolve(newNameStr), false, true); }

  public static boolean isEmpty(FilePath filePath) { return (filePath == null) || strNullOrBlank(filePath.toString()); }

  @Override public int hashCode()               { return innerVal.hashCode(); }
  @Override public String toString()            { return innerVal.getPathStr(); }
  @Override public int compareTo(FilePath o)    { return toPath().compareTo(o.toPath()); }
  @Override public boolean equals(Object other) { return (other instanceof FilePath oFilePath) && innerVal.equals(oFilePath.innerVal); }

  /**
   * If this file is a directory, will return just the directory name. If it is not a directory, will return just the file name.
   */
  public FilePath getNameOnly() { return new FilePath(FilenameUtils.getName(toString())); }

  /**
   * If this file is a directory, will return the entire path. If it is not a directory, will return the parent directory.
   */
  public FilePath getDirOnly() { return isDirectory() ? this : new FilePath(FilenameUtils.getFullPathNoEndSeparator(toString())); }

  /**
   * this = base, parameter = relative, output = resolved
   */
  public FilePath resolve(FilePath relativeFilePath) { return new FilePath(toPath().resolve(relativeFilePath.toPath())); }

  /**
   * this = base, parameter = relative, output = resolved
   */
  public FilePath resolve(String relativeStr) { return new FilePath(toPath().resolve(Paths.get(relativeStr.strip()))); }

  /**
   * this = base, parameters = relative parts, output = resolved
   */
  public FilePath resolve(String... relativeStrs)
  {
    Path path = toPath();

    for (String part : relativeStrs)
      path = path.resolve(Paths.get(part.strip()));

    return new FilePath(path);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * this = base, parameter = resolved, output = relative
   */
  public FilePath relativize(FilePath resolvedFilePath)
  {
    try { return new FilePath(toPath().relativize(resolvedFilePath.toPath())); }
    catch (IllegalArgumentException e) { return null; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean moveOrCopy(FilePath destFilePath, boolean confirmOverwrite, boolean move) throws IOException
  {
    if (equals(destFilePath))
      throw new IOException("Source file is the same as the destination file.");

    boolean startWatcher = folderTreeWatcher.stop();

    try
    {
      if (destFilePath.exists() && confirmOverwrite)
      {
        if (ui == null)
          return false;

        if (confirmDialog("Destination file exists. Overwrite?", false) == false)
          return false;

        if (FileDeletion.ofFile(destFilePath).interactive().execute() != SUCCESS)
          return false;
      }

      if (move)
      {
        boolean fromUnentered = (ui != null) && (db != null) && (db.isOnline()) && isFile() && getParent().equals(db.unenteredPath());

        Files.move(toPath(), destFilePath.toPath(), StandardCopyOption.REPLACE_EXISTING);

        if (fromUnentered)
          ui.notifyOfImport(this);
      }
      else
        Files.copy(toPath(), destFilePath.toPath(), StandardCopyOption.REPLACE_EXISTING);
    }
    finally
    {
      if (startWatcher)
        folderTreeWatcher.createNewWatcherAndStart();

      FileManager.setNeedRefresh();
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern invalidCharsPattern = Pattern.compile("[\\\\/:*?\"<>|']");

  public static String removeInvalidFileNameChars(CharSequence fileTitle)
  {
    return convertToEnglishChars(fileTitle).replaceAll(invalidCharsPattern.pattern(), "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isFilenameValid(String fileName)
  {
    if (strNullOrBlank(fileName)) return false;

    if (fileName.length() > 255)
      return false;

    if (invalidCharsPattern.matcher(fileName).find())
      return false;

    try
    {
      Path path = Paths.get(fileName);

      if (fileName.equals(path.normalize().toString()) == false)
        return false;

      Files.getLastModifiedTime(path);
    }
    catch (NoSuchFileException e)
    {
      return true;
    }
    catch (IOException | InvalidPathException e)
    {
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates a directory by creating all nonexistent parent directories first.
   * Does not throw an exception if directory already exists.
   */
  public void createDirectories() throws IOException
  {
    Files.createDirectories(toPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates a new directory.
   * @throws IOException if directory already exists or some other file system error occurs
   */
  public void createDirectory() throws IOException
  {
    Files.createDirectory(toPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void renameDirectory(FilePath destFilePath) throws IOException
  {
    FileManager.setNeedRefresh();
    Files.move(getDirOnly().toPath(), destFilePath.toPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addDirContentsToSet(FilePathSet set) throws HyperDataException
  {
    try
    {
      Files.walkFileTree(getDirOnly().toPath(), new SimpleFileVisitor<>()
      {
        @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
        {
          set.add(new FilePath(file));
          return FileVisitResult.CONTINUE;
        }

        @Override public FileVisitResult visitFileFailed(Path file, IOException e)
        {
          return FileVisitResult.SKIP_SUBTREE;
        }

        @Override public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
        {
          set.add(new FilePath(dir));
          return FileVisitResult.CONTINUE;
        }
      });
    }
    catch (IOException e)
    {
      throw new HyperDataException(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * On Windows, clears the DOS read-only attribute on this path before deletion or replacement.
   * OneDrive sets {@code FILE_ATTRIBUTE_READONLY} on synced files and directories as a sync-state
   * signal, which causes {@code RemoveDirectoryW}, {@code DeleteFileW}, and {@code MoveFileExW}
   * (with {@code MOVEFILE_REPLACE_EXISTING}) to fail with {@code ERROR_ACCESS_DENIED}. File Explorer
   * clears or ignores this attribute; this method makes programmatic file operations behave
   * consistently with Explorer.
   * <p>
   * On non-Windows systems this is a no-op. Errors are silently swallowed; if clearing fails,
   * the subsequent operation will produce its own diagnostic error.
   */
  public void clearReadOnlyOnWindows()
  {
    if (IS_OS_WINDOWS == false) return;

    try
    {
      DosFileAttributeView view = Files.getFileAttributeView(toPath(), DosFileAttributeView.class, LinkOption.NOFOLLOW_LINKS);
      if ((view != null) && view.readAttributes().isReadOnly())
        view.setReadOnly(false);
    }
    catch (IOException e)
    {
      // Best-effort: if clearing the attribute fails, the subsequent operation will produce its own error
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Check whether this path can likely be moved or deleted without conflict.
   * <p>
   * On Windows, the behavior depends on whether this path is a file or directory:
   * <ul>
   *   <li><b>Regular files:</b> Attempts to open in read-write mode and acquire an
   *       exclusive lock via {@link FileChannel#tryLock()}. This catches the common
   *       case where an application holds the file open without
   *       {@code FILE_SHARE_DELETE}. Read-only files return {@code true} because the
   *       read-only attribute does not prevent moves or deletes on Windows.
   *       Sharing violations (file opened by another process) are distinguished
   *       from read-only files via {@link Files#isWritable(Path)}.</li>
   *   <li><b>Directories:</b> Performs a probe rename (rename to a temporary sibling
   *       name, then rename back). This detects directory-level locks such as a
   *       terminal window whose working directory is the folder. It does not detect
   *       locked files inside the directory; use {@link #findLockedFileInDir()} or
   *       {@link #anyOpenFilesInDir()} for that.</li>
   * </ul>
   * <p>
   * On POSIX systems (macOS, Linux), always returns {@code true} because open
   * file handles reference inodes rather than paths, so files and directories can
   * be moved or deleted while open.
   * <p>
   * This check is subject to TOCTOU races. It is a best-effort pre-check to
   * catch the common case, not a guarantee.
   * <p>
   * If the JVM crashes between the two renames of a directory probe, the
   * directory will be left with a {@link org.hypernomicon.Const#LOCK_PROBE_SUFFIX}
   * suffix. These leftovers are cleaned up at database load time by
   * {@code HyperDB.checkWhetherFoldersExist()}.
   *
   * @return true if the path appears unlocked
   * @throws IOException if an unexpected I/O error occurs (including failure to
   *         rename a directory back to its original name after a successful probe)
   */
  public boolean canObtainLock() throws IOException
  {
    if (IS_OS_WINDOWS == false) return true;

    if (exists() == false) return true;

    if (isDirectory())
    {
      // Probe-rename: rename to a temp sibling, then rename back.
      // If the rename fails, the directory is locked (e.g., terminal working directory).

      Path src = toPath(),
           tmp = src.resolveSibling(src.getFileName().toString() + LOCK_PROBE_SUFFIX + System.nanoTime());

      try
      {
        Files.move(src, tmp);
      }
      catch (IOException e)
      {
        return false;
      }

      try
      {
        Files.move(tmp, src);
      }
      catch (IOException e)
      {
        throw new IOException("Lock probe renamed \"" + src.getFileName()
          + "\" to \"" + tmp.getFileName() + "\" but could not rename back", e);
      }

      return true;
    }

    try (RandomAccessFile raFile = new RandomAccessFile(toFile(), "rw");
         FileChannel channel = raFile.getChannel(); FileLock lock = channel.tryLock())
    {
      if (lock == null)
        return false;
    }
    catch (FileNotFoundException e)
    {
      // RandomAccessFile("rw") throws FileNotFoundException for both read-only
      // files and sharing violations (another process has the file open).
      // Read-only files can still be moved/deleted on Windows; sharing
      // violations cannot. Files.isWritable checks the attribute, not the
      // sharing state, so it distinguishes the two.

      return (exists() == false) || (Files.isWritable(toPath()) == false);
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Walks the directory tree and returns the first locked file found, or the
   * first locked directory if no locked file is found, or {@code null} if
   * everything is unlocked. On POSIX systems, always returns {@code null}.
   * <p>
   * Prefers reporting locked files over locked directories, since a locked
   * interior file also causes ancestor directories to fail the probe-rename.
   */
  public FilePath findLockedFileInDir()
  {
    if (IS_OS_WINDOWS == false)
      return null;

    // Phase 1: collect all paths (files and directories) in the tree

    List<FilePath> paths = new ArrayList<>();

    try
    {
      Files.walkFileTree(getDirOnly().toPath(), new SimpleFileVisitor<>()
      {
        @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
        {
          paths.add(new FilePath(file));
          return FileVisitResult.CONTINUE;
        }

        @Override public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
        {
          paths.add(new FilePath(dir));
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
      return null;
    }

    // Phase 2: check each path with canObtainLock()

    FilePath firstLockedDir = null;

    for (FilePath filePath : paths)
    {
      try
      {
        if (filePath.canObtainLock() == false)
        {
          if (filePath.isFile())
            return filePath;

          if (firstLockedDir == null)
            firstLockedDir = filePath;
        }
      }
      catch (IOException e)
      {
        return null;
      }
    }

    return firstLockedDir;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean anyOpenFilesInDir()
  {
    FilePath lockedPath = findLockedFileInDir();

    if (lockedPath == null)
      return false;

    errorPopup("Unable to obtain lock for path: " + lockedPath);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Check if the given path is contained within this path's directory tree.
   * <p>
   * Returns true if {@code descendant} is a subpath (descendant) of this path.
   * For example, if this path is "/a/b" and descendant is "/a/b/c/d", returns true.
   * This is the inverse of {@link java.nio.file.Path#startsWith(Path)}.
   *
   * @param descendant the path to check
   * @return true if descendant is under this path (i.e., this path contains descendant)
   */
  public boolean contains(FilePath descendant)
  {
    // Use cached real paths when available

    Path thisReal = innerVal.getRealPath(),
         descendantReal = descendant.innerVal.getRealPath();

    if ((thisReal != null) && (descendantReal != null))
      return descendantReal.startsWith(thisReal);

    // Fallback for non-existent paths: use normalized absolute paths
    // Path.startsWith() is case-insensitive on Windows automatically

    Path thisNorm = toPath().toAbsolutePath().normalize(),
         descendantNorm = descendant.toPath().toAbsolutePath().normalize();

    return descendantNorm.startsWith(thisNorm);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String toURLString()
  {
    String pathStr;

    try
    {
      pathStr = toURI().toURL().toExternalForm();
    }
    catch (MalformedURLException e)
    {
      return "";
    }

    if (pathStr.startsWith("file:/") && !pathStr.startsWith("file://"))
      pathStr = "file://" + pathStr.substring(5);

    return pathStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean dirContainsAnyFiles() throws IOException
  {
    final MutableBoolean hasFiles = new MutableBoolean(false);

    Files.walkFileTree(getDirOnly().toPath(), new SimpleFileVisitor<>()
    {
      @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs)
      {
        if (new FilePath(file).isDirectory() == false)
          hasFiles.setTrue();

        return FileVisitResult.CONTINUE;
      }

      @Override public FileVisitResult visitFileFailed(Path file, IOException e)
      {
        return FileVisitResult.SKIP_SUBTREE;
      }

      @Override public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs)
      {
        return FileVisitResult.CONTINUE;
      }
    });

    return hasFiles.booleanValue();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final long MAX_SIZE_TO_READ_INTO_STRING_LIST = 500000;

  public List<String> readToStrList() throws IOException
  {
    if (size() > MAX_SIZE_TO_READ_INTO_STRING_LIST)
      throw new IOException("File is too large");

    return FileUtils.readLines(toFile(), Charset.defaultCharset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
