/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.util.filePath;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.net.MalformedURLException;
import java.net.URI;
import java.nio.channels.FileChannel;
import java.nio.file.DirectoryStream;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.view.fileManager.FileManager;

public class FilePath implements Comparable<FilePath>
{
  private final InnerFilePath innerVal;

  public FilePath(File file)      { innerVal = new InnerFilePath(file); }
  public FilePath(Path path)      { innerVal = new InnerFilePath(path); }
  public FilePath(String pathStr) { innerVal = new InnerFilePath(pathStr); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public File toFile()                  { return innerVal.getFile(); }
  public Path toPath()                  { return innerVal.getPath(); }
  public URI toURI()                    { return nullSwitch(innerVal.getFile(), null, File::toURI); }
  public boolean exists()               { return innerVal.getFile().exists(); }
  public long size() throws IOException { return Files.size(innerVal.getPath()); }
  public boolean isFile()               { return innerVal.getFile().isFile();  }
  public boolean isDirectory()          { return innerVal.getFile().isDirectory(); }
  public FilePath getParent()           { return new FilePath(innerVal.getPath().getParent()); }
  public Instant lastModified()         { return Instant.ofEpochMilli(innerVal.getFile().lastModified()); }
  public String getExtensionOnly()      { return FilenameUtils.getExtension(innerVal.getPathStr()); }

  public boolean copyTo(FilePath destFilePath, boolean confirmOverwrite) throws IOException { return moveOrCopy(destFilePath, confirmOverwrite, false); }
  public boolean moveTo(FilePath destFilePath, boolean confirmOverwrite) throws IOException { return moveOrCopy(destFilePath, confirmOverwrite, true); }

  public boolean renameTo(String newNameStr) throws IOException { return moveOrCopy(getDirOnly().resolve(newNameStr), false, true); }

  public static boolean isEmpty(FilePath filePath) { return (filePath == null) || (safeStr(filePath.toString()).length() == 0); }

  @Override public int hashCode()            { return innerVal.hashCode(); }
  @Override public String toString()         { return innerVal.getPathStr(); }
  @Override public int compareTo(FilePath o) { return toPath().compareTo(o.toPath()); }

  // If this file is a directory, will return just the directory name. If it is not a directory, will return just the file name.
  public FilePath getNameOnly() { return new FilePath(FilenameUtils.getName(innerVal.getPathStr())); }

  // If this file is a directory, will return the entire path. If it is not a directory, will return the parent directory
  public FilePath getDirOnly() { return isDirectory() ? this : new FilePath(FilenameUtils.getFullPathNoEndSeparator(innerVal.getPathStr())); }

  // this = base, parameter = relative, output = resolved
  public FilePath resolve(FilePath relativeFilePath) { return new FilePath(innerVal.getPath().resolve(relativeFilePath.toPath())); }

  // this = base, parameter = relative, output = resolved
  public FilePath resolve(String relativeStr) { return new FilePath(innerVal.getPath().resolve(Paths.get(relativeStr))); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // this = base, parameter = resolved, output = relative
  public FilePath relativize(FilePath resolvedFilePath)
  {
    try { return new FilePath(innerVal.getPath().relativize(resolvedFilePath.toPath())); }
    catch (IllegalArgumentException e) { return null; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void delete(boolean noExistOK) throws IOException
  {
    if (noExistOK && (exists() == false)) return;

    boolean startWatcher = folderTreeWatcher.stop();

    Files.delete(toPath());

    fileManagerDlg.setNeedRefresh();

    if (startWatcher)
      folderTreeWatcher.createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean deleteReturnsBoolean(boolean noExistOK) { return deleteReturnsBoolean(noExistOK, null); }

  public boolean deleteReturnsBoolean(boolean noExistOK, StringBuilder errorSB)
  {
    try { delete(noExistOK); }
    catch (Exception e)
    {
      if (errorSB != null) assignSB(errorSB, e.getMessage());
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean deletePromptOnFail(boolean noExistOK)
  {
    StringBuilder errorSB = new StringBuilder("");
    String msgStr;

    while (deleteReturnsBoolean(noExistOK, errorSB) == false)
    {
      if (errorSB.length() > 0)
        msgStr = "Attempt to delete file failed: \"" + errorSB.toString() + System.lineSeparator() + System.lineSeparator() + "Try again?";
      else
        msgStr = "Attempt to delete file failed: \"" + toString() + "\". Try again?";

      if (confirmDialog(msgStr) == false)
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object other)
  {
    FilePath otherFilePath;

    if (other == null) return false;

    if      (other instanceof FilePath) otherFilePath = FilePath.class.cast(other);
    else if (other instanceof String)   otherFilePath = new FilePath(String.class.cast(other));
    else if (other instanceof File)     otherFilePath = new FilePath(File.class.cast(other));
    else if (other instanceof Path)     otherFilePath = new FilePath(Path.class.cast(other));
    else return false;

    return innerVal.equals(otherFilePath.innerVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean moveOrCopy(FilePath destFilePath, boolean confirmOverwrite, boolean move) throws IOException
  {
    if (equals(destFilePath))
      return falseWithErrorMessage("Source file is the same as the destination file.");

    boolean startWatcher = folderTreeWatcher.stop();

    try
    {
      if (destFilePath.exists() && confirmOverwrite)
      {
        if (confirmDialog("Destination file exists. Overwrite?") == false)
          return false;

        if (destFilePath.toFile().delete() == false)
          return falseWithErrorMessage("Unable to delete the file.");
      }

      if (move)
        Files.move(toPath(), destFilePath.toPath(), StandardCopyOption.REPLACE_EXISTING);
      else
        Files.copy(toPath(), destFilePath.toPath(), StandardCopyOption.REPLACE_EXISTING);
    }
    finally
    {
      if (startWatcher)
        folderTreeWatcher.createNewWatcherAndStart();

      nullSwitch(fileManagerDlg, FileManager::setNeedRefresh);
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isFilenameValid(String fileName)
  {
    fileName = safeStr(fileName);
    if (ultraTrim(fileName).length() == 0) return false;

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
    catch (Exception e)
    {
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String removeInvalidFileNameChars(String fileTitle)
  {
    return convertToEnglishChars(fileTitle).replace("?", "").replace(":", "").replace("*" , "").replace("<" , "").replace(">", "")
                                           .replace("|", "").replace("/", "").replace("\\", "").replace("\"", "").replace("'", "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void deleteDirectory(boolean singleCall) throws IOException
  {
    FilePath filePath = getDirOnly();

    fileManagerDlg.setNeedRefresh();

    if (singleCall && SystemUtils.IS_OS_WINDOWS)
    {
      ProcessBuilder pb = new ProcessBuilder("cmd", "/c", "RD /S /Q \"" + filePath + "\"");
      pb.redirectErrorStream(true);

      Process process = pb.start();
      try (BufferedReader inStreamReader = new BufferedReader(new InputStreamReader(process.getInputStream())))
      {
        String errStr = "", line = inStreamReader.readLine();
        while (line != null)
        {
          errStr = errStr + line;
          line = inStreamReader.readLine();
        }

        if (errStr.length() > 0)
        {
          if ((errStr.toLowerCase().contains("denied") || (errStr.toLowerCase().contains("access"))))
            errStr = errStr + System.lineSeparator() + System.lineSeparator() + "It may work to restart Hypernomicon and try again.";
          throw new IOException(errStr);
        }
      }
    }
    else
    {
      FileUtils.deleteDirectory(filePath.toFile());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void renameDirectory(FilePath destFilePath) throws IOException
  {
    FilePath srcFilePath = getDirOnly();

    fileManagerDlg.setNeedRefresh();

    if (SystemUtils.IS_OS_WINDOWS)
    {
      ProcessBuilder pb = new ProcessBuilder("cmd", "/c", "ren \"" + srcFilePath + "\" \"" + destFilePath.getNameOnly() + "\"");
      pb.redirectErrorStream(true);

      Process process = pb.start();
      try (BufferedReader inStreamReader = new BufferedReader(new InputStreamReader(process.getInputStream())))
      {
        String errStr = "", line = inStreamReader.readLine();
        while (line != null)
        {
          errStr = errStr + line;
          line = inStreamReader.readLine();
        }

        if (errStr.length() > 0)
        {
          if (errStr.toLowerCase().contains("denied"))
            errStr = errStr + System.lineSeparator() + System.lineSeparator() + "It may work to restart Hypernomicon and try again.";
          throw new IOException(errStr);
        }
      }
    }
    else
    {
      FileUtils.moveDirectory(srcFilePath.toFile(), destFilePath.toFile());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean addDirContentsToSet(FilePathSet set)
  {
    try
    {
      Files.walkFileTree(getDirOnly().toPath(), new SimpleFileVisitor<Path>()
      {
        @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws FileNotFoundException, IOException
        {
          set.add(new FilePath(file));
          return FileVisitResult.CONTINUE;
        }

        @Override public FileVisitResult visitFileFailed(Path file, IOException e) throws IOException
        {
          return FileVisitResult.SKIP_SUBTREE;
        }

        @Override public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws FileNotFoundException, IOException
        {
          set.add(new FilePath(dir));
          return FileVisitResult.CONTINUE;
        }
      });
    }
    catch (IOException e)
    {
      messageDialogSameThread(e.getMessage(), mtError);
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean canObtainLock() throws IOException
  {
    if (exists() == false) return true;

    if (isDirectory())
    {
      try
      {
        FileUtils.touch(toFile());
      }
      catch (IOException e)
      {
        return false;
      }

      return true;
    }

    try (RandomAccessFile raFile = new RandomAccessFile(toFile(), "rw"))
    {
      try (FileChannel channel = raFile.getChannel())
      {
        if (channel.tryLock() == null)
          return false;

        channel.close();
      }
    }
    catch (FileNotFoundException e)
    {
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean anyOpenFilesInDir()
  {
    try
    {
      Files.walkFileTree(getDirOnly().toPath(), new SimpleFileVisitor<Path>()
      {
        @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws FileNotFoundException, IOException
        {
          if (new FilePath(file).canObtainLock() == false)
            throw new IOException("Unable to obtain lock for file: " + file.toString());

          return FileVisitResult.CONTINUE;
        }

        @Override public FileVisitResult visitFileFailed(Path file, IOException e) throws IOException
        {
          return FileVisitResult.SKIP_SUBTREE;
        }

        @Override public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws FileNotFoundException, IOException
        {
          FileUtils.touch(dir.toFile());

          return FileVisitResult.CONTINUE;
        }
      });
    }
    catch (IOException e)
    {
      messageDialogSameThread(e.getMessage(), mtError);
      return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isSubpath(FilePath subFilePath)
  {
    if (equals(subFilePath)) return true;

    FilePath parent = subFilePath.getParent();
    if (FilePath.isEmpty(parent)) return false;

    return isSubpath(parent);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String toURLString()
  {
    String pathStr = "";

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

  public boolean dirContainsAnyFiles(boolean checkSubdirs) throws IOException
  {
    if (checkSubdirs == false)
    {
      try (DirectoryStream<Path> stream = Files.newDirectoryStream(getDirOnly().toPath(), "**"))
      {
        if (findFirst(stream, entry -> new FilePath(entry).isDirectory()) != null)
          return true;
      }

      return false;
    }

    final MutableBoolean hasFiles = new MutableBoolean(false);

    Files.walkFileTree(getDirOnly().toPath(), new SimpleFileVisitor<Path>()
    {
      @Override public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException
      {
        if (new FilePath(file).isDirectory() == false)
          hasFiles.setTrue();

        return FileVisitResult.CONTINUE;
      }

      @Override public FileVisitResult visitFileFailed(Path file, IOException e) throws IOException
      {
        return FileVisitResult.SKIP_SUBTREE;
      }

      @Override public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) throws IOException
      {
        return FileVisitResult.CONTINUE;
      }
    });

    return hasFiles.booleanValue();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
