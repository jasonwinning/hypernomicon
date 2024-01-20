/*
 * Copyright 2015-2024 Jason Winning
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

import static org.hypernomicon.util.Util.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

class InnerFilePath
{
  private String pathStr = null;
  private File file = null;
  private Path path = null;

//---------------------------------------------------------------------------

  InnerFilePath(String pathStr) { this.pathStr = pathStr; }
  InnerFilePath(Path path)      { this.path = path; }
  InnerFilePath(File file)      { this.file = file; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString() { return getPathStr(); }
  @Override public int hashCode()    { return getPath().hashCode(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getPathStr()
  {
    if (pathStr != null) return pathStr;

    if      (path != null) pathStr = path.toString();
    else if (file != null) pathStr = file.getAbsolutePath();

    return pathStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Path getPath()
  {
    if (path != null) return path;

    if      (file    != null) path = file.toPath();
    else if (pathStr != null) path = Paths.get(pathStr);

    return path;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  File getFile()
  {
    if (file != null) return file;

    if      (path != null)    file = path.toFile();
    else if (pathStr != null) file = new File(pathStr);

    return file;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object other)
  {
    if ((other instanceof InnerFilePath) == false) return false;

    InnerFilePath otherFilePath = (InnerFilePath)other;

    try { return Files.isSameFile(getPath(), otherFilePath.getPath()); }
    catch (IOException e) { noOp(); }

    try { return getFile().getCanonicalFile().equals(otherFilePath.getFile().getCanonicalFile()); }
    catch (IOException e) { noOp(); }

    return getPath().normalize().equals(otherFilePath.getPath().normalize());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
