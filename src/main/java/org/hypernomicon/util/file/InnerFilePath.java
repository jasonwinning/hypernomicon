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

import static org.hypernomicon.util.StringUtil.*;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;

//---------------------------------------------------------------------------

class InnerFilePath
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String pathStr = null;
  private File file = null;
  private Path path = null, realPath = null;
  private boolean realPathResolved = false;

//---------------------------------------------------------------------------

  InnerFilePath(String pathStr) { this.pathStr = pathStr; }
  InnerFilePath(Path path)      { this.path = path; }
  InnerFilePath(File file)      { this.file = file; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString() { return getPathStr(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    Path real = getRealPath();

    if (real != null)
      return real.hashCode();

    // Fallback to normalized path if real path is not available

    return getPath().normalize().hashCode();
  }

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

    if      (path    != null) file = path.toFile();
    else if (pathStr != null) file = new File(pathStr);

    return file;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Path getRealPath()
  {
    if (realPathResolved) return realPath;

    try
    {
      realPath = getPath().toRealPath();
    }
    catch (IOException e)
    {
      realPath = null;
    }

    realPathResolved = true;
    return realPath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object other)
  {
    if (this == other) return true;
    if ((other instanceof InnerFilePath) == false) return false;

    InnerFilePath otherFilePath = (InnerFilePath) other;

    String thisStr = safeStr(toString()),
           otherStr = safeStr(otherFilePath.toString());

    if (thisStr.equals(otherStr)) return true;

    // Use cached real paths for comparison

    Path thisReal = getRealPath(),
         otherReal = otherFilePath.getRealPath();

    if ((thisReal != null) && (otherReal != null))
      return thisReal.equals(otherReal);

    // If one exists and the other doesn't, they can't be the same

    if ((thisReal != null) || (otherReal != null))
      return false;

    // Neither file exists; compare by normalized path structure

    return getPath().normalize().equals(otherFilePath.getPath().normalize());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
