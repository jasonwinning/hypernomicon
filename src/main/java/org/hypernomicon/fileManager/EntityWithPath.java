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

package org.hypernomicon.fileManager;

import java.io.File;
import java.util.Set;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.util.filePath.FilePath;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class EntityWithPath extends AbstractEntityWithPath
{

//---------------------------------------------------------------------------

  private final FilePath filePath;
  private HyperPath hyperPath;
  private boolean isDir, isDirSet = false, isHyperPathSet = false;

//---------------------------------------------------------------------------

  public EntityWithPath(File file)
  {
    filePath = new FilePath(file);
  }

//---------------------------------------------------------------------------

  @Override public FilePath getFilePath() { return filePath; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean isDirectory()
  {
    if (isDirSet == false)
    {
      isDir = filePath.isDirectory();
      isDirSet = true;
    }

    return isDir;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperPath getHyperPath()
  {
    if (isHyperPathSet == false)
    {
      Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);

      if (set.size() > 0)
        hyperPath = set.iterator().next();

      isHyperPathSet = true;
    }

    return hyperPath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
