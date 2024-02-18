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

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public abstract class AbstractEntityWithPath
{
  private boolean related, relatedSet = false;

//---------------------------------------------------------------------------

  public abstract FilePath getFilePath();
  public abstract boolean isDirectory();
  public abstract HyperPath getHyperPath();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isRelated()
  {
    if (relatedSet == false)
    {
      related = nullSwitch(getHyperPath(), false, hyperPath -> hyperPath.getRecordsString().length() > 0);

      relatedSet = true;
    }

    return related;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
