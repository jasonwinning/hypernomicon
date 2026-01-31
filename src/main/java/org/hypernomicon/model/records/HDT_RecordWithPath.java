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

package org.hypernomicon.model.records;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

public interface HDT_RecordWithPath extends HDT_Record
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperPath getPath();

//---------------------------------------------------------------------------

  default boolean  pathNotEmpty()   { return nullSwitch(getPath(), false, HyperPath::isNotEmpty); }
  default FilePath filePath()       { return nullSwitch(getPath(), null , HyperPath::filePath); }
  default HDT_Folder parentFolder() { return nullSwitch(getPath(), null , HyperPath::parentFolder); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
