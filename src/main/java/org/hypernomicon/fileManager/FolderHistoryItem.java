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

package org.hypernomicon.fileManager;

import org.hypernomicon.model.records.*;
import org.hypernomicon.util.filePath.FilePath;

//---------------------------------------------------------------------------

record FolderHistoryItem(HDT_Folder folder, FilePath fileName, HDT_Record record)
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  FolderHistoryItem(FileRow folderRow, FileRow fileRow, HDT_Record record)
  {
    this(folderRow.getRecord(), fileRow == null ? null : fileRow.getFilePath().getNameOnly(), record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
