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

package org.hypernomicon.bib.mendeley;

import java.time.Instant;

import org.hypernomicon.bib.BibEntity;

//---------------------------------------------------------------------------

interface MendeleyEntity extends BibEntity
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The instant that the Mendeley server says it was last modified
   * @return The instant that the Mendeley server says it was last modified
   */
  Instant lastModifiedOnServer();

  String Document_Last_Modified_JSON_Key = "last_modified",
         Folder_Last_Modified_JSON_Key   = "modified";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
