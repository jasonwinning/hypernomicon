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

package org.hypernomicon.fts;

//---------------------------------------------------------------------------

/**
 * Whether a full-text search can find a file under the database root, and if not, why not.
 * Reported by {@link FullTextIndexer#listFiles} for the files a blank query lists: the listing
 * comes from the file registry rather than the index, so it holds files the index does not,
 * and this says of each one whether a search could have found it.
 */
public enum FileSearchability
{
  INDEXED         ("Indexed",          "Its text is in the index, so searches can find it."),
  NO_TEXT         ("No text",          "No text could be extracted from it (for example, a scanned PDF with no text layer), so searches cannot find it."),
  FAILED          ("Failed",           "Text extraction failed; the indexer will try again."),
  ABANDONED       ("Abandoned",        "Text extraction failed repeatedly; the indexer will not try again until the file changes or the index is rebuilt."),
  PENDING         ("Not indexed yet",  "The indexer has not processed this file yet."),
  RESERVED_FILE   ("Reserved file",    "Reserved for internal use by the database and never indexed."),
  EXCLUDED        ("Excluded",         "It is in an excluded folder or matches an excluded file mask (see the Full-Text Search tab of the Settings dialog), so it is not indexed."),
  UNSUPPORTED_TYPE("Unsupported type", "Files of this type are not indexed, so searches cannot find it.");

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String displayName, description;

//---------------------------------------------------------------------------

  FileSearchability(String displayName, String description)
  {
    this.displayName = displayName;
    this.description = description;
  }

//---------------------------------------------------------------------------

  /** Short form, for a table column. */
  public String displayName() { return displayName; }

  /** A sentence saying what the status means for searching. */
  public String description() { return description; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
