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

import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * Metadata for a single indexed file. Tracks the last-modified time, size,
 * and extraction status so the indexer can detect when a file needs reindexing
 * and skip files whose extraction previously failed. Immutable so that
 * {@code ConcurrentHashMap.put()} establishes a happens-before edge,
 * guaranteeing visibility of all fields to concurrent readers. Extensible
 * for future fields (page count, annotation count, etc.) via JSON defaults.
 */
record FileIndexEntry(long mtime, long size, IndexStatus status)
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  enum IndexStatus
  {
    INDEXED,    // Text successfully extracted and added to Lucene
    FAILED,     // extractText() threw an exception or returned null
    NO_TEXT,    // Extraction succeeded but returned blank (scanned/image-only PDFs)
    ABANDONED;  // Failed on two consecutive attempts with the file unchanged; no longer retried (resets to FAILED if the file changes)

    /** True for an extraction-failure status: {@link #FAILED} (will be retried) or {@link #ABANDONED}
     *  (retries exhausted). {@link #INDEXED} and {@link #NO_TEXT} are successes, not failures. */
    public boolean isFailedOrAbandoned() { return (this == FAILED) || (this == ABANDONED); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  JsonObj toJson(String relativePath)
  {
    JsonObj obj = new JsonObj();
    obj.put("path", relativePath);
    obj.put("mtime", mtime);
    obj.put("size", size);

    if (status != IndexStatus.INDEXED)
      obj.put("status", status.name());

    return obj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static FileIndexEntry fromJson(JsonObj obj)
  {
    String statusStr = obj.getStr("status");
    IndexStatus status = nullSwitch(statusStr, IndexStatus.INDEXED, IndexStatus::valueOf);

    return new FileIndexEntry(obj.getLong("mtime", 0L), obj.getLong("size", 0L), status);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
