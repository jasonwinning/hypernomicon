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

import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Represents a filesystem event relevant to full-text indexing. Constructed from
 * filesystem activity ({@link org.hypernomicon.FolderTreeWatcher FolderTreeWatcher}
 * events, {@code FileDeletion}'s post-deletion hook, and {@code FilePath}'s own
 * file operations) and passed to {@link FullTextIndexer#queueEvent}.
 *
 * @param kind     the type of event
 * @param oldPath  the previous path (for DELETE and MOVE events)
 * @param newPath  the new or current path (for CREATE, MODIFY, MOVE, and OVERFLOW events)
 * @param isDir    whether the affected path is a directory
 */
public record IndexEvent(Kind kind, FilePath oldPath, FilePath newPath, boolean isDir)
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum Kind { CREATE, DELETE, MODIFY, MOVE, OVERFLOW, SHUTDOWN }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static IndexEvent create  (FilePath path, boolean isDir) { return new IndexEvent(Kind.CREATE  , null, path, isDir); }
  public static IndexEvent delete  (FilePath path, boolean isDir) { return new IndexEvent(Kind.DELETE  , path, null, isDir); }
  public static IndexEvent modify  (FilePath path)                { return new IndexEvent(Kind.MODIFY  , null, path, false); }
  public static IndexEvent overflow(FilePath dir)                 { return new IndexEvent(Kind.OVERFLOW, null, dir , true ); }
  public static IndexEvent shutdown()                             { return new IndexEvent(Kind.SHUTDOWN, null, null, false); }

  public static IndexEvent move(FilePath oldPath, FilePath newPath, boolean isDir) { return new IndexEvent(Kind.MOVE, oldPath, newPath, isDir); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
