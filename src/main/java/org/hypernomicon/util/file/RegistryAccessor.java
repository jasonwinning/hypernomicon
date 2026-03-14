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

import java.nio.file.Path;
import java.util.Set;

import org.hypernomicon.model.items.HyperPath;

//---------------------------------------------------------------------------

/**
 * Capability interface for accessing the {@link FilePathRegistry}. Only authorized
 * components receive a reference to this interface during database session start.
 * <p>
 * This follows the same pattern as {@link org.hypernomicon.model.DatasetAccessor DatasetAccessor},
 * which provides a narrow window into {@link org.hypernomicon.model.data.HyperDataset HyperDataset}
 * and its internal {@code HyperCore}. Here, the sealed interface ensures only
 * {@code FilePathRegistry} can implement it, and the gated
 * {@link FilePathRegistry#getAccessor()} controls who obtains a reference.
 *
 * @see FilePathRegistry#getAccessor()
 */
public sealed interface RegistryAccessor permits FilePathRegistry
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Return the canonical {@link FilePath} for the given absolute path, creating and caching it
   * if necessary.
   */
  FilePath getOrCreate(Path absPath);

  /**
   * Return the set of {@link HyperPath} instances associated with the given path.
   * Returns a defensive copy.
   */
  Set<HyperPath> getHyperPaths(FilePath filePath);

  /** Register a {@link HyperPath} association for the given path. */
  void addHyperPath(FilePath filePath, HyperPath hyperPath);

  /** Remove a {@link HyperPath} association for the given path. */
  void removeHyperPath(FilePath filePath, HyperPath hyperPath);

  /** Re-register HyperPath associations after a directory move. */
  void onSubtreeMoved(FilePath oldDir);

  /** Populate the registry by walking the filesystem under the root path. */
  void populate(FilePath rootPath);

  /** Activate the registry for unit testing without filesystem walking. */
  void populateForTesting(FilePath rootPath, Path... paths);

  /** Clear all entries and deactivate the registry. */
  void clear();

  /** Return whether the registry is populated and active. */
  boolean isActive();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
