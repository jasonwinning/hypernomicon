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

//---------------------------------------------------------------------------

/**
 * Public bridge for test classes outside {@code org.hypernomicon.util.file} that need to
 * activate the {@link FilePathRegistry} with {@link RegistryAccessor#populateForTesting}.
 */
public final class FilePathRegistryTestHelper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePathRegistryTestHelper() { }

//---------------------------------------------------------------------------

  /**
   * Activate the registry for testing via the gated {@link FilePathRegistry#getAccessor()}.
   *
   * @param root  the root directory (typically a {@code @TempDir})
   * @param paths optional paths to pre-intern
   * @return the {@link RegistryAccessor} for further test interaction
   */
  public static RegistryAccessor activateForTesting(Path root, Path... paths)
  {
    RegistryAccessor accessor = FilePathRegistry.getAccessor();
    accessor.populateForTesting(FilePath.of(root), paths);
    return accessor;
  }

//---------------------------------------------------------------------------

  /**
   * Deactivate the registry. Delegates to {@link RegistryAccessor#clear()}.
   */
  public static void deactivate()
  {
    FilePathRegistry.getAccessor().clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
