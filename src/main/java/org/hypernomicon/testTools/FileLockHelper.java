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

package org.hypernomicon.testTools;

import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;

//---------------------------------------------------------------------------

/**
 * Standalone helper for cross-process file locking tests. Spawned as a child
 * JVM by {@link org.hypernomicon.fileManager.FileManagerTestRunner} to hold an exclusive file lock that
 * prevents same-JVM {@code FileChannel.tryLock()} from throwing
 * {@code OverlappingFileLockException}.
 * <p>
 * Usage: {@code java FileLockHelper <file-path>}
 * <p>
 * Prints "LOCKED" to stdout once the lock is acquired, then blocks on stdin
 * until the parent process destroys it.
 */
public final class FileLockHelper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FileLockHelper() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("resource") // Resources released when the process is destroyed
  static void main(String[] args) throws Exception
  {
    if (args.length < 1)
    {
      System.err.println("Usage: FileLockHelper <file-path>");
      System.exit(1);
    }

    RandomAccessFile raf = new RandomAccessFile(args[0], "rw");
    FileChannel channel = raf.getChannel();
    FileLock lock = channel.lock();

    System.out.println("LOCKED");
    System.out.flush();

    // Block until parent kills this process

    System.in.read();

    lock.release();
    channel.close();
    raf.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
