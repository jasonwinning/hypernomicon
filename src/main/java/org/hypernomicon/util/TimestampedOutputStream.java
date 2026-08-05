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

package org.hypernomicon.util;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

//---------------------------------------------------------------------------

/**
 * Prefixes every line written through it with a wall-clock timestamp
 * ({@code [HH:mm:ss.SSS] }). Inserted between the logging {@code PrintStream}
 * and the console/file tee, so all log output gains timing with no call-site
 * changes. Needed because several open diagnostic questions (viewer warm time,
 * overlay hide timing in the preview overhaul) are timing questions the
 * un-stamped log could not answer.
 * <p>
 * Blank lines are passed through without a stamp. Not thread-safe by itself;
 * the wrapping {@code PrintStream}'s internal synchronization serializes
 * writers, matching normal {@code System.out} usage.
 */
public final class TimestampedOutputStream extends FilterOutputStream
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("HH:mm:ss.SSS");

  private boolean atLineStart = true;

//---------------------------------------------------------------------------

  public TimestampedOutputStream(OutputStream out)
  {
    super(out);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void write(int b) throws IOException
  {
    write(new byte[] { (byte) b }, 0, 1);
  }

//---------------------------------------------------------------------------

  @Override public void write(byte[] buf, int off, int len) throws IOException
  {
    ByteArrayOutputStream result = new ByteArrayOutputStream(len + 16);

    for (int ndx = off; ndx < (off + len); ndx++)
    {
      byte curByte = buf[ndx];

      if (atLineStart && (curByte != '\n') && (curByte != '\r'))
      {
        result.writeBytes(('[' + LocalTime.now().format(FORMATTER) + "] ").getBytes(StandardCharsets.US_ASCII));
        atLineStart = false;
      }

      result.write(curByte);

      if (curByte == '\n')
        atLineStart = true;
    }

    out.write(result.toByteArray());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
