/*
 * Copyright 2015-2022 Jason Winning
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

public class StopWatch
{
  long startTime, elapsedTime;
  boolean stopped;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public StopWatch()
  {
    stopped = true;
    elapsedTime = 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void stop()
  {
    if (stopped) return;

    elapsedTime = elapsed();
    stopped = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void resetAndStart()
  {
    reset();
    start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reset()
  {
    stop();
    elapsedTime = 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void start()
  {
    if (stopped == false) return;

    startTime = System.nanoTime();
    stopped = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String elapsedStr()
  {
    long num = 1000000000000000L + elapsed();

    String str = String.valueOf(num);
    str = str.substring(0, 7) + "." + str.substring(7);
    str = str.replaceFirst("^10*", "");

    if (str.startsWith("."))
      str = "0" + str;

    return str + " sec";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public long elapsed()
  {
    if (stopped) return elapsedTime;

    return elapsedTime + (System.nanoTime() - startTime);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
