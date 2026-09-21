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

package org.hypernomicon.util.json;

import static org.junit.jupiter.api.Assertions.*;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

import org.json.simple.parser.ParseException;

import org.junit.jupiter.api.Test;

import org.hypernomicon.HyperTask.HyperThread;

//---------------------------------------------------------------------------

class JsonObjTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int THREAD_COUNT = 4,
                           ROUND_COUNT  = 100,
                           ITEM_COUNT   = 100;

//---------------------------------------------------------------------------

  /** A document that only one thread parses: the thread's number is all through it, so
   *  anything that strays in from another thread's document shows. */
  private static String documentFor(int threadNdx)
  {
    StringBuilder sb = new StringBuilder("{\"owner\":").append(threadNdx).append(",\"items\":[");

    for (int itemNdx = 0; itemNdx < ITEM_COUNT; itemNdx++)
    {
      if (itemNdx > 0)
        sb.append(',');

      sb.append("{\"key\":\"T").append(threadNdx).append('-').append(itemNdx)
        .append("\",\"title\":\"Title ").append(itemNdx).append(" of thread ").append(threadNdx)
        .append("\",\"owner\":").append(threadNdx).append('}');
    }

    return sb.append("]}").toString();
  }

//---------------------------------------------------------------------------

  /**
   * Parses and copies the document over and over, every way callers do, and checks each
   * result against what a parse with no other thread running produced.
   *
   * @return what went wrong first, or an empty string
   */
  private static String parseRepeatedly(String document, JsonObj expected, CountDownLatch startSignal)
  {
    try
    {
      startSignal.await();

      for (int roundNdx = 0; roundNdx < ROUND_COUNT; roundNdx++)
      {
        JsonObj fromString = JsonObj.parseJsonObj(document),
                fromReader = JsonObj.parseJsonObj(new StringReader(document));

        JsonArray asArray = JsonObj.parseJson(document);

        if (expected.jObj.equals(fromString.jObj) == false)
          return "Round " + roundNdx + ": parsing a string gave " + fromString;

        if (expected.jObj.equals(fromReader.jObj) == false)
          return "Round " + roundNdx + ": parsing from a reader gave " + fromReader;

        if (expected.jObj.equals(fromString.deepCopy().jObj) == false)
          return "Round " + roundNdx + ": a deep copy of the object differs from it";

        if ((asArray.size() != 1) || (expected.jObj.equals(asArray.getObj(0).jObj) == false))
          return "Round " + roundNdx + ": parsing into an array gave " + asArray;

        if (expected.getArray("items").jArr.equals(fromString.getArray("items").deepCopy().jArr) == false)
          return "Round " + roundNdx + ": a deep copy of the array differs from it";
      }

      return "";
    }
    catch (Throwable e)  // The lexer reports input it cannot match with an Error
    {
      return e.toString();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Parsing happens on the JavaFX thread, on the HTTP request threads, on the thread of
   *  a reference-manager sync, and on the browser's threads, and a deep copy is a parse
   *  too. One parser shared by all of them would mix their input. */
  @Test
  void parsingOnSeveralThreadsAtOnceGivesEachItsOwnDocument() throws ParseException, InterruptedException, ExecutionException, TimeoutException
  {
    @SuppressWarnings("resource")  // Shut down in the finally block
    ExecutorService executor = Executors.newFixedThreadPool(THREAD_COUNT, runnable -> new HyperThread("JsonObjTest", runnable).asDaemon());

    try
    {
      CountDownLatch startSignal = new CountDownLatch(1);
      List<Future<String>> futures = new ArrayList<>();

      for (int threadNdx = 0; threadNdx < THREAD_COUNT; threadNdx++)
      {
        String document = documentFor(threadNdx);
        JsonObj expected = JsonObj.parseJsonObj(document);

        assertEquals(ITEM_COUNT, expected.getArray("items").size());

        futures.add(executor.submit(() -> parseRepeatedly(document, expected, startSignal)));
      }

      startSignal.countDown();

      for (Future<String> future : futures)
        assertEquals("", future.get(60, TimeUnit.SECONDS));
    }
    finally
    {
      executor.shutdownNow();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
