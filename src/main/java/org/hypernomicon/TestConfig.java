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

package org.hypernomicon;

//---------------------------------------------------------------------------

public final class TestConfig
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Run automated test that navigates to and saves all records.
   * When enabled, view date updating is suppressed to avoid polluting timestamps.
   */
  public static boolean runRecordSaveCycleTest() { //if (Boolean.TRUE) return true;
    return false; }

  /**
   * During {@link #runRecordSaveCycleTest()}, also test entering edit mode on main text for each record.
   */
  static boolean runMainTextEditingTest() { //if (Boolean.TRUE) return true;
    return false; }

  /**
   * Enable long-running stress tests in unit test suites.
   */
  public static boolean runLongTests() { if (Boolean.TRUE) return true;
    return false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TestConfig() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
