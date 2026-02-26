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

package org.hypernomicon.fileManager;

import static org.hypernomicon.fileManager.OverwriteResolver.Decision.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Tests for {@link OverwriteResolver}, the state machine that tracks
 * per-file overwrite-or-skip decisions across a series of file conflicts.
 */
class OverwriteResolverTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void initialState_mrYes_overwrite()
  {
    var resolver = new OverwriteResolver();

    assertEquals(OVERWRITE, resolver.resolve(mrYes));
  }

//---------------------------------------------------------------------------

  @Test
  void initialState_mrNo_skip()
  {
    var resolver = new OverwriteResolver();

    assertEquals(SKIP, resolver.resolve(mrNo));
  }

//---------------------------------------------------------------------------

  @Test
  void mrYesToAll_thenSubsequent_allOverwrite()
  {
    var resolver = new OverwriteResolver();

    assertEquals(OVERWRITE, resolver.resolve(mrYesToAll));
    assertEquals(OVERWRITE, resolver.resolve(null));
    assertEquals(OVERWRITE, resolver.resolve(null));
  }

//---------------------------------------------------------------------------

  @Test
  void mrNoToAll_thenSubsequent_allSkip()
  {
    var resolver = new OverwriteResolver();

    assertEquals(SKIP, resolver.resolve(mrNoToAll));
    assertEquals(SKIP, resolver.resolve(null));
    assertEquals(SKIP, resolver.resolve(null));
  }

//---------------------------------------------------------------------------

  @Test
  void mrYes_thenMrNo_individualDecisions()
  {
    var resolver = new OverwriteResolver();

    assertEquals(OVERWRITE, resolver.resolve(mrYes));
    assertEquals(SKIP,      resolver.resolve(mrNo));
  }

//---------------------------------------------------------------------------

  @Test
  void mrNo_thenMrYes_individualDecisions()
  {
    var resolver = new OverwriteResolver();

    assertEquals(SKIP,      resolver.resolve(mrNo));
    assertEquals(OVERWRITE, resolver.resolve(mrYes));
  }

//---------------------------------------------------------------------------

  @Test
  void mrNoToAll_thenManySubsequent_allSkip()
  {
    var resolver = new OverwriteResolver();

    assertEquals(SKIP, resolver.resolve(mrNoToAll));

    for (int ndx = 0; ndx < 5; ndx++)
      assertEquals(SKIP, resolver.resolve(null), "File " + ndx + " should be skipped");
  }

//---------------------------------------------------------------------------

  @Test
  void mrYesToAll_thenManySubsequent_allOverwrite()
  {
    var resolver = new OverwriteResolver();

    assertEquals(OVERWRITE, resolver.resolve(mrYesToAll));

    for (int ndx = 0; ndx < 5; ndx++)
      assertEquals(OVERWRITE, resolver.resolve(null), "File " + ndx + " should be overwritten");
  }

//---------------------------------------------------------------------------
//region needsUserPrompt
//---------------------------------------------------------------------------

  @Test
  void needsUserPrompt_initial_returnsTrue()
  {
    var resolver = new OverwriteResolver();

    assertTrue(resolver.needsUserPrompt());
  }

//---------------------------------------------------------------------------

  @Test
  void needsUserPrompt_afterYesToAll_returnsFalse()
  {
    var resolver = new OverwriteResolver();

    resolver.resolve(mrYesToAll);

    assertFalse(resolver.needsUserPrompt());
  }

//---------------------------------------------------------------------------

  @Test
  void needsUserPrompt_afterNoToAll_returnsFalse()
  {
    var resolver = new OverwriteResolver();

    resolver.resolve(mrNoToAll);

    assertFalse(resolver.needsUserPrompt());
  }

//---------------------------------------------------------------------------

  @Test
  void needsUserPrompt_afterYes_stillReturnsTrue()
  {
    var resolver = new OverwriteResolver();

    resolver.resolve(mrYes);

    assertTrue(resolver.needsUserPrompt(), "Single Yes should not change state to overwriteAll");
  }

//---------------------------------------------------------------------------

  @Test
  void needsUserPrompt_afterNo_stillReturnsTrue()
  {
    var resolver = new OverwriteResolver();

    resolver.resolve(mrNo);

    assertTrue(resolver.needsUserPrompt(), "Single No should not change state to overwriteNone");
  }

//---------------------------------------------------------------------------
//endregion
//region Independence of two resolvers
//---------------------------------------------------------------------------

  @Test
  void twoResolvers_independent()
  {
    OverwriteResolver resolverA = new OverwriteResolver(),
                      resolverB = new OverwriteResolver();

    assertEquals(OVERWRITE, resolverA.resolve(mrYesToAll));
    assertEquals(SKIP,      resolverB.resolve(mrNoToAll));

    // Each resolver maintains its own state independently

    assertEquals(OVERWRITE, resolverA.resolve(null));
    assertEquals(SKIP,      resolverB.resolve(null));

    assertFalse(resolverA.needsUserPrompt());
    assertFalse(resolverB.needsUserPrompt());
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
