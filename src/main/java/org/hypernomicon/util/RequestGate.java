/*
 * Copyright 2026 Jason Winning
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

//---------------------------------------------------------------------------

/**
 * Decides when a requested action runs: immediately, or held until some
 * condition and then possibly superseded by a later request. {@link SettleGate}
 * is the production policy (the request a burst settles on); the preview hosts
 * take the gate as a collaborator so their contract tests can hold a request
 * and release it deliberately.
 */
public interface RequestGate
{
  /** Runs the action now, or holds it (replacing any held action; latest wins). */
  void request(Runnable action);

  /** Discards any held action. */
  void cancel();
}
