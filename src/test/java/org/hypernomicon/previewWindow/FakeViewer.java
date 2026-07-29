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

package org.hypernomicon.previewWindow;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Recording {@link ViewerPort} for reconciler contract tests: every command
 * the pane issues is captured as a {@link Call} in order. This harness has the
 * same status as the production classes (the front door for boundary changes
 * must stay cheaper than any side door): protocol changes update this fake and
 * its tests in the same commit.
 */
final class FakeViewer implements ViewerPort
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * One recorded command. Fields not applicable to a command kind hold
   * {@code -1}/{@code null}.
   */
  record Call(String method, long gen, FilePath filePath, int pageNum, int ndxOnPage, String hitsJson, ProgressVariant variant) { }

//---------------------------------------------------------------------------

  private final List<Call> calls = new ArrayList<>();

//---------------------------------------------------------------------------

  /** All recorded calls, in issue order. */
  List<Call> calls() { return calls; }

  /** Just the method names, in issue order; the cheapest sequence assertion. */
  List<String> methods() { return calls.stream().map(Call::method).toList(); }

  /** The most recent call of the given method, or null. */
  Call last(String method)
  {
    for (int ndx = calls.size() - 1; ndx >= 0; ndx--)
      if (calls.get(ndx).method().equals(method))
        return calls.get(ndx);

    return null;
  }

  /** The generation of the most recent show command; -1 if none was issued. */
  long lastShownGen()
  {
    for (int ndx = calls.size() - 1; ndx >= 0; ndx--)
      if ("showDocument".equals(calls.get(ndx).method()) || "showContent".equals(calls.get(ndx).method()))
        return calls.get(ndx).gen();

    return -1;
  }

  void clear() { calls.clear(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void showEmpty()
  {
    calls.add(new Call("showEmpty", -1, null, -1, -1, null, null));
  }

  @Override public void showProgress(FilePath sourceFile, ProgressVariant variant)
  {
    calls.add(new Call("showProgress", -1, sourceFile, -1, -1, null, variant));
  }

  @Override public void showUnable(FilePath sourceFile)
  {
    calls.add(new Call("showUnable", -1, sourceFile, -1, -1, null, null));
  }

  @Override public void showDocument(long gen, FilePath documentPath, int pageNum)
  {
    calls.add(new Call("showDocument", gen, documentPath, pageNum, -1, null, null));
  }

  @Override public void showContent(long gen, FilePath contentPath)
  {
    calls.add(new Call("showContent", gen, contentPath, -1, -1, null, null));
  }

  @Override public void setHits(long gen, String hitsJson)
  {
    calls.add(new Call("setHits", gen, null, -1, -1, hitsJson, null));
  }

  @Override public void clearHits(long gen)
  {
    calls.add(new Call("clearHits", gen, null, -1, -1, null, null));
  }

  @Override public void goToPage(long gen, int pageNum)
  {
    calls.add(new Call("goToPage", gen, null, pageNum, -1, null, null));
  }

  @Override public void scrollToMatch(long gen, int matchNdx, int pageNum, int ndxOnPage)
  {
    calls.add(new Call("scrollToMatch", gen, null, pageNum, ndxOnPage, null, null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
