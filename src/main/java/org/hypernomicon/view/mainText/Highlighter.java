/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.view.mainText;

import static org.hypernomicon.App.ui;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;

import org.apache.commons.text.StringEscapeUtils;

import javafx.scene.web.WebEngine;

//---------------------------------------------------------------------------

public class Highlighter
{
  private static final StringBuilder markJSContents      = new StringBuilder(),
                                     matchJumpJSContents = new StringBuilder();

  private final WebEngine engine;

  public Highlighter(WebEngine engine)
  {
    this.engine = engine;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean neverHilited()             { return ((boolean) engine.executeScript("'markInstance' in window")) == false; }
  boolean hasSearchResults()         { return (boolean) engine.executeScript("('markInstance' in window) && (results.length > 0)"); }

  public void nextSearchResult()     { engine.executeScript("if ('markInstance' in window) nextResult();"); }
  public void previousSearchResult() { engine.executeScript("if ('markInstance' in window) previousResult();"); }

  void clear()                       { engine.executeScript("if ('markInstance' in window) { clearAll(); markInstance.unmark({}); }"); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void hilite()                                      { hilite(false); }

  public void hilite(boolean onlyIfNonBlank)                { hilite(ui.currentFindInDescriptionText(), onlyIfNonBlank); }

  public void hilite(String string)                         { hilite(string, false); }

  public void hilite(String string, boolean onlyIfNonBlank) { hilite(string, onlyIfNonBlank, false); }

  public void hiliteAlreadyTagged()                         { hilite("", false, true); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void hilite(String string, boolean onlyIfNonBlank, boolean matchesAlreadyTagged)
  {
    if (onlyIfNonBlank && safeStr(string).isBlank())
      return;

    string = StringEscapeUtils.escapeEcmaScript(string);

    if (neverHilited())
    {
      engine.executeScript(markJSContents.toString());
      engine.executeScript("var markInstance = new Mark(\"body\"), results, lastNdx = -1, currentNdx = 0;");

      engine.executeScript(matchJumpJSContents.toString());
    }

    if (matchesAlreadyTagged)
      engine.executeScript("clearCurrent(); lastNdx=-1; results=document.getElementsByClassName('hypernomiconHilite'); currentNdx=0; jumpTo();");
    else
      engine.executeScript("clearAll(); lastNdx=-1; markInstance.unmark({done: function(){markInstance.mark(\"" + string + "\",{ className:\"hypernomiconHilite\",iframes:true,ignoreJoiners:true,separateWordSearch:false,acrossElements:true,done: function(){results=document.getElementsByClassName('hypernomiconHilite'); currentNdx=0; jumpTo();}});}});");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void init() throws IOException
  {
    readResourceTextFile("resources/mark.es6.min.js", markJSContents, false);
    readResourceTextFile("resources/match-jump.js", matchJumpJSContents, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
