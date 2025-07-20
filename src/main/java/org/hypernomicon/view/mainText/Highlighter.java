/*
 * Copyright 2015-2025 Jason Winning
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
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;

import java.io.IOException;

import org.apache.commons.text.StringEscapeUtils;

import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public class Highlighter
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final StringBuilder markJSContents      = new StringBuilder(),
                                     matchJumpJSContents = new StringBuilder();

  private final WebEngine engine;

  private String currentString;

//---------------------------------------------------------------------------

  public Highlighter(WebEngine engine)
  {
    this.engine = engine;
  }

//---------------------------------------------------------------------------

  public Highlighter(WebView webView)
  {
    this(webView.getEngine());

    webView.focusWithinProperty().addListener((obs, ov, nv) ->
    {
      if (Boolean.TRUE.equals(nv))
        clear();
    });
  }

//---------------------------------------------------------------------------

  boolean neverHilited()             { return ((boolean) engine.executeScript("'markInstance' in window")) == false; }
  private boolean hasSearchResults() { return (boolean) engine.executeScript("('markInstance' in window) && (results.length > 0)"); }
  void clear()                       { engine.executeScript("if ('markInstance' in window) { clearAll(); markInstance.unmark({}); }"); currentString = ""; }
  public void nextSearchResult()     { advance("nextResult();"); }
  public void previousSearchResult() { advance("previousResult();"); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void init() throws IOException
  {
    readResourceTextFile("resources/mark.es6.min.js", markJSContents     , false);
    readResourceTextFile("resources/match-jump.js"  , matchJumpJSContents, false);
  }

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
    string = safeStr(string);

    if (onlyIfNonBlank && string.isBlank())
      return;

    if (hasSearchResults() && string.equals(currentString))
      return;

    currentString = string;
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

  private void advance(String jsCode)
  {
    if (hasSearchResults() == false)
    {
      if (currentString.isBlank())
        hilite();
      else
        hilite(currentString);
    }
    else
      engine.executeScript(jsCode);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
