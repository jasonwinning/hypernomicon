/*
 * Copyright 2015-2021 Jason Winning
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

package org.hypernomicon.view.controls;

import javafx.scene.control.Tooltip;
import javafx.concurrent.Worker;
import javafx.scene.CacheHint;
import javafx.scene.control.ContentDisplay;
import javafx.scene.text.FontSmoothingType;
import javafx.scene.web.WebView;
import javafx.util.Duration;

public class WebTooltip extends Tooltip
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String DivID = "ttDiv";

  public WebTooltip(String text)
  {
    setShowDuration(Duration.INDEFINITE);

    WebView webView = new WebView();

    webView.setPrefSize(1024, -1);
    webView.setFontSmoothingType(FontSmoothingType.GRAY);
    webView.setMouseTransparent(true);
    webView.setCache(true);
    webView.setCacheHint(CacheHint.QUALITY);
    webView.getEngine().loadContent("<html><head><style>" +

      "a, a:visited, a:hover, a:active { color: #0000FF; } " +
      "td { white-space: nowrap; vertical-align: text-top; } body { white-space: nowrap; overflow-x: hidden; overflow-y: hidden; } " +
      "code { font-weight: bold; color: orangered; } " +
      "body, table { font-size: 9pt; font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Oxygen-Sans,Ubuntu,Cantarell,sans-serif; line-height:10pt; } " +

      "</style></head><body><div id='" + DivID + "' style='overflow:hidden'>" + text + "</div></body></html>");

    webView.getEngine().getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
    {
      if (newState == Worker.State.SUCCEEDED)
      {
        Object result = webView.getEngine().executeScript("document.getElementById('" + DivID + "').offsetHeight");

        if (result instanceof Integer)
          webView.setPrefHeight(Integer.class.cast(result) + 15);

        result = webView.getEngine().executeScript("document.getElementById('" + DivID + "').scrollWidth");

        if (result instanceof Integer)
          webView.setPrefWidth(Integer.class.cast(result) + 25);
      }
    });

    setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
    setGraphic(webView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
