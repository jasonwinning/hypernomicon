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

package org.hypernomicon.view.controls;

import javafx.scene.control.Tooltip;
import javafx.concurrent.Worker;
import javafx.scene.CacheHint;
import javafx.scene.control.ContentDisplay;
import javafx.scene.control.Control;
import javafx.scene.text.FontSmoothingType;
import javafx.scene.web.WebView;
import javafx.util.Duration;

//---------------------------------------------------------------------------

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
      "body, table { font-size: 11pt; font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Oxygen-Sans,Ubuntu,Cantarell,sans-serif; line-height:14pt; } " +

      "</style></head><body><div id='" + DivID + "' style='overflow:hidden'>" + text + "</div></body></html>");

    webView.getEngine().getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
    {
      if (newState == Worker.State.SUCCEEDED)
      {
        Object result = webView.getEngine().executeScript("document.getElementById('" + DivID + "').offsetHeight");

        if (result instanceof Integer offsetHeight)
          webView.setPrefHeight(offsetHeight + 15);

        result = webView.getEngine().executeScript("document.getElementById('" + DivID + "').scrollWidth");

        if (result instanceof Integer scrollWidth)
          webView.setPrefWidth(scrollWidth + 25);
      }
    });

    setContentDisplay(ContentDisplay.GRAPHIC_ONLY);
    setGraphic(webView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * <p>Sets up a click handler for a control that displays the tooltip of another control (potentially the same one) when clicked.
   * This function assumes that the tooltip has already been set for the `tooltipSrc` control.
   * The tooltip will be displayed at the position of the mouse click with an offset.
   * The tooltip will be hidden when the mouse exits the `eventSrc` control.
   *
   * <p>Note: This has the side effect of clearing the OnMouseExited event handler of eventSrc.
   *
   * @param eventSrc   the control that will trigger the display of the tooltip when clicked
   * @param tooltipSrc the control whose tooltip will be displayed
   */
  public static void setupClickHandler(Control eventSrc, Control tooltipSrc)
  {
    eventSrc.setOnMouseClicked(event ->
    {
      tooltipSrc.getTooltip().show(tooltipSrc, event.getScreenX() + 7, event.getScreenY() + 10);

      eventSrc.setOnMouseExited(exitEvent ->
      {
        tooltipSrc.getTooltip().hide();
        eventSrc.setOnMouseExited(null);
      });
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
