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

package org.hypernomicon.dialogs;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.text.Font;
import javafx.scene.web.WebView;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.text.DecimalFormat;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.query.engines.QueryEngine.QueryType;
import org.hypernomicon.query.reports.ReportEngine;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextUtil;

import com.sun.javafx.runtime.VersionInfo;

public class AboutDlgCtrlr extends HyperDlg
{
  @FXML private WebView webView;
  @FXML private TabPane tabPane;
  @FXML private Tab tabGeneral, tabContributors, tabAcknowledgements;

  private String buildDate, htmlStart, nextVersionHtml, tabContributorsHtml, tabAcknowledgementsHtml;

  private static final AsyncHttpClient httpClient = new AsyncHttpClient();

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static AboutDlgCtrlr build()
  {
    return ((AboutDlgCtrlr) create("AboutDlg", "About " + appTitle, false)).init();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private AboutDlgCtrlr init()
  {
    buildDate = manifestValue("Build-Time");

    if (safeStr(buildDate).isEmpty())
      buildDate = "not found";

    String family = Font.getDefault().getFamily();

    htmlStart = "<html><head>" + MainTextUtil.scriptContent +
        "<style>a:link { color:#906f6f; } a:visited { color:#906f6f; }" +
               "a.download:link { color:#eef4ff; } a.download:visited { color:#eef4ff; } </style>" +
        "</head><body style='margin: 0; padding: 0; font-family: " + family + "; font-size: 10pt; color: #906f6f;' bgcolor=\"#241f24\">";

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      MainTextUtil.handleJSEvent("", webView.getEngine(), new TextViewInfo());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    tabContributorsHtml = htmlStart + "Original design and development: " + anchorTag("Jason Winning", "http://jasonwinning.com") + "<br><br>" +
        anchorTag("List at GitHub", "https://github.com/jasonwinning/hypernomicon/contributors") + "&nbsp;&nbsp;&nbsp;" +
        anchorTag("Add your name to this list!", "https://akrabat.com/the-beginners-guide-to-contributing-to-a-github-project/") +
        "</body></html>";

    tabAcknowledgementsHtml = htmlStart + "<div style='-webkit-column-count: 3;'>Hypernomicon uses the following software:<br><ul>" +

        "<li>" + "Apache " + anchorTag("Commons", "https://commons.apache.org/") + ", " +
        anchorTag("PDFBox", "https://pdfbox.apache.org/") + ", " +
        anchorTag("Tika", "https://tika.apache.org/") + ", " +
        anchorTag("HttpClient", "https://hc.apache.org/httpcomponents-client-ga/") + "</li>" +
        "<li>" + anchorTag("Guava", "https://github.com/google/guava") + "</li>" +
        "<li>" + anchorTag("JxBrowser", "https://www.teamdev.com/jxbrowser") + "</li>" +
        "<li>" + anchorTag("PDF.js", "https://mozilla.github.io/pdf.js/") + "</li>" +
        "<li>" + anchorTag("jsoup", "https://jsoup.org/") + "</li>" +
        "<li>" + anchorTag("jQuery", "https://jquery.com/") + "</li>" +
        "<li>" + anchorTag("ICU4J", "http://site.icu-project.org/home") + "</li>" +
        "<li>" + anchorTag("ControlsFX", "http://fxexperience.com/controlsfx/") + "</li>" +
        "<li>" + anchorTag("JSON.simple", "https://code.google.com/archive/p/json-simple/") + "</li>" +
        "<li>" + anchorTag("ScribeJava", "https://github.com/scribejava/scribejava") + "</li>" +
        "<li>" + anchorTag("XMP Toolkit for Java", "https://www.adobe.com/devnet/xmp.html") + "</li>" +
        "<li>" + anchorTag("Mammoth .docx to HTML converter", "https://github.com/mwilliamson/java-mammoth") + "</li>" +
        "<li>" + anchorTag("JBibTex", "https://github.com/jbibtex/jbibtex") + "</li>" +
        "<li>" + anchorTag("highlight", "http://johannburkard.de/blog/programming/javascript/highlight-javascript-text-higlighting-jquery-plugin.html") +

        "</li></ul>Icons:<br><ul>" +

        "<li>" + anchorTag("FatCow", "http://www.fatcow.com/free-icons") + "</li>" +
        "<li>" + anchorTag("Fugue", "http://p.yusukekamiyamane.com/") + "</li></ul>" +

        "</div></body></html>";

    tabPane.getSelectionModel().selectedItemProperty().addListener((ob, oldTab, newTab) -> updateHtml(newTab));

    nextVersionHtml = "Checking to see if a newer version exists...";

    updateHtml(tabPane.getSelectionModel().getSelectedItem());

    checkForNewVersion(httpClient, newVersion ->
    {
      if (newVersion.compareTo(app.getVersion()) > 0)
        nextVersionHtml = "<b><a class=download href=\"\" onclick=\"openURL('https://sourceforge.net/projects/hypernomicon/files/latest/download'); return false;\">" +
                          "Newer version " + newVersion.toString() + " is available for download.</a></b>";
      else
        nextVersionHtml = "You have the latest version.";

      updateHtml(tabPane.getSelectionModel().getSelectedItem());
    }, () ->
    {
      nextVersionHtml = "Unable to determine latest version.";

      updateHtml(tabPane.getSelectionModel().getSelectedItem());
    });

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String anchorTag(String text, String url)
  {
    return "<a href=\"\" onclick=\"openURL('" + url + "'); return false;\">" + text + "</a>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateHtml(Tab tab)
  {
    if      (tab == tabGeneral         ) webView.getEngine().loadContent(getGeneralTabHtml()    );
    else if (tab == tabContributors    ) webView.getEngine().loadContent(tabContributorsHtml    );
    else if (tab == tabAcknowledgements) webView.getEngine().loadContent(tabAcknowledgementsHtml);
    else
    {
      dialogStage.close();

      Platform.runLater(() -> ui.showSearch(true, QueryType.qtReport, ReportEngine.QUERY_LICENSE_AND_NOTICE, null, null, null, ""));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getGeneralTabHtml()
  {
    String maxHeap = app.debugging() ? "Max heap space: " + new DecimalFormat("#,###").format(Runtime.getRuntime().maxMemory()) + "<br>" : "";

    return htmlStart +

        "Version: " + app.getVersion() + "&nbsp;&nbsp;&nbsp;&nbsp;" + nextVersionHtml + "<br>" +
        "Build date: " + buildDate + "<br>" +
        "Copyright \u00a9 2015-2022 Jason Winning.<br><br>" +
        "Operating system: " + SystemUtils.OS_NAME + "<br>" +
        "Operating system version: " + SystemUtils.OS_VERSION + "<br>" +
        "Java runtime: " + SystemUtils.JAVA_RUNTIME_VERSION + " " + SystemUtils.JAVA_RUNTIME_NAME + "<br>" +
        maxHeap +
        "JavaFX version: " + VersionInfo. getRuntimeVersion() + "<br>" +
        anchorTag("Website", "http://hypernomicon.org/") + "&nbsp;&nbsp;&nbsp;" +
        anchorTag("Release Notes", "https://sourceforge.net/p/hypernomicon/wiki/ReleaseNotes/") + "&nbsp;&nbsp;&nbsp;" +
        anchorTag("GitHub repo", "https://github.com/jasonwinning/hypernomicon") + "</body></html>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
