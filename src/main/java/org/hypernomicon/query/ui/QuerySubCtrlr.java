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

package org.hypernomicon.query.ui;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;

import org.hypernomicon.model.records.HDT_Record;

import javafx.fxml.FXML;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.layout.AnchorPane;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

/**
 * Base class for sub-tab controllers within the Queries tab. Both record-query
 * tabs ({@link QueryCtrlr}) and file-content-search tabs ({@link FTSQueryCtrlr})
 * share the same lifecycle: activate/deactivate with WebView reparenting, scroll
 * save/restore, and record removal handling.
 */
public abstract class QuerySubCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML protected AnchorPane apDescription;

  protected final WebView webView;
  protected Tab tab;
  protected int scrollPosPriorToBeingDeactivated;

//---------------------------------------------------------------------------

  protected QuerySubCtrlr(WebView webView)
  {
    this.webView = webView;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Tab getTab() { return tab; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when this sub-tab becomes the active tab within the Queries tab.
   * Must add the shared WebView to this tab's description pane and restore
   * any cached content and scroll position.
   */
  abstract void activate();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when this sub-tab is being replaced by another sub-tab.
   * Must save scroll position and remove the shared WebView from the
   * description pane.
   */
  void deactivate()
  {
    scrollPosPriorToBeingDeactivated = webEngineScrollPos(webView.getEngine());
    removeFromParent(webView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when a database record is deleted. Subclasses must remove or
   * clear any references to the deleted record in their result data.
   */
  abstract void removeRecord(HDT_Record record);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Execute the current query or search. For record queries this runs the
   * query; for FTS tabs this runs the file content search.
   */
  abstract void executeOrSearch();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when this sub-tab is selected within the inner tab pane.
   * Handles activation, toolbar visibility, and any tab-type-specific
   * setup that {@code QueriesTabCtrlr.tabPaneChange} previously dispatched.
   */
  abstract void onTabSelected(QueriesTabCtrlr queriesTabCtrlr);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when this sub-tab is being closed. Subclasses should perform
   * any cleanup (e.g., saving column widths).
   */
  abstract void onTabClosing();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called during {@link QueriesTabCtrlr#clear}. Subclasses should perform
   * cleanup and remove their tab from the tab pane.
   */
  abstract void onClear(TabPane tabPane);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
