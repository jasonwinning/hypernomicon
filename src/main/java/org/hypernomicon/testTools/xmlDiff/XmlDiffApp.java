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

package org.hypernomicon.testTools.xmlDiff;

import java.io.IOException;
import java.util.prefs.Preferences;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configurator;
import org.apache.logging.log4j.core.config.DefaultConfiguration;

import org.hypernomicon.App;
import org.hypernomicon.util.file.FilenameRules;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.stage.Stage;

//---------------------------------------------------------------------------

public final class XmlDiffApp extends Application
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Object LOCK = new Object();

  public static XmlDiffApp xmlDiffApp;
  public static XmlDiffCtrlr xmlDiffCtrlr;

  public final Preferences prefs;

//---------------------------------------------------------------------------

  // Runs in FX application thread before init and before start

  public XmlDiffApp()
  {
    super();

    synchronized(LOCK)
    {
      if (xmlDiffApp != null)
        throw new UnsupportedOperationException("XmlDiffApp can only be instantiated once.");

      xmlDiffApp = this;
    }

    try (LoggerContext _ = Configurator.initialize(new DefaultConfiguration())) { Configurator.setRootLevel(Level.WARN); }

    Preferences appPrefs;

    try
    {
      appPrefs = Preferences.userNodeForPackage(App.class);
    }
    catch (SecurityException e)
    {
      errorPopup("Initialization error: " + getThrowableMessage(e));

      prefs = null;
      Platform.exit();
      return;
    }

    prefs = appPrefs;

    FilenameRules.initialize(prefs);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Runs in launcher thread (not FX application thread) after constructor and before start

  @Override public void init() throws Exception
  {
    super.init();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Runs in FX application thread, only if start was called
  // Occurs if all windows are closed, or if Platform.exit is called

  @Override public void stop() throws Exception
  {
    super.stop();

    xmlDiffCtrlr.savePaths();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Runs in FX application thread after init

  @Override public void start(Stage stage)
  {
    stage.setTitle("XML Diff Utility");

    try
    {
      initMainWindows(stage);
    }
    catch(IOException e)
    {
      errorPopup("Initialization error: " + getThrowableMessage(e));

      Platform.exit();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initMainWindows(Stage stage) throws IOException
  {
    setUserAgentStylesheet(STYLESHEET_MODENA);

    XmlDiffCtrlr.create(stage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
