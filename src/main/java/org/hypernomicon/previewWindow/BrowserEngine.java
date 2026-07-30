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

package org.hypernomicon.previewWindow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.teamdev.jxbrowser.browser.Browser;
import com.teamdev.jxbrowser.engine.*;
import com.teamdev.jxbrowser.engine.event.EngineCrashed;
import com.teamdev.jxbrowser.net.Scheme;
import com.teamdev.jxbrowser.view.javafx.BrowserView;

import org.hypernomicon.App;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion;

import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.*;

//---------------------------------------------------------------------------

/**
 * Owner of the application-wide JxBrowser {@link Engine}.
 * <p>
 * One Engine hosts every browser in the application: the preview/viewer
 * browsers (hardware-accelerated) and the off-screen pdf.js extractor pool.
 * The Chromium Main process starts when the engine is created and
 * {@link #shutdown()} terminates the entire process tree.
 * <p>
 * Lifecycle: {@link #initialize()} is blocking and must be called OFF the
 * JavaFX Application Thread; {@link App#init()} calls it on the launcher
 * thread on every platform, so the engine is ready before the UI shows. It is
 * idempotent and thread-safe. On success it sets {@code jxBrowserInitialized};
 * on failure it sets {@code jxBrowserDisabled} and closes the preview windows.
 * <p>
 * The custom {@code hnres:} resource scheme ({@link ResourceServer}) is
 * registered here because JxBrowser only accepts scheme registrations at
 * engine creation.
 * <p>
 * The license key is read from the classpath resource {@code /jxbrowser.key},
 * which the build copies from the untracked {@code local-resources} directory.
 */
public final class BrowserEngine
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BrowserEngine() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

  private static Engine engine = null;

  private static final String LICENSE_KEY_RESOURCE = "/jxbrowser.key",
                              USER_DATA_DIR_PREFIX = "hnChromiumData-";

//---------------------------------------------------------------------------

  /** Whether the engine exists and has not crashed or failed to start. */
  public static synchronized boolean isInitialized() { return (engine != null) && (jxBrowserDisabled == false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates the engine if it does not already exist. Blocking; call off the
   * FX thread. On failure (missing license key, engine startup error), shows
   * an error popup once and disables browser functionality; subsequent calls
   * return without retrying. An environment that cannot support the Chromium
   * sandbox is not treated as a failure: the engine is recreated with the
   * sandbox disabled.
   */
  public static synchronized void initialize()
  {
    if ((engine != null) || jxBrowserDisabled) return;

    // On macOS, make JxBrowser resolve host-window native handles through the
    // JavaFX glass internals (reflection; works because JavaFX is loaded from
    // the classpath) instead of its default accessibility-tree marker walk
    // (ToolkitLibrary.findNsViewByAxLink), which provokes a JavaFX accessibility
    // recursion that crashes the JVM; see primeModalAttach for the mechanism and
    // the second, independent mitigation kept alongside this one. Presence of
    // the property is what JxBrowser checks. The reflection path would silently
    // stop applying if this application ever moved JavaFX to the module path
    // (jlink/jpackage) without --add-exports.

    if (IS_OS_MAC)
      System.setProperty("jxbrowser.javafx.jni.embedding.disabled", "true");

    try
    {
      String licenseKey = loadLicenseKey();

      try
      {
        engine = createEngine(licenseKey, false);
      }
      catch (SandboxNotSupportedException e)
      {
        // Chromium's Linux sandbox is built on unprivileged user namespaces,
        // which Ubuntu 23.10+ denies to any binary lacking an AppArmor profile
        // that grants them; JxBrowser's Chromium, extracted to a temp dir, has
        // no profile, so on such systems sandboxed startup can never succeed.
        // Running unsandboxed beats losing previews and full-text indexing.

        engine = createEngine(licenseKey, true);
      }

      engine.on(EngineCrashed.class, event ->
      {
        System.out.println("Browser engine crashed with exit code " + event.exitCode());

        // Disable before notifying: work already in flight (a completing office
        // conversion, e.g.) keeps running while the popup is up, and must hit
        // the disabled guards rather than the dead engine.

        disable();

        // Fire-and-forget on purpose, even though errorPopup marshals itself to
        // the FX thread: called directly, it would park this JxBrowser observer
        // thread until the user dismisses the popup, stalling the library's own
        // event dispatch and crash bookkeeping; and if the crash arrives while
        // the FX toolkit is exiting, the discarded runnable would park this
        // thread forever, costing the organic exit.

        runInFXThread(() -> errorPopup("The browser engine has crashed. Previews will be unavailable until the application is restarted."));
      });

      jxBrowserInitialized = true;
    }
    catch (IOException | RuntimeException | LinkageError e)
    {
      engine = null;
      errorPopup("Unable to initialize preview window: " + getThrowableMessage(e));
      disable();
      return;
    }

    if (app.debugging == false)
      Logger.getLogger("com.teamdev.jxbrowser").setLevel(Level.WARNING);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates the engine, blocking until the Chromium Main process is up.
   * @param disableSandbox whether to launch Chromium without its sandbox; the
   *                       retry path for environments that cannot support it
   * @throws SandboxNotSupportedException when sandboxed startup is requested
   *                                      but the environment cannot support it
   */
  private static Engine createEngine(String licenseKey, boolean disableSandbox)
  {
    // HARDWARE_ACCELERATED is the display pipeline this application has
    // always shipped (JxBrowser 6 ran its heavyweight ancestor by default).
    // Two of its properties shape the preview display code in PDFJSWrapper:
    // the view is a native surface that paints over sibling JavaFX nodes
    // regardless of z-order (so the alt display hides the surface via
    // setVisible rather than overlaying it), and, observed in 9.3.1, a
    // document open dispatched in the same pulse as the surface being
    // re-shown can leave the surface blank while the document renders in
    // Chromium (mitigated by deferring opens a couple of render pulses; see
    // PDFJSWrapper.issueOpen). If that mitigation ever proves insufficient,
    // RenderingMode.OFF_SCREEN eliminates the native surface entirely, at
    // a cost to interactive rendering feel. But note it is no escape from
    // the macOS crash primeModalAttach mitigates: both rendering modes share
    // OffScreenRenderWidget.show, which is what triggers the window-handle
    // lookup behind that crash.

    EngineOptions.Builder builder = EngineOptions.newBuilder(RenderingMode.HARDWARE_ACCELERATED)
      .licenseKey(licenseKey)
      .userDataDir(tempDir().resolve(USER_DATA_DIR_PREFIX + InterProcClient.getInstanceID()).toPath())
      .addScheme(Scheme.of(ResourceServer.SCHEME_NAME), ResourceServer.callback());

    // History note (Aug 2026): with JxBrowser 9.4.0 and VoiceOver active,
    // opening a preview hard-hung the FX thread inside libtoolkit.dylib's
    // accessibility bridge (JxBrowser grafts Chromium's remote AX tree into
    // the host window; serving VoiceOver's queries of those elements never
    // terminated). 9.4.1 ships a fix for a macOS freeze with an active
    // accessibility client, which is why this application requires at least
    // that version. --disable-renderer-accessibility (macOS) suppressed part
    // of the bridge but did not prevent the hang, so it is not used: with the
    // vendor fix in place, screen readers should get preview content.

    if (disableSandbox)
      builder.disableSandbox();

    return Engine.newInstance(builder.build());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void disable()
  {
    jxBrowserDisabled = true;

    Platform.runLater(() ->
    {
      PreviewWindow .close(false);
      ContentsWindow.close(false);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Whether a BrowserView has already been attached inside a modal window this session;
   *  see {@link #primeModalAttach()}. */
  private static boolean modalAttachDone = false;

  /** Records that a modal window is about to host a BrowserView, which makes priming
   *  unnecessary. Called by {@link DialogPreviewHost}, whose previews are always
   *  dialog-hosted. */
  static void noteModalAttach() { modalAttachDone = true; }

//---------------------------------------------------------------------------

  /**
   * Ensures the process has attached a BrowserView inside a modal window, performing a
   * throwaway attach if none has happened yet. No-op on every platform but macOS, and after
   * the first time. Must be called on the FX thread, immediately before creating a viewer
   * that will attach into a non-modal window.
   *
   * <p>Ordering is what matters, and it does not need waiting for: JxBrowser completes an
   * attach through {@code Platform.runLater}, so as long as the throwaway window is shown
   * before the caller creates its own viewer, the throwaway's attach is queued first and
   * therefore runs first.
   *
   * <p>Attaching a BrowserView makes JxBrowser resolve its host window's native handle.
   * {@code NativeAwareWindow.discoverNativeId} branches by platform for this, and macOS is
   * the only one that goes through accessibility: it stamps a unique marker into the scene
   * root's accessible text and then walks the native accessibility tree to find the NSView
   * carrying it ({@code ToolkitLibrary.findNsViewByAxLink}). Windows and Linux instead
   * locate the window by screen bounds, which is why neither is affected.
   *
   * <p>That accessibility walk queries windows for their attributes, and JavaFX backs some
   * windows with NSPanel ({@code GlassWindow_Panel}): both UTILITY-styled stages, which is
   * what modal dialogs use here, and popups. The query recurses between glass and JavaFX's
   * Java accessibility code until the stack is exhausted and the JVM dies; the crash frame
   * is {@code -[GlassWindow_Panel accessibilityAttributeValue:]}, and {@code MacAccessible}'s
   * {@code NSAccessibilityParentAttribute} case (which answers with the host window and then
   * asks AppKit for its unignored ancestor) is the most likely half of the cycle.
   *
   * <p>{@link #initialize()} additionally sets the
   * {@code jxbrowser.javafx.jni.embedding.disabled} system property on macOS, which makes
   * JxBrowser resolve the handle by reflecting on {@code com.sun.glass.ui.Window} instead,
   * skipping the accessibility walk altogether; that removes the crash vector at its source.
   * The priming here is kept as an independent second mitigation, because the reflection
   * path works only while this application loads JavaFX from the classpath, where nothing is
   * encapsulated: moving to the module path would silently send JxBrowser back to the
   * accessibility walk, and the priming rule would then be the only thing standing.
   *
   * <p>The crash does not happen on every attach, and the rule was established empirically: if the
   * first attach of the process happens in a modal window, no later attach crashes, in any
   * order or quantity; if the first attach happens in a non-modal window, the second attach
   * in a modal dialog reliably kills the JVM. Why the first attach's window context should
   * decide this is not understood (presumably accessibility state initialized on first use
   * inherits it) so this is a mitigation, not a fix. The defect is JavaFX's (reproduced on
   * 25 and 26); JxBrowser's window-handle lookup only provokes it.
   *
   * <p>The throwaway window is 1x1 and fully transparent. Teardown is deliberately
   * unhurried: JxBrowser finishes the attach asynchronously (its handle lookup retries up to
   * ten times), and tearing down while that is in flight has crashed the JVM on its own.
   */
  @SuppressWarnings("resource")  // the browser outlives the method by design; the delayed teardown chain closes it
  static void primeModalAttach()
  {
    if ((IS_OS_MAC == false) || modalAttachDone || (isInitialized() == false) || (ui == null))
      return;

    modalAttachDone = true;  // one attempt only, however it goes

    Browser browser = null;

    try
    {
      browser = newBrowser();

      if (browser == null) return;

      BrowserView browserView = BrowserView.newInstance(browser);

      Stage stage = new Stage(StageStyle.UTILITY);
      stage.initOwner(ui.getStage());
      stage.initModality(Modality.APPLICATION_MODAL);
      stage.setOpacity(0.0);
      stage.setScene(new Scene(new AnchorPane(browserView), 1.0, 1.0));
      stage.show();

      Browser toClose = browser;

      runDelayedInFXThread(1, 1000, () ->
      {
        removeFromParent(browserView);  // never let a window hide with a BrowserView still in its scene
        stage.hide();

        runDelayedInFXThread(1, 1000, () -> runOutsideFXThread(() ->
        {
          try
          {
            if (toClose.isClosed() == false)
              toClose.close();
          }
          catch (RuntimeException e)
          {
            System.out.println("BrowserEngine: error closing priming browser: " + getThrowableMessage(e));
          }
        }));
      });
    }
    catch (RuntimeException e)
    {
      System.out.println("BrowserEngine.primeModalAttach: " + getThrowableMessage(e));

      if (browser != null)
        try { browser.close(); } catch (RuntimeException e2) { noOp(); }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates a hardware-accelerated browser (viewer/preview use).
   * @return the new browser, or null if the engine is unavailable
   */
  public static synchronized Browser newBrowser()
  {
    return isInitialized() ? engine.newBrowser() : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates an off-screen browser (pdf.js extractor pool use).
   * @return the new browser, or null if the engine is unavailable
   */
  public static synchronized Browser newOffScreenBrowser()
  {
    return isInitialized() ? engine.profiles().defaultProfile().newBrowser(RenderingMode.OFF_SCREEN) : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Closes the engine, terminating the Chromium Main/GPU/renderer processes
   * and closing every browser created from it. Fast (~200 ms measured) and
   * safe to call whether or not the engine was ever created.
   */
  public static synchronized void shutdown()
  {
    if (engine == null) return;

    Engine toClose = engine;
    engine = null;

    try
    {
      if (toClose.isClosed() == false)
        toClose.close();
    }
    catch (RuntimeException e)
    {
      System.out.println("Shutdown: error closing browser engine: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Deletes leftover Chromium user-data directories from previous runs. Called at
   * startup when this is the only running application instance; the current
   * instance's directory does not exist yet at that point, and a directory locked
   * by a concurrently running instance simply fails to delete, which is fine.
   * Also removes the JxBrowser 6 era's context folder if one is still around.
   */
  public static void clearStaleDataDirs()
  {
    FilePath tempFilePath = tempDir(),
             legacyFolder = tempFilePath.resolve("hnJxBrowserContext");

    if (legacyFolder.exists())
      FileDeletion.ofDirWithContents(legacyFolder).nonInteractiveFailureOK().execute();

    try (var stream = Files.newDirectoryStream(tempFilePath.toPath(), USER_DATA_DIR_PREFIX + '*'))
    {
      for (Path dataDirPath : stream)
        FileDeletion.ofDirWithContents(FilePath.of(dataDirPath)).nonInteractiveFailureOK().execute();
    }
    catch (IOException e)
    {
      noOp();
    }

    // Office-preview artifact caches from previous runs (both the legacy flat
    // hnTempOfficePreview dir and the instance-scoped hnTempOfficePreview-* dirs)

    try (var stream = Files.newDirectoryStream(tempFilePath.toPath(), "hnTempOfficePreview*"))
    {
      for (Path artifactDirPath : stream)
        FileDeletion.ofDirWithContents(FilePath.of(artifactDirPath)).nonInteractiveFailureOK().execute();
    }
    catch (IOException e)
    {
      noOp();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String loadLicenseKey() throws IOException
  {
    try (InputStream inputStream = App.class.getResourceAsStream(LICENSE_KEY_RESOURCE))
    {
      if (inputStream == null)
        throw new IOException("JxBrowser license key not found.");

      String key = new String(inputStream.readAllBytes(), StandardCharsets.UTF_8).strip();

      if (key.isBlank())
        throw new IOException("JxBrowser license key file is empty.");

      return key;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
