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

package org.hypernomicon.view.mainText;

import static org.hypernomicon.view.mainText.MainTextWrapper.*;
import static org.junit.jupiter.api.Assertions.*;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import javax.imageio.ImageIO;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

import org.hypernomicon.util.FxTestUtil;
import org.hypernomicon.util.file.FilePath;

import javafx.concurrent.Worker;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

class MainTextWrapperTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @TempDir private Path tempDir;

  private String pictureURL, missingPictureURL;

//---------------------------------------------------------------------------

  @BeforeAll
  static void initFx()
  {
    FxTestUtil.initJfx();
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void writePicture() throws IOException
  {
    Path picture = Files.createDirectories(tempDir.resolve("folder with spaces")).resolve("picture.png");
    ImageIO.write(new BufferedImage(120, 80, BufferedImage.TYPE_INT_RGB), "png", picture.toFile());

    // URLs made the way MainTextUtil.prepHtmlForDisplay makes one for a picture from a misc. file record

    pictureURL = FilePath.of(picture).toURLString();
    missingPictureURL = FilePath.of(tempDir.resolve("missing.png")).toURLString();
  }

//---------------------------------------------------------------------------

  private static Document description(String imgSrc)
  {
    return Jsoup.parse("<html><head></head><body><p>Text</p><img src=\"" + imgSrc + "\" alt=\"\" width=\"300px\"/></body></html>");
  }

//---------------------------------------------------------------------------

  /**
   * Loads the HTML into a WebView through {@code loadContent}, as {@code setReadOnlyHTML}
   * does, and returns the natural width of the page's image: zero if it was not loaded.
   */
  private static int widthOfLoadedImage(String html) throws InterruptedException
  {
    CountDownLatch loaded = new CountDownLatch(1);
    AtomicReference<WebView> webView = new AtomicReference<>();  // Keeps the view reachable until its page has loaded
    AtomicReference<Object> width = new AtomicReference<>();

    FxTestUtil.runFxAndWait(() ->
    {
      webView.set(new WebView());
      WebEngine engine = webView.get().getEngine();

      engine.getLoadWorker().stateProperty().addListener((ob, oldState, newState) ->
      {
        if (newState != Worker.State.SUCCEEDED) return;

        width.set(engine.executeScript("document.images[0].naturalWidth"));
        loaded.countDown();
      });

      engine.loadContent(html);
    });

    assertTrue(loaded.await(30, TimeUnit.SECONDS), "The page did not finish loading");

    return ((Number) width.get()).intValue();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The src keeps the file's URL so that content copied or dragged out of the view still links to the local file. */
  @Test
  void aPictureLinkedByFileURLGetsItsContentsAsSrcsetAndKeepsItsURL()
  {
    Document doc = description(pictureURL);

    embedLocalImages(doc);

    Element img = doc.selectFirst("img");

    assertTrue(img.attr("srcset").startsWith("data:image/png;base64,"), img.attr("srcset"));
    assertEquals(pictureURL, img.attr("src"));
    assertEquals("300px", img.attr("width"));
  }

//---------------------------------------------------------------------------

  @Test
  void aPictureThatCannotBeEmbeddedGetsNoSrcset()
  {
    for (String src : new String[] { missingPictureURL, "https://example.org/picture.png", "file:not a URL" })
    {
      Document doc = description(src);

      embedLocalImages(doc);

      Element img = doc.selectFirst("img");

      assertFalse(img.hasAttr("srcset"), src);
      assertEquals(src, img.attr("src"));
    }
  }

//---------------------------------------------------------------------------

  /** Since WebKit 623.1 (JavaFX 26.0.1), a page loaded through {@code loadContent} is not
   *  allowed to load a {@code file:} URL, so a picture shows only once it has a srcset.
   *  The first assertion is a tripwire: it fails once WebKit loads such URLs again. */
  @Test
  void aLoadedDescriptionShowsAPictureOnceItIsEmbedded() throws InterruptedException
  {
    Document doc = description(pictureURL);

    assertEquals(0, widthOfLoadedImage(doc.html()), "WebKit loaded a file: URL into a page loaded from a string; the srcset workaround in MainTextWrapper.embedLocalImages may no longer be needed");

    embedLocalImages(doc);

    assertEquals(120, widthOfLoadedImage(doc.html()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
