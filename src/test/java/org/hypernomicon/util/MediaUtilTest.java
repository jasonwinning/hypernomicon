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

package org.hypernomicon.util;

import static org.hypernomicon.util.MediaUtil.*;
import static org.junit.jupiter.api.Assertions.*;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.nio.file.*;
import java.util.Base64;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

class MediaUtilTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @TempDir private Path tempDir;

//---------------------------------------------------------------------------

  private Path writePng(String fileName) throws IOException
  {
    Path path = tempDir.resolve(fileName);
    ImageIO.write(new BufferedImage(12, 8, BufferedImage.TYPE_INT_RGB), "png", path.toFile());
    return path;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void anImageFileBecomesADataURIOfItsContentsAndMediaType() throws IOException
  {
    Path png = writePng("picture.png");

    String dataURI = imgDataURI(FilePath.of(png)),
           prefix = "data:image/png;base64,";

    assertTrue(dataURI.startsWith(prefix), dataURI);
    assertArrayEquals(Files.readAllBytes(png), Base64.getDecoder().decode(dataURI.substring(prefix.length())));
  }

//---------------------------------------------------------------------------

  @Test
  void anImageIsRecognizedByItsContentsWhenItsNameHasNoExtension() throws IOException
  {
    assertTrue(imgDataURI(FilePath.of(writePng("picture"))).startsWith("data:image/png;base64,"));
  }

//---------------------------------------------------------------------------

  /** WebKit recognizes a bitmap by its contents whatever the data URI says it is, but it
   *  shows an SVG drawing only if the data URI says it is one. */
  @Test
  void anSvgDrawingGetsTheMediaTypeItNeedsToBeShown() throws IOException
  {
    Path svg = Files.writeString(tempDir.resolve("drawing.svg"), "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"4\" height=\"3\"/>");

    assertTrue(imgDataURI(FilePath.of(svg)).startsWith("data:image/svg+xml;base64,"));
  }

//---------------------------------------------------------------------------

  /** A misc. file record inserted as a picture can be any kind of file, and a file that is
   *  not an image is not read at all. */
  @Test
  void whatIsNotAReadableImageFileGivesNoDataURI() throws IOException
  {
    Path pdf = Files.writeString(tempDir.resolve("notes.pdf"), "%PDF-1.4\n%%EOF\n");

    assertNull(imgDataURI(FilePath.of(pdf)));
    assertNull(imgDataURI(FilePath.of(tempDir.resolve("missing.png"))));
    assertNull(imgDataURI((FilePath) null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
