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

import java.awt.Image;
import java.awt.Toolkit;
import java.awt.datatransfer.*;
import java.awt.image.BufferedImage;
import java.io.IOException;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * Utility class for retrieving image data from the system clipboard using AWT.
 * <p>
 * This class provides a robust fallback mechanism for clipboard image extraction,
 * especially useful when JavaFX's native clipboard handling is unreliable or inconsistent.
 * It handles both standard image flavors and platform-specific quirks (e.g., DIBV5 from Firefox).
 * </p>
 * <p>
 * This class is non-instantiable and contains only static methods.
 * </p>
 */
public final class ClipboardImageHelper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ClipboardImageHelper()  { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  /**
   * Attempts to retrieve an image from the system clipboard using AWT's {@link Toolkit}.
   * <p>
   * If the clipboard contains an image in the {@link DataFlavor#imageFlavor} format,
   * it is converted into a {@link BufferedImage} with a 24-bit BGR color model
   * ({@link BufferedImage#TYPE_3BYTE_BGR}) to ensure compatibility with JPEG encoding
   * and consistent rendering across platforms.
   * </p>
   *
   * @return A {@link BufferedImage} if an image is available and successfully retrieved;
   *         {@code null} otherwise.
   */
  public static BufferedImage getClipboardImageViaAWT()
  {
    Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
    Transferable contents = clipboard.getContents(null);

    if ((contents != null) && contents.isDataFlavorSupported(DataFlavor.imageFlavor))
    {
      try
      {
        Image awtImage = (Image) contents.getTransferData(DataFlavor.imageFlavor);

        // Convert to BufferedImage explicitly
        BufferedImage buffered = new BufferedImage(awtImage.getWidth(null), awtImage.getHeight(null), BufferedImage.TYPE_3BYTE_BGR);

        buffered.getGraphics().drawImage(awtImage, 0, 0, null);
        return buffered;
      }
      catch (UnsupportedFlavorException e)
      {
        System.out.println("Clipboard does not contain image flavor: " + getThrowableMessage(e));
      }
      catch (IOException e)
      {
        System.out.println("Failed to read image from clipboard: " + getThrowableMessage(e));
      }
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Prints all available {@link DataFlavor} types currently present on the system clipboard.
   * <p>
   * This method is useful for debugging clipboard content and understanding what formats
   * are currently available for retrieval. It can help identify why certain clipboard operations
   * (e.g., image extraction) may fail due to unsupported or unexpected data flavors.
   * </p>
   * <p>
   * Typical use cases include:
   * <ul>
   *   <li>Diagnosing why {@link DataFlavor#imageFlavor} is not available</li>
   *   <li>Inspecting clipboard contents after copying from browsers, image editors, or file managers</li>
   *   <li>Verifying platform-specific clipboard behavior (e.g., DIBV5 from Firefox)</li>
   * </ul>
   * </p>
   * <p>
   * Output is printed to {@code System.out}, listing each flavor's MIME type and representation class.
   * </p>
   */
  public static void inspectDataFormatOnClipboard()
  {
    Transferable t = Toolkit.getDefaultToolkit().getSystemClipboard().getContents(null);
    for (DataFlavor f : t.getTransferDataFlavors())
      System.out.println("Flavor: " + f);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
