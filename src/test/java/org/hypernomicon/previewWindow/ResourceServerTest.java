/*
 * Copyright 2026 Jason Winning
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

import static org.junit.jupiter.api.Assertions.*;

import java.nio.file.Path;

import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * Round-trip contract of the file URL registry. A viewer page event names its
 * document by the URL the document was opened under, and the Java side must
 * get back to the file it issued from that URL alone; the URL for a file must
 * also be stable, since the comparison happens on a later event.
 */
class ResourceServerTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @TempDir Path tempDirPath;

//---------------------------------------------------------------------------

  @Test
  void fileForUrlReturnsTheFileTheUrlWasMintedFor()
  {
    FilePath filePath = FilePath.of(tempDirPath.resolve("Some Work (2nd ed.) & notes.pdf"));

    String url = ResourceServer.urlForFile(filePath);

    assertEquals(filePath, ResourceServer.fileForUrl(url));
    assertEquals(url, ResourceServer.urlForFile(filePath), "the same file must always map to the same URL");

    // The name segment is decorative; the token alone identifies the file

    assertEquals(filePath, ResourceServer.fileForUrl(url.substring(0, url.lastIndexOf('/'))));
  }

//---------------------------------------------------------------------------

  @Test
  void fileForUrlDistinguishesFiles()
  {
    FilePath first  = FilePath.of(tempDirPath.resolve("first.pdf")),
             second = FilePath.of(tempDirPath.resolve("second.pdf"));

    String firstUrl  = ResourceServer.urlForFile(first),
           secondUrl = ResourceServer.urlForFile(second);

    assertNotEquals(firstUrl, secondUrl);

    assertEquals(first , ResourceServer.fileForUrl(firstUrl ));
    assertEquals(second, ResourceServer.fileForUrl(secondUrl));
  }

//---------------------------------------------------------------------------

  @Test
  void fileForUrlRejectsUrlsThatAreNotItsFileUrls()
  {
    assertNull(ResourceServer.fileForUrl(null));
    assertNull(ResourceServer.fileForUrl(""));
    assertNull(ResourceServer.fileForUrl("not a url at all"));
    assertNull(ResourceServer.fileForUrl(ResourceServer.viewerUrl()));
    assertNull(ResourceServer.fileForUrl("hnres://app/file/999999999/unregistered.pdf"));

    // Same path shape under a foreign scheme must not resolve to a registered file

    String url = ResourceServer.urlForFile(FilePath.of(tempDirPath.resolve("registered.pdf")));

    assertNull(ResourceServer.fileForUrl(url.replaceFirst("^hnres:", "http:")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
