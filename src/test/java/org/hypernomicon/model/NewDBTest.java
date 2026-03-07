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

package org.hypernomicon.model;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.util.HashMap;
import java.util.Map;
import java.util.function.UnaryOperator;
import java.util.prefs.InvalidPreferencesFormatException;
import java.util.prefs.Preferences;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.xml.stream.*;

import org.apache.commons.io.FilenameUtils;

import org.hypernomicon.App;
import org.hypernomicon.util.PopupRobot;
import org.hypernomicon.util.VersionNumber;
import org.hypernomicon.util.prefs.XmlSupport;

import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import javafx.scene.control.Alert.AlertType;

//---------------------------------------------------------------------------

class NewDBTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp()
  {
    PopupRobot.clear();
  }

//---------------------------------------------------------------------------

  @AfterEach
  void tearDown()
  {
    db.setRecordsLoadFilter(null, null);
    db.setSettingsLoadFilter(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void newDBTemplateTest()
  {
    Map<String, String> computedChecksums = new HashMap<>();
    String manifestStr = null;

    try (ZipInputStream zis = new ZipInputStream(App.class.getResourceAsStream(BLANK_DB_RESOURCE_NAME)))
    {
      ZipEntry entry;

      Path settingsFileNamePath = Paths.get(SETTINGS_FILE_NAME);

      while ((entry = zis.getNextEntry()) != null)
      {
        if (entry.isDirectory())
          continue;

        Path path = Paths.get(entry.getName());

        if (path.getFileName().equals(settingsFileNamePath))
        {
          byte[] bytes = zis.readAllBytes();

          Preferences prefs = XmlSupport.importPreferences(new ByteArrayInputStream(bytes), XML_FILES_CHARSET).node("org").node("hypernomicon").node("model");

          String versionStr = prefs.get(PrefKey.SETTINGS_VERSION, "");

          assertFalse(versionStr.isBlank(), "Settings version number not found in " + SETTINGS_FILE_NAME + " in new database template");

          VersionNumber settingsVersion = new VersionNumber(versionStr);

          assertEquals(appVersionToMaxSettingsXMLVersion.get(appVersion), settingsVersion, "Settings version in " + SETTINGS_FILE_NAME + " in the new database template is not the most up to date settings version number.");

          manifestStr = prefs.get(PrefKey.INTEGRITY_CHECKSUMS, "");
        }
        else if ("xml".equalsIgnoreCase(FilenameUtils.getExtension(entry.getName())))
        {
          byte[] bytes = zis.readAllBytes();

          computedChecksums.put(path.getFileName().toString(), computeMd5(bytes));

          XMLInputFactory xmlInputFactory = XMLInputFactory.newInstance();

          // The next 3 lines are a workaround for https://bugs.openjdk.org/browse/JDK-8368902

          xmlInputFactory.setProperty("jdk.xml.totalEntitySizeLimit"     , 0);
          xmlInputFactory.setProperty("jdk.xml.maxGeneralEntitySizeLimit", 0);
          xmlInputFactory.setProperty("jdk.xml.entityExpansionLimit"     , 0);

          XMLEventReader eventReader = xmlInputFactory.createXMLEventReader(new ByteArrayInputStream(bytes), XML_FILES_CHARSET.name());

          VersionNumber dataVersion = getVersionNumberFromXML(eventReader);

          assertEquals(appVersionToMaxRecordsXMLVersion.get(appVersion), dataVersion, "Record data version in " + path + " in the new database template is not the most up to date data version number.");
        }
      }
    }
    catch (IOException | InvalidPreferencesFormatException | XMLStreamException | FactoryConfigurationError e)
    {
      fail("Error occurred while reading the template zip file: " + getThrowableMessage(e));
    }

    assertNotNull(manifestStr, "Integrity checksums manifest not found in " + SETTINGS_FILE_NAME + " in new database template");
    assertFalse(manifestStr.isBlank(), "Integrity checksums manifest is empty in " + SETTINGS_FILE_NAME + " in new database template");

    Map<String, String> manifest = parseManifest(manifestStr);

    assertFalse(manifest.isEmpty(), "Integrity checksums manifest could not be parsed");
    assertFalse(computedChecksums.isEmpty(), "No XML files were found in the new database template");

    for (Map.Entry<String, String> manifestEntry : manifest.entrySet())
    {
      String filename = manifestEntry.getKey(),
             expected = manifestEntry.getValue(),
             actual   = computedChecksums.get(filename);

      if (actual != null)
        assertEquals(expected, actual, "Integrity checksum mismatch for " + filename + " in new database template");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @ParameterizedTest
  @MethodSource("org.hypernomicon.model.AbstractHyperDB#recordXMLFileNames")
  void integrityChecksumMismatchTest(String targetFileName)
  {
    // Insert an XML comment to change the checksum while keeping the XML valid

    db.setRecordsLoadFilter(targetFileName, bytes ->
    {
      String xml = new String(bytes, XML_FILES_CHARSET);
      return xml.replace("</records>", "<!-- modified -->\n</records>").getBytes(XML_FILES_CHARSET);
    });

    db.closeAndOpen();

    assertEquals(1, PopupRobot.getInvocationCount(), "Expected exactly one warning popup for " + targetFileName);
    assertEquals(AlertType.WARNING, PopupRobot.getLastType(), "Expected a warning popup for " + targetFileName);
    assertTrue(PopupRobot.getLastMessage().contains(targetFileName), "Warning should mention " + targetFileName);
    assertTrue(PopupRobot.getLastMessage().contains("A database file appears"), "Warning should use singular form for single mismatch");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void integrityChecksumMultipleMismatchTest()
  {
    UnaryOperator<byte[]> filter = bytes ->
    {
      String xml = new String(bytes, XML_FILES_CHARSET);
      return xml.replace("</records>", "<!-- modified -->\n</records>").getBytes(XML_FILES_CHARSET);
    };

    db.setRecordsLoadFilter("People.xml", filter);
    db.setRecordsLoadFilter("Works.xml", filter);

    db.closeAndOpen();

    assertEquals(1, PopupRobot.getInvocationCount(), "Expected exactly one warning popup for multiple mismatches");
    assertEquals(AlertType.WARNING, PopupRobot.getLastType());
    assertTrue(PopupRobot.getLastMessage().contains("People.xml"), "Warning should mention People.xml");
    assertTrue(PopupRobot.getLastMessage().contains("Works.xml"), "Warning should mention Works.xml");
    assertTrue(PopupRobot.getLastMessage().contains("One or more database files appear"), "Warning should use plural form for multiple mismatches");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void missingManifestWithCurrentVersionTest()
  {
    db.setSettingsLoadFilter(bytes ->
    {
      String xml = new String(bytes, XML_FILES_CHARSET);
      return xml.replaceAll("\\s*<entry key=\"integrityChecksums\"[^/]*/>\n?", "\n").getBytes(XML_FILES_CHARSET);
    });

    db.closeAndOpen();

    assertEquals(1, PopupRobot.getInvocationCount(), "Expected exactly one warning popup for missing manifest");
    assertEquals(AlertType.WARNING, PopupRobot.getLastType(), "Expected a warning popup for missing manifest");
    assertTrue(PopupRobot.getLastMessage().contains("integrity checksums manifest is missing"), "Warning should mention missing manifest");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String computeMd5(byte[] data)
  {
    MessageDigest md = newMessageDigest();
    md.update(data);
    return digestHexStr(md);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Map<String, String> parseManifest(String manifest)
  {
    if (strNullOrBlank(manifest))
      return Map.of();

    Map<String, String> result = new HashMap<>();

    for (String entry : manifest.split(";"))
    {
      int colonNdx = entry.indexOf(':');

      if (colonNdx > 0)
        result.put(entry.substring(0, colonNdx), entry.substring(colonNdx + 1));
    }

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
