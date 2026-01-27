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
import static org.hypernomicon.util.Util.*;
import static org.junit.jupiter.api.Assertions.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.prefs.InvalidPreferencesFormatException;
import java.util.prefs.Preferences;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;

import org.apache.commons.io.FilenameUtils;

import org.hypernomicon.App;
import org.hypernomicon.util.VersionNumber;
import org.hypernomicon.util.prefs.XmlSupport;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class NewDBTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void newDBTemplateTest()
  {
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
          Preferences prefs = XmlSupport.importPreferences(new ByteArrayInputStream(zis.readAllBytes()), XML_FILES_CHARSET).node("org").node("hypernomicon").node("model");

          String versionStr = prefs.get(PrefKey.SETTINGS_VERSION, "");

          assertFalse(versionStr.isBlank(), "Settings version number not found in Settings.xml in new database template");

          VersionNumber settingsVersion = new VersionNumber(versionStr);

          assertEquals(appVersionToMaxSettingsXMLVersion.get(appVersion), settingsVersion, "Settings version in Settings.xml in the new database template is not the most up to date settings version number.");
        }
        else if ("xml".equalsIgnoreCase(FilenameUtils.getExtension(entry.getName())))
        {
          XMLEventReader eventReader = XMLInputFactory.newInstance().createXMLEventReader(new ByteArrayInputStream(zis.readAllBytes()), XML_FILES_CHARSET.name());

          VersionNumber dataVersion = getVersionNumberFromXML(eventReader);

          assertEquals(appVersionToMaxRecordsXMLVersion.get(appVersion), dataVersion, "Record data version in " + path + " in the new database template is not the most up to date data version number.");
        }
      }
    }
    catch (IOException | InvalidPreferencesFormatException | XMLStreamException | FactoryConfigurationError e)
    {
      fail("Error occurred while reading the template zip file: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
