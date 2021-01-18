/*
 * Copyright 2015-2021 Jason Winning
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

package org.hypernomicon.util.prefs;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;

import java.util.prefs.InvalidPreferencesFormatException;
import java.util.prefs.Preferences;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.ErrorHandler;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

// This class consists of code adapted from java.util.prefs.XmlSupport from JRE 11

public class XmlSupport
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Import preferences from the specified input stream, which is assumed to
   * contain an XML document in the format described in the Preferences spec.
   *
   * @throws IOException
   *           if reading from the specified output stream results in an
   *           {@code IOException}.
   * @throws InvalidPreferencesFormatException
   *           Data on input stream does not constitute a valid XML document
   *           with the mandated document type.
   */
  public static Preferences importPreferences(InputStream is) throws IOException, InvalidPreferencesFormatException
  {
    try
    {
      Document doc = loadPrefsDoc(is);
      String xmlVersion = doc.getDocumentElement().getAttribute("EXTERNAL_XML_VERSION");
      if (xmlVersion.compareTo(EXTERNAL_XML_VERSION) > 0) throw new InvalidPreferencesFormatException("Exported preferences file format version " + xmlVersion + " is not supported. This java installation can read" + " versions " + EXTERNAL_XML_VERSION + " or older. You may need" + " to install a newer version of JDK.");

      TransientPreferences prefsRoot = new TransientPreferences();
      importSubtree(prefsRoot, doc.getDocumentElement().getChildNodes().item(0));

      return prefsRoot;
    }
    catch (SAXException e)
    {
      throw new InvalidPreferencesFormatException(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Load an XML document from specified input stream, which must have the
   * requisite DTD URI.
   */
  private static Document loadPrefsDoc(InputStream in) throws SAXException, IOException
  {
    DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();

    dbf.setIgnoringElementContentWhitespace(true);
    dbf.setValidating(true);
    dbf.setCoalescing(true);
    dbf.setIgnoringComments(true);

    try
    {
      DocumentBuilder db = dbf.newDocumentBuilder();

      db.setEntityResolver((pid, sid) ->
      {
        if (sid.equals(PREFS_DTD_URI) == false)
          throw new SAXException("Invalid system identifier: " + sid);

        InputSource is = new InputSource(new StringReader(PREFS_DTD));
        is.setSystemId(PREFS_DTD_URI);
        return is;
      });

      db.setErrorHandler(new ErrorHandler()
      {
        @Override public void error     (SAXParseException x) throws SAXException { throw x; }
        @Override public void fatalError(SAXParseException x) throws SAXException { throw x; }
        @Override public void warning   (SAXParseException x) throws SAXException { throw x; }
      });

      return db.parse(new InputSource(in));
    }
    catch (ParserConfigurationException e)
    {
      throw new AssertionError(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Recursively traverse the specified preferences node and store the described
   * preferences into the system or current user preferences tree, as
   * appropriate.
   */
  private static void importSubtree(TransientPreferences prefsNode, Node xmlNode)
  {
    // If removed, return silently
    if (prefsNode.isRemoved()) return;

    NodeList xmlKids = xmlNode.getChildNodes();
    int numXmlKids = xmlKids.getLength();

    NodeList entries = xmlKids.item(0).getChildNodes();
    for (int i = 0, numEntries = entries.getLength(); i < numEntries; i++)
    {
      Element entry = (Element) entries.item(i);
      prefsNode.put(entry.getAttribute("key"), entry.getAttribute("value"));
    }

    TransientPreferences[] prefsKids = new TransientPreferences[numXmlKids - 1];

    // Get involved children
    for (int i = 1; i < numXmlKids; i++)
    {
      Element xmlKid = (Element) xmlKids.item(i);
      prefsKids[i - 1] = (TransientPreferences) prefsNode.node(xmlKid.getAttribute("name"));
    }

    // import children
    for (int i = 1; i < numXmlKids; i++)
      importSubtree(prefsKids[i - 1], xmlKids.item(i));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // The required DTD URI for exported preferences
  private static final String PREFS_DTD_URI = "http://java.sun.com/dtd/preferences.dtd";

  // The actual DTD corresponding to the URI
  private static final String PREFS_DTD =

    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" +

    "<!-- DTD for preferences -->" +

    "<!ELEMENT preferences (root) >" +
    "<!ATTLIST preferences" +
    " EXTERNAL_XML_VERSION CDATA \"0.0\"  >" +

    "<!ELEMENT root (map, node*) >" +
    "<!ATTLIST root" +
    "          type (system|user) #REQUIRED >" +

    "<!ELEMENT node (map, node*) >" +
    "<!ATTLIST node" +
    "          name CDATA #REQUIRED >" +

    "<!ELEMENT map (entry*) >" +
    "<!ATTLIST map" +
    "  MAP_XML_VERSION CDATA \"0.0\"  >" +
    "<!ELEMENT entry EMPTY >" +
    "<!ATTLIST entry" +
    "          key CDATA #REQUIRED" +
    "          value CDATA #REQUIRED >";

  /**
   * Version number for the format exported preferences files.
   */
  private static final String EXTERNAL_XML_VERSION = "1.0";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
