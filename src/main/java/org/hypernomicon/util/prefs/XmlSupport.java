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

package org.hypernomicon.util.prefs;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;

import java.io.*;
import java.nio.charset.Charset;
import java.util.prefs.InvalidPreferencesFormatException;
import java.util.prefs.Preferences;

import javax.xml.parsers.*;

import org.w3c.dom.*;
import org.xml.sax.*;

//---------------------------------------------------------------------------

/**
 * This class consists of code adapted from java.util.prefs.XmlSupport from JRE 11
 */
public final class XmlSupport
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private XmlSupport() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

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
  public static Preferences importPreferences(InputStream is, Charset encoding) throws IOException, InvalidPreferencesFormatException
  {
    Document doc = loadPrefsDoc(is, encoding);
    String xmlVersion = doc.getDocumentElement().getAttribute("EXTERNAL_XML_VERSION");

    if (xmlVersion.compareTo(EXTERNAL_XML_VERSION) > 0)
      throw new InvalidPreferencesFormatException(String.join(" ",
          "Exported preferences file format version", xmlVersion,
          "is not supported. This Java installation can read versions",
          EXTERNAL_XML_VERSION, "or older. You may need to install a newer JDK."));

    TransientPreferences prefsRoot = new TransientPreferences();
    importSubtree(prefsRoot, doc.getDocumentElement().getChildNodes().item(0));

    return prefsRoot;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Load an XML document from specified input stream, which must have the
   * requisite DTD URI.
   * @throws InvalidPreferencesFormatException if the XML format is invalid or unsupported.
   */
  private static Document loadPrefsDoc(InputStream inputStream, Charset encoding) throws IOException, InvalidPreferencesFormatException
  {
    DocumentBuilderFactory documentBuilderFactory = DocumentBuilderFactory.newInstance();

    documentBuilderFactory.setIgnoringElementContentWhitespace(true);
    documentBuilderFactory.setValidating(true);
    documentBuilderFactory.setCoalescing(true);
    documentBuilderFactory.setIgnoringComments(true);

    try
    {
      DocumentBuilder documentBuilder = documentBuilderFactory.newDocumentBuilder();

      // publicIdentifier: The public identifier of the external entity being referenced
      // systemIdentifier: The system identifier of the external entity being referenced.

      documentBuilder.setEntityResolver((publicIdentifier, systemIdentifier) ->
      {
        if (PREFS_DTD_URI.equals(systemIdentifier) == false)
          throw new SAXException("Invalid system identifier: " + systemIdentifier);

        InputSource inputSource = new InputSource(new StringReader(PREFS_DTD));
        inputSource.setEncoding(encoding.name());
        inputSource.setSystemId(PREFS_DTD_URI);
        return inputSource;
      });

      documentBuilder.setErrorHandler(new ErrorHandler()
      {
        @Override public void error     (SAXParseException e) throws SAXException { throw e; }
        @Override public void fatalError(SAXParseException e) throws SAXException { throw e; }
        @Override public void warning   (SAXParseException e) throws SAXException { throw e; }
      });

      return documentBuilder.parse(new InputSource(inputStream));
    }
    catch (ParserConfigurationException e)
    {
      throw newAssertionError(e);
    }
    catch (SAXException e)
    {
      throw new InvalidPreferencesFormatException(e);
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

    NodeList entries = xmlKids.item(0).getChildNodes();
    for (int i = 0; i < entries.getLength(); i++)
    {
      Element entry = (Element) entries.item(i);
      prefsNode.put(entry.getAttribute("key"), entry.getAttribute("value"));
    }

    // import children
    for (int i = 1; i < xmlKids.getLength(); i++)
    {
      if (xmlKids.item(i) instanceof Element xmlKid)
        importSubtree((TransientPreferences) prefsNode.node(xmlKid.getAttribute("name")), xmlKid);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // The required DTD URI for exported preferences
  private static final String PREFS_DTD_URI = "http://java.sun.com/dtd/preferences.dtd";

  // The actual DTD corresponding to the URI
  private static final String PREFS_DTD = """
    <?xml version="1.0" encoding="%s"?>

    <!-- DTD for preferences -->

    <!ELEMENT preferences (root) >
    <!ATTLIST preferences
     EXTERNAL_XML_VERSION CDATA "0.0" >

    <!ELEMENT root (map, node*) >
    <!ATTLIST root
              type (system|user) #REQUIRED >

    <!ELEMENT node (map, node*) >
    <!ATTLIST node
              name CDATA #REQUIRED >

    <!ELEMENT map (entry*) >
    <!ELEMENT entry EMPTY >
    <!ATTLIST entry
              key CDATA #REQUIRED
              value CDATA #REQUIRED >""".formatted(XML_FILES_CHARSET);

  /**
   * Version number for the format exported preferences files.
   */
  private static final String EXTERNAL_XML_VERSION = "1.0";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
