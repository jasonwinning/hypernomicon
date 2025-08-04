/*
 * Copyright 2015-2025 Jason Winning
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

package org.hypernomicon.bib.data;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.model.items.BibliographicDate.DateType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.*;
import java.util.function.Predicate;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.Strings;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentInformation;
import org.apache.pdfbox.pdmodel.common.PDMetadata;
import org.apache.pdfbox.text.PDFTextStripper;

import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.BibliographicDate.DateType;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.util.filePath.FilePath;

import com.adobe.internal.xmp.*;
import com.adobe.internal.xmp.properties.XMPPropertyInfo;

//---------------------------------------------------------------------------

public class PDFBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// See the following for some library code that might be useful:
// https://svn.apache.org/viewvc/pdfbox/trunk/examples/src/main/java/org/apache/pdfbox/examples/pdmodel/AddMetadataFromDocInfo.java?view=markup
// Or google "import org.apache.xmpbox.schema"

  private static final class PathParts
  {
    private String prefix = null, name = null;
    int arrayNdx = -1;

    private PathParts(String str)
    {
      if (strNullOrEmpty(str)) return;

      if (str.startsWith("["))
      {
        arrayNdx = parseInt(str.substring(1, str.indexOf(']')), 0) - 1;
        return;
      }

      int ndx = str.indexOf(':');
      prefix = str.substring(0, ndx);
      name = str.substring(ndx + 1);

      ndx = name.indexOf('[');
      if (ndx >= 0)
      {
        arrayNdx = parseInt(name.substring(ndx + 1, name.indexOf(']')), 0) - 1;
        name = name.substring(0, ndx);
      }
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final class XMPNode
  {
    private final XMPNode parent;
    private final XMPMeta xmpMeta;
    private final XMPPropertyInfo propInfo;

    private final Map<String, Map<String, XMPNode>> prefixToNameToChild = new LinkedHashMap<>();
    private final List<XMPNode> elements = new ArrayList<>();
    private final String ns, path, value, prefix, name;
    private final int arrayNdx;

    @SuppressWarnings("unused")
    private XMPPropertyInfo getPropInfo() { return propInfo; }
    private String getNamespace()         { return ns; }

  //---------------------------------------------------------------------------

    private XMPNode(XMPMeta xmpMeta, XMPNode parent, XMPPropertyInfo propInfo)
    {
      this.xmpMeta = xmpMeta;
      this.parent = parent;
      this.propInfo = propInfo;

      if (propInfo != null)
      {
        ns = nullSwitch(propInfo.getNamespace(), parent.getNamespace());
        path = propInfo.getPath();
        value = propInfo.getValue();
      }
      else
      {
        ns = null;
        path = null;
        value = null;
      }

      if (path != null)
      {
        int ndx = path.lastIndexOf('/');
        String subPath = path.substring(ndx + 1);

        PathParts parts = new PathParts(subPath);

        prefix = parts.prefix;
        name = parts.name;
        arrayNdx = parts.arrayNdx;
      }
      else
      {
        prefix = null;
        name = null;
        arrayNdx = -1;
      }
    }

  //---------------------------------------------------------------------------

    private void addDescendant(XMPPropertyInfo targetInfo)
    {
      String subPath = targetInfo.getPath().substring(safeStr(path).length());
      if (subPath.startsWith("/"))
        subPath = subPath.substring(1);

      int ndx = subPath.indexOf('/');
      if (ndx >= 0)
        subPath = subPath.substring(0, ndx);

      PathParts parts = new PathParts(subPath);

      if (subPath.contains(":") == false)
      {
        if (elements.size() > parts.arrayNdx)
          elements.get(parts.arrayNdx).addDescendant(targetInfo);
        else
          elements.add(new XMPNode(xmpMeta, this, targetInfo));

        return;
      }

      Map<String, XMPNode> nameToChild;

      if (prefixToNameToChild.containsKey(parts.prefix) == false)
        prefixToNameToChild.put(parts.prefix, nameToChild = new LinkedHashMap<>());
      else
        nameToChild = prefixToNameToChild.get(parts.prefix);

      if (nameToChild.containsKey(parts.name))
      {
        nameToChild.get(parts.name).addDescendant(targetInfo);
        return;
      }

      nameToChild.put(parts.name, new XMPNode(xmpMeta, this, targetInfo));
    }

  //---------------------------------------------------------------------------

    private void addCsvLines(List<List<String>> csvFile)
    {
      List<String> line = new ArrayList<>();

      if (strNotNullOrBlank(value))
      {
        String str = "";

        if (arrayNdx >= 0)
          str = "[" + (arrayNdx + 1) + "] ";

        line.add(str + value);
        line.addAll(getCsvPath());

        csvFile.add(line);
      }

      prefixToNameToChild.values().forEach(nameToChild ->
        nameToChild.values().forEach(child ->
          child.addCsvLines(csvFile)));

      elements.forEach(child -> child.addCsvLines(csvFile));
    }

    //---------------------------------------------------------------------------

    private List<String> getCsvPath()
    {
      List<String> line = parent == null ? new ArrayList<>() : parent.getCsvPath();

      if (strNotNullOrEmpty(name))
      {
        line.add(prefix);
        line.add(name);
      }

      return line;
    }

  //---------------------------------------------------------------------------

    private static boolean nameIsNotExcluded(String nameStr)
    {
      return Strings.CI.containsAny(safeStr(nameStr), "journaldoi", "instanceid", "documentid") == false;
    }

  //---------------------------------------------------------------------------

    private void extractDOIandISBNs(PDFBibData bd)
    {
      if (nameIsNotExcluded(name))
        bd.extractDOIandISBNs(value);

      prefixToNameToChild.values().forEach(nameToChild -> nameToChild.forEach((childName, child) ->
      {
        if (nameIsNotExcluded(childName))
          child.extractDOIandISBNs(bd);
      }));

      elements.forEach(child -> child.extractDOIandISBNs(bd));
    }

  //---------------------------------------------------------------------------

    private void extractBibData(PDFBibData bd)
    {
      if (elements.isEmpty() == false)
      {
        if ("dc".equals(prefix))
        {
          switch (name)
          {
            case "creator":

              authors.clear();
              elements.forEach(child -> authors.add(author, new PersonName(child.value)));
              break;

            case "title":

              bd.setMultiStr(bfTitle, Collections.emptyList());

              elements.forEach(child ->
              {
                if ("untitled".equalsIgnoreCase(child.value) == false)
                  bd.addStr(bfTitle, child.value);
              });

              break;

            case "description":

              elements.forEach(child -> bd.addStr(bfMisc, child.value));
              break;

            case "publisher":

              bd.setStr(bfPublisher, elements.getFirst().value);
              break;
          }
        }
        else if (safeStr(prefix).startsWith("prism"))
        {
          DateType dt = getByDesc(name);

          if (dt != dtUnknown)
            bd.setDate(BibliographicDate.fromUserStr(elements.getFirst().value), dt, false);
        }
      }

      if (safeStr(prefix).startsWith("prism"))
      {
        DateType dt = getByDesc(name);

        if (dt != dtUnknown)
          bd.setDate(BibliographicDate.fromUserStr(value), dt, false);
        else
        {
          switch (name)
          {
            case "aggregationType" : bd.setEntryType(parsePrismAggregationType(value)); break;
            case "issn"            : bd.addISSN(value);                  break;
            case "publicationName" : bd.addStr(bfContainerTitle, value); break;
            case "volume"          : bd.setStr(bfVolume, value);         break;
            case "number"          :
            case "issueIdentifier" : bd.setStr(bfIssue, value);          break;
            case "pageRange"       : bd.setStr(bfPages, value);          break;
            case "startingPage"    : bd.setStartPage(value);             break;
            case "endingPage"      : bd.setEndPage(value);               break;
            case "url"             : bd.setStr(bfURL, value);            break;
          }
        }
      }
      else if ("xmp".equals(safeStr(prefix)))
      {
        if ("Label".equals(name))
          bd.addStr(bfMisc, value);
      }
      else if ("dc".equals(safeStr(prefix)))
      {
        if ("source".equals(name) && isStringUrl(value))
          bd.setStr(bfURL, value);
      }

      prefixToNameToChild.values().forEach(nameToChild ->
        nameToChild.values().forEach(child ->
          child.extractBibData(bd)));

      elements.forEach(child -> child.extractBibData(bd));
    }

  //---------------------------------------------------------------------------

    private static EntryType parsePrismAggregationType(String paType) { return switch (paType.toLowerCase())
    {
      case "book"       -> etBook;
      case "catalog"    -> etCatalogItem;
      case "feed"       -> etFeedItem;
      case "journal"    -> etJournalArticle;
      case "magazine"   -> etMagazineArticle;
      case "manual"     -> etManual;
      case "newsletter" -> etNewsletterArticle;
      case "pamphlet"   -> etPamphlet;

      default           -> etOther;  // Includes type "other"
    };}
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private PDDocumentInformation docInfo = null;
  private XMPNode xmpRoot = null;

//---------------------------------------------------------------------------

  public PDFBibData(FilePath filePath) throws IOException
  {
    try (PDDocument pdfDoc = Loader.loadPDF(filePath.toFile()))
    {
      setDocInfo(pdfDoc.getDocumentInformation());
      PDMetadata metadata = pdfDoc.getDocumentCatalog().getMetadata();

      if (metadata != null)
      {
        try { setXmpRoot(metadata.toByteArray()); }
        catch (XMPException e)
        {
          errorPopup("An error occurred while parsing XMP data from PDF file: " +
                     nullSwitch(e.getCause(), getThrowableMessage(e), Throwable::getMessage));

          xmpRoot = null;
        }
      }

      if (getStr(bfDOI).length() > 0) return;

      PDFTextStripper pdfStripper = new PDFTextStripper();

      int numPages = pdfDoc.getNumberOfPages();

      parseAndExtractIDs(pdfDoc, pdfStripper, 1, numPages > 60 ? 11 : Math.max(numPages, 11));

      if (getStr(bfDOI).length() > 0) return;

      if (numPages > 11)
        parseAndExtractIDs(pdfDoc, pdfStripper, numPages - 3, numPages);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void parseAndExtractIDs(PDDocument pdfDoc, PDFTextStripper pdfStripper, int startPage, int endPage) throws IOException
  {
    if (startPage < 1) startPage = 1;

    pdfStripper.setStartPage(startPage);
    pdfStripper.setEndPage(endPage);

    String parsedText = pdfStripper.getText(pdfDoc);

    extractDOIandISBNs(parsedText);

    if (getStr(bfDOI).isEmpty())
      extractDOIandISBNs(parsedText.replaceAll("\\h+", ""));  // remove horizontal whitespaces and check again
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setDocInfo(PDDocumentInformation docInfo)
  {
    if ((this.docInfo = docInfo) == null) return;

    extractDOIandISBNs(docInfo.getAuthor  ());
    extractDOIandISBNs(docInfo.getCreator ());
    extractDOIandISBNs(docInfo.getKeywords());
    extractDOIandISBNs(docInfo.getProducer());
    extractDOIandISBNs(docInfo.getSubject ());
    extractDOIandISBNs(docInfo.getTitle   ());
    extractDOIandISBNs(docInfo.getTrapped ());

    docInfo.getMetadataKeys().stream().filter(key -> key.toLowerCase().contains("journaldoi") == false)
                                      .forEachOrdered(key -> extractDOIandISBNs(docInfo.getCustomMetadataValue(key)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setXmpRoot(byte[] byteArray) throws XMPException
  {
    XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(byteArray);

    xmpRoot = new XMPNode(xmpMeta, null, null);

    XMPIterator it = xmpMeta.iterator();

    while (it.hasNext())
    {
      XMPPropertyInfo propInfo = (XMPPropertyInfo) it.next();

      if (propInfo.getPath() != null)
        xmpRoot.addDescendant(propInfo);
    }

    xmpRoot.extractDOIandISBNs(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void populateFromFile()
  {
    if (strNotNullOrBlank(docInfo.getAuthor()))
    {
      authors.clear();
      authors.setOneLiner(docInfo.getAuthor());
    }

    if (strNotNullOrBlank(docInfo.getTitle()))
    {
      String title = docInfo.getTitle();
      if ("untitled".equalsIgnoreCase(title) == false)
        setTitle(title);
    }

    if (strNotNullOrBlank(docInfo.getSubject()))
      addStr(bfMisc, docInfo.getSubject());

    if (xmpRoot != null)
      xmpRoot.extractBibData(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addCsvLines(List<List<String>> csvFile)
  {
    if (xmpRoot != null) xmpRoot.addCsvLines(csvFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static PDFBibData createFromFiles(List<FilePath> filePaths) throws IOException
  {
    List<FilePath> pdfFilePaths = new ArrayList<>(filePaths);

    pdfFilePaths.removeIf(filePath -> (FilePath.isEmpty(filePath) || (filePath.exists() == false) ||
                                       (getMediaType(filePath).toString().contains("pdf") == false)));

    if (pdfFilePaths.isEmpty())
      return null;

    PDFBibData firstPdfBD = null, lastPdfBD = null, goodPdfBD = null;
    List<String> isbns = new ArrayList<>();
    String doi = "";

    for (FilePath pdfFilePath : pdfFilePaths)
    {
      lastPdfBD = new PDFBibData(pdfFilePath);
      if (firstPdfBD == null)
        firstPdfBD = lastPdfBD;

      if (doi.isEmpty())
      {
        doi = safeStr(lastPdfBD.getStr(bfDOI));

        if ((doi.length() > 0) && (goodPdfBD == null))
          goodPdfBD = lastPdfBD;
      }

      List<String> curIsbns = lastPdfBD.getMultiStr(bfISBNs);

      if (curIsbns.isEmpty() == false)
      {
        if (isbns.isEmpty() && (goodPdfBD == null))
          goodPdfBD = lastPdfBD;

        curIsbns.stream().filter(Predicate.not(isbns::contains)).forEachOrdered(isbns::add);
      }
    }

    if (goodPdfBD == null)
      goodPdfBD = firstPdfBD;

    goodPdfBD.populateFromFile();

    doi = goodPdfBD.getStr(bfDOI);

    Iterator<FilePath> it = pdfFilePaths.iterator();

    while (doi.isEmpty() && it.hasNext())
      doi = matchDOI(FilenameUtils.removeExtension(it.next().getNameOnly().toString()));

    goodPdfBD.setStr(bfDOI, doi);

    it = pdfFilePaths.iterator();

    while (isbns.isEmpty() && it.hasNext())
      isbns = matchISBN(it.next().getNameOnly().toString());

    goodPdfBD.setMultiStr(bfISBNs, isbns);

    return goodPdfBD;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
