/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.bib;

import static org.hypernomicon.bib.BibData.BibFieldEnum.*;
import static org.hypernomicon.util.Util.*;

import org.apache.pdfbox.pdmodel.PDDocumentInformation;

public class PdfMetadata
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private PDDocumentInformation docInfo = null;
  private XMPNode xmpRoot = null;
  public final BibDataStandalone bd = new BibDataStandalone();

  public XMPNode getXmpRoot()               { return xmpRoot; }
  public PDDocumentInformation getDocInfo() { return docInfo; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setDocInfo(PDDocumentInformation docInfo)
  {
    this.docInfo = docInfo;
    if (docInfo == null) return;

    bd.extractDOIandISBNs(docInfo.getAuthor());
    bd.extractDOIandISBNs(docInfo.getCreator());
    bd.extractDOIandISBNs(docInfo.getKeywords());
    bd.extractDOIandISBNs(docInfo.getProducer());
    bd.extractDOIandISBNs(docInfo.getSubject());
    bd.extractDOIandISBNs(docInfo.getTitle());
    bd.extractDOIandISBNs(docInfo.getTrapped());

    docInfo.getMetadataKeys().forEach(key -> {
      if (key.toLowerCase().contains("journaldoi") == false)
        bd.extractDOIandISBNs(docInfo.getCustomMetadataValue(key)); });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setXmpRoot(XMPNode xmpRoot)
  {
    this.xmpRoot = xmpRoot;

    if (xmpRoot != null)
      xmpRoot.extractDOIandISBNs(bd);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public BibData extractBibData()
  {
    if (safeStr(docInfo.getAuthor()).length() > 0)
    {
      bd.getAuthors().clear();
      BibAuthorsStandalone.class.cast(bd.getAuthors()).setOneLiner(docInfo.getAuthor());
    }

    if (safeStr(docInfo.getTitle()).length() > 0)
      bd.setTitle(docInfo.getTitle());

    if (safeStr(docInfo.getSubject()).length() > 0)
      bd.addStr(bfMisc, docInfo.getSubject());

    if (xmpRoot != null)
      xmpRoot.extractBibData(bd);

    return bd;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}