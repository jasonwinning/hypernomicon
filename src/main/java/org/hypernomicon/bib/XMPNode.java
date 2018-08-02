/*
 * Copyright 2015-2018 Jason Winning
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPIterator;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.XMPMetaFactory;
import com.adobe.internal.xmp.properties.XMPPropertyInfo;

import org.hypernomicon.model.PersonName;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.bib.BibData.*;
import static org.hypernomicon.bib.BibData.BibFieldEnum.*;
import static org.hypernomicon.bib.BibData.YearType.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// See the following for some library code that might be useful:
// https://svn.apache.org/viewvc/pdfbox/trunk/examples/src/main/java/org/apache/pdfbox/examples/pdmodel/AddMetadataFromDocInfo.java?view=markup
// Or google "import org.apache.xmpbox.schema"

public class XMPNode
{
  private XMPNode parent = null;
  private XMPMeta xmpMeta = null;
  private LinkedHashMap<String, LinkedHashMap<String, XMPNode>> prefixToNameToChild = new LinkedHashMap<>();
  private ArrayList<XMPNode> elements = new ArrayList<>();
  private String ns = null, prefix = null, name = null, path = null, value = null;
  private int arrayNdx = -1;
  private XMPPropertyInfo propInfo = null;

  public String getNamespace()         { return ns; }
  public XMPPropertyInfo getPropInfo() { return propInfo; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static XMPNode createRoot(byte[] byteArray) throws XMPException
  {
    XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(byteArray);

    XMPNode root = new XMPNode(xmpMeta, null, null);
    
    XMPIterator it = xmpMeta.iterator();
    
    while (it.hasNext())
    {
      XMPPropertyInfo propInfo = (XMPPropertyInfo) it.next();
      
      if (propInfo.getPath() != null)
        root.addDescendant(propInfo);     
    }
    
    return root;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class PathParts
  {
    public String prefix = null, name = null;
    int arrayNdx = -1;
    
    public PathParts(String str)
    {
      if (str == null) return;
      if (str.length() == 0) return;
      
      if (str.startsWith("["))
      {
        arrayNdx = parseInt(str.substring(1, str.indexOf(']')), 0) - 1;
        return;
      }
      
      int ndx = str.indexOf(":");
      prefix = str.substring(0, ndx);
      name = str.substring(ndx + 1);
      
      ndx = name.indexOf("[");
      if (ndx >= 0)
      {
        arrayNdx = parseInt(name.substring(ndx + 1, name.indexOf(']')), 0) - 1;
        name = name.substring(0, ndx);
      }      
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public XMPNode(XMPMeta xmpMeta, XMPNode parent, XMPPropertyInfo propInfo)
  {
    this.xmpMeta = xmpMeta;
    this.parent = parent;
    this.propInfo = propInfo;

    if (propInfo != null)
    {
      this.ns = propInfo.getNamespace();
      
      if (ns == null)
        ns = parent.getNamespace();
      
      this.path = propInfo.getPath();
      this.value = propInfo.getValue();
      
      if (path != null)
      {
        int ndx = path.lastIndexOf("/");
        String subPath = path.substring(ndx + 1);

        PathParts parts = new PathParts(subPath);
        
        prefix = parts.prefix;
        name = parts.name;
        arrayNdx = parts.arrayNdx;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addDescendant(XMPPropertyInfo targetInfo)
  {
    String subPath = targetInfo.getPath().substring(safeStr(path).length());
    if (subPath.startsWith("/"))
      subPath = subPath.substring(1);
    
    int ndx = subPath.indexOf("/");
    if (ndx >= 0)
      subPath = subPath.substring(0, ndx);

    PathParts parts = new PathParts(subPath);
    
    if (subPath.contains(":"))
    {
      LinkedHashMap<String, XMPNode> nameToChild;
      
      if (prefixToNameToChild.containsKey(parts.prefix) == false)
      {
        nameToChild = new LinkedHashMap<>();
        prefixToNameToChild.put(parts.prefix, nameToChild);
      }
      else
        nameToChild = prefixToNameToChild.get(parts.prefix);
      
      if (nameToChild.containsKey(parts.name))
      {
        nameToChild.get(parts.name).addDescendant(targetInfo);
        return;
      }
      
      XMPNode child = new XMPNode(xmpMeta, this, targetInfo);
      nameToChild.put(parts.name, child);
      return;      
    }
    
    if (elements.size() > parts.arrayNdx)
    {
      elements.get(parts.arrayNdx).addDescendant(targetInfo);
      return;
    }
    
    XMPNode child = new XMPNode(xmpMeta, this, targetInfo);
    elements.add(child);
    return;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String escape(String str) { return str.replace(",", "@&$"); }
  
  public void addCsvLines(ArrayList<String> csvFile)
  {
    String line = "";
    
    if (safeStr(value).length() > 0)
    {
      if (arrayNdx >= 0)
        line = "[" + (arrayNdx + 1) + "]";
      
      line = line + escape(value) + "," + getCsvPath();
    }
    
    line = convertToSingleLine(line);
    line = line.replace("\"", "");
    
    if (line.length() > 0)
      if (csvFile.contains(line) == false)
        csvFile.add(line);
    
    prefixToNameToChild.values().forEach(nameToChild ->
      nameToChild.values().forEach(child ->
        child.addCsvLines(csvFile)));
    
    elements.forEach(child -> child.addCsvLines(csvFile));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getCsvPath()
  {
    String line = "";
    
    if (parent != null)
      line = parent.getCsvPath();
    
    if (safeStr(name).length() > 0)
    {
      if (line.length() > 0)
        line = line + "," + escape(prefix) + "," + escape(name);
      else
        line = escape(prefix) + "," + escape(name);
    }
    
    return line;
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void extractDOIandISBNs(BibData bd)
  {
    if (safeStr(name).toLowerCase().contains("journaldoi") == false)
      if (safeStr(value).length() > 0)
        BibUtils.extractDOIandISBNs(value, bd);
    
    prefixToNameToChild.values().forEach(nameToChild ->
      nameToChild.values().forEach(child ->
        child.extractDOIandISBNs(bd)));
    
    elements.forEach(child -> child.extractDOIandISBNs(bd));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void extractBibData(BibDataStandalone bd)
  {
    if (elements.isEmpty() == false)
    {
      if (prefix.equals("dc")) 
      {
        if (name.equals("creator"))
        {
          bd.getAuthors().clear();
          
          elements.forEach(child -> bd.getAuthors().add(AuthorType.author, new PersonName(child.value)));
        }
        else if (name.equals("title"))
        {
          bd.setMultiStr(bfTitle, Collections.emptyList());
          
          elements.forEach(child -> bd.addStr(bfTitle, child.value));
        }
        else if (name.equals("description"))
        {
          elements.forEach(child -> bd.addStr(bfMisc, child.value));
        }
      }
      else if (safeStr(prefix).startsWith("prism"))
      {
        if (elements.size() > 0)
        {
          YearType yt = YearType.getByDesc(name);
          
          if (yt != ytUnknown)
            bd.setYear(elements.get(0).value, yt);
        }
      }
    }
    
    if (safeStr(prefix).startsWith("prism"))
    {
      YearType yt = YearType.getByDesc(name);
      
      if (yt != ytUnknown)
        bd.setYear(value, yt);
      else
      {
        switch (name)
        {
          case "aggregationType" : bd.setEntryType(BibUtils.parsePrismAggregationType(value)); break;
          case "issn" : bd.addStr(bfISSNs, value); break;
        }
      }
    }
    else if (safeStr(prefix).equals("xmp"))
    {
      if (name.equals("Label"))
        bd.addStr(bfMisc, value);
    }

    prefixToNameToChild.values().forEach(nameToChild ->
      nameToChild.values().forEach(child ->
        child.extractBibData(bd)));
    
    elements.forEach(child -> child.extractBibData(bd));
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
