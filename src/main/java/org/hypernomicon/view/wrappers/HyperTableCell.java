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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_Record.makeSortKeyByType;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Work;

//---------------------------------------------------------------------------  

public final class HyperTableCell implements Comparable <HyperTableCell>, Cloneable
{
  public static enum HyperCellSortMethod
  {
    hsmStandard,
    hsmTextSimple,
    hsmNumeric,
    hsmLast,
    hsmWork
  }
  
  public static final HyperTableCell trueCell = new HyperTableCell(1, "", hdtNone);
  public static final HyperTableCell falseCell = new HyperTableCell(0, "", hdtNone);
  
  private int id;
  private String text;
  private HDT_RecordType type;
  private HyperCellSortMethod sortMethod = hsmStandard;

  public int getID()              { return id; }
  public String getText()         { return text; }
  public HDT_RecordType getType() { return type; }
  
//---------------------------------------------------------------------------  
  
  public HyperTableCell(int newID, String newText, HDT_RecordType newType)
  {
    this(newID, newText, newType, hsmStandard);
  }
  
  public HyperTableCell(HDT_Base record)
  {
    this(record.getID(), "", record.getType(), hsmStandard);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public HyperTableCell(int newID, String newText, HDT_RecordType newType, HyperCellSortMethod newSortMethod)
  {
    id = newID;
    text = newText;
    type = newType;
    sortMethod = newSortMethod;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + id;
    result = prime * result + ((text == null) ? 0 : text.hashCode());
    result = prime * result + ((type == null) ? 0 : type.hashCode());
    return result;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    
    HyperTableCell other = (HyperTableCell) obj;
    
    if (type != other.type) return false;
    if (id != other.id) return false;
    if (text == null)
    {
      if (other.text != null) return false;
    }
    else if (!text.equals(other.text)) return false;
    
    return true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell clone() 
  { try { return (HyperTableCell) super.clone(); } catch (CloneNotSupportedException ex) { throw new RuntimeException(ex); }}

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public HyperTableCell getCopyWithID(int newID)
  {
    HyperTableCell newCell = clone();
    newCell.id = newID;
    return newCell;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public static HyperTableCell simpleSortValue(HyperTableCell cell)
  {
    HyperTableCell newCell = cell.clone();
    newCell.sortMethod = hsmTextSimple;
    return newCell;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public static int getCellID(HyperTableCell cell)
  {
    if (cell == null)
      return -1;
       
    return cell.id;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public static String getCellText(HyperTableCell cell)
  {
    if (cell == null)
      return "";
    
    return safeStr(cell.text);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public static HDT_RecordType getCellType(HyperTableCell cell)
  {
    if (cell == null)
      return hdtNone;
    
    if (cell.type == null)
      return hdtNone;
    
    return cell.type;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  @Override public int compareTo(HyperTableCell otherCell)
  {
    String thisKey = "", otherKey = "";
    
    if (sortMethod == hsmLast)
      return Integer.MAX_VALUE;
    else if (otherCell.sortMethod == hsmLast)
      return Integer.MIN_VALUE + 1;
    
    if (sortMethod == hsmTextSimple)
      return text.compareTo(otherCell.text);
    else if (sortMethod == hsmNumeric)
    {
      return parseInt(text, Integer.MAX_VALUE) - parseInt(HyperTableCell.getCellText(otherCell), Integer.MAX_VALUE);
    }     
    else if (sortMethod == hsmWork)
    {
      HDT_Work thisWork = (HDT_Work) getRecord(this), otherWork = (HDT_Work) getRecord(otherCell);
            
      int numAuthors = Math.max(thisWork.getAuthors().size(), otherWork.getAuthors().size());
      int ndx, cResult;
      
      for (ndx = 0; ndx < numAuthors; ndx++)
      {
        if ((ndx >= thisWork.getAuthors().size()) || (ndx >= otherWork.getAuthors().size()))
          return thisWork.getAuthors().size() - otherWork.getAuthors().size();
        
        cResult = thisWork.getAuthors().get(ndx).getSortKey().compareTo(otherWork.getAuthors().get(ndx).getSortKey());
        
        if (cResult != 0) return cResult;
      }
      
      cResult = thisWork.getYear().compareTo(otherWork.getYear());
      if (cResult != 0) return cResult;
      
      return thisWork.getSortKey().compareTo(otherWork.getSortKey());
    }
        
    if (id > 0)
      if (type != null)
        if (type != hdtNone)
          thisKey = db.records(type).getByID(id).getSortKey();
    
    if (thisKey.length() == 0) thisKey = makeSortKeyByType(text, type);
    
    if (otherCell.id > 0)
      if (otherCell.type != null)
        if (otherCell.type != hdtNone)
          otherKey = db.records(otherCell.type).getByID(otherCell.id).getSortKey();
    
    if (otherKey.length() == 0) otherKey = makeSortKeyByType(otherCell.text, otherCell.type);
    
    return thisKey.compareTo(otherKey);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public static HDT_Base getRecord(HyperTableCell cell)
  {
    int id = getCellID(cell);
    if (id < 1) return null;
    
    HDT_RecordType type = getCellType(cell);
    if (type == hdtNone) return null;
    
    return db.records(type).getByID(id);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public static HyperTableCell fromBoolean(boolean boolVal)
  {
    return boolVal ? trueCell : falseCell;
  }
 
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
