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

package org.hypernomicon.view.wrappers;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Record.HDT_DateType;
import org.hypernomicon.view.wrappers.ResultsTable.ResultCellValue;
import org.hypernomicon.model.records.HDT_RecordType;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.HDT_Record.HDT_DateType.*;

import java.time.Instant;
import java.util.HashMap;

public class ResultsRow
{
  private HashMap<Integer, String> customValues = new HashMap<>();  
  
  private HDT_Base record;
  private String cbText;
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ResultsRow(HDT_Base record) { this.record = record; this.cbText = ""; }
  public ResultsRow(String cbText)   { this.record = null;   this.cbText = cbText; }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public  HDT_Base getRecord()                              { return record; }
  public  void setCustomValue(int customCode, String value) { customValues.put(customCode, value); }
  public  String getTagText(Tag tag)                        { return record == null ? "" : record.getResultTextForTag(tag); }
  public  String getCustomText(int customCode)              { return customValues.getOrDefault(customCode, ""); } 
  public  String getRecordID()                              { return record == null ? "" : String.valueOf(record.getID()); }
  public  String getRecordName()                            { return record == null ? "" : record.listName(); }
  public  String getSearchKey()                             { return record == null ? "" : record.getSearchKey(); }
  public  String getSortKey()                               { return record == null ? "" : record.getSortKey(); }  
  public  String getCBText()                                { return record == null ? cbText : record.listName(); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ResultCellValue<Instant> getCreationDateCellValue() { return getDateCellValue(dateTypeCreation); }
  public ResultCellValue<Instant> getModifiedDateCellValue() { return getDateCellValue(dateTypeModified); }
  public ResultCellValue<Instant> getViewDateCellValue()     { return getDateCellValue(dateTypeView); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ResultCellValue<Instant> getDateCellValue(HDT_DateType dateType)
  {
    if (record == null)
      return new ResultCellValue<Instant>("", Instant.MIN);
    
    if (record.getType() == hdtNone)
      return new ResultCellValue<Instant>("", Instant.MIN);
    
    Instant i = null;
    
    switch (dateType)
    {
      case dateTypeCreation: i = record.getCreationDate(); break;         
      case dateTypeModified: i = record.getModifiedDate(); break;
      case dateTypeView:     i = record.getViewDate();     break;
        
      default: break;
    }
    
    return nullSwitch(i, new ResultCellValue<Instant>("", Instant.MIN), j -> new ResultCellValue<Instant>(dateTimeToUserReadableStr(j), j));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getRecordType()
  {
    if (record == null) return "";
    
    HDT_RecordType type = record.getType();
    return type == hdtNone ? "" : db.getTypeName(type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
