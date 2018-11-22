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

package org.hypernomicon.querySources;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Set;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

public class CombinedUnfilteredQuerySource implements QuerySource
{
  private Set<HDT_RecordType> types;
  private int firstNdxThisType = 0, lastNdxThisType = 0, total = 0;
  HDT_RecordType lastType = hdtNone;
  private EnumMap<HDT_RecordType, Integer> firstNdxForType = new EnumMap<>(HDT_RecordType.class);
  private EnumMap<HDT_RecordType, Integer> lastNdxForType = new EnumMap<>(HDT_RecordType.class);
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public CombinedUnfilteredQuerySource()
  {
    types = EnumSet.allOf(HDT_RecordType.class);
    types.remove(hdtNone);
    types.remove(hdtAuxiliary);
    types.remove(hdtHub);
    
    for (HDT_RecordType type : types)
    {
      firstNdxForType.put(type, total);
      total += db.records(type).size();
      lastNdxForType.put(type, total - 1);
    }
  }
 
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public CombinedUnfilteredQuerySource(EnumSet<HDT_RecordType> types)
  {
    this.types = types;
    
    types.forEach(type ->
    {
      firstNdxForType.put(type, total);
      total += db.records(type).size();
      lastNdxForType.put(type, total - 1);
    });

  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public int count()                             { return total; }
  @Override public QuerySourceType sourceType()            { return QuerySourceType.QST_combinedUnfilteredRecords; }
  @Override public boolean containsRecord(HDT_Base record) { return types.contains(record.getType()); }
  @Override public HyperTableCell getCell(int ndx)         { return nullSwitch(getRecord(ndx), null, HyperTableCell::new); }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public HDT_Base getRecord(int ndx)
  {
    if ((lastType != hdtNone) && (ndx >= firstNdxThisType) && (ndx <= lastNdxThisType))
      return db.records(lastType).getByIDNdx(ndx - firstNdxThisType);
    
    for (HDT_RecordType type : types)
    {
      firstNdxThisType = firstNdxForType.get(type);
      lastNdxThisType = lastNdxForType.get(type);
      
      if ((ndx >= firstNdxThisType) && (ndx <= lastNdxThisType))
      {
        lastType = type;
        return db.records(lastType).getByIDNdx(ndx - firstNdxThisType);
      }
    }
    
    return null;  // this should never happen
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

}
