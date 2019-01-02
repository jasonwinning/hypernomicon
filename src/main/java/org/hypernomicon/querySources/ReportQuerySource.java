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

package org.hypernomicon.querySources;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.view.wrappers.HyperTableCell;

public abstract class ReportQuerySource implements QuerySource
{
  protected final List<HyperTableCell> list = new ArrayList<>();
  protected final int query;
  protected final HyperTableCell op1, op2, op3;
  protected final HyperDB db;

  private boolean generated = false;
  
  public ReportQuerySource(int query, HyperTableCell op1, HyperTableCell op2) { this(query, op1, op2, null); }
  public ReportQuerySource(int query, HyperTableCell op1)                     { this(query, op1, null, null); }
  public ReportQuerySource(int query)                                         { this(query, null, null, null); }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public ReportQuerySource(int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    this.query = query;
    this.op1 = op1;
    this.op2 = op2;
    this.op3 = op3;
    this.db = HyperDB.db;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public int count()                             { ensureGenerated(); return list.size(); }
  @Override public HDT_Record getRecord(int ndx)           { return null; }
  @Override public boolean containsRecord(HDT_Base record) { return false; }
  @Override public QuerySourceType sourceType()            { return QuerySourceType.QST_report; }
  
  protected void ensureGenerated()                         { if (!generated) { generate(); generated = true; }}
   
  protected abstract void generate();
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
}
