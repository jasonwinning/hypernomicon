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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjPointer;

public class HDT_Concept extends HDT_RecordWithConnector
{
  public final HyperSubjPointer<HDT_Term, HDT_Concept> term;
  public final HyperObjPointer<HDT_Concept, HDT_Glossary> glossary;
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public HDT_Concept(HDT_RecordState xmlState, HyperDataset<HDT_Concept> dataset)
  {
    super(xmlState, dataset, tagName);
    
    term = getSubjPointer(rtConceptOfTerm);
    glossary = getObjPointer(rtGlossaryOfConcept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  @Override public String name()                        { return term.get().name(); }
  @Override public String getSearchKey()                { return term.get().getSearchKey(); }
  @Override public List<SearchKeyword> getSearchKeys()  { return db.getKeysByRecord(term.get()); }
  @Override public String getCBText()                   { return getExtendedName(); }
  @Override public String getXMLObjectName()            { return getExtendedName(); }
  @Override public String getNameEngChar()              { return term.get().getNameEngChar(); }
  @Override public String getFirstActiveKeyWord()       { return term.get().getFirstActiveKeyWord(); }
  @Override public void setName(String str)             { term.get().setName(str); }  
  @Override public String listName()                    { return name(); }
  @Override public HDT_RecordType getType()             { return hdtConcept; }
  @Override public boolean isUnitable()                 { return true; }  

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setNameInternal(String str, boolean update) 
  { 
    if (term.isNotNull())
      term.get().setNameInternal(str, update);
    
    super.setNameInternal(str, update);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getExtendedName()
  {
    if (glossary.isNull())
      return name();
    
    if (glossary.get().getID() == 1)
      if (term.get().concepts.size() == 1)
        return name();
    
    return name() + " (" + glossary.get().name() + ")";
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setSearchKey(String newKey) throws SearchKeyException       
  { 
    if (term.isNotNull())
      term.get().setSearchKey(newKey); 
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setSearchKey(String newKey, boolean noMod) throws SearchKeyException 
  { 
    if (term.isNotNull())
      term.get().setSearchKey(newKey, noMod); 
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
