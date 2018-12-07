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

import org.hypernomicon.bib.BibData.AuthorType;
import org.hypernomicon.model.records.HDT_Work;

public class WorkBibAuthors extends BibAuthors
{
  private final HDT_Work work;
  
  public WorkBibAuthors(HDT_Work work) { this.work = work; }
  
  @Override public boolean isEmpty() { return work.getAuthors().isEmpty(); }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  @Override public void getLists(ArrayList<BibAuthor> authorList, ArrayList<BibAuthor> editorList, ArrayList<BibAuthor> translatorList)
  {
    work.getAuthors().forEach(author ->
    {
      if ((author.getIsEditor() == false) && (author.getIsTrans() == false))
        authorList.add(new BibAuthor(AuthorType.author, author));
      
      if (author.getIsEditor())
        editorList.add(new BibAuthor(AuthorType.editor, author));
      
      if (author.getIsTrans())
        translatorList.add(new BibAuthor(AuthorType.translator, author));
    });
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

}
