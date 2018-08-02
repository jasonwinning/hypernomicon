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

package org.hypernomicon.view.workMerge;

import org.hypernomicon.bib.BibData.BibFieldEnum;

public class BibFieldRow
{
  public BibFieldEnum bibFieldEnum = null;
  public MergeWorksSLController slCtrlr = null;
  public MergeWorksMLController mlCtrlr = null;
  public MergeWorksCBController cbCtrlr = null;
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public BibFieldRow(BibFieldEnum bibField, MergeWorksSLController slCtrlr)
  {
    this.bibFieldEnum = bibField;
    this.slCtrlr = slCtrlr;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public BibFieldRow(MergeWorksCBController ctrlr)
  {
    this.cbCtrlr = ctrlr;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public BibFieldRow(BibFieldEnum bibField, MergeWorksMLController mlCtrlr)
  {
    this.bibFieldEnum = bibField;
    this.mlCtrlr = mlCtrlr;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}