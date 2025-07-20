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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.List;
import java.util.Objects;
import java.util.stream.Stream;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.authors.RecordAuthors;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.KeyWork;

//---------------------------------------------------------------------------

public class HDT_WorkLabel extends HDT_RecordWithMainText
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final List<HDT_WorkLabel> parentLabels, subLabels;

//---------------------------------------------------------------------------

  public HDT_WorkLabel(RecordState xmlState, DatasetAccessor<HDT_WorkLabel> dataset)
  {
    super(xmlState, dataset);

    parentLabels = getObjList (rtParentLabelOfLabel);
    subLabels    = getSubjList(rtParentLabelOfLabel);
  }

//---------------------------------------------------------------------------

  @Override public String listName()          { return name(); }
  @Override public String getCBText()         { return extendedText(); }
  @Override public final boolean isUnitable() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a stream of the works and misc. files having this label.
   * <p>
   * Note: the stream only includes the works/files for the current label, not its sub-labels.
   * @return The stream
   */
  public Stream<HDT_RecordWithAuthors<? extends RecordAuthors>> worksAndMiscFilesStream()
  {
    return getMainText().getKeyWorksUnmod().stream().map(KeyWork::getRecord)
                                                    .filter(Objects::nonNull)
                                                    .map(record -> (HDT_RecordWithAuthors<? extends RecordAuthors>) record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String extendedText()
  {
    if (parentLabels.size() > 0)
    {
      if (parentLabels.get(0).getID() == 1) return name();
      String parentText = parentLabels.get(0).extendedText();
      if (parentText.length() > 0)
        return parentText + " / " + name();
    }

    return name();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
