/*
 * Copyright 2015-2026 Jason Winning
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

  /**
   * {@inheritDoc}
   */
  @Override public String defaultChoiceText() { return extendedText(); }
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
    return keyWorksStream().map(KeyWork::getRecord)
                           .filter(Objects::nonNull)
                           .map(record -> (HDT_RecordWithAuthors<? extends RecordAuthors>) record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the name of this label prefixed with its ancestor hierarchy,
   * excluding the root "All Labels" record (ID=1).
   * @return the extended text with " / " separators
   */
  public String extendedText() { return extendedText(false); }

  /**
   * Returns the name of this label prefixed with its ancestor hierarchy.
   * @param includeRoot whether to include the root "All Labels" record (ID=1)
   * @return the extended text with " / " separators
   */
  public String extendedText(boolean includeRoot)
  {
    if (parentLabels.isEmpty())
      return name();

    HDT_WorkLabel parent = parentLabels.getFirst();

    if (parent.getID() == 1)
      return includeRoot ? (parent.name() + " / " + name()) : name();

    String parentText = parent.extendedText(includeRoot);

    return parentText.isEmpty() ? name() : (parentText + " / " + name());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
