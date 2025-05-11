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

package org.hypernomicon.tree;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Glossary;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;

import javafx.fxml.FXML;
import javafx.scene.control.Label;

public class DragConceptDlgCtrlr extends ModalDialog
{
  @FXML private Label lblMove, lblMoveInfo, lblAdd, lblAddInfo;

  private final HDT_Glossary newGlossary;
  private final HDT_Concept sourceChildConcept, newParentConcept;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  DragConceptDlgCtrlr(HDT_Concept childConcept, HDT_Record newParent)
  {
    super("tree/DragConceptDlg", "Copy or Move Record to Destination", true, true);

    sourceChildConcept = childConcept;

    HDT_Glossary oldGlossary = childConcept.glossary.get();

    if (newParent instanceof HDT_Concept parentConcept)
    {
      newParentConcept = parentConcept;
      newGlossary = newParentConcept.glossary.get();
    }
    else
    {
      newParentConcept = null;
      newGlossary = (HDT_Glossary) newParent;
    }

    lblMove.setText("Move definition \"" + childConcept.listName() +
                    "\" from Glossary \"" + oldGlossary.name() + "\" to Glossary \"" + newGlossary.name() + '"');

    lblMoveInfo.setText("In this case, the Concept \"" + childConcept.listName() + "\" will no longer appear in the Glossary \"" +
                        oldGlossary.name() + "\". Its definition in Glossary \"" + newGlossary.name() +
                        "\" will be the same as what it was in Glossary \"" + oldGlossary.name() +
                        "\". Parent/Child relations for this Concept in Glossary \"" + oldGlossary.name() + "\" will be lost.");

    lblAdd.setText("Add an entry (Concept) for \"" + childConcept.listName() + "\" to Glossary \"" + newGlossary.name() + '"');

    lblAddInfo.setText("In this case, the existing entry (Concept) in Glossary \"" + oldGlossary.name() +
                       "\" and its definition will remain intact. The definition for the new entry (Concept) in Glossary \"" + newGlossary.name() +
                       "\" will be blank.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnMoveClick()
  {
    sourceChildConcept.glossary.set(newGlossary);

    List.copyOf(sourceChildConcept.parentConcepts).forEach(sourceChildConcept::removeParent);
    List.copyOf(sourceChildConcept.subConcepts).forEach(subConcept -> subConcept.removeParent(sourceChildConcept));

    try
    {
      if (newParentConcept != null)
        sourceChildConcept.addParentConcept(newParentConcept);
    }
    catch (RelationCycleException e)
    {
      throw newAssertionError(e);
    }
    finally
    {
      okClicked = true;
      stage.close();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnAddClick()
  {
    HDT_Concept targetChildConcept = db.createNewBlankRecord(hdtConcept);

    HDT_ConceptSense sense = null;

    if (sourceChildConcept.term.get().getGlossaries().contains(newGlossary))
      sense = sourceChildConcept.sense.get();

    sourceChildConcept.term.get().concepts.add(targetChildConcept);
    targetChildConcept.glossary.set(newGlossary);
    targetChildConcept.sense.set(sense);

    try
    {
      if (newParentConcept != null)
        targetChildConcept.addParentConcept(newParentConcept);
    }
    catch (RelationCycleException e)
    {
      throw newAssertionError(e);
    }
    finally
    {
      okClicked = true;
      stage.close();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
