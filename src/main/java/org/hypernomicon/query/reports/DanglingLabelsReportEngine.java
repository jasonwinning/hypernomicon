package org.hypernomicon.query.reports;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_WorkLabel;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.SetMultimap;

import javafx.collections.FXCollections;
import javafx.scene.control.TableView;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class DanglingLabelsReportEngine extends ReportEngine
{

//---------------------------------------------------------------------------

  private final SetMultimap<HDT_WorkLabel, HDT_WorkLabel> matches = LinkedHashMultimap.create();
  private final List<HyperTableRow> rows = new ArrayList<>();
  private HyperTable ht;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableRow> getRows()     { return rows; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void generate(HyperTask task, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    rows.clear();
    matches.clear();

    if (db.isLoaded() == false) return;

    for (HDT_WorkLabel childLabel : db.workLabels)
    {
      if (childLabel.hasHub() == false)
        continue;

      HDT_Hub childHub = childLabel.getHub();
      HDT_Debate childDebate = childHub.getDebate();

      if (childDebate != null)
      {
        for (HDT_Debate parentDebate : childDebate.largerDebates)
        {
          HDT_WorkLabel parentLabel = nullSwitch(parentDebate.getHub(), null, HDT_Hub::getLabel);
          if ((parentLabel != null) && (parentLabel.subLabels.contains(childLabel) == false))
            addMatch(parentLabel, childLabel);
        }

        for (HDT_Position parentPos : childDebate.largerPositions)
        {
          HDT_WorkLabel parentLabel = nullSwitch(parentPos.getHub(), null, HDT_Hub::getLabel);
          if ((parentLabel != null) && (parentLabel.subLabels.contains(childLabel) == false))
            addMatch(parentLabel, childLabel);
        }
      }

      HDT_Position childPos = childHub.getPosition();
      if (childPos != null)
      {
        for (HDT_Debate parentDebate : childPos.largerDebates)
        {
          HDT_WorkLabel parentLabel = nullSwitch(parentDebate.getHub(), null, HDT_Hub::getLabel);
          if ((parentLabel != null) && (parentLabel.subLabels.contains(childLabel) == false))
            addMatch(parentLabel, childLabel);
        }

        for (HDT_Position parentPos : childPos.largerPositions)
        {
          HDT_WorkLabel parentLabel = nullSwitch(parentPos.getHub(), null, HDT_Hub::getLabel);
          if ((parentLabel != null) && (parentLabel.subLabels.contains(childLabel) == false))
            addMatch(parentLabel, childLabel);
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addMatch(HDT_WorkLabel parentLabel, HDT_WorkLabel childLabel)
  {
    if (matches.containsEntry(parentLabel, childLabel))
      return;

    matches.put(parentLabel, childLabel);

    HyperTableCell cell = new HyperTableCell(parentLabel, "Parent: \"" + parentLabel.name() + "\" Child: \"" + childLabel.name() + '"');
    rows.add(new HyperTableRow(FXCollections.observableArrayList(cell), ht));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override HyperTable prepTable(TableView<HyperTableRow> tv)
  {
    this.tv = tv;

    addCol("Parent and child label", 600);

    ht = new HyperTable(tv, 0, false, "");

    ht.addLabelCol(hdtWorkLabel);

    return ht;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
