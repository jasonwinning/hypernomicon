/*
 * Copyright 2015-2024 Jason Winning
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

package org.hypernomicon.dialogs.workMerge;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.util.UIUtil.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hypernomicon.App;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.zotero.ZoteroItem;
import org.hypernomicon.dialogs.workMerge.BibFieldCtrlr.*;
import org.hypernomicon.model.records.HDT_Work;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Insets;
import javafx.scene.control.Hyperlink;
import javafx.scene.control.Toggle;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;

//---------------------------------------------------------------------------

public abstract class BibFieldRow<BibFieldCtrlr_T extends BibFieldCtrlr>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final AnchorPane ap;
  final BibFieldEnum bibFieldEnum;
  final List<BibFieldCtrlr_T> ctrlrList;

  final AnchorPane getAnchorPane() { return ap; }

  protected abstract void mergeInto(BibData mergedBD);
  protected abstract Double getHeight();
  protected abstract BibFieldCtrlr_T addCtrlr(BibData bibData, HDT_Work destWork) throws IOException;

  private static final double MULTI_LINE_HEIGHT = 150.0,
                              AUTHORS_HEIGHT = 200.0,
                              SINGLE_LINE_HEIGHT = 58.0,
                              SINGLE_LINE_HEIGHT_NO_LABEL = 30.0;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  BibFieldRow(BibFieldEnum bibFieldEnum, List<BibData> bibDataList, HDT_Work destWork) throws IOException
  {
    this.bibFieldEnum = bibFieldEnum;

    ctrlrList = new ArrayList<>();

    SimpleObjectProperty<GridPane> gridPaneProp = new SimpleObjectProperty<>();
    ap = initAnchorPane(gridPaneProp);
    GridPane gp = gridPaneProp.get();

    for (int ndx = 0; ndx < bibDataList.size(); ndx++)
    {
      BibData bibData = bibDataList.get(ndx);

      BibFieldCtrlr_T ctrlr = addCtrlr(bibData, destWork);

      ctrlrList.add(ctrlr);

      if (bibFieldEnum == bfTitle)
      {
        gp.getRowConstraints().add(new RowConstraints(SINGLE_LINE_HEIGHT_NO_LABEL, SINGLE_LINE_HEIGHT_NO_LABEL, SINGLE_LINE_HEIGHT_NO_LABEL));

        GridPane.setRowIndex(ctrlr.ap, ndx + 1);
      }
      else
      {
        GridPane.setColumnIndex(ctrlr.ap, ndx);

        ColumnConstraints cc = new ColumnConstraints();
        cc.setHgrow(Priority.SOMETIMES);

        if (ndx < (bibDataList.size() - 1))
          cc.setPercentWidth(100.0 / bibDataList.size());

        gp.getColumnConstraints().add(cc);
      }

      addToParent(ctrlr.ap, gp);
      GridPane.setMargin(ctrlr.ap, new Insets(0, 4, 4, 4));

      ctrlr.setLabelVisible(ndx == 0);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Initializes the AnchorPane and GridPane for this row.
   * @param gridPaneProp Mutable value used to return the GridPane
   * @return The AnchorPane
   * @throws IOException if unable to load FXML
   */
  AnchorPane initAnchorPane(Property<GridPane> gridPaneProp) throws IOException
  {
    GridPane gp = new GridPane();
    gridPaneProp.setValue(gp);

    setAnchors(gp, 0.0, 0.0, 0.0, 0.0);
    AnchorPane newAP = new AnchorPane(gp);

    GridPane.setMargin(newAP, new Insets(0, 4, 4, 4));

    return newAP;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <BibFieldRow_T extends BibFieldRow<BibFieldCtrlr_T>, BibFieldCtrlr_T extends BibFieldCtrlr>
    BibFieldRow_T create(BibFieldEnum bibFieldEnum, List<BibData> bibDataList, HDT_Work destWork) throws IOException
  {
    return switch (bibFieldEnum)
    {
      case bfAuthors   -> (BibFieldRow_T) new AuthorsRow(bibDataList, destWork);
      case bfEntryType -> (BibFieldRow_T) new EntryTypeRow(bibDataList);
      case bfWorkType  -> (BibFieldRow_T) new WorkTypeRow(bibDataList);
      case bfTitle     -> (BibFieldRow_T) new TitleRow(bibDataList);
      case bfDate      -> (BibFieldRow_T) new DateRow(bibDataList);

      case bfISBNs,
           bfISSNs     -> (BibFieldRow_T) new MultiLineCheckBoxRow(bibFieldEnum, bibDataList);

      case bfTranslators,
           bfEditors -> throw new UnsupportedOperationException("Unimplemented case: " + bibFieldEnum);

      default ->

        bibFieldEnum.isMultiLine() ?
          (BibFieldRow_T) new ToggleRow<MultiLineCtrlr>(bibFieldEnum, bibDataList)
        :
          (BibFieldRow_T) new ToggleRow<SingleLineCtrlr>(bibFieldEnum, bibDataList);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static final class MultiLineCheckBoxRow extends BibFieldRow<MultiLineCheckBoxCtrlr>
  {
    private MultiLineCheckBoxRow(BibFieldEnum bibFieldEnum, List<BibData> bibDataList) throws IOException
    {
      super(bibFieldEnum, bibDataList, null);
    }

    @Override protected void mergeInto(BibData mergedBD)
    {
      mergedBD.setMultiStr(bibFieldEnum, ctrlrList.stream().flatMap(MultiLineCheckBoxCtrlr::getLines).toList());
    }

    @Override protected MultiLineCheckBoxCtrlr addCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      return new MultiLineCheckBoxCtrlr(bibFieldEnum, bibData);
    }

    @Override protected Double getHeight() { return MULTI_LINE_HEIGHT; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class ToggleRow<ToggleCtrlr_T extends ToggleCtrlr> extends BibFieldRow<ToggleCtrlr_T>
  {
    private ToggleGroup toggleGroup;               // toggleGroup and toggleMap can't be final because they need to get initialized before the
    private Map<Toggle, ToggleCtrlr_T> toggleMap;  // constructor is called; the super constructor calls addCtrlr and they need to be initialized then

    ToggleRow(BibFieldEnum bibFieldEnum, List<BibData> bibDataList) throws IOException
    {
      this(bibFieldEnum, bibDataList, null);
    }

    ToggleRow(BibFieldEnum bibFieldEnum, List<BibData> bibDataList, HDT_Work destWork) throws IOException
    {
      super(bibFieldEnum, bibDataList, destWork);

      for (ToggleCtrlr_T ctrlr : ctrlrList)
      {
        if (ctrlr.sourceNotEmpty())
        {
          ctrlr.getToggle().setSelected(true);
          return;
        }
      }

      ctrlrList.get(0).getToggle().setSelected(true);
    }

    @Override protected void mergeInto(BibData mergedBD) { selectedCtrlr().mergeInto(mergedBD); }

    @Override protected final ToggleCtrlr_T addCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      if (toggleGroup == null)
      {
        toggleGroup = new ToggleGroup();
        toggleMap = new HashMap<>();
      }

      ToggleCtrlr_T ctrlr = createCtrlr(bibData, destWork);

      ctrlr.getToggle().setToggleGroup(toggleGroup);
      toggleMap.put(ctrlr.getToggle(), ctrlr);

      return ctrlr;
    }

    @SuppressWarnings({ "unchecked", "unused" })
    protected ToggleCtrlr_T createCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      return bibFieldEnum.isMultiLine() ?
          (ToggleCtrlr_T) new MultiLineCtrlr(bibFieldEnum, bibData)
        :
          (ToggleCtrlr_T) new SingleLineCtrlr(bibFieldEnum, bibData);
    }

    ToggleCtrlr_T selectedCtrlr() { return toggleMap.get(toggleGroup.getSelectedToggle()); }

    @Override protected Double getHeight() { return bibFieldEnum.isMultiLine() ? MULTI_LINE_HEIGHT : SINGLE_LINE_HEIGHT; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class TitleRow extends ToggleRow<TitleCtrlr>
  {
    @FXML private GridPane gpTitle;
    @FXML private Hyperlink hlFixCase;

    private TitleRow(List<BibData> bibDataList) throws IOException
    {
      super(bfTitle, bibDataList);

      hlFixCase.setOnAction(event -> selectedCtrlr().fixCase());
    }

    @Override protected TitleCtrlr createCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      return new TitleCtrlr(bibData);
    }

    @Override protected AnchorPane initAnchorPane(Property<GridPane> gridPaneProp) throws IOException
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource("dialogs/workMerge/TitleRowCtrl.fxml"), null, null, klass -> this);

      AnchorPane newAP = loader.load();
      gridPaneProp.setValue(gpTitle);

      return newAP;
    }

    @Override protected Double getHeight() { return null; }

    public void focus()
    {
      safeFocus(ctrlrList.get(0).getToggle());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static final class AuthorsRow extends ToggleRow<AuthorsCtrlr>
  {
    private AuthorsRow(List<BibData> bibDataList, HDT_Work destWork) throws IOException
    {
      super(bfAuthors, bibDataList, destWork);
    }

    @Override protected AuthorsCtrlr createCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      return new AuthorsCtrlr(bibData, destWork);
    }

    @Override protected Double getHeight() { return AUTHORS_HEIGHT; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class WorkTypeRow extends ToggleRow<WorkTypeCtrlr>
  {
    private WorkTypeRow(List<BibData> bibDataList) throws IOException
    {
      super(bfWorkType, bibDataList);
    }

    @Override protected WorkTypeCtrlr createCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      return new WorkTypeCtrlr(bibData);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class EntryTypeRow extends ToggleRow<EntryTypeCtrlr>
  {
    private EntryTypeRow(List<BibData> bibDataList) throws IOException
    {
      super(bfEntryType, bibDataList);
    }

    @Override protected EntryTypeCtrlr createCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      return new EntryTypeCtrlr(bibData);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static final class DateRow extends ToggleRow<DateCtrlr>
  {
    private DateRow(List<BibData> bibDataList) throws IOException
    {
      super(bfDate, bibDataList);
    }

    @Override protected DateCtrlr createCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      return (bibData instanceof ZoteroItem zoteroItem) && (zoteroItem.linkedToWork() == false) ?
        new RawDateCtrlr(zoteroItem)
      :
        new StructuredDateCtrlr(bibData);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
