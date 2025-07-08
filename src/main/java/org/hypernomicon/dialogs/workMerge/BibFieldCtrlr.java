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

package org.hypernomicon.dialogs.workMerge;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.dialogs.WorkDlgCtrlr.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;
import java.time.Month;
import java.util.List;
import java.util.stream.Stream;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.App;
import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.BibManager;
import org.hypernomicon.bib.data.*;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.zotero.ZoteroItem;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.*;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public abstract class BibFieldCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final AnchorPane ap;
  final BibData bibData;
  final BibFieldEnum bibFieldEnum;

  BibFieldCtrlr(String fxmlName, BibFieldEnum bibFieldEnum, BibData bibData) throws IOException
  {
    FXMLLoader loader = new FXMLLoader(App.class.getResource("dialogs/workMerge/" + fxmlName + ".fxml"), null, null, klass -> this);

    ap = loader.load();

    this.bibFieldEnum = bibFieldEnum;
    this.bibData = bibData;
  }

  public boolean sourceNotEmpty() { return bibData.fieldNotEmpty(bibFieldEnum); }

  protected abstract void mergeInto(BibData mergedBD);
  public abstract void setLabelVisible(boolean value);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class MultiLineCheckBoxCtrlr extends BibFieldCtrlr
  {
    @FXML private Label label;
    @FXML private CheckBox checkBox;
    @FXML private TextArea ta;

    MultiLineCheckBoxCtrlr(BibFieldEnum bibFieldEnum, BibData bibData) throws IOException
    {
      super("MultiLineCheckBoxCtrl", bibFieldEnum, bibData);

      label.setText(bibFieldEnum.getUserFriendlyName());

      if (bibData.fieldNotEmpty(bibFieldEnum))
        ta.setText(strListToStr(bibData.getMultiStr(bibFieldEnum), true));
    }

    Stream<String> getLines() { return checkBox.isSelected() ? convertMultiLineStrToStrList(ta.getText(), true).stream() : Stream.empty(); }

    @Override public void setLabelVisible(boolean value) { label.setVisible(value); }

    @Override protected void mergeInto(BibData mergedBD) { }  // Merging is done by the row object
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static abstract class ToggleCtrlr extends BibFieldCtrlr
  {
    ToggleCtrlr(String fxmlName, BibFieldEnum bibFieldEnum, BibData bibData) throws IOException
    {
      super(fxmlName, bibFieldEnum, bibData);
    }

    public abstract Toggle getToggle();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class TitleCtrlr extends ToggleCtrlr
  {
    @FXML private Label label;
    @FXML private RadioButton radioBtn;
    @FXML private TextField tf;

    private final MutableBoolean alreadyChangingTitle;

    TitleCtrlr(BibData bibData) throws IOException
    {
      super("SingleLineCtrl", bfTitle, bibData);

      alreadyChangingTitle = new MutableBoolean(false);

      tf.setText(bibData.getStr(bfTitle));

      tf.setTextFormatter(titleFormatter(alreadyChangingTitle, radioBtn));

      label.setVisible(false);  // There is a different label in the row fxml
    }

    @Override public RadioButton getToggle()             { return radioBtn; }
    @Override public void setLabelVisible(boolean value) { }  // There is a different label in the row fxml
    @Override protected void mergeInto(BibData mergedBD) { mergedBD.setTitle(tf.getText().strip()); }

    public boolean isBlank()                             { return tf.getText().isBlank(); }
    public TextField getTextField()                      { return tf; }

    public void fixCase()
    {
      alreadyChangingTitle.setTrue();
      tf.setText(HDT_Work.fixCase(tf.getText()));
      alreadyChangingTitle.setFalse();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class MultiLineCtrlr extends ToggleCtrlr
  {
    @FXML private Label label;
    @FXML private RadioButton radioBtn;
    @FXML private TextArea ta;

    MultiLineCtrlr(BibFieldEnum bibFieldEnum, BibData bibData) throws IOException
    {
      super("MultiLineCtrl", bibFieldEnum, bibData);

      label.setText(bibFieldEnum.getUserFriendlyName());

      if (bibData.fieldNotEmpty(bibFieldEnum))
        ta.setText(strListToStr(bibData.getMultiStr(bibFieldEnum), true));

      ta.textProperty().addListener((obs, ov, nv) -> radioBtn.setSelected(true));
    }

    @Override public RadioButton getToggle()             { return radioBtn; }
    @Override public void setLabelVisible(boolean value) { label.setVisible(value); }
    @Override protected void mergeInto(BibData mergedBD) { mergedBD.setMultiStr(bibFieldEnum, convertMultiLineStrToStrList(ta.getText(), true)); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class SingleLineCtrlr extends ToggleCtrlr
  {
    @FXML private Label label;
    @FXML private RadioButton radioBtn;
    @FXML private TextField tf;

    SingleLineCtrlr(BibFieldEnum bibFieldEnum, BibData bibData) throws IOException
    {
      super("SingleLineCtrl", bibFieldEnum, bibData);

      label.setText(bibFieldEnum.getUserFriendlyName());

      if (bibData.fieldNotEmpty(bibFieldEnum))
        tf.setText(bibData.getStr(bibFieldEnum));

      tf.textProperty().addListener((obs, ov, nv) -> radioBtn.setSelected(true));
    }

    @Override public RadioButton getToggle()             { return radioBtn; }
    @Override public void setLabelVisible(boolean value) { label.setVisible(value); }
    @Override protected void mergeInto(BibData mergedBD) { mergedBD.setStr(bibFieldEnum, tf.getText()); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class AuthorsCtrlr extends ToggleCtrlr
  {
    @FXML private Label label;
    @FXML private RadioButton radioBtn;
    @FXML private TableView<HyperTableRow> tvAuthors;

    private final HyperTable htAuthors;
    private final boolean sourceAuthorsNotEmpty;

    AuthorsCtrlr(BibData bibData, HDT_Work destWork) throws IOException
    {
      super("AuthorsCtrl", bfAuthors, bibData);

      htAuthors = new HyperTable(tvAuthors, 0, true, "");

      htAuthors.initConstrainedResize();

      htAuthors.addAuthorEditCol(() -> destWork, (row, cellVal, nextColNdx, nextPopulator) -> row.setCheckboxValue(1, HyperTableCell.getCellID(cellVal) > 0));

      HDT_Work workRecord = nullSwitch(bibData.getWork(), destWork);

      htAuthors.addCheckboxColWithUpdateHandler(createAuthorRecordHandler(htAuthors, () -> workRecord));

      htAuthors.addCheckboxCol();
      htAuthors.addCheckboxCol();

      htAuthors.addChangeOrderMenuItem();
      htAuthors.addRemoveMenuItem();

      htAuthors.addContextMenuItem("Remove this row",
        row -> strNotNullOrEmpty(row.getText(0)) && (row.getID(0) < 1),
        htAuthors::removeRow);

      if (bibData.getWork() != null)
      {
        htAuthors.buildRows(bibData.getWork().getAuthors(), (row, author) ->
        {
          HDT_Person authorRecord = author.getPerson();

          if (authorRecord == null)
          {
            Populator pop = htAuthors.getPopulator(0);
            pop.populate(false);
            pop.addEntry(author.nameLastFirst());
            row.setCellValue(0, author.nameLastFirst(), hdtPerson);
          }
          else
          {
            row.setCellValue(0, authorRecord, authorRecord.listName());
            row.setCheckboxValue(1, true);
          }

          row.setCheckboxValue(2, author.getIsEditor());
          row.setCheckboxValue(3, author.getIsTrans());
        });
      }
      else
      {
        htAuthors.getPopulator(0).populate(false);

        loadFromBibAuthors(bibData.getAuthors(), htAuthors, false, destWork);
      }

      sourceAuthorsNotEmpty = (htAuthors.dataRowCount() > 0);
    }

    @Override public boolean sourceNotEmpty()            { return sourceAuthorsNotEmpty; }
    @Override public RadioButton getToggle()             { return radioBtn; }
    @Override public void setLabelVisible(boolean value) { label.setVisible(value); }

    @Override protected void mergeInto(BibData mergedBD)
    {
      HDT_Work work = mergedBD.getWork();
      List<ObjectGroup> authGroups =  htAuthors.getAuthorGroups(work, 0, -1, 2, 3);

      if (work != null)
        work.setAuthors(authGroups);
      else
        mergedBD.setAllAuthorsFromTable(authGroups);
    }

    public void refresh()
    {
      htAuthors.getPopulator(0).populate(true);

      htAuthors.regenerateCellsBasedOnIDs(0);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class WorkTypeCtrlr extends ToggleCtrlr
  {
    @FXML private Label label;
    @FXML private RadioButton radioBtn;
    @FXML private ComboBox<HyperTableCell> cb;

    private final HyperCB hcb;

    WorkTypeCtrlr(BibData bibData) throws IOException
    {
      super("WorkTypeCtrl", bfWorkType, bibData);

      hcb = new HyperCB(cb, ctEditableLimitedDropDown, new StandardPopulator(hdtWorkType, id -> HDT_WorkType.workTypeIDToEnumVal(id) != wtUnenteredSet));

      hcb.selectIDofRecord(getSourceWorkType());

      cb.getSelectionModel().selectedItemProperty().addListener((obs, ov, nv) -> radioBtn.setSelected(true));
    }

    private HDT_WorkType getSourceWorkType()
    {
      HDT_Work workRecord = bibData.getWork();

      return workRecord == null ? bibData.getWorkType() : workRecord.workType.get();
    }

    @Override public boolean sourceNotEmpty()            { return getSourceWorkType() != null; }
    @Override public RadioButton getToggle()             { return radioBtn; }
    @Override public void setLabelVisible(boolean value) { label.setVisible(value); }

    public HDT_WorkType getWorkType()       { return hcb.selectedRecord(); }
    public ComboBox<HyperTableCell> getCB() { return cb; }

    @Override protected void mergeInto(BibData mergedBD)
    {
      HDT_Work work = mergedBD.getWork();
      if (work != null)
        work.workType.set(getWorkType());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class EntryTypeCtrlr extends ToggleCtrlr
  {
    @FXML private Label label;
    @FXML private RadioButton radioBtn;
    @FXML private ComboBox<EntryType> cb;

    private final boolean sourceEntryTypeNotEmpty;

    EntryTypeCtrlr(BibData bibData) throws IOException
    {
      super("EntryTypeCtrl", bfEntryType, bibData);

      BibManager.instance().initEntryTypeCB(cb);

      if (bibData.entryTypeNotEmpty())
      {
        EntryType entryType = bibData.getEntryType();
        if (cb.getItems().contains(entryType) == false)
        {
          String invalidMsg = '"' + entryType.getUserFriendlyName() + "\" is not a valid " + db.bibLibraryUserFriendlyName() + " entry type.";

          if ((entryType == EntryType.etMonograph) && (cb.getItems().contains(EntryType.etBook)))
          {
            warningPopup(invalidMsg + "\n\"" + EntryType.etBook.getUserFriendlyName() + "\" will be used instead.");
            cb.getSelectionModel().select(EntryType.etBook);
          }
          else
          {
            warningPopup(invalidMsg);
            cb.getSelectionModel().select(null);
          }
        }
        else
        {
          cb.getSelectionModel().select(entryType);
        }
      }

      sourceEntryTypeNotEmpty = (cb.getSelectionModel().getSelectedItem() != null);

      cb.getSelectionModel().selectedItemProperty().addListener((obs, ov, nv) -> radioBtn.setSelected(true));
    }

    public EntryType getEntryType()    { return cb.getValue(); }
    public ComboBox<EntryType> getCB() { return cb; }

    @Override public boolean sourceNotEmpty()            { return sourceEntryTypeNotEmpty; }
    @Override public RadioButton getToggle()             { return radioBtn; }
    @Override public void setLabelVisible(boolean value) { label.setVisible(value); }

    @Override protected void mergeInto(BibData mergedBD) { }  // Entry type should already have been set by the consumer of
  }                                                           // the Merge Works dialog by the time this gets called

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static abstract class DateCtrlr extends ToggleCtrlr
  {
    DateCtrlr(String fxmlName, BibData bibData) throws IOException
    {
      super(fxmlName, bfDate, bibData);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class RawDateCtrlr extends DateCtrlr
  {
    @FXML private Label label;
    @FXML private RadioButton radioBtn;
    @FXML private TextField tf;

    RawDateCtrlr(BibData bibData) throws IOException
    {
      super("SingleLineCtrl", bibData);

      label.setText("Date");

      tf.setText(bibData.getDateRawStr());
      if (tf.getText().isEmpty() == false) radioBtn.setSelected(true);

      tf.textProperty().addListener((obs, ov, nv) -> radioBtn.setSelected(true));
    }

    @Override public boolean sourceNotEmpty()            { return strNotNullOrBlank(bibData.getDateRawStr()); }
    @Override public RadioButton getToggle()             { return radioBtn; }
    @Override public void setLabelVisible(boolean value) { label.setVisible(value); }

    @Override protected void mergeInto(BibData mergedBD)
    {
      if (mergedBD instanceof ZoteroItem zoteroItem)
      {
        zoteroItem.setDateFromRawStr(tf.getText());
        return;
      }

      if (mergedBD instanceof WorkBibData workBibData)
      {
        BibEntry<?, ?> bibEntry = workBibData.getBibEntry();
        if (bibEntry instanceof ZoteroItem zoteroItem)
        {
          zoteroItem.setDateFromRawStr(tf.getText());
          return;
        }
      }

      mergedBD.setDate(BibliographicDate.fromUserStr(tf.getText()));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class StructuredDateCtrlr extends DateCtrlr
  {
    @FXML private Label label;
    @FXML private RadioButton radioBtn;
    @FXML private ComboBox<Month> cbMonth;
    @FXML private TextField tfYear, tfDay;

    private final DateControlsWrapper dateCtrls;

    StructuredDateCtrlr(BibData bibData) throws IOException
    {
      super("DateCtrl", bibData);

      dateCtrls = new DateControlsWrapper(tfYear, cbMonth, tfDay, bibData.getDate());

      dateCtrls.valueProperty().addListener((obs, ov, nv) -> radioBtn.setSelected(true));
    }

    @Override public RadioButton getToggle()             { return radioBtn; }
    @Override public void setLabelVisible(boolean value) { label.setVisible(value); }

    @Override protected void mergeInto(BibData mergedBD)
    {
      mergedBD.setDate(dateCtrls.getDate());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
