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

package org.hypernomicon.settings;

import static org.hypernomicon.App.app;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.settings.SettingsDlgCtrlr.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.model.records.HDT_WorkFile.FileNameAuthor;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.SplitString;
import org.hypernomicon.util.Util;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.skin.TableColumnHeader;
import javafx.scene.layout.VBox;
import javafx.scene.text.TextAlignment;

//---------------------------------------------------------------------------

public class WorkFileNamingSettingsCtrlr implements SettingsControl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum WorkFileNameComponentType
  {
    fncAuthorLastNames("Author last names", AUTHOR_FN_COMPONENT),
    fncYear("Year", YEAR_FN_COMPONENT),
    fncTitleNoSub("Title (no subtitle)", TITLE_FN_COMPONENT),
    fncTranslators("Translators", TRANS_FN_COMPONENT),
    fncEditors("Editors", EDITOR_FN_COMPONENT),
    fncContainerNoSub("Container title (no subtitle)", CONTAINER_FN_COMPONENT),
    fncPublisher("Publisher", PUBLISHER_FN_COMPONENT),
    fncBlank("", BLANK_FN_COMPONENT);

    private final String caption;
    final int prefValue;

//---------------------------------------------------------------------------

    WorkFileNameComponentType(String caption, int prefValue)
    {
      this.caption = caption;
      this.prefValue = prefValue;
    }

//---------------------------------------------------------------------------

    static WorkFileNameComponentType forInteger(int prefValue)
    {
      return Arrays.stream(values()).filter(type -> type.prefValue == prefValue).findFirst().orElse(fncBlank);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class WorkFileNameComponent
  {
    public final WorkFileNameComponentType type;
    public final String beforeSep, withinSep, afterSep, testStr;
    public final Set<HDT_WorkType> excludedWorkTypes;

//---------------------------------------------------------------------------

    /**
     *
     * @param prefNdx 1-based index used for preference keys
     */
    private WorkFileNameComponent(int prefNdx)
    {
      type = WorkFileNameComponentType.forInteger(db.prefs.getInt(PREF_KEY_FN_COMPONENT + prefNdx, 0));
      beforeSep = db.prefs.get(PREF_KEY_FN_BEFORE_SEP + prefNdx, "");
      withinSep = db.prefs.get(PREF_KEY_FN_WITHIN_SEP + prefNdx, "");
      afterSep  = db.prefs.get(PREF_KEY_FN_AFTER_SEP  + prefNdx, "");
      testStr   = db.prefs.get(PREF_KEY_FN_TEST       + prefNdx, "");

      excludedWorkTypes = new HashSet<>();

      new SplitString(db.prefs.get(PREF_KEY_FN_EXCL_WORK_TYPES + prefNdx, ""), ';').forEach(workTypeStr ->
      {
        String trimmedWorkTypeStr = ultraTrim(workTypeStr);
        HDT_WorkType workType = db.workTypes.getByID(parseInt(trimmedWorkTypeStr, -1));
        if (HDT_Record.isEmpty(workType) == false)
          excludedWorkTypes.add(workType);
      });
    }

//---------------------------------------------------------------------------

    private WorkFileNameComponent(WorkFileNameComponentType type, String exclTypesStr, String beforeSep, String withinSep, String afterSep, String testStr)
    {
      this.type      = type;
      this.beforeSep = beforeSep;
      this.withinSep = withinSep;
      this.afterSep  = afterSep;
      this.testStr   = testStr;

      excludedWorkTypes  = new HashSet<>();

      new SplitString(exclTypesStr, ';').forEach(workTypeStr ->
      {
        String trimmedWorkTypeStr = ultraTrim(workTypeStr);

        db.workTypes.forEach(workType ->
        {
          if (trimmedWorkTypeStr.equalsIgnoreCase(workType.name()))
            excludedWorkTypes.add(workType);
        });
      });
    }

//---------------------------------------------------------------------------

    private void saveToPrefs(int prefNdx)
    {
      db.prefs.putInt(PREF_KEY_FN_COMPONENT    + prefNdx, type.prefValue);
      db.prefs.put(PREF_KEY_FN_BEFORE_SEP      + prefNdx, beforeSep     );
      db.prefs.put(PREF_KEY_FN_WITHIN_SEP      + prefNdx, withinSep     );
      db.prefs.put(PREF_KEY_FN_AFTER_SEP       + prefNdx, afterSep      );
      db.prefs.put(PREF_KEY_FN_TEST            + prefNdx, testStr       );

      String prefStr = excludedWorkTypes.stream().map(workType -> String.valueOf(workType.getID())).reduce((s1, s2) -> s1 + ';' + s2).orElse("");
      db.prefs.put(PREF_KEY_FN_EXCL_WORK_TYPES + prefNdx, prefStr);
    }

//---------------------------------------------------------------------------

    public static List<WorkFileNameComponent> loadFromPrefs()
    {
      int componentCount = db.prefs.getInt(PREF_KEY_FN_COMPONENT_COUNT, 5);  // Before a TableView was used, there were always 5 components

      return IntStream.range(1, componentCount + 1).boxed().map(WorkFileNameComponent::new).toList();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnRevert;
  @FXML private CheckBox chkAddInitial, chkLowercase, chkPosix, chkTreatEdAsAuthor, chkYearLetter;
  @FXML private Label lblExample;
  @FXML private TextField tfExample, tfMaxChar;
  @FXML private VBox vbox;
  @FXML private TableView<HyperTableRow> tv;

  private HyperTable hyperTable;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(boolean noDB)
  {
    hyperTable = new HyperTable(tv, 0, true, "");

    Populator pop = Populator.create(CellValueType.cvtFileNameComponent,
        EnumSet.allOf(WorkFileNameComponentType.class).stream().map(type -> new HyperTableCell(type.prefValue, type.caption, hdtNone)).toList());

    hyperTable.addColAltPopulatorWithUpdateHandler(hdtNone, HyperCtrlType.ctDropDown, pop, (row, cellVal, nextColNdx, nextPopulator) -> refreshExample());

    hyperTable.addLabelEditCol((row, colNdx) ->
    {
      ExclWorkTypesDlgCtrlr extdc = new ExclWorkTypesDlgCtrlr(getComponentFromRow(row).excludedWorkTypes, row.getText(0));
      extdc.showModal();

      setExclWorkTypesCellValue(extdc.exclTypes(), row);
      row.setCellValue(1, extdc.exclTypes().map(HDT_WorkType::name).reduce((s1, s2) -> s1 + "; " + s2).orElse(""), hdtNone);
    });

    initColumn(2);
    initColumn(3);
    initColumn(4);
    initColumn(5);

    if (noDB) return;

    btnRevert.setOnAction(event -> reloadFromPrefs());

    hyperTable.setDefaultValue(3, new HyperTableCell(" ", hdtNone)); // Default within-separator is space
    hyperTable.addRemoveMenuItem(row ->
    {
      HyperTableRow lastRow = hyperTable.getRows().get(hyperTable.getRows().size() - 1);
      return lastRow != row;

    }, row -> refreshExample());

    hyperTable.addChangeOrderMenuItem(true, this::refreshExample);

    lblExample.setOnMouseClicked(event -> refreshExample());

    initCheckBox(db.prefs, chkTreatEdAsAuthor, PREF_KEY_FN_TREAT_ED_AS_AUTHOR, true , nv -> refreshExample());
    initCheckBox(db.prefs, chkAddInitial     , PREF_KEY_FN_ADD_INITIAL       , false, nv -> refreshExample());
    initCheckBox(db.prefs, chkYearLetter     , PREF_KEY_FN_YEAR_LETTER       , false, nv -> refreshExample());
    initCheckBox(db.prefs, chkPosix          , PREF_KEY_FN_POSIX             , false, nv -> refreshExample());
    initCheckBox(db.prefs, chkLowercase      , PREF_KEY_FN_LOWERCASE         , false, nv -> refreshExample());

    initMaxChar(tfMaxChar, PREF_KEY_FN_MAX_CHAR);

    if (app.debugging == false)
      vbox.getChildren().remove(1, 3);

    reloadFromPrefs();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void reloadFromPrefs()
  {
    hyperTable.clear();

    hyperTable.buildRows(WorkFileNameComponent.loadFromPrefs(), (row, component) ->
    {
      row.setCellValue(0, new HyperTableCell(component.type.prefValue, component.type.caption, hdtNone));

      setExclWorkTypesCellValue(component.excludedWorkTypes.stream(), row);

      row.setCellValue(2, new HyperTableCell(component.beforeSep, hdtNone));
      row.setCellValue(3, new HyperTableCell(component.withinSep, hdtNone));
      row.setCellValue(4, new HyperTableCell(component.afterSep , hdtNone));
      row.setCellValue(5, new HyperTableCell(component.testStr  , hdtNone));
    });

    refreshExample();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initColumn(int colNdx)
  {
    hyperTable.addTextEditColWithUpdateHandler(hdtNone, true, (row, cellVal, nextColNdx, nextPopulator) -> refreshExample());

    Platform.runLater(() ->
    {
      TableColumnHeader header = (TableColumnHeader) tv.getColumns().get(colNdx).getStyleableNode();
      Label label = (Label) header.lookup(".label");
      label.setTextAlignment(TextAlignment.CENTER);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void save(boolean noDB)
  {
    if (noDB) return;

    int componentCount = db.prefs.getInt(PREF_KEY_FN_COMPONENT_COUNT, 5);  // Before a TableView was used, there were always 5 components

    for (int prefNdx = 1; prefNdx <= componentCount; prefNdx++)
    {
      db.prefs.remove(PREF_KEY_FN_COMPONENT );
      db.prefs.remove(PREF_KEY_FN_EXCL_WORK_TYPES);
      db.prefs.remove(PREF_KEY_FN_BEFORE_SEP);
      db.prefs.remove(PREF_KEY_FN_WITHIN_SEP);
      db.prefs.remove(PREF_KEY_FN_AFTER_SEP );
      db.prefs.remove(PREF_KEY_FN_TEST      );
    }

    List<WorkFileNameComponent> components = saveComponentsFromTableToList();

    int prefNdx = 0;

    for (WorkFileNameComponent component : components)
    {
      prefNdx++;
      component.saveToPrefs(prefNdx);
    }

    db.prefs.putInt(PREF_KEY_FN_COMPONENT_COUNT, prefNdx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initMaxChar(TextField tf, String prefKey)
  {
    tf.setText(String.valueOf(db.prefs.getInt(prefKey, 255)));

    tf.setTextFormatter(new TextFormatter<>(change ->
    {
      if (change.getText().matches(".*[^0-9].*") && change.isAdded())
        change.setText("");

      return change;
    }));

    tf.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      int intVal = parseInt(newValue, -1);
      if (intVal < 1)
        intVal = 255;

      if (intVal < 14)
        intVal = 14;

      db.prefs.putInt(prefKey, intVal);
      refreshExample();
    });

    tf.focusedProperty().addListener((obs, ov, nv) ->
    {
      if (Boolean.FALSE.equals(nv))
        tf.setText(String.valueOf(db.prefs.getInt(prefKey, 255)));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<WorkFileNameComponent> saveComponentsFromTableToList()
  {
    List<WorkFileNameComponent> components = new ArrayList<>();

    for (HyperTableRow row : hyperTable.dataRows())
    {
      WorkFileNameComponent component = getComponentFromRow(row);
      if (component.type == WorkFileNameComponentType.fncBlank)
        continue;

      components.add(component);
    }

    return components;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static WorkFileNameComponent getComponentFromRow(HyperTableRow row)
  {
    return new WorkFileNameComponent(WorkFileNameComponentType.forInteger(row.getID(0)), row.getText(1), row.getText(2), row.getText(3), row.getText(4), row.getText(5));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void setExclWorkTypesCellValue(Stream<HDT_WorkType> workTypes, HyperTableRow row)
  {
    row.setCellValue(1, workTypes.map(HDT_WorkType::name).reduce((s1, s2) -> s1 + "; " + s2).orElse(""), hdtNone);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshExample()
  {
    String author = "", title = "", year = "", trans = "", editor = "", publisher = "", container = "";
    List<WorkFileNameComponent> components = saveComponentsFromTableToList();

    for (WorkFileNameComponent component : components)
    {
      switch (component.type)
      {
        case fncAuthorLastNames : author    = ultraTrim(component.testStr); break;
        case fncTitleNoSub      : title     = ultraTrim(component.testStr); break;
        case fncYear            : year      = ultraTrim(component.testStr); break;
        case fncTranslators     : trans     = ultraTrim(component.testStr); break;
        case fncEditors         : editor    = ultraTrim(component.testStr); break;
        case fncPublisher       : publisher = ultraTrim(component.testStr); break;
        case fncContainerNoSub  : container = ultraTrim(component.testStr); break;

        default                 : break;
      }
    }

    List<FileNameAuthor> authors = new ArrayList<>();

    addAuthorsToList(authors, author, false, false);
    addAuthorsToList(authors, trans , false, true );
    addAuthorsToList(authors, editor, true , false);

    tfExample.setText(HDT_WorkFile.makeFileName(authors, null, year, title, container, publisher, "pdf", Collections.unmodifiableList(components)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addAuthorsToList(List<FileNameAuthor> authors, String authorsStr, boolean isEditor, boolean isTrans)
  {
    new SplitString(authorsStr, ';').stream().map(Util::ultraTrim)
                                             .filter(str -> str.length() > 0)
                                             .forEach(str -> authors.add(new FileNameAuthor(str, isEditor, isTrans)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
