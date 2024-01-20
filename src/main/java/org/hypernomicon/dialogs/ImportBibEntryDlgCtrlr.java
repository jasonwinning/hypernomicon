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

package org.hypernomicon.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextField;
import javafx.stage.FileChooser;

import static java.nio.charset.StandardCharsets.*;

public class ImportBibEntryDlgCtrlr extends HyperDlg
{
  @FXML private Button btnBrowse, btnCreateNew, btnExisting;
  @FXML private CheckBox chkDeleteFile, chkNewEntry;
  @FXML private ComboBox<HyperTableCell> cbWork;
  @FXML private TextArea taContents;
  @FXML private TextField tfFile;

  private final HyperCB hcbWork;

  private boolean createNewWork;
  private boolean failedToLoad = false;

  public boolean getCreateNewWork()     { return createNewWork; }
  public HDT_Work getRecord()           { return hcbWork.selectedRecord(); }
  public boolean getCreateNewBibEntry() { return chkNewEntry.isSelected(); }
  public boolean getDeleteFile()        { return chkDeleteFile.isSelected(); }
  public boolean getFailedToLoad()      { return failedToLoad; }
  public List<String> getLines()        { return convertMultiLineStrToStrList(taContents.getText(), false); }
  public FilePath getFilePath()         { return tfFile.getText().length() > 0 ? new FilePath(tfFile.getText()) : null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ImportBibEntryDlgCtrlr(List<String> lines, FilePath filePath)
  {
    super("ImportBibEntryDlg", "Import Bibliography File", true);

    if (db.bibLibraryIsLinked())
      chkNewEntry.setText("Create new " + db.getBibLibrary().type().getUserFriendlyName() + " entry (unless existing work is already assigned to one)");
    else
    {
      chkNewEntry.setSelected(false);
      chkNewEntry.setVisible(false);
    }

    onShown = () -> disableCache(taContents);

    chkDeleteFile.disableProperty().bind(tfFile.textProperty().isEmpty());

    btnBrowse.setOnAction(event -> btnBrowseClick());
    btnCreateNew.setOnAction(event -> doImport(true));
    btnExisting.setOnAction(event -> doImport(false));

    hcbWork = new HyperCB(cbWork, ctDropDownList, new StandardPopulator(hdtWork));
    hcbWork.populate(false);

    if (cbWork.getItems().size() > 0)
      hcbWork.select(cbWork.getItems().get(0));

    if (lines == null)
    {
      if (FilePath.isEmpty(filePath))
        btnBrowseClick();
      else
        loadEntry(filePath);
    }
    else
      loadEntry(lines);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadEntry(FilePath filePath)
  {
    List<String> lines;

    try
    {
      lines = FileUtils.readLines(filePath.toFile(), UTF_8);
    }
    catch (IOException e)
    {
      messageDialog("An error occurred while trying to read the file " + filePath + ": " + e.getMessage(), mtError);
      failedToLoad = true;
      return;
    }

    tfFile.setText(filePath.toString());

    loadEntry(lines);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadEntry(List<String> lines)
  {
    taContents.clear();

    if (collEmpty(lines))
    {
      failedToLoad = true;
      return;
    }

    taContents.setText(strListToStr(lines, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doImport(boolean createNewWork)
  {
    this.createNewWork = createNewWork;

    if (isValid() == false) return;

    okClicked = true;
    dialogStage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return (createNewWork != false) || (hcbWork.selectedID() >= 1) || falseWithErrorMessage("You must select a work record.", cbWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnBrowseClick()
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().addAll(new FileChooser.ExtensionFilter("RIS File (*.ris)", "*.ris"),
                                             new FileChooser.ExtensionFilter("BibTeX File (*.bib)", "*.bib"),
                                             new FileChooser.ExtensionFilter("All Files (*.*)", "*.*"));


    fileChooser.setInitialDirectory(db.unenteredPath().toFile());

    FilePath filePath = ui.windows.showOpenDialog(fileChooser, shownAlready() ? getStage() : ui.getStage());

    if (FilePath.isEmpty(filePath))
    {
      failedToLoad = true;
      return;
    }

    loadEntry(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
