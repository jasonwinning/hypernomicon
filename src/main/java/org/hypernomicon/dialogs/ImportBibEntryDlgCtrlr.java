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

package org.hypernomicon.dialogs;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;
import java.util.List;

import org.hypernomicon.bib.data.BibDataStandalone;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;

import org.jbibtex.ParseException;
import org.jbibtex.TokenMgrException;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------

public class ImportBibEntryDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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

  public ImportBibEntryDlgCtrlr(List<String> lines, FilePath filePath)
  {
    super("ImportBibEntryDlg", "Import Bibliography File", true);

    if (db.bibLibraryIsLinked())
      chkNewEntry.setText("Create new " + db.bibLibraryUserFriendlyName() + " entry (unless existing work is already assigned to one)");
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

    hcbWork = new HyperCB(cbWork, ctEditableLimitedDropDown, new StandardPopulator(hdtWork));
    hcbWork.populate(false);

    if (lines == null)
    {
      if (FilePath.isEmpty(filePath))
        btnBrowseClick();
      else
        loadEntry(filePath);
    }
    else
      loadEntry(lines);

    // Select most recently viewed work if no work was detected

    if ((hcbWork.selectedRecord() == null) && (cbWork.getItems().size() > 0))
      hcbWork.select(cbWork.getItems().get(0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadEntry(FilePath filePath)
  {
    List<String> lines;

    try
    {
      lines = filePath.readToStrList();
    }
    catch (IOException e)
    {
      errorPopup("An error occurred while trying to read the file " + filePath + ": " + getThrowableMessage(e));
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

    BibDataStandalone fileBibData = null;

    try
    {
      fileBibData = BibDataStandalone.detectWithinTextFile(lines, null);
    }
    catch (TokenMgrException | IOException | ParseException e)
    {
      noOp();
    }

    if (fileBibData == null)
      return;

    HDT_Work work = fileBibData.findMatchingWork();

    if (work != null)
      hcbWork.selectIDofRecord(work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doImport(boolean createNewWork)
  {
    this.createNewWork = createNewWork;

    if (isValid() == false) return;

    okClicked = true;
    stage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return (createNewWork != false) || (hcbWork.selectedID() >= 1) || falseWithErrorPopup("You must select a work record.", cbWork);
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

    FilePath filePath = showOpenDialog(fileChooser);

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
