/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.view.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.DesktopApi;
import org.hypernomicon.util.FileDownloadUtility;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.settings.LaunchCommandsDlgCtrlr;
import org.hypernomicon.view.tabs.PersonTabCtrlr;
import org.hypernomicon.view.wrappers.HyperTableCell;

import java.io.IOException;
import java.util.Set;

import org.apache.commons.io.FilenameUtils;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.geometry.Rectangle2D;
import javafx.geometry.Point2D;
import javafx.scene.Cursor;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.AnchorPane;
import javafx.scene.shape.Rectangle;
import javafx.scene.paint.Color;
import javafx.stage.FileChooser;

public class PictureDlgCtrlr extends HyperDlg
{
  private static Image picture;
  public static final AsyncHttpClient httpClient = new AsyncHttpClient();

  private Point2D mouseStart, mouseEnd, cropStart, cropEnd;
  private Rectangle cropRect = null;
  private Rectangle2D picRect, origViewPort;
  private boolean changed = false, noRemoveCrop = false, bufferOutOfDate = true, dontRefresh = false;
  private FileDownloadUtility.Buffer imageBuffer;
  private String bufferFileName = "";

  @FXML private AnchorPane apPicture;
  @FXML private Button btnBrowse, btnPaste, btnRefresh, btnDelete, btnShow, btnEdit, btnWebSrch, btnStop;
  @FXML private CheckBox chkMove;
  @FXML private ImageView ivPicture;
  @FXML private Label lblChangeName;
  @FXML private ProgressBar progressBar;
  @FXML private RadioButton rbNone, rbFile, rbWeb, rbCurrent;
  @FXML private TextField tfCurrent, tfFile, tfWeb, tfName;

  private PersonTabCtrlr personHyperTab;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static PictureDlgCtrlr build(Rectangle2D viewPort)
  {
    return ((PictureDlgCtrlr) create("PictureDlg.fxml", "Edit Picture", true)).init(viewPort);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private PictureDlgCtrlr init(Rectangle2D viewPort)
  {
    this.personHyperTab = ui.personHyperTab();

    ivPicture.fitWidthProperty ().bind(apPicture.widthProperty ());
    ivPicture.fitHeightProperty().bind(apPicture.heightProperty());

    btnPaste.setOnAction(event -> tfWeb.setText(getClipboardText(true)));
    setToolTip(btnPaste, "Paste text from clipboard");

    apPicture.widthProperty ().addListener((ob, oldValue, newValue) -> resizeCrop(newValue .doubleValue(), apPicture.getHeight  ()));
    apPicture.heightProperty().addListener((ob, oldValue, newValue) -> resizeCrop(apPicture.getWidth   (), newValue .doubleValue()));

    btnBrowse.setOnAction(event -> btnBrowseClick());

    rbNone.   selectedProperty().addListener((ob, oldValue, newValue) -> { if (Boolean.TRUE.equals(newValue)) rbNoneSelected    (); });
    rbCurrent.selectedProperty().addListener((ob, oldValue, newValue) -> { if (Boolean.TRUE.equals(newValue)) rbCurrentSelected (); });
    rbFile.   selectedProperty().addListener((ob, oldValue, newValue) -> { if (Boolean.TRUE.equals(newValue)) rbFileSelected    (); });
    rbWeb.    selectedProperty().addListener((ob, oldValue, newValue) -> { if (Boolean.TRUE.equals(newValue)) rbWebSelected     (); });

    tfWeb.setTextFormatter(new TextFormatter<>(change ->
    {
      if (change.isContentChange() == false)
        return change;

      bufferOutOfDate = true;

      if (change.getText().length() > 1)
      {
        Platform.runLater(() -> rbWeb.setSelected(true));
      }
      else
      {
        Platform.runLater(() ->
        {
          dontRefresh = true;
          rbWeb.setSelected(true);
          dontRefresh = false;
        });
      }

      return change;
    }));

    tfWeb.focusedProperty().addListener((ob, oldValue, newValue) -> Platform.runLater(() ->
    {
      if (tfWeb.isFocused() && !tfWeb.getText().isEmpty())
        tfWeb.selectAll();
    }));

    tfName.focusedProperty().addListener((ob, oldValue, newValue) -> Platform.runLater(() ->
    {
      if (tfName.isFocused() && !tfName.getText().isEmpty())
      {
        if (FilenameUtils.indexOfExtension(tfName.getText()) >= 0)
          tfName.selectRange(0, FilenameUtils.indexOfExtension(tfName.getText()));
        else
          tfName.selectAll();
      }
    }));

    btnRefresh.setOnAction(event ->
    {
      if      (rbCurrent.isSelected()) rbCurrentSelected();
      else if (rbFile   .isSelected()) rbFileSelected();
      else if (rbWeb    .isSelected()) rbWebSelected();
    });

    btnShow   .disableProperty().bind(rbCurrent.selectedProperty().not().and(rbFile.selectedProperty().not()));
    btnRefresh.disableProperty().bind(rbNone.selectedProperty());
    btnEdit   .disableProperty().bind(rbNone.selectedProperty());
    tfName    .disableProperty().bind(rbNone.selectedProperty());

    btnStop.setOnAction(event -> stopClicked());

    btnShow.setOnAction(event ->
    {
      FilePath filePath = null;

      if (rbCurrent.isSelected())
        filePath = personHyperTab.curPicture;
      else if (rbFile.isSelected())
        filePath = new FilePath(tfFile.getText());

      if (FilePath.isEmpty(filePath)) return;

      highlightFileInExplorer(filePath);
    });

    btnWebSrch.setText(ui.webButtonMap.get(PREF_KEY_PERSON_IMG_SRCH).getCaption());

    btnWebSrch.setOnAction(event ->
    {
      String first = personHyperTab.tfFirst.getText(),
             last  = personHyperTab.tfLast.getText();

      ui.webButtonMap.get(PREF_KEY_PERSON_IMG_SRCH)

        .first(WebButtonField.FirstName , first)
        .next (WebButtonField.LastName  , last)
        .next (WebButtonField.SingleName, last.length() > 0 ? last : first)
        .next (WebButtonField.Field     , HyperTableCell.getCellText(personHyperTab.cbField.getSelectionModel().getSelectedItem()))
        .go();
    });

    ivPicture.setOnMousePressed(event ->
    {
      if (picture == null) return;

      mouseStart = mouseCoordsToPercent(event.getX(), event.getY());

      removeCrop();
    });

    ivPicture.setOnMouseReleased(event ->
    {
      if (picture == null) return;

      mouseEnd = mouseCoordsToPercent(event.getX(), event.getY());

      setCropPercents();
    });

    ivPicture.setOnMouseDragged(event ->
    {
      if (picture == null) return;

      mouseEnd = mouseCoordsToPercent(event.getX(), event.getY());

      setCropPercents();
      updateCropRect();
    });

    lblChangeName.setOnMouseClicked(event -> autoGenerateName());

    btnEdit  .setOnAction(event -> btnEditClick  ());
    btnDelete.setOnAction(event -> btnDeleteClick());

    btnDelete.disableProperty().bind(rbCurrent.selectedProperty().not());

    if (FilePath.isEmpty(personHyperTab.curPicture) == false)
      rbCurrent.setSelected(true);
    else
    {
      rbNone.setSelected(true);
      rbCurrent.setDisable(true);
    }

    origViewPort = viewPort;

    if ((viewPort != null) && (picture != null))
    {
      cropStart = new Point2D(viewPort.getMinX() / picture.getWidth(), viewPort.getMinY() / picture.getHeight());
      cropEnd   = new Point2D(viewPort.getMaxX() / picture.getWidth(), viewPort.getMaxY() / picture.getHeight());

      setPicRect(ivPicture.getFitWidth(), ivPicture.getFitHeight());
      updateCropRect();
    }

    changed = false;

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void autoGenerateName()
  {
    String ext = "";

    FilePath filePath = getDestFilePath(tfName.getText());
    ext = filePath.getExtensionOnly();
    if (ext.isEmpty()) ext = "jpg";

    String fileName = makeFileName(personHyperTab.tfLast.getText(), ext);
    HDT_Person person = personHyperTab.activeRecord();

    if (cantUseName(fileName)) fileName = makeFileName(personHyperTab.tfLast.getText() + personHyperTab.tfFirst.getText(), ext);
    if (cantUseName(fileName)) fileName = makeFileName(person.getLastName(), ext);
    if (cantUseName(fileName)) fileName = makeFileName(person.listName(), ext);
    if (cantUseName(fileName)) return;

    tfName.setText(fileName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String makeFileName(String name, String ext)
  {
    name = FilePath.removeInvalidFileNameChars(name.replaceAll("\\h+", ""));

    String newName = "";
    for (int pos = 0; pos < name.length(); pos++)
    {
      if (((name.charAt(pos) >= 'A') && (name.charAt(pos) <= 'Z')) ||
          ((name.charAt(pos) >= 'a') && (name.charAt(pos) <= 'z')) ||
          ((name.charAt(pos) >= '0') && (name.charAt(pos) <= '9')) ||
          (name.charAt(pos) == '-'))
        newName = newName + name.charAt(pos);
    }

    return newName + FilenameUtils.EXTENSION_SEPARATOR_STR + ext;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean cantUseName(String fileName)
  {
    if (FilePath.isFilenameValid(fileName) == false) return true;

    FilePath filePath = getDestFilePath(fileName);
    if (filePath.equals(personHyperTab.curPicture)) return false;

    return filePath.exists();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateCropRect()
  {
    double width =  (cropEnd.getX() - cropStart.getX()) * picRect.getWidth(),
           height = (cropEnd.getY() - cropStart.getY()) * picRect.getHeight();

    if (cropRect == null)
    {
      cropRect = new Rectangle(picRect.getMinX() + (cropStart.getX() * picRect.getWidth()),
                               picRect.getMinY() + (cropStart.getY() * picRect.getHeight()), width, height);

      cropRect.setFill(Color.TRANSPARENT);
      cropRect.setStroke(Color.LIGHTSEAGREEN);
      cropRect.setStrokeWidth(2);
      cropRect.getStrokeDashArray().addAll(10d, 10d);
      cropRect.setCursor(Cursor.CROSSHAIR);
      cropRect.setOnMousePressed(event -> removeCrop());
      apPicture.getChildren().add(cropRect);
    }
    else
    {
      cropRect.setX(picRect.getMinX() + (cropStart.getX() * picRect.getWidth()));
      cropRect.setY(picRect.getMinY() + (cropStart.getY() * picRect.getHeight()));
      cropRect.setWidth(width);
      cropRect.setHeight(height);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Rectangle2D getViewPort()
  {
    if (cropRect == null)
      return null;

    if (changed == false)
      return origViewPort;

    int x1 = Double.valueOf((cropStart.getX() * picture.getWidth ()) + 0.5).intValue(),
        x2 = Double.valueOf((cropEnd  .getX() * picture.getWidth ()) + 0.5).intValue(),
        y1 = Double.valueOf((cropStart.getY() * picture.getHeight()) + 0.5).intValue(),
        y2 = Double.valueOf((cropEnd  .getY() * picture.getHeight()) + 0.5).intValue();

    if (x1 < 0) x1 = 0;
    if (x1 > (picture.getWidth() - 1)) x1 = (int) (picture.getWidth() - 1);
    if (x2 < 0) x2 = 0;
    if (x2 > (picture.getWidth() - 1)) x2 = (int) (picture.getWidth() - 1);
    if (y1 < 0) y1 = 0;
    if (y1 > (picture.getHeight() - 1)) y1 = (int) (picture.getHeight() - 1);
    if (y2 < 0) y2 = 0;
    if (y2 > (picture.getHeight() - 1)) y2 = (int) (picture.getHeight() - 1);

    return new Rectangle2D(x1, y1, (x2 + 1) - x1, (y2 + 1) - y1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeCrop()
  {
    if (noRemoveCrop) return;

    apPicture.getChildren().remove(cropRect);
    cropRect = null;
    changed = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setCropPercents()
  {
    double x1, x2, y1, y2;

    if (mouseStart.getX() < mouseEnd.getX())
    {
      x1 = mouseStart.getX();
      x2 = mouseEnd.getX();
    }
    else
    {
      x2 = mouseStart.getX();
      x1 = mouseEnd.getX();
    }

    if (mouseStart.getY() < mouseEnd.getY())
    {
      y1 = mouseStart.getY();
      y2 = mouseEnd.getY();
    }
    else
    {
      y2 = mouseStart.getY();
      y1 = mouseEnd.getY();
    }

    if (x1 < .001) x1 = .001;
    if (x2 < .002) x2 = .002;
    if (x1 > .998) x1 = .998;
    if (x2 > .999) x2 = .999;
    if (y1 < .001) y1 = .001;
    if (y2 < .002) y2 = .002;
    if (y1 > .998) y1 = .998;
    if (y2 > .999) y2 = .999;

    cropStart = new Point2D(x1, y1);
    cropEnd = new Point2D(x2, y2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void resizeCrop(double newWidth, double newHeight)
  {
    if (cropRect != null)
    {
      setPicRect(newWidth, newHeight);

      double width =  (cropEnd.getX() - cropStart.getX()) * picRect.getWidth(),
             height = (cropEnd.getY() - cropStart.getY()) * picRect.getHeight();

      cropRect.setX(picRect.getMinX() + (cropStart.getX() * picRect.getWidth()));
      cropRect.setY(picRect.getMinY() + (cropStart.getY() * picRect.getHeight()));
      cropRect.setWidth(width);
      cropRect.setHeight(height);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPicRect(double ivWidth, double ivHeight)
  {
    if (picture == null) return;

    double picRatio = picture.getWidth() / picture.getHeight(),
           ivRatio = ivWidth / ivHeight,
           sizeRatio = 1.0;

    if (ivRatio >= picRatio)
    {
      sizeRatio = ivPicture.getFitHeight() / picture.getHeight(); // Extra space is at the sides, not the top and bottom
      picRect = new Rectangle2D((ivWidth - (picture.getWidth() * sizeRatio)) / 2.0, 0, picture.getWidth() * sizeRatio, ivHeight);
    }
    else
    {
      sizeRatio = ivPicture.getFitWidth() / picture.getWidth();
      picRect = new Rectangle2D(0, (ivHeight - (picture.getHeight() * sizeRatio)) / 2.0, ivWidth, picture.getHeight() * sizeRatio);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Point2D mouseCoordsToPercent(double mouseX, double mouseY)
  {
    setPicRect(ivPicture.getFitWidth(), ivPicture.getFitHeight());

    return new Point2D(mouseX / picRect.getWidth(), mouseY / picRect.getHeight());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnDeleteClick()
  {
    if (FilePath.isEmpty(personHyperTab.curPicture))
      return;

    if (confirmDialog("Are you sure you want to delete the file \"" + personHyperTab.curPicture.getNameOnly() + "\"?") == false)
      return;

    if (personHyperTab.curPicture.deletePromptOnFail(false) == false)
      return;

    db.unmapFilePath(personHyperTab.curPicture);
    removeCrop();
    personHyperTab.curPicture = null;
    tfCurrent.setText("");
    rbNone.setSelected(true);
    rbCurrent.setDisable(true);
    ivPicture.setImage(null);
    picture = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnEditClick()
  {
    FilePath src, dest;
    boolean sameFile;

    if (rbCurrent.isSelected())
    {
      if (FilePath.isEmpty(personHyperTab.curPicture) || tfName.getText().isEmpty()) return;

      src = personHyperTab.curPicture;
      dest = getDestFilePath(tfName.getText());

      if (src.equals(dest) == false)
      {
        if (confirmDialog("File will be renamed first. Proceed?") == false)
          return;

        if (isValid() == false)
          return;
      }
    }
    else if (rbFile.isSelected() || rbWeb.isSelected())
    {
      if (rbFile.isSelected())
      {
        if (tfFile.getText().isEmpty()) return;
        src = new FilePath(tfFile.getText());
        dest = getDestFilePath(tfName.getText());
        sameFile = src.equals(dest);
      }
      else
      {
        if (tfWeb.getText().isEmpty()) return;
        sameFile = false;
      }

      if (sameFile == false)
      {
        if (confirmDialog("File will be copied to database folder first. Proceed?") == false)
          return;

        if (isValid() == false)
          return;
      }
    }
    else
      return;

    String execPath = appPrefs.get(PREF_KEY_IMAGE_EDITOR, "");
    FilePath picturePath = getDestFilePath(tfName.getText());

    if (execPath.isEmpty())
      DesktopApi.edit(picturePath);
    else
      LaunchCommandsDlgCtrlr.launch(execPath, picturePath, PREF_KEY_IMAGE_EDITOR_COMMANDS, PREF_KEY_IMAGE_EDITOR_COMMAND_TYPE, -1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbNoneSelected()
  {
    removeCrop();

    ivPicture.setImage(null);
    picture = null;
    tfName.setText("");
    chkMove.setDisable(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbWebSelected()
  {
    String url = tfWeb.getText();

    removeCrop();

    chkMove.setDisable(true);
    chkMove.setSelected(true);

    if (url.isEmpty())
    {
      ivPicture.setImage(null);
      picture = null;
      tfName.setText("");
      return;
    }

    if (bufferOutOfDate == false)
    {
      refreshFromBuffer();
      return;
    }

    if (dontRefresh) return;

    StringBuilder fileName = new StringBuilder();

    progressBar.setVisible(true);
    btnStop.setVisible(true);

    FileDownloadUtility.downloadToBuffer(url, fileName, true, httpClient, buffer ->
    {
      imageBuffer = buffer;
      bufferFileName = fileName.toString();
      refreshFromBuffer();

      progressBar.setVisible(false);
      btnStop.setVisible(false);

    }, this::exceptionHappened);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stopClicked()
  {
    httpClient.stop();

    ivPicture.setImage(null);
    picture = null;
    tfName.setText("");

    progressBar.setVisible(false);
    btnStop.setVisible(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void exceptionHappened(Exception e)
  {
    if ((e instanceof TerminateTaskException) == false)
      messageDialog("An error occurred while trying to display the picture: " + e.getMessage(), mtError);

    stopClicked();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshFromBuffer()
  {
    try
    {
      FilePath tempFile = new FilePath(java.io.File.createTempFile("temp", bufferFileName));
      imageBuffer.saveToFile(tempFile);
      picture = new Image(tempFile.toURI().toString());
      tempFile.delete(true);

      if (picture.isError())
        throw picture.getException();

      ivPicture.setImage(picture);

      tfName.setText(bufferFileName);
      bufferOutOfDate = false;
    }
    catch (Exception e)
    {
      exceptionHappened(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbFileSelected()
  {
    FilePath path = new FilePath(tfFile.getText());

    removeCrop();

    tfName.setText(path.getNameOnly().toString());
    if (FilePath.isEmpty(path))
    {
      ivPicture.setImage(null);
      picture = null;
      return;
    }

    if (db.getRootPath().isSubpath(path) == false)
    {
      chkMove.setDisable(true);
      chkMove.setSelected(true);
    }
    else if (path.getDirOnly().equals(db.picturesPath()))
    {
      chkMove.setDisable(true);
      chkMove.setSelected(false);
    }
    else
    {
      chkMove.setDisable(false);
      chkMove.setSelected(false);
    }

    picture = new Image(path.toURI().toString());
    if (picture.isError())
    {
      messageDialog("An error occurred while trying to display the picture: " + picture.getException().getMessage(), mtError);
      ivPicture.setImage(null);
      picture = null;
      return;
    }

    ivPicture.setImage(picture);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbCurrentSelected()
  {
    removeCrop();

    picture = new Image(personHyperTab.curPicture.toURI().toString());

    if (db.getRootPath().isSubpath(personHyperTab.curPicture) == false)
    {
      chkMove.setDisable(true);
      chkMove.setSelected(true);
    }
    else if (personHyperTab.curPicture.getDirOnly().equals(db.picturesPath()))
    {
      chkMove.setDisable(true);
      chkMove.setSelected(false);
    }
    else
    {
      chkMove.setDisable(false);
      chkMove.setSelected(false);
    }

    if (!picture.isError())
    {
      ivPicture.setImage(picture);

      tfCurrent.setText(db.getRootPath().relativize(personHyperTab.curPicture).toString());
      tfName.setText(personHyperTab.curPicture.getNameOnly().toString());
    }
    else
    {
      messageDialog("An error occurred while trying to display the picture: " + picture.getException().getMessage(), mtError);
      ivPicture.setImage(null);
      picture = null;
      rbNone.setSelected(true);
      rbCurrent.setDisable(true);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnBrowseClick()
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().addAll(new FileChooser.ExtensionFilter("Image files", "*.jpg;*.gif;*.png;*.jpeg"),
                                             new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    fileChooser.setInitialDirectory(db.picturesPath().toFile());

    FilePath filePath = ui.windows.showOpenDialog(fileChooser, getStage());

    if (FilePath.isEmpty(filePath)) return;

    HDT_RecordWithPath existingRecord = HyperPath.getRecordFromFilePath(filePath);

    if ((existingRecord != null) && (existingRecord != personHyperTab.activeRecord()))
    {
      messageDialog(HyperPath.alreadyInUseMessage(filePath, existingRecord), mtInformation);
      return;
    }

    tfFile.setText(filePath.toString());

    if (rbFile.isSelected())
      rbFileSelected();
    else
      rbFile.setSelected(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePath getDestFilePath(String fileName)
  {
    fileName = new FilePath(fileName).getNameOnly().toString();

    if (rbCurrent.isSelected())
    {
      if (chkMove.isSelected() == false)
        return personHyperTab.curPicture.getDirOnly().resolve(new FilePath(fileName));
    }

    else if (rbFile.isSelected())
    {
      FilePath filePath = new FilePath(tfFile.getText());
      if (db.getRootPath().isSubpath(filePath) && (chkMove.isSelected() == false))
        return filePath.getDirOnly().resolve(new FilePath(fileName));
    }

    return db.picturesPath(fileName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (rbNone.isSelected())
    {
      personHyperTab.curPicture = null;
      noRemoveCrop = true;
      return true;
    }

    final FilePath curFile = personHyperTab.curPicture;
    final String curFileName = FilePath.isEmpty(curFile) ? "" : curFile.getNameOnly().toString();

    FilePath newFileSrc = null,
             newFileDest = getDestFilePath(tfName.getText());

    String newFileSrcName = "",
           newFileDestName = tfName.getText();

  //---------------------------------------------------------------------------
  // Perform checks before deleting previous image
  //---------------------------------------------------------------------------

    if (newFileDestName.isBlank())
      return falseWithErrorMessage("Name cannot be blank.", tfName);

    Set<HyperPath> set;

    if (rbWeb.isSelected())
    {
      if (tfWeb.getText().isBlank())
        return falseWithErrorMessage("Please enter a web address.", tfWeb);

      set = HyperPath.getHyperPathSetForFilePath(newFileDest);

      if (set.size() > 0)
      {
        for (HyperPath hyperPath : set)
        {
          if (hyperPath.getRecord() != personHyperTab.activeRecord())
            return falseWithErrorMessage("Destination file is already in use.");
        }
      }

      if (newFileDest.exists())
      {
        if (confirmDialog("A file with that name already exists. Okay to overwrite?") == false)
          return false;

        try
        {
          newFileDest.delete(false);
        }
        catch (IOException e)
        {
          return falseWithErrorMessage("File cannot be overwritten: " + e.getMessage());
        }
      }
    }
    else if (rbCurrent.isSelected())
    {
      newFileSrc = curFile;
      newFileSrcName = curFileName;
    }
    else // (rbFile.isSelected())
    {
      newFileSrc = new FilePath(tfFile.getText());
      newFileSrcName = newFileSrc.getNameOnly().toString();

      if (FilePath.isEmpty(newFileSrc) || newFileSrcName.isBlank())
        return falseWithErrorMessage("Please specify a file from the local file system.", tfFile);

      if (newFileSrc.exists() == false)
        return falseWithErrorMessage("Please select an existing file.", tfFile);

      if (newFileSrc.equals(curFile))
        return falseWithErrorMessage("Previous file and new file are the same.", tfFile);
    }

  //---------------------------------------------------------------------------
  // Put new image in place
  //---------------------------------------------------------------------------

    if (rbWeb.isSelected())
    {
      try
      {
        if (bufferOutOfDate == false)
          imageBuffer.saveToFile(newFileDest);
        else
        {
          progressBar.setVisible(true);
          btnStop.setVisible(true);

          FileDownloadUtility.downloadToFile(tfWeb.getText(), db.picturesPath(), newFileDestName,
                                             new StringBuilder(), true, httpClient, buffer ->
          {
            personHyperTab.curPicture = newFileDest;
            noRemoveCrop = true;
            rbCurrent.setSelected(true);

            okClicked = true;
            dialogStage.close();

          }, this::exceptionHappened);

          return false;
        }
      }
      catch (IOException ex)
      {
        return falseWithErrorMessage("An error occurred while saving the file: " + ex.getMessage());
      }
    }
    else
    {
      HDT_RecordWithPath existingRecord = HyperPath.getRecordFromFilePath(newFileDest);

      boolean replacingPictureForSamePerson = false;

      if (existingRecord != null)
      {
        if (existingRecord != personHyperTab.activeRecord())
          return falseWithErrorMessage(HyperPath.alreadyInUseMessage(newFileDest, existingRecord));

        replacingPictureForSamePerson = true;
      }

      if (newFileSrc.equals(newFileDest) == false)
      {
        if (newFileDest.exists() && (replacingPictureForSamePerson == false))
          return falseWithErrorMessage("Unable to move/rename file: A file with that name already exists.");

        if (newFileSrc.exists() == false)
          return falseWithErrorMessage("Source file does not exist.");

        if (newFileSrc.isDirectory())
          return falseWithErrorMessage("Source file cannot be a directory.");

        set = HyperPath.getHyperPathSetForFilePath(newFileSrc);

        if (set.isEmpty())
        {
          DialogResult result = mrMove;

          if (chkMove.isDisabled())
          {
            result = new PopupDialog("Should the file be moved or copied from its present location?")

            .addButton("Move", mrMove)
            .addButton("Copy", mrCopy)

            .showModal();
          }

          if (result == mrMove)
          {
            try
            {
              newFileSrc.moveTo(newFileDest, true);
            }
            catch (IOException e)
            {
              return falseWithErrorMessage("An error occurred while moving the file: " + e.getMessage());
            }

            db.unmapFilePath(newFileSrc);
          }
          else
          {
            try
            {
              newFileSrc.copyTo(newFileDest, true);
            }
            catch (IOException e)
            {
              return falseWithErrorMessage("An error occurred while copying the file: " + e.getMessage());
            }
          }
        }
        else
        {
          HDT_Folder destFolder = HyperPath.getFolderFromFilePath(newFileDest.getDirOnly(), true);

          for (HyperPath hyperPath : set)
          {
            try
            {
              if (!hyperPath.moveToFolder(destFolder.getID(), false, true, newFileDestName))
                return falseWithErrorMessage("Unable to move the file.");
            }
            catch (IOException e)
            {
              return falseWithErrorMessage("An error occurred while moving the file: " + e.getMessage());
            }
          }
        }
      }
    }

    personHyperTab.curPicture = newFileDest;
    noRemoveCrop = true;
    rbCurrent.setSelected(true);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}