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

package org.hypernomicon.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.settings.LaunchCommandsDlgCtrlr;
import org.hypernomicon.util.*;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.WebButton.WebButtonField;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.http.AsyncHttpClient;
import org.hypernomicon.util.http.FileDownloadUtility;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.tabs.PersonTabCtrlr;

import java.awt.image.BufferedImage;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Objects;
import java.util.Set;

import javax.imageio.ImageIO;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.Strings;

import javafx.application.Platform;
import javafx.embed.swing.SwingFXUtils;
import javafx.fxml.FXML;
import javafx.geometry.Rectangle2D;
import javafx.geometry.Point2D;
import javafx.scene.Cursor;
import javafx.scene.control.*;
import javafx.scene.image.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.shape.Rectangle;
import javafx.scene.paint.Color;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------

public class PictureDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final AsyncHttpClient httpClient = new AsyncHttpClient();

  private static Image picture;

  private final Rectangle2D origViewPort;
  private final PersonTabCtrlr personHyperTab;

  private Point2D mouseStart, mouseEnd, cropStart, cropEnd;
  private Rectangle cropRect = null;
  private Rectangle2D picRect;
  private boolean changed = false, noRemoveCrop = false, webBufferOutOfDate = true, dontRefresh = false;
  private BufferedImage clipboardImageBuffer;
  private FileDownloadUtility.Buffer webImageBuffer;
  private String webBufferFileName = "";

  @FXML private AnchorPane apPicture;
  @FXML private Button btnBrowse, btnPaste, btnRefresh, btnDelete, btnShow, btnEdit, btnWebSrch, btnStop;
  @FXML private CheckBox chkMove;
  @FXML private ImageView ivPicture;
  @FXML private Label lblChangeName;
  @FXML private ProgressBar progressBar;
  @FXML private RadioButton rbNone, rbFile, rbWeb, rbCurrent, rbClipboard;
  @FXML private TextField tfCurrent, tfFile, tfWeb, tfName;
  @FXML private ToggleButton btnJpg, btnPng;

//---------------------------------------------------------------------------

  public PictureDlgCtrlr(Rectangle2D viewPort)
  {
    super("PictureDlg", "Edit Picture", true);

    this.personHyperTab = ui.personHyperTab();

    ivPicture.fitWidthProperty ().bind(apPicture.widthProperty ());
    ivPicture.fitHeightProperty().bind(apPicture.heightProperty());

    btnPaste.setOnAction(event -> tfWeb.setText(getClipboardText(true)));
    setToolTip(btnPaste, "Paste text from clipboard");

    apPicture.widthProperty ().addListener((ob, ov, nv) -> resizeCrop(nv       .doubleValue(), apPicture.getHeight  ()));
    apPicture.heightProperty().addListener((ob, ov, nv) -> resizeCrop(apPicture.getWidth   (), nv       .doubleValue()));

    btnBrowse.setOnAction(event -> btnBrowseClick());

    rbNone     .selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbNoneSelected     (); });
    rbCurrent  .selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbCurrentSelected  (); });
    rbFile     .selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbFileSelected     (); });
    rbWeb      .selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbWebSelected      (); });
    rbClipboard.selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbClipboardSelected(); });

    btnJpg.selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) updateExtensionBasedOnClipboardSelection(); });
    btnPng.selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) updateExtensionBasedOnClipboardSelection(); });

    tfWeb.setTextFormatter(new TextFormatter<>(change ->
    {
      if (change.isContentChange() == false)
        return change;

      webBufferOutOfDate = true;

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
      if (tfWeb.isFocused() && (tfWeb.getText().isBlank() == false))
        tfWeb.selectAll();
    }));

    tfName.focusedProperty().addListener((ob, oldValue, newValue) -> Platform.runLater(() ->
    {
      if (tfName.isFocused() && !tfName.getText().isEmpty())
      {
        try
        {
          if (FilenameUtils.indexOfExtension(tfName.getText()) >= 0)
            tfName.selectRange(0, FilenameUtils.indexOfExtension(tfName.getText()));
          else
            tfName.selectAll();
        }
        catch (IllegalArgumentException e) { noOp(); }
      }
    }));

    btnRefresh.setOnAction(event ->
    {
      if      (rbCurrent  .isSelected()) rbCurrentSelected  ();
      else if (rbFile     .isSelected()) rbFileSelected     ();
      else if (rbWeb      .isSelected()) rbWebSelected      ();
      else if (rbClipboard.isSelected()) rbClipboardSelected();
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
        filePath = personHyperTab.getCurPicture();
      else if (rbFile.isSelected())
        filePath = new FilePath(tfFile.getText());

      if (FilePath.isEmpty(filePath)) return;

      highlightFileInExplorer(filePath);
    });

    btnWebSrch.setText(ui.webButtonMap.get(WebButtonContextPrefKey.PERSON_IMG).getCaption());

    setToolTip(btnWebSrch, "Search for image using " + ui.webButtonMap.get(WebButtonContextPrefKey.PERSON_IMG).getName());

    btnWebSrch.setOnAction(event ->
    {
      String first = personHyperTab.tfFirst.getText(),
             last  = personHyperTab.tfLast.getText();

      ui.webButtonMap.get(WebButtonContextPrefKey.PERSON_IMG)

        .first(WebButtonField.FirstName , first)
        .next (WebButtonField.LastName  , last)
        .next (WebButtonField.SingleName, last.length() > 0 ? last : first)
        .next (WebButtonField.Field     , HyperTableCell.getCellText(personHyperTab.hcbField.selectedHTC()))
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
      if ((mouseStart == null) || (picture == null)) return;

      mouseEnd = mouseCoordsToPercent(event.getX(), event.getY());

      setCropPercents();
    });

    ivPicture.setOnMouseDragged(event ->
    {
      if ((mouseStart == null) || (picture == null)) return;

      mouseEnd = mouseCoordsToPercent(event.getX(), event.getY());

      setCropPercents();
      updateCropRect();
    });

    lblChangeName.setOnMouseClicked(event -> autoGenerateName());

    setToolTip(lblChangeName, "Auto-generate file name");

    btnEdit  .setOnAction(event -> btnEditClick  ());
    btnDelete.setOnAction(event -> btnDeleteClick());

    btnDelete.disableProperty().bind(rbCurrent.selectedProperty().not());

    forceToggleSelection(btnJpg.getToggleGroup());

    if (FilePath.isEmpty(personHyperTab.getCurPicture()) == false)
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
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void autoGenerateName()
  {
    FilePath filePath = getDestFilePath(tfName.getText());
    String ext = filePath.getExtensionOnly();
    if (ext.isEmpty()) ext = "jpg";

    String fileName = makeFileName(personHyperTab.tfLast.getText(), ext);
    HDT_Person person = personHyperTab.activeRecord();

    if (cantUseName(fileName)) fileName = makeFileName(personHyperTab.tfLast.getText() + personHyperTab.tfFirst.getText(), ext);
    if (cantUseName(fileName)) fileName = makeFileName(person.getLastName(), ext);
    if (cantUseName(fileName)) fileName = makeFileName(person.defaultCellText(), ext);
    if (cantUseName(fileName)) return;

    tfName.setText(fileName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeFileName(String name, String ext)
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
    if (filePath.equals(personHyperTab.getCurPicture())) return false;

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

    if (x1 < 0) x1 = 0;  if (x1 > (picture.getWidth () - 1)) x1 = (int) (picture.getWidth () - 1);
    if (x2 < 0) x2 = 0;  if (x2 > (picture.getWidth () - 1)) x2 = (int) (picture.getWidth () - 1);
    if (y1 < 0) y1 = 0;  if (y1 > (picture.getHeight() - 1)) y1 = (int) (picture.getHeight() - 1);
    if (y2 < 0) y2 = 0;  if (y2 > (picture.getHeight() - 1)) y2 = (int) (picture.getHeight() - 1);

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
      x2 = mouseEnd  .getX();
    }
    else
    {
      x2 = mouseStart.getX();
      x1 = mouseEnd  .getX();
    }

    if (mouseStart.getY() < mouseEnd.getY())
    {
      y1 = mouseStart.getY();
      y2 = mouseEnd  .getY();
    }
    else
    {
      y2 = mouseStart.getY();
      y1 = mouseEnd  .getY();
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
    cropEnd   = new Point2D(x2, y2);
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
           ivRatio = ivWidth / ivHeight;

    if (ivRatio >= picRatio)
    {
      double sizeRatio = ivPicture.getFitHeight() / picture.getHeight(); // Extra space is at the sides, not the top and bottom
      picRect = new Rectangle2D((ivWidth - (picture.getWidth() * sizeRatio)) / 2.0, 0, picture.getWidth() * sizeRatio, ivHeight);
    }
    else
    {
      double sizeRatio = ivPicture.getFitWidth() / picture.getWidth();
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
    if (FilePath.isEmpty(personHyperTab.getCurPicture()))
      return;

    if (confirmDialog("Are you sure you want to permanently delete the file \"" + personHyperTab.getCurPicture().getNameOnly() + "\"?", false) == false)
      return;

    if (personHyperTab.getCurPicture().deletePromptOnFail(false) == false)
      return;

    db.unmapFilePath(personHyperTab.getCurPicture());
    removeCrop();
    personHyperTab.assignPicture(null, false);
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

    if (rbCurrent.isSelected())
    {
      if (FilePath.isEmpty(personHyperTab.getCurPicture()) || tfName.getText().isEmpty()) return;

      src = personHyperTab.getCurPicture();
      dest = getDestFilePath(tfName.getText());

      if (src.equals(dest) == false)
      {
        if (confirmDialog("File will be renamed first. Proceed?", false) == false)
          return;

        if (isValid() == false)
          return;
      }
    }
    else if (rbFile.isSelected() || rbWeb.isSelected() || rbClipboard.isSelected())
    {
      boolean sameFile;

      if (rbFile.isSelected())
      {
        if (tfFile.getText().isEmpty()) return;
        src = new FilePath(tfFile.getText());
        dest = getDestFilePath(tfName.getText());
        sameFile = src.equals(dest);
      }
      else
      {
        if (rbWeb.isSelected() && tfWeb.getText().isEmpty()) return;
        if (rbClipboard.isSelected() && (clipboardImageBuffer == null)) return;
        sameFile = false;
      }

      if (sameFile == false)
      {
        if (confirmDialog("File will be copied to database folder first. Proceed?", false) == false)
          return;

        if (isValid() == false)
          return;
      }
    }
    else
      return;

    String execPath = app.prefs.get(PrefKey.IMAGE_EDITOR, "");
    FilePath picturePath = getDestFilePath(tfName.getText());

    if (execPath.isEmpty())
      editFile(picturePath);
    else
      LaunchCommandsDlgCtrlr.launch(execPath, picturePath, PrefKey.IMAGE_EDITOR_COMMANDS, PrefKey.IMAGE_EDITOR_COMMAND_TYPE, -1);
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

  private void rbClipboardSelected()
  {
    removeCrop();

    //ClipboardImageHelper.inspectDataFormatOnClipboard();  // Inspect the actual data format on the clipboard

    clipboardImageBuffer = ClipboardImageHelper.getClipboardImageViaAWT();

    if (clipboardImageBuffer == null)
    {
      ivPicture.setImage(null);
      picture = null;
      return;
    }

    picture = new WritableImage(clipboardImageBuffer.getWidth(), clipboardImageBuffer.getHeight());
    SwingFXUtils.toFXImage(clipboardImageBuffer, (WritableImage) picture);

    ivPicture.setImage(picture);

    updateExtensionBasedOnClipboardSelection();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateExtensionBasedOnClipboardSelection()
  {
    if (rbClipboard.isSelected() == false) return;

    FilePath fileName = new FilePath(tfName.getText());

    if (FilePath.isEmpty(fileName)) return;

    String oldExt = fileName.getExtensionOnly(),
           newExt = btnJpg.getToggleGroup().getSelectedToggle() == btnJpg ? "jpg" : "png";

    if (Objects.equals(oldExt, newExt)) return;

    tfName.setText(oldExt.isEmpty() ?
      (fileName.toString().strip() + FilenameUtils.EXTENSION_SEPARATOR + newExt)
    :
      (Strings.CS.removeEnd(fileName.toString(), oldExt) + newExt));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbWebSelected()
  {
    String url = tfWeb.getText();

    removeCrop();

    chkMove.setDisable(true);
    chkMove.setSelected(true);

    if (url.isBlank())
    {
      ivPicture.setImage(null);
      picture = null;
      tfName.setText("");
      return;
    }

    if (webBufferOutOfDate == false)
    {
      refreshFromWebBuffer();
      return;
    }

    if (dontRefresh) return;

    StringBuilder fileName = new StringBuilder();

    progressBar.setVisible(true);
    btnStop.setVisible(true);

    FileDownloadUtility.downloadToBuffer(url, fileName, true, httpClient, buffer ->
    {
      webImageBuffer = buffer;
      webBufferFileName = fileName.toString();
      refreshFromWebBuffer();

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
    if ((e instanceof CancelledTaskException) == false)
      errorPopup("An error occurred while trying to display the picture: " + getThrowableMessage(e));

    stopClicked();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void refreshFromWebBuffer()
  {
    try
    {
      FilePath tempFile = new FilePath(java.io.File.createTempFile("temp", webBufferFileName));
      tempFile.deleteOnExit();
      webImageBuffer.saveToFile(tempFile);
      picture = new Image(tempFile.toURI().toString());
      tempFile.delete(true);

      if (picture.isError())
        throw picture.getException();

      ivPicture.setImage(picture);

      tfName.setText(webBufferFileName);
      webBufferOutOfDate = false;
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
      errorPopup("An error occurred while trying to display the picture: " + getThrowableMessage(picture.getException()));
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

    picture = new Image(personHyperTab.getCurPicture().toURI().toString());

    if (db.getRootPath().isSubpath(personHyperTab.getCurPicture()) == false)
    {
      chkMove.setDisable(true);
      chkMove.setSelected(true);
    }
    else if (personHyperTab.getCurPicture().getDirOnly().equals(db.picturesPath()))
    {
      chkMove.setDisable(true);
      chkMove.setSelected(false);
    }
    else
    {
      chkMove.setDisable(false);
      chkMove.setSelected(false);
    }

    if (picture.isError() == false)
    {
      ivPicture.setImage(picture);

      tfCurrent.setText(db.getRootPath().relativize(personHyperTab.getCurPicture()).toString());
      tfName.setText(personHyperTab.getCurPicture().getNameOnly().toString());
    }
    else
    {
      errorPopup("An error occurred while trying to display the picture: " + getThrowableMessage(picture.getException()));
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

    FilePath filePath = showOpenDialog(fileChooser);

    if (FilePath.isEmpty(filePath)) return;

    HDT_RecordWithPath existingRecord = HyperPath.getRecordFromFilePath(filePath);

    if ((existingRecord != null) && (existingRecord != personHyperTab.activeRecord()))
    {
      infoPopup(HyperPath.alreadyInUseMessage(filePath, existingRecord));
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
        return personHyperTab.getCurPicture().getDirOnly().resolve(new FilePath(fileName));
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
      personHyperTab.assignPicture(null, true);
      noRemoveCrop = true;
      return true;
    }

    final FilePath curFile = personHyperTab.getCurPicture();

    String newFileDestName = tfName.getText();

  //---------------------------------------------------------------------------
  // Perform checks before deleting previous image
  //---------------------------------------------------------------------------

    if (newFileDestName.isBlank())
      return falseWithErrorPopup("Name cannot be blank.", tfName);

    if (FilePath.isFilenameValid(newFileDestName) == false)
      return falseWithErrorPopup("Invalid file name: \"" + newFileDestName + '"', tfName);

    FilePath newFileSrc = null,
             newFileDest = getDestFilePath(newFileDestName);

    if (rbWeb.isSelected() || rbClipboard.isSelected())
    {
      if (rbWeb.isSelected())
      {
        if (tfWeb.getText().isBlank())
          return falseWithErrorPopup("Please enter a web address.", tfWeb);
      }
      else
      {
        if (clipboardImageBuffer == null)
          return falseWithWarningPopup("An image has not been loaded from the clipboard. Try clicking Refresh button.", rbClipboard);

        if (newFileDest.getExtensionOnly().isBlank())
        {
          updateExtensionBasedOnClipboardSelection();
          newFileDestName = tfName.getText();
          newFileDest = getDestFilePath(newFileDestName);
        }
      }

      for (HyperPath hyperPath : HyperPath.getHyperPathSetForFilePath(newFileDest))
        if (hyperPath.getRecord() != personHyperTab.activeRecord())
          return falseWithErrorPopup("Destination file is already in use.");

      if (newFileDest.exists())
      {
        if (confirmDialog("A file with that name already exists. Okay to overwrite?", false) == false)
          return false;

        try
        {
          newFileDest.delete(false);
        }
        catch (IOException e)
        {
          return falseWithErrorPopup("File cannot be overwritten: " + getThrowableMessage(e));
        }
      }
    }
    else if (rbCurrent.isSelected())
    {
      newFileSrc = curFile;
    }
    else // (rbFile.isSelected())
    {
      newFileSrc = new FilePath(tfFile.getText());

      if (FilePath.isEmpty(newFileSrc) || newFileSrc.getNameOnly().toString().isBlank())
        return falseWithErrorPopup("Please specify a file from the local file system.", tfFile);

      if (newFileSrc.exists() == false)
        return falseWithErrorPopup("Please select an existing file.", tfFile);

      if (newFileSrc.equals(curFile))
        return falseWithErrorPopup("Previous file and new file are the same.", tfFile);
    }

  //---------------------------------------------------------------------------
  // Put new image in place
  //---------------------------------------------------------------------------

    FilePath finalNewFileDest = newFileDest;

    if (rbWeb.isSelected())
    {
      try
      {
        if (webBufferOutOfDate == false)
          webImageBuffer.saveToFile(newFileDest);
        else
        {
          progressBar.setVisible(true);
          btnStop.setVisible(true);

          FileDownloadUtility.downloadToFile(tfWeb.getText(), db.picturesPath(), newFileDestName,
                                             new StringBuilder(), true, httpClient, buffer ->
          {
            personHyperTab.assignPicture(finalNewFileDest, true);
            noRemoveCrop = true;
            rbCurrent.setSelected(true);

            okClicked = true;
            stage.close();

          }, this::exceptionHappened);

          return false;
        }
      }
      catch (IOException e)
      {
        return falseWithErrorPopup("An error occurred while saving the file: " + getThrowableMessage(e));
      }
    }
    else if (rbClipboard.isSelected())
    {
      String format = btnJpg.getToggleGroup().getSelectedToggle() == btnJpg ? "jpg" : "png";

      try (FileOutputStream fos = new FileOutputStream(newFileDest.toFile()))
      {
        ImageIO.write(clipboardImageBuffer, format, fos);
      }
      catch (IOException e)
      {
        return falseWithErrorPopup("An error occurred while saving the file: " + getThrowableMessage(e));
      }
    }
    else
    {
      HDT_RecordWithPath existingRecord = HyperPath.getRecordFromFilePath(newFileDest);

      boolean replacingPictureForSamePerson = false;

      if (existingRecord != null)
      {
        if (existingRecord != personHyperTab.activeRecord())
          return falseWithErrorPopup(HyperPath.alreadyInUseMessage(newFileDest, existingRecord));

        replacingPictureForSamePerson = true;
      }

      if (newFileSrc.equals(newFileDest) == false)
      {
        if (newFileDest.exists() && (replacingPictureForSamePerson == false))
          return falseWithErrorPopup("Unable to move/rename file: A file with that name already exists.");

        if (newFileSrc.exists() == false)
          return falseWithErrorPopup("Source file does not exist.");

        if (newFileSrc.isDirectory())
          return falseWithErrorPopup("Source file cannot be a directory.");

        Set<HyperPath> setOfHyperPathsAlreadyAssignedToSourcePathOfNewFile = HyperPath.getHyperPathSetForFilePath(newFileSrc);

        if (setOfHyperPathsAlreadyAssignedToSourcePathOfNewFile.isEmpty())
        {
          DialogResult result = mrMove;

          if (chkMove.isDisabled())
          {
            result = new PopupDialog("Should the file be moved or copied from its present location?")

            .addDefaultButton("Move", mrMove)
            .addButton       ("Copy", mrCopy)

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
              return falseWithErrorPopup("An error occurred while moving the file: " + getThrowableMessage(e));
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
              return falseWithErrorPopup("An error occurred while copying the file: " + getThrowableMessage(e));
            }
          }
        }
        else
        {
          HDT_Folder destFolder = HyperPath.getFolderFromFilePath(newFileDest.getDirOnly(), true);

          for (HyperPath hyperPath : setOfHyperPathsAlreadyAssignedToSourcePathOfNewFile)
          {
            try
            {
              if (hyperPath.moveToFolder(destFolder.getID(), false, true, newFileDestName) == false)
                return falseWithErrorPopup("Unable to move the file.");
            }
            catch (IOException | HDB_InternalError e)
            {
              return falseWithErrorPopup("An error occurred while moving the file: " + getThrowableMessage(e));
            }
          }
        }
      }
    }

    personHyperTab.assignPicture(newFileDest, true);
    noRemoveCrop = true;
    rbCurrent.setSelected(true);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
