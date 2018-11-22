/*
 * Copyright 2015-2018 Jason Winning
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
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.DesktopApi;
import org.hypernomicon.util.FileDownloadUtility;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.tabs.PersonTabController;
import org.hypernomicon.view.wrappers.HyperTableCell;

import java.io.IOException;
import java.util.function.UnaryOperator;

import org.apache.commons.io.FilenameUtils;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.geometry.Rectangle2D;
import javafx.geometry.Point2D;
import javafx.scene.Cursor;
import javafx.scene.control.Button;
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

public class PictureDialogController extends HyperDialog
{
  private static Image picture;
  public static final AsyncHttpClient httpClient = new AsyncHttpClient();
  
  private Point2D mouseStart, mouseEnd, cropStart, cropEnd;
  private Rectangle cropRect = null;
  private Rectangle2D picRect, origViewPort;
  private boolean changed = false, noRemoveCrop = false, bufferOutOfDate = true, dontRefresh = false; 
  private FileDownloadUtility.Buffer imageBuffer;
  private String bufferFileName = "";
  
  @FXML private RadioButton rbNone;
  @FXML private RadioButton rbFile;
  @FXML private RadioButton rbWeb;
  @FXML private RadioButton rbCurrent;
  @FXML private TextField tfCurrent;
  @FXML private TextField tfFile;
  @FXML private TextField tfWeb;
  @FXML private Button btnBrowse;
  @FXML private TextField tfName;
  @FXML private Button btnRefresh;
  @FXML private Button btnDelete;
  @FXML private Button btnShow;
  @FXML private Button btnEdit;
  @FXML private Button btnOK;
  @FXML private Button btnCancel;
  @FXML private Button btnGoogle;
  @FXML private AnchorPane apPicture;
  @FXML private ImageView ivPicture;
  @FXML private Label lblChangeName;
  @FXML private ProgressBar progressBar;
  @FXML private Button btnStop;
  
  private PersonTabController personCtrlr;

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public static PictureDialogController create(String title, Rectangle2D viewPort, PersonTabController personCtrlr)
  {
    PictureDialogController pdc = HyperDialog.create("PictureDialog.fxml", title, true);
    pdc.init(viewPort, personCtrlr);
    return pdc;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void init(Rectangle2D viewPort, PersonTabController personCtrlr)
  {  
    this.personCtrlr = personCtrlr;
    
    ivPicture.fitWidthProperty().bind(apPicture.widthProperty());
    ivPicture.fitHeightProperty().bind(apPicture.heightProperty());
    
    apPicture.widthProperty().addListener((observable, oldValue, newValue) ->
    {
      resizeCrop(newValue.doubleValue(), apPicture.getHeight());
    });

    apPicture.heightProperty().addListener((observable, oldValue, newValue) ->
    {
      resizeCrop(apPicture.getWidth(), newValue.doubleValue());
    });

    btnBrowse.setOnAction(event -> btnBrowseClick());
    
    rbNone.   selectedProperty().addListener((observable, oldValue, newValue) -> { if (newValue) rbNoneSelected    (); });
    rbCurrent.selectedProperty().addListener((observable, oldValue, newValue) -> { if (newValue) rbCurrentSelected (); });
    rbFile.   selectedProperty().addListener((observable, oldValue, newValue) -> { if (newValue) rbFileSelected    (); });
    rbWeb.    selectedProperty().addListener((observable, oldValue, newValue) -> { if (newValue) rbWebSelected     (); });
       
    UnaryOperator<TextFormatter.Change> filter = (change) ->
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
    };
    
    tfWeb.setTextFormatter(new TextFormatter<>(filter));   
    
    tfWeb.focusedProperty().addListener((ov, oldValue, newValue) -> 
    {
      Platform.runLater(() ->
      {
        if (tfWeb.isFocused() && !tfWeb.getText().isEmpty())
          tfWeb.selectAll();
      });
    });

    tfName.focusedProperty().addListener((ov, oldValue, newValue) -> 
    {
      Platform.runLater(() -> 
      {
        if (tfName.isFocused() && !tfName.getText().isEmpty()) 
        {
          if (FilenameUtils.indexOfExtension(tfName.getText()) >= 0)
            tfName.selectRange(0, FilenameUtils.indexOfExtension(tfName.getText()));
          else
            tfName.selectAll();
        }
      });
    });
    
    btnRefresh.setOnAction(event ->
    {
      if      (rbCurrent.isSelected()) rbCurrentSelected();
      else if (rbFile.isSelected())    rbFileSelected();
      else if (rbWeb.isSelected())     rbWebSelected();
    });
    
    btnShow.disableProperty().bind(rbCurrent.selectedProperty().not().and(rbFile.selectedProperty().not())); 
    btnRefresh.disableProperty().bind(rbNone.selectedProperty());
    btnEdit.disableProperty().bind(rbNone.selectedProperty());
    tfName.disableProperty().bind(rbNone.selectedProperty());
    
    btnStop.setOnAction(event -> stopClicked());
    
    btnShow.setOnAction(event -> 
    {
      FilePath filePath = null;
      
      if (rbCurrent.isSelected())
        filePath = personCtrlr.curPicture;
      else if (rbFile.isSelected())
        filePath = new FilePath(tfFile.getText());

      if (FilePath.isEmpty(filePath)) return;
      
      highlightFileInExplorer(filePath);
    });
    
    btnGoogle.setOnAction(event ->
    {
      String term = personCtrlr.tfFirst.getText() + " " +
                    personCtrlr.tfLast.getText() + " " +
                    HyperTableCell.getCellText(personCtrlr.cbField.getSelectionModel().getSelectedItem());
      
      searchGoogleImage(term);
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
    
    btnEdit.setOnAction(event -> btnEditClick());
    btnDelete.setOnAction(event -> btnDeleteClick());
    
    btnDelete.disableProperty().bind(rbCurrent.selectedProperty().not());
    
    if (FilePath.isEmpty(personCtrlr.curPicture) == false)
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
      cropEnd = new Point2D(viewPort.getMaxX() / picture.getWidth(), viewPort.getMaxY() / picture.getHeight());
  
      setPicRect(ivPicture.getFitWidth(), ivPicture.getFitHeight());
      updateCropRect();
    }
    
    changed = false;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void autoGenerateName()
  {
    String ext = "";
    
    FilePath filePath = db.getPath(PREF_KEY_PICTURES_PATH, new FilePath(tfName.getText()));
    ext = filePath.getExtensionOnly();
    if (ext.length() == 0) ext = "jpg";
    
    String fileName = makeFileName(personCtrlr.tfLast.getText(), ext);
    HDT_Person person = personCtrlr.activeRecord();
        
    if (cantUseName(fileName)) fileName = makeFileName(personCtrlr.tfLast.getText() + personCtrlr.tfFirst.getText(), ext);
    if (cantUseName(fileName)) fileName = makeFileName(person.getLastName(), ext);
    if (cantUseName(fileName)) fileName = makeFileName(person.listName(), ext);
    if (cantUseName(fileName)) return;
    
    this.tfName.setText(fileName);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private String makeFileName(String name, String ext)
  {
    name = name.replaceAll("\\h+", "");
    name = FilePath.removeInvalidFileNameChars(name);
    
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
    
    FilePath filePath = db.getPath(PREF_KEY_PICTURES_PATH, new FilePath(fileName));
    if (filePath.equals(personCtrlr.curPicture)) return false;
    
    return filePath.exists(); 
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
  private void updateCropRect()
  {
    double width =  (cropEnd.getX() - cropStart.getX()) * picRect.getWidth();
    double height = (cropEnd.getY() - cropStart.getY()) * picRect.getHeight();
    
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
    
    int x1 = Double.valueOf((cropStart.getX() * picture.getWidth()) + 0.5).intValue();
    int x2 = Double.valueOf((cropEnd.getX() * picture.getWidth()) + 0.5).intValue();
    int y1 = Double.valueOf((cropStart.getY() * picture.getHeight()) + 0.5).intValue();
    int y2 = Double.valueOf((cropEnd.getY() * picture.getHeight()) + 0.5).intValue();
    
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
      
      double width =  (cropEnd.getX() - cropStart.getX()) * picRect.getWidth();
      double height = (cropEnd.getY() - cropStart.getY()) * picRect.getHeight();
      
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
    
    double picRatio = picture.getWidth() / picture.getHeight();
    double ivRatio = ivWidth / ivHeight;
    double sizeRatio = 1.0;
    
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
    if (FilePath.isEmpty(personCtrlr.curPicture))
      return;
   
    if (confirmDialog("Are you sure you want to delete the file \"" + personCtrlr.curPicture.getNameOnly() + "\"?") == false)
      return;

    if (personCtrlr.curPicture.deletePromptOnFail(false) == false)
      return;
    
    db.unmapFilePath(personCtrlr.curPicture);
    removeCrop();
    personCtrlr.curPicture = null;
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
      if (FilePath.isEmpty(personCtrlr.curPicture)) return;
      if (tfName.getText().length() == 0) return;
      
      src = personCtrlr.curPicture;
      dest = db.getPath(PREF_KEY_PICTURES_PATH, new FilePath(tfName.getText()));
      
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
        if (tfFile.getText().length() == 0) return;       
        src = new FilePath(tfFile.getText());
        dest = db.getPath(PREF_KEY_PICTURES_PATH, new FilePath(tfName.getText()));
        sameFile = src.equals(dest);
      }
      else
      {
        if (tfWeb.getText().length() == 0) return;
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
    FilePath picturePath = db.getPath(PREF_KEY_PICTURES_PATH, new FilePath(tfName.getText()));
    
    if (execPath.length() == 0)
      DesktopApi.edit(picturePath);
    else
      launchExplicit(execPath, picturePath.toString());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void rbNoneSelected()
  {
    removeCrop();
    
    ivPicture.setImage(null);
    picture = null;
    tfName.setText("");
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void rbWebSelected()
  {
    String url = tfWeb.getText();
       
    removeCrop();
    
    if (url.length() == 0)
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
    
    }, ex -> exceptionHappened(ex));    
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
    
    picture = new Image(path.toURI().toString());
    if (!picture.isError())
      ivPicture.setImage(picture);
    else
    {
      messageDialog("An error occurred while trying to display the picture: " + picture.getException().getMessage(), mtError);
      ivPicture.setImage(null);
      picture = null;
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void rbCurrentSelected()
  {
    removeCrop();
    
    picture = new Image(personCtrlr.curPicture.toURI().toString());
    
    if (!picture.isError())
    {
      ivPicture.setImage(picture);

      tfCurrent.setText(personCtrlr.curPicture.getNameOnly().toString());
      tfName.setText(personCtrlr.curPicture.getNameOnly().toString());
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

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("Image files", "*.jpg;*.gif;*.png;*.jpeg"));
    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));
    
    fileChooser.setInitialDirectory(db.getPath(PREF_KEY_PICTURES_PATH, null).toFile());

    FilePath filePath = new FilePath(fileChooser.showOpenDialog(getStage()));

    if (FilePath.isEmpty(filePath)) return;
    
    tfFile.setText(filePath.toString());
    
    if (rbFile.isSelected())
      rbFileSelected();
    else
      rbFile.setSelected(true);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  { 
    FilePath curFile = personCtrlr.curPicture,
             newFileOrig = null,
             newFileNew = db.getPath(PREF_KEY_PICTURES_PATH, new FilePath(tfName.getText()));
    
    String curFileName = "",
           newFileOldName, 
           newFileNewName = tfName.getText();
    
    if (FilePath.isEmpty(personCtrlr.curPicture) == false)
      curFileName = personCtrlr.curPicture.getNameOnly().toString();

    if (rbNone.isSelected())
    {
      personCtrlr.curPicture = null;
      noRemoveCrop = true;
      return true;
    }
    
  //---------------------------------------------------------------------------    
  // Perform checks before deleting previous image
  //---------------------------------------------------------------------------
    
    if (newFileNewName.length() == 0)
    {
      messageDialog("Name cannot be blank.", mtError);
      safeFocus(tfName);
      return false;
    }

    if (rbWeb.isSelected())
    {
      newFileOldName = "";
      
      if (tfWeb.getText().length() == 0)
      {
        messageDialog("Please enter a web address.", mtError);
        safeFocus(tfWeb);
        return false;
      }
      
      if (newFileNew.exists())
      {
        if (confirmDialog("A file with that name already exists. Okay to overwrite?") == false)
          return false;
        
        try
        {
          newFileNew.delete(false);
        }
        catch (IOException e)
        {
          return falseWithErrorMessage("File cannot be overwritten: " + e.getMessage());
        }
      }
    }
    else if (rbCurrent.isSelected())
    {
      newFileOldName = curFileName;
      newFileOrig = curFile;
    }
    else // (rbFile.isSelected())
    {
      newFileOrig = new FilePath(tfFile.getText());
      newFileOldName = newFileOrig.getNameOnly().toString();
      
      if (FilePath.isEmpty(newFileOrig) || (newFileOldName.length() == 0))
      {
        messageDialog("Please specify a file from the local file system.", mtError);
        safeFocus(tfFile);
        return false;
      }
      
      if (newFileOrig.exists() == false)
      {
        messageDialog("Please select an existing file.", mtError);
        safeFocus(tfFile);
        return false;
      }
      
      if (FilePath.isEmpty(curFile) == false)
        if (curFile.equals(newFileOrig))
        {
          messageDialog("Previous file and new file are the same.", mtError);
          safeFocus(tfFile);
          return false;
        }
    }
    
  //---------------------------------------------------------------------------    
  // Put new image in place
  //---------------------------------------------------------------------------   
    
    if (rbWeb.isSelected())
    {      
      try
      {
        if (bufferOutOfDate == false)
          imageBuffer.saveToFile(newFileNew);
        else
        {
          progressBar.setVisible(true);
          btnStop.setVisible(true);
          
          FileDownloadUtility.downloadToFile(tfWeb.getText(), db.getPath(PREF_KEY_PICTURES_PATH, null), newFileNewName, 
                                             new StringBuilder(), true, httpClient, buffer ->
          {
            personCtrlr.curPicture = newFileNew;
            noRemoveCrop = true;
            rbCurrent.setSelected(true);
            
            okClicked = true;
            dialogStage.close();

          }, ex -> exceptionHappened(ex));
          
          return false;
        }        
      } 
      catch (IOException ex) 
      {
        return falseWithErrorMessage("An error occurred while saving the file: " + ex.getMessage());
      }      
    }
    else if (rbFile.isSelected())
    {
      if (newFileOrig.equals(newFileNew) == false)
      {
        PopupDialog dlg = new PopupDialog("Should the file be moved or copied from its present location?");
        
        dlg.addButton("Move", mrMove);
        dlg.addButton("Copy", mrCopy);
        
        if (dlg.showModal() == mrMove)
        {
          try
          {
            newFileOrig.moveTo(newFileNew, true);
          } 
          catch (IOException e)
          {
            return falseWithErrorMessage("An error occurred while moving the file: " + e.getMessage());
          }
          
          db.unmapFilePath(newFileOrig);
        }
        else
        {
          try
          {
            newFileOrig.copyTo(newFileNew, true);
          } 
          catch (IOException e)
          {
            return falseWithErrorMessage("An error occurred while copying the file: " + e.getMessage());
          }
        }
      }
    }
    else if (curFile.equals(newFileNew) == false) // (rbCurrent.isSelected())
    {
      if (newFileNew.exists())
        return falseWithErrorMessage("Unable to rename file: A file with that name already exists.");
      
      try
      {
        HyperPath.renameFile(curFile, newFileNewName);
      }
      catch (SecurityException | IOException e)
      {
        return falseWithErrorMessage("An error occurred while renaming the file: " + e.getMessage());
      }
    }
  
    personCtrlr.curPicture = newFileNew;
    noRemoveCrop = true;
    rbCurrent.setSelected(true);
    
    return true;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

}