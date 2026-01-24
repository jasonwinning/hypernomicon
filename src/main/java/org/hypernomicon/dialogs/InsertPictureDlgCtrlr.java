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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.awt.image.BufferedImage;
import java.io.*;
import java.nio.file.Files;
import java.util.function.Consumer;
import java.util.function.Function;

import javax.imageio.ImageIO;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_FileType;
import org.hypernomicon.util.*;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.http.AsyncHttpClient;
import org.hypernomicon.util.http.FileDownloadUtility;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;

import javafx.application.Platform;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.web.WebView;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------

public class InsertPictureDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnPasteUrl, btnStop;
  @FXML private ComboBox<HyperTableCell> cbExisting;
  @FXML private ProgressBar progressBar;
  @FXML private RadioButton rbExistingRecord, rbLocalFile, rbWebAddress, rbClipboard;
  @FXML private TextField tfLocalFile, tfWebUrl;
  @FXML private ToggleButton btnJpg, btnPng;
  @FXML private ToggleGroup tgFormat;
  @FXML private WebView webView;

  private static final AsyncHttpClient httpClient = new AsyncHttpClient();

  private final HyperCB hcbExisting;
  private final Consumer<HDT_MiscFile> miscFileConsumer;

  private boolean webBufferOutOfDate = true, dontRefresh = false;
  private FileDownloadUtility.Buffer webImageBuffer;
  private BufferedImage clipboardImageBuffer;
  private String bufferFileName = "";

//---------------------------------------------------------------------------

  public InsertPictureDlgCtrlr(Consumer<HDT_MiscFile> miscFileConsumer)
  {
    super("InsertPictureDlg", "Insert Picture", true);

    Function<HDT_Record, String> textFunction = record ->
    {
      HDT_MiscFile miscFile = (HDT_MiscFile)record;

      String text = miscFile.name(),
             ext = miscFile.pathNotEmpty() ? miscFile.filePath().getExtensionOnly() : "";

      return ext.isBlank() ? text : (text + " (" + ext + ')');
    };

    hcbExisting = new HyperCB(cbExisting, ctEditableLimitedDropDown, new StandardPopulator(hdtMiscFile, null, textFunction));

    hcbExisting.addListener((ov, nv) ->
    {
      if (rbExistingRecord.isSelected())
        rbExistingRecordSelected();
      else
        rbExistingRecord.setSelected(true);
    });

    this.miscFileConsumer = miscFileConsumer;

    btnPasteUrl.setOnAction(event -> tfWebUrl.setText(getClipboardText(true)));
    setToolTip(btnPasteUrl, "Paste text from clipboard");

    tfWebUrl.setTextFormatter(new TextFormatter<>(change ->
    {
      if (change.isContentChange() == false)
        return change;

      webBufferOutOfDate = true;

      if (change.getText().length() > 1)
      {
        Platform.runLater(() -> rbWebAddress.setSelected(true));
      }
      else
      {
        Platform.runLater(() ->
        {
          dontRefresh = true;
          rbWebAddress.setSelected(true);
          dontRefresh = false;
        });
      }

      return change;
    }));

    tfWebUrl.focusedProperty().addListener((ob, oldValue, newValue) -> Platform.runLater(() ->
    {
      if (tfWebUrl.isFocused() && (tfWebUrl.getText().isBlank() == false))
        tfWebUrl.selectAll();
    }));

    forceToggleSelection(tgFormat);

    rbExistingRecord.selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbExistingRecordSelected(); });
    rbLocalFile     .selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbLocalFileSelected     (); });
    rbWebAddress    .selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbWebAddressSelected    (); });
    rbClipboard     .selectedProperty().addListener((ob, ov, nv) -> { if (Boolean.TRUE.equals(nv)) rbClipboardSelected     (); });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.setOnDragOver   (Event::consume);
    webView.setOnDragDropped(Event::consume);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbExistingRecordSelected()
  {
    displayFilePath(nullSwitch(hcbExisting.selectedRecord(), null, HDT_MiscFile::filePath));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbLocalFileSelected()
  {
    displayFilePath(new FilePath(tfLocalFile.getText()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void displayFilePath(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath))
    {
      webView.getEngine().loadContent("");
      return;
    }

    String url = filePath.toURLString();

    String html = """
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Centered Image</title>
  <style>

    body, html
    {
      width: 100%;
      height: 100%;
      margin: 0;
      display: flex;
      justify-content: center;
      align-items: center;
    }
    img
    {
      max-width: 100%;
      max-height: 100%;
      width: auto;
      height: auto;
    }

  </style>
</head>
<body>
  <img id="responsiveImage" src="$url" alt="Centered Image">
  <script>
    var imageHeight, imageWidth;

    function start()
    {
      imageHeight = document.getElementById('responsiveImage').naturalHeight;
      imageWidth = document.getElementById('responsiveImage').naturalWidth;

      adjustImageSize();
    }

    function adjustImageSize()
    {
      const img = document.getElementById('responsiveImage');
      const containerHeight = window.innerHeight;
      const containerWidth = window.innerWidth;

      if ((imageHeight < containerHeight) && (imageWidth < containerWidth))
      {
        // Display image in its natural size
        img.style.width = imageWidth + 'px';
        img.style.height = imageHeight + 'px';
      }
      else
      {
        // Scale image to fit within the container
        img.style.width = 'auto';
        img.style.height = 'auto';
        img.style.maxWidth = '100%';
        img.style.maxHeight = '100%';
      }
    }

    window.addEventListener('resize', adjustImageSize);
    window.addEventListener('load', start);
  </script>
</body>
</html>""".replace("$url", url);

    webView.getEngine().loadContent(html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbWebAddressSelected()
  {
    String url = tfWebUrl.getText();

    if (url.isBlank())
    {
      webView.getEngine().loadContent("");
      webImageBuffer = null;
      return;
    }

    if (webBufferOutOfDate == false)
    {
      refreshFromWebBuffer();
      return;
    }

    if (dontRefresh) return;

    progressBar.setVisible(true);
    btnStop.setVisible(true);

    StringBuilder fileName = new StringBuilder();

    FileDownloadUtility.downloadToBuffer(url, fileName, true, httpClient, buffer ->
    {
      webImageBuffer = buffer;
      bufferFileName = fileName.toString();
      refreshFromWebBuffer();

      progressBar.setVisible(false);
      btnStop.setVisible(false);
    },

    e ->
    {
      webImageBuffer = null;
      exceptionHappened(e);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePath writeBufferedImageToTempFile(BufferedImage bufferedImage) throws IOException
  {
    String format = tgFormat.getSelectedToggle() == btnJpg ? "jpg" : "png";

    File tempFile = java.io.File.createTempFile("temp", '.' + format);
    tempFile.deleteOnExit();

    try (FileOutputStream fos = new FileOutputStream(tempFile))
    {
      ImageIO.write(bufferedImage, format, fos);
    }

    return new FilePath(tempFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rbClipboardSelected()
  {
    clipboardImageBuffer = ClipboardImageHelper.getClipboardImageViaAWT();

    if (clipboardImageBuffer == null)
    {
      webView.getEngine().loadContent("");
      return;
    }

    try
    {
      FilePath tempFile = writeBufferedImageToTempFile(clipboardImageBuffer);

      displayFilePath(tempFile);

      runOutsideFXThread(2000, () ->
      {
        try { Files.delete(tempFile.toPath()); } catch (IOException e) { noOp(); }
      });
    }
    catch (IOException e)
    {
      exceptionHappened(e);
      webView.getEngine().loadContent("");
      clipboardImageBuffer = null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stopClicked()
  {
    httpClient.stop();

    webView.getEngine().loadContent("");

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
      FilePath tempFile = new FilePath(java.io.File.createTempFile("temp", bufferFileName));
      tempFile.deleteOnExit();
      webImageBuffer.saveToFile(tempFile);

      displayFilePath(tempFile);

      runOutsideFXThread(2000, () ->
      {
        try { Files.delete(tempFile.toPath()); } catch (IOException e) { noOp(); }
      });

      webBufferOutOfDate = false;
    }
    catch (Exception e)
    {
      exceptionHappened(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnBrowseClick()
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().addAll(new FileChooser.ExtensionFilter("Image files", "*.jpg;*.gif;*.png;*.jpeg"),
                                             new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    FilePath chosenFilePath = new FilePath(tfLocalFile.getText());

    fileChooser.setInitialDirectory(FilePath.isEmpty(chosenFilePath) == false ? chosenFilePath.getDirOnly().toFile() : db.unenteredPath().toFile());

    FilePath filePath = showOpenDialog(fileChooser);

    if (FilePath.isEmpty(filePath)) return;

    HDT_RecordWithPath existingRecord = HyperPath.getRecordFromFilePath(filePath);

    if (existingRecord != null)
    {
      infoPopup(HyperPath.alreadyInUseMessage(filePath, existingRecord));
      return;
    }

    tfLocalFile.setText(filePath.toString());

    if (rbLocalFile.isSelected())
      rbLocalFileSelected();
    else
      rbLocalFile.setSelected(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    HDT_MiscFile miscFile = null;
    FilePath srcFilePath = null;

    if (rbExistingRecord.isSelected())
    {
      miscFile = hcbExisting.selectedRecord();

      if (miscFile == null)
        return falseWithWarningPopup("You must select a record.", cbExisting);
    }
    else if (rbLocalFile.isSelected())
    {
      if (tfLocalFile.getText().isBlank())
        return falseWithWarningPopup("You must select a file.", tfLocalFile);

      srcFilePath = new FilePath(tfLocalFile.getText());
    }
    else if (rbWebAddress.isSelected())
    {
      if (tfWebUrl.getText().isBlank())
        return falseWithWarningPopup("You must enter a web adddress.", tfWebUrl);

      if (webImageBuffer == null)
        return falseWithWarningPopup("An image has not been loaded. Try reselecting the web address option.", rbLocalFile);

      try
      {
        srcFilePath = new FilePath(java.io.File.createTempFile("temp", bufferFileName));
        srcFilePath.deleteOnExit();
        webImageBuffer.saveToFile(srcFilePath);
      }
      catch (IOException e)
      {
        errorPopup("An error occurred while trying to save the picture: " + getThrowableMessage(e));
        return false;
      }
    }
    else if (rbClipboard.isSelected())
    {
      if (clipboardImageBuffer == null)
        return falseWithWarningPopup("An image has not been loaded from the clipboard. Try reselecting the clipboard option.", rbLocalFile);

      try
      {
        srcFilePath = writeBufferedImageToTempFile(clipboardImageBuffer);
      }
      catch (IOException e)
      {
        errorPopup("An error occurred while trying to save the picture: " + getThrowableMessage(e));
        return false;
      }
    }

    if (miscFile == null)
    {
      miscFile = db.createNewBlankRecord(hdtMiscFile);

      FileDlgCtrlr fdc = new FileDlgCtrlr("Image File Record", miscFile, "", true);

      if (FilePath.isEmpty(srcFilePath) == false)
        fdc.setSrcFilePath(srcFilePath, true);

      if (fdc.showModal() == false)
      {
        db.deleteRecord(miscFile);

        if (rbWebAddress.isSelected() || rbClipboard.isSelected())
          try { Files.delete(srcFilePath.toPath()); } catch (IOException e) { noOp(); }

        return false;
      }

      if (miscFile != fdc.getFileRecord())
      {
        db.deleteRecord(miscFile);
        miscFile = fdc.getFileRecord();
      }

      miscFile.setName(fdc.tfRecordName.getText());
      HyperTableCell cell = fdc.hcbType.selectedHTC();
      int fileTypeID = HyperTableCell.getCellID(cell);

      if ((fileTypeID < 1) && (HyperTableCell.getCellText(cell).length() > 0))
      {
        HDT_FileType fileType = db.createNewBlankRecord(hdtFileType);
        fileTypeID = fileType.getID();
        fileType.setName(HyperTableCell.getCellText(cell));
      }

      miscFile.fileType.setID(fileTypeID);

      if (rbWebAddress.isSelected() || rbClipboard.isSelected())
      {
        if (srcFilePath.exists())
          try { Files.delete(srcFilePath.toPath()); } catch (IOException e) { noOp(); }
      }
    }

    miscFileConsumer.accept(miscFile);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
